;;; eas-text-tile.el --- rect marks as nested, outlined tiles in text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, terminal half (eas-7r1.15).  A rect mark (not a bar: its
;; items have no orient) tiles a region: heatmap cells, treemap groups
;; nested in their parents.  Text draws it as SVG paints it:
;;
;;   - painter's order: a later rect takes the cells it covers, so a
;;     group nested in its parent shows in its own color;
;;   - a transparent rect (fill "none" or "transparent") draws no
;;     blocks: its cells keep what lies under them and gain its hover
;;     (datum and tooltip);
;;   - a stroked rect at least two cells each way outlines itself with
;;     box-drawing lines, laid on a lattice through cell centres, so
;;     adjacent rects share one line and their corners join (┬ ┼ ┤);
;;   - an opaque fill, made legible on the paper like any glyph,
;;     becomes the background of whatever is drawn over it in its view
;;     (edges, labels, line dots): a label sits on its tile, not in a
;;     hole, its color made legible against the fill.
;;
;; `eas-text-tile-draw' records fills and edges per cell while the
;; view's marks draw; `eas-text-tile-resolve' lays them down after.

;;; Code:

(require 'eas-core)
(require 'eas-color-names)
(require 'eas-text-ink)

(defvar eas-text-trace)
(defvar eas-text-trace-item)
(declare-function eas-text--grid-cols "eas-text")
(declare-function eas-text--grid-rows "eas-text")
(declare-function eas-text--grid-cw "eas-text")
(declare-function eas-text--grid-ch "eas-text")
(declare-function eas-text--grid-chars "eas-text")
(declare-function eas-text--grid-props "eas-text")
(declare-function eas-text--grid-prio "eas-text")
(declare-function eas-text--grid-cover "eas-text")
(declare-function eas-text--grid-dots "eas-text")
(declare-function eas-text--grid-dot-props "eas-text")
(declare-function eas-text--grid-dot-prio "eas-text")
(declare-function eas-text--cells "eas-text")
(declare-function eas-text--translucent "eas-text")
(declare-function eas-text--item-props "eas-text")

(defvar eas-text-tile--cells nil
  "Cell index -> tile record while a view's marks draw.
A record is a vector [FILL FILL-PRIO BITS EDGE EDGE-PRIO EDGE-PROPS]:
the background an opaque rect leaves, the box-drawing BITS crossing the
cell (`eas-text-tile--box'), their stroke color, priority and props.")

(defconst eas-text-tile--box
  [nil ?╵ ?╷ ?│ ?╴ ?┘ ?┐ ?┤ ?╶ ?└ ?┌ ?├ ?─ ?┴ ?┬ ?┼]
  "Box glyph for each set of lines leaving a cell centre.
Bits: 1 up, 2 down, 4 left, 8 right.")

(defun eas-text-tile-p (mark item)
  "Non-nil when ITEM of MARK draws as a tile: a rect item, not a bar."
  (and (equal (plist-get mark :mark) "rect")
       (member (plist-get item :orient) '(nil "none"))))

(defvar eas-text-tile--pool (make-vector 64 nil)
  "Tile records free for reuse, a stack in its first `eas-text-tile--free'.
A frame of a tiled view (pacman's maze) records thousands of cells:
reusing the records spares a vector each (eas-b2s.5).")

(defvar eas-text-tile--free 0 "Records on `eas-text-tile--pool'.")

(defvar eas-text-tile--table nil
  "A cell table free for the next view's tiles, or nil while one is in use.")

(defun eas-text-tile--record (i)
  "The tile record of cell index I, made when missing."
  (or (gethash i eas-text-tile--cells)
      (puthash i (if (= eas-text-tile--free 0) (vector nil nil 0 nil nil nil)
                   (let ((rec (aref eas-text-tile--pool (setq eas-text-tile--free (1- eas-text-tile--free)))))
                     (aset eas-text-tile--pool eas-text-tile--free nil)
                     (aset rec 0 nil) (aset rec 1 nil) (aset rec 2 0) (aset rec 3 nil) (aset rec 4 nil) (aset rec 5 nil)
                     rec))
               eas-text-tile--cells)))

(defun eas-text-tile-table ()
  "An empty cell table for `eas-text-tile--cells'.
Give it back with `eas-text-tile-release' once resolved."
  (or (prog1 eas-text-tile--table (setq eas-text-tile--table nil))
      (make-hash-table :test 'eql)))

(defun eas-text-tile-release (table)
  "Return TABLE's records to the pool and keep TABLE, emptied, for reuse."
  (maphash (lambda (_ rec)
             (when (= eas-text-tile--free (length eas-text-tile--pool))
               (setq eas-text-tile--pool (vconcat eas-text-tile--pool (make-vector (length eas-text-tile--pool) nil))))
             (aset eas-text-tile--pool eas-text-tile--free rec)
             (setq eas-text-tile--free (1+ eas-text-tile--free)))
           table)
  (clrhash table)
  (setq eas-text-tile--table table))

(defvar eas-text-tile--derived (make-hash-table :test 'eq :weakness 'key)
  "Interned props -> ((BG . PROPS) ...) derived from them by a tile.")

(defun eas-text-tile--derived (props bg)
  "PROPS with face foreground BG, or with no face when BG is nil; shared.
PROPS are interned (`eas-text--item-props'), so are these: a frame
derives them once per color, not once per tile (eas-b2s.5)."
  (let ((known (gethash props eas-text-tile--derived)))
    (or (cdr (assoc bg known))
        (let ((p (if bg (plist-put (copy-sequence props) 'face (list :foreground bg))
                   (let ((p (copy-sequence props))) (cl-remf p 'face) p))))
          (puthash props (cons (cons bg p) known) eas-text-tile--derived)
          p))))

(defun eas-text-tile--clear-p (item)
  "Non-nil when ITEM's fill paints nothing."
  (or (member (plist-get item :fill) '(nil "none" "transparent"))
      (<= (* (or (plist-get item :opacity) 1) (or (plist-get item :fillOpacity) 1)) 0)))

(defun eas-text-tile--stroke (item)
  "ITEM's visible stroke color, or nil."
  (let ((s (plist-get item :stroke)))
    (and (stringp s) (not (member s '("none" "transparent")))
         (> (or (plist-get item :strokeWidth) 1) 0)
         (> (or (plist-get item :strokeOpacity) 1) 0)
         s)))

(defun eas-text-tile--edge (g c0 r0 c1 r1 clip color prio props)
  "Record a lattice line from cell C0 R0 to C1 R1 (one row or column) in G.
Inside CLIP, in COLOR at PRIO; PROPS go to cells nothing holds."
  (let ((cols (eas-text--grid-cols g)) (rows (eas-text--grid-rows g))
        (across (= r0 r1)))
    (cl-loop for k from (if across c0 r0) to (if across c1 r1)
             for col = (if across k c0) for row = (if across r0 k)
             when (and (<= (max 0 (aref clip 0)) col) (< col (min cols (aref clip 2)))
                       (<= (max 0 (aref clip 1)) row) (< row (min rows (aref clip 3))))
             do (let ((rec (eas-text-tile--record (+ col (* row cols))))
                      (bits (logior (if (> k (if across c0 r0)) (if across 4 1) 0)
                                    (if (< k (if across c1 r1)) (if across 8 2) 0))))
                  (when (and eas-text-trace eas-text-trace-item) (funcall eas-text-trace col row))
                  (aset rec 2 (logior (aref rec 2) bits))
                  (aset rec 3 color) (aset rec 4 prio) (aset rec 5 props)))))

(defun eas-text-tile-draw (g view mark item clip prio)
  "Draw rect ITEM of MARK in VIEW into grid G at PRIO inside CLIP."
  (let* ((cw (eas-text--grid-cw g)) (ch (eas-text--grid-ch g)) (cols (eas-text--grid-cols g))
         (x (plist-get item :x)) (y (plist-get item :y)) (w (plist-get item :w)) (h (plist-get item :h))
         (xs (eas-text--cells x w cw)) (ys (eas-text--cells y h ch))
         (c0 (car xs)) (c1 (max (1+ c0) (cdr xs))) (r0 (car ys)) (r1 (max (1+ r0) (cdr ys)))
         (clear (eas-text-tile--clear-p item))
         (alpha (* (or (plist-get item :opacity) 1) (or (plist-get item :fillOpacity) 1)))
         (glyph (eas-text--translucent ?█ alpha))
         (bg (and (not clear) (eq glyph ?█) (eas-text-tile--fill (plist-get item :fill))))
         (props (let ((p (eas-text--item-props view mark item (plist-get item :datum))))
                  (if bg (eas-text-tile--derived p bg) p)))
         (hover (eas-text-tile--derived props nil))
         (stroke (eas-text-tile--stroke item)))
    (unless (or (< w 0.01) (< h 0.01))
      (cl-loop for row from (max r0 (aref clip 1) 0) below (min r1 (aref clip 3) (eas-text--grid-rows g))
               do (cl-loop for col from (max c0 (aref clip 0) 0) below (min c1 (aref clip 2) cols)
                           for i = (+ col (* row cols))
                           when (and eas-text-trace eas-text-trace-item) do (funcall eas-text-trace col row)
                           when (>= prio (aref (eas-text--grid-prio g) i))
                           do (if clear
                                  ;; See-through: what lies under stays, with this rect's hover.
                                  (aset (eas-text--grid-props g) i
                                        (append hover (let ((f (plist-get (aref (eas-text--grid-props g) i) 'face)))
                                                        (and f (list 'face f)))))
                                (aset (eas-text--grid-chars g) i glyph)
                                (aset (eas-text--grid-props g) i props)
                                (aset (eas-text--grid-prio g) i prio)
                                (aset (eas-text--grid-cover g) i 1.0)
                                (let ((rec (if bg (eas-text-tile--record i) (gethash i eas-text-tile--cells))))
                                  (when rec (aset rec 0 bg) (aset rec 1 prio))))))
      ;; Too small to outline without hiding the rect: its color shows it.
      (when (and stroke (>= (- c1 c0) 2) (>= (- r1 r0) 2))
        (dolist (side (list (list c0 r0 c1 r0) (list c0 r1 c1 r1) (list c0 r0 c0 r1) (list c1 r0 c1 r1)))
          (apply #'eas-text-tile--edge g (append side (list clip stroke prio hover))))))))

(defun eas-text-tile--fill (color)
  "COLOR as a tile's fill, legible on the paper like every other glyph."
  (eas-text-ink-legible (or (eas-color-hex color) color)))

(defun eas-text-tile--paper-p (color)
  "Non-nil when COLOR is all but white, the paper scenes are drawn for."
  (and (eas-text-ink--rgb color) (< (eas-text-ink-contrast color "#ffffff") 1.1)))

(defun eas-text-tile--on (fg bg)
  "FG, or the ink or background color best seen on BG when FG is not.
Nil FG counts as the ink."
  (let ((fg (or fg (eas-text-ink-foreground))))
    (if (and (eas-text-ink--rgb fg) (>= (eas-text-ink-contrast fg bg) eas-text-ink-min-contrast)) fg
      (let ((ink (eas-text-ink-foreground)) (paper (eas-text-ink-background)))
        (if (>= (eas-text-ink-contrast ink bg) (eas-text-ink-contrast paper bg)) ink paper)))))

(defun eas-text-tile--edge-color (stroke bg)
  "Box line color for STROKE drawn on BG (nil: the terminal's background)."
  (cond ((null bg) (eas-text-ink-legible stroke))
        ((and (eas-text-ink--rgb stroke) (>= (eas-text-ink-contrast stroke bg) eas-text-ink-min-contrast)) stroke)
        (t (eas-text-tile--on nil bg))))

(defun eas-text-tile--backed (props bg)
  "PROPS with background BG, their foreground kept legible on it.
PROPS whose face has a background already are returned as they are."
  (let ((face (plist-get props 'face)))
    (cond ((and (consp face) (keywordp (car face)) (plist-get face :background)) props)
          ((or (null face) (and (consp face) (keywordp (car face))))
           (plist-put (copy-sequence props) 'face
                      (list :foreground (eas-text-tile--on (plist-get face :foreground) bg) :background bg)))
          (t (plist-put (copy-sequence props) 'face (list (list :background bg) face))))))

(defun eas-text-tile-resolve (g)
  "Lay the recorded tile edges and backgrounds into grid G."
  (when eas-text-tile--cells
    (maphash
     (lambda (i rec)
       (let ((bg (aref rec 0)) (bits (aref rec 2)) (prio (aref (eas-text--grid-prio g) i)))
         (cond
          ;; A stroke the color of the paper is a gap between tiles:
          ;; with no fill behind it, SVG shows nothing there either.
          ((and (> bits 0) (<= prio (aref rec 4)) (or bg (not (eas-text-tile--paper-p (aref rec 3)))))
           (let ((props (if (> prio -1) (aref (eas-text--grid-props g) i) (aref rec 5))))
             (aset (eas-text--grid-chars g) i (aref eas-text-tile--box bits))
             (aset (eas-text--grid-props g) i
                   (plist-put (copy-sequence props) 'face
                              (append (list :foreground (eas-text-tile--edge-color (aref rec 3) bg))
                                      (and bg (list :background bg)))))
             (aset (eas-text--grid-prio g) i (aref rec 4))))
          ((and bg (> prio (aref rec 1)))
           ;; Drawn over the fill: a label or a symbol keeps the tile behind it.
           (aset (eas-text--grid-props g) i (eas-text-tile--backed (aref (eas-text--grid-props g) i) bg))))
         (when (and bg (> (aref (eas-text--grid-dots g) i) 0) (>= (aref (eas-text--grid-dot-prio g) i) (max prio (aref rec 1))))
           (aset (eas-text--grid-dot-props g) i (eas-text-tile--backed (aref (eas-text--grid-dot-props g) i) bg)))))
     eas-text-tile--cells)))

(provide 'eas-text-tile)
;;; eas-text-tile.el ends here

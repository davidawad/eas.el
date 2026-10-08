;;; eas-geoshape-render.el --- geoshape items in SVG, text and hit tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5 for geoshape items (eas-geoshape.el).  An item holds its
;; projected path relative to its anchor :x :y: :paths, a vector of
;; [CLOSED XY...] (CLOSED t for a polygon ring, :false for a line),
;; :circles of [DX DY R] (point geometries) and :box [X0 Y0 X1 Y1].
;; Everything here only reads those coordinates.
;;
;;   SVG    one path per item (rings closed with Z, points as circles),
;;          filled by the nonzero rule as Vega's canvas fills them.
;;   text   a filled shape takes the full block of every cell whose
;;          centre it holds (scanline, nonzero rule); its painted
;;          outline (and an unfilled shape: a graticule, a ring) is
;;          braille along the strokes, so borders and coasts show; a
;;          point's circle smaller than a cell is a dot glyph.
;;   hit    the smallest shape holding the pointer; else the nearest box.

;;; Code:

(require 'cl-lib)
(require 'eas-core)

(declare-function eas-svg--n "eas-svg")
(declare-function eas-svg--node "eas-svg")
(declare-function eas-mark-style-svg "eas-mark-style")
(declare-function eas-text--put "eas-text")
(declare-function eas-text--col "eas-text")
(declare-function eas-text--row "eas-text")
(declare-function eas-text--dot-line "eas-text")
(declare-function eas-text--item-props "eas-text")
(declare-function eas-text--grid-cw "eas-text")
(declare-function eas-text--grid-ch "eas-text")
(defvar eas-text--dot-prio)

(defun eas-geoshape--visible-fill-p (item)
  "Non-nil when ITEM's fill paints."
  (let ((f (plist-get item :fill)))
    (and f (not (equal f "none")) (not (equal (plist-get item :fillOpacity) 0)))))

(defun eas-geoshape--visible-stroke-p (item)
  "Non-nil when ITEM's stroke paints."
  (let ((s (plist-get item :stroke)))
    (and s (not (equal s "none")) (not (equal (plist-get item :strokeOpacity) 0))
         (not (equal (plist-get item :strokeWidth) 0)))))

;;; SVG

(defconst eas-geoshape--ints
  (let ((v (make-vector 8192 nil))) (dotimes (i 8192) (aset v i (number-to-string i))) v)
  "The decimal strings of 0 to 8191, the integer parts of path numbers.")

(defconst eas-geoshape--fracs
  (let ((v (make-vector 100 nil)))
    (dotimes (r 100)
      (aset v r (cond ((= r 0) "") ((= (% r 10) 0) (format ".%d" (/ r 10))) (t (format ".%02d" r)))))
    v)
  "The trimmed decimals of the hundredths 0 to 99: \"\", \".1\", \".01\"...")

(defun eas-geoshape--n-slow (v)
  "Number V as `eas-svg--n' writes a float: \"%.2f\" trimmed."
  (let ((s (format "%.2f" v)))
    (cond ((string-suffix-p ".00" s) (substring s 0 -3))
          ((eq (aref s (1- (length s))) ?0) (substring s 0 -1))
          (t s))))

(defmacro eas-geoshape--push-n (v place)
  "Push float V's strings onto PLACE, as \"%.2f\" writes V, trimmed.
A trailing \".00\" or \"0\" goes, as in `eas-svg--n'.  V times 100
rounds to the hundredths \"%.2f\" prints unless it lies within 1e-4
of a tie (its rounding error is below 1e-9 under 8191): those, NaNs
and large numbers take `format'.  Whole parts and decimals come from
tables, so a number allocates no string."
  (macroexp-let2 nil v v
    `(let* ((s (* ,v 100.0)) (n (and (< -819100.0 s 819100.0) (round s))))
       (if (and n (< -0.4999 (- s n) 0.4999))
           (let ((m (abs n)))
             (when (or (< n 0) (and (= n 0) (< (copysign 1.0 ,v) 0))) (push "-" ,place))
             (push (aref eas-geoshape--ints (/ m 100)) ,place)
             (push (aref eas-geoshape--fracs (% m 100)) ,place))
         (push (eas-geoshape--n-slow ,v) ,place)))))

(defun eas-geoshape--svg-rings (x y paths)
  "SVG path data of PATHS ([CLOSED XY...] vectors) moved by X Y.
Each number as `eas-svg--n' writes it, \"%.2f\" without a trailing
\".00\" or \"0\", from `eas-geoshape--push-n's tables and one `concat':
a world map's thousands of points are most of its SVG's cost."
  (let ((parts nil))
    (seq-doseq (p paths)
      (let* ((flat (aref p 1)) (n (length flat)) (i 0))
        (while (< i n)
          (push (if (= i 0) "M" "L") parts)
          (eas-geoshape--push-n (+ x (aref flat i)) parts)
          (push "," parts)
          (eas-geoshape--push-n (+ y (aref flat (1+ i))) parts)
          (setq i (+ i 2)))
        (when (eq (aref p 0) t) (push "Z" parts))))
    (if parts (apply #'concat (nreverse parts)) "")))

(defun eas-geoshape-svg-d (item)
  "The SVG path data of geoshape ITEM, in absolute pixels."
  (let* ((x (plist-get item :x)) (y (plist-get item :y))
         (parts (list (eas-geoshape--svg-rings x y (plist-get item :paths)))))
    (seq-doseq (c (plist-get item :circles))
      (let* ((cx (+ x (aref c 0))) (cy (+ y (aref c 1))) (r (aref c 2)) (rs (eas-svg--n r)))
        (push (format "M%s,%sA%s,%s,0,1,1,%s,%sA%s,%s,0,1,1,%s,%sZ"
                      (eas-svg--n (+ cx r)) (eas-svg--n cy) rs rs (eas-svg--n (- cx r)) (eas-svg--n cy)
                      rs rs (eas-svg--n (+ cx r)) (eas-svg--n cy))
              parts)))
    (apply #'concat (nreverse parts))))

(defun eas-geoshape-svg (item fill stroke opacity)
  "The SVG node of geoshape ITEM painted FILL, STROKE and OPACITY."
  (eas-mark-style-svg
   (eas-svg--node 'path :d (eas-geoshape-svg-d item) :fill (or fill "none")
                  :stroke (unless (equal stroke "none") stroke)
                  :stroke-width (unless (equal stroke "none") (plist-get item :strokeWidth))
                  :opacity opacity)
   item))

;;; Geometry helpers

(defun eas-geoshape--crossings (item y)
  "Sorted (X . WINDING) crossings of ITEM's closed rings with the line Y.
Y is relative to the item's anchor."
  (let (out)
    (seq-doseq (p (plist-get item :paths))
      (when (eq (aref p 0) t)
        (let* ((flat (aref p 1)) (n (/ (length flat) 2)))
          (dotimes (i n)
            (let* ((j (mod (1+ i) n))
                   (x0 (aref flat (* 2 i))) (y0 (aref flat (1+ (* 2 i))))
                   (x1 (aref flat (* 2 j))) (y1 (aref flat (1+ (* 2 j)))))
              (when (or (and (<= y0 y) (> y1 y)) (and (<= y1 y) (> y0 y)))
                (push (cons (+ x0 (* (- y y0) (/ (- x1 x0) (- y1 y0)))) (if (> y1 y0) 1 -1)) out)))))))
    (sort out (lambda (a b) (< (car a) (car b))))))

(defun eas-geoshape--inside-p (item px py)
  "Non-nil when pixel PX PY lies inside ITEM (nonzero rule, or a circle)."
  (let ((dx (- px (plist-get item :x))) (dy (- py (plist-get item :y))))
    (or (seq-some (lambda (c) (<= (+ (expt (- dx (aref c 0)) 2) (expt (- dy (aref c 1)) 2)) (expt (aref c 2) 2)))
                  (plist-get item :circles))
        (let ((w 0))
          (cl-loop for (x . d) in (eas-geoshape--crossings item dy)
                   while (< x dx) do (setq w (+ w d)))
          (/= w 0)))))

;;; Text

(defun eas-geoshape--text-fill (g item props clip prio)
  "Put full blocks with PROPS at PRIO in G's cells (within CLIP) ITEM fills.
A shape holding no cell's centre (a county in a small window) takes its
anchor's cell at a lower rank, so it shows unless another shape of the
mark covers that cell."
  (let* ((filled nil) (cw (float (eas-text--grid-cw g))) (ch (float (eas-text--grid-ch g)))
         (x (plist-get item :x)) (y (plist-get item :y)) (box (plist-get item :box))
         (r0 (max (aref clip 1) (eas-text--row g (+ y (aref box 1)))))
         (r1 (min (1- (aref clip 3)) (eas-text--row g (+ y (aref box 3))))))
    (cl-loop for row from r0 to r1
             for cy = (- (* (+ row 0.5) ch) y)
             do (let ((w 0) (start nil))
                  (dolist (c (eas-geoshape--crossings item cy))
                    (let ((was w))
                      (setq w (+ w (cdr c)))
                      (cond ((and (= was 0) (/= w 0)) (setq start (car c)))
                            ((and (/= was 0) (= w 0))
                             (cl-loop for col from (max (aref clip 0) (ceiling (- (/ (+ x start) cw) 0.5)))
                                      to (min (1- (aref clip 2)) (floor (- (/ (+ x (car c)) cw) 0.5)))
                                      do (eas-text--put g col row ?█ props prio) (setq filled t))))))))
    (unless filled
      (let ((col (eas-text--col g x)) (row (eas-text--row g y)))
        (when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3)))
          (eas-text--put g col row ?█ props prio 0.5))))))

(defun eas-geoshape--text-stroke (g item props clip prio)
  "Draw ITEM's rings and lines into G as braille with PROPS at PRIO in CLIP."
  (let ((eas-text--dot-prio prio) (pf (lambda (_) props))
        (x (plist-get item :x)) (y (plist-get item :y)))
    (seq-doseq (p (plist-get item :paths))
      (let* ((flat (aref p 1)) (n (/ (length flat) 2)))
        (dotimes (i (if (eq (aref p 0) t) n (1- n)))
          (let ((j (mod (1+ i) n)))
            (eas-text--dot-line g (+ x (aref flat (* 2 i))) (+ y (aref flat (1+ (* 2 i))))
                                (+ x (aref flat (* 2 j))) (+ y (aref flat (1+ (* 2 j)))) pf clip)))))))

(defun eas-geoshape-text (g view mark item clip prio)
  "Draw geoshape ITEM of MARK in VIEW into grid G at PRIO inside CLIP cells."
  (let ((props (eas-text--item-props view mark item (plist-get item :datum)))
        (cw (float (eas-text--grid-cw g))) (ch (float (eas-text--grid-ch g)))
        (x (plist-get item :x)) (y (plist-get item :y)))
    (when (eas-geoshape--visible-fill-p item)
      (eas-geoshape--text-fill g item props clip prio))
    (when (eas-geoshape--visible-stroke-p item)
      (eas-geoshape--text-stroke
       g item (eas-text--item-props view mark (plist-put (copy-sequence item) :fill "none") (plist-get item :datum))
       clip prio))
    ;; Points: a circle within a cell is one glyph; a larger one was drawn above.
    (seq-doseq (c (plist-get item :circles))
      (when (< (* 2 (aref c 2)) (max cw ch))
        (let ((col (eas-text--col g (+ x (aref c 0)))) (row (eas-text--row g (+ y (aref c 1)))))
          (when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3)))
            (eas-text--put g col row (if (eas-geoshape--visible-fill-p item) ?● ?○) props prio)))))))

;;; Hit test

(defun eas-geoshape-hit (mark px py)
  "Return the hit candidate of geoshape MARK at pixel PX PY.
The candidate is shaped as `eas-hit-mark' gives it.
A shape holding the pointer is at distance (almost) 0, the smallest
first; otherwise the nearest bounding box wins."
  (let ((items (plist-get mark :items)) best best-d)
    (dotimes (i (length items))
      (let* ((item (aref items i)) (b (plist-get item :box))
             (x0 (+ (plist-get item :x) (aref b 0))) (y0 (+ (plist-get item :y) (aref b 1)))
             (x1 (+ (plist-get item :x) (aref b 2))) (y1 (+ (plist-get item :y) (aref b 3)))
             (ex (max 0 (- x0 px) (- px x1))) (ey (max 0 (- y0 py) (- py y1)))
             (d (if (and (= ex 0) (= ey 0))
                    (if (eas-geoshape--inside-p item px py)
                        (* 1e-9 (sqrt (max 0 (* (- x1 x0) (- y1 y0)))))
                      ;; Inside the box but not the shape: as far as the box's nearest edge.
                      (min (- px x0) (- x1 px) (- py y0) (- y1 py)))
                  (sqrt (+ (* ex ex) (* ey ey))))))
        (when (or (null best-d) (< d best-d)) (setq best i best-d d))))
    (when best
      (let ((item (aref items best)))
        (list :mark (plist-get mark :id) :item best :datum (plist-get item :datum)
              :distance best-d :x (plist-get item :x) :y (plist-get item :y))))))

(provide 'eas-geoshape-render)
;;; eas-geoshape-render.el ends here

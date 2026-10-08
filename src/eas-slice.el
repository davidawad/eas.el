;;; eas-slice.el --- draw a GUI chart as tiles, re-rasterizing changed ones -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 glue (eas-e3s).  In a GUI frame librsvg rasterizes the
;; whole chart image whenever its SVG changes, and that raster scales
;; with image pixels: 74-105 ms of every changed frame at 1000x640 on a
;; Retina NS window, against 1-3 ms of engine work (engine-spikes.md
;; 8.14).  Most frames change a small part of the picture: a clock's
;; hands, the routes of one airport, a few ladder bars.
;;
;; `eas-slice-frame' cuts the chart into a grid of cells (about
;; `eas-slice-size' pixels) and draws it as images of runs of cells,
;; each the chart's document under a root whose viewBox is its part of
;; the chart.  An image is drawn again only when the frame changed what
;; it shows; an unchanged image keeps the very descriptor of the last
;; frame, so Emacs finds it in its image cache and does not rasterize
;; it again.  What a frame changed is read from the scenes
;; (`eas-slice-dirty'): marks are compared item by item and the bounds
;; of every changed item, before and after, padded for strokes and
;; anti-aliasing, mark the cells to redraw (a bar that grew marks only
;; the strip its end moved over).  Anything else that changed (size,
;; axes, legends, titles, theme) redraws everything.
;;
;; Each image also costs a fixed time besides its pixels, so the changed
;; cells of a row are drawn as one image per run, and frames that keep
;; changing most of the chart (a running share of at least
;; `eas-slice-whole-share', a full redraw counting as all of it) are
;; drawn as one image, as before tiles.
;;
;; The images lie in rows, one buffer line each, with `line-height' t
;; on the newline so the line is exactly as tall as its images.  Emacs
;; only rasterizes the images a window shows, so a chart larger than
;; its window costs the visible part only.  An image's :map holds the
;; hot spots it overlaps, clipped and in its own pixels, their
;; help-echo text kept aside (`eas-slice-help-echo') so a changed
;; tooltip does not change the image spec; :eas-origin is its offset in
;; the chart (`eas-mode-event-px' adds it back).
;;
;; A replaced image is not flushed at once: a bounded ring
;; (`eas-slice-ring-bytes' of raster) keeps recent ones in the image
;; cache, so an animation that revisits a frame (pacman) finds them.
;; Images leaving the ring are flushed with `image-flush'.
;;
;; `eas-slice-stats' counts images: :rastered (new images Emacs must
;; rasterize, and :pixels their chart pixels), :kept (unchanged),
;; :ring-hits (found in the ring) and :full (frames that redrew
;; everything).  Each image rasterizes to exactly the pixels of the same
;; part of the current scene drawn afresh, and to those of a whole
;; render but for a few anti-aliased edge pixels: eas-slice-test.el
;; checks both with rsvg-convert.

;;; Code:

(require 'cl-lib)
(require 'eas-marks-bounds)
(require 'eas-layout)
(require 'eas-arc)

(declare-function eas-geoshape-item-box "eas-geoshape")

(defcustom eas-slice-tiles 'auto
  "Whether a GUI chart is drawn as tiles that re-rasterize separately.
`auto' tiles in graphic frames, t always, nil never (one image)."
  :type '(choice (const auto) (const t) (const nil)) :group 'eas-chart)

(defcustom eas-slice-size 120
  "Target edge of a tile, in chart pixels.
Smaller tiles redraw less area per change and cost more images."
  :type 'integer :group 'eas-chart)

(defcustom eas-slice-whole-share 0.6
  "Share of the chart frames must change to be drawn as one image.
The share is a running mean over frames, a full redraw counting as 1.
Each image costs a fixed time besides its pixels: frames changing
most of the chart are cheaper as one image than as many tiles."
  :type 'number :group 'eas-chart)

(defcustom eas-slice-ring-bytes (* 32 1024 1024)
  "Raster bytes of replaced tiles kept in the image cache, for revisits.
Zero flushes every replaced tile at once."
  :type 'integer :group 'eas-chart)

(defvar eas-slice-ring-max 256
  "Most replaced tiles the ring holds, whatever their size.")

(defvar eas-slice-min-height 32
  "Least height of a tile row, in chart pixels.
A row is a buffer line; it must be at least a line of text tall.")

(defvar eas-slice-pad 3
  "Pixels added around a changed item's bounds, for anti-aliasing.")

(defvar eas-slice-stats nil
  "Tile counters, a plist: :frames :tiles :rastered :kept :ring-hits :full.
:pixels counts the chart pixels of rastered tiles, :chart-pixels those
of every frame's chart.
`eas-slice-stats-reset' zeroes it.")

(defun eas-slice-stats-reset ()
  "Zero `eas-slice-stats'."
  (setq eas-slice-stats (list :frames 0 :tiles 0 :rastered 0 :kept 0 :ring-hits 0 :full 0
                              :pixels 0 :chart-pixels 0)))

(eas-slice-stats-reset)

(defun eas-slice--count (key n)
  "Add N to counter KEY of `eas-slice-stats'."
  (setq eas-slice-stats (plist-put eas-slice-stats key (+ n (or (plist-get eas-slice-stats key) 0)))))

(cl-defstruct (eas-slice-frame (:constructor eas-slice--make-frame) (:copier nil))
  "One frame drawn as tiles.
SCENE and THEME are what it was drawn from, XS and YS the grid's
column and row edges, TILES a vector of image descriptors in display
order, SEGS for each its cells [R0 R1 C0 C1] (rows R0 to R1 and
columns C0 to C1, the ends excluded) and MAPS its :map.  HEAVY is the
running share of the chart frames changed (`eas-slice-whole-share').  HELP maps hot-spot ids to their help-echo text."
  scene theme xs ys segs tiles maps help heavy)

;;; The grid

(defun eas-slice--edges (len target least)
  "Edges cutting LEN pixels into parts near TARGET long, none under LEAST.
A vector of integers from 0 to LEN."
  (let* ((len (max 1 (round len)))
         (n (max 1 (min (round len (max 1 target)) (floor len (max 1 least))))))
    (vconcat (cl-loop for i from 0 to n collect (/ (* i len) n)))))

;;; What a frame changed

(defconst eas-slice--mark-keys '(:id :path :rows :index :items)
  "Mark keys that do not draw (or are compared item by item).")

(defun eas-slice--plist-equal (a b skip)
  "Non-nil when plists A and B are `equal' on every key but those in SKIP."
  (and (cl-loop for (k v) on a by #'cddr
                always (or (memq k skip) (equal v (plist-get b k))))
       (cl-loop for (k _) on b by #'cddr
                always (or (memq k skip) (plist-member a k)))))

(defun eas-slice--metrics ()
  "Text metrics item bounds are measured with."
  (eas-layout-metrics 'svg nil nil))

(defun eas-slice--item-box (kind item metrics)
  "Bounds [X0 Y0 X1 Y1] of ITEM of mark KIND as drawn, padded; or nil.
Nil means unknown: the caller redraws everything."
  (let* ((sw (let ((w (plist-get item :strokeWidth))) (if (numberp w) w 1)))
         (box (condition-case nil
                  (pcase kind
                    ((or "bar" "rect" "brush" "image")
                     (let ((x (plist-get item :x)) (y (plist-get item :y)) (w (plist-get item :w)) (h (plist-get item :h)))
                       (and (numberp x) (numberp y) (numberp w) (numberp h)
                            (vector (min x (+ x w)) (min y (+ y h)) (max x (+ x w)) (max y (+ y h))))))
                    ((or "rule" "tick" "line" "area" "trail" "arc")
                     (eas-marks-item-bounds kind item metrics))
                    ("geoshape" (and (fboundp 'eas-geoshape-item-box) (eas-geoshape-item-box item)))
                    ("text"
                     ;; The font Emacs draws with may be wider than the
                     ;; metrics compile measured: grow by half again.
                     (let ((b (eas-marks-item-bounds kind item metrics))
                           (fs (let ((s (plist-get item :fontSize))) (if (numberp s) s 11))))
                       (and b (let ((dx (+ fs (* 0.5 (- (aref b 2) (aref b 0))))))
                                (vector (- (aref b 0) dx) (- (aref b 1) fs) (+ (aref b 2) dx) (+ (aref b 3) fs))))))
                    (_
                     ;; Symbols: a shape of area :size; custom paths may
                     ;; reach past the circle of that area.
                     (let ((x (plist-get item :x)) (y (plist-get item :y))
                           (r (sqrt (let ((s (plist-get item :size))) (if (numberp s) (abs s) 64)))))
                       (and (numberp x) (numberp y) (vector (- x r) (- y r) (+ x r) (+ y r))))))
                (error nil))))
    (when (and box (cl-every #'numberp box))
      (let ((p (+ eas-slice-pad sw)))
        (vector (- (aref box 0) p) (- (aref box 1) p) (+ (aref box 2) p) (+ (aref box 3) p))))))

(defconst eas-slice--rect-geometry '(:x :y :w :h :datum :tooltip)
  "Rect item keys that move it (or do not draw): the rest is its style.")

(defun eas-slice--rect-edges (kind a b)
  "Boxes of the edges rect A moved to become B, of mark KIND; or nil.
Nil unless A and B are rects of one style sharing one side's span: a
bar that grew changes only the strip between its old and new ends
\(widened by its corner radius), not the whole bar."
  (when (and (member kind '("bar" "rect"))
             (cl-every (lambda (k) (numberp (plist-get a k))) '(:x :y :w :h))
             (cl-every (lambda (k) (numberp (plist-get b k))) '(:x :y :w :h))
             (eas-slice--plist-equal a b eas-slice--rect-geometry))
    (let* ((box (lambda (r) (let ((x (plist-get r :x)) (y (plist-get r :y)) (w (plist-get r :w)) (h (plist-get r :h)))
                              (vector (min x (+ x w)) (min y (+ y h)) (max x (+ x w)) (max y (+ y h))))))
           (p (let ((sw (plist-get b :strokeWidth)) (corners (plist-get b :corners)))
                (+ eas-slice-pad (if (numberp sw) sw 1)
                   (cond ((numberp corners) corners)
                         ((and (vectorp corners) (> (length corners) 0) (cl-every #'numberp corners))
                          (apply #'max (append corners nil)))
                         ((null corners) 0)
                         (t 1000)))))
           (ra (funcall box a)) (rb (funcall box b))
           ;; A strip per moved edge E: from its old to its new place,
           ;; across the rect's span on the other axis.
           (strips (lambda (edges)
                     (let ((out nil))
                       (dolist (e edges out)
                         (let ((lo (min (aref ra e) (aref rb e))) (hi (max (aref ra e) (aref rb e))))
                           (unless (= lo hi)
                             (push (if (memq e '(0 2))
                                       (vector (- lo p) (- (aref ra 1) p) (+ hi p) (+ (aref ra 3) p))
                                     (vector (- (aref ra 0) p) (- lo p) (+ (aref ra 2) p) (+ hi p)))
                                   out))))))))
      (cond ((and (= (aref ra 1) (aref rb 1)) (= (aref ra 3) (aref rb 3))) (or (funcall strips '(0 2)) 'same))
            ((and (= (aref ra 0) (aref rb 0)) (= (aref ra 2) (aref rb 2))) (or (funcall strips '(1 3)) 'same))))))

(defun eas-slice--mark-boxes (old new metrics)
  "Boxes that changed between mark OLD and mark NEW, or t when unknown.
Text is measured with METRICS."
  (let* ((kind (plist-get new :mark))
         (a (plist-get old :items)) (b (plist-get new :items))
         (a (if (vectorp a) a (vconcat a))) (b (if (vectorp b) b (vconcat b)))
         (boxes nil))
    (catch 'all
      (let ((add (lambda (k item)
                   (let ((box (eas-slice--item-box k item metrics)))
                     (unless box (throw 'all t))
                     (push box boxes)))))
        (if (and (equal kind (plist-get old :mark)) (= (length a) (length b))
                 (eas-slice--plist-equal old new eas-slice--mark-keys))
            (dotimes (i (length b))
              (let ((x (aref a i)) (y (aref b i)))
                (unless (or (eq x y) (equal x y))
                  (let ((edges (eas-slice--rect-edges kind x y)))
                    (cond ((eq edges 'same))
                          (edges (setq boxes (nconc edges boxes)))
                          (t (funcall add kind x) (funcall add kind y)))))))
          (seq-doseq (x a) (funcall add (plist-get old :mark) x))
          (seq-doseq (y b) (funcall add kind y))))
      boxes)))

(defun eas-slice-dirty (old new &optional metrics)
  "What scene NEW changed since scene OLD, as boxes [X0 Y0 X1 Y1].
Return t when everything may have changed, else a list of the boxes
\(nil: nothing).  METRICS default to `eas-slice--metrics'."
  (if (or (null old) (not (eas-slice--plist-equal old new '(:views :params)))
          (/= (length (plist-get old :views)) (length (plist-get new :views))))
      t
    (catch 'all
      (let ((boxes nil) (metrics (or metrics (eas-slice--metrics))))
        (cl-loop
         for ov across (plist-get old :views) for nv across (plist-get new :views)
         do (unless (eas-slice--plist-equal ov nv '(:marks :params :scales))
              (throw 'all t))
         do (let ((am (plist-get ov :marks)) (bm (plist-get nv :marks)))
              (unless (and (= (length am) (length bm))
                           (cl-every (lambda (x y) (equal (plist-get x :id) (plist-get y :id))) am bm))
                (throw 'all t))
              (cl-loop for x across (if (vectorp am) am (vconcat am))
                       for y across (if (vectorp bm) bm (vconcat bm))
                       unless (or (eq x y)
                                  (and (let ((a (plist-get x :items)) (b (plist-get y :items)))
                                         (or (eq a b) (equal a b)))
                                       (eas-slice--plist-equal x y eas-slice--mark-keys)))
                       do (let ((b (eas-slice--mark-boxes x y metrics)))
                            (when (eq b t) (throw 'all t))
                            (setq boxes (nconc b boxes))))))
        boxes))))

;;; Tiles

(defvar-local eas-slice--help nil
  "Help-echo text of the hot spots on display, by area id (a hash table).")

(defun eas-slice--props (props help id)
  "Area PROPS with a string help-echo moved into hash HELP under ID.
The area's help-echo becomes `eas-slice-help-echo', so the tile's
image spec does not change when only a tooltip's text does."
  (let ((text (plist-get props 'help-echo)))
    (if (not (stringp text)) props
      (puthash id text help)
      (plist-put (copy-sequence props) 'help-echo #'eas-slice-help-echo))))

(defun eas-slice--tile-map (map x0 y0 x1 y1 &optional help)
  "Areas of hot-spot MAP overlapping tile X0 Y0 X1 Y1, in tile pixels.
Rects are clipped to the tile, so an area that changed elsewhere
leaves this tile's map alone.  With hash HELP, string help-echos move
into it (`eas-slice--props')."
  (let ((out nil) (w (- x1 x0)) (h (- y1 y0)))
    (dolist (area map)
      (let* ((shape (car area))
             (moved
              (pcase (car shape)
                ('rect (let ((a (cadr shape)) (b (cddr shape)))
                         (when (and (< (car a) x1) (> (car b) x0) (< (cdr a) y1) (> (cdr b) y0))
                           (cons 'rect (cons (cons (max 0 (- (car a) x0)) (max 0 (- (cdr a) y0)))
                                             (cons (min w (- (car b) x0)) (min h (- (cdr b) y0))))))))
                ('circle (let ((c (cadr shape)) (r (cddr shape)))
                           (when (and (< (- (car c) r) x1) (> (+ (car c) r) x0)
                                      (< (- (cdr c) r) y1) (> (+ (cdr c) r) y0))
                             (cons 'circle (cons (cons (- (car c) x0) (- (cdr c) y0)) r)))))
                ('poly (let* ((v (cdr shape)) (n (length v)) (moved (copy-sequence v))
                              (minx most-positive-fixnum) (maxx most-negative-fixnum)
                              (miny most-positive-fixnum) (maxy most-negative-fixnum))
                         (cl-loop for i from 0 below n by 2
                                  do (setq minx (min minx (aref v i)) maxx (max maxx (aref v i))
                                           miny (min miny (aref v (1+ i))) maxy (max maxy (aref v (1+ i))))
                                  do (aset moved i (- (aref v i) x0))
                                  do (aset moved (1+ i) (- (aref v (1+ i)) y0)))
                         (when (and (> n 0) (< minx x1) (> maxx x0) (< miny y1) (> maxy y0))
                           (cons 'poly moved))))
                (_ shape))))
        (when moved
          (push (list moved (nth 1 area) (if help (eas-slice--props (nth 2 area) help (nth 1 area)) (nth 2 area)))
                out))))
    (nreverse out)))

(defun eas-slice--inside-p (shape x y)
  "Non-nil when point X Y lies in :map SHAPE."
  (pcase (car shape)
    ('rect (let ((a (cadr shape)) (b (cddr shape)))
             (and (<= (car a) x) (< x (car b)) (<= (cdr a) y) (< y (cdr b)))))
    ('circle (let ((c (cadr shape)) (r (cddr shape)))
               (<= (+ (expt (- x (car c)) 2) (expt (- y (cdr c)) 2)) (* r r))))
    ('poly (let* ((v (cdr shape)) (n (/ (length v) 2)) (in nil) (j (1- n)))
             ;; Even-odd crossings, as Emacs tests a polygon.
             (dotimes (i n)
               (let ((xi (aref v (* 2 i))) (yi (aref v (1+ (* 2 i))))
                     (xj (aref v (* 2 j))) (yj (aref v (1+ (* 2 j)))))
                 (when (and (not (eq (> yi y) (> yj y)))
                            (< x (+ xi (/ (* (- xj xi) (- y yi)) (float (- yj yi))))))
                   (setq in (not in))))
               (setq j i))
             in))))

(defun eas-slice-area-at (image x y)
  "The id of the hot spot of tile IMAGE at X Y (its pixels), or nil.
The last area listed wins, as in Emacs."
  (let ((id nil))
    (dolist (area (plist-get (cdr image) :map) id)
      (when (eas-slice--inside-p (car area) x y) (setq id (nth 1 area))))))

(defun eas-slice-help-echo (window _object pos)
  "Help-echo of the hot spot under the mouse in the tile at POS of WINDOW.
The text was recorded by the frame that drew the tile."
  (let* ((buffer (window-buffer window))
         (image (get-text-property pos 'display buffer))
         (glyph (posn-x-y (posn-at-point pos window)))
         (mouse (mouse-pixel-position))
         (edges (window-inside-pixel-edges window)))
    (when (and image glyph (numberp (cadr mouse)) (numberp (cddr mouse)))
      (let ((id (eas-slice-area-at image (- (cadr mouse) (nth 0 edges) (car glyph))
                                   (- (cddr mouse) (nth 1 edges) (cdr glyph))))
            (help (buffer-local-value 'eas-slice--help buffer)))
        (and id help (gethash id help))))))

(defconst eas-slice--background "<rect width=\"100%\" height=\"100%\""
  "How `eas-svg-dom' starts the chart's background rect.")

(defun eas-slice--split (svg)
  "Split chart SVG into the parts every tile's SVG is made of.
Return (ATTRS BEFORE . AFTER): ATTRS the text after the root's viewBox
up to its `>'; BEFORE the body up to the background rect's attributes
\(sized in percent of the viewport), AFTER the rest, or nil when there
is no such rect.  Nil when SVG does not start as `eas-svg-dom' prints."
  (when (string-match "\\`<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"[^\"]*\" height=\"[^\"]*\" viewBox=\"[^\"]*\"\\([^>]*\\)>" svg)
    (let* ((attrs (match-string 1 svg)) (start (match-end 0))
           (bg (string-search eas-slice--background svg start)))
      (if bg
          (cons attrs (cons (substring svg start (+ bg 5)) (substring svg (+ bg 5))))
        (cons attrs (cons (substring svg start) nil))))))

(defun eas-slice-tile-svg (parts x y w h)
  "The SVG of the tile at X Y of size W H, from PARTS of `eas-slice--split'.
The chart's document under a root whose viewBox is the tile; the
background rect, sized in percent of the viewport, is moved onto it."
  (concat (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"%d %d %d %d\"%s>"
                  w h x y w h (car parts))
          (cadr parts)
          (when (cddr parts) (format " x=\"%d\" y=\"%d\"" x y))
          (cddr parts)))

(defun eas-slice--image (data map x y)
  "An image descriptor of tile DATA at X Y, with hot spots MAP."
  (let ((props (list :map map :original-map map :scale 1 :ascent 'center :eas-origin (cons x y))))
    (if (and (display-images-p) (image-type-available-p 'svg))
        (apply #'create-image data 'svg t props)
      (append (list 'image :type 'svg :data data) props))))

(defun eas-slice--touches (boxes x0 y0 x1 y1)
  "Non-nil when one of BOXES overlaps the tile X0 Y0 X1 Y1."
  (cl-some (lambda (b) (and (< (aref b 0) x1) (> (aref b 2) x0) (< (aref b 1) y1) (> (aref b 3) y0)))
           boxes))

;;; The ring of replaced tiles

(defvar-local eas-slice--ring nil
  "Replaced tile images kept in the image cache, the latest first.
Each entry is (BYTES . IMAGE).")

(defun eas-slice--bytes (image)
  "Raster bytes IMAGE takes in the image cache, an estimate."
  (let* ((scale (if (and (display-graphic-p) (fboundp 'frame-scale-factor)) (frame-scale-factor) 1))
         (data (plist-get (cdr image) :data))
         (w (and (string-match "\\`<svg[^>]* width=\"\\([0-9]+\\)\" height=\"\\([0-9]+\\)\"" data)
                 (string-to-number (match-string 1 data))))
         (h (and w (string-to-number (match-string 2 data)))))
    (round (* 4 (or w 100) (or h 100) scale scale))))

(defun eas-slice--flush (image)
  "Drop IMAGE from the image cache."
  (when (fboundp 'image-flush) (image-flush image t)))

(defun eas-slice--retire (images shown)
  "Put replaced IMAGES in the ring, flushing what falls out of it.
SHOWN is the list of images on display, which are never flushed."
  (dolist (image images)
    (unless (cl-some (lambda (e) (eq (cdr e) image)) eas-slice--ring)
      (push (cons (eas-slice--bytes image) image) eas-slice--ring)))
  (let ((total 0) (n 0) (keep nil) (drop nil))
    (dolist (e eas-slice--ring)
      (if (and (< n eas-slice-ring-max) (<= (+ total (car e)) eas-slice-ring-bytes))
          (progn (push e keep) (setq total (+ total (car e)) n (1+ n)))
        (push e drop)))
    (setq eas-slice--ring (nreverse keep))
    (dolist (e drop)
      ;; One cache entry serves every `equal' spec: keep a shown one.
      (unless (cl-some (lambda (s) (equal s (cdr e))) shown)
        (eas-slice--flush (cdr e))))))

(defun eas-slice--from-ring (image)
  "An image of the ring `equal' to IMAGE, taken out of the ring; or nil."
  (let ((e (cl-find-if (lambda (e) (equal (cdr e) image)) eas-slice--ring)))
    (when e
      (setq eas-slice--ring (delq e eas-slice--ring))
      (cdr e))))

(defun eas-slice-ring-clear ()
  "Flush the images of the ring and empty it."
  (mapc (lambda (e) (eas-slice--flush (cdr e))) eas-slice--ring)
  (setq eas-slice--ring nil))

;;; A frame

(defun eas-slice--runs (cols)
  "Group the sorted integers COLS into spans of consecutive ones.
Each span is (C0 . C1), C1 excluded."
  (let ((runs nil))
    (dolist (c cols)
      (if (and runs (= (cdar runs) c)) (setcdr (car runs) (1+ c))
        (push (cons c (1+ c)) runs)))
    (nreverse runs)))

(defun eas-slice-frame (scene svg map theme prev)
  "Tile SCENE, drawn as SVG with hot spots MAP, reusing frame PREV.
THEME is anything else the drawing depended on (face colors): a
change redraws every tile.  PREV is the last `eas-slice-frame' of
this buffer, or nil.  Return (FRAME . REPLACED): FRAME the new
`eas-slice-frame', REPLACED the images of PREV it no longer shows.

The grid cuts the chart into cells of about `eas-slice-size'; an
image covers a run of cells of one row.  The cells a frame changed
are redrawn as one image per run of changed cells, so a change that
spans a row costs one image, not one per cell (each image has a
fixed cost besides its pixels).  An image some of whose cells changed
is redrawn whole: its unchanged cells become images of their own."
  (let* ((size (plist-get scene :size))
         (w (round (plist-get size :w))) (h (round (plist-get size :h)))
         (xs (eas-slice--edges w eas-slice-size 1))
         (ys (eas-slice--edges h eas-slice-size (max eas-slice-min-height 1)))
         (nx (1- (length xs))) (ny (1- (length ys)))
         (same-grid (and prev (equal xs (eas-slice-frame-xs prev)) (equal ys (eas-slice-frame-ys prev))
                         (equal theme (eas-slice-frame-theme prev))))
         (boxes (if same-grid (eas-slice-dirty (eas-slice-frame-scene prev) scene) t))
         (old-segs (and same-grid (eas-slice-frame-segs prev)))
         (dirty (make-vector ny nil)) (share 0) (heavy 0)
         (parts nil) (segs nil) (tiles nil) (maps nil)
         (help (make-hash-table :test 'eq))
         (replaced nil) (rastered 0) (kept 0) (hits 0) (pixels 0))
    ;; The cells the frame changed, and their share of the chart.
    (dotimes (r ny)
      (let ((y0 (aref ys r)) (y1 (aref ys (1+ r))) (v (make-bool-vector nx (eq boxes t))))
        (unless (eq boxes t)
          (dotimes (c nx)
            (when (eas-slice--touches boxes (aref xs c) y0 (aref xs (1+ c)) y1)
              (aset v c t)
              (setq share (+ share (* (- (aref xs (1+ c)) (aref xs c)) (- y1 y0)))))))
        (aset dirty r v)))
    ;; Tiles redraw every old image a changed cell lies in: count those,
    ;; unless the last frame was one image (it must split again).
    (when (and same-grid (consp boxes) (> (length old-segs) 1))
      (setq share 0)
      (seq-doseq (seg old-segs)
        (when (cl-loop for r from (aref seg 0) below (aref seg 1)
                       thereis (cl-loop for c from (aref seg 2) below (aref seg 3) thereis (aref (aref dirty r) c)))
          (setq share (+ share (* (- (aref xs (aref seg 3)) (aref xs (aref seg 2)))
                                  (- (aref ys (aref seg 1)) (aref ys (aref seg 0)))))))))
    (setq share (if (eq boxes t) 1.0 (/ share (float (max 1 (* w h))))))
    (setq heavy (if (and prev (eas-slice-frame-heavy prev))
                    (/ (+ share (eas-slice-frame-heavy prev)) 2.0)
                  share))
    (cl-flet ((draw (r0 r1 c0 c1)
                ;; A new image of rows R0 to R1 and columns C0 to C1, or the ring's.
                (let* ((x0 (aref xs c0)) (x1 (aref xs c1)) (y0 (aref ys r0)) (y1 (aref ys r1))
                       (seg-map (eas-slice--tile-map map x0 y0 x1 y1 help)))
                  (unless parts
                    (setq parts (or (eas-slice--split svg) (error "Unexpected SVG root for tiles"))))
                  (let* ((new (eas-slice--image (eas-slice-tile-svg parts x0 y0 (- x1 x0) (- y1 y0)) seg-map x0 y0))
                         (ring (eas-slice--from-ring new)))
                    (if ring (setq hits (1+ hits))
                      (setq rastered (1+ rastered) pixels (+ pixels (* (- x1 x0) (- y1 y0)))))
                    (list (vector r0 r1 c0 c1) (or ring new) seg-map))))
              (keep (k)
                (list (aref old-segs k) (aref (eas-slice-frame-tiles prev) k) (aref (eas-slice-frame-maps prev) k))))
      (let ((out nil))
        (cond
         ;; Nothing changed: every image stays, a whole one too.
         ((and same-grid (null boxes)
               (cl-loop for k below (length old-segs)
                        for seg = (aref old-segs k)
                        always (equal (eas-slice--tile-map map (aref xs (aref seg 2)) (aref ys (aref seg 0))
                                                           (aref xs (aref seg 3)) (aref ys (aref seg 1)) help)
                                      (aref (eas-slice-frame-maps prev) k))))
          (push (cl-loop for k below (length old-segs) collect (keep k)) out)
          (setq kept (length old-segs)))
         ;; Frames change most of the chart, of late: one image
         ;; (`eas-slice-whole-share').  The share is a running mean, so
         ;; a chart does not alternate between one image and tiles: each
         ;; switch to tiles redraws everything.
         ((>= heavy eas-slice-whole-share)
          (when same-grid (setq replaced (append (eas-slice-frame-tiles prev) replaced)))
          (push (list (draw 0 ny 0 nx)) out))
         (t
          (dotimes (r ny)
            (let ((redraw (make-bool-vector nx (not same-grid))) (row nil) (d (aref dirty r)))
              ;; Keep the old images of this row whose cells and hot spots
              ;; are unchanged; an image spanning rows (a whole frame) is
              ;; replaced.
              (when same-grid
                (dotimes (k (length old-segs))
                  (let ((seg (aref old-segs k)))
                    (when (and (<= (aref seg 0) r) (< r (aref seg 1)))
                      (let ((c0 (aref seg 2)) (c1 (aref seg 3)))
                        (if (and (= (aref seg 1) (1+ (aref seg 0)))
                                 (cl-loop for c from c0 below c1 never (aref d c))
                                 (equal (eas-slice--tile-map map (aref xs c0) (aref ys r) (aref xs c1) (aref ys (1+ r)) help)
                                        (aref (eas-slice-frame-maps prev) k)))
                            (progn (push (keep k) row) (setq kept (1+ kept)))
                          (unless (memq (aref (eas-slice-frame-tiles prev) k) replaced)
                            (push (aref (eas-slice-frame-tiles prev) k) replaced))
                          (cl-loop for c from c0 below c1 do (aset redraw c t))))))))
              ;; The rest: a run of changed cells is one image, as is a run
              ;; of the unchanged cells of a replaced image.
              (dolist (run (append (eas-slice--runs (cl-loop for c below nx when (and (aref redraw c) (aref d c)) collect c))
                                   (eas-slice--runs (cl-loop for c below nx when (and (aref redraw c) (not (aref d c))) collect c))))
                (push (draw r (1+ r) (car run) (cdr run)) row))
              (push (sort row (lambda (a b) (< (aref (car a) 2) (aref (car b) 2)))) out)))))
        ;; OUT holds the rows, the last first.
        (dolist (e (apply #'append (nreverse out)))
          (push (nth 0 e) segs) (push (nth 1 e) tiles) (push (nth 2 e) maps))))
    (when (and prev (not same-grid))
      (setq replaced (append (eas-slice-frame-tiles prev) replaced)))
    ;; An image both replaced and shown again (from the ring) stays.
    (setq replaced (cl-remove-if (lambda (i) (memq i tiles)) replaced))
    (eas-slice--count :frames 1)
    (eas-slice--count :tiles (length tiles))
    (eas-slice--count :rastered rastered)
    (eas-slice--count :pixels pixels)
    (eas-slice--count :chart-pixels (* w h))
    (eas-slice--count :kept kept)
    (eas-slice--count :ring-hits hits)
    (when (eq boxes t) (eas-slice--count :full 1))
    (cons (eas-slice--make-frame :scene scene :theme theme :xs xs :ys ys
                                 :segs (vconcat (nreverse segs)) :tiles (vconcat (nreverse tiles))
                                 :maps (vconcat (nreverse maps)) :help help :heavy heavy)
          replaced)))

;;; The buffer

(defvar-local eas-slice--frame nil
  "The `eas-slice-frame' this buffer shows, or nil.")

(defun eas-slice-p ()
  "Non-nil when the current buffer's chart should be drawn as tiles."
  (pcase eas-slice-tiles
    ('auto (and (display-graphic-p) (image-type-available-p 'svg)))
    (v v)))

(defun eas-slice-insert (frame)
  "Insert FRAME's tiles at point, a row per line, each line ending in a newline."
  (let ((nx (1- (length (eas-slice-frame-xs frame))))
        (tiles (eas-slice-frame-tiles frame)) (segs (eas-slice-frame-segs frame)))
    (dotimes (i (length tiles))
      (insert (propertize "#" 'display (aref tiles i) 'rear-nonsticky t))
      (when (= (aref (aref segs i) 3) nx)
        ;; The line is exactly as tall as its tiles.
        (insert (propertize "\n" 'line-height t 'line-spacing 0))))))

(defun eas-slice-redraw (scene svg map theme strip)
  "Make the current buffer show SCENE (drawn as SVG, hot spots MAP) as tiles.
THEME is as for `eas-slice-frame'; STRIP is the text under the chart.
Unchanged tiles keep their images; replaced ones go to the ring."
  (let* ((prev (and eas-slice--frame
                    (eq (get-text-property (point-min) 'display) (aref (eas-slice-frame-tiles eas-slice--frame) 0))
                    eas-slice--frame))
         (result (eas-slice-frame scene svg map theme prev))
         (frame (car result)))
    (unless prev (setq result (cons frame (and eas-slice--frame (append (eas-slice-frame-tiles eas-slice--frame) nil)))))
    (erase-buffer)
    (eas-slice-insert frame)
    (insert strip)
    (setq eas-slice--frame frame eas-slice--help (eas-slice-frame-help frame))
    (eas-slice--retire (cdr result) (append (eas-slice-frame-tiles frame) nil))
    frame))

(defun eas-slice-forget ()
  "Forget the current buffer's tiles, flushing them and the ring."
  (when eas-slice--frame
    (mapc #'eas-slice--flush (eas-slice-frame-tiles eas-slice--frame))
    (setq eas-slice--frame nil))
  (eas-slice-ring-clear))

(provide 'eas-slice)
;;; eas-slice.el ends here

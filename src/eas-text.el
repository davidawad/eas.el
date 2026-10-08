;;; eas-text.el --- scene/v1 -> propertized character grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L5, terminal half.  Draws a text-target scene into a grid of cells
;; (pixel X,Y lands in cell floor(X/W), floor(Y/H) for the scene's cell
;; size).  Series are braille (2x4 dots per cell), areas and bars use
;; eighth blocks, axes are box-drawing lines.  Every mark cell carries
;; `eas-view', `eas-mark', `eas-datum' and `help-echo', so moving
;; point is the terminal's hover.  Output is a deterministic string.
;; Like the SVG renderer it reads only the scene, and the background
;; it draws on: colors go through `eas-text-ink-legible'.

;;; Code:

(require 'eas-core)
(require 'eas-gc)
(require 'eas-glyph)
(require 'eas-hit)
(require 'eas-arc)
(require 'eas-text-arc)
(require 'eas-axis-extra)
(require 'eas-symbols)
(require 'eas-marks-image)
(require 'eas-scale)
(require 'eas-text-ink)
(require 'eas-text-band)
(require 'eas-text-ramp)
(require 'eas-render-cache)
(require 'eas-text-tile)

(defface eas-axis '((t :inherit shadow)) "Face for eas axis lines and grid." :group 'faces)
(defface eas-label '((t :inherit default)) "Face for eas tick labels." :group 'faces)
(defface eas-title '((t :inherit bold)) "Face for eas chart and axis titles." :group 'faces)

(defvar eas-text-trace nil
  "When non-nil, a function called with COL ROW for each cell drawn.
It sees every in-grid cell a mark item draws (whether or not a
higher-priority glyph wins the cell), while `eas-text-trace-item' names
the item as (VIEW-ID MARK-ID INDEX).
eas-text-check.el uses it to prove every item lands in a cell.")

(defvar eas-text-trace-item nil
  "The (VIEW-ID MARK-ID INDEX) being drawn, for `eas-text-trace'.")

(cl-defstruct (eas-text--grid (:constructor eas-text--grid-make))
  cols rows cw ch chars props prio dots dot-props dot-prio cover bands brush)

(defun eas-text--new (scene &optional shape-only)
  "An empty grid sized for SCENE.
With SHAPE-ONLY, only its size: `eas-text--fill' allocates the cells."
  (let* ((size (plist-get scene :size)) (cell (plist-get size :cell))
         (cw (aref cell 0)) (ch (aref cell 1))
         (cols (max 1 (round (/ (float (plist-get size :w)) cw))))
         (rows (max 1 (round (/ (float (plist-get size :h)) ch))))
         (g (eas-text--grid-make :cols cols :rows rows :cw cw :ch ch)))
    (if shape-only g (eas-text--fill g))))

(defun eas-text--fill (g)
  "Give grid G empty cells; return it."
  (let ((n (* (eas-text--grid-cols g) (eas-text--grid-rows g))))
    (setf (eas-text--grid-chars g) (make-vector n ?\s) (eas-text--grid-props g) (make-vector n nil)
          (eas-text--grid-prio g) (make-vector n -1) (eas-text--grid-dots g) (make-vector n 0)
          (eas-text--grid-dot-props g) (make-vector n nil) (eas-text--grid-dot-prio g) (make-vector n -1)
          (eas-text--grid-cover g) (make-vector n 0.0) (eas-text--grid-bands g) (make-hash-table :test 'eql)
          (eas-text--grid-brush g) (make-vector n nil))
    g))

(defvar eas-text--dot-prio 2
  "Priority of the braille dots being drawn.
A cell shows its dots unless a glyph of higher priority holds it.")

(defvar eas-text--mask nil
  "Nil, or a `bool-vector' of the grid rows a frame may write (eas-b2s.7).
An incremental frame repaints only the rows its changed items touch:
every write site asks `eas-text--touch' first, and a row outside the
mask keeps what the last frame left in it.")

(defvar eas-text--step-rows nil
  "Nil, or a `bool-vector' collecting the rows the running step tries.")

(defvar eas-text--lo 0 "Lowest row the item being drawn has tried.")
(defvar eas-text--hi -1 "Highest row the item being drawn has tried.")

(defsubst eas-text--touch (row)
  "Record that the step and item being drawn try ROW; non-nil if writable.
ROW must be on the grid.  It is recorded whether or not the write wins
its cell, so a later frame knows which steps a changed row needs."
  (when eas-text--step-rows
    (aset eas-text--step-rows row t)
    (when (< row eas-text--lo) (setq eas-text--lo row))
    (when (> row eas-text--hi) (setq eas-text--hi row)))
  (or (null eas-text--mask) (aref eas-text--mask row)))

(defsubst eas-text--writable-p (row)
  "Non-nil when this frame may write ROW (see `eas-text--mask')."
  (or (null eas-text--mask) (aref eas-text--mask row)))

(defun eas-text--put (g col row char props prio &optional cover)
  "Put CHAR with PROPS at COL ROW of grid G when PRIO wins.
COVER ranks a bar's glyph (`eas-text--coverage'): within one priority
\(one mark) it takes a cell only from a lower rank, so a bar's thin end
cannot replace its neighbour's full block."
  (when (and (< -1 col (eas-text--grid-cols g)) (< -1 row (eas-text--grid-rows g)))
    (let* ((i (+ col (* row (eas-text--grid-cols g))))
           (old (aref (eas-text--grid-prio g) i)))
      (when (and eas-text-trace eas-text-trace-item) (funcall eas-text-trace col row))
      (when (and (eas-text--touch row) (>= prio old)
                 (or (null cover) (> prio old) (> cover (aref (eas-text--grid-cover g) i))))
        (aset (eas-text--grid-chars g) i char)
        (aset (eas-text--grid-props g) i props)
        (aset (eas-text--grid-prio g) i prio)
        (aset (eas-text--grid-cover g) i (or cover 1.0))))))

(defun eas-text--string (g x y text align props prio)
  "Put TEXT anchored at pixel X Y with ALIGN into grid G.
Multi-line TEXT puts each line on the next row down."
  (if (string-search "\n" text)
      (seq-do-indexed (lambda (line i) (eas-text--string g x (+ y (* i (eas-text--grid-ch g))) line align props prio))
                      (split-string text "\n"))
    (eas-text--string-1 g x y text align props prio)))

(defun eas-text-string-span (cols cw ch x y text align &optional rows)
  "Return (ROW START . END) of the cells one-line TEXT takes.
TEXT is anchored at pixel X Y with ALIGN on a canvas COLS cells wide
\(ROWS high) of CW x CH pixel cells.  Text that would run off any side
is moved in, as long as it fits:
a label centred on the canvas's last pixel row still shows."
  (let* ((len (string-width text))
         (c (round (/ x (float cw))))
         (start (pcase align ("left" c) ("right" (- c len)) (_ (round (- (/ x (float cw)) (/ len 2.0))))))
         (start (if (<= len cols) (max 0 (min start (- cols len))) start)))
    (cons (let ((row (floor (/ y (float ch))))) (if rows (max 0 (min row (1- rows))) row))
          (cons start (+ start len)))))

(defvar eas-text--clamp-rows nil
  "Non-nil while drawing text kept on the grid's first or last row.
Such text is moved onto the row rather than lost past it: tick labels
and text marks.  Titles are not, so one placed off the canvas cannot
cover the labels it sits by.")

(defun eas-text--string-1 (g x y text align props prio)
  "Put one-line TEXT anchored at pixel X Y with ALIGN into grid G.
A double-width character takes two cells; the second holds 0, which
`eas-text--compose-row' leaves out."
  (let* ((span (eas-text-string-span (eas-text--grid-cols g) (eas-text--grid-cw g) (eas-text--grid-ch g)
                                     x y text align (and eas-text--clamp-rows (eas-text--grid-rows g))))
         (row (car span)) (col (cadr span)))
    (dotimes (k (length text))
      (let ((w (char-width (aref text k))))
        (eas-text--put g col row (aref text k) props prio)
        (dotimes (j (1- w)) (eas-text--put g (+ col 1 j) row 0 props prio))
        (setq col (+ col w))))))

(defun eas-text--col (g x) "Cell column of pixel X in grid G." (floor (/ x (float (eas-text--grid-cw g)))))
(defun eas-text--row (g y) "Cell row of pixel Y in grid G." (floor (/ y (float (eas-text--grid-ch g)))))

(defun eas-text--dot (g dx dy props clip)
  "Set braille dot DX DY (dot coordinates) of G with PROPS in CLIP cells."
  (let ((col (floor dx 2)) (row (floor dy 4)))
    (when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3))
               (< -1 col (eas-text--grid-cols g)) (< -1 row (eas-text--grid-rows g)))
      (let ((i (+ col (* row (eas-text--grid-cols g)))))
        (when (and eas-text-trace eas-text-trace-item) (funcall eas-text-trace col row))
        (when (eas-text--touch row)
        (aset (eas-text--grid-dots g) i
              (logior (aref (eas-text--grid-dots g) i)
                      (aref (aref eas-glyph-braille-dots (mod dy 4)) (mod dx 2))))
        (aset (eas-text--grid-dot-props g) i props)
        (aset (eas-text--grid-dot-prio g) i (max eas-text--dot-prio (aref (eas-text--grid-dot-prio g) i))))))))

(defun eas-text--dasher (dash)
  "Return a function telling whether the next dot of a DASH stroke is drawn.
It takes no arguments.
Each dash and gap length counts in dots, so a pattern stays readable."
  (let* ((runs (vconcat (mapcar (lambda (v) (max 1 (round v))) dash)))
         (period (apply #'+ (append runs nil))) (n -1))
    (if (or (< (length runs) 2) (zerop (aref dash 1))) (lambda () t)
      (lambda ()
        (setq n (mod (1+ n) period))
        (let ((k 0) (acc 0))
          (while (>= n (+ acc (aref runs k))) (setq acc (+ acc (aref runs k)) k (1+ k)))
          (cl-evenp k))))))

(defun eas-text--dot-line (g x1 y1 x2 y2 props-fn clip &optional show-p props)
  "Draw a braille line in G from pixel X1 Y1 to X2 Y2 inside CLIP cells.
PROPS-FN maps a pixel x to props; with PROPS non-nil every dot takes
PROPS and PROPS-FN is not called.  SHOW-P, when non-nil, is called per
dot and skips the dot when it says nil."
  (let* ((sx (/ 2.0 (eas-text--grid-cw g))) (sy (/ 4.0 (eas-text--grid-ch g)))
         (a (floor (* x1 sx))) (b (floor (* y1 sy))) (c (floor (* x2 sx))) (d (floor (* y2 sy)))
         (dx (abs (- c a))) (dy (- (abs (- d b)))) (stepx (if (< a c) 1 -1)) (stepy (if (< b d) 1 -1))
         (err (+ dx dy)) (done nil)
         ;; Dots of one dot column share props: look them up once
         ;; (eas-b2s.9: a step's riser asked for every dot).
         (last-a nil) (last-props nil))
    (while (not done)
      (when (or (null show-p) (funcall show-p))
        (eas-text--dot g a b (or props (if (eql a last-a) last-props
                                         (setq last-a a last-props (funcall props-fn (/ (+ a 0.5) sx)))))
                       clip))
      (if (and (= a c) (= b d)) (setq done t)
        (let ((e2 (* 2 err)))
          (when (>= e2 dy) (setq err (+ err dy) a (+ a stepx)))
          (when (<= e2 dx) (setq err (+ err dx) b (+ b stepy))))))))

(defun eas-text--tooltip (item)
  "Return the `help-echo' text for ITEM."
  (when-let* ((tip (plist-get item :tooltip)))
    (mapconcat (lambda (p) (format "%s: %s" (plist-get p :title) (plist-get p :value))) tip "\n")))

(defvar eas-text--props-memo (make-hash-table :test 'equal)
  "Interned item props: (INK VIEW MARK DATUM COLOR TOOLTIP . LEGIBLE) -> plist.
INK is `eas-text-ink--colors', LEGIBLE what else ink depends on.
A frame looks props up instead of consing a plist, a face and a tooltip
string per item (eas-b2s.5); equal inputs give one `eq' plist, which
rows compare and redisplay's face cache hit cheaply.  Never mutate one.")

(defvar eas-text--props-memo-max 8192
  "Entries `eas-text--props-memo' holds before it starts afresh.")

(defvar eas-text--props-key (make-list 8 nil)
  "The key `eas-text--item-props' looks up with, reused (no allocation).")

(defun eas-text--item-props (view mark item datum)
  "Text properties for a cell of ITEM (DATUM) in MARK of VIEW.
The result is shared (`eas-text--props-memo'): copy it before changing it."
  (let* ((color (let ((f (plist-get item :fill)) (s (plist-get item :stroke)))
                  (if (or (null f) (equal f "none")) s f)))
         (key eas-text--props-key) (k key))
    (if (null eas-text-ink--colors) (eas-text--item-props-1 view mark item datum color)
      (setcar k eas-text-ink--colors) (setq k (cdr k))
      (setcar k (plist-get view :id)) (setq k (cdr k))
      (setcar k (plist-get mark :id)) (setq k (cdr k))
      (setcar k datum) (setq k (cdr k))
      (setcar k color) (setq k (cdr k))
      (setcar k (plist-get item :tooltip)) (setq k (cdr k))
      ;; The ink function and its threshold (a test may rebind them).
      (setcar k (symbol-function 'eas-text-ink-legible)) (setcar (cdr k) eas-text-ink-min-contrast)
      (prog1 (or (gethash key eas-text--props-memo)
                 (progn
                   (when (>= (hash-table-count eas-text--props-memo) eas-text--props-memo-max)
                     (clrhash eas-text--props-memo))
                   (puthash (copy-sequence key) (eas-text--item-props-1 view mark item datum color)
                            eas-text--props-memo)))
        ;; Let go of the datum: the scratch key must not keep it alive.
        (setcar (nthcdr 3 key) nil) (setcar (nthcdr 5 key) nil)))))

(defun eas-text--item-props-1 (view mark item datum color)
  "Text properties for ITEM (DATUM) in MARK of VIEW drawn in COLOR, consed."
  (append (list 'eas-view (plist-get view :id) 'eas-mark (plist-get mark :id) 'eas-datum datum)
          (when-let* ((tip (eas-text--tooltip item))) (list 'help-echo tip))
          (when-let* ((ink (and color (not (equal color "none")) (eas-text-ink-legible color))))
            (list 'face (list :foreground ink)))))

(defun eas-text--xs (points)
  "The x of each of POINTS, as a vector `eas-text--interp' bisects."
  (vconcat (mapcar (lambda (p) (aref p 0)) points)))

(defun eas-text--interp (points x &optional xs)
  "Linear interpolation of the polyline POINTS ([x y] vector) at X, or nil.
XS is POINTS' `eas-text--xs', when computed once for many X."
  (let ((n (length points)))
    (when (and (> n 0) (<= (aref (aref points 0) 0) x (aref (aref points (1- n)) 0)))
      (let ((i (eas-hit--bisect (or xs (eas-text--xs points)) x)))
        (let* ((p (aref points i))
               (q (aref points (if (and (< (aref p 0) x) (< i (1- n))) (1+ i) (if (> i 0) (1- i) i)))))
          (if (= (aref p 0) (aref q 0)) (aref p 1)
            (+ (aref p 1) (* (- (aref q 1) (aref p 1)) (/ (- x (aref p 0)) (float (- (aref q 0) (aref p 0))))))))))))

(defvar eas-text--free-conses nil
  "Conses free for reuse: area slices are made of them (eas-b2s.5).
An area records a slice per cell it covers, every frame; the slices
die when the mark's cells are resolved, so they go back here.")

(defsubst eas-text--cons (a b)
  "A cons of A and B, reused from `eas-text--free-conses' when it can be."
  (let ((c eas-text--free-conses))
    (if (null c) (cons a b)
      (setq eas-text--free-conses (cdr c))
      (setcar c a) (setcdr c b)
      c)))

(defun eas-text--free-list (list)
  "Give LIST's conses back to `eas-text--free-conses'."
  (while list
    (let ((next (cdr list)))
      (setcar list nil) (setcdr list eas-text--free-conses)
      (setq eas-text--free-conses list list next))))

(defvar eas-text--column-centers nil
  "(CW COLS . CENTERS): pixel x of each column's center, for areas.")

(defun eas-text--column-centers (g)
  "A vector of the pixel x of each column center of grid G."
  (let ((cw (eas-text--grid-cw g)) (cols (eas-text--grid-cols g)) (memo eas-text--column-centers))
    (if (and memo (eql (car memo) cw) (eql (cadr memo) cols)) (cddr memo)
      (let ((v (make-vector cols nil)))
        (dotimes (col cols) (aset v col (* (+ col 0.5) cw)))
        (setq eas-text--column-centers (cons cw (cons cols v)))
        v))))

(defun eas-text--area-edge (g col row y0 y1 top bottom props prio)
  "Draw cell COL ROW of grid G (Y0 to Y1) an area slice TOP to BOTTOM cuts.
PROPS color it at PRIO.  The slice was recorded when it covers the cell."
  (let ((ch (eas-text--grid-ch g)) (covered (- (min y1 bottom) (max y0 top))))
    (when (> covered 0)
      (let ((i (+ col (* row (eas-text--grid-cols g)))) (bands (eas-text--grid-bands g)))
        (puthash i (eas-text--cons (eas-text--cons top (eas-text--cons bottom (eas-text--cons props nil)))
                                   (gethash i bands))
                 bands)))
    (cond
     ((>= covered (- ch 0.01)) (eas-text--put g col row ?█ props prio))
     ((and (> top y0) (> covered 0))
      ;; At least an eighth: a thin stacked slice still lands in a cell.
      (eas-text--put g col row (eas-glyph-lower (max 1 (round (* 8 (/ covered ch))))) props prio))
     ((>= covered (/ ch 2.0)) (eas-text--put g col row ?█ props prio))
     ;; A slice that is only a sliver against this cell's top edge (a
     ;; stacked slice at the plot's top) still marks its place.
     ((and (> covered 0) (>= top y0) (<= bottom y1)
           (< (aref (eas-text--grid-prio g) (+ col (* row (eas-text--grid-cols g)))) prio))
      (eas-text--put g col row (if (>= covered (* 0.375 ch)) ?▀ ?▔) props prio))
     ;; Above the slice beneath's eighth block, the sliver colors the
     ;; block's empty top.
     ((and (> covered 0) (>= top y0) (<= bottom y1))
      (eas-text--under g col row props prio)))))

(defun eas-text--series (g view mark item clip prio)
  "Draw line or area ITEM of MARK in VIEW into G at PRIO inside CLIP."
  (let* ((points (plist-get item :points))
         (anchors (eas-hit--anchors item))
         (axs (vconcat (mapcar (lambda (p) (aref p 0)) anchors)))
         ;; Cells of one datum share props: look them up once per datum
         ;; (eas-b2s.1: once per braille column allocated a plist each).
         (memo (make-vector (max 1 (length axs)) nil))
         (props-fn (lambda (x) (let ((k (eas-hit--bisect axs x)))
                                 (or (aref memo k)
                                     (aset memo k (eas-text--item-props view mark item (aref (plist-get item :datum) k)))))))
         (ch (eas-text--grid-ch g)))
    (if-let* ((base (plist-get item :base)))
        (cl-loop with pxs = (eas-text--xs points) with bxs = (eas-text--xs base)
                 with cxs = (eas-text--column-centers g)
                 with cols = (eas-text--grid-cols g) with bands = (eas-text--grid-bands g)
                 for col from (aref clip 0) below (aref clip 2)
                 for cx = (if (< -1 col (length cxs)) (aref cxs col) (* (+ col 0.5) (eas-text--grid-cw g)))
                 for p = (eas-text--interp points cx pxs)
                 for q = (eas-text--interp base cx bxs)
                 ;; A band (errorband, ranged area) may give its lower edge first.
                 for top = (and p q (min p q))
                 for bottom = (and p q (max p q))
                 ;; The column's props, looked up at its first cell.
                 for cp = nil
                 when (and top bottom)
                 do (cl-loop for row from (max (aref clip 1) (floor top ch)) below (min (aref clip 3) (ceiling bottom ch))
                             for y0 = (* row ch) for y1 = (* (1+ row) ch)
                             for props = (or cp (setq cp (funcall props-fn cx)))
                             do (if (and (<= top y0) (>= bottom y1))
                                    ;; The slice covers the whole cell: a full block,
                                    ;; without an edge cell's float arithmetic (eas-b2s.9).
                                    (let ((i (+ col (* row cols))))
                                      (puthash i (eas-text--cons (eas-text--cons top (eas-text--cons bottom (eas-text--cons props nil)))
                                                                 (gethash i bands))
                                               bands)
                                      (eas-text--put g col row ?█ props prio))
                                  (eas-text--area-edge g col row y0 y1 top bottom props prio))))
      (let ((show-p (and (plist-get item :strokeDash) (eas-text--dasher (plist-get item :strokeDash))))
            (eas-text--dot-prio prio))
        (dotimes (k (max 0 (1- (length points))))
          (let ((p (aref points k)) (q (aref points (1+ k))))
            (eas-text--dot-line g (aref p 0) (aref p 1) (aref q 0) (aref q 1) props-fn clip show-p))))
      (when (= (length points) 1)
        (let ((p (aref points 0)) (eas-text--dot-prio prio))
          (eas-text--dot-line g (aref p 0) (aref p 1) (aref p 0) (aref p 1) props-fn clip))))))

(defun eas-text--reach-p (segs y)
  "Non-nil when one of SEGS ((TOP BOTTOM PROPS) ...) ends at or below Y."
  (while (and segs (< (cadr (car segs)) y)) (setq segs (cdr segs)))
  segs)

(defun eas-text--start-p (segs y)
  "Non-nil when one of SEGS ((TOP BOTTOM PROPS) ...) begins at or above Y."
  (while (and segs (> (car (car segs)) y)) (setq segs (cdr segs)))
  segs)

(defun eas-text--resolve-bands (g prio)
  "Compose the cells of grid G that area slices drawn at PRIO tile.
See `eas-text-band-resolve'.  Then forget the slices."
  (let* ((ch (eas-text--grid-ch g)) (cols (eas-text--grid-cols g)) (bands (eas-text--grid-bands g))
         (slack (/ ch 4.0)))
    (unless (zerop (hash-table-count bands))
    (maphash (lambda (i segs)
               ;; A slice can be recorded past the grid's edge; it draws nothing.
               (when (and (< -1 i (length (eas-text--grid-prio g)))
                          (eas-text--writable-p (/ i cols))
                          (= (aref (eas-text--grid-prio g) i) prio))
                 (let* ((y0 (* (/ i cols) ch)) (own (car segs)))
                  (if (and (<= (car own) y0) (>= (cadr own) (+ y0 ch)))
                     ;; The newest slice covers the cell: `eas-text-band-resolve'
                     ;; gives its full block, given a second slice to tile with.
                     (when (or (cdr segs)
                               (eas-text--reach-p (gethash (- i cols) bands) (- y0 slack))
                               (eas-text--start-p (gethash (+ i cols) bands) (+ y0 ch slack)))
                       (aset (eas-text--grid-chars g) i ?█)
                       (aset (eas-text--grid-props g) i (nth 2 own)))
                 (pcase (let* ((y0 (* (/ i cols) ch))
                               (up (gethash (- i cols) bands)) (down (gethash (+ i cols) bands)))
                          ;; A slice ending just past the cell's edge meets this one.
                          ;; A lone slice that meets none tiles nothing (eas-b2s.9:
                          ;; no lists to find that out).
                          (when (or (cdr segs) (eas-text--reach-p up (- y0 slack))
                                    (eas-text--start-p down (+ y0 ch slack)))
                            (eas-text-band-resolve
                             (append segs
                                     (cl-loop for s in up when (>= (cadr s) (- y0 slack)) collect s)
                                     (cl-loop for s in down when (<= (car s) (+ y0 ch slack)) collect s))
                             y0 ch)))
                   (`(,char ,props ,under)
                    (aset (eas-text--grid-chars g) i char)
                    (aset (eas-text--grid-props g) i
                          (if (not under) props
                            (let ((bg (plist-get (plist-get under 'face) :foreground)))
                              (if (not bg) props
                                (plist-put (copy-sequence props) 'face
                                           (append (list :background bg) (plist-get props 'face)))))))))))))
             bands)
    (maphash (lambda (_ segs)
               (dolist (s segs) (eas-text--free-list s))
               (eas-text--free-list segs))
             bands)
    (clrhash bands))))

(defun eas-text--arc-dot (g dx dy props i clip arcs)
  "Record braille dot DX DY of arc item I (PROPS) of G in ARCS within CLIP.
ARCS maps a cell to (BITS . ((I COUNT . PROPS) ...)); see
`eas-text--resolve-arcs'."
  (let ((col (floor dx 2)) (row (floor dy 4)))
    (when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3))
               (< -1 col (eas-text--grid-cols g)) (< -1 row (eas-text--grid-rows g)))
      (when (and eas-text-trace eas-text-trace-item) (funcall eas-text-trace col row))
      (eas-text--touch row)
      (let* ((k (+ col (* row (eas-text--grid-cols g))))
             (cell (or (gethash k arcs) (puthash k (list 0) arcs)))
             (own (or (assq i (cdr cell)) (car (setcdr cell (cons (cons i (cons 0 props)) (cdr cell)))))))
        (setcar cell (logior (car cell) (aref (aref eas-glyph-braille-dots (mod dy 4)) (mod dx 2))))
        (cl-incf (cadr own))))))

(defun eas-text--resolve-arcs (g arcs prio clip)
  "Draw into G the cells an arc mark's wedges recorded in ARCS at PRIO.
CLIP bounds the cells.  A cell whose eight dots are all inside the arc is
a full block, the rest keep their braille dots; either takes the color
of the wedge with most dots (the later wedge on a tie), so wedges meet
without a seam."
  (let ((cols (eas-text--grid-cols g)) (eas-text--dot-prio prio))
    (maphash (lambda (k cell)
               (let* ((best (car (sort (copy-sequence (cdr cell))
                                       (lambda (a b) (or (> (cadr a) (cadr b))
                                                         (and (= (cadr a) (cadr b)) (> (car a) (car b))))))))
                      (props (cddr best)) (col (% k cols)) (row (/ k cols)))
                 (if (= (car cell) #xff)
                     (eas-text--put g col row ?█ props prio)
                   (dotimes (j 8)
                     (let ((dx (+ (* 2 col) (% j 2))) (dy (+ (* 4 row) (/ j 2))))
                       (when (/= 0 (logand (car cell) (aref (aref eas-glyph-braille-dots (mod dy 4)) (mod dx 2))))
                         (eas-text--dot g dx dy props clip)))))))
             arcs)))

(defun eas-text--under (g col row props prio)
  "Give the lower eighth block at COL ROW of grid G a PROPS background.
The block, drawn at PRIO, takes the color of PROPS as its background:
the slice stacked on it shows in the block's empty top.  Nothing when the
cell holds anything else."
  (let* ((i (+ col (* row (eas-text--grid-cols g))))
         (old (aref (eas-text--grid-props g) i))
         (face (plist-get old 'face))
         (color (plist-get (plist-get props 'face) :foreground)))
    (when (and color (eas-text--touch row) (= (aref (eas-text--grid-prio g) i) prio)
               (memq (aref (eas-text--grid-chars g) i) (cdr (butlast (append eas-glyph-blocks nil)))))
      (when (and eas-text-trace eas-text-trace-item) (funcall eas-text-trace col row))
      (aset (eas-text--grid-props g) i
            (plist-put (copy-sequence old) 'face (append (list :background color) face))))))

(defun eas-text--vglyph (a b)
  "Glyph for a fill covering A..B (fractions of a cell from its top)."
  (let ((d (- b a)))
    (cond ((>= d 0.94) ?█)
          ((>= b 0.94) (eas-glyph-lower (max 1 (round (* 8 d)))))
          ((<= a 0.06) (if (>= d 0.375) ?▀ ?▔))
          ((>= d 0.5) ?█)
          (t ?━))))

(defun eas-text--hglyph (a b)
  "Glyph for a fill covering A..B (fractions of a cell from its left)."
  (let ((d (- b a)))
    (cond ((>= d 0.94) ?█)
          ((<= a 0.06) (eas-glyph-left (max 1 (round (* 8 d)))))
          ((>= b 0.94) (if (>= d 0.375) ?▐ ?▕))
          ((>= d 0.5) ?█)
          (t ?┃))))

(defun eas-text--coverage (char)
  "Rank of fill glyph CHAR where two bars of one mark share a cell.
A full block ranks first, then eighth blocks by size (they read to an
eighth), then the coarse half and edge glyphs.  Stacked segments meet on
the lower segment's eighth block, as stacked areas do."
  (cond ((memq char '(?█ ?▓ ?▒ ?░)) 2.0)
        ((seq-position eas-glyph-blocks char) (+ 1 (/ (seq-position eas-glyph-blocks char) 8.0)))
        ((seq-position eas-glyph-left-blocks char) (+ 1 (/ (seq-position eas-glyph-left-blocks char) 8.0)))
        ((memq char '(?▀ ?▐)) 0.5)
        (t 0.125)))

(defun eas-text--falling (char)
  "CHAR as a falling ranged bar draws it.
Shaded where at least 3/8 of the cell is covered, its thin ends as they
are."
  (if (memq char '(?█ ?▀ ?▐ ?▄ ?▅ ?▆ ?▇ ?▌ ?▋ ?▊ ?▉)) ?▒ char))

(defun eas-text--translucent (char alpha)
  "CHAR as a fill of opacity ALPHA (below 1) draws it.
A full block becomes a shade block as light as ALPHA: light (░) to a
quarter, medium (▒) to a half, dark (▓) to three quarters; a fill more
opaque than that stays solid.  Eighth and half blocks keep their shape,
which carries the bar's end."
  (if (and alpha (<= alpha 0.75) (eq char ?█))
      (cond ((<= alpha 0.25) ?░) ((<= alpha 0.5) ?▒) (t ?▓))
    char))

(defun eas-text--cells (p len size)
  "(FIRST . END) cells of SIZE pixels a span from P of LEN pixels rounds to.
A span narrower than a cell takes the cell holding its middle."
  (let ((a (round p size)) (b (round (+ p len) size)))
    (if (> b a) (cons a b)
      (let ((m (floor (+ p (/ len 2.0)) size))) (cons m (1+ m))))))

(defun eas-text--zero (view channel)
  "Pixel of zero on VIEW's continuous CHANNEL scale, when 0 is in its domain."
  (let* ((scale (plist-get (plist-get view :scales) channel)) (d (plist-get scale :domain)))
    (when (and (member (plist-get scale :type) '("linear" "pow" "sqrt" "symlog")) (vectorp d) (= (length d) 2)
               (numberp (aref d 0)) (numberp (aref d 1)) (<= (min (aref d 0) (aref d 1)) 0 (max (aref d 0) (aref d 1))))
      (eas-scale-apply scale 0))))

(defun eas-text--rect (g view mark item clip prio)
  "Draw bar, rect or brush ITEM of MARK in VIEW into G at PRIO inside CLIP.
A bar's ends take eighth or half blocks in the direction it grows, so
its baseline and value land on their own cells.  A ranged bar whose
second value lies below its first (:rise :false, a falling candle) is
shaded, a rising one solid; color tells them apart too.  A bar or
rect less than opaque shades its full cells (`eas-text--translucent')."
  (let* ((cw (eas-text--grid-cw g)) (ch (eas-text--grid-ch g))
         (x (plist-get item :x)) (y (plist-get item :y)) (w (plist-get item :w)) (h (plist-get item :h))
         (orient (plist-get item :orient))
         (brush (equal (plist-get mark :mark) "brush"))
         (props (if brush (list 'eas-brush (plist-get mark :param))
                  (eas-text--item-props view mark item (plist-get item :datum))))
         (vertical (equal orient "vertical")) (horizontal (equal orient "horizontal"))
         (falling (eq (plist-get item :rise) :false))
         ;; A see-through fill (opacity < 1) shades its full cells.
         (alpha (and (not brush) (* (or (plist-get item :opacity) 1) (or (plist-get item :fillOpacity) 1))))
         ;; The end standing on the scale's zero (the baseline) fills its
         ;; cell; only the value end shows a partial block.
         (zero (and (or vertical horizontal) (not (plist-get item :rise))
                    (eas-text--zero view (if vertical :y :x))))
         (lo (if vertical y x)) (hi (+ lo (if vertical h w)))
         ;; Only the end nearer zero snaps: a bar under half a pixel tall
         ;; has both ends near it, and is no full cell.
         (snap-lo (and zero (< (abs (- zero lo)) 0.5) (< (abs (- zero lo)) (abs (- zero hi)))))
         (snap-hi (and zero (< (abs (- zero hi)) 0.5) (<= (abs (- zero hi)) (abs (- zero lo)))))
         (cols (if horizontal (cons (floor x cw) (ceiling (- (+ x w) 0.001) cw)) (eas-text--cells x w cw)))
         (rows (if vertical (cons (floor y ch) (ceiling (- (+ y h) 0.001) ch)) (eas-text--cells y h ch)))
         (c0 (car cols)) (c1 (max (1+ c0) (cdr cols)))
         (r0 (car rows)) (r1 (max (1+ r0) (cdr rows)))
         ;; A ranged bar straddling two cells, under half of each: keep the
         ;; better covered one so the body never vanishes.
         (body-row (and vertical (plist-get item :rise) (> (- r1 r0) 1)
                        (let ((covers (cl-loop for row from r0 below r1
                                               collect (cons row (- (min 1.0 (/ (- (+ y h) (* row ch)) (float ch)))
                                                                    (max 0.0 (/ (- y (* row ch)) (float ch))))))))
                          (unless (seq-some (lambda (c) (>= (cdr c) 0.5)) covers)
                            (car (car (sort covers (lambda (a b) (> (cdr a) (cdr b)))))))))))
    ;; A zero-length bar (a value of 0) draws nothing, as in SVG, and an
    ;; empty interval (a click, no drag) no brush.
    (unless (or (and vertical (< h 0.01)) (and horizontal (< w 0.01))
                (and brush (or (< w 0.5) (< h 0.5))))
     (cl-loop with x2 = (+ x w) with y2 = (+ y h)
             with gcols = (eas-text--grid-cols g) with grows = (eas-text--grid-rows g)
             with vchars = (eas-text--grid-chars g) with vprops = (eas-text--grid-props g)
             with vprio = (eas-text--grid-prio g) with vcover = (eas-text--grid-cover g)
             for row from (max r0 (aref clip 1)) below (min r1 (aref clip 3))
             do (cl-loop for col from (max c0 (aref clip 0)) below (min c1 (aref clip 2))
                         for char = (cond
                                     (brush nil)
                                     ;; A cell the bar covers whole is a full
                                     ;; block, whatever the end rules below say;
                                     ;; this skips their float arithmetic.
                                     ((and vertical (not (plist-get item :rise))
                                           (<= y (* row ch)) (<= (* (1+ row) ch) y2))
                                      ?█)
                                     ((and horizontal (<= x (* col cw)) (<= (* (1+ col) cw) x2)) ?█)
                                     ;; A ranged bar (a candle body) taller than a cell
                                     ;; snaps to whole cells: an end cell it fills at
                                     ;; least half of is full, a lesser one is left to
                                     ;; the wick under it, which would otherwise break
                                     ;; there behind a mostly empty eighth block.
                                     ((and vertical (plist-get item :rise) (> (- r1 r0) 1))
                                      (let ((cover (- (min 1.0 (/ (- (+ y h) (* row ch)) (float ch)))
                                                      (max 0.0 (/ (- y (* row ch)) (float ch))))))
                                        (and (or (>= cover 0.5) (eql row body-row)) ?█)))
                                     (vertical (eas-text--vglyph (if snap-lo 0.0 (max 0.0 (/ (- y (* row ch)) (float ch))))
                                                                 (if snap-hi 1.0 (min 1.0 (/ (- (+ y h) (* row ch)) (float ch))))))
                                     (horizontal (eas-text--hglyph (if snap-lo 0.0 (max 0.0 (/ (- x (* col cw)) (float cw))))
                                                                   (if snap-hi 1.0 (min 1.0 (/ (- (+ x w) (* col cw)) (float cw))))))
                                     (t ?█))
                         for glyph = (cond ((and char falling) (eas-text--falling char))
                                           (char (eas-text--translucent char alpha)))
                         do (cond
                             ((and glyph eas-text-trace) (eas-text--put g col row glyph props prio (eas-text--coverage glyph)))
                             ;; `eas-text--put', inline: a long bar puts thousands of cells.
                             (glyph
                              (when (and (< -1 col gcols) (< -1 row grows) (eas-text--touch row))
                                (let* ((i (+ col (* row gcols))) (old (aref vprio i)) (cover (eas-text--coverage glyph)))
                                  (when (and (>= prio old) (or (> prio old) (> cover (aref vcover i))))
                                    (aset vchars i glyph) (aset vprops i props) (aset vprio i prio) (aset vcover i cover)))))
                             ;; The brush shades its cells whatever they hold
                             ;; when the grid is composed: one solid region.
                             (brush
                              (when (and (< -1 col (eas-text--grid-cols g)) (< -1 row (eas-text--grid-rows g))
                                         (eas-text--touch row))
                                (aset (eas-text--grid-brush g) (+ col (* row (eas-text--grid-cols g))) props)))))))))

(defun eas-text--brush-props (props brush shade)
  "PROPS of a cell under BRUSH (its props), shaded with color SHADE.
The glyph's color is made legible on SHADE, the background it now has."
  (let ((face (plist-get props 'face)))
    (append brush
            (plist-put (copy-sequence props) 'face
                       (cond ((null face) (list :background shade))
                             ((keywordp (car-safe face)) (eas-text--shade-face face shade))
                             ((consp face)
                              (cons (list :background shade)
                                    (mapcar (lambda (f) (if (keywordp (car-safe f)) (eas-text--shade-face f shade) f))
                                            face)))
                             (t (list (list :background shade) face)))))))

(defun eas-text--shade-face (face shade)
  "Face plist FACE on the brush's SHADE, its foreground legible there."
  (let ((fg (plist-get face :foreground)))
    (append (list :background shade)
            (if (stringp fg) (plist-put (copy-sequence face) :foreground (eas-text-ink-legible-on fg shade)) face))))

(defun eas-text--segment (g seg props clip prio)
  "Draw segment SEG [x1 y1 x2 y2] into G with box glyphs.
Braille draws it when diagonal.  PROPS, CLIP and PRIO apply to its cells."
  (let ((x1 (aref seg 0)) (y1 (aref seg 1)) (x2 (aref seg 2)) (y2 (aref seg 3)))
    (cond
     ((< (abs (- x1 x2)) 0.5)
      (let ((col (eas-text--col g x1)))
        (cl-loop for row from (eas-text--row g (min y1 y2)) to (eas-text--row g (- (max y1 y2) 0.01))
                 when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3)))
                 do (eas-text--put g col row ?│ props prio))))
     ((< (abs (- y1 y2)) 0.5)
      (let ((row (eas-text--row g y1)))
        (cl-loop for col from (eas-text--col g (min x1 x2)) to (eas-text--col g (- (max x1 x2) 0.01))
                 when (and (<= (aref clip 0) col) (< col (aref clip 2)) (<= (aref clip 1) row) (< row (aref clip 3)))
                 do (eas-text--put g col row ?─ props prio))))
     (t (let ((eas-text--dot-prio prio)) (eas-text--dot-line g x1 y1 x2 y2 nil clip nil props))))))

(defun eas-text--inside (bounds x)
  "X, nudged inside the plot when it lies on BOUNDS' right edge."
  (if (< (abs (- x (+ (aref bounds 0) (aref bounds 2)))) 0.005) (- x 0.01) x))

(defconst eas-text--fills '("bar" "rect" "arc" "area" "brush" "geoshape")
  "Marks that fill a region; strokes drawn before one sit under it.")

(defun eas-text--translucent-p (mark)
  "Non-nil when every item of MARK is mostly see-through.
One opaque item (a hovered series' label) keeps the mark on top."
  (let ((items (plist-get mark :items)))
    (and (> (length items) 0)
         (seq-every-p (lambda (item) (< (* (or (plist-get item :opacity) 1) (or (plist-get item :fillOpacity) 1)) 0.5))
                      items))))

(defun eas-text--mark-prios (view)
  "Cell priority of each mark of VIEW, a list in mark order.
A cell holds one glyph, so marks follow Vega's painter's order within
three tiers: fills (1), strokes (2: rules, ticks, lines) and symbols
\(3: points, text, images).  A stroke drawn before an opaque fill sits
under it (a candle's wick under its body); a mostly see-through symbol
sits under strokes.  Later marks win ties."
  (let* ((marks (append (plist-get view :marks) nil)) (k -1))
    (cl-loop for (mark . later) on marks
             for type = (plist-get mark :mark)
             do (cl-incf k)
             collect (+ (min k 99) 0.0
                        (* 100 (cond ((member type eas-text--fills) 1)
                                     ((member type '("rule" "tick" "line" "trail"))
                                      (if (seq-some (lambda (m) (and (member (plist-get m :mark) eas-text--fills)
                                                                     (not (eas-text--translucent-p m))))
                                                    later)
                                          1 2))
                                     ((eas-text--translucent-p mark) 1.5)
                                     (t 3)))))))

(declare-function eas-geoshape-text "eas-geoshape-render")

(defun eas-text--clip (g view)
  "The cells [COL0 ROW0 COL1 ROW1) of VIEW's plot in grid G."
  (let* ((b (plist-get view :bounds))
         (clip (vector (eas-text--col g (aref b 0)) (eas-text--row g (aref b 1))
                       (eas-text--col g (+ (aref b 0) (aref b 2) -0.01)) (eas-text--row g (+ (aref b 1) (aref b 3) -0.01)))))
    (vector (aref clip 0) (aref clip 1) (1+ (aref clip 2)) (1+ (aref clip 3)))))

(defun eas-text--marks (g view)
  "Draw every mark of VIEW into grid G, clipped to its plot."
  (let ((prios (mapcar (lambda (p) (/ p 100.0)) (eas-text--mark-prios view)))
        (clip (eas-text--clip g view))
        (eas-text-tile--cells (eas-text-tile-table)))
    (seq-doseq (mark (plist-get view :marks))
      (eas-text--mark g view mark (pop prios) clip))
    (eas-text-tile-resolve g)
    (eas-text-tile-release eas-text-tile--cells)))

(defun eas-text--tiled-p (view)
  "Non-nil when some rect of VIEW draws as a tile (`eas-text-tile-p').
Tiles share one table across the view's marks and resolve after the
last of them, so such a view paints its marks as one step."
  (seq-some (lambda (mark)
              (and (member (plist-get mark :mark) '("bar" "rect" "brush"))
                   (seq-some (lambda (item) (eas-text-tile-p mark item)) (plist-get mark :items))))
            (plist-get view :marks)))

(defvar eas-text--infos nil
  "Per mark of the step being painted, in order: [ITEMS RANGES SPARE].
ITEMS are the mark's items when RANGES, a vector, were recorded: item
by item the rows it tried, packed by `eas-text--range', or nil.  SPARE
is a vector to record into.  `eas-text--mark' pops one per mark.")

(defsubst eas-text--range ()
  "The rows the item just drawn tried, packed in a fixnum, or nil."
  (and (>= eas-text--hi 0) (+ (* eas-text--lo 65536) eas-text--hi)))

(defun eas-text--meets-p (range mask wide)
  "Non-nil when packed RANGE, a row more each way when WIDE, meets MASK."
  (and range
       (let* ((n (length mask))
              (lo (max 0 (- (/ range 65536) (if wide 1 0))))
              (hi (min (1- n) (+ (% range 65536) (if wide 1 0)))))
         (while (and (<= lo hi) (not (aref mask lo))) (setq lo (1+ lo)))
         (<= lo hi))))

(defun eas-text--wide-p (mark)
  "Non-nil when an item of MARK depends on its neighbour rows (area slices)."
  (member (plist-get mark :mark) '("line" "area" "trail")))

(defun eas-text--mark (g view mark prio clip)
  "Draw MARK of VIEW into grid G at PRIO, clipped to CLIP cells.
In an incremental frame (`eas-text--mask') an item the last frame drew
the same, whose rows miss the mask, is not drawn again."
  (let* ((b (plist-get view :bounds))
         (info (pop eas-text--infos))
         (items (plist-get mark :items)) (n (length items))
         (old-items (and info eas-text--mask (aref info 0))) (old-ranges (and info (aref info 1)))
         (ranges (and info (let ((v (aref info 2))) (if (and v (= (length v) n)) v (make-vector n nil)))))
         (wide (eas-text--wide-p mark)))
     (let ((arcs (and (equal (plist-get mark :mark) "arc") (make-hash-table :test 'eql))))
      (seq-do-indexed
       (lambda (item i)
        (if (and old-items (< i (length old-items)) (eq item (aref old-items i))
                 (not (eas-text--meets-p (aref old-ranges i) eas-text--mask wide)))
            (aset ranges i (aref old-ranges i))
         (setq eas-text--lo most-positive-fixnum eas-text--hi -1)
         (unless (equal (plist-get item :opacity) 0)
          (let ((eas-text-trace-item (and eas-text-trace (list (plist-get view :id) (plist-get mark :id) i))))
           (pcase (plist-get mark :mark)
             ((or "line" "area" "trail") (eas-text--series g view mark item clip prio))
             ((or "bar" "rect" "brush") (if (eas-text-tile-p mark item) (eas-text-tile-draw g view mark item clip prio)
                                          (eas-text--rect g view mark item clip prio)))
             ("geoshape" (eas-geoshape-text g view mark item clip prio))
             ("arc" (let ((props (eas-text--item-props view mark item (plist-get item :datum))))
                      (eas-text-arc-dots item (eas-text--grid-cw g) (eas-text--grid-ch g)
                                         (lambda (dx dy) (eas-text--arc-dot g dx dy props i clip arcs)))))
             ((or "rule" "tick")
              ;; A rule on the plot's right edge (the last datum's crosshair)
              ;; belongs to the last column, not the clipped one past it.
              (eas-text--segment g (if (equal (plist-get mark :mark) "rule")
                                         (vector (eas-text--inside b (plist-get item :x1)) (plist-get item :y1)
                                                 (eas-text--inside b (plist-get item :x2)) (plist-get item :y2))
                                       (vector (plist-get item :x1) (plist-get item :y1) (plist-get item :x2) (plist-get item :y2)))
                                   (eas-text--item-props view mark item (plist-get item :datum)) clip prio))
             ("image" (if-let* ((cells (and (fboundp 'eas-contour-text-image-cells)
                                            (eas-contour-text-image-cells
                                             (plist-get item :url) (plist-get item :x) (plist-get item :y)
                                             (plist-get item :w) (plist-get item :h)
                                             (eas-text--grid-cw g) (eas-text--grid-ch g)))))
                          ;; PNG data (a heatmap): each cell in the color under it.
                          (pcase-dolist (`(,col ,row ,color ,alpha) cells)
                            (when (and (>= alpha 0.125) (<= (aref clip 0) col) (< col (aref clip 2))
                                       (<= (aref clip 1) row) (< row (aref clip 3)))
                              (eas-text--put g col row (eas-text--translucent ?█ alpha)
                                             (eas-text--item-props view mark (plist-put (copy-sequence item) :fill color)
                                                                   (plist-get item :datum))
                                             prio)))
                        (eas-text--put g (eas-text--col g (+ (plist-get item :x) (/ (plist-get item :w) 2.0)))
                                       (eas-text--row g (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))
                                       eas-marks-image-glyph
                                       (eas-text--item-props view mark item (plist-get item :datum)) prio)))
             ("text" (let ((eas-text--clamp-rows t))
                      (eas-text--string g (plist-get item :x) (plist-get item :y) (plist-get item :text)
                                         (plist-get item :align)
                                         (eas-text--item-props view mark item (plist-get item :datum)) prio)))
             (_ (let* ((filled (not (equal (plist-get item :fill) "none")))
                       ;; A point on the plot's far edge belongs to the last cell.
                       (col (min (eas-text--col g (plist-get item :x)) (1- (aref clip 2))))
                       (row (min (eas-text--row g (plist-get item :y)) (1- (aref clip 3))))
                       ;; A path shape (geopath's) covers the cells it holds.
                       (cells (and (fboundp 'eas-contour-text-path-cells)
                                   (eas-contour-text-path-cells (plist-get item :shape) (plist-get item :x) (plist-get item :y)
                                                                (or (plist-get item :size) 0)
                                                                (eas-text--grid-cw g) (eas-text--grid-ch g) (not filled)))))
                  (if cells
                      (let ((glyph (if filled (eas-text--translucent ?█ (* (or (plist-get item :opacity) 1)
                                                                           (or (plist-get item :fillOpacity) 1)))
                                     ?·))
                            (props (eas-text--item-props view mark item (plist-get item :datum))))
                        (pcase-dolist (`(,c . ,r) (and (consp cells) cells))
                          (when (and (<= (aref clip 0) c) (< c (aref clip 2)) (<= (aref clip 1) r) (< r (aref clip 3)))
                            (eas-text--put g c r glyph props prio))))
                  (when (and (<= (aref clip 0) col) (<= (aref clip 1) row))
                    (eas-text--put g col row
                                     (eas-symbols-glyph (plist-get item :shape) filled (plist-get item :angle))
                                     (eas-text--item-props view mark item (plist-get item :datum)) prio))))))))
         (when ranges (aset ranges i (eas-text--range)))))
       items)
      (when arcs (eas-text--resolve-arcs g arcs prio clip))
      (eas-text--resolve-bands g prio))
     (when info
       ;; What this frame drew becomes the last frame; the old ranges are spare.
       (aset info 2 (and (not (eq old-ranges ranges)) old-ranges))
       (aset info 1 ranges) (aset info 0 items))))

(defvar eas-text--label-cells nil
  "Hash of (COL . ROW) cells holding a tick label, while a scene renders.")

(defvar eas-text--label-log :off
  "While a step paints in full: its label decisions so far, latest first.
It is :off when nobody keeps them.")

(defvar eas-text--label-replay nil
  "While a step repaints some rows: (DECISIONS . NEXT), its logged decisions.
A step repainting a few rows sees none of the claims of the labels
before it, so it replays what it decided when it painted in full.")

(defun eas-text--label-room-p (g tk)
  "Non-nil when tick TK's label fits beside the labels already placed in G.
It then claims its cells.  A label that would overwrite another one is
left out, as Vega's labelOverlap drops it, so no label is garbled."
  (if eas-text--label-replay
      (let ((r eas-text--label-replay))
        (prog1 (aref (car r) (cdr r)) (setcdr r (1+ (cdr r)))))
    (let ((room (eas-text--label-room-1 g tk)))
      (unless (eq eas-text--label-log :off) (push room eas-text--label-log))
      room)))

(defun eas-text--label-room-1 (g tk)
  "Non-nil when tick TK's label fits in G, claiming its cells; see above."
  (let ((label (plist-get tk :label)))
    (or (not (stringp label)) (string-empty-p label) (null eas-text--label-cells)
        (let* ((lines (split-string label "\n"))
               (spans (seq-map-indexed
                       (lambda (line i)
                         (eas-text-string-span (eas-text--grid-cols g) (eas-text--grid-cw g) (eas-text--grid-ch g)
                                               (plist-get tk :lx) (+ (plist-get tk :ly) (* i (eas-text--grid-ch g)))
                                               line (plist-get tk :align) (eas-text--grid-rows g)))
                       lines))
               (cells (cl-loop for (row c0 . c1) in spans
                               append (cl-loop for c from c0 below c1 collect (cons c row)))))
          (unless (seq-some (lambda (c) (gethash c eas-text--label-cells)) cells)
            (dolist (c cells) (puthash c t eas-text--label-cells))
            t)))))

(defun eas-text--axes (g view)
  "Draw VIEW's axes and grid into G."
  (let ((all (vector 0 0 (eas-text--grid-cols g) (eas-text--grid-rows g)))
        (axis-props (list 'face 'eas-axis)) left bottom)
    (seq-doseq (axis (plist-get view :axes))
      (seq-doseq (tk (plist-get axis :ticks))
        (when-let* ((grid (plist-get tk :grid)))
          (eas-text--segment g grid (list 'face 'eas-axis) all 0)
          ;; Grid lines are dotted so marks stay legible.  Earlier grid
          ;; lines are dotted already, so only this one's cells can
          ;; hold a solid line at priority 0 (eas-b2s.1: the whole grid
          ;; was scanned per tick).
          (let ((vertical (< (abs (- (aref grid 0) (aref grid 2))) 0.5))
                (cols (eas-text--grid-cols g)) (rows (eas-text--grid-rows g)))
            (cl-loop for row from (max 0 (1- (eas-text--row g (min (aref grid 1) (aref grid 3)))))
                     to (min (1- rows) (1+ (eas-text--row g (max (aref grid 1) (aref grid 3)))))
                     do (cl-loop for col from (max 0 (1- (eas-text--col g (min (aref grid 0) (aref grid 2)))))
                                 to (min (1- cols) (1+ (eas-text--col g (max (aref grid 0) (aref grid 2)))))
                                 for k = (+ col (* row cols))
                                 when (and (eas-text--touch row) (= (aref (eas-text--grid-prio g) k) 0)
                                           (memq (aref (eas-text--grid-chars g) k) '(?│ ?─)))
                                 do (aset (eas-text--grid-chars g) k (if vertical ?┊ ?┈))))))))
    (seq-doseq (axis (plist-get view :axes))
      (let* ((seg (plist-get axis :domain-line)) (orient (plist-get axis :orient))
             (is-bottom (member orient '("bottom" "top"))))
        (unless (or (plist-get axis :domain-off) (plist-get axis :no-domain))
          (when seg (eas-text--segment g seg axis-props all 4))
          (pcase orient ("bottom" (setq bottom seg)) ("left" (setq left seg))))
        (seq-doseq (tk (plist-get axis :ticks))
          (let ((ts (plist-get tk :tick)))
            (when (and ts (eas-axis-extra-tick-color axis tk t))
              (eas-text--put g (eas-text--col g (aref ts (if is-bottom 0 2))) (eas-text--row g (aref ts (if is-bottom 1 3)))
                             (pcase orient ("bottom" ?┬) ("top" ?┴) ("right" ?├) (_ ?┤)) axis-props 4)))
          (when (eas-text--label-room-p g tk)
            (let ((eas-text--clamp-rows t))
             (eas-text--string g (plist-get tk :lx) (plist-get tk :ly) (plist-get tk :label) (plist-get tk :align)
                              (list 'face 'eas-label) 5))))
        (when-let* ((tm (plist-get axis :title-mark)))
          (eas-text--string g (plist-get tm :x) (plist-get tm :y) (plist-get tm :text) (plist-get tm :align)
                              (list 'face 'eas-title) 5))))
    (when (and left bottom)
      (eas-text--put g (eas-text--col g (aref left 0)) (eas-text--row g (aref bottom 1)) ?└ axis-props 4))))

(defun eas-text--dash-glyph (dash)
  "A box-drawing glyph suggesting stroke DASH (a dash-gap vector)."
  (let ((on (aref dash 0)) (off (if (> (length dash) 1) (aref dash 1) 0)))
    (cond ((zerop off) ?━) ((> (length dash) 2) ?┄) ((>= on 4) ?╍) ((>= on 2) ?┅) (t ?┉))))

(defun eas-text--legends (g view)
  "Draw VIEW's legends into G."
  (seq-doseq (legend (plist-get view :legends))
    (when-let* ((tm (plist-get legend :title-mark)))
      (eas-text--string g (plist-get tm :x) (plist-get tm :y) (plist-get tm :text) "left" (list 'face 'eas-title) 5))
    (when-let* ((bar (plist-get legend :bar)))
      (pcase-dolist (`(,col ,row ,char ,fg ,bg)
                     (eas-text-ramp-cells bar (plist-get legend :stops) (eas-text--grid-cw g) (eas-text--grid-ch g)
                                          ;; Text lays every bar out upright, whatever its direction.
                                          (> (aref bar 2) (aref bar 3))))
        (eas-text--put g col row char
                       (list 'eas-view (plist-get view :id) 'eas-legend-ramp (plist-get legend :channel)
                             'face (list :foreground (eas-text-ink-legible fg) :background (eas-text-ink-legible bg)))
                       5)))
    (seq-doseq (e (plist-get legend :entries))
      (let ((props (list 'eas-view (plist-get view :id) 'eas-legend (plist-get e :value)
                         'help-echo (or (plist-get e :full) (plist-get e :label)))))
        (when (plist-get e :color)
          (eas-text--put g (eas-text--col g (plist-get e :sx)) (eas-text--row g (plist-get e :sy))
                           (cond ((plist-get e :dash) (eas-text--dash-glyph (plist-get e :dash)))
                                 ((plist-get e :shape) (eas-symbols-glyph (plist-get e :shape) t))
                                 (t (pcase (plist-get legend :shape) ("square" ?■) ("stroke" ?━) (_ ?●))))
                           (append props (list 'face (list :foreground (eas-text-ink-legible (plist-get e :color))))) 5))
        (eas-text--string g (plist-get e :lx) (plist-get e :ly) (plist-get e :label) "left"
                            (append props (list 'face 'eas-label)) 5)))))

(defvar eas-text--row-scratch (make-vector 0 nil)
  "Chars of the row being composed, reused (eas-b2s.5).")

(defvar eas-text--row-vectors (make-vector 0 nil)
  "A reused vector of each length N, to `concat' a row of N chars from.")

(defvar eas-text--reversed (make-hash-table :test 'eq :weakness 'key)
  "Props -> `eas-text--plist-reverse' of them; interned props hit.")

(defun eas-text--row-vector (n)
  "A vector of length N reused across rows."
  (when (>= n (length eas-text--row-vectors))
    (setq eas-text--row-vectors (vconcat eas-text--row-vectors (make-vector (- (1+ n) (length eas-text--row-vectors)) nil))))
  (or (aref eas-text--row-vectors n) (aset eas-text--row-vectors n (make-vector n nil))))

(defun eas-text--cell-props (g i shade)
  "The props cell I of grid G composes with; SHADE colors a brush."
  (let* ((use-dots (and (> (aref (eas-text--grid-dots g) i) 0)
                        (<= (aref (eas-text--grid-prio g) i) (aref (eas-text--grid-dot-prio g) i))))
         (p (if use-dots (aref (eas-text--grid-dot-props g) i) (aref (eas-text--grid-props g) i))))
    (if-let* ((brush (aref (eas-text--grid-brush g) i))) (eas-text--brush-props p brush shade) p)))

(defun eas-text--compose-row (g row shade)
  "Return ROW of grid G as a propertized string; SHADE colors a brush.
One string per row; each run of equal props is set on it, in the order
`concat' would leave them (eas-b2s.1).  The chars go through reused
vectors and runs are set as they end, so a row allocates its string
and its intervals only (eas-b2s.5)."
  (let* ((cols (eas-text--grid-cols g)) (base (* row cols))
         (chars (eas-text--grid-chars g)) (dots (eas-text--grid-dots g))
         (prio (eas-text--grid-prio g)) (dot-prio (eas-text--grid-dot-prio g))
         (brush (eas-text--grid-brush g))
         ;; Blank cells past the last glyph are trimmed, unless brushed.
         (end (let ((col (1- cols)))
                (while (and (>= col 0)
                            (let ((i (+ base col)))
                              (and (eq (aref chars i) ?\s) (null (aref brush i))
                                   (or (zerop (aref dots i)) (> (aref prio i) (aref dot-prio i))))))
                  (setq col (1- col)))
                (1+ col)))
         (scratch (if (>= (length eas-text--row-scratch) cols) eas-text--row-scratch
                    (setq eas-text--row-scratch (make-vector cols nil))))
         (n 0))
    (dotimes (col end)
      (let* ((i (+ base col)) (d (aref dots i))
             (char (if (and (> d 0) (<= (aref prio i) (aref dot-prio i))) (+ #x2800 d) (aref chars i))))
        (unless (eq char 0) (aset scratch n char) (setq n (1+ n)))))
    (let ((string (let ((v (eas-text--row-vector n)))
                    (dotimes (k n) (aset v k (aref scratch k)))
                    (concat v)))
          (start 0) (k 0) (props :unset))
      (dotimes (col end)
        (let* ((i (+ base col)) (p (eas-text--cell-props g i shade)))
          (unless (or (eq p props) (equal p props))
            (when (> k start) (eas-text--set-run string start k props))
            (setq start k props p))
          (unless (and (eq (aref chars i) 0) (not (and (> (aref dots i) 0) (<= (aref prio i) (aref dot-prio i)))))
            (setq k (1+ k)))))
      (when (> k start) (eas-text--set-run string start k props))
      string)))

(cl-defstruct (eas-text-row (:constructor eas-text--row-make (grid row shade strings)) (:copier nil))
  "A row of a cached text frame left uncomposed (eas-b2s.9).
The terminal glue writes it into its buffer cell by cell
\(`eas-text-row-scan'); `eas-text-row-string' composes it.  STRINGS is
the frame's row vector, where it stands until then."
  grid row shade strings)

(defvar eas-text--defer-rows nil
  "Non-nil: cached frames leave repainted rows as `eas-text-row' records.")

(defun eas-text--row-out (g row shade strings)
  "Row ROW of grid G (SHADE colors a brush) for the frame's row vector STRINGS.
A string, or a record when `eas-text--defer-rows'."
  (if eas-text--defer-rows (eas-text--row-make g row shade strings) (eas-text--compose-row g row shade)))

(defun eas-text-row-string (row)
  "ROW as a propertized string: ROW itself, or its record composed.
A record of the frame's row vector is replaced there by its string, so
the next frame that keeps the row returns the same string."
  (if (not (eas-text-row-p row)) row
    (let ((string (eas-text--compose-row (eas-text-row-grid row) (eas-text-row-row row) (eas-text-row-shade row)))
          (strings (eas-text-row-strings row)))
      (when (and strings (eq (aref strings (eas-text-row-row row)) row))
        (aset strings (eas-text-row-row row) string))
      string)))

(defun eas-text-row-cols (row)
  "Most cells record ROW has."
  (eas-text--grid-cols (eas-text-row-grid row)))

(defun eas-text-row-scan (row fn)
  "Call FN with J, CHAR and PROPS for each character J of record ROW.
That is the string `eas-text-row-string' would give, without making
it: PROPS is the plist character J's text properties `equal'.  Return
the number of characters."
  (let* ((g (eas-text-row-grid row)) (shade (eas-text-row-shade row))
         (cols (eas-text--grid-cols g)) (base (* (eas-text-row-row row) cols))
         (chars (eas-text--grid-chars g)) (dots (eas-text--grid-dots g))
         (prio (eas-text--grid-prio g)) (dot-prio (eas-text--grid-dot-prio g))
         (brush (eas-text--grid-brush g))
         (end (let ((col (1- cols)))
                (while (and (>= col 0)
                            (let ((i (+ base col)))
                              (and (eq (aref chars i) ?\s) (null (aref brush i))
                                   (or (zerop (aref dots i)) (> (aref prio i) (aref dot-prio i))))))
                  (setq col (1- col)))
                (1+ col)))
         (j 0))
    (dotimes (col end)
      (let* ((i (+ base col)) (d (aref dots i))
             (char (if (and (> d 0) (<= (aref prio i) (aref dot-prio i))) (+ #x2800 d) (aref chars i))))
        (unless (eq char 0)
          (let ((p (eas-text--cell-props g i shade)))
            (funcall fn j char (and p (or (gethash p eas-text--reversed)
                                          (puthash p (eas-text--plist-reverse p) eas-text--reversed)))))
          (setq j (1+ j)))))
    j))

(defun eas-text--set-run (string start end props)
  "Set PROPS on STRING from START to END, in `concat''s order."
  (unless (or (null props) (eq props :unset))
    (set-text-properties start end
                         (or (gethash props eas-text--reversed)
                             (puthash props (eas-text--plist-reverse props) eas-text--reversed))
                         string)))

(defun eas-text--plist-reverse (plist)
  "PLIST with its property-value pairs in reverse order."
  (let (out)
    (while plist
      (setq out (cons (car plist) (cons (cadr plist) out)) plist (cddr plist)))
    out))

(defun eas-text-render (scene)
  "Return SCENE drawn as a propertized string (rows joined by newlines)."
  (mapconcat #'identity (eas-text-render-lines scene) "\n"))

(defun eas-text-render-lines (scene)
  "Return SCENE drawn as a list of propertized rows, as `eas-text-render'.
A row equal to the last render's on the same canvas is the same string,
so `eas-mode-patch-lines' skips it with `eq'."
  (let ((rows (let ((eas-text--defer-rows nil)) (eas-text--render-rows scene))))
    (cl-loop for cell on rows do (setcar cell (eas-text-row-string (car cell))))
    rows))

(defun eas-text-render-rows (scene)
  "SCENE's rows as `eas-text-render-lines', a repainted one left uncomposed.
Such a row is an `eas-text-row' record, the same object while the row
stays the same: `eas-mode-patch-lines' writes its cells straight into
the buffer (eas-b2s.9), and `eas-text-row-string' gives its string."
  (let ((eas-text--defer-rows t)) (eas-text--render-rows scene)))

(defun eas-text--render-rows (scene)
  "SCENE's rows, cached or not; see `eas-text-render-rows'.
Collection waits until they are drawn (`eas-gc-defer-render')."
  (eas-gc-defer-render)
  (eas-text-ink-with
   (let* ((g (eas-text--new scene t))
          ;; What every step depends on besides its key; nil caches nothing.
          (env (and eas-render-cache-enabled (not eas-text-trace)
                    (list (eas-text--grid-cols g) (eas-text--grid-rows g) (eas-text--grid-cw g)
                          (eas-text--grid-ch g) eas-text-ink--colors eas-image-base-directory
                          ;; The ink and band functions (a test may rebind them).
                          (symbol-function 'eas-text-ink-legible) eas-text-ink-min-contrast
                          (symbol-function 'eas-text-band-resolve)
                          (eas-text-ink-shade)))))
     (if env (eas-text--frame g env (eas-text--steps g scene))
       (eas-text--fill g)
       (let ((eas-text--label-cells (make-hash-table :test 'equal)))
         (dolist (step (eas-text--steps g scene)) (funcall (cdr step) g)))
       (let ((shade (eas-text-ink-shade)))
         (cl-loop for row below (eas-text--grid-rows g) collect (eas-text--compose-row g row shade)))))))

;;; Incremental frames (eas-b2s.7)
;;
;; A live view's frame changes a few items of a few marks.  The grid of
;; the last frame on a canvas is kept, with what each paint step tried:
;; the rows a step wrote (a bool-vector) and, for a mark step, the rows
;; each item wrote (a packed range).  A frame whose steps differ from
;; the last only in mark items repaints just the rows those items held
;; or hold now: it clears them and runs, in paint order and writing
;; only there, the steps that try them, and in those steps only the
;; items that try them (`eas-text--mark').  Every write site records
;; the rows it tries (`eas-text--touch'), so layering, priorities, area
;; slices and tiles resolve as in a full paint.  Labels whose overlap
;; was decided in the full paint replay that decision.  Only the rows
;; repainted are composed again; the rest are last frame's strings,
;; with no comparison.  Anything else (another canvas, axes, legends,
;; titles, the step list) paints in full.

(defun eas-text--step-marks (key)
  "Return the list of what a mark step with KEY paints, else nil."
  (pcase (car key)
    (:mark (list (nth 3 key)))
    (:tiled-marks (append (nth 2 key) nil))))

(defconst eas-text--item-keys '(:items :rows :index)
  "Mark keys that change with its items; text draws only from :items.
:rows and :index are the hit test's.")

(defun eas-text--same-shape-p (a b)
  "Return non-nil when mark A and mark B differ at most in their items."
  (and (consp a) (consp b) (= (length a) (length b))
       (cl-loop for (k v) on b by #'cddr
                always (or (memq k eas-text--item-keys) (let ((w (plist-get a k))) (or (eq v w) (equal v w)))))))

(defun eas-text--classify (old new)
  "How step key NEW differs from OLD: same, items, all, or nil (incomparable).
Items when only the items of its marks changed: then the changed rows
are those the changed items held (`eas-text--changed-rows')."
  (let ((m0 (eas-text--step-marks old)) (m1 (eas-text--step-marks new)))
    (cond ((and (null m1) (null m0)) (and (equal old new) 'same))
          ((not (and m0 m1 (eq (car old) (car new)) (equal (nth 1 old) (nth 1 new))
                     (or (not (eq (car new) :mark)) (equal (nth 2 old) (nth 2 new)))))
           nil)
          ;; Marks the update path listed as changed skip the comparison.
          ((and (not (seq-some (lambda (m) (eas-render-cache-dirty-p (car (nth 1 new)) m)) m1)) (equal old new)) 'same)
          ((and (= (length m0) (length m1)) (cl-every #'eas-text--same-shape-p m0 m1)) 'items)
          (t 'all))))

(defun eas-text--or-range (rows range wide)
  "Set in `bool-vector' ROWS the rows of packed RANGE, one more each way if WIDE."
  (when range
    (let ((lo (max 0 (- (/ range 65536) (if wide 1 0))))
          (hi (min (1- (length rows)) (+ (% range 65536) (if wide 1 0)))))
      (while (<= lo hi) (aset rows lo t) (setq lo (1+ lo))))))

(defun eas-text--item-rows (g mark item)
  "Rows of grid G that ITEM of MARK may draw in, packed, or nil if unknown.
A guess from its geometry: an incremental frame
repaints these rows with the ones the item held, so that it seldom
needs a second pass (`eas-text--repaint' checks what was drawn)."
  (let* ((ch (float (eas-text--grid-ch g)))
         (span (pcase (plist-get mark :mark)
                 ((or "rule" "tick") (let ((a (plist-get item :y1)) (b (plist-get item :y2)))
                                       (and (numberp a) (numberp b) (cons (min a b) (max a b)))))
                 ((or "bar" "rect" "brush")
                  ;; Cells as `eas-text--rect' and `eas-text-tile-draw' take them.
                  (let ((y (plist-get item :y)) (h (plist-get item :h)))
                    (and (numberp y) (numberp h) (>= h 0)
                         (let ((rows (if (equal (plist-get item :orient) "vertical")
                                         (cons (floor y ch) (ceiling (- (+ y h) 0.001) ch))
                                       (eas-text--cells y h ch))))
                           (cons (* ch (car rows)) (* ch (1- (max (1+ (car rows)) (cdr rows)))))))))
                 ("text" (let ((y (plist-get item :y)) (text (plist-get item :text)))
                           (and (numberp y) (stringp text)
                                (cons y (+ y (* ch (cl-count ?\n text)))))))
                 ((or "line" "area" "trail")
                  (let ((lo nil) (hi nil))
                    (dolist (k '(:points :base))
                      (let ((pts (plist-get item k)))
                        (when (vectorp pts)
                          (dotimes (i (length pts))
                            (let ((y (aref (aref pts i) 1)))
                              (when (or (null lo) (< y lo)) (setq lo y))
                              (when (or (null hi) (> y hi)) (setq hi y)))))))
                    (and lo (cons lo hi))))
                 ((or "point" "circle" "square" "symbol")
                  (let ((y (plist-get item :y)) (r (sqrt (or (plist-get item :size) 30))))
                    (and (numberp y) (cons (- y r) (+ y r))))))))
    (when span
      (let ((lo (max 0 (floor (car span) ch)))
            (hi (min (1- (eas-text--grid-rows g)) (floor (cdr span) ch))))
        (and (<= lo hi) (+ (* lo 65536) hi))))))

(defun eas-text--changed-rows (g rows marks infos)
  "Set in ROWS the rows that changed items held.
Those are the items of MARKS not `eq' to the ones INFOS recorded.  The
rows they may hold now on grid G are set too (`eas-text--item-rows')."
  (cl-loop for mark in marks for info in infos
           for items = (plist-get mark :items) for wide = (eas-text--wide-p mark)
           for old = (aref info 0) for ranges = (aref info 1)
           do (dotimes (i (max (length items) (length old)))
                (unless (and (< i (length items)) (< i (length old)) (eq (aref items i) (aref old i)))
                  (when (< i (length ranges)) (eas-text--or-range rows (aref ranges i) wide))
                  (when (< i (length items))
                    (eas-text--or-range rows (eas-text--item-rows g mark (aref items i)) wide))))))

(defun eas-text--clear-rows (g mask)
  "Empty the cells of grid G in the rows set in MASK."
  (let ((cols (eas-text--grid-cols g)))
    (dotimes (row (length mask))
      (when (aref mask row)
        (let ((i (* row cols)) (end (* (1+ row) cols)))
          (while (< i end)
            (aset (eas-text--grid-chars g) i ?\s) (aset (eas-text--grid-props g) i nil)
            (aset (eas-text--grid-prio g) i -1) (aset (eas-text--grid-dots g) i 0)
            (aset (eas-text--grid-dot-props g) i nil) (aset (eas-text--grid-dot-prio g) i -1)
            (aset (eas-text--grid-cover g) i 0.0) (aset (eas-text--grid-brush g) i nil)
            (setq i (1+ i))))))))

(defun eas-text--meets-rows-p (a b)
  "Non-nil when bool-vectors A and B set a row in common."
  (let ((i 0) (n (length a)))
    (while (and (< i n) (not (and (aref a i) (aref b i)))) (setq i (1+ i)))
    (< i n)))

(defun eas-text--run (g step data mask full)
  "Paint STEP (KEY . FN) into grid G, recording into its DATA.
DATA is [KEY ROWS INFOS LOG].  MASK limits the rows written; FULL
records afresh, else rows add to what the step tried before."
  (let ((rows (aref data 1)))
    (when full (fillarray rows nil))
    (let ((eas-text--mask mask) (eas-text--step-rows rows)
          (eas-text--lo most-positive-fixnum) (eas-text--hi -1)
          (eas-text--infos (aref data 2))
          (eas-text--label-replay (and (not full) (aref data 3) (cons (aref data 3) 0)))
          (eas-text--label-log (if full nil :off)))
      (funcall (cdr step) g)
      (when (and full (eq (car (car step)) :axes))
        (aset data 3 (vconcat (nreverse eas-text--label-log)))))))

(defun eas-text--frame (g env steps)
  "Paint a canvas with ENV, reusing its last frame; return the rows.
STEPS are the paint steps.
G gives the canvas's shape.  See the commentary above."
  (let* ((entry (assoc env eas-render-cache--frames))
         (old (cdr entry))
         (n (length steps)) (nrows (eas-text--grid-rows g))
         (classes (and old (= n (length (aref old 1)))
                       (catch 'no
                         (cl-loop for step in steps for data across (aref old 1)
                                  collect (or (eas-text--classify (aref data 0) (car step)) (throw 'no nil))))))
         ;; The first step that changed, and where the snapshot is.
         (k (and classes (cl-position-if (lambda (c) (not (eq c 'same))) classes)))
         (j (and old (aref old 5))))
    (cond
     ((null classes) (eas-text--full-frame g env steps entry nil))
     ((null k)
      (cl-loop for step in steps for data across (aref old 1) do (eas-text--adopt data (car step)))
      (eas-render-cache--count :text-hits)
      (eas-render-cache--count :row-hits nrows)
      (append (aref old 2) nil))
     ;; The changes moved off the snapshot twice in a row: paint in
     ;; full once, with the snapshot before the step that changes now.
     ((and (/= k j) (>= (aset old 6 (1+ (aref old 6))) 2))
      (eas-text--full-frame g env steps entry k))
     (t
      (when (= k j) (aset old 6 0))
      (eas-render-cache--count :text-hits)
      (eas-text--repaint old steps classes (if (>= k j) j 0) nrows)
      (eas-text--compose-changed (aref old 0) old (aref old 7) (car (last env)))))))

(defun eas-text--repaint (old steps classes start nrows)
  "Repaint the rows of frame OLD that changed.
STEPS are this frame's paint steps, CLASSES how they changed.
The first pass starts at step START (0, or the snapshot's step).  The
grid has NROWS rows; those repainted are left in OLD's slot 7, a
`bool-vector'."
  (let* ((g (aref old 0)) (datas (aref old 1)) (snap (aref old 4)) (j (aref old 5))
         (total (make-bool-vector nrows nil)))
    ;; The rows changed items held, and every row of a step that changed whole.
    (cl-loop for class in classes for data across datas for step in steps
             do (pcase class
                  ('items (eas-text--changed-rows g total (eas-text--step-marks (car step)) (aref data 2)))
                  ('all (bool-vector-union total (aref data 1) total))))
    (let ((mask (copy-sequence total)) (first t))
      (while mask
        (if (> start 0) (eas-text--copy-rows snap g mask) (eas-text--clear-rows g mask))
        (let ((eas-text--label-cells (aref old 3)))
          (cl-loop for step in steps for data across datas for class in classes for i from 0
                   do (when (and (= i j) (= start 0) (> j 0)) (eas-text--copy-rows g snap mask))
                   do (cond
                       ((< i start) (eas-render-cache--count :text-skipped))
                       ((or (and first (not (eq class 'same))) (eas-text--meets-rows-p (aref data 1) mask))
                        (when (and first (eq class 'all))
                          ;; No item of a mark that changed whole is kept.
                          (dolist (info (aref data 2)) (aset info 0 nil)))
                        (eas-text--run g step data mask (and first (eq class 'all))))
                       (t (eas-render-cache--count :text-skipped)))
                   do (eas-text--adopt data (car step)))
          (when (and (= start 0) (>= j (length steps))) (eas-text--copy-rows g snap mask)))
        ;; Rows the changed items hold now that were not repainted: a
        ;; second pass, from the first step, paints them.
        (let ((more (make-bool-vector nrows nil)))
          (when first
            (cl-loop for class in classes for data across datas
                     do (pcase class
                          ('items (eas-text--changed-rows-now more data))
                          ('all (bool-vector-union more (aref data 1) more)))))
          (bool-vector-set-difference more total more)
          ;; Steps before the snapshot changed nothing: it holds for every row.
          (setq first nil mask nil start (if (> start 0) start 0))
          (unless (zerop (bool-vector-count-population more))
            (bool-vector-union total more total)
            (setq mask more)))))
    (aset old 7 total)))

(defun eas-text--copy-rows (from to mask)
  "Copy the cells of grid FROM in the rows set in MASK into grid TO."
  (let ((cols (eas-text--grid-cols from)))
    (dotimes (row (length mask))
      (when (aref mask row)
        (let ((i (* row cols)) (end (* (1+ row) cols))
              (c0 (eas-text--grid-chars from)) (c1 (eas-text--grid-chars to))
              (p0 (eas-text--grid-props from)) (p1 (eas-text--grid-props to))
              (r0 (eas-text--grid-prio from)) (r1 (eas-text--grid-prio to))
              (d0 (eas-text--grid-dots from)) (d1 (eas-text--grid-dots to))
              (q0 (eas-text--grid-dot-props from)) (q1 (eas-text--grid-dot-props to))
              (s0 (eas-text--grid-dot-prio from)) (s1 (eas-text--grid-dot-prio to))
              (v0 (eas-text--grid-cover from)) (v1 (eas-text--grid-cover to))
              (b0 (eas-text--grid-brush from)) (b1 (eas-text--grid-brush to)))
          (while (< i end)
            (aset c1 i (aref c0 i)) (aset p1 i (aref p0 i)) (aset r1 i (aref r0 i)) (aset d1 i (aref d0 i))
            (aset q1 i (aref q0 i)) (aset s1 i (aref s0 i)) (aset v1 i (aref v0 i)) (aset b1 i (aref b0 i))
            (setq i (1+ i))))))))

(defun eas-text--adopt (data key)
  "Make step DATA's last key KEY, `equal' to it, and its items too."
  (unless (eq (aref data 0) key)
    (cl-loop for mark in (eas-text--step-marks key) for info in (aref data 2)
             do (when (and (consp mark) (equal (plist-get mark :items) (aref info 0)))
                  (aset info 0 (plist-get mark :items))))
    (aset data 0 key)))

(defun eas-text--changed-rows-now (rows data)
  "Set in ROWS the rows of the items of step DATA whose rows moved.
Run after the step painted: its infos' ranges are this frame's and
their spare ranges the last frame's, which the changed rows hold."
  (cl-loop for mark in (eas-text--step-marks (aref data 0)) for info in (aref data 2)
           for ranges = (aref info 1) for spare = (aref info 2) for wide = (eas-text--wide-p mark)
           do (dotimes (i (length ranges))
                (unless (and spare (< i (length spare)) (eql (aref spare i) (aref ranges i)))
                  (eas-text--or-range rows (aref ranges i) wide)))))

(defun eas-text--compose-changed (g old rows shade)
  "Compose the rows of grid G set in `bool-vector' ROWS; reuse OLD's others.
SHADE colors a brush."
  (let ((strings (aref old 2)))
    (dotimes (row (length rows))
      (if (aref rows row)
          (aset strings row (eas-text--row-out g row shade strings))
        (eas-render-cache--count :row-hits)))
    (append strings nil)))

(defun eas-text--full-frame (g env steps entry snap-at)
  "Paint a fresh grid G with ENV, recording for the next frame.
STEPS are the paint steps.
ENTRY is the canvas's last frame, whose grid is reused when it has one.
The grid is kept as it is before step SNAP-AT (default: the first mark
step, or the last frame's) for later frames to restart from."
  (let* ((old (cdr entry))
         (g (if old (let ((og (aref old 0))) (eas-text--clear-all og) og) (eas-text--fill g)))
         (labels (if old (let ((h (aref old 3))) (clrhash h) h) (make-hash-table :test 'equal)))
         (snap (if old (aref old 4) (eas-text--fill (copy-eas-text--grid g))))
         (j (or snap-at (and old (aref old 5))
                (or (cl-position-if (lambda (step) (eas-text--step-marks (car step))) steps) 0)))
         (nrows (eas-text--grid-rows g))
         (all (make-bool-vector nrows t))
         (datas (vconcat (mapcar (lambda (step)
                                   (vector (car step) (make-bool-vector nrows nil)
                                           (mapcar (lambda (_) (vector nil nil nil)) (eas-text--step-marks (car step)))
                                           nil))
                                 steps))))
    (let ((eas-text--label-cells labels))
      (cl-loop for step in steps for data across datas for i from 0
               do (when (= i j) (eas-text--copy-rows g snap all))
               do (eas-text--run g step data nil t)))
    (when (>= j (length steps)) (eas-text--copy-rows g snap all))
    (let* ((shade (car (last env)))
           (strings (make-vector nrows nil)))
      (dotimes (row nrows) (aset strings row (eas-text--row-out g row shade strings)))
      (setq eas-render-cache--frames
            (cons (cons env (vector g datas strings labels snap j 0 nil))
                  (seq-take (delq entry eas-render-cache--frames) (max 0 (1- eas-render-cache-text-entries)))))
      (append strings nil))))

(defun eas-text--clear-all (g)
  "Empty every cell of grid G."
  (fillarray (eas-text--grid-chars g) ?\s) (fillarray (eas-text--grid-props g) nil)
  (fillarray (eas-text--grid-prio g) -1) (fillarray (eas-text--grid-dots g) 0)
  (fillarray (eas-text--grid-dot-props g) nil) (fillarray (eas-text--grid-dot-prio g) -1)
  (fillarray (eas-text--grid-cover g) 0.0) (fillarray (eas-text--grid-brush g) nil)
  (clrhash (eas-text--grid-bands g)))

(defun eas-text--steps (g scene)
  "Return how SCENE paints on a grid like G: a list of (KEY . FN).
The list is in paint order.  FN paints into the grid it is given;
KEY holds every input it reads besides the grid, for
`eas-render-cache-paint'."
  (let (steps)
    (seq-doseq (view (plist-get scene :views))
      (push (cons (list :axes (plist-get view :axes)) (lambda (g) (eas-text--axes g view))) steps)
      (when-let* ((h (plist-get view :header)))
        (push (cons (list :header h)
                    (lambda (g) (eas-text--string g (plist-get h :x) (plist-get h :y) (plist-get h :text) "left"
                                                  (list 'face 'eas-title) 5)))
              steps))
      (let ((prios (mapcar (lambda (p) (/ p 100.0)) (eas-text--mark-prios view)))
            (clip (eas-text--clip g view))
            (where (list (plist-get view :id) (plist-get view :bounds) (plist-get view :scales))))
       (if (eas-text--tiled-p view)
           ;; Tiles resolve across the view's marks: one step for all of them.
           (push (cons (list :tiled-marks where (plist-get view :marks))
                       (lambda (g) (eas-text--marks g view)))
                 steps)
        (seq-doseq (mark (plist-get view :marks))
          (let ((prio (pop prios)))
            (push (cons (list :mark where prio
                              ;; An image may read a file: never reuse it.
                              (if (equal (plist-get mark :mark) "image") (make-symbol "image") mark))
                        (lambda (g) (eas-text--mark g view mark prio clip)))
                  steps)))))
      (push (cons (list :legends (plist-get view :id) (plist-get view :legends))
                  (lambda (g) (eas-text--legends g view)))
            steps))
    (dolist (title (let ((tt (plist-get scene :title))) (and tt (delq nil (list tt (plist-get tt :subtitle))))))
      (push (cons (list :title title)
                  (lambda (g) (eas-text--string g (plist-get title :x) (plist-get title :y) (plist-get title :text)
                                                "center" (list 'face 'eas-title) 5)))
            steps))
    (nreverse steps)))

(provide 'eas-text)
;;; eas-text.el ends here

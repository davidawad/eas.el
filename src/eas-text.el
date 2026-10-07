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

(defun eas-text--put (g col row char props prio &optional cover)
  "Put CHAR with PROPS at COL ROW of grid G when PRIO wins.
COVER ranks a bar's glyph (`eas-text--coverage'): within one priority
\(one mark) it takes a cell only from a lower rank, so a bar's thin end
cannot replace its neighbour's full block."
  (when (and (< -1 col (eas-text--grid-cols g)) (< -1 row (eas-text--grid-rows g)))
    (let* ((i (+ col (* row (eas-text--grid-cols g))))
           (old (aref (eas-text--grid-prio g) i)))
      (when (and eas-text-trace eas-text-trace-item) (funcall eas-text-trace col row))
      (when (and (>= prio old)
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
`eas-text--compose' leaves out."
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
        (aset (eas-text--grid-dots g) i
              (logior (aref (eas-text--grid-dots g) i)
                      (aref (aref eas-glyph-braille-dots (mod dy 4)) (mod dx 2))))
        (aset (eas-text--grid-dot-props g) i props)
        (aset (eas-text--grid-dot-prio g) i (max eas-text--dot-prio (aref (eas-text--grid-dot-prio g) i)))))))

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

(defun eas-text--dot-line (g x1 y1 x2 y2 props-fn clip &optional show-p)
  "Draw a braille line in G from pixel X1 Y1 to X2 Y2 inside CLIP cells.
PROPS-FN maps a pixel x to props.  SHOW-P, when non-nil, is called per
dot and skips the dot when it says nil."
  (let* ((sx (/ 2.0 (eas-text--grid-cw g))) (sy (/ 4.0 (eas-text--grid-ch g)))
         (a (floor (* x1 sx))) (b (floor (* y1 sy))) (c (floor (* x2 sx))) (d (floor (* y2 sy)))
         (dx (abs (- c a))) (dy (- (abs (- d b)))) (stepx (if (< a c) 1 -1)) (stepy (if (< b d) 1 -1))
         (err (+ dx dy)) (done nil))
    (while (not done)
      (when (or (null show-p) (funcall show-p))
        (eas-text--dot g a b (funcall props-fn (/ (+ a 0.5) sx)) clip))
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
                 for col from (aref clip 0) below (aref clip 2)
                 for cx = (* (+ col 0.5) (eas-text--grid-cw g))
                 for p = (eas-text--interp points cx pxs)
                 for q = (eas-text--interp base cx bxs)
                 ;; A band (errorband, ranged area) may give its lower edge first.
                 for top = (and p q (min p q))
                 for bottom = (and p q (max p q))
                 when (and top bottom)
                 do (cl-loop for row from (max (aref clip 1) (floor top ch)) below (min (aref clip 3) (ceiling bottom ch))
                             for y0 = (* row ch) for y1 = (* (1+ row) ch)
                             for covered = (- (min y1 bottom) (max y0 top))
                             when (> covered 0)
                             do (let ((i (+ col (* row (eas-text--grid-cols g)))) (bands (eas-text--grid-bands g)))
                                  (puthash i (eas-text--cons (eas-text--cons top (eas-text--cons bottom (eas-text--cons (funcall props-fn cx) nil)))
                                                             (gethash i bands))
                                           bands))
                             do (cond
                                 ((>= covered (- ch 0.01)) (eas-text--put g col row ?█ (funcall props-fn cx) prio))
                                 ((and (> top y0) (> covered 0))
                                  ;; At least an eighth: a thin stacked slice still lands in a cell.
                                  (eas-text--put g col row (eas-glyph-lower (max 1 (round (* 8 (/ covered ch)))))
                                                 (funcall props-fn cx) prio))
                                 ((>= covered (/ ch 2.0)) (eas-text--put g col row ?█ (funcall props-fn cx) prio))
                                 ;; A slice that is only a sliver against this cell's
                                 ;; top edge (a stacked slice at the plot's top) still
                                 ;; marks its place.
                                 ((and (> covered 0) (>= top y0) (<= bottom y1)
                                       (< (aref (eas-text--grid-prio g) (+ col (* row (eas-text--grid-cols g)))) prio))
                                  (eas-text--put g col row (if (>= covered (* 0.375 ch)) ?▀ ?▔) (funcall props-fn cx) prio))
                                 ;; Above the slice beneath's eighth block, the
                                 ;; sliver colors the block's empty top.
                                 ((and (> covered 0) (>= top y0) (<= bottom y1))
                                  (eas-text--under g col row (funcall props-fn cx) prio)))))
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
                 (pcase (let ((y0 (* (/ i cols) ch)))
                          ;; A slice ending just past the cell's edge meets this one.
                          (eas-text-band-resolve
                           (append segs
                                   (seq-filter (lambda (s) (>= (cadr s) (- y0 slack))) (gethash (- i cols) bands))
                                   (seq-filter (lambda (s) (<= (car s) (+ y0 ch slack))) (gethash (+ i cols) bands)))
                           y0 ch))
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
    (when (and color (= (aref (eas-text--grid-prio g) i) prio)
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
                              (when (and (< -1 col gcols) (< -1 row grows))
                                (let* ((i (+ col (* row gcols))) (old (aref vprio i)) (cover (eas-text--coverage glyph)))
                                  (when (and (>= prio old) (or (> prio old) (> cover (aref vcover i))))
                                    (aset vchars i glyph) (aset vprops i props) (aset vprio i prio) (aset vcover i cover)))))
                             ;; The brush shades its cells whatever they hold
                             ;; when the grid is composed: one solid region.
                             (brush
                              (when (and (< -1 col (eas-text--grid-cols g)) (< -1 row (eas-text--grid-rows g)))
                                (aset (eas-text--grid-brush g) (+ col (* row (eas-text--grid-cols g))) props)))))))))

(defun eas-text--brush-props (props brush shade)
  "PROPS of a cell under BRUSH (its props), shaded with color SHADE."
  (let ((face (plist-get props 'face)))
    (append brush
            (plist-put (copy-sequence props) 'face
                       (cond ((null face) (list :background shade))
                             ((keywordp (car-safe face)) (append (list :background shade) face))
                             (t (list (list :background shade) face)))))))

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
     (t (let ((eas-text--dot-prio prio)) (eas-text--dot-line g x1 y1 x2 y2 (lambda (_) props) clip))))))

(defun eas-text--inside (bounds x)
  "X, nudged inside the plot when it lies on BOUNDS' right edge."
  (if (< (abs (- x (+ (aref bounds 0) (aref bounds 2)))) 0.005) (- x 0.01) x))

(defconst eas-text--fills '("bar" "rect" "arc" "area" "brush" "geoshape")
  "Marks that fill a region; strokes drawn before one sit under it.")

(defun eas-text--translucent-p (mark)
  "Non-nil when MARK's first item is mostly see-through."
  (let ((item (and (> (length (plist-get mark :items)) 0) (aref (plist-get mark :items) 0))))
    (and item (< (* (or (plist-get item :opacity) 1) (or (plist-get item :fillOpacity) 1)) 0.5))))

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

(defun eas-text--mark (g view mark prio clip)
  "Draw MARK of VIEW into grid G at PRIO, clipped to CLIP cells."
  (let ((b (plist-get view :bounds)))
     (let ((arcs (and (equal (plist-get mark :mark) "arc") (make-hash-table :test 'eql))))
      (seq-do-indexed
       (lambda (item i)
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
                                     (eas-text--item-props view mark item (plist-get item :datum)) prio)))))))))
       (plist-get mark :items))
      (when arcs (eas-text--resolve-arcs g arcs prio clip))
      (eas-text--resolve-bands g prio))))

(defvar eas-text--label-cells nil
  "Hash of (COL . ROW) cells holding a tick label, while a scene renders.")

(defun eas-text--label-room-p (g tk)
  "Non-nil when tick TK's label fits beside the labels already placed in G.
It then claims its cells.  A label that would overwrite another one is
left out, as Vega's labelOverlap drops it, so no label is garbled."
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
                                 when (and (= (aref (eas-text--grid-prio g) k) 0)
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

(defun eas-text--compose (g &optional env)
  "Return grid G as a propertized string, braille dots merged, lines trimmed.
ENV, when non-nil, lets `eas-render-cache-rows' reuse unchanged rows."
  (mapconcat #'identity (eas-text--compose-lines g env) "\n"))

(defun eas-text--compose-lines (g &optional env)
  "Return grid G's rows as a list of propertized strings, as `eas-text--compose'.
A row reused from the last render on ENV is the same string object."
  (let ((shade (eas-text-ink-shade)))
    (eas-render-cache-grid-rows (and env (cons shade env)) g (eas-text--grid-rows g) #'eas-text--same-row-p
                                (lambda (row) (eas-text--compose-row g row shade)))))

(defun eas-text--same-row-p (old g row)
  "Non-nil when ROW of grid G composes as it did in grid OLD.
Chars, dots, props and brush must be `equal' cell for cell; where a
cell has dots, so must its dot props and whether its dots show
\(`eas-text--compose-row' reads priorities nowhere else)."
  (let* ((cols (eas-text--grid-cols g)) (i (* row cols)) (b (+ i cols)))
    (and (= cols (eas-text--grid-cols old))
         (let ((c0 (eas-text--grid-chars old)) (c1 (eas-text--grid-chars g))
               (d0 (eas-text--grid-dots old)) (d1 (eas-text--grid-dots g))
               (p0 (eas-text--grid-props old)) (p1 (eas-text--grid-props g))
               (b0 (eas-text--grid-brush old)) (b1 (eas-text--grid-brush g)))
           (while (and (< i b)
                       (eq (aref c0 i) (aref c1 i))
                       (let ((d (aref d1 i)))
                         (and (eq (aref d0 i) d)
                              (let ((x (aref p0 i)) (y (aref p1 i))) (or (eq x y) (equal x y)))
                              (let ((x (aref b0 i)) (y (aref b1 i))) (or (eq x y) (equal x y)))
                              (or (eq d 0)
                                  (and (let ((x (aref (eas-text--grid-dot-props old) i))
                                             (y (aref (eas-text--grid-dot-props g) i)))
                                         (or (eq x y) (equal x y)))
                                       (eq (<= (aref (eas-text--grid-prio old) i) (aref (eas-text--grid-dot-prio old) i))
                                           (<= (aref (eas-text--grid-prio g) i) (aref (eas-text--grid-dot-prio g) i))))))))
             (setq i (1+ i)))
           (= i b)))))

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
  (eas-text-ink-with
   (let* ((g (eas-text--new scene t))
          ;; What every step depends on besides its key; nil caches nothing.
          (env (and eas-render-cache-enabled (not eas-text-trace)
                    (list (eas-text--grid-cols g) (eas-text--grid-rows g) (eas-text--grid-cw g)
                          (eas-text--grid-ch g) eas-text-ink--colors
                          eas-image-base-directory)))
          ;; A render restarting from a snapshot never fills G.
          (state (eas-render-cache-paint env (lambda () (cons (eas-text--fill g) (make-hash-table :test 'equal)))
                                         (eas-text--steps g scene) #'eas-text--run-step)))
     (eas-text--compose-lines (car state) env))))

(defun eas-text--run-step (fn state)
  "Call FN on the grid of STATE (GRID . LABEL-CELLS)."
  (let ((eas-text--label-cells (cdr state))) (funcall fn (car state))))

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

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

(defun eas-text--new (scene)
  "An empty grid sized for SCENE."
  (let* ((size (plist-get scene :size)) (cell (plist-get size :cell))
         (cw (aref cell 0)) (ch (aref cell 1))
         (cols (max 1 (round (/ (float (plist-get size :w)) cw))))
         (rows (max 1 (round (/ (float (plist-get size :h)) ch))))
         (n (* cols rows)))
    (eas-text--grid-make :cols cols :rows rows :cw cw :ch ch
                           :chars (make-vector n ?\s) :props (make-vector n nil)
                           :prio (make-vector n -1) :dots (make-vector n 0) :dot-props (make-vector n nil)
                           :dot-prio (make-vector n -1) :cover (make-vector n 0.0)
                           :bands (make-hash-table :test 'eql) :brush (make-vector n nil))))

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

(defun eas-text--item-props (view mark item datum)
  "Text properties for a cell of ITEM (DATUM) in MARK of VIEW."
  (let ((color (let ((f (plist-get item :fill)) (s (plist-get item :stroke)))
                 (if (or (null f) (equal f "none")) s f))))
    (append (list 'eas-view (plist-get view :id) 'eas-mark (plist-get mark :id) 'eas-datum datum)
            (when-let* ((tip (eas-text--tooltip item))) (list 'help-echo tip))
            (when-let* ((ink (and color (not (equal color "none")) (eas-text-ink-legible color))))
              (list 'face (list :foreground ink))))))

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

(defun eas-text--series (g view mark item clip prio)
  "Draw line or area ITEM of MARK in VIEW into G at PRIO inside CLIP."
  (let* ((points (plist-get item :points))
         (anchors (eas-hit--anchors item))
         (axs (vconcat (mapcar (lambda (p) (aref p 0)) anchors)))
         ;; Dots in one braille column share x: look its props up once.
         (memo (make-hash-table :test 'eql))
         (props-fn (lambda (x) (or (gethash x memo)
                                   (puthash x (eas-text--item-props
                                               view mark item (aref (plist-get item :datum) (eas-hit--bisect axs x)))
                                            memo))))
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
                             do (push (list top bottom (funcall props-fn cx))
                                      (gethash (+ col (* row (eas-text--grid-cols g))) (eas-text--grid-bands g)))
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

(defun eas-text--resolve-bands (g prio)
  "Compose the cells of grid G that area slices drawn at PRIO tile.
See `eas-text-band-resolve'.  Then forget the slices."
  (let* ((ch (eas-text--grid-ch g)) (cols (eas-text--grid-cols g)) (bands (eas-text--grid-bands g))
         (slack (/ ch 4.0)))
    (maphash (lambda (i segs)
               (when (= (aref (eas-text--grid-prio g) i) prio)
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
                                           (append (list :background bg) (plist-get props 'face)))))))))))
             bands)
    (clrhash bands)))

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
     (cl-loop for row from (max r0 (aref clip 1)) below (min r1 (aref clip 3))
             do (cl-loop for col from (max c0 (aref clip 0)) below (min c1 (aref clip 2))
                         for char = (cond
                                     (brush nil)
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
                             (glyph (eas-text--put g col row glyph props prio (eas-text--coverage glyph)))
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

(defconst eas-text--fills '("bar" "rect" "arc" "area" "brush")
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

(defun eas-text--marks (g view)
  "Draw every mark of VIEW into grid G, clipped to its plot."
  (let* ((b (plist-get view :bounds))
         (prios (mapcar (lambda (p) (/ p 100.0)) (eas-text--mark-prios view)))
         (clip (vector (eas-text--col g (aref b 0)) (eas-text--row g (aref b 1))
                       (eas-text--col g (+ (aref b 0) (aref b 2) -0.01)) (eas-text--row g (+ (aref b 1) (aref b 3) -0.01))))
         (clip (vector (aref clip 0) (aref clip 1) (1+ (aref clip 2)) (1+ (aref clip 3)))))
    (seq-doseq (mark (plist-get view :marks))
     (let ((prio (pop prios)) (arcs (and (equal (plist-get mark :mark) "arc") (make-hash-table :test 'eql))))
      (seq-do-indexed
       (lambda (item i)
         (unless (equal (plist-get item :opacity) 0)
          (let ((eas-text-trace-item (and eas-text-trace (list (plist-get view :id) (plist-get mark :id) i))))
           (pcase (plist-get mark :mark)
             ((or "line" "area" "trail") (eas-text--series g view mark item clip prio))
             ((or "bar" "rect" "brush") (eas-text--rect g view mark item clip prio))
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
             ("image" (eas-text--put g (eas-text--col g (+ (plist-get item :x) (/ (plist-get item :w) 2.0)))
                                     (eas-text--row g (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))
                                     eas-marks-image-glyph
                                     (eas-text--item-props view mark item (plist-get item :datum)) prio))
             ("text" (let ((eas-text--clamp-rows t))
                      (eas-text--string g (plist-get item :x) (plist-get item :y) (plist-get item :text)
                                         (plist-get item :align)
                                         (eas-text--item-props view mark item (plist-get item :datum)) prio)))
             (_ (let ((filled (not (equal (plist-get item :fill) "none")))
                      ;; A point on the plot's far edge belongs to the last cell.
                      (col (min (eas-text--col g (plist-get item :x)) (1- (aref clip 2))))
                      (row (min (eas-text--row g (plist-get item :y)) (1- (aref clip 3)))))
                  (when (and (<= (aref clip 0) col) (<= (aref clip 1) row))
                    (eas-text--put g col row
                                     (eas-symbols-glyph (plist-get item :shape) filled (plist-get item :angle))
                                     (eas-text--item-props view mark item (plist-get item :datum)) prio))))))))
       (plist-get mark :items))
      (when arcs (eas-text--resolve-arcs g arcs prio clip))
      (eas-text--resolve-bands g prio)))))

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
          ;; Grid lines are dotted so marks stay legible.
          (let ((vertical (< (abs (- (aref grid 0) (aref grid 2))) 0.5)))
            (cl-loop for i across (eas-text--grid-prio g) for k from 0
                     when (and (= i 0) (memq (aref (eas-text--grid-chars g) k) '(?│ ?─)))
                     do (aset (eas-text--grid-chars g) k (if vertical ?┊ ?┈)))))))
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

(defun eas-text--compose (g)
  "Return grid G as a propertized string, braille dots merged, lines trimmed."
  (let ((cols (eas-text--grid-cols g)) (shade (eas-text-ink-shade)) lines)
    (dotimes (row (eas-text--grid-rows g))
      (let ((runs nil) (chars nil) (props :unset)
            ;; Blank cells past the last glyph are trimmed, unless brushed.
            (end (cl-loop for col downfrom (1- cols) to 0
                          for i = (+ col (* row cols))
                          unless (and (eq (aref (eas-text--grid-chars g) i) ?\s) (null (aref (eas-text--grid-brush g) i))
                                      (or (zerop (aref (eas-text--grid-dots g) i))
                                          (> (aref (eas-text--grid-prio g) i) (aref (eas-text--grid-dot-prio g) i))))
                          return (1+ col)
                          finally return 0)))
        (dotimes (col end)
          (let* ((i (+ col (* row cols)))
                 (dots (aref (eas-text--grid-dots g) i))
                 (use-dots (and (> dots 0) (<= (aref (eas-text--grid-prio g) i) (aref (eas-text--grid-dot-prio g) i))))
                 (char (if use-dots (+ #x2800 dots) (aref (eas-text--grid-chars g) i)))
                 (p (if use-dots (aref (eas-text--grid-dot-props g) i) (aref (eas-text--grid-props g) i)))
                 (p (if-let* ((brush (aref (eas-text--grid-brush g) i))) (eas-text--brush-props p brush shade) p)))
            (unless (equal p props)
              (when chars (push (apply #'propertize (apply #'string (nreverse chars)) (unless (eq props :unset) props)) runs))
              (setq chars nil props p))
            (unless (eq char 0) (push char chars))))
        (when chars (push (apply #'propertize (apply #'string (nreverse chars)) (unless (eq props :unset) props)) runs))
        (push (apply #'concat (nreverse runs)) lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defun eas-text-render (scene)
  "Return SCENE drawn as a propertized string (rows joined by newlines)."
  (eas-text-ink-with
   (let ((g (eas-text--new scene)) (eas-text--label-cells (make-hash-table :test 'equal)))
    (seq-doseq (view (plist-get scene :views))
      (eas-text--axes g view)
      (when-let* ((h (plist-get view :header)))
        (eas-text--string g (plist-get h :x) (plist-get h :y) (plist-get h :text) "left" (list 'face 'eas-title) 5))
      (eas-text--marks g view)
      (eas-text--legends g view))
    (dolist (title (let ((tt (plist-get scene :title))) (and tt (delq nil (list tt (plist-get tt :subtitle))))))
      (eas-text--string g (plist-get title :x) (plist-get title :y) (plist-get title :text) "center"
                          (list 'face 'eas-title) 5))
    (eas-text--compose g))))

(provide 'eas-text)
;;; eas-text.el ends here

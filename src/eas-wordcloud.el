;;; eas-wordcloud.el --- the wordcloud transform: spiral word placement -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's wordcloud transform (Jason Davies's d3-cloud
;; layout) as an x-eas domain transform.  Each row is a word with a
;; font size, weight, font and rotation; the layout places the largest
;; word first and walks every word along a spiral from a random start
;; until its box touches no placed word and stays inside SIZE.  Rows
;; gain x and y (the text anchor: centered, on the alphabetic
;; baseline), font, fontSize, fontStyle, fontWeight and angle; a word
;; that finds no room gets null x and y and is not drawn, as in Vega.
;;
;;   {"x-eas:transform": "wordcloud", "text": "text", "size": [800, 400],
;;    "fontSize": {"field": "count"}, "fontSizeRange": [12, 56],
;;    "rotate": [-45, 0, 45], "padding": 2, "seed": 1}
;;
;; Differences from Vega, on purpose:
;;
;; - Determinism.  Vega draws start points and spiral directions from
;;   Math.random; here they come from a seeded generator, so the same
;;   spec and seed give the same cloud on every run and machine.  The
;;   draws happen in d3-cloud's order.  "rotate" may be an array of
;;   angles, one picked per word from the same generator.
;; - Collision.  d3-cloud tests rasterized glyph sprites on a canvas.
;;   Emacs has no canvas in batch, so a word is its measured text box:
;;   the advance width from `eas-font-text-width' (Arial, Times or a
;;   registered font file) and an ascent and descent read from the
;;   glyphs it holds, rotated with the word and grown by "padding".
;;   The boxes are rasterized onto d3-cloud's occupancy bitmap.  Words
;;   cannot nest inside another word's counters, so a cloud is a little
;;   looser than Vega's.
;;
;; The geometry knows nothing of words: `eas-wordcloud-text-box'
;; measures any text's box, `eas-wordcloud-box' turns and places it,
;; `eas-wordcloud-overlap-p' tests two turned boxes exactly (separating
;; axes) and the board packs many.  A layout that places rotated text,
;; such as rotated axis labels, can test its collisions with them.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)
(require 'eas-expr)
(require 'eas-font)
(require 'eas-memo)

;;; Seeded random numbers

(defun eas-wordcloud-random (seed)
  "Return a function of no arguments giving floats in [0, 1).
The sequence is a 32-bit linear congruential generator started from
integer SEED, the same on every run and machine."
  (let ((state (logand (+ (* (if (integerp seed) seed 0) 2654435761) 1013904223) #xFFFFFFFF)))
    (lambda ()
      (setq state (logand (+ (* state 1664525) 1013904223) #xFFFFFFFF))
      (/ (float state) 4294967296.0))))

;;; Text boxes

(defconst eas-wordcloud--ascent-cap 0.716
  "Height of capitals and digits above the baseline, in em (Arial).")

(defconst eas-wordcloud--ascent-x 0.519
  "Height of lowercase letters without ascenders, in em (Arial).")

(defconst eas-wordcloud--descent 0.21
  "Depth of descenders below the baseline, in em (Arial).")

(defun eas-wordcloud-text-box (text size &optional weight font)
  "Box of TEXT at SIZE px as (X0 Y0 X1 Y1) around its anchor.
The anchor is the text's centre on its alphabetic baseline; y grows
down.  The width is measured in FONT (a family name, nil for Arial)
with WEIGHT; the ascent and descent follow the glyphs TEXT holds:
capitals, digits and tall lowercase reach the cap height, the other
lowercase the x-height, and descenders drop below the baseline."
  (let* ((w (let ((eas-font-family font)) (eas-font-text-width text size weight)))
         (case-fold-search nil)
         (ascent (cond ((string-match-p "[A-Z0-9bdfhiklt'\"!?/|()[{}&%$#@]" text) eas-wordcloud--ascent-cap)
                       ((string-match-p "[^[:space:]]" text) eas-wordcloud--ascent-x)
                       (t 0)))
         (descent (if (string-match-p "[gjpqyQ,;()[{}|/@$]" text) eas-wordcloud--descent 0)))
    (list (- (/ w 2.0)) (- (* ascent size)) (/ w 2.0) (* descent size))))

;; A placed box is one flat vector, so a test at an offset needs no moved copy:
;;   0-3   axis-aligned bounds X0 Y0 X1 Y1
;;   4-11  the four corners X Y, in order around the box
;;   12-15 its first edge normal UX UY and the box's extent LO HI on it
;;   16-19 the same for the second edge normal

(defun eas-wordcloud-box (x y box angle &optional pad)
  "The box BOX (X0 Y0 X1 Y1 around an anchor) placed at X Y.
It is grown by PAD px on every side and turned ANGLE degrees clockwise
about the anchor (y grows down).  Return a box vector for
`eas-wordcloud-overlap-p' and `eas-wordcloud-sprite'."
  (let* ((pad (or pad 0))
         (x0 (- (nth 0 box) pad)) (y0 (- (nth 1 box) pad))
         (x1 (+ (nth 2 box) pad)) (y1 (+ (nth 3 box) pad))
         (a (degrees-to-radians (or angle 0))) (c (cos a)) (s (sin a))
         (out (make-vector 20 0)) (i 4))
    (dolist (p (list (cons x0 y0) (cons x1 y0) (cons x1 y1) (cons x0 y1)))
      (aset out i (+ x (- (* c (car p)) (* s (cdr p)))))
      (aset out (1+ i) (+ y (* s (car p)) (* c (cdr p))))
      (setq i (+ i 2)))
    (aset out 0 (min (aref out 4) (aref out 6) (aref out 8) (aref out 10)))
    (aset out 1 (min (aref out 5) (aref out 7) (aref out 9) (aref out 11)))
    (aset out 2 (max (aref out 4) (aref out 6) (aref out 8) (aref out 10)))
    (aset out 3 (max (aref out 5) (aref out 7) (aref out 9) (aref out 11)))
    ;; The normals of a rectangle's edges are its two turned axes.
    (cl-loop for (ux uy) in (list (list c s) (list (- s) c)) for k in '(12 16)
             do (let ((lo 1.0e+INF) (hi -1.0e+INF))
                  (dotimes (j 4)
                    (let ((d (+ (* ux (aref out (+ 4 (* 2 j)))) (* uy (aref out (+ 5 (* 2 j)))))))
                      (setq lo (min lo d) hi (max hi d))))
                  (aset out k ux) (aset out (+ k 1) uy) (aset out (+ k 2) lo) (aset out (+ k 3) hi)))
    out))

(defun eas-wordcloud--apart-p (a b dx dy)
  "Non-nil when a normal of box A separates it from box B moved DX DY."
  (let ((apart nil) (k 12))
    (while (and (not apart) (<= k 16))
      (let ((ux (aref a k)) (uy (aref a (1+ k))) (lo 1.0e+INF) (hi -1.0e+INF))
        (dotimes (j 4)
          (let ((d (+ (* ux (+ dx (aref b (+ 4 (* 2 j))))) (* uy (+ dy (aref b (+ 5 (* 2 j))))))))
            (setq lo (min lo d) hi (max hi d))))
        (when (or (<= hi (aref a (+ k 2))) (<= (aref a (+ k 3)) lo)) (setq apart t)))
      (setq k (+ k 4)))
    apart))

(defun eas-wordcloud-overlap-p (a b &optional dx dy)
  "Non-nil when boxes A, moved DX DY, and B overlap.
A and B come from `eas-wordcloud-box'; touching edges do not overlap
\(the separating axis theorem over both boxes' edge normals)."
  (let ((dx (or dx 0)) (dy (or dy 0)))
    (and (< (+ dx (aref a 0)) (aref b 2)) (< (aref b 0) (+ dx (aref a 2)))
         (< (+ dy (aref a 1)) (aref b 3)) (< (aref b 1) (+ dy (aref a 3)))
         (not (eas-wordcloud--apart-p a b (- dx) (- dy)))
         (not (eas-wordcloud--apart-p b a dx dy)))))

;;; The board: occupied pixels

;; As d3-cloud does, the layout keeps a bitmap of the canvas: one vector
;; of 60-bit fixnum chunks per pixel row.  A word is its box rasterized
;; once into one span of columns per row (a turned rectangle is convex),
;; so testing a position is integer work on a few chunks per row.

(defconst eas-wordcloud--bits 60
  "Pixels per board chunk; a chunk is a fixnum.")

(defun eas-wordcloud-board (w h)
  "An empty board of W by H pixels."
  (let ((n (1+ (/ (ceiling w) eas-wordcloud--bits))) (board (make-vector (ceiling h) nil)))
    (dotimes (i (length board)) (aset board i (make-vector n 0)))
    board))

(defun eas-wordcloud--row-extent (corners y0 y1)
  "(MIN . MAX) x of convex polygon CORNERS within the strip Y0 <= y <= Y1.
CORNERS is a list of (X . Y); nil when the polygon misses the strip."
  (let ((lo nil) (hi nil) (n (length corners)))
    (cl-flet ((take (x) (setq lo (if lo (min lo x) x) hi (if hi (max hi x) x))))
      (dotimes (i n)
        (let* ((p (nth i corners)) (q (nth (% (1+ i) n) corners))
               (px (car p)) (py (cdr p)) (qx (car q)) (qy (cdr q)))
          (when (<= y0 py y1) (take px))
          (unless (= py qy)
            (dolist (y (list y0 y1))
              (when (and (<= (min py qy) y) (<= y (max py qy)))
                (take (+ px (* (- qx px) (/ (- y py) (- qy py)))))))))))
    (and lo (cons lo hi))))

(defun eas-wordcloud-sprite (box)
  "BOX (from `eas-wordcloud-box', around an anchor at 0 0) as pixel spans.
Return a vector [ROW0 X0 X1 ...]: ROW0 is the first pixel row and each
pair the first and last pixel columns the box touches in that row and
the ones after it, relative to the anchor.  Every pixel the box covers
any part of is in a span."
  (let* ((corners (cl-loop for i from 4 below 12 by 2 collect (cons (aref box i) (aref box (1+ i)))))
         (row0 (floor (aref box 1))) (row1 (1- (ceiling (aref box 3))))
         (out (list row0)))
    (cl-loop for row from row0 to (max row0 row1)
             for ext = (eas-wordcloud--row-extent corners row (1+ row))
             do (setq out (if ext
                              (append (list (1- (max (1+ (floor (car ext))) (ceiling (cdr ext)))) (floor (car ext))) out)
                            ;; A row the box only grazes keeps an empty span.
                            (append (list -1 0) out))))
    (vconcat (nreverse out))))

(defun eas-wordcloud--span-hit-p (row a b)
  "Non-nil when a pixel of board ROW in columns A..B is occupied."
  (let ((bits eas-wordcloud--bits) (hit nil) (c (/ a eas-wordcloud--bits)) (cb (/ b eas-wordcloud--bits)))
    (while (and (not hit) (<= c cb))
      (let* ((base (* c bits)) (lo (max a base)) (hi (min b (+ base bits -1))))
        (setq hit (/= 0 (logand (aref row c) (ash (1- (ash 1 (1+ (- hi lo)))) (- lo base)))))
        (setq c (1+ c))))
    hit))

(defun eas-wordcloud-board-hits-p (board sprite x y)
  "Non-nil when SPRITE with its anchor at integer X Y covers a set pixel.
The sprite must lie inside BOARD."
  (let ((row (+ y (aref sprite 0))) (i 1) (n (length sprite)) (hit nil))
    (while (and (not hit) (< i n))
      (let ((a (aref sprite i)) (b (aref sprite (1+ i))))
        (when (<= a b) (setq hit (eas-wordcloud--span-hit-p (aref board row) (+ x a) (+ x b)))))
      (setq row (1+ row) i (+ i 2)))
    hit))

(defun eas-wordcloud-board-add (board sprite x y)
  "Set the pixels SPRITE covers with its anchor at integer X Y on BOARD."
  (let ((row (+ y (aref sprite 0))) (bits eas-wordcloud--bits))
    (cl-loop for i from 1 below (length sprite) by 2
             for a = (+ x (aref sprite i)) for b = (+ x (aref sprite (1+ i)))
             do (let ((cells (aref board row)))
                  (cl-loop for c from (/ a bits) to (/ (max a b) bits)
                           for base = (* c bits)
                           for lo = (max a base) for hi = (min b (+ base bits -1))
                           when (<= lo hi)
                           do (aset cells c (logior (aref cells c) (ash (1- (ash 1 (1+ (- hi lo)))) (- lo base))))))
                (setq row (1+ row)))
    board))

;;; Spirals

(defun eas-wordcloud--spiral (kind w h)
  "The spiral function of KIND for a W by H canvas: T -> (DX . DY).
KIND is \"archimedean\" (the default) or \"rectangular\", d3-cloud's."
  (if (equal kind "rectangular")
      (let ((dy 4) (dx (/ (* 4.0 w) h)) (x 0) (y 0))
        (lambda (tt)
          (let ((sign (if (< tt 0) -1 1)))
            (pcase (logand (truncate (- (sqrt (+ 1 (* 4 sign tt))) sign)) 3)
              (0 (setq x (+ x dx)))
              (1 (setq y (+ y dy)))
              (2 (setq x (- x dx)))
              (_ (setq y (- y dy))))
            (cons x y))))
    (let ((e (/ (float w) h)))
      (lambda (tt)
        (let ((tt (* tt 0.1)))
          (cons (* e tt (cos tt)) (* tt (sin tt))))))))

;;; Layout

(defun eas-wordcloud-place (words w h &rest options)
  "Place WORDS on a W by H canvas; return their positions.
WORDS is a list of plists (:text :size :angle :weight :font).  OPTIONS
are :padding (px around each word, default 1), :spiral (see
`eas-wordcloud--spiral') and :random (a function giving floats in
\[0, 1), default seed 0).  Words are placed largest first.  The result
is a list, in WORDS's order, of (X . Y) anchors or nil for a word that
found no room."
  (let* ((random (or (plist-get options :random) (eas-wordcloud-random 0)))
         (padding (or (plist-get options :padding) 1))
         (spiral (plist-get options :spiral))
         (max-delta (sqrt (+ (* w w) (* h h))))
         ;; Past this turn the archimedean spiral (dx/e, dy) is farther
         ;; than the canvas's diagonal from the start, so every later
         ;; point is off the canvas: d3-cloud's walk would only skip them.
         (last (and (not (equal spiral "rectangular"))
                    (* 10 (+ 2 (sqrt (+ (expt (/ (float w) (/ (float w) h)) 2) (* h h)))))))
         (board (eas-wordcloud-board w h))
         (order (sort (number-sequence 0 (1- (length words)))
                      (lambda (i j) (> (plist-get (nth i words) :size) (plist-get (nth j words) :size)))))
         (out (make-vector (length words) nil)))
    (dolist (i order)
      (let* ((word (nth i words))
             (start-x (ash (truncate (* w (+ (funcall random) 0.5))) -1))
             (start-y (ash (truncate (* h (+ (funcall random) 0.5))) -1))
             (shape (and (> (plist-get word :size) 0)
                         (eas-wordcloud-text-box (plist-get word :text) (plist-get word :size)
                                                 (plist-get word :weight) (plist-get word :font))))
             (dt (if (< (funcall random) 0.5) 1 -1)))
        (when shape
          (let* ((step (eas-wordcloud--spiral spiral w h))
                 ;; The turned box once, at the origin; each step moves it.
                 (sprite (eas-wordcloud-sprite (eas-wordcloud-box 0 0 shape (plist-get word :angle) padding)))
                 (top (aref sprite 0)) (bottom (+ top (/ (length sprite) 2)))
                 (left (cl-loop for i from 1 below (length sprite) by 2 minimize (aref sprite i)))
                 (right (cl-loop for i from 2 below (length sprite) by 2 maximize (1+ (aref sprite i))))
                 (tt (- dt)) (done nil))
            (while (not done)
              (setq tt (+ tt dt))
              (let* ((d (funcall step tt)) (dx (truncate (car d))) (dy (truncate (cdr d))))
                (if (or (>= (min (abs dx) (abs dy)) max-delta) (and last (> (abs tt) last)))
                    (setq done t)
                  (let ((x (+ start-x dx)) (y (+ start-y dy)))
                    (when (and (>= (+ x left) 0) (>= (+ y top) 0)
                               (<= (+ x right) w) (<= (+ y bottom) h)
                               (not (eas-wordcloud-board-hits-p board sprite x y)))
                      (eas-wordcloud-board-add board sprite x y)
                      (aset out i (cons x y))
                      (setq done t))))))))))
    (append out nil)))

;;; The transform

(defun eas-wordcloud--field (name)
  "Row key of field NAME; a Vega \"datum.\" prefix is dropped."
  (eas-key (if (string-prefix-p "datum." name) (substring name 6) name)))

(defun eas-wordcloud--getter (value random)
  "Function (ROW INDEX) -> the per-word value VALUE describes.
VALUE is a constant, {\"field\": F}, {\"expr\": E} (random() in E is
deterministic per row) or, for angles, an array of choices one of which
RANDOM picks per word."
  (cond
   ((and (eas-object-p value) value (stringp (plist-get value :field)))
    (let ((key (eas-wordcloud--field (plist-get value :field))))
      (lambda (row _i) (plist-get row key))))
   ((and (eas-object-p value) value (stringp (plist-get value :expr)))
    (let ((expr (plist-get value :expr)))
      (lambda (row i)
        (eas-expr-evaluate expr (if (plist-member row :_eas_row) row (append (list :_eas_row i) row))))))
   ((and (vectorp value) (> (length value) 0))
    (lambda (_row _i) (aref value (min (1- (length value)) (truncate (* (funcall random) (length value)))))))
   (t (lambda (_row _i) value))))

(defun eas-wordcloud--size-scale (sizes range)
  "Function mapping a font size value onto RANGE [LO HI] by square root.
SIZES are every word's value, whose extent is the domain (Vega's sqrt
scale); with no RANGE values are used as they are."
  (if (not (and (vectorp range) (= (length range) 2)))
      #'identity
    (let* ((nums (seq-filter #'numberp sizes))
           (lo (if nums (sqrt (max 0 (apply #'min nums))) 0))
           (hi (if nums (sqrt (max 0 (apply #'max nums))) 0))
           (r0 (aref range 0)) (r1 (aref range 1)))
      (lambda (v)
        (cond ((not (numberp v)) r0)
              ((= lo hi) (/ (+ r0 r1) 2.0))
              (t (+ r0 (* (- r1 r0) (/ (- (sqrt (max 0 v)) lo) (- hi lo))))))))))

(defvar eas-wordcloud--memo (eas-memo-table 16)
  "Placed clouds by rows, parameters and registered fonts.
A view resolves its template again on every open and the doctor
resolves every example, and a cloud costs a spiral walk per word.")

(defun eas-wordcloud--transform (rows params)
  "The wordcloud domain transform: ROWS placed per PARAMS.
The result is cached (`eas-memo'); each call gets fresh row plists."
  (let ((fonts (mapcar (lambda (f) (list (plist-get f :family) (plist-get f :weight) (plist-get f :src)))
                       eas-font-file--faces)))
    (vconcat (mapcar #'copy-sequence
                     (eas-memo eas-wordcloud--memo (list rows params fonts)
                       (eas-wordcloud--layout rows params))))))

(defun eas-wordcloud--layout (rows params)
  "ROWS with the positions and fonts of their words placed per PARAMS."
  (let* ((random (eas-wordcloud-random (plist-get params :seed)))
         (size (plist-get params :size)) (w (aref size 0)) (h (aref size 1))
         (text-key (eas-wordcloud--field (plist-get params :text)))
         (get (lambda (key) (eas-wordcloud--getter (plist-get params key) random)))
         (font-of (funcall get :font)) (weight-of (funcall get :fontWeight))
         (style-of (funcall get :fontStyle)) (size-of (funcall get :fontSize))
         (angle-of (funcall get :rotate))
         (rows (append rows nil))
         (raw (seq-map-indexed (lambda (row i) (funcall size-of row i)) rows))
         (scale (eas-wordcloud--size-scale raw (plist-get params :fontSizeRange)))
         ;; Per word in row order, as d3-cloud reads them before it sorts.
         (words (cl-loop for row in rows for i from 0 for v in raw
                         collect (list :text (let ((s (plist-get row text-key)))
                                               (if (stringp s) s (eas-expr--string s)))
                                       :font (funcall font-of row i)
                                       :style (funcall style-of row i)
                                       :weight (funcall weight-of row i)
                                       :angle (let ((a (funcall angle-of row i))) (if (numberp a) a 0))
                                       :size (let ((s (funcall scale v))) (if (numberp s) (truncate s) 0)))))
         (font-name (lambda (f) (and (stringp f) (string-trim (car (split-string f ","))))))
         (placed (eas-wordcloud-place
                  (mapcar (lambda (word) (plist-put (copy-sequence word) :font (funcall font-name (plist-get word :font))))
                          words)
                  w h :padding (plist-get params :padding) :spiral (plist-get params :spiral) :random random))
         (as (mapcar #'eas-key (plist-get params :as))))
    (vconcat
     (cl-mapcar (lambda (row word at)
                  (let ((out (copy-sequence row)))
                    (cl-loop for key in as
                             for v in (list (if at (car at) :null) (if at (cdr at) :null)
                                            (plist-get word :font) (plist-get word :size)
                                            (plist-get word :style) (plist-get word :weight)
                                            (plist-get word :angle))
                             do (setq out (eas-plist-put out key v)))
                    out))
                rows words placed))))

(eas-register-transform
 "wordcloud"
 :doc "Spiral word-cloud placement of text rows: x, y, font, size, style, weight and angle (Vega's wordcloud)."
 :schema '(:text (:type "string" :default "text" :doc "the word field")
           :size (:type "array" :default [256 256] :doc "[width height] of the canvas, px")
           :font (:type "any" :default "sans-serif" :doc "font family, or {field} / {expr}")
           :fontStyle (:type "any" :default "normal" :doc "font style, or {field} / {expr}")
           :fontWeight (:type "any" :default "normal" :doc "font weight, or {field} / {expr}")
           :fontSize (:type "any" :default 14 :doc "font size, or {field} / {expr}")
           :fontSizeRange (:type "any" :default [10 50] :doc "[min max] px the sizes are scaled to (sqrt), or null")
           :rotate (:type "any" :default 0 :doc "angle in degrees, {field} / {expr}, or an array to pick from")
           :padding (:type "number" :default 1 :doc "px kept clear around each word")
           :spiral (:type "string" :default "archimedean" :doc "archimedean or rectangular")
           :seed (:type "integer" :default 0 :doc "seed of the start points, directions and picked angles")
           :as (:type "array" :default ["x" "y" "font" "fontSize" "fontStyle" "fontWeight" "angle"]
                    :doc "output fields"))
 :fn #'eas-wordcloud--transform)

(provide 'eas-wordcloud)
;;; eas-wordcloud.el ends here

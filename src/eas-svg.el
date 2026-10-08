;;; eas-svg.el --- scene/v1 -> SVG image with :map hot spots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L5, GUI half.  Draws a scene exactly as compiled; it reads only the
;; scene and the theme and never branches on chart kind.  Per the
;; fc-qx1.14 spike the DOM is consed directly (svg.el's per-node
;; append is quadratic) and every series is a single <path>.
;;
;; The theme is a Vega config object (the JSON bin/chart accepts),
;; overlaid on the config the scene was compiled with: background,
;; font, axis.{domain,tick,grid,label,title}Color and widths, font sizes
;; and weights, legend.{label,title}Color, title.color.  With no theme,
;; GUI frames map colors from Emacs faces; batch keeps the scene's.
;; Text sits on Vega's baselines (top 0.79em, middle 0.30em, bottom
;; -0.21em, rounded) and the generic "sans-serif" is drawn as Arial, the
;; font compile measured text with (eas-font.el).
;;
;; `eas-svg-image' adds :map hot spots for discrete items (bars,
;; points, text, legend entries) with help-echo and pointer; continuous
;; series hover by scale inversion through `eas-hit' instead.

;;; Code:

(require 'dom)
(require 'svg)
(require 'eas-core)
(require 'eas-render-cache)
(require 'eas-svg-retain)
(require 'eas-theme)
(require 'eas-paint)
(require 'eas-arc)
(require 'eas-axis-extra)
(require 'eas-symbols)
(require 'eas-marks-image)
(require 'eas-mark-style)
(require 'eas-legend-style)
(require 'eas-font-file)

(defcustom eas-svg-font-embed 'url
  "How an SVG carries the font files its scene registered (:fonts).
`url' links each file with a file: URL in an @font-face rule; `data'
inlines it base64-encoded, so an exported SVG is self-contained (and
larger); nil writes no @font-face.  Emacs draws SVG with librsvg,
which ignores @font-face and finds fonts through fontconfig: a frame
draws a registered family only when it is also installed."
  :type '(choice (const :tag "Link the font file" url)
                 (const :tag "Inline the font file" data)
                 (const :tag "No @font-face" nil))
  :group 'eas-font)

(defvar eas-svg--fragments nil
  "Non-nil while `eas-svg-render' may draw a mark as cached SVG text.")

(defun eas-svg--face-color (face attribute)
  "FACE's ATTRIBUTE color as a string, or nil when unspecified."
  (let ((c (face-attribute face attribute nil t)))
    (and (stringp c) (not (string-prefix-p "unspecified" c))
         (if (string-prefix-p "#" c) c
           (when-let* ((rgb (color-values c)))
             (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb)))))))

(defun eas-svg-theme-from-faces ()
  "A Vega config object mapped from the current Emacs faces."
  (let ((fg (or (eas-svg--face-color 'default :foreground) "#000"))
        (bg (or (eas-svg--face-color 'default :background) "white"))
        (dim (or (eas-svg--face-color 'shadow :foreground) "#888")))
    (list :background bg :font (or (face-attribute 'default :family nil t) "sans-serif")
          :axis (list :domainColor dim :tickColor dim :gridColor dim :gridOpacity 0.3
                      :labelColor fg :titleColor fg)
          :legend (list :labelColor fg :titleColor fg)
          :title (list :color fg))))

(defun eas-svg--theme (theme scene)
  "The config SCENE was compiled with, under GUI face colors or THEME."
  (eas-theme-merge (or (plist-get scene :config) eas-theme-default)
                     (when-let* ((bg (plist-get scene :background))) (list :background bg))
                     (and (null theme) (display-graphic-p) (eas-svg-theme-from-faces))
                     theme))

(defun eas-svg--font (font)
  "SVG font-family for theme FONT.
The generic families resolve as in bin/chart's font database (usvg's
defaults): sans-serif to Arial, serif to Times New Roman and monospace
to Courier New, each with its metric-compatible Liberation font."
  (pcase font
    ((or 'nil "sans-serif") "Arial, Liberation Sans, sans-serif")
    ("serif" "Times New Roman, Liberation Serif, serif")
    ("monospace" "Courier New, Liberation Mono, monospace")
    (_ font)))

(defvar eas-svg--n-cache (make-hash-table :test 'eql :size 4096)
  "Floats formatted by `eas-svg--n', by value.")

(defvar eas-svg-n-cache-max 65536
  "Floats `eas-svg--n-cache' holds before it is emptied.")

(defun eas-svg--n (v)
  "Format number V compactly for SVG attributes."
  (if (integerp v) (number-to-string v)
    ;; Pixel coordinates repeat from frame to frame: `format' is the
    ;; cost of a float, a lookup a tenth of it.
    (or (gethash v eas-svg--n-cache)
        (progn
          (when (>= (hash-table-count eas-svg--n-cache) eas-svg-n-cache-max)
            (clrhash eas-svg--n-cache))
          ;; "%.2f" always has two decimals: drop ".00" or a trailing "0"
          ;; (a regexp here dominated rendering long paths).
          (puthash v (let ((s (format "%.2f" v)))
                       (cond ((string-suffix-p ".00" s) (substring s 0 -3))
                             ((eq (aref s (1- (length s))) ?0) (substring s 0 -1))
                             (t s)))
                   eas-svg--n-cache)))))

(defun eas-svg--escape (text)
  "Escape TEXT for XML."
  (let ((s (if (stringp text) text (format "%s" text))))
    (if (not (string-match-p "[&<>\"]" s)) s
      (replace-regexp-in-string
       "[&<>\"]" (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;")))
       s t t))))

(defvar eas-svg--attr-names (make-hash-table :test 'eq)
  "Attribute symbols by keyword: :fill to fill.")

(defun eas-svg--node (tag &rest attrs)
  "DOM node TAG with ATTRS (a plist, nil values dropped) and no children."
  (dom-node tag (cl-loop for (k v) on attrs by #'cddr
                         when v collect (cons (or (gethash k eas-svg--attr-names)
                                                  (puthash k (intern (substring (symbol-name k) 1))
                                                           eas-svg--attr-names))
                                              (if (numberp v) (eas-svg--n v) (eas-svg--escape v))))))

(defun eas-svg--anchor (align)
  "SVG text-anchor for scene ALIGN."
  (pcase align ("left" "start") ("right" "end") (_ "middle")))

(defun eas-svg--text (text x y size &rest props)
  "A <text> node for TEXT at X Y with font SIZE.
PROPS: :align :baseline :angle :fill :weight :opacity, and :font,
:style (font style) and :line-height, and :lines-down
\(the first line on the anchor, the rest below)."
  (let* ((baseline (plist-get props :baseline))
         (dy (floor (+ 0.5 (* size (pcase baseline ("top" 0.79) ("middle" 0.30) ("bottom" -0.21) (_ 0))))))
         (angle (or (plist-get props :angle) 0))
         (node (eas-svg--node 'text :x x :y (+ y (if (zerop angle) dy 0))
                                :dy (unless (zerop angle) (eas-svg--n dy))
                                :font-size size :fill (plist-get props :fill)
                                :font-weight (let ((w (plist-get props :weight))) (and w (format "%s" w)))
                                :font-family (plist-get props :font) :font-style (plist-get props :style)
                                :opacity (plist-get props :opacity)
                                :text-anchor (eas-svg--anchor (plist-get props :align))
                                :transform (unless (zerop angle)
                                             (format "rotate(%s %s %s)" (eas-svg--n angle)
                                                     (eas-svg--n x) (eas-svg--n y))))))
    (if (not (string-search "\n" text))
        (append node (list (eas-svg--escape text)))
      ;; Multi-line text: one tspan per line, raised per the baseline.
      (let* ((lines (split-string text "\n")) (lh (or (plist-get props :line-height) (+ size 2)))
             (lift (pcase (if (plist-get props :lines-down) "top" baseline) ("top" 0) ("middle" (/ (* lh (1- (length lines))) 2.0))
                     (_ (* lh (1- (length lines)))))))
        (append node
                (seq-map-indexed (lambda (line i)
                                   (append (eas-svg--node 'tspan :x x :dy (eas-svg--n (if (zerop i) (- lift) lh)))
                                           (list (eas-svg--escape line))))
                                 lines))))))

(defun eas-svg--line (seg color &optional width opacity dash cap)
  "A <line> for SEG [x1 y1 x2 y2] in COLOR (CAP its stroke-linecap).
WIDTH (default 1), OPACITY and DASH set its stroke width, opacity and
dash array."

  (eas-svg--node 'line :x1 (aref seg 0) :y1 (aref seg 1) :x2 (aref seg 2) :y2 (aref seg 3)
                   :stroke color :stroke-width (or width 1) :stroke-opacity opacity
                   :stroke-dasharray (and dash (mapconcat #'eas-svg--n dash ","))
                   :stroke-linecap cap))

(defun eas-svg--path (points &optional base)
  "SVG path data through POINTS, closing along BASE reversed when given."
  (concat (mapconcat (lambda (p) (concat (eas-svg--n (aref p 0)) "," (eas-svg--n (aref p 1))))
                     points "L")
          (when base
            (concat "L" (mapconcat (lambda (p) (concat (eas-svg--n (aref p 0)) "," (eas-svg--n (aref p 1))))
                                   (reverse base) "L")
                    "Z"))))

(defun eas-svg--trail (points widths)
  "Path data for a trail through POINTS with full WIDTHS per vertex.
Each segment is the hull of its end discs, so joints and ends are round."
  (let ((n #'eas-svg--n) (out nil))
    (dotimes (i (length points))
      (let* ((p (aref points i)) (r (/ (aref widths i) 2.0)))
        (push (format "M%s,%sa%s,%s 0 1 0 %s,0a%s,%s 0 1 0 %s,0Z" (funcall n (- (aref p 0) r)) (funcall n (aref p 1))
                      (funcall n r) (funcall n r) (funcall n (* 2 r)) (funcall n r) (funcall n r) (funcall n (* -2 r)))
              out)
        (when (< (1+ i) (length points))
          (let* ((q (aref points (1+ i))) (s (/ (aref widths (1+ i)) 2.0))
                 (dx (- (aref q 0) (aref p 0))) (dy (- (aref q 1) (aref p 1))) (len (sqrt (+ (* dx dx) (* dy dy)))))
            (when (> len 0)
              (let ((ux (/ (- dy) len)) (uy (/ dx len)))
                (push (format "M%s,%sL%s,%sL%s,%sL%s,%sZ"
                              (funcall n (+ (aref p 0) (* ux r))) (funcall n (+ (aref p 1) (* uy r)))
                              (funcall n (+ (aref q 0) (* ux s))) (funcall n (+ (aref q 1) (* uy s)))
                              (funcall n (- (aref q 0) (* ux s))) (funcall n (- (aref q 1) (* uy s)))
                              (funcall n (- (aref p 0) (* ux r))) (funcall n (- (aref p 1) (* uy r))))
                      out)))))))
    (apply #'concat (nreverse out))))

(defun eas-svg--rounded-rect (x y w h corners)
  "Path data for the W x H rect at X Y with CORNERS [TL TR BR BL] radii."
  (let* ((lim (/ (min w h) 2.0))
         (c (mapcar (lambda (r) (min r lim)) (append corners nil)))
         (tl (nth 0 c)) (tr (nth 1 c)) (br (nth 2 c)) (bl (nth 3 c))
         (n #'eas-svg--n))
    (concat "M" (funcall n (+ x tl)) "," (funcall n y)
            "H" (funcall n (- (+ x w) tr))
            (if (> tr 0) (format "A%s,%s 0 0 1 %s,%s" (funcall n tr) (funcall n tr) (funcall n (+ x w)) (funcall n (+ y tr))) "")
            "V" (funcall n (- (+ y h) br))
            (if (> br 0) (format "A%s,%s 0 0 1 %s,%s" (funcall n br) (funcall n br) (funcall n (- (+ x w) br)) (funcall n (+ y h))) "")
            "H" (funcall n (+ x bl))
            (if (> bl 0) (format "A%s,%s 0 0 1 %s,%s" (funcall n bl) (funcall n bl) (funcall n x) (funcall n (- (+ y h) bl))) "")
            "V" (funcall n (+ y tl))
            (if (> tl 0) (format "A%s,%s 0 0 1 %s,%s" (funcall n tl) (funcall n tl) (funcall n (+ x tl)) (funcall n y)) "")
            "Z")))

(defun eas-svg--symbol (shape x y size &rest attrs)
  "A Vega symbol of SHAPE and area SIZE centred on X Y, with ATTRS.
SHAPE is circle, square, another Vega symbol (`eas-symbols-path') or
SVG path data.  ATTRS may hold :angle, degrees clockwise."
  (let ((r (/ (sqrt (max 0 size)) 2.0)) (angle (plist-get attrs :angle))
        (attrs (eas--plist-without attrs :angle)))
    (cond
     ((and (equal shape "square") (not angle))
      (apply #'eas-svg--node 'rect :x (- x r) :y (- y r) :width (* 2 r) :height (* 2 r) attrs))
     ((eas-symbols-path shape x y size angle)
      (apply #'eas-svg--node 'path :d (eas-symbols-path shape x y size angle) attrs))
     ((and (stringp shape) (string-match-p "\\`[ \t]*[Mm]" shape))
      ;; Vega draws a path symbol at sqrt(size)/2 per unit.
      (let ((sw (plist-get attrs :stroke-width)))
        (apply #'eas-svg--node 'path :d shape
               :transform (format "translate(%s,%s) scale(%s)" (eas-svg--n x) (eas-svg--n y) (eas-svg--n r))
               (if (and sw (> r 0)) (plist-put (copy-sequence attrs) :stroke-width (/ sw r)) attrs))))
     (t (apply #'eas-svg--node 'circle :cx x :cy y :r r attrs)))))

(declare-function eas-geoshape-svg "eas-geoshape-render")

(defun eas-svg--item (mark item)
  "SVG node for ITEM of MARK."
  (let ((fill (eas-paint-svg-fill item)) (stroke (plist-get item :stroke))
        (opacity (let ((o (plist-get item :opacity))) (and o (/= o 1) o))))
    (pcase (plist-get mark :mark)
      ((or "bar" "rect" "brush")
       (eas-svg--styled
        (if-let* ((corners (plist-get item :corners)))
            (eas-svg--node 'path :d (eas-svg--rounded-rect (plist-get item :x) (plist-get item :y)
                                                           (max 0 (plist-get item :w)) (max 0 (plist-get item :h)) corners)
                           :fill fill :stroke (unless (equal stroke "none") stroke) :opacity opacity)
          (eas-svg--node 'rect :x (plist-get item :x) :y (plist-get item :y)
                         :width (max 0 (plist-get item :w)) :height (max 0 (plist-get item :h))
                         :fill fill :stroke (unless (equal stroke "none") stroke) :opacity opacity))
        item t))
      ("image"
       (eas-svg--node 'image :x (plist-get item :x) :y (plist-get item :y)
                      :width (plist-get item :w) :height (plist-get item :h)
                      :preserveAspectRatio (if (eq (plist-get item :aspect) :false) "none" "xMidYMid")
                      :href (eas-marks-image-href (plist-get item :url)) :opacity opacity))
      ((or "rule" "tick")
       (eas-svg--line (vector (plist-get item :x1) (plist-get item :y1) (plist-get item :x2) (plist-get item :y2))
                        stroke (plist-get item :strokeWidth) opacity (plist-get item :strokeDash) (plist-get item :strokeCap)))
      ("geoshape" (eas-geoshape-svg item fill stroke opacity))
      ("arc" (eas-mark-style-svg
              (eas-svg--node 'path :d (eas-arc-path item) :fill fill
                             :stroke (unless (equal stroke "none") stroke)
                             :stroke-width (unless (equal stroke "none") (plist-get item :strokeWidth))
                             :opacity opacity)
              item))
      ("text" (apply #'eas-svg--text (plist-get item :text) (plist-get item :x) (plist-get item :y)
                     (plist-get item :fontSize)
                     (list :align (plist-get item :align) :baseline (plist-get item :baseline) :fill fill
                           :opacity opacity :weight (plist-get item :fontWeight) :angle (plist-get item :angle)
                           :font (eas-svg--font-name (plist-get item :font)) :style (plist-get item :fontStyle)
                           :line-height (plist-get item :lineHeight)
                           ;; Vega draws a text mark's first line on the anchor, the rest below it.
                           :lines-down t)))
      ("trail" (eas-mark-style-svg
                (eas-svg--node 'path :d (eas-svg--trail (plist-get item :points) (plist-get item :widths))
                               :fill (if (equal fill "none") stroke fill) :opacity opacity)
                ;; A trail is filled: its stroke properties do not apply.
                (eas--plist-without (eas--plist-without (eas--plist-without item :strokeCap) :strokeJoin) :strokeDash)))
      ((or "line" "area")
       (let ((area (plist-get item :base)))
         (eas-mark-style-svg
          (eas-svg--node 'path :d (concat "M" (eas-svg--path (plist-get item :points) area))
                           :fill (if area fill (or fill "none")) :stroke (unless (or area (equal stroke "none")) stroke)
                           :stroke-width (unless area (plist-get item :strokeWidth))
                           :stroke-linecap (unless area (plist-get item :strokeCap))
                           :stroke-linejoin (unless area (plist-get item :strokeJoin))
                           :stroke-dasharray (and (plist-get item :strokeDash)
                                                  (mapconcat #'eas-svg--n (plist-get item :strokeDash) ","))
                           :opacity opacity)
          item)))
      (_ (eas-svg--styled
          (eas-svg--symbol (plist-get item :shape) (plist-get item :x) (plist-get item :y) (plist-get item :size)
                           :angle (plist-get item :angle) :fill fill :stroke (unless (equal stroke "none") stroke)
                           :stroke-width (unless (equal stroke "none") (plist-get item :strokeWidth))
                           :opacity opacity)
          item)))))

;;; Direct printers

;; The common item kinds print straight to the string `eas-svg--item''s
;; node prints to, with no DOM node, no `apply' and no `format' per
;; attribute name.  Each mirrors its branch of `eas-svg--item' (and
;; `eas-svg-retain--pieces'); `eas-svg-retain-direct-printers-print-nodes'
;; checks them against it.  Pieces are pushed last first.

(defsubst eas-svg--a (name v acc)
  "Push attribute NAME (\" x=\\\"\") with value V onto ACC; ACC when V is nil.
V is formatted as `eas-svg--node' does."
  (if v (cl-list* "\"" (if (numberp v) (eas-svg--n v) (eas-svg--escape v)) name acc) acc))

(defconst eas-svg--styled-attrs
  '((:fillOpacity . " fill-opacity=\"") (:strokeOpacity . " stroke-opacity=\"")
    (:strokeDash . " stroke-dasharray=\"") (:strokeCap . " stroke-linecap=\"")
    (:strokeJoin . " stroke-linejoin=\""))
  "The attributes `eas-svg--styled' adds, in its order.")

(defun eas-svg--styled-pieces (item width acc)
  "Push the attributes `eas-svg--styled' gives ITEM (and WIDTH) onto ACC."
  (dolist (a (if width (append eas-svg--styled-attrs '((:strokeWidth . " stroke-width=\"")))
               eas-svg--styled-attrs))
    (let ((v (plist-get item (car a))))
      (cond ((numberp v) (setq acc (cl-list* "\"" (eas-svg--n v) (cdr a) acc)))
            ((stringp v) (setq acc (cl-list* "\"" v (cdr a) acc)))
            ((vectorp v) (setq acc (cl-list* "\"" (mapconcat #'eas-svg--n v ",") (cdr a) acc))))))
  acc)

(defun eas-svg--close (acc tag)
  "ACC, an element's pieces last first, closed with no children as TAG."
  (apply #'concat (nreverse (cons tag acc))))

(defun eas-svg--rect-string (item fill stroke opacity)
  "The printed <rect> of bar ITEM (no corners) with FILL STROKE OPACITY."
  (let ((acc (list "<rect")))
    (setq acc (eas-svg--a " x=\"" (plist-get item :x) acc)
          acc (eas-svg--a " y=\"" (plist-get item :y) acc)
          acc (eas-svg--a " width=\"" (max 0 (plist-get item :w)) acc)
          acc (eas-svg--a " height=\"" (max 0 (plist-get item :h)) acc)
          acc (eas-svg--a " fill=\"" fill acc)
          acc (eas-svg--a " stroke=\"" (unless (equal stroke "none") stroke) acc)
          acc (eas-svg--a " opacity=\"" opacity acc))
    (eas-svg--close (eas-svg--styled-pieces item t acc) "></rect>")))

(defun eas-svg--line-string (item stroke opacity)
  "The printed <line> of rule ITEM with STROKE and OPACITY."
  (let ((acc (list "<line")) (dash (plist-get item :strokeDash)))
    (setq acc (eas-svg--a " x1=\"" (plist-get item :x1) acc)
          acc (eas-svg--a " y1=\"" (plist-get item :y1) acc)
          acc (eas-svg--a " x2=\"" (plist-get item :x2) acc)
          acc (eas-svg--a " y2=\"" (plist-get item :y2) acc)
          acc (eas-svg--a " stroke=\"" stroke acc)
          acc (eas-svg--a " stroke-width=\"" (or (plist-get item :strokeWidth) 1) acc)
          acc (eas-svg--a " stroke-opacity=\"" opacity acc)
          acc (eas-svg--a " stroke-dasharray=\"" (and dash (mapconcat #'eas-svg--n dash ",")) acc)
          acc (eas-svg--a " stroke-linecap=\"" (plist-get item :strokeCap) acc))
    (eas-svg--close acc "></line>")))

(defun eas-svg--symbol-string (item fill stroke opacity)
  "The printed symbol of point ITEM with FILL STROKE OPACITY, or nil.
Nil for a symbol drawn from SVG path data, left to `eas-svg--item'."
  (let* ((shape (plist-get item :shape)) (x (plist-get item :x)) (y (plist-get item :y))
         (size (plist-get item :size)) (angle (plist-get item :angle))
         (r (/ (sqrt (max 0 size)) 2.0))
         (square-p (and (equal shape "square") (not angle)))
         (d (and (not square-p) (eas-symbols-path shape x y size angle))))
    (unless (and (not square-p) (not d) (stringp shape) (string-match-p "\\`[ \t]*[Mm]" shape))
      (let ((acc (cond (square-p (eas-svg--a " height=\"" (* 2 r)
                                           (eas-svg--a " width=\"" (* 2 r)
                                                       (eas-svg--a " y=\"" (- y r)
                                                                   (eas-svg--a " x=\"" (- x r) (list "<rect"))))))
                       (d (eas-svg--a " d=\"" d (list "<path")))
                       (t (eas-svg--a " r=\"" r (eas-svg--a " cy=\"" y (eas-svg--a " cx=\"" x (list "<circle")))))))
            (none (equal stroke "none")))
        (setq acc (eas-svg--a " fill=\"" fill acc)
              acc (eas-svg--a " stroke=\"" (unless none stroke) acc)
              acc (eas-svg--a " stroke-width=\"" (unless none (plist-get item :strokeWidth)) acc)
              acc (eas-svg--a " opacity=\"" opacity acc))
        (eas-svg--close (eas-svg--styled-pieces item nil acc)
                        (cond (square-p "></rect>") (d "></path>") (t "></circle>")))))))

(defun eas-svg--text-string (item fill opacity)
  "The printed <text> of one-line text ITEM with FILL and OPACITY, or nil."
  (let ((text (plist-get item :text)))
    (when (and (stringp text) (not (string-search "\n" text)))
      (let* ((x (plist-get item :x)) (y (plist-get item :y)) (size (plist-get item :fontSize))
             (dy (floor (+ 0.5 (* size (pcase (plist-get item :baseline)
                                         ("top" 0.79) ("middle" 0.30) ("bottom" -0.21) (_ 0))))))
             (angle (or (plist-get item :angle) 0))
             (flat (zerop angle))
             (w (plist-get item :fontWeight))
             (acc (list "<text")))
        (setq acc (eas-svg--a " x=\"" x acc)
              acc (eas-svg--a " y=\"" (+ y (if flat dy 0)) acc)
              acc (eas-svg--a " dy=\"" (unless flat (eas-svg--n dy)) acc)
              acc (eas-svg--a " font-size=\"" size acc)
              acc (eas-svg--a " fill=\"" fill acc)
              acc (eas-svg--a " font-weight=\"" (and w (format "%s" w)) acc)
              acc (eas-svg--a " font-family=\"" (eas-svg--font-name (plist-get item :font)) acc)
              acc (eas-svg--a " font-style=\"" (plist-get item :fontStyle) acc)
              acc (eas-svg--a " opacity=\"" opacity acc)
              acc (eas-svg--a " text-anchor=\"" (eas-svg--anchor (plist-get item :align)) acc)
              acc (eas-svg--a " transform=\"" (unless flat
                                                (format "rotate(%s %s %s)" (eas-svg--n angle)
                                                        (eas-svg--n x) (eas-svg--n y)))
                              acc))
        (eas-svg--close (cons (eas-svg--escape text) (cons ">" acc)) "</text>")))))

(defun eas-svg--item-string (kind item)
  "ITEM of a mark of KIND printed directly, or nil when no printer has it.
The string is what `eas-svg--item''s node prints to.  An item with a
gradient fill records a definition as it prints: nil."
  (unless (plist-get item :gradient)
    (let* ((fill (plist-get item :fill)) (stroke (plist-get item :stroke))
           (opacity (let ((o (plist-get item :opacity))) (and o (/= o 1) o))))
      (pcase kind
        ((or "bar" "rect" "brush")
         (unless (plist-get item :corners) (eas-svg--rect-string item fill stroke opacity)))
        ((or "rule" "tick") (eas-svg--line-string item stroke opacity))
        ((or "point" "circle" "square") (eas-svg--symbol-string item fill stroke opacity))
        ("text" (eas-svg--text-string item fill opacity))))))

(defun eas-svg--styled (node item &optional width)
  "NODE with ITEM's fillOpacity and strokeOpacity (and strokeWidth when WIDTH)."
  (let ((extra (cl-loop for (key attr) in (append '((:fillOpacity fill-opacity) (:strokeOpacity stroke-opacity)
                                                    (:strokeDash stroke-dasharray) (:strokeCap stroke-linecap)
                                                    (:strokeJoin stroke-linejoin))
                                                  (when width '((:strokeWidth stroke-width))))
                        for v = (plist-get item key)
                        when (numberp v) collect (cons attr (eas-svg--n v))
                        when (stringp v) collect (cons attr v)
                        when (vectorp v) collect (cons attr (mapconcat #'eas-svg--n v ",")))))
    (if extra (cl-list* (car node) (append (cadr node) extra) (cddr node)) node)))

(defun eas-svg--dash (dash)
  "DASH (a config dash array) when it is one, else nil."
  (and (vectorp dash) (> (length dash) 0) dash))

(defun eas-svg--font-name (font)
  "SVG font-family for an axis or legend FONT property, or nil."
  (and (stringp font) (eas-svg--font font)))

(defun eas-svg-frame-offset (frame view)
  "Vega's group stroke offset for a view FRAME under config VIEW.
Half a pixel for a stroke about 1 px wide, so it lands on whole pixels."
  (let ((sw (or (plist-get view :strokeWidth) 1)))
    (if (and (stringp (plist-get frame :stroke)) (numberp sw) (< 0.5 sw 1.5)) (- 0.5 (abs (- sw 1))) 0)))

(defun eas-svg--axis (axis theme)
  "SVG nodes for placed AXIS under THEME."
  (let* ((horizontal (member (plist-get axis :orient) '("bottom" "top")))
         (channel (if horizontal :x :y))
         ;; An axis's own style (eas-axis.el) overrides the theme.
         (style (plist-get axis :style))
         (get (lambda (key) (if (plist-member style key) (plist-get style key) (eas-theme-axis theme channel key))))
         out)
    (seq-doseq (tk (plist-get axis :ticks))
      (when (plist-get tk :grid)
        (push (eas-svg--line (plist-get tk :grid) (or (plist-get tk :grid-color) (funcall get :gridColor))
                               (or (funcall get :gridWidth) 1)
                               (funcall get :gridOpacity) (or (plist-get tk :grid-dash) (eas-svg--dash (funcall get :gridDash)))
                               (funcall get :gridCap))
              out)))
    (when-let* ((domain (plist-get axis :domain-line)))
      (unless (or (plist-get axis :domain-off) (plist-get axis :no-domain))
        (push (eas-svg--line domain (funcall get :domainColor) (or (funcall get :domainWidth) 1)
                             (funcall get :domainOpacity) (eas-svg--dash (funcall get :domainDash)) (funcall get :domainCap))
              out)))
    (seq-doseq (tk (plist-get axis :ticks))
      (unless (or (equal (plist-get axis :tickSize) 0) (null (plist-get tk :tick)))
        (when-let* ((color (eas-axis-extra-tick-color axis tk (funcall get :tickColor))))
          (push (eas-svg--line (plist-get tk :tick) color (or (funcall get :tickWidth) 1)
                               (funcall get :tickOpacity) (or (plist-get tk :tick-dash) (eas-svg--dash (funcall get :tickDash)))
                               (funcall get :tickCap))
                out)))
      (unless (string-empty-p (plist-get tk :label))
        (push (eas-svg--text (plist-get tk :label) (plist-get tk :lx) (plist-get tk :ly)
                               (or (funcall get :labelFontSize) 10)
                               :align (plist-get tk :align) :baseline (plist-get tk :baseline)
                               :angle (if horizontal (plist-get axis :labelAngle) 0)
                               :weight (funcall get :labelFontWeight)
                               :font (eas-svg--font-name (funcall get :labelFont)) :style (funcall get :labelFontStyle)
                               :opacity (funcall get :labelOpacity)
                               :fill (or (plist-get tk :label-color) (funcall get :labelColor)))
              out)))
    (when-let* ((tm (plist-get axis :title-mark)))
      (push (eas-svg--text (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) (or (funcall get :titleFontSize) 11)
                             :align (plist-get tm :align) :baseline (plist-get tm :baseline)
                             :angle (plist-get tm :angle) :weight (or (funcall get :titleFontWeight) "bold")
                             :font (eas-svg--font-name (funcall get :titleFont)) :style (funcall get :titleFontStyle)
                             :opacity (funcall get :titleOpacity)
                             :fill (funcall get :titleColor))
            out))
    (nreverse out)))

(defun eas-svg--axis-parts (axis theme &optional slot)
  "AXIS's nodes under THEME as (GRID . REST), grid lines first.
While `eas-svg--fragments' is set each part is its printed SVG text,
retained between frames in SLOT (`eas-svg-retain-part')."
  (cl-flet ((split ()
              (let ((n (seq-count (lambda (tk) (plist-get tk :grid)) (plist-get axis :ticks)))
                    (nodes (eas-svg--axis axis theme)))
                (cons (seq-take nodes n) (seq-drop nodes n)))))
    (if (not eas-svg--fragments) (split)
      (eas-svg-retain-part slot (cons axis theme)
                           (lambda ()
                             (pcase-let ((`(,grid . ,rest) (split)))
                               (cons (and grid (list (eas-svg-retain-string grid)))
                                     (and rest (list (eas-svg-retain-string rest))))))))))

(defun eas-svg--axes (axes theme &optional view-id)
  "Return the SVG nodes of AXES under THEME as (BEHIND . FRONT).
BEHIND goes under the marks, FRONT over them.  Every grid lies beneath
every axis, as Vega-Lite puts the grids in axes of their own ahead of
the others.  An axis's zindex above 0 draws it in front; its grid
follows :grid-zindex when it has one.  VIEW-ID names the slots their
printed text is retained in."
  (let ((z (lambda (a key) (> (or (plist-get a key) 0) 0)))
        grids-back grids-front back front)
    (seq-do-indexed
     (lambda (axis i)
       (pcase-let ((`(,grid . ,rest) (eas-svg--axis-parts axis theme (list 'axis view-id i))))
         (if (funcall z axis (if (plist-member axis :grid-zindex) :grid-zindex :zindex))
             (push grid grids-front)
           (push grid grids-back))
         (if (funcall z axis :zindex) (push rest front) (push rest back))))
     axes)
    (cl-flet ((join (parts) (apply #'append (nreverse parts))))
      (cons (append (join grids-back) (join back)) (append (join grids-front) (join front))))))

(defun eas-svg--gradient-id (legend)
  "Stable id of LEGEND's gradient definition."
  (format "grad-%s-%s%s" (plist-get legend :channel) (abs (sxhash-equal (plist-get legend :stops)))
          (if (equal (plist-get legend :direction) "horizontal") "-h" "")))

(defun eas-svg--gradient-def (legend)
  "A vertical <linearGradient> for LEGEND's color stops (high values on top)."
  (let* ((stops (plist-get legend :stops)) (n (length stops)))
    (append (if (equal (plist-get legend :direction) "horizontal")
                (eas-svg--node 'linearGradient :id (eas-svg--gradient-id legend) :x1 0 :y1 0 :x2 1 :y2 0)
              (eas-svg--node 'linearGradient :id (eas-svg--gradient-id legend) :x1 0 :y1 1 :x2 0 :y2 0))
            (seq-map-indexed (lambda (c i) (eas-svg--node 'stop :offset (/ i (float (max 1 (1- n)))) :stop-color c))
                             stops))))

(defun eas-svg--clip-id (box)
  "Id of the clip path for legend symbol BOX [X Y W H]."
  (format "lclip-%x" (abs (sxhash-equal box))))

(defun eas-svg--legend (legend theme)
  "SVG nodes for placed LEGEND under THEME."
  (let ((get (lambda (key) (eas-legend-style-paint legend (lambda (k) (eas-theme-get theme :legend k)) key)))
        (fs (or (plist-get legend :font-size) 10)) out)
    (when-let* ((tm (plist-get legend :title-mark)))
      (push (eas-svg--text (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) (or (funcall get :titleFontSize) 11)
                             :align "left" :baseline "top" :weight (or (funcall get :titleFontWeight) "bold")
                             :font (eas-svg--font-name (funcall get :titleFont)) :style (funcall get :titleFontStyle)
                             :opacity (funcall get :titleOpacity) :fill (funcall get :titleColor))
            out))
    (when-let* ((bar (plist-get legend :bar)))
      (push (eas-svg--node 'rect :x (aref bar 0) :y (aref bar 1) :width (aref bar 2) :height (aref bar 3)
                             :fill (format "url(#%s)" (eas-svg--gradient-id legend)))
            out))
    (seq-doseq (e (plist-get legend :entries))
      (when-let* ((c (plist-get e :clip)))
        (push (dom-node 'clipPath `((id . ,(eas-svg--clip-id c)))
                        (eas-svg--node 'rect :x (aref c 0) :y (aref c 1) :width (aref c 2) :height (aref c 3)))
              out))
      (when (plist-get e :size)
        (push (eas-svg--symbol (or (plist-get e :shape) (plist-get legend :symbol-type)) (plist-get e :sx) (plist-get e :sy) (plist-get e :size)
                                 :clip-path (and (plist-get e :clip) (format "url(#%s)" (eas-svg--clip-id (plist-get e :clip))))
                                 :fill (plist-get e :fill) :fill-opacity (plist-get e :fill-opacity)
                                 :stroke (plist-get e :stroke)
                                 :stroke-width (and (plist-get e :stroke) (plist-get e :stroke-width))
                                 :stroke-dasharray (and (plist-get e :dash) (mapconcat #'eas-svg--n (plist-get e :dash) ","))
                                 :opacity (let ((o (plist-get e :opacity))) (and o (/= o 1) o)))
              out))
      (push (eas-svg--text (plist-get e :label) (plist-get e :lx) (plist-get e :ly) fs :align (or (plist-get e :align) "left")
                             :baseline (or (plist-get e :baseline) "middle") :fill (funcall get :labelColor)
                             :weight (funcall get :labelFontWeight) :font (eas-svg--font-name (funcall get :labelFont))
                             :style (funcall get :labelFontStyle) :opacity (funcall get :labelOpacity))
            out))
    (nreverse out)))

(defun eas-svg--legend-nodes (legend theme view-id)
  "LEGEND's nodes under THEME, a legend of view VIEW-ID.
While `eas-svg--fragments' is set they are its printed SVG text,
retained between frames (`eas-svg-retain-part')."
  (if (not eas-svg--fragments) (eas-svg--legend legend theme)
    (eas-svg-retain-part (list 'legend view-id (plist-get legend :channel)) (cons legend theme)
                         (lambda () (list (eas-svg-retain-string (eas-svg--legend legend theme)))))))

(defun eas-svg--item-print (kind mark item)
  "ITEM of MARK (of KIND) as printed SVG text, or nil when it draws nothing.
A direct printer's string when one has the item, else the retained
print of its node (`eas-svg-retain-item')."
  (or (eas-svg--item-string kind item)
      (eas-svg-retain-item kind item (lambda () (eas-svg--item mark item)))))

(defun eas-svg--mark-nodes (mark)
  "SVG nodes of MARK's items.
While `eas-svg--fragments' is set each is its printed SVG text
\=(`eas-svg--item-print')."
  (let ((kind (plist-get mark :mark)))
    (delq nil (mapcar (lambda (item)
                        ;; Fully transparent items draw nothing.
                        (unless (equal (plist-get item :opacity) 0)
                          (if (and eas-svg--fragments (not (equal kind "image")))
                              (eas-svg--item-print kind mark item)
                            (eas-svg--item mark item))))
                      (plist-get mark :items)))))

(defun eas-svg--mark-strings (mark prev-items prev-strings)
  "MARK's items printed, as (STRINGS . PER-ITEM).
STRINGS is the list of SVG texts in order; PER-ITEM a vector, item by
item, of its text or nil.  An item `equal' to the item at its index in
PREV-ITEMS (the last version of the mark) takes its text from
PREV-STRINGS, so a frame that changed a few items prints only those."
  (let* ((kind (plist-get mark :mark))
         (items (let ((v (plist-get mark :items))) (if (vectorp v) v (vconcat v))))
         (n (length items)) (per (make-vector n nil)) (out nil)
         (np (if (vectorp prev-items) (min (length prev-items) (length prev-strings)) 0))
         (reused 0))
    (dotimes (i n)
      (let* ((item (aref items i))
             ;; Fully transparent items draw nothing.
             (s (unless (equal (plist-get item :opacity) 0)
                  (if (and (< i np) (not (plist-get item :gradient)) (equal item (aref prev-items i)))
                      (progn (setq reused (1+ reused)) (aref prev-strings i))
                    (eas-svg--item-print kind mark item)))))
        (aset per i s)
        (when s (push s out))))
    (eas-svg-retain-count-reused reused)
    (cons (nreverse out) per)))

(defun eas-svg--mark-children (mark &optional view-id)
  "MARK's children of its view's group: its nodes, or their SVG text.
While `eas-svg--fragments' is set the text is retained in a slot of
view VIEW-ID (`eas-svg-retain-mark'); svg-print inserts it as it is."
  (if (and eas-svg--fragments eas-render-cache-enabled
           (not (equal (plist-get mark :mark) "image")))
      (list (eas-svg-retain-mark (list 'mark view-id (plist-get mark :id)) mark
                                 (lambda (items strings) (eas-svg--mark-strings mark items strings))
                                 (eas-render-cache-dirty-p view-id mark)))
    (eas-svg--mark-nodes mark)))

(defun eas-svg-dom (scene &optional theme)
  "Return the SVG DOM for SCENE under THEME (a Vega config plist)."
  (let* ((theme (eas-svg--theme theme scene))
         (size (plist-get scene :size))
         (eas-paint--svg-defs nil)
         (children nil) (defs nil))
    (push (eas-svg--node 'rect :width "100%" :height "100%"
                           :fill (or (plist-get theme :background) "white"))
          children)
    (seq-doseq (view (plist-get scene :views))
      (let* ((b (plist-get view :bounds))
             (clip (concat "clip-" (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (plist-get view :id))))
             ;; (BEHIND . FRONT): axes draw on either side of the marks.
             (axes (eas-svg--axes (plist-get view :axes) theme (plist-get view :id))))
        (push (append (eas-svg--node 'clipPath :id clip)
                      (list (eas-svg--node 'rect :x (aref b 0) :y (aref b 1) :width (aref b 2) :height (aref b 3))))
              defs)
        ;; The view's background: config.view fill under its frame stroke.
        (let ((frame (plist-get view :frame)) (vc (plist-get (plist-get scene :config) :view)))
          (when (and (not (eq (plist-get view :cell) :false)) (or frame (stringp (plist-get vc :fill))))
            (push (eas-svg--node 'rect :x (+ (aref b 0) (eas-svg-frame-offset frame vc))
                                 :y (+ (aref b 1) (eas-svg-frame-offset frame vc)) :width (aref b 2) :height (aref b 3)
                                 :fill (let ((f (plist-get vc :fill))) (if (stringp f) f "none"))
                                 :stroke (plist-get frame :stroke) :stroke-width (plist-get vc :strokeWidth)
                                 :stroke-dasharray (and (vectorp (plist-get vc :strokeDash)) (mapconcat #'eas-svg--n (plist-get vc :strokeDash) ","))
                                 :opacity (plist-get vc :opacity) :fill-opacity (plist-get vc :fillOpacity)
                                 :stroke-opacity (plist-get vc :strokeOpacity))
                  children)))
        ;; An axis with zindex above 0 draws over the marks.
        (setq children (append (reverse (car axes)) children))
        (when-let* ((h (plist-get view :header)))
          (push (eas-svg--text (plist-get h :text) (plist-get h :x) (plist-get h :y) (plist-get h :fontSize)
                               :align (plist-get h :align) :baseline (plist-get h :baseline)
                               :angle (plist-get h :angle) :fill "black")
                children))
        (push (apply #'dom-node 'g (when (eq (plist-get view :clip) t)
                                     (list (cons 'clip-path (format "url(#%s)" clip))))
                     (apply #'append (mapcar (lambda (mark) (eas-svg--mark-children mark (plist-get view :id)))
                                             (plist-get view :marks))))
              children)
        (setq children (append (reverse (cdr axes)) children))
        (seq-doseq (legend (plist-get view :legends))
          (when (plist-get legend :bar) (push (eas-svg--gradient-def legend) defs))
          (setq children (append (reverse (eas-svg--legend-nodes legend theme (plist-get view :id))) children)))))
    (dolist (title (let ((tt (plist-get scene :title))) (and tt (list tt (plist-get tt :subtitle)))))
      (seq-do-indexed
       (lambda (line i)
         (push (eas-svg--text line (plist-get title :x) (+ (plist-get title :y) (* i (or (plist-get title :lineHeight) 0)))
                              (plist-get title :fontSize) :align (or (plist-get title :align) "center") :baseline "top"
                              :weight (or (plist-get title :fontWeight) "bold")
                              :font (eas-svg--font-name (plist-get title :font)) :style (plist-get title :fontStyle)
                              :fill (or (plist-get title :color) (plist-get (plist-get theme :title) :color)))
               children))
       (if title (or (plist-get title :lines) (vector (plist-get title :text))) [])))
    (apply #'dom-node 'svg
           `((xmlns . "http://www.w3.org/2000/svg")
             (width . ,(eas-svg--n (plist-get size :w))) (height . ,(eas-svg--n (plist-get size :h)))
             (viewBox . ,(format "0 0 %s %s" (eas-svg--n (plist-get size :w)) (eas-svg--n (plist-get size :h))))
             (font-family . ,(eas-svg--escape (eas-svg--font (plist-get theme :font)))))
           (cons (apply #'dom-node 'defs nil
                        (append (let ((css (and eas-svg-font-embed
                                                (eas-font-file-css (plist-get scene :fonts) eas-svg-font-embed))))
                                  (unless (member css '(nil ""))
                                    (list (dom-node 'style nil (eas-svg--escape css)))))
                                (nreverse defs) eas-paint--svg-defs))
                 (nreverse children)))))

(defun eas-svg-render (scene &optional theme)
  "Return SCENE drawn as an SVG string under THEME."
  (eas-svg-retain-to-string (let ((eas-svg--fragments t)) (eas-svg-dom scene theme))))

;;; Hot spots

(defun eas-svg--tooltip-text (tooltip)
  "Render TOOLTIP pairs as \"title: value\" lines."
  (mapconcat (lambda (p) (format "%s: %s" (plist-get p :title) (plist-get p :value))) tooltip "\n"))

(defvar eas-svg-hot-spot-functions nil
  "Functions (SCENE) giving more :map areas, after the items and legends.")

(defvar eas-svg--area-ids (make-hash-table :test 'equal)
  "Hot-spot id symbols per \"VIEW|MARK\" prefix, a vector by item index.")

(defun eas-svg--area-id (view mark i)
  "The hot-spot id eas:VIEW|MARK|I of item I of MARK in VIEW."
  (let* ((prefix (format "%s|%s" (plist-get view :id) (plist-get mark :id)))
         (ids (gethash prefix eas-svg--area-ids)))
    (when (<= (length ids) i)
      (setq ids (vconcat ids (make-vector (max (1+ i) (length ids)) nil)))
      (puthash prefix ids eas-svg--area-ids))
    (or (aref ids i) (aset ids i (intern (format "eas:%s|%d" prefix i))))))

(defun eas-svg--item-area (item)
  "ITEM's hot spot as (SHAPE . PROPS), its :map shape and properties."
  (cons (cond
         ((plist-member item :startAngle) (cons 'poly (eas-arc-polygon item)))
         ((plist-member item :w)
          (cons 'rect (cons (cons (round (plist-get item :x)) (round (plist-get item :y)))
                            (cons (round (+ (plist-get item :x) (max 1 (plist-get item :w))))
                                  (round (+ (plist-get item :y) (max 1 (plist-get item :h))))))))
         (t (cons 'circle (cons (cons (round (plist-get item :x)) (round (plist-get item :y)))
                                (max 3 (round (/ (sqrt (or (plist-get item :size) 30)) 2)))))))
        (list 'help-echo (and (plist-get item :tooltip) (eas-svg--tooltip-text (plist-get item :tooltip)))
              'pointer (if (plist-get item :href) 'hand 'arrow))))

(defun eas-svg--mark-areas (view mark)
  "Image :map areas for the items of MARK in VIEW, in item order."
  (let ((areas nil))
    (seq-do-indexed
     (lambda (item i)
       (let ((area (eas-svg--item-area item)))
         (push (list (car area) (eas-svg--area-id view mark i) (cdr area)) areas)))
     (plist-get mark :items))
    (nreverse areas)))

(defun eas-svg-hot-spots (scene)
  "Image :map areas for SCENE's discrete items and legend entries.
Each area id is a symbol eas:VIEW|MARK|ITEM (or eas-legend:VIEW|CHANNEL|I).
`eas-svg-hot-spot-functions' add theirs last."
  (let (areas)
    (seq-doseq (view (plist-get scene :views))
      (seq-doseq (mark (plist-get view :marks))
        (when (member (plist-get mark :mark) '("bar" "rect" "point" "circle" "square" "text" "arc"))
          ;; Retained per mark: a frame that changed other marks reuses them.
          (setq areas (append (reverse (eas-svg-retain-part
                                        (list 'areas (plist-get view :id) (plist-get mark :id)) mark
                                        (lambda () (eas-svg--mark-areas view mark))
                                        (eas-render-cache-dirty-p (plist-get view :id) mark)))
                              areas))))
      (seq-doseq (legend (plist-get view :legends))
        (seq-do-indexed
         (lambda (e i)
           (let ((b (plist-get e :bounds)))
             (push (list (cons 'rect (cons (cons (round (aref b 0)) (round (aref b 1)))
                                           (cons (round (+ (aref b 0) (aref b 2))) (round (+ (aref b 1) (aref b 3))))))
                         (intern (format "eas-legend:%s|%s|%d" (plist-get view :id) (plist-get legend :channel) i))
                         (list 'help-echo (plist-get e :label) 'pointer 'hand))
                   areas)))
         (plist-get legend :entries))))
    (append (nreverse areas) (seq-mapcat (lambda (f) (funcall f scene)) eas-svg-hot-spot-functions))))

(defun eas-svg--scale-map (map scale)
  "MAP with every coordinate multiplied by SCALE."
  (if (= scale 1) map
    (mapcar (lambda (area)
              (let ((shape (car area)) (s (lambda (v) (round (* v scale)))))
                (cons (pcase (car shape)
                        ('rect (cons 'rect (cons (cons (funcall s (car (cadr shape))) (funcall s (cdr (cadr shape))))
                                                 (cons (funcall s (car (cddr shape))) (funcall s (cdr (cddr shape)))))))
                        ('circle (cons 'circle (cons (cons (funcall s (car (cadr shape))) (funcall s (cdr (cadr shape))))
                                                     (funcall s (cddr shape)))))
                        (_ shape))
                      (cdr area))))
            map)))

(defun eas-svg-image (scene &rest props)
  "Return an image descriptor for SCENE with :map hot spots.
PROPS may include :theme and any image property (:scale, :ascent).
:scale defaults to 1, because the scene is compiled at display pixels
\(`create-image' would otherwise apply `image-scaling-factor').  :map is
in display pixels and :original-map in scene pixels; passing both stops
`create-image' from deriving one from the other through `image-size',
which rasterizes the SVG twice more (fc-qx1.23: 3x the cost of a redraw)."
  (let* ((theme (plist-get props :theme))
         (scale (or (plist-get props :scale) 1))
         (map (eas-svg-hot-spots scene))
         (img-props (append (list :map (eas-svg--scale-map map scale) :original-map map :scale scale)
                            (eas--plist-without (eas--plist-without props :theme) :scale)
                            (unless (plist-member props :ascent) (list :ascent 'center))))
         (data (eas-svg-render scene theme)))
    ;; `create-image' signals in a batch NS Emacs ("Window system frame
    ;; should be used"); the plain descriptor is equivalent for callers.
    (if (and (display-images-p) (image-type-available-p 'svg))
        (apply #'create-image data 'svg t img-props)
      (append (list 'image :type 'svg :data data) img-props))))

(provide 'eas-svg)
;;; eas-svg.el ends here

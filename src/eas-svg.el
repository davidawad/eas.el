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
(require 'eas-theme)
(require 'eas-paint)
(require 'eas-arc)

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
  "SVG font-family for theme FONT: the generic sans-serif resolves to Arial."
  (if (member font '(nil "sans-serif")) "Arial, Liberation Sans, sans-serif" font))

(defun eas-svg--n (v)
  "Format number V compactly for SVG attributes."
  (if (integerp v) (number-to-string v)
    (let ((s (format "%.2f" v)))
      (replace-regexp-in-string "\\.?0+\\'" "" s))))

(defun eas-svg--escape (text)
  "Escape TEXT for XML."
  (replace-regexp-in-string
   "[&<>\"]" (lambda (m) (pcase m ("&" "&amp;") ("<" "&lt;") (">" "&gt;") ("\"" "&quot;")))
   (format "%s" text) t t))

(defun eas-svg--node (tag &rest attrs)
  "DOM node TAG with ATTRS (a plist, nil values dropped) and no children."
  (dom-node tag (cl-loop for (k v) on attrs by #'cddr
                         when v collect (cons (intern (substring (symbol-name k) 1))
                                              (if (numberp v) (eas-svg--n v) (eas-svg--escape v))))))

(defun eas-svg--anchor (align)
  "SVG text-anchor for scene ALIGN."
  (pcase align ("left" "start") ("right" "end") (_ "middle")))

(defun eas-svg--text (text x y size &rest props)
  "A <text> node for TEXT at X Y with font SIZE.
PROPS: :align :baseline :angle :fill :weight :opacity."
  (let* ((baseline (plist-get props :baseline))
         (dy (floor (+ 0.5 (* size (pcase baseline ("top" 0.79) ("middle" 0.30) ("bottom" -0.21) (_ 0))))))
         (angle (or (plist-get props :angle) 0))
         (node (eas-svg--node 'text :x x :y (+ y (if (zerop angle) dy 0))
                                :dy (unless (zerop angle) (eas-svg--n dy))
                                :font-size size :fill (plist-get props :fill)
                                :font-weight (let ((w (plist-get props :weight))) (and w (format "%s" w)))
                                :opacity (plist-get props :opacity)
                                :text-anchor (eas-svg--anchor (plist-get props :align))
                                :transform (unless (zerop angle)
                                             (format "rotate(%s %s %s)" (eas-svg--n angle)
                                                     (eas-svg--n x) (eas-svg--n y))))))
    (append node (list (eas-svg--escape text)))))

(defun eas-svg--line (seg color &optional width opacity dash)
  "A <line> for SEG [x1 y1 x2 y2] in COLOR."
  (eas-svg--node 'line :x1 (aref seg 0) :y1 (aref seg 1) :x2 (aref seg 2) :y2 (aref seg 3)
                   :stroke color :stroke-width (or width 1) :stroke-opacity opacity
                   :stroke-dasharray (and dash (mapconcat #'eas-svg--n dash ","))))

(defun eas-svg--path (points &optional base)
  "SVG path data through POINTS, closing along BASE reversed when given."
  (concat (mapconcat (lambda (p) (concat (eas-svg--n (aref p 0)) "," (eas-svg--n (aref p 1))))
                     points "L")
          (when base
            (concat "L" (mapconcat (lambda (p) (concat (eas-svg--n (aref p 0)) "," (eas-svg--n (aref p 1))))
                                   (reverse base) "L")
                    "Z"))))

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

(defconst eas-svg--shape-paths
  '(("diamond" . "M-1,0L0,-1L1,0L0,1Z")
    ("cross" . "M-1,-0.333H-0.333V-1H0.333V-0.333H1V0.333H0.333V1H-0.333V0.333H-1Z")
    ("triangle-up" . "M0,-0.866L1,0.866L-1,0.866Z") ("triangle" . "M0,-0.866L1,0.866L-1,0.866Z")
    ("triangle-down" . "M0,0.866L1,-0.866L-1,-0.866Z")
    ("triangle-right" . "M0.866,0L-0.866,1L-0.866,-1Z") ("triangle-left" . "M-0.866,0L0.866,1L0.866,-1Z"))
  "Vega's named symbols as unit paths (within -1..1, scaled by sqrt(size)/2).")

(defun eas-svg--symbol (shape x y size &rest attrs)
  "A Vega symbol of SHAPE and area SIZE centred on X Y, with ATTRS.
SHAPE is circle, square, another Vega symbol name or SVG path data."
  (let ((r (/ (sqrt (max 0 size)) 2.0)))
    (cond
     ((equal shape "square")
      (apply #'eas-svg--node 'rect :x (- x r) :y (- y r) :width (* 2 r) :height (* 2 r) attrs))
     ((and (stringp shape) (or (assoc shape eas-svg--shape-paths) (string-match-p "\\`[ \t]*[Mm]" shape)))
      ;; Vega draws a path symbol at sqrt(size)/2 per unit.
      (let ((sw (plist-get attrs :stroke-width)))
        (apply #'eas-svg--node 'path :d (or (cdr (assoc shape eas-svg--shape-paths)) shape)
               :transform (format "translate(%s,%s) scale(%s)" (eas-svg--n x) (eas-svg--n y) (eas-svg--n r))
               (if (and sw (> r 0)) (plist-put (copy-sequence attrs) :stroke-width (/ sw r)) attrs))))
     (t (apply #'eas-svg--node 'circle :cx x :cy y :r r attrs)))))

(defun eas-svg--item (mark item)
  "SVG node for ITEM of MARK."
  (let ((fill (eas-paint-svg-fill item)) (stroke (plist-get item :stroke))
        (opacity (let ((o (plist-get item :opacity))) (and o (/= o 1) o))))
    (pcase (plist-get mark :mark)
      ((or "bar" "rect" "brush")
       (if-let* ((corners (plist-get item :corners)))
           (eas-svg--node 'path :d (eas-svg--rounded-rect (plist-get item :x) (plist-get item :y)
                                                              (max 0 (plist-get item :w)) (max 0 (plist-get item :h)) corners)
                            :fill fill :stroke (unless (equal stroke "none") stroke) :opacity opacity)
         (eas-svg--node 'rect :x (plist-get item :x) :y (plist-get item :y)
                          :width (max 0 (plist-get item :w)) :height (max 0 (plist-get item :h))
                          :fill fill :stroke (unless (equal stroke "none") stroke) :opacity opacity)))
      ((or "rule" "tick")
       (eas-svg--line (vector (plist-get item :x1) (plist-get item :y1) (plist-get item :x2) (plist-get item :y2))
                        stroke (plist-get item :strokeWidth) opacity (plist-get item :strokeDash)))
      ("arc" (eas-svg--node 'path :d (eas-arc-path item) :fill fill
                            :stroke (unless (equal stroke "none") stroke)
                            :stroke-width (unless (equal stroke "none") (plist-get item :strokeWidth))
                            :opacity opacity))
      ("text" (apply #'eas-svg--text (plist-get item :text) (plist-get item :x) (plist-get item :y)
                     (plist-get item :fontSize)
                     (list :align (plist-get item :align) :baseline (plist-get item :baseline) :fill fill
                           :opacity opacity)))
      ((or "line" "area")
       (let ((area (plist-get item :base)))
         (eas-svg--node 'path :d (concat "M" (eas-svg--path (plist-get item :points) area))
                          :fill (if area fill "none") :stroke (unless (or area (equal stroke "none")) stroke)
                          :stroke-width (unless area (plist-get item :strokeWidth))
                          :stroke-linecap (unless area (plist-get item :strokeCap))
                          :stroke-linejoin (unless area (plist-get item :strokeJoin))
                          :stroke-dasharray (and (plist-get item :strokeDash)
                                                 (mapconcat #'eas-svg--n (plist-get item :strokeDash) ","))
                          :opacity opacity)))
      (_ (eas-svg--symbol (plist-get item :shape) (plist-get item :x) (plist-get item :y) (plist-get item :size)
                            :fill fill :stroke (unless (equal stroke "none") stroke)
                            :stroke-width (unless (equal stroke "none") (plist-get item :strokeWidth))
                            :opacity opacity)))))

(defun eas-svg--axis (axis theme)
  "SVG nodes for placed AXIS under THEME."
  (let* ((channel (if (equal (plist-get axis :orient) "bottom") :x :y))
         (get (lambda (key) (eas-theme-axis theme channel key)))
         out)
    (seq-doseq (tk (plist-get axis :ticks))
      (when (plist-get tk :grid)
        (push (eas-svg--line (plist-get tk :grid) (funcall get :gridColor) (or (funcall get :gridWidth) 1)
                               (funcall get :gridOpacity))
              out)))
    (when-let* ((domain (plist-get axis :domain-line)))
      (push (eas-svg--line domain (funcall get :domainColor) (or (funcall get :domainWidth) 1)) out))
    (seq-doseq (tk (plist-get axis :ticks))
      (unless (equal (plist-get axis :tickSize) 0)
        (push (eas-svg--line (plist-get tk :tick) (funcall get :tickColor) (or (funcall get :tickWidth) 1)) out))
      (unless (string-empty-p (plist-get tk :label))
        (push (eas-svg--text (plist-get tk :label) (plist-get tk :lx) (plist-get tk :ly)
                               (or (funcall get :labelFontSize) 10)
                               :align (plist-get tk :align) :baseline (plist-get tk :baseline)
                               :angle (if (equal (plist-get axis :orient) "bottom")
                                          (plist-get axis :labelAngle) 0)
                               :fill (funcall get :labelColor))
              out)))
    (when-let* ((tm (plist-get axis :title-mark)))
      (push (eas-svg--text (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) (or (funcall get :titleFontSize) 11)
                             :align (plist-get tm :align) :baseline (plist-get tm :baseline)
                             :angle (plist-get tm :angle) :weight (or (funcall get :titleFontWeight) "bold")
                             :fill (funcall get :titleColor))
            out))
    (nreverse out)))

(defun eas-svg--gradient-id (legend)
  "Stable id of LEGEND's gradient definition."
  (format "grad-%s-%s" (plist-get legend :channel) (abs (sxhash-equal (plist-get legend :stops)))))

(defun eas-svg--gradient-def (legend)
  "A vertical <linearGradient> for LEGEND's color stops (high values on top)."
  (let* ((stops (plist-get legend :stops)) (n (length stops)))
    (append (eas-svg--node 'linearGradient :id (eas-svg--gradient-id legend) :x1 0 :y1 1 :x2 0 :y2 0)
            (seq-map-indexed (lambda (c i) (eas-svg--node 'stop :offset (/ i (float (max 1 (1- n)))) :stop-color c))
                             stops))))

(defun eas-svg--legend (legend theme)
  "SVG nodes for placed LEGEND under THEME."
  (let ((get (lambda (key) (eas-theme-get theme :legend key)))
        (fs (or (plist-get legend :font-size) 10)) out)
    (when-let* ((tm (plist-get legend :title-mark)))
      (push (eas-svg--text (plist-get tm :text) (plist-get tm :x) (plist-get tm :y) (or (funcall get :titleFontSize) 11)
                             :align "left" :baseline "top" :weight (or (funcall get :titleFontWeight) "bold")
                             :fill (funcall get :titleColor))
            out))
    (when-let* ((bar (plist-get legend :bar)))
      (push (eas-svg--node 'rect :x (aref bar 0) :y (aref bar 1) :width (aref bar 2) :height (aref bar 3)
                             :fill (format "url(#%s)" (eas-svg--gradient-id legend)))
            out))
    (seq-doseq (e (plist-get legend :entries))
      (when (plist-get e :size)
        (push (eas-svg--symbol (plist-get legend :symbol-type) (plist-get e :sx) (plist-get e :sy) (plist-get e :size)
                                 :fill (plist-get e :fill) :stroke (plist-get e :stroke)
                                 :stroke-width (and (plist-get e :stroke) (plist-get e :stroke-width))
                                 :opacity (let ((o (plist-get e :opacity))) (and o (/= o 1) o)))
              out))
      (push (eas-svg--text (plist-get e :label) (plist-get e :lx) (plist-get e :ly) fs :align "left"
                             :baseline (or (plist-get e :baseline) "middle") :fill (funcall get :labelColor))
            out))
    (nreverse out)))

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
             (clip (concat "clip-" (replace-regexp-in-string "[^A-Za-z0-9_-]" "_" (plist-get view :id)))))
        (push (append (eas-svg--node 'clipPath :id clip)
                      (list (eas-svg--node 'rect :x (aref b 0) :y (aref b 1) :width (aref b 2) :height (aref b 3))))
              defs)
        (when-let* ((frame (plist-get view :frame)))
          (push (eas-svg--node 'rect :x (aref b 0) :y (aref b 1) :width (aref b 2) :height (aref b 3)
                                 :fill "none" :stroke (plist-get frame :stroke))
                children))
        (seq-doseq (axis (plist-get view :axes))
          (setq children (append (reverse (eas-svg--axis axis theme)) children)))
        (when-let* ((h (plist-get view :header)))
          (push (eas-svg--text (plist-get h :text) (plist-get h :x) (plist-get h :y) (plist-get h :fontSize)
                               :align (plist-get h :align) :baseline (plist-get h :baseline)
                               :angle (plist-get h :angle) :fill "black")
                children))
        (push (apply #'dom-node 'g (when (eq (plist-get view :clip) t)
                                     (list (cons 'clip-path (format "url(#%s)" clip))))
                     (apply #'append
                            (mapcar (lambda (mark)
                                      (delq nil (mapcar (lambda (item)
                                                          ;; Fully transparent items draw nothing.
                                                          (unless (equal (plist-get item :opacity) 0)
                                                            (eas-svg--item mark item)))
                                                        (plist-get mark :items))))
                                    (plist-get view :marks))))
              children)
        (seq-doseq (legend (plist-get view :legends))
          (when (plist-get legend :bar) (push (eas-svg--gradient-def legend) defs))
          (setq children (append (reverse (eas-svg--legend legend theme)) children)))))
    (when-let* ((title (plist-get scene :title)))
      (push (eas-svg--text (plist-get title :text) (plist-get title :x) (plist-get title :y)
                             (plist-get title :fontSize) :align (or (plist-get title :align) "center") :baseline "top"
                             :weight (or (plist-get title :fontWeight) "bold")
                             :fill (plist-get (plist-get theme :title) :color))
            children))
    (apply #'dom-node 'svg
           `((xmlns . "http://www.w3.org/2000/svg")
             (width . ,(eas-svg--n (plist-get size :w))) (height . ,(eas-svg--n (plist-get size :h)))
             (viewBox . ,(format "0 0 %s %s" (eas-svg--n (plist-get size :w)) (eas-svg--n (plist-get size :h))))
             (font-family . ,(eas-svg--escape (eas-svg--font (plist-get theme :font)))))
           (cons (apply #'dom-node 'defs nil (append (nreverse defs) eas-paint--svg-defs)) (nreverse children)))))

(defun eas-svg-render (scene &optional theme)
  "Return SCENE drawn as an SVG string under THEME."
  (with-temp-buffer
    (svg-print (eas-svg-dom scene theme))
    (buffer-string)))

;;; Hot spots

(defun eas-svg--tooltip-text (tooltip)
  "Render TOOLTIP pairs as \"title: value\" lines."
  (mapconcat (lambda (p) (format "%s: %s" (plist-get p :title) (plist-get p :value))) tooltip "\n"))

(defun eas-svg-hot-spots (scene)
  "Image :map areas for SCENE's discrete items and legend entries.
Each area id is a symbol eas:VIEW|MARK|ITEM (or eas-legend:VIEW|CHANNEL|I)."
  (let (areas)
    (seq-doseq (view (plist-get scene :views))
      (seq-doseq (mark (plist-get view :marks))
        (when (member (plist-get mark :mark) '("bar" "rect" "point" "circle" "square" "text" "arc"))
          (seq-do-indexed
           (lambda (item i)
             (let ((id (intern (format "eas:%s|%s|%d" (plist-get view :id) (plist-get mark :id) i)))
                   (props (list 'help-echo (and (plist-get item :tooltip) (eas-svg--tooltip-text (plist-get item :tooltip)))
                                'pointer (if (plist-get item :href) 'hand 'arrow))))
               (push (list (cond
                            ((plist-member item :startAngle) (cons 'poly (eas-arc-polygon item)))
                            ((plist-member item :w)
                               (cons 'rect (cons (cons (round (plist-get item :x)) (round (plist-get item :y)))
                                                 (cons (round (+ (plist-get item :x) (max 1 (plist-get item :w))))
                                                       (round (+ (plist-get item :y) (max 1 (plist-get item :h))))))))
                            (t (cons 'circle (cons (cons (round (plist-get item :x)) (round (plist-get item :y)))
                                                   (max 3 (round (/ (sqrt (or (plist-get item :size) 30)) 2)))))))
                           id props)
                     areas)))
           (plist-get mark :items))))
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
    (nreverse areas)))

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

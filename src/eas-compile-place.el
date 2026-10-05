;;; eas-compile-place.el --- view sizes, chrome and placement -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A layout tree is (:group G) or (:concat "v"|"h"
;; :children NODES).  Each group G is a mutable plist holding its plot
;; size (:w :h), its chrome (space for axes, titles and legends) and,
;; once arranged, its plot origin (:x0 :y0).  Natural sizes follow the
;; theme's view config (continuousWidth/Height, step 20 per category);
;; concats space cells 20 apart with plot origins aligned.  With a
;; target size the plots are fitted so the whole chart fills it, which
;; is how charts follow Emacs window sizes.
;;
;; In svg, chrome is what Vega's autosize "pad" computes: the union of
;; the axes' bounds, the legends' boxes and the marks' overhang
;; (:mark-over, measured once items exist), each side rounded up.
;; Legends stack from the plot's top, 18px (legend.offset) right of the
;; plot and of any series marks overhanging it, 8px apart.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-theme)
(require 'eas-legend)
(require 'eas-legend-fit)
(require 'eas-compile-scales)
(require 'eas-bins)
(require 'eas-facet)
(require 'eas-title)
(require 'eas-polar)
(require 'eas-compile-shared)
(require 'eas-compile-grid)

(defun eas-place-natural-size (group metrics)
  "Set GROUP's natural :w and :h from its spec and scales under METRICS."
  (let ((text (eas-layout-text-p metrics))
        (cw (aref (plist-get metrics :cell) 0)) (ch (aref (plist-get metrics :cell) 1)))
    (dolist (dim '((:w :spec-w :x :width 40) (:h :spec-h :y :height 10)))
      (let* ((spec (plist-get group (nth 1 dim)))
             (scale (plist-get (plist-get group :scales) (nth 2 dim)))
             (n (and (member (plist-get scale :type) '("band" "point")) (length (plist-get scale :domain))))
             (step (cond ((and (eas-object-p spec) (plist-get spec :step)) (plist-get spec :step))
                         (text (* 2 (if (eq (car dim) :w) cw ch)))
                         (t (plist-get metrics :step)))))
        (plist-put group (car dim)
                   (cond ((numberp spec) spec)
                         ;; Vega-Lite: step times the scale's band space.
                         (n (* step (max 1 (eas-bins-band-space scale n))))
                         ;; Vega-Lite: an unencoded position gets one discrete step.
                         ((and (null scale) (not text) (eas-place--unencoded-p group (nth 2 dim))) step)
                         (text (* (nth 4 dim) (if (eq (car dim) :w) cw ch)))
                         (t (plist-get metrics (nth 3 dim)))))))))

(defun eas-place--unencoded-p (group channel)
  "Non-nil when no unit of GROUP encodes position CHANNEL (nor its partner).
Polar units have no position channels and keep the view size."
  (let ((partner (if (eq channel :x) :x2 :y2)))
    (seq-every-p (lambda (u) (let ((enc (plist-get u :encoding)))
                               (not (or (plist-get enc channel) (plist-get enc partner)
                                        (eas-polar-unit-p u)))))
                 (plist-get group :units))))

(defun eas-place--local-scale (group channel)
  "GROUP's CHANNEL scale mapped onto its plot with the origin at 0,0."
  (when-let* ((s (plist-get (plist-get group :scales) channel)))
    (eas-compile-set-range s (if (or (eq channel :x) (member (plist-get s :type) '("band" "point")))
                                   (vector 0 (if (eq channel :x) (plist-get group :w) (plist-get group :h)))
                                 (vector (plist-get group :h) 0)))))

(defun eas-place--svg-chrome (group axes legends metrics)
  "Chrome and legend offsets of GROUP in svg; return the chrome plist."
  (let* ((w (plist-get group :w)) (h (plist-get group :h)) (plot (vector 0 0 w h))
         (placed (mapcar (lambda (axis)
                           (eas-layout-axis-place axis (eas-place--local-scale
                                                          group (eas-key (plist-get axis :channel)))
                                                    plot metrics))
                         axes))
         (yaxes (seq-filter (lambda (a) (member (plist-get a :orient) '("left" "right"))) placed))
         (over (or (plist-get group :mark-over) [0 0 0 0]))
         (lx (+ (ceiling (max (+ w (or (plist-get group :scope-over) 0))
                              (aref (apply #'eas-layout-union plot (mapcar (lambda (a) (plist-get a :bounds)) yaxes)) 2)))
                (plist-get metrics :legend-offset)))
         (ly 0) (offsets nil)
         (box (apply #'eas-layout-union plot
                     (vector (- (aref over 0)) (- (aref over 1)) (+ w (aref over 2)) (+ h (aref over 3)))
                     (mapcar (lambda (a) (plist-get a :bounds)) placed))))
    (let ((col-w 0) (limit (plist-get group :legend-limit)))
      (dolist (legend legends)
        (let ((corner (member (plist-get legend :orient) '("top-left" "top-right" "bottom-left" "bottom-right"))))
         (if corner
            ;; Inside the plot, its box flush with the corner less the offset.
            (let* ((b (plist-get (eas-legend-place legend 0 0 metrics) :box))
                   (off (plist-get metrics :legend-offset))
                   (right (string-suffix-p "right" (car corner)))
                   (bottom (string-prefix-p "bottom" (car corner))))
              (push (cons (if right (- w off (aref b 2)) (- off (aref b 0)))
                          (if bottom (- h off (aref b 3)) (- off (aref b 1))))
                    offsets))
         (if (equal (plist-get legend :orient) "none")
            (let ((at (cons (plist-get legend :legendX) (plist-get legend :legendY))))
              (push at offsets)
              (setq box (eas-layout-union box (plist-get (eas-legend-place legend (car at) (cdr at) metrics) :box))))
          (let* ((lx (+ lx (- (or (plist-get legend :offset) (plist-get metrics :legend-offset))
                              (plist-get metrics :legend-offset))))
                 (b (plist-get (eas-legend-place legend lx ly metrics) :box)))
            ;; A legend that would end below the target height starts a new column.
            (when (and limit (> ly 0) (> (+ ly (- (aref b 3) (aref b 1))) limit))
              (setq lx (+ lx col-w (plist-get metrics :legend-offset)) ly 0 col-w 0
                    b (plist-get (eas-legend-place legend lx ly metrics) :box)))
            (push (cons lx ly) offsets)
            (setq box (eas-layout-union box b)
                  col-w (max col-w (ceiling (- (aref b 2) (aref b 0))))
                  ly (+ ly (ceiling (- (aref b 3) (aref b 1))) (plist-get metrics :legend-margin)))))))))
    (plist-put group :legend-offsets (nreverse offsets))
    ;; The exact left edge of the content; Vega's frame-bounds titles start there.
    (plist-put group :content-x1 (aref box 0))
    (list :left (max 0 (ceiling (- (aref box 0)))) :top (max 0 (ceiling (- (aref box 1))))
          :right (max 0 (ceiling (- (aref box 2) w))) :bottom (max 0 (ceiling (- (aref box 3) h))))))

(defun eas-place--text-legend-flow (sizes limit w metrics)
  "Text legends of SIZES ((W . H) each) stacked from the plot's top-right.
A legend that would end below LIMIT (a height, or nil) starts a new
column to the right.  Return (OFFSETS RIGHT HEIGHT): offsets from the
plot origin (W is the plot width), the chrome right of the plot and the
tallest column."
  (let ((x 0) (y 0) (col-w 0) (tallest 0) offsets)
    (dolist (s sizes)
      (when (and limit (> y 0) (> (+ y (cdr s)) limit))
        (setq x (+ x col-w) y 0 col-w 0))
      (push (cons (+ w (plist-get metrics :legend-offset) x) y) offsets)
      (setq y (+ y (cdr s)) col-w (max col-w (car s)) tallest (max tallest y)))
    (list (nreverse offsets) (+ x col-w) tallest)))

(defun eas-place-chrome (group metrics)
  "Compute GROUP's axis and legend models and its chrome under METRICS."
  (when-let* ((range (eas-bins-size-range group (lambda (ch) (eas-place--local-scale group ch)))))
    (plist-put (plist-get group :scales) :size
               (eas-compile-set-range (plist-get (plist-get group :scales) :size) range)))
  (let* ((scales (plist-get group :scales))
         (defs (plist-get group :axis-defs))
         (axes (delq nil (list (eas-layout-axis :x (plist-get defs :x) (plist-get scales :x)
                                                  (plist-get group :w) metrics)
                               (eas-layout-axis :y (plist-get defs :y) (plist-get scales :y)
                                                  (plist-get group :h) metrics))))
         ;; Fitted to a size, legends get the height beside the plot.
         (room (and (plist-get group :fit-height)
                    (- (plist-get group :fit-height) (or (plist-get (plist-get group :chrome) :top) 0))))
         ;; Independent layers' own axes (eas-independent.el).
         (axes (append axes (delq nil (mapcar (lambda (pair)
                                                (eas-layout-axis (car pair) (cdr pair) (plist-get scales (car pair))
                                                                 (plist-get group (if (string-prefix-p ":x" (symbol-name (car pair))) :w :h))
                                                                 metrics))
                                              (plist-get group :extra-axes)))))
         (legends (delq nil (mapcar (lambda (spec)
                                      (let ((l (eas-legend-model spec metrics)))
                                        (and l (eas-legend-fit
                                                (eas-legend-sized
                                                 (append l (list :plot-h (plist-get group :h))) metrics)
                                                room metrics))))
                                    (plist-get group :legend-specs))))
         (chrome (list :left 0 :right 0 :top 0 :bottom 0)))
    (if (not (eas-layout-text-p metrics))
        (setq chrome (eas-place--svg-chrome group axes legends metrics))
      (dolist (axis axes)
        (dolist (side (eas-layout-axis-extent axis metrics))
          (plist-put chrome (car side) (max (plist-get chrome (car side)) (cdr side)))))
      (when legends
        (let ((flow (eas-place--text-legend-flow (mapcar (lambda (l) (eas-legend-size l metrics)) legends)
                                                 (plist-get group :legend-limit) (plist-get group :w) metrics)))
          (plist-put group :legend-offsets (nth 0 flow))
          (plist-put chrome :right (nth 1 flow))
          (plist-put chrome :legend-h (nth 2 flow)))))
    ;; A facet cell's header sits outside its axes.
    (when-let* ((header (plist-get group :header)))
      (let ((e (eas-facet-header-extent header metrics)))
        (plist-put group :header-inset (plist-get chrome (car e)))
        (plist-put chrome (car e) (+ (plist-get chrome (car e)) (cdr e)))))
    ;; A concat cell's title sits above its axes (eas-title.el).
    (when-let* ((node (plist-get group :title-node)))
      (plist-put group :axis-top (plist-get chrome :top))
      (plist-put chrome :top (+ (plist-get chrome :top) (eas-title-height node metrics))))
    (plist-put group :axes-model axes)
    (plist-put group :legends-model legends)
    (plist-put group :chrome chrome)))

(defun eas-place-title-mark (group metrics)
  "GROUP's view title as a list of one non-interactive text mark, or nil."
  (when-let* ((title (plist-get group :title)))
    (list (list :id (format "%s/title" (plist-get group :id)) :mark "text" :interactive-off t :rows []
                :items (vector (list :text title :x (plist-get group :x0)
                                     :y (- (plist-get group :y0) (plist-get (plist-get group :chrome) :top))
                                     :fontSize (plist-get metrics :chart-title-size)
                                     :fontWeight (plist-get metrics :chart-title-weight)
                                     :align "left" :baseline "top"
                                     :fill (or (eas-theme-get (plist-get metrics :config) :title :color) "black")))))))

(defun eas-place--groups (node)
  "All groups under layout NODE, in order."
  (if (plist-get node :group) (list (plist-get node :group))
    (apply #'append (mapcar #'eas-place--groups (plist-get node :children)))))

(defun eas-place--lead (node key)
  "Chrome on side KEY (:top or :left) before NODE's leading plots.
Nested concatenations align their plots with their siblings' too."
  (cond ((plist-get node :group) (plist-get (plist-get (plist-get node :group) :chrome) key))
        ((eas-place-grid-p node) 0)
        ((equal (plist-get node :concat) (if (eq key :top) "h" "v"))
         (apply #'max 0 (mapcar (lambda (ch) (eas-place--lead ch key)) (plist-get node :children))))
        (t (eas-place--lead (car (plist-get node :children)) key))))

(defun eas-place-arrange (node ox oy metrics)
  "Place NODE's groups with the block's top-left at OX OY; return (W . H)."
  (let ((spacing (or (plist-get node :spacing) (plist-get metrics :spacing))))
    (if (eas-place-grid-p node) (eas-place-arrange-grid node ox oy metrics)
    (if-let* ((g (plist-get node :group)))
        (let ((c (plist-get g :chrome)))
          (plist-put g :x0 (+ ox (plist-get c :left)))
          (plist-put g :y0 (+ oy (plist-get c :top)))
          (cons (+ (plist-get c :left) (plist-get g :w) (plist-get c :right))
                (+ (plist-get c :top)
                   (max (+ (plist-get g :h) (plist-get c :bottom)) (or (plist-get c :legend-h) 0)))))
      (let* ((vertical (equal (plist-get node :concat) "v"))
             (children (plist-get node :children))
             (key (if vertical :left :top))
             (align (apply #'max 0 (mapcar (lambda (ch) (eas-place--lead ch key)) children)))
             (cursor 0) (cross 0))
        (dolist (child children)
          (let* ((inset (- align (eas-place--lead child key)))
                 (size (if vertical
                           (eas-place-arrange child (+ ox inset) (+ oy cursor) metrics)
                         (eas-place-arrange child (+ ox cursor) (+ oy inset) metrics))))
            (setq cursor (+ cursor (if vertical (cdr size) (car size)) spacing)
                  cross (max cross (+ inset (if vertical (car size) (cdr size)))))))
        (setq cursor (max 0 (- cursor spacing)))
        (if vertical (cons cross cursor) (cons cursor cross)))))))

(defun eas-place-fit (node width height metrics)
  "Resize NODE's plots so its block is WIDTH by HEIGHT."
  (let ((min-w (* 4 (aref (plist-get metrics :cell) 0)))
        (min-h (* 2 (aref (plist-get metrics :cell) 1))))
    (if (eas-place-grid-p node) (eas-place-fit-grid node width height metrics)
    (if-let* ((g (plist-get node :group)))
        (let ((c (plist-get g :chrome)))
          (plist-put g :fit-height height)
          (plist-put g :w (max min-w (- width (plist-get c :left) (plist-get c :right))))
          (plist-put g :h (max min-h (- height (plist-get c :top) (plist-get c :bottom)))))
      (let* ((vertical (equal (plist-get node :concat) "v"))
             (children (plist-get node :children))
             (spacing (or (plist-get node :spacing) (plist-get metrics :spacing)))
             (natural (mapcar (lambda (ch) (eas-place-arrange ch 0 0 metrics)) children))
             (along (mapcar (lambda (s) (if vertical (cdr s) (car s))) natural))
             (total (max 1 (apply #'+ along)))
             (avail (- (if vertical height width) (* spacing (1- (length children))))))
        (cl-loop for child in children for a in along
                 do (if vertical
                        (eas-place-fit child width (* avail (/ (float a) total)) metrics)
                      (eas-place-fit child (* avail (/ (float a) total)) height metrics))))))))

(defun eas-place-layout (tree metrics title-h size &optional sized)
  "Size, chrome and arrange TREE; return the scene size (W . H).
TITLE-H is the chart title's height.  SIZE, when non-nil, is the target
\(W . H) the plots are fitted to.  SIZED non-nil keeps the groups' current
plot sizes (a relayout after marks were measured)."
  (let ((groups (eas-place--groups tree)) (pad (plist-get metrics :pad)) (shared 0))
    (dolist (g groups)
      (unless sized (eas-place-natural-size g metrics))
      (eas-place-chrome g metrics))
    ;; Legends shared across concat views sit right of the whole block.
    (setq shared (eas-shared-extent tree metrics (plist-get (car groups) :h)))
    (when size
      ;; Legends taller than the target flow into columns.
      (dolist (g groups)
        (plist-put g :legend-limit (- (cdr size) (* 2 pad) title-h))
        (eas-place-chrome g metrics)))
    (when size
      (dotimes (_ 2)
        (eas-place-fit tree (- (car size) (* 2 pad) shared) (- (cdr size) (* 2 pad) title-h) metrics)
        (dolist (g groups) (eas-place-chrome g metrics))))
    (let ((block (eas-place-arrange tree pad (+ pad title-h) metrics)))
      (eas-shared-place tree groups metrics (+ pad (car block)) (plist-get (car groups) :y0))
      ;; A size too small for the chrome grows the canvas rather than clip it.
      (let ((need (cons (+ (car block) shared (* 2 pad)) (+ (cdr block) title-h (* 2 pad)))))
        (if size (cons (max (car size) (car need)) (max (cdr size) (cdr need))) need)))))

(provide 'eas-compile-place)
;;; eas-compile-place.el ends here

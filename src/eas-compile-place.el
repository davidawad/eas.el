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
(require 'eas-legend)
(require 'eas-legend-fit)
(require 'eas-compile-scales)

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
                         (n (* step (max 1 n)))
                         (text (* (nth 4 dim) (if (eq (car dim) :w) cw ch)))
                         (t (plist-get metrics (nth 3 dim)))))))))

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
         (yaxes (seq-filter (lambda (a) (equal (plist-get a :orient) "left")) placed))
         (over (or (plist-get group :mark-over) [0 0 0 0]))
         (lx (+ (ceiling (max (+ w (or (plist-get group :scope-over) 0))
                              (aref (apply #'eas-layout-union plot (mapcar (lambda (a) (plist-get a :bounds)) yaxes)) 2)))
                (plist-get metrics :legend-offset)))
         (ly 0) (offsets nil)
         (box (apply #'eas-layout-union plot
                     (vector (- (aref over 0)) (- (aref over 1)) (+ w (aref over 2)) (+ h (aref over 3)))
                     (mapcar (lambda (a) (plist-get a :bounds)) placed))))
    (dolist (legend legends)
      (if (equal (plist-get legend :orient) "none")
          (let ((at (cons (plist-get legend :legendX) (plist-get legend :legendY))))
            (push at offsets)
            (setq box (eas-layout-union box (plist-get (eas-legend-place legend (car at) (cdr at) metrics) :box))))
        (let ((b (plist-get (eas-legend-place legend lx ly metrics) :box)))
          (push (cons lx ly) offsets)
          (setq box (eas-layout-union box b)
                ly (+ ly (ceiling (- (aref b 3) (aref b 1))) (plist-get metrics :legend-margin))))))
    (plist-put group :legend-offsets (nreverse offsets))
    (list :left (max 0 (ceiling (- (aref box 0)))) :top (max 0 (ceiling (- (aref box 1))))
          :right (max 0 (ceiling (- (aref box 2) w))) :bottom (max 0 (ceiling (- (aref box 3) h))))))

(defun eas-place-chrome (group metrics)
  "Compute GROUP's axis and legend models and its chrome under METRICS."
  (let* ((scales (plist-get group :scales))
         (defs (plist-get group :axis-defs))
         (axes (delq nil (list (eas-layout-axis :x (plist-get defs :x) (plist-get scales :x)
                                                  (plist-get group :w) metrics)
                               (eas-layout-axis :y (plist-get defs :y) (plist-get scales :y)
                                                  (plist-get group :h) metrics))))
         ;; Fitted to a size, legends get the height beside the plot.
         (room (and (plist-get group :fit-height)
                    (- (plist-get group :fit-height) (or (plist-get (plist-get group :chrome) :top) 0))))
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
        (let ((sizes (mapcar (lambda (l) (eas-legend-size l metrics)) legends)))
          (plist-put chrome :right (apply #'max (mapcar #'car sizes)))
          (plist-put chrome :legend-h (apply #'+ (mapcar #'cdr sizes))))))
    (plist-put group :axes-model axes)
    (plist-put group :legends-model legends)
    (plist-put group :chrome chrome)))

(defun eas-place--groups (node)
  "All groups under layout NODE, in order."
  (if (plist-get node :group) (list (plist-get node :group))
    (apply #'append (mapcar #'eas-place--groups (plist-get node :children)))))

(defun eas-place-arrange (node ox oy metrics)
  "Place NODE's groups with the block's top-left at OX OY; return (W . H)."
  (let ((spacing (plist-get metrics :spacing)))
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
             (align (apply #'max 0 (mapcar (lambda (ch) (if (plist-get ch :group)
                                                            (plist-get (plist-get (plist-get ch :group) :chrome) key)
                                                          0))
                                           children)))
             (cursor 0) (cross 0))
        (dolist (child children)
          (let* ((inset (if (plist-get child :group)
                            (- align (plist-get (plist-get (plist-get child :group) :chrome) key))
                          0))
                 (size (if vertical
                           (eas-place-arrange child (+ ox inset) (+ oy cursor) metrics)
                         (eas-place-arrange child (+ ox cursor) (+ oy inset) metrics))))
            (setq cursor (+ cursor (if vertical (cdr size) (car size)) spacing)
                  cross (max cross (+ inset (if vertical (car size) (cdr size)))))))
        (setq cursor (max 0 (- cursor spacing)))
        (if vertical (cons cross cursor) (cons cursor cross))))))

(defun eas-place-fit (node width height metrics)
  "Resize NODE's plots so its block is WIDTH by HEIGHT."
  (let ((min-w (* 4 (aref (plist-get metrics :cell) 0)))
        (min-h (* 2 (aref (plist-get metrics :cell) 1))))
    (if-let* ((g (plist-get node :group)))
        (let ((c (plist-get g :chrome)))
          (plist-put g :fit-height height)
          (plist-put g :w (max min-w (- width (plist-get c :left) (plist-get c :right))))
          (plist-put g :h (max min-h (- height (plist-get c :top) (plist-get c :bottom)))))
      (let* ((vertical (equal (plist-get node :concat) "v"))
             (children (plist-get node :children))
             (spacing (plist-get metrics :spacing))
             (natural (mapcar (lambda (ch) (eas-place-arrange ch 0 0 metrics)) children))
             (along (mapcar (lambda (s) (if vertical (cdr s) (car s))) natural))
             (total (max 1 (apply #'+ along)))
             (avail (- (if vertical height width) (* spacing (1- (length children))))))
        (cl-loop for child in children for a in along
                 do (if vertical
                        (eas-place-fit child width (* avail (/ (float a) total)) metrics)
                      (eas-place-fit child (* avail (/ (float a) total)) height metrics)))))))

(defun eas-place-layout (tree metrics title-h size &optional sized)
  "Size, chrome and arrange TREE; return the scene size (W . H).
TITLE-H is the chart title's height.  SIZE, when non-nil, is the target
\(W . H) the plots are fitted to.  SIZED non-nil keeps the groups' current
plot sizes (a relayout after marks were measured)."
  (let ((groups (eas-place--groups tree)) (pad (plist-get metrics :pad)))
    (dolist (g groups)
      (unless sized (eas-place-natural-size g metrics))
      (eas-place-chrome g metrics))
    (when size
      (dotimes (_ 2)
        (eas-place-fit tree (- (car size) (* 2 pad)) (- (cdr size) (* 2 pad) title-h) metrics)
        (dolist (g groups) (eas-place-chrome g metrics))))
    (let ((block (eas-place-arrange tree pad (+ pad title-h) metrics)))
      (or size (cons (+ (car block) (* 2 pad)) (+ (cdr block) title-h (* 2 pad)))))))

(provide 'eas-compile-place)
;;; eas-compile-place.el ends here

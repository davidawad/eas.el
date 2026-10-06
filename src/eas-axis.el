;;; eas-axis.el --- axis properties and top/right/offset axes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, beside eas-layout.el.  `eas-layout-axis' builds an axis
;; model from the theme alone; `eas-axis-extras' adds what a spec's
;; axis object (or its config) asks for beyond that:
;;
;;   orient top|right, domain/ticks/labels false, offset, minExtent,
;;   labelPadding, titlePadding, tickSize, tickBand "extent", and per
;;   axis style (colors, widths, fonts, gridDash, gridColor with a
;;   condition on datum.value), carried to the renderers in :style.
;;
;; Axes using any of them are placed by `eas-axis-place' (all four
;; orients, the same Vega geometry as `eas-layout-axis-place': lines on
;; the half pixel, labels at tick + padding, titles past the labels'
;; bounds); the rest keep the original placement untouched.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-theme)
(require 'eas-layout)
(require 'eas-transform)
(require 'eas-axis-pos)

(defconst eas-axis--style-keys
  '(:domainColor :domainWidth :domainDash :tickColor :tickWidth :gridColor :gridWidth :gridOpacity
    :gridDash :labelColor :labelFontSize :labelFontWeight :titleColor :titleFontSize :titleFontWeight
    ;; fc-qx1.43
    :labelFont :labelFontStyle :labelOpacity :titleFont :titleFontStyle :titleOpacity
    :tickOpacity :domainOpacity :domainDash :tickDash
    :domainCap :tickCap :gridCap)
  "Axis properties the renderers read, overridable per axis.")

(defconst eas-axis--layout-keys
  '(:orient :no-domain :no-ticks :no-labels :offset :min-extent :label-pad :title-pad :tick-size
    :tick-band :grid-cond)
  "Model keys that need `eas-axis-place'.")

(defun eas-axis-extras (def channel metrics)
  "Model properties for CHANNEL's axis from DEF's axis object and the config.
METRICS carries the config.  Return a plist to prepend to the model."
  (let* ((axis (let ((a (plist-get def :axis))) (and (eas-object-p a) a)))
         (config (plist-get metrics :config))
         (get (lambda (k) (if (plist-member axis k) (plist-get axis k) (eas-theme-axis config channel k))))
         (num (lambda (k) (let ((v (plist-get axis k))) (and (numberp v) v))))
         (style (cl-loop for k in eas-axis--style-keys
                         for v = (plist-get axis k)
                         when (and (plist-member axis k) (not (eas-object-p v)) (not (eq v :null)))
                         append (list k v)))
         (grid-color (plist-get axis :gridColor))
         (orient (plist-get axis :orient)))
    (append
     (when (member orient '("top" "right" "bottom" "left"))
       (unless (equal orient (if (eq channel :x) "bottom" "left")) (list :orient orient)))
     (when (eq (funcall get :domain) :false) (list :no-domain t))
     (when (eq (funcall get :ticks) :false) (list :no-ticks t))
     (when (eq (funcall get :labels) :false) (list :no-labels t))
     (let ((v (funcall get :offset))) (when (and (numberp v) (/= v 0)) (list :offset v)))
     (let ((v (funcall get :minExtent))) (when (and (numberp v) (> v 0)) (list :min-extent v)))
     (let ((v (funcall num :labelPadding))) (when v (list :label-pad v)))
     (let ((v (funcall num :titlePadding))) (when v (list :title-pad v)))
     (let ((v (funcall num :tickSize))) (when v (list :tick-size v)))
     (when (equal (funcall get :tickBand) "extent") (list :tick-band "extent"))
     (when (and (eas-object-p grid-color) (plist-get grid-color :condition))
       (list :grid-cond grid-color))
     (when style (list :style style)))))

(defun eas-axis-extended-p (axis)
  "Non-nil when AXIS needs `eas-axis-place'."
  (or (seq-some (lambda (k) (plist-get axis k)) eas-axis--layout-keys)
      (member (plist-get axis :orient) '("top" "right"))))

(defun eas-axis--grid-color (axis value)
  "Grid color for tick VALUE under AXIS's gridColor condition, or nil."
  (when-let* ((gc (plist-get axis :grid-cond)))
    (let* ((c (plist-get gc :condition))
           (hit (seq-find (lambda (c) (eas-transform-predicate (plist-get c :test) (list :value value) nil))
                          (if (vectorp c) c (list c)))))
      (plist-get (or hit gc) :value))))

(defun eas-axis--tick-positions (axis scale)
  "AXIS's ticks with :pos; with tickBand extent, :tick-pos at band edges.
SCALE places the ticks."
  (let* ((half (/ (or (plist-get scale :bandwidth) 0) 2.0))
         (band (member (plist-get scale :type) '("band" "point")))
         (edge (and band (equal (plist-get axis :tick-band) "extent")))
         (step (or (plist-get scale :step) 0))
         (gap (- step (* 2 half))))
    (seq-filter (lambda (tk) (plist-get tk :pos))
                (seq-map (lambda (tk)
                           (let ((p (eas-scale-apply scale (plist-get tk :value))))
                             (append tk (list :pos (and p (+ p half)))
                                     (when (and p edge) (list :tick-pos (- p (/ gap 2.0)))))))
                         (plist-get axis :ticks)))))

(defun eas-axis--frame (orient bounds offset)
  "Where an ORIENT axis's line sits for plot BOUNDS, moved out by OFFSET.
Return (POS . DIR): the line's coordinate across the axis and the
outward direction (+1 or -1)."
  (let ((x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3)))
    (pcase orient
      ("bottom" (cons (+ y0 h offset) 1)) ("top" (cons (- y0 offset) -1))
      ("left" (cons (- x0 offset) -1)) (_ (cons (+ x0 w offset) 1)))))

(defun eas-axis-place (axis scale bounds metrics)
  "AXIS (with extras) placed for SCALE in plot BOUNDS under METRICS."
  (if (eas-layout-text-p metrics) (eas-axis--place-text axis scale bounds metrics)
    (let* ((x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3))
           (orient (plist-get axis :orient))
           (horiz (member orient '("bottom" "top")))
           (frame (eas-axis--frame orient bounds (or (plist-get axis :offset) 0)))
           (a (car frame)) (dir (cdr frame))
           (tick (if (plist-get axis :no-ticks) 0
                   (or (plist-get axis :tick-size) (eas-layout--tick (list :orient (if horiz "bottom" "left")) metrics))))
           (pad (or (plist-get axis :label-pad) (plist-get metrics :label-pad)))
           (size (or (plist-get (plist-get axis :style) :labelFontSize) (plist-get metrics :label-size)))
           (lweight (plist-get (plist-get axis :style) :labelFontWeight))
           (flush (and horiz (eq (plist-get axis :discrete) :false)))
           (angle (if horiz (plist-get axis :labelAngle) 0))
           (band (equal (plist-get scale :type) "band"))
           (ticks (mapcar (lambda (tk) (if band (plist-put tk :pos (- (plist-get tk :pos) 0.5)) tk))
                          (eas-axis--tick-positions axis scale)))
           (tick-pos (lambda (tk) (let ((p (or (plist-get tk :tick-pos) (plist-get tk :pos))))
                                    (if band (eas-layout--round p) p))))
           (out (+ a (* dir (+ tick pad))))
           (label (lambda (tk)
                    (let ((p (plist-get tk :pos)))
                      (if horiz
                          (list :lx p :ly out
                                :align (or (eas-axis-pos-x-align angle (< dir 0)) (eas-layout--bottom-align p x0 w flush 0))
                                :baseline (eas-axis-pos-x-baseline angle (< dir 0)))
                        (list :lx out :ly p :align (if (> dir 0) "left" "right") :baseline "middle")))))
           (box (lambda (tk) (let ((l (funcall label tk)))
                               (eas-layout-text-bounds metrics (plist-get tk :label) size (plist-get l :lx) (plist-get l :ly)
                                                       (plist-get l :align) (plist-get l :baseline) angle lweight))))
           (labelled (unless (plist-get axis :no-labels)
                       (seq-remove (lambda (tk) (string-empty-p (plist-get tk :label))) ticks)))
           (shown (if (null (plist-get axis :overlap)) labelled
                    (eas-layout--thin
                     labelled
                     (lambda (p q) (let ((p (funcall box p)) (q (funcall box q)))
                                     (> 0 (max (- (aref q 0) (aref p 2)) (- (aref p 0) (aref q 2))
                                               (- (aref q 1) (aref p 3)) (- (aref p 1) (aref q 3))))))
                     (plist-get axis :overlap) t)))
           (line-box (if horiz (vector x0 (min a (+ a (* dir tick))) (+ x0 w) (max a (+ a (* dir tick))))
                       (vector (min a (+ a (* dir tick))) y0 (max a (+ a (* dir tick))) (+ y0 h))))
           (ab (apply #'eas-layout-union line-box
                      (unless (plist-get axis :no-ticks)
                        (mapcar (lambda (tk) (let ((p (funcall tick-pos tk)) (e (+ a (* dir tick))))
                                               (if horiz (vector (- p 1) (1- (min a e)) (+ p 1) (1+ (max a e)))
                                                 (vector (1- (min a e)) (- p 1) (1+ (max a e)) (+ p 1)))))
                                ticks))))
           (edge-ticks (when (and (plist-get axis :tick-band) band ticks)
                         ;; tickBand extent: one more tick closing the last band.
                         (let* ((last (car (last ticks))) (p (+ (plist-get last :tick-pos) (plist-get scale :step))))
                           (list (list :value nil :label "" :edge t :pos p :tick-pos p)))))
           (placed
            (vconcat
             (seq-map
              (lambda (tk)
                (let* ((p (+ 0.5 (funcall tick-pos tk))) (l (funcall label tk)) (show (memq tk shown))
                       (gc (and (not (plist-get tk :edge)) (eas-axis--grid-color axis (plist-get tk :value)))))
                  (when show (setq ab (eas-layout-union ab (funcall box tk))))
                  (append (eas--plist-without tk :label)
                          (list :label (if show (plist-get tk :label) ""))
                          (list :tick (unless (plist-get axis :no-ticks)
                                        (if horiz (vector p (+ a 0.5) p (+ a (* dir tick) 0.5))
                                          (vector (+ a 0.5) p (+ a (* dir tick) 0.5) p))))
                          (list :lx (+ 0.5 (plist-get l :lx)) :ly (+ 0.5 (plist-get l :ly))
                                :align (plist-get l :align) :baseline (plist-get l :baseline))
                          (when gc (list :grid-color gc))
                          (when (eq (plist-get axis :grid) t)
                            (list :grid (if horiz (vector p (+ y0 0.5) p (+ y0 h 0.5))
                                          (vector (+ x0 0.5) p (+ x0 w 0.5) p)))))))
              (append ticks edge-ticks))))
           (title (plist-get axis :title))
           (tsize (or (plist-get (plist-get axis :style) :titleFontSize) (plist-get metrics :title-size)))
           (tpad (or (plist-get axis :title-pad) (plist-get metrics :title-pad)))
           (weight (or (plist-get (plist-get axis :style) :titleFontWeight) (plist-get metrics :title-weight)))
           (reach (max (or (plist-get axis :min-extent) 0)
                       (if horiz (if (> dir 0) (- (aref ab 3) a) (- a (aref ab 1)))
                         (if (> dir 0) (- (aref ab 2) a) (- a (aref ab 0))))))
           (tp (+ a (* dir (+ reach tpad))))
           (tm (when title
                 (if horiz
                     (list :text title :x (+ x0 (/ w 2.0) 0.5) :y (+ tp 0.5)
                           :align "center" :baseline (if (> dir 0) "top" "bottom") :angle 0)
                   (list :text title :x (+ tp 0.5) :y (+ y0 (/ h 2.0) 0.5)
                         :align "center" :baseline "bottom" :angle (if (> dir 0) 90 -90))))))
      (when tm
        (setq ab (eas-layout-union ab (if (string-empty-p title)
                                          (eas-layout-empty-title-point tm tpad)
                                        (eas-layout-text-bounds metrics title tsize (- (plist-get tm :x) 0.5)
                                                                (- (plist-get tm :y) 0.5) (plist-get tm :align)
                                                                (plist-get tm :baseline) (plist-get tm :angle) weight)))))
      (append (eas--plist-without axis :ticks)
              (list :ticks placed
                    :domain-line (unless (plist-get axis :no-domain)
                                   (if horiz (vector (+ x0 0.5) (+ a 0.5) (+ x0 w 0.5) (+ a 0.5))
                                     (vector (+ a 0.5) (+ y0 0.5) (+ a 0.5) (+ y0 h 0.5))))
                    :bounds ab)
              (when tm (list :title-mark tm))))))

(defun eas-axis--without (plist keys)
  "PLIST without any of KEYS."
  (dolist (k keys plist) (setq plist (eas--plist-without plist k))))

(defun eas-axis--place-text (axis scale bounds metrics)
  "AXIS placed on the character grid; top and right mirror bottom and left.
SCALE, BOUNDS and METRICS are the axis's scale, plot and layout."

  (let* ((orient (plist-get axis :orient))
         (x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3))
         (mirror (member orient '("top" "right")))
         ;; Place as the mirrored orient, then reflect across the plot.
         (base (eas-layout-axis-place-text
                (plist-put (copy-sequence axis) :orient (if (member orient '("top" "bottom")) "bottom" "left"))
                scale bounds metrics))
         (tick (eas-layout--tick (list :orient (if (member orient '("top" "bottom")) "bottom" "left")) metrics))
         (flip (lambda (v horiz) (if (not mirror) v
                                   (if horiz (- (+ y0 y0 h) v) (- (+ x0 x0 w) v)))))
         (horiz (member orient '("top" "bottom"))))
    (append
     (eas-axis--without base '(:ticks :domain-line :orient :title-mark))
     (list :orient orient
           :ticks (vconcat
                   (seq-map (lambda (tk)
                              (let ((ts (plist-get tk :tick)))
                                (append (eas-axis--without tk '(:tick :label :lx :ly :align))
                                        (list :label (if (plist-get axis :no-labels) "" (plist-get tk :label))
                                              :tick (unless (plist-get axis :no-ticks)
                                                      (if horiz (vector (aref ts 0) (funcall flip (aref ts 1) t)
                                                                        (aref ts 2) (funcall flip (aref ts 3) t))
                                                        (vector (funcall flip (aref ts 0) nil) (aref ts 1)
                                                                (funcall flip (aref ts 2) nil) (aref ts 3))))
                                              :lx (if horiz (plist-get tk :lx)
                                                    (if mirror (+ x0 w tick) (plist-get tk :lx)))
                                              :ly (if (and horiz mirror) (- y0 tick (plist-get metrics :label-size))
                                                    (plist-get tk :ly))
                                              :align (if (and mirror (not horiz)) "left" (plist-get tk :align))))))
                            (plist-get base :ticks)))
           :domain-line (unless (plist-get axis :no-domain)
                          (let ((d (plist-get base :domain-line)))
                            (if horiz (vector (aref d 0) (funcall flip (aref d 1) t) (aref d 2) (funcall flip (aref d 3) t))
                              (vector (funcall flip (aref d 0) nil) (aref d 1) (funcall flip (aref d 2) nil) (aref d 3))))))
     (when-let* ((tm (plist-get base :title-mark)))
       (list :title-mark
             (cond ((and mirror horiz)
                    (plist-put (copy-sequence tm) :y (- y0 tick (* 2 (plist-get metrics :label-size)))))
                   ;; Right: flush with the labels' far edge, like the left title.
                   (mirror (plist-put (plist-put (copy-sequence tm) :x (+ x0 w tick (eas-layout-axis-label-extent axis metrics)))
                                      :align "right"))
                   (t tm)))))))

(provide 'eas-axis)
;;; eas-axis.el ends here

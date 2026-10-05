;;; eas-axis-extra.el --- axis properties beyond ticks and titles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  `eas-layout-axis' builds an axis model from a channel
;; definition; `eas-axis-extra-apply' then honours the Vega-Lite axis
;; properties that edit that model rather than its geometry:
;;
;;   domain, ticks, labels  false hides the line, the ticks, the labels
;;                          (axis or config.axis/axisX/axisY)
;;   labelColor, tickColor  a value or {condition: {test}, value}, per
;;                          tick; a null color hides that label or tick
;;   labelOverlap           false keeps every label; true or "parity"
;;                          and "greedy" choose Vega's strategy
;;   labelFlush             false centres the end labels too
;;   minExtent              least room for ticks and labels; the title
;;                          moves out to it
;;   tickBand               "extent" moves a band axis's ticks and grid
;;                          to the band edges, plus one closing tick
;;   grid                   config.axis grid true also grids band axes
;;
;; Ticks whose label and tick are both hidden are dropped.  Renderers
;; read :domain-off, :ticks-off and per tick :tick-hidden, :label-color
;; and :tick-color.

;;; Code:

(require 'eas-core)
(require 'eas-expr)
(require 'eas-theme)

(require 'eas-transform)
(require 'eas-scale)
(require 'eas-axis-pos)
(declare-function eas-layout-text-p "eas-layout")
(declare-function eas-layout-text-bounds "eas-layout")
(declare-function eas-layout-union "eas-layout")

(defun eas-axis-extra--prop (axis config channel key)
  "Axis property KEY from the channel's AXIS def, else CONFIG; :none if unset."
  (cond ((plist-member axis key) (plist-get axis key))
        ((let ((v (eas-theme-axis config channel key))) (and v (list v))) (eas-theme-axis config channel key))
        (t :none)))

(defun eas-axis-extra--off-p (axis config channel key)
  "Non-nil when axis property KEY is false."
  (eq (eas-axis-extra--prop axis config channel key) :false))

(defun eas-axis-extra--color (spec datum env)
  "Color SPEC (a string or a conditional value) for DATUM; :null hides."
  (cond
   ((stringp spec) spec)
   ((eq spec :null) :null)
   ((and (eas-object-p spec) (plist-member spec :condition))
    (let* ((c (plist-get spec :condition))
           (hit (seq-find (lambda (c) (eas-transform-predicate (plist-get c :test) datum env))
                          (if (vectorp c) (append c nil) (list c)))))
      (eas-axis-extra--color (if hit (plist-get hit :value) (plist-get spec :value)) datum env)))
   ((eas-object-p spec) (eas-axis-extra--color (plist-get spec :value) datum env))))

(defun eas-axis-extra--tick (tk axis env)
  "Tick TK of AXIS def with labelExpr and colors applied, or nil when hidden."
  (let* ((datum (list :value (plist-get tk :value) :label (plist-get tk :label)))
         (label (plist-get tk :label))
         (lc (and (plist-member axis :labelColor) (eas-axis-extra--color (plist-get axis :labelColor) datum env)))
         (tc (and (plist-member axis :tickColor) (eas-axis-extra--color (plist-get axis :tickColor) datum env))))
    (unless (and (eq lc :null) (eq tc :null))
      (append (list :value (plist-get tk :value) :label (if (eq lc :null) "" label))
              (when (stringp lc) (list :label-color lc))
              (when (stringp tc) (list :tick-color tc))
              (when (eq tc :null) (list :tick-hidden t))
              ;; Per-tick dashes from eas-layout-axis-style.
              (cl-loop for (k v) on tk by #'cddr
                       unless (memq k '(:value :label)) append (list k v))))))

(defun eas-axis-extra-apply (model def channel config &optional env)
  "MODEL, the axis model for CHANNEL's DEF, with its extra properties applied.
CONFIG is the Vega config in force; ENV holds param values."
  (let* ((axis (let ((a (plist-get def :axis))) (and (eas-object-p a) a)))
         (overlap (eas-axis-extra--prop axis config channel :labelOverlap))
         (labels-off (eas-axis-extra--off-p axis config channel :labels))
         (ticks (delq nil (mapcar (lambda (tk) (eas-axis-extra--tick tk axis env))
                                  (append (plist-get model :ticks) nil)))))
    (when labels-off (setq ticks (mapcar (lambda (tk) (plist-put tk :label "")) ticks)))
    (setq model (plist-put (copy-sequence model) :ticks (vconcat ticks)))
    ;; An explicit labelOverlap false keeps every label, colliding or not.
    (when (eq overlap :false) (setq model (plist-put model :label-overlap-off t)))
    (unless (eq overlap :none)
      (setq model (plist-put model :overlap-set t))
      (setq model (plist-put model :overlap (pcase overlap
                                              (:false nil) ("greedy" "greedy")
                                              (_ "parity")))))
    (let ((min (eas-axis-extra--prop axis config channel :minExtent)))
      (when (numberp min) (setq model (plist-put model :min-extent min))))
    (setq model (append model (eas-axis-pos-props (lambda (k) (eas-axis-extra--prop axis config channel k)))))
    (when (eq (eas-axis-extra--prop axis config channel :labelFlush) :false)
      (setq model (plist-put model :label-flush :false)))
    (when (equal (eas-axis-extra--prop axis config channel :tickBand) "extent")
      (setq model (plist-put model :tick-band "extent")))
    (when (and (eq (plist-get model :discrete) t) (not (plist-member axis :grid))
               (eq (eas-theme-axis config channel :grid) t))
      (setq model (plist-put model :grid t)))
    (when (eas-axis-extra--off-p axis config channel :domain) (setq model (plist-put model :domain-off t)))
    (when (eas-axis-extra--off-p axis config channel :ticks) (setq model (plist-put model :ticks-off t)))
    ;; zindex 1 or more draws the axis (grid too) in front of the marks.
    (let ((z (eas-axis-extra--prop axis config channel :zindex)))
      (when (and (numberp z) (> z 0)) (setq model (plist-put model :zindex z))))
    model))

(defun eas-axis-extra-tick-color (axis tk default)
  "Stroke of tick TK on placed AXIS: nil when hidden, else its color or DEFAULT."
  (unless (or (plist-get axis :ticks-off) (plist-get tk :tick-hidden))
    (or (plist-get tk :tick-color) default)))

(defun eas-axis-extra--hidden-label (axis tk p metrics)
  "Bounds of TK's label drawn transparent at edge P of AXIS.\nVega's extra closing tick has such a label."
  (let ((size (plist-get metrics :label-size)))
    (if (equal (plist-get axis :orient) "bottom")
        (eas-layout-text-bounds metrics (plist-get tk :label) size p (- (plist-get tk :ly) 0.5)
                                (plist-get tk :align) (plist-get tk :baseline) (plist-get axis :labelAngle))
      (eas-layout-text-bounds metrics (plist-get tk :label) size (- (plist-get tk :lx) 0.5) p "right" "middle"))))

(defun eas-axis-extra--min-extent (axis metrics)
  "Placed svg AXIS with its title pushed out to its :min-extent under METRICS."
  (let* ((min (plist-get axis :min-extent)) (ab (plist-get axis :bounds))
         (line (plist-get axis :domain-line)) (tm (plist-get axis :title-mark))
         (tpad (or (plist-get axis :title-padding) (plist-get metrics :title-pad)))
         (bottom (equal (plist-get axis :orient) "bottom"))
         (edge (and min ab line (if bottom (- (aref line 1) 0.5) (- (aref line 0) 0.5))))
         ;; How far ticks and labels reach: the title sits titlePadding beyond.
         (reach (and edge (if tm (if bottom (- (plist-get tm :y) 0.5 tpad) (+ (plist-get tm :x) -0.5 tpad))
                            (if bottom (aref ab 3) (aref ab 0)))))
         (short (and reach (if bottom (- (+ edge min) reach) (- reach (- edge min))))))
    (if (not (and short (> short 0))) axis
      (let ((out (copy-sequence axis)))
        (when tm
          (setq out (plist-put out :title-mark (if bottom (plist-put (copy-sequence tm) :y (+ (plist-get tm :y) short))
                                                 (plist-put (copy-sequence tm) :x (- (plist-get tm :x) short))))))
        (plist-put out :bounds (if bottom (vector (aref ab 0) (aref ab 1) (aref ab 2) (+ (aref ab 3) short))
                                 (vector (- (aref ab 0) short) (aref ab 1) (aref ab 2) (aref ab 3))))))))

(defun eas-axis-extra-place (axis scale metrics)
  "AXIS after placement: minExtent, then tickBand (`eas-axis-extra--band')."
  (eas-axis-extra--band (if (eas-layout-text-p metrics) axis (eas-axis-extra--min-extent axis metrics)) scale metrics))

(defun eas-axis-extra--band (axis scale metrics)
  "Placed AXIS with tickBand extent applied for band SCALE under METRICS.
Vega pegs the closing tick to the first band's start and bounds its
label there, transparent; the character grid needs no half pixels."
  (if (not (and (equal (plist-get axis :tick-band) "extent") (equal (plist-get scale :type) "band")))
      axis
    (let* ((text (eas-layout-text-p metrics))
           (bottom (equal (plist-get axis :orient) "bottom"))
           (gap (/ (- (plist-get scale :step) (plist-get scale :bandwidth)) 2.0))
           (at (lambda (p) (if text p (+ 0.5 (round (- p 0.5))))))
           (move (lambda (tk p extra)
                   (let* ((tseg (plist-get tk :tick))
                          (tick (if bottom (vector p (aref tseg 1) p (aref tseg 3))
                                  (vector (aref tseg 0) p (aref tseg 2) p))))
                     (append (list :tick tick)
                             (when (plist-get tk :grid)
                               (list :grid (if bottom (vector p (aref (plist-get tk :grid) 1) p (aref (plist-get tk :grid) 3))
                                             (vector (aref (plist-get tk :grid) 0) p (aref (plist-get tk :grid) 2) p))))
                             (when extra (list :value nil :label "" :extra t))
                             tk))))
           (ticks (append (plist-get axis :ticks) nil))
           (moved (mapcar (lambda (tk) (let ((s (eas-scale-apply scale (plist-get tk :value))))
                                          (if s (funcall move tk (funcall at (- s gap)) nil) tk)))
                          ticks))
           (last (car (last ticks)))
           (end (and last (eas-scale-apply scale (plist-get last :value)))))
      (let ((out (plist-put (copy-sequence axis) :ticks
                            (vconcat moved (when end (list (funcall move last (funcall at (+ end (plist-get scale :bandwidth) gap)) t))))))
            (first (car ticks)))
        (when (and first (not text) (plist-get axis :bounds) (not (string-empty-p (plist-get first :label))))
          (setq out (plist-put out :bounds (eas-layout-union
                                            (plist-get axis :bounds)
                                            (eas-axis-extra--hidden-label
                                             axis first (- (eas-scale-apply scale (plist-get first :value)) gap) metrics)))))
        out))))

(provide 'eas-axis-extra)
;;; eas-axis-extra.el ends here

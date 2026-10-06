;;; eas-legend-merge.el --- one legend for continuous color and size -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, beside eas-legend.el (eas-7r1.11).  Vega-Lite merges the
;; legends of channels that encode the same field: a continuous color
;; and a size of one quantitative field give a single symbol legend,
;; each entry sized by the size scale and colored by the color scale,
;; where the color alone would be a gradient.  As in Vega, the size
;; scale picks the entries: legend.values when given, else five ticks
;; of its domain, less a first one of size zero.  A legend: null on
;; either channel keeps them apart (the other draws its own legend).

;;; Code:

(require 'eas-core)
(require 'eas-scale)

(declare-function eas-expr--string "eas-expr")

(defconst eas-legend-merge-size-types '("linear" "log" "sqrt" "pow")
  "Continuous size scale types whose legend a color legend absorbs.")

(defun eas-legend-merge-p (color-def size-def color-scale size-scale)
  "Non-nil when SIZE-SCALE of SIZE-DEF joins the legend of COLOR-DEF.
That is when both read the same field, COLOR-SCALE is a continuous
color ramp, SIZE-SCALE is continuous and neither legend is null."
  (and color-def size-def color-scale size-scale
       (plist-get color-def :field)
       (equal (plist-get color-def :field) (plist-get size-def :field))
       (equal (plist-get color-scale :type) "sequential")
       (member (plist-get size-scale :type) eas-legend-merge-size-types)
       (not (memq (plist-get color-def :legend) '(:null :false)))
       (not (memq (plist-get size-def :legend) '(:null :false)))))

(defun eas-legend-merge-model (base spec)
  "The merged symbol legend of SPEC on top of the model BASE.
SPEC carries the color :scale, its :def and the :size-scale."
  (let* ((scale (plist-get spec :scale)) (size (plist-get spec :size-scale))
         (legend (plist-get (plist-get spec :def) :legend))
         (domain (plist-get size :domain))
         (lo (aref domain 0)) (hi (aref domain (1- (length domain))))
         (lv (and (eas-object-p legend) (vectorp (plist-get legend :values)) (plist-get legend :values)))
         (values (if lv (append lv nil) (eas-scale-linear-ticks lo hi 5)))
         (values (if (and (not lv) values (zerop (or (eas-scale-apply size (car values)) 0)))
                     (cdr values) values))
         (fmt (eas-scale-tick-format (list :type "linear" :domain (vector lo hi)) 5)))
    (append base
            (list :type "symbol"
                  :entries (vconcat (mapcar (lambda (v)
                                              (list :value v :label (if (numberp v) (funcall fmt v) (eas-expr--string v))
                                                    :color (eas-scale-apply scale v)
                                                    :size (max 0 (or (eas-scale-apply size v) 0))))
                                            values))))))

(provide 'eas-legend-merge)
;;; eas-legend-merge.el ends here

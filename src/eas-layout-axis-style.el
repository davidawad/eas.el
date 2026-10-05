;;; eas-layout-axis-style.el --- per-axis label and tick properties -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, for eas-layout.el.  An encoding's "axis" object may
;; override tick, label and title geometry for that axis alone (tickSize,
;; labelPadding, labelAlign, labelBaseline, labelOffset, titlePadding), rewrite each
;; label with labelExpr (datum.value and datum.label; an array result is
;; a multi-line label) and give ticks and grid lines conditional dashes:
;;
;;   "gridDash": {"condition": {"test": {"field": "value", "timeUnit": "month",
;;                                       "equal": 1}, "value": []},
;;                "value": [2, 2]}
;;
;; Conditions test the tick's datum {value, label} with the ordinary
;; predicate grammar.  Geometry overrides apply to the svg target; the
;; text target keeps its one-cell ticks and labels.

;;; Code:

(require 'eas-core)
(require 'eas-expr)
(require 'eas-transform)

(defconst eas-layout-axis-style-geometry
  '((:tickSize . :tick-size) (:labelPadding . :label-padding) (:labelAlign . :label-align)
    (:labelBaseline . :label-baseline) (:labelOffset . :label-offset) (:titlePadding . :title-padding))
  "Axis properties that override layout, and the model keys they become.")

(defun eas-layout-axis-style-props (axis)
  "Model properties for AXIS's geometry overrides."
  (cl-loop for (key . model) in eas-layout-axis-style-geometry
           for v = (plist-get axis key)
           when (or (numberp v) (stringp v)) append (list model v)))

(defun eas-layout-axis-style-label (axis value label)
  "LABEL for tick VALUE after AXIS's labelExpr, lines joined by newlines."
  (let ((expr (plist-get axis :labelExpr)))
    (if (not (stringp expr)) label
      (let ((out (eas-expr-evaluate expr (list :value value :label label))))
        (cond ((vectorp out) (mapconcat #'eas-expr--string (seq-remove (lambda (v) (memq v '(nil :null))) out) "\n"))
              ((memq out '(nil :null)) "")
              (t (eas-expr--string out)))))))

(defun eas-layout-axis-style--value (def value label)
  "Property DEF (a constant or {condition, value}) for tick VALUE and LABEL."
  (if (not (and (consp def) (keywordp (car def)))) def
    (let* ((datum (list :value value :label label))
           (conds (plist-get def :condition))
           (hit (seq-find (lambda (c) (eas-transform-predicate (plist-get c :test) datum nil))
                          (if (vectorp conds) conds (and conds (list conds))))))
      (plist-get (or hit def) :value))))

(defun eas-layout-axis-style-tick (axis value label)
  "Per-tick properties of AXIS for VALUE: :tick-dash and :grid-dash."
  (cl-loop for (key . model) in '((:tickDash . :tick-dash) (:gridDash . :grid-dash))
           for v = (and (plist-member axis key) (eas-layout-axis-style--value (plist-get axis key) value label))
           when (and (vectorp v) (> (length v) 0)) append (list model v)))

(provide 'eas-layout-axis-style)
;;; eas-layout-axis-style.el ends here

;;; eas-spec-props-vocab.el --- Vega-Lite style properties, with probe values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2 (eas-spec-props.el).  Each scope's Vega-Lite 6.4.1
;; properties as (KEY PROBE [CONTEXT]): PROBE is a non-default value,
;; (:any V ...) values of which one must have an effect, or
;; (:one-of DEFAULT V ...) an enumeration judged value by value,
;; CONTEXT (KEY VALUE ...) is set on the same object in both the base
;; and the probed chart (a property that needs a partner, such as
;; tension needing a cardinal interpolate).

;;; Code:

(defconst eas-spec-props--mark-common
  '((color "#d62728") (fill "#d62728") (stroke "#d62728") (opacity 0.4) (fillOpacity 0.3)
    (strokeOpacity 0.3 (stroke "#000000")) (strokeWidth 5 (stroke "#000000"))
    (strokeDash [6 3] (stroke "#000000")) (strokeDashOffset 3 (stroke "#000000" strokeDash [6 3]))
    (strokeCap "square" (stroke "#000000" strokeWidth 6)) (strokeJoin "bevel" (stroke "#000000" strokeWidth 6))
    (strokeMiterLimit 1 (stroke "#000000" strokeWidth 6))
    (cursor "pointer") (href "https://example.com") (tooltip t) (blend "multiply")
    (invalid "filter") (clip t) (aria :false) (ariaRole "img") (ariaRoleDescription "mark")
    (description "a mark") (style "probe") (filled (:any :false t)) (xOffset 7) (yOffset 7) (x2Offset 7) (y2Offset 7)
    (timeUnitBandSize 0.5) (timeUnitBandPosition 0.5) (smooth t))
  "Properties every Vega-Lite mark takes.")

(defconst eas-spec-props--mark-types
  '(("bar" (binSpacing 5) (continuousBandSize 3) (discreteBandSize 6) (minBandSize 3)
     (cornerRadius 6) (cornerRadiusEnd 6) (cornerRadiusTopLeft 6) (cornerRadiusTopRight 6)
     (cornerRadiusBottomLeft 6) (cornerRadiusBottomRight 6) (orient (:one-of "vertical" "horizontal"))
     (align "left") (baseline "top") (width 5) (height 5) (size 6))
    ("line" (interpolate (:one-of "linear" "linear-closed" "step" "step-before" "step-after" "monotone" "basis" "cardinal" "natural" "bundle" "catmull-rom")) (tension 0.2 (interpolate "cardinal")) (point t) (orient (:one-of "vertical" "horizontal")) (size 6))
    ("area" (interpolate (:one-of "linear" "step" "step-before" "step-after" "monotone" "basis" "cardinal" "natural")) (tension 0.2 (interpolate "cardinal")) (point t) (line t) (orient (:one-of "vertical" "horizontal")))
    ("point" (shape "square") (size 200) (angle 45 (shape "triangle")))
    ("rule" (size 6))
    ("tick" (thickness 4) (bandSize 6) (orient (:one-of "vertical" "horizontal")) (size 6))
    ("text" (align "left") (baseline "top") (dx 8) (dy 8) (angle 30) (font "Courier New") (fontSize 16)
     (fontStyle "italic") (fontWeight "bold") (limit 12) (lineBreak "e" (text "one\ntwo"))
     (lineHeight 30 (text ["one" "two"])) (ellipsis "~" (limit 12)) (dir "rtl") (text "probe")
     (radius 20 (theta 1)) (theta 1 (radius 20))))
  "The calculations gallery's mark types with the properties of their own.")

(defconst eas-spec-props--axis
  '((aria :false) (bandPosition 0.2) (description "axis") (domain :false) (domainCap "square" (domainWidth 6))
    (domainColor "#d62728") (domainDash [4 2]) (domainDashOffset 2 (domainDash [4 2])) (domainOpacity 0.3)
    (domainWidth 4) (format ".1f") (formatType "number" (format ".1f")) (grid :false)
    (gridCap "square" (gridWidth 6)) (gridColor "#d62728") (gridDash [4 2]) (gridDashOffset 2 (gridDash [4 2]))
    (gridOpacity 0.2) (gridWidth 3) (labelAlign "left") (labelAngle 45) (labelBaseline "top") (labelBound t)
    (labelColor "#d62728") (labelExpr "'v' + datum.label") (labelFlush :false) (labelFlushOffset 6 (labelFlush t))
    (labelFont "Courier New") (labelFontSize 15) (labelFontStyle "italic") (labelFontWeight "bold")
    (labelLimit 20) (labelLineHeight 30) (labelOffset 6) (labelOpacity 0.3) (labelOverlap t)
    (labelPadding 12) (labelSeparation 30) (labels :false) (maxExtent 5) (minExtent 80) (offset 12)
    (orient "right") (position 20) (style "probe") (tickBand "extent") (tickCap "square" (tickWidth 6))
    (tickColor "#d62728") (tickCount 3) (tickDash [4 2]) (tickDashOffset 2 (tickDash [4 2]))
    (tickExtra t (tickBand "extent")) (tickMinStep 5) (tickOffset 4) (tickOpacity 0.3) (tickRound :false)
    (tickSize 12) (tickWidth 4) (ticks :false) (title "Probe") (titleAlign "right") (titleAnchor "end")
    (titleAngle 30) (titleBaseline "top") (titleColor "#d62728") (titleFont "Courier New") (titleFontSize 18)
    (titleFontStyle "italic") (titleFontWeight "bold") (titleLimit 10 (title "A rather long axis title for the probe")) (titleLineHeight 30)
    (titleOpacity 0.3) (titlePadding 20) (titleX 20) (titleY 20) (translate 3) (values [1 2]) (zindex 1))
  "Vega-Lite axis properties.")

(defconst eas-spec-props--legend
  '((aria :false) (clipHeight 6) (columnPadding 30 (columns 2)) (columns (:one-of 1 2))
    (cornerRadius 6 (strokeColor "#000000")) (description "legend") (direction "horizontal")
    (fillColor "#eeeeee") (format ".1f") (formatType "number" (format ".1f")) (gradientLength 60)
    (gradientOpacity 0.3) (gradientStrokeColor "#d62728") (gradientStrokeWidth 3) (gradientThickness 30)
    (gridAlign "none" (columns 2)) (labelAlign "right") (labelBaseline "top") (labelColor "#d62728")
    (labelExpr "'v' + datum.label") (labelFont "Courier New") (labelFontSize 16) (labelFontStyle "italic")
    (labelFontWeight "bold") (labelLimit 20) (labelOffset 15) (labelOpacity 0.3) (labelOverlap t)
    (labelPadding 12) (labelSeparation 20) (legendX 10 (orient "none")) (legendY 10 (orient "none"))
    (offset 40) (orient (:one-of "right" "left" "top" "bottom" "top-left" "top-right" "bottom-left" "bottom-right" "none"))
    (padding 12) (rowPadding 12) (strokeColor "#d62728")
    (symbolDash [2 2]) (symbolDashOffset 1 (symbolDash [2 2])) (symbolFillColor "#d62728") (symbolLimit 1)
    (symbolOffset 10) (symbolOpacity 0.3) (symbolSize 300) (symbolStrokeColor "#d62728")
    (symbolStrokeWidth 4) (symbolType "square") (tickCount 2) (tickMinStep 5) (title "Probe")
    (titleAlign "right") (titleAnchor "end") (titleBaseline "bottom") (titleColor "#d62728")
    (titleFont "Courier New") (titleFontSize 18) (titleFontStyle "italic") (titleFontWeight "bold")
    (titleLimit 10) (titleLineHeight 30) (titleOpacity 0.3) (titleOrient "left") (titlePadding 20)
    (type "symbol") (values ["b"]) (zindex 1))
  "Vega-Lite legend properties.")

(defconst eas-spec-props--scale
  '((type "sqrt") (domain [0 20]) (domainMax 20) (domainMin -5) (domainMid 1) (domainRaw :null)
    (range (:any [0 50] ["#d62728" "#2ca02c" "#9467bd" "#8c564b"])) (rangeMax 50) (rangeMin 20) (scheme "reds") (interpolate "hcl") (reverse t)
    (round t) (clamp t (domain [0 2])) (nice :false (zero :false)) (zero :false) (padding 20) (paddingInner 0.5)
    (paddingOuter 1) (align 0) (base 2 (type "log")) (exponent 3 (type "pow")) (constant 2 (type "symlog"))
    (bins [0 5 10]))
  "Vega-Lite scale properties.")

(defconst eas-spec-props--title
  '((text "Probe") (subtitle "Sub") (align "right") (anchor (:one-of "start" "middle" "end")) (angle 10) (aria :false) (baseline "bottom")
    (color "#d62728") (dx 15) (dy 15) (font "Courier New") (fontSize 20) (fontStyle "italic")
    (fontWeight "normal") (frame "group") (limit 20) (lineHeight 40 (text ["a" "b"])) (offset 30)
    (orient (:one-of "top" "bottom" "left" "right")) (style "probe") (subtitleColor "#d62728" (subtitle "Sub"))
    (subtitleFont "Courier New" (subtitle "Sub")) (subtitleFontSize 20 (subtitle "Sub"))
    (subtitleFontStyle "italic" (subtitle "Sub")) (subtitleFontWeight "bold" (subtitle "Sub"))
    (subtitleLineHeight 40 (subtitle ["a" "b"])) (subtitlePadding 20 (subtitle "Sub")) (zindex 1))
  "Vega-Lite title properties.")

(defconst eas-spec-props--view
  '((stroke "#d62728") (strokeWidth 4 (stroke "#d62728")) (strokeDash [4 2] (stroke "#d62728"))
    (strokeOpacity 0.3 (stroke "#d62728")) (fill "#eeeeee")
    (fillOpacity 0.5 (fill "#eeeeee")) (cornerRadius 8) (opacity 0.4) (cursor "pointer") (clip t)
    (continuousWidth 100) (continuousHeight 100) (discreteWidth 100) (discreteHeight 100) (step 40))
  "config.view properties.")

(defconst eas-spec-props--config
  '((background "#eeeeee") (padding 30) (font "Courier New") (autosize "none") (countTitle "N")
    (customFormatTypes t) (fieldTitle "functional") (lineBreak "e") (locale (:number (:decimal ",")))
    (numberFormat ".1f") (normalizedNumberFormat ".1%") (timeFormat "%Y") (tooltipFormat (:numberFormat ".1f")))
  "Top-level config properties (the ones that are not blocks).")

(defconst eas-spec-props-inert
  '(aria ariaRole ariaRoleDescription description zindex cursor href tooltip style
    customFormatTypes timeUnitBandSize timeUnitBandPosition smooth)
  "Properties that never change Vega's static picture: accessibility
metadata, z-order among equals, pointer and link affordances (the scene
carries them per item for the runtime), style names that only select
config, and time-unit band settings with no time unit in play.")

(defun eas-spec-props-vocabulary (scope)
  "The (KEY PROBE [CONTEXT]) entries of SCOPE.
SCOPE is a mark type string (\"config.TYPE\" for its config block), or
one of the symbols axis, legend, scale, title, view, config and
config-axis, config-legend, config-title (the config.axis, config.legend
and config.title blocks).  A mark type's own entries win over the
common ones."
  (pcase scope
    ((or 'axis 'config-axis) eas-spec-props--axis) ((or 'legend 'config-legend) eas-spec-props--legend)
    ('scale eas-spec-props--scale) ((or 'title 'config-title) eas-spec-props--title)
    ('view eas-spec-props--view) ('config eas-spec-props--config)
    ((pred stringp)
     (let ((own (cdr (assoc (string-remove-prefix "config." scope) eas-spec-props--mark-types))))
       (append own (seq-remove (lambda (e) (assq (car e) own)) eas-spec-props--mark-common))))))

(defun eas-spec-props-scopes ()
  "Every scope the vocabulary covers."
  (let ((types (mapcar #'car eas-spec-props--mark-types)))
    (append types '(axis legend scale title view config config-axis config-legend config-title)
            (mapcar (lambda (type) (concat "config." type)) types))))

(provide 'eas-spec-props-vocab)
;;; eas-spec-props-vocab.el ends here

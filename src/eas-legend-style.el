;;; eas-legend-style.el --- a legend's own properties over config.legend -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, beside eas-legend.el (fc-qx1.42).  An encoding's legend
;; object may restyle that legend alone, as config.legend restyles all:
;;
;;   layout   labelFontSize titleFontSize titleFontWeight titlePadding
;;            rowPadding labelOffset symbolSize symbolType
;;            symbolStrokeWidth gradientThickness
;;   paint    labelColor titleColor symbolFillColor symbolStrokeColor
;;            symbolOpacity symbolDash
;;   labels   values (the entries shown, in that order), format (d3
;;            number format of numeric labels), labelExpr (datum.value,
;;            datum.label)
;;
;; The model carries them as :overrides; `eas-legend-style-metrics'
;; overlays the layout ones on the svg metrics wherever a legend is
;; sized or placed, so its bounds and its neighbours' placement follow;
;; the svg renderer reads the paint ones before the theme.  As in
;; Vega-Lite, a symbol paint the legend's scale encodes (the fill of a
;; filled color legend, the stroke of a stroke legend) stays the
;; scale's.  The text target keeps its one-cell rows and reads labels.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-layout)
(require 'eas-layout-axis-style)
(require 'eas-format)

(defconst eas-legend-style-metric-keys
  '((:labelFontSize . :legend-label-size) (:titleFontSize . :legend-title-size)
    (:titleFontWeight . :legend-title-weight) (:titlePadding . :legend-title-pad)
    (:rowPadding . :legend-row-pad) (:labelOffset . :legend-label-offset)
    (:symbolSize . :symbol-size) (:symbolType . :symbol-type)
    (:symbolStrokeWidth . :symbol-stroke-width) (:gradientThickness . :gradient-thickness))
  "Legend properties that change layout, and the metrics they override.")

(defconst eas-legend-style-keys
  (append (mapcar #'car eas-legend-style-metric-keys)
          '(:labelColor :titleColor :symbolFillColor :symbolStrokeColor :symbolOpacity :symbolDash
            :values :format :labelExpr
            :titleFont :titleFontStyle :labelFont :labelFontStyle))
  "Legend properties honored per legend.")

(defun eas-legend-style-overrides (def)
  "The honored properties of legend object DEF, as a plist."
  (and (eas-object-p def)
       (cl-loop for k in eas-legend-style-keys
                for v = (plist-get def k)
                when (and v (not (eq v :null)) (or (not (eas-object-p v)) (vectorp v))) append (list k v))))

(defun eas-legend-style-metrics (legend metrics)
  "METRICS with LEGEND's layout overrides (svg target only)."
  (let ((o (plist-get legend :overrides)))
    (if (or (null o) (eas-layout-text-p metrics)) metrics
      (let ((m (copy-sequence metrics)))
        (dolist (pair eas-legend-style-metric-keys m)
          (let ((v (plist-get o (car pair))))
            (when (or (numberp v) (stringp v)) (setq m (plist-put m (cdr pair) v)))))))))

(defun eas-legend-style--label (o value label &optional formatted)
  "LABEL of entry VALUE after overrides O's format and labelExpr.
FORMATTED non-nil: LABEL carries the format already (a bucket's range)."
  (let* ((fmt (plist-get o :format))
         (label (if (and (stringp fmt) (numberp value) (not formatted))
                    (eas-format-number fmt value)
                  label)))
    (eas-layout-axis-style-label o value label)))

(defun eas-legend-style-labels (legend)
  "LEGEND with its entries' labels formatted per its overrides."
  (let ((o (plist-get legend :overrides)))
    (if (not (or (plist-get o :format) (plist-get o :labelExpr))) legend
      (eas-plist-put legend :entries
                     (vconcat (mapcar (lambda (e) (eas-plist-put e :label (eas-legend-style--label
                                                                           o (plist-get e :value) (plist-get e :label)
                                                                           (plist-get e :formatted))))
                                      (plist-get legend :entries)))))))

(defun eas-legend-style-model (def model)
  "Return legend MODEL with legend object DEF's overrides applied.
Entries are chosen by values, labels formatted, the rest carried as
:overrides."
  (let ((o (eas-legend-style-overrides def)))
    (if (or (null model) (null o)) model
      (let* ((model (append model (list :overrides o)))
             (values (plist-get o :values)))
        (when (and (vectorp values) (equal (plist-get model :type) "symbol"))
          (let ((entries (append (plist-get model :entries) nil)))
            (setq model (eas-plist-put model :entries
                                       (vconcat (delq nil (mapcar (lambda (v) (seq-find (lambda (e) (equal (plist-get e :value) v))
                                                                                         entries))
                                                                  values)))))))
        (eas-legend-style-labels model)))))

(defun eas-legend-style-looks (legend)
  "Placed LEGEND with its symbol paint overrides on each entry."
  (let* ((o (plist-get legend :overrides)) (channel (plist-get legend :channel))
         (stroked (member channel '("stroke" "strokeDash")))
         (look (append
                (when (and (stringp (plist-get o :symbolFillColor)) (not (member channel '("color" "fill"))))
                  (list :fill (plist-get o :symbolFillColor)))
                (when (and (stringp (plist-get o :symbolStrokeColor)) (not stroked))
                  (list :stroke (plist-get o :symbolStrokeColor)))
                (when (numberp (plist-get o :symbolOpacity)) (list :opacity (plist-get o :symbolOpacity)))
                (when (and (vectorp (plist-get o :symbolDash)) (not (equal channel "strokeDash")))
                  (list :dash (plist-get o :symbolDash))))))
    (if (null look) legend
      (eas-plist-put legend :entries
                     (vconcat (mapcar (lambda (e)
                                        ;; A dash shows only on a symbol that has a stroke.
                                        (let ((stroke (or (plist-get look :stroke) (plist-get e :stroke))))
                                          (append (if (and stroke (not (member stroke '("none" "transparent")))) look
                                                    (eas--plist-without look :dash))
                                                  e)))
                                      (plist-get legend :entries)))))))

(defun eas-legend-style-paint (legend theme-get key)
  "LEGEND's paint property KEY: its override, else THEME-GET's."
  (let ((v (plist-get (plist-get legend :overrides) key)))
    (if (and v (not (eas-object-p v))) v (funcall theme-get key))))

(provide 'eas-legend-style)
;;; eas-legend-style.el ends here

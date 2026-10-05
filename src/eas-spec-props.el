;;; eas-spec-props.el --- Vega-Lite properties the native renderer ignores -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2, beside eas-spec.el (fc-qx1.42).  `eas-spec-features'
;; refuses keys outside the chart/v1 vocabulary, and compile then falls
;; back to a static image.  Many valid Vega-Lite properties of marks,
;; axes, legends, titles, scales and config are inside the vocabulary
;; (or not walked at all) yet not drawn natively: the chart renders
;; without them.  `eas-spec-props-findings' names each such property a
;; spec sets, with its JSON pointer:
;;
;;   (:code "UNSUPPORTED_FEATURE" :ignored t :path "/encoding/x/axis/labelFont"
;;    :feature "axis/labelFont" :message "...")
;;
;; check reports them as warnings; they do not block compile, so the
;; chart stays native and interactive.  The tables below are the
;; properties measured to have no effect natively (render with and
;; without; test/vl-examples/area-circular custom specs exercise the
;; honored ones), not every unknown key.

;;; Code:

(require 'eas-core)

(defun eas-spec-props--obj-p (v)
  "Non-nil when V is a non-empty JSON object (`eas-object-p' holds for nil)."
  (and v (eas-object-p v)))

(defconst eas-spec-props-ignored
  '((mark . (:aria :description :ariaRole :ariaRoleDescription :font :fontStyle :limit
                   :timeUnitBandPosition :timeUnitBandSize))
    (axis . (:aria :description :domainCap :domainDash :domainDashOffset :domainOpacity :gridCap
                   :gridDashOffset :labelBound :labelFlushOffset :labelFont :labelFontStyle :labelLineHeight
                   :labelOpacity :labelSeparation :maxExtent :position :style :tickCap :tickDashOffset
                   :tickExtra :tickMinStep :tickOffset :tickOpacity :tickRound :titleAlign :titleAnchor
                   :titleAngle :titleBaseline :titleFont :titleFontStyle :titleLimit :titleLineHeight
                   :titleOpacity :titleX :titleY :translate :zindex))
    (legend . (:aria :description :columnPadding :cornerRadius :fillColor :gradientOpacity
                     :gradientStrokeColor :gradientStrokeWidth :gridAlign :labelAlign :labelBaseline :labelFont
                     :labelFontStyle :labelFontWeight :labelLimit :labelOpacity :labelOverlap :labelPadding
                     :labelSeparation :padding :strokeColor :symbolDashOffset :symbolLimit :symbolOffset
                     :tickCount :tickMinStep :titleAlign :titleAnchor :titleBaseline :titleFont :titleFontStyle
                     :titleLimit :titleLineHeight :titleOpacity :titleOrient :type :zindex :layout :disable))
    (title . (:align :angle :aria :baseline :font :fontStyle :limit :lineHeight :orient :style
                     :subtitleFont :subtitleFontStyle :subtitleLineHeight :zindex))
    (scale . (:clamp))
    (position-scale . (:range :rangeMin :rangeMax :clamp))
    (view . (:fill :fillOpacity :opacity :strokeWidth :strokeDash :strokeDashOffset :strokeOpacity
                   :strokeCap :strokeJoin :strokeMiterLimit :cornerRadius :cursor :discreteWidth :discreteHeight))
    (config . (:autosize :numberFormat :numberFormatType :timeFormat :timeFormatType :normalizedNumberFormat
                         :normalizedNumberFormatType :customFormatTypes :lineBreak :locale :style :countTitle
                         :fieldTitle :aria :axisBottom :axisTop :axisLeft :axisRight :axisBand :axisPoint
                         :axisQuantitative :axisTemporal :axisDiscrete :axisXBand :axisXPoint :axisXQuantitative
                         :axisXTemporal :axisXDiscrete :axisYBand :axisYPoint :axisYQuantitative :axisYTemporal
                         :axisYDiscrete :headerColumn :headerRow :headerFacet)))
  "Properties, by where they sit, that native rendering ignores.")

(defconst eas-spec-props-ignored-mark-config
  '(:line :point)
  "config.MARK properties ignored (a mark's own line/point overlay is honored).")

(defconst eas-spec-props-honored-legend-orients '("right" "none")
  "legend.orient values placed natively.")

(defun eas-spec-props--finding (where key path)
  "The finding for ignored property KEY of WHERE at PATH."
  (let ((name (eas-key-name key)))
    (list :code "UNSUPPORTED_FEATURE" :ignored t :path (concat path "/" name)
          :feature (format "%s/%s" where name)
          :message (format "%s.%s is not honored natively; the chart renders without it" where name))))

(defun eas-spec-props--scan (obj where list path)
  "Findings for OBJ's keys in the ignored LIST, named WHERE, under PATH."
  (when (eas-spec-props--obj-p obj)
    (cl-loop for key in (eas-plist-keys obj)
             when (memq key list) collect (eas-spec-props--finding where key path))))

(defun eas-spec-props--ignored (kind)
  "The ignored property list of KIND."
  (cdr (assq kind eas-spec-props-ignored)))

(defun eas-spec-props--def (channel def path)
  "Findings in encoding CHANNEL's DEF at PATH: its axis, legend and scale."
  (when (eas-spec-props--obj-p def)
    (let ((axis (plist-get def :axis)) (legend (plist-get def :legend)) (scale (plist-get def :scale)))
      (append
       (eas-spec-props--scan axis "axis" (eas-spec-props--ignored 'axis) (concat path "/axis"))
       (eas-spec-props--scan legend "legend" (eas-spec-props--ignored 'legend) (concat path "/legend"))
       (when (and (eas-spec-props--obj-p legend) (stringp (plist-get legend :orient))
                  (not (member (plist-get legend :orient) eas-spec-props-honored-legend-orients)))
         (list (eas-spec-props--finding "legend" :orient (concat path "/legend"))))
       (when (and (eas-spec-props--obj-p legend) (numberp (plist-get legend :columns)) (> (plist-get legend :columns) 1))
         ;; One column is the vertical layout every legend has.
         (list (eas-spec-props--finding "legend" :columns (concat path "/legend"))))
       (when (and (eas-spec-props--obj-p legend) (stringp (plist-get legend :direction))
                  (not (member (plist-get def :type) '("quantitative" "temporal"))))
         ;; Only a gradient legend turns horizontal natively.
         (list (eas-spec-props--finding "legend" :direction (concat path "/legend"))))
       (eas-spec-props--scan scale "scale"
                             (eas-spec-props--ignored (if (memq channel '(:x :y)) 'position-scale 'scale))
                             (concat path "/scale"))))))

(defun eas-spec-props--config (config)
  "Findings in CONFIG."
  (when (eas-spec-props--obj-p config)
    (append
     (eas-spec-props--scan config "config" (eas-spec-props--ignored 'config) "/config")
     (cl-loop for (section kind) in '((:axis axis) (:axisX axis) (:axisY axis) (:legend legend)
                                      (:title title) (:view view))
              append (eas-spec-props--scan (plist-get config section) (concat "config." (eas-key-name section))
                                           (eas-spec-props--ignored kind) (concat "/config/" (eas-key-name section))))
     (cl-loop for section in '(:mark :area :arc :line :bar :point :text :rect)
              for c = (plist-get config section)
              append (eas-spec-props--scan c (concat "config." (eas-key-name section))
                                           (append (eas-spec-props--ignored 'mark)
                                                   (unless (eq section :line) eas-spec-props-ignored-mark-config))
                                           (concat "/config/" (eas-key-name section)))))))

(defun eas-spec-props--view (view path styles)
  "Findings in VIEW (a unit, layer or concat) at PATH, recursively.
STYLES is config.style: a mark style naming one of its entries is ignored."
  (when (eas-spec-props--obj-p view)
    (append
     (let ((mark (plist-get view :mark)))
       (append (eas-spec-props--scan mark "mark" (eas-spec-props--ignored 'mark) (concat path "/mark"))
               (when (and (eas-spec-props--obj-p mark)
                          (seq-some (lambda (st) (plist-member styles (eas-key st)))
                                    (let ((st (plist-get mark :style))) (cond ((stringp st) (list st)) ((vectorp st) (append st nil))))))
                 (list (eas-spec-props--finding "mark" :style (concat path "/mark"))))))
     (let ((title (plist-get view :title)))
       (eas-spec-props--scan title "title" (eas-spec-props--ignored 'title) (concat path "/title")))
     (let ((enc (plist-get view :encoding)))
       (when (eas-spec-props--obj-p enc)
         (cl-loop for (channel def) on enc by #'cddr
                  append (eas-spec-props--def channel def (concat path "/encoding/" (eas-key-name channel))))))
     (cl-loop for key in '(:layer :vconcat :hconcat :concat)
              for children = (plist-get view key)
              when (vectorp children)
              append (cl-loop for child across children for i from 0
                              append (eas-spec-props--view child (format "%s/%s/%d" path (eas-key-name key) i) styles)))
     (when (eas-spec-props--obj-p (plist-get view :spec))
       (eas-spec-props--view (plist-get view :spec) (concat path "/spec") styles)))))

(defun eas-spec-props-findings (spec)
  "UNSUPPORTED_FEATURE findings (with :ignored t) for the Vega-Lite
properties SPEC sets that native rendering ignores."
  (when (eas-spec-props--obj-p spec)
    (append (eas-spec-props--view spec "" (plist-get (plist-get spec :config) :style))
            (eas-spec-props--config (plist-get spec :config)))))

(provide 'eas-spec-props)
;;; eas-spec-props.el ends here

;;; eas-composite-boxplot.el --- the boxplot composite mark, expanded as Vega-Lite does -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (fc-qx1.49).  Like errorbar and errorband
;; (eas-composite.el), a boxplot is a macro over primitive marks, and
;; `eas-composite-boxplot-expand' rewrites one into the layers
;; Vega-Lite's normalizer produces, so every renderer draws it from
;; ordinary units:
;;
;;   whiskers   two rules, lower whisker to q1 and q3 to upper whisker
;;   box        a bar from q1 to q3, mark.size (default 14) thick
;;   median     a white tick at the median, as thick as the box
;;   outliers   points beyond the whiskers (not with extent "min-max")
;;
;; extent "min-max" puts the whiskers at the extremes; a number K
;; (default 1.5) at the furthest data inside q1 - K*IQR .. q3 + K*IQR.
;; Data fields are grouped by every other channel's field.

;;; Code:

(require 'eas-core)

(declare-function eas-composite--merge "eas-composite")
(declare-function eas-composite--continuous "eas-composite")

(defun eas-composite-boxplot--grouping (encoding channel partner)
  "Return (GROUPBY TIME-TRANSFORMS ENCODING) for the other channels.
The other channels are ENCODING's channels besides CHANNEL and PARTNER:
GROUPBY is the fields a boxplot groups by, TIME-TRANSFORMS the
timeUnits they need, and ENCODING the encoding that shows them."

  (let (groupby time-tx enc)
    (cl-loop for (ch d) on encoding by #'cddr
             unless (memq ch (list channel partner))
             do (if (and (eas-object-p d) d (stringp (plist-get d :field)) (not (plist-get d :aggregate)))
                    (let* ((unit (plist-get d :timeUnit))
                           (as (if unit (format "%s_%s" unit (plist-get d :field)) (plist-get d :field))))
                      (when unit (push (list :timeUnit unit :field (plist-get d :field) :as as) time-tx))
                      (push as groupby)
                      (setq enc (append enc (list ch (append (list :field as :type (or (plist-get d :type) (if unit "temporal" "nominal")))
                                                             (unless (plist-member d :title)
                                                               (list :title (if unit (format "%s (%s)" (plist-get d :field) unit)
                                                                              (plist-get d :field))))
                                                             (eas--plist-without (eas--plist-without (eas--plist-without d :field) :timeUnit) :type))))))
                  (setq enc (append enc (list ch d)))))
    (list (vconcat (delete-dups (nreverse groupby))) (nreverse time-tx) enc)))

(defun eas-composite-boxplot--fences (field k groupby)
  "Transforms adding FIELD's quartiles and K*IQR fences to every row of GROUPBY."
  (list (list :joinaggregate (vector (list :op "q1" :field field :as (concat "lower_box_" field))
                                     (list :op "q3" :field field :as (concat "upper_box_" field)))
              :groupby groupby)
        (list :calculate (format "datum['lower_box_%s'] - %s * (datum['upper_box_%s'] - datum['lower_box_%s'])" field k field field)
              :as (concat "lower_fence_" field))
        (list :calculate (format "datum['upper_box_%s'] + %s * (datum['upper_box_%s'] - datum['lower_box_%s'])" field k field field)
              :as (concat "upper_fence_" field))))

(defun eas-composite-boxplot-expand (node inherited)
  "A layer of primitive units drawing boxplot NODE; INHERITED as for errorbar."
  (let* ((mark (plist-get node :mark))
         (encoding (eas-composite--merge inherited (plist-get node :encoding)))
         (channel (or (eas-composite--continuous encoding)
                      (eas-signal "INVALID_INPUT" "boxplot needs a quantitative field to span" :path "/encoding")))
         (partner (if (eq channel :x) :x2 :y2))
         (def (plist-get encoding channel))
         (field (plist-get def :field))
         (grouping (eas-composite-boxplot--grouping encoding channel partner))
         (groupby (nth 0 grouping)) (time-tx (nth 1 grouping)) (enc (nth 2 grouping))
         (extent (or (plist-get mark :extent) 1.5))
         (minmax (equal extent "min-max"))
         (size (or (plist-get mark :size) 14))
         (orient (if (eq channel :y) "vertical" "horizontal"))
         (title (if (plist-member def :title) (plist-get def :title) field))
         (pos (lambda (f &optional titled)
                (append (list :field (concat f "_" field) :type "quantitative")
                        (when titled (list :title title))
                        (eas--plist-without (eas--plist-without (eas--plist-without (eas--plist-without def :field) :type)
                                                                :aggregate)
                                            :title))))
         (span (lambda (lo hi) (append enc (list channel (funcall pos lo t) partner (list :field (concat hi "_" field))))))
         (stats (vconcat
                 (append (plist-get node :transform) time-tx
                         (if minmax
                             (list (list :aggregate (vector (list :op "min" :field field :as (concat "lower_whisker_" field))
                                                            (list :op "max" :field field :as (concat "upper_whisker_" field))
                                                            (list :op "q1" :field field :as (concat "lower_box_" field))
                                                            (list :op "q3" :field field :as (concat "upper_box_" field))
                                                            (list :op "median" :field field :as (concat "mid_box_" field)))
                                         :groupby groupby))
                           (append (eas-composite-boxplot--fences field extent groupby)
                                   (list (list :joinaggregate (vector (list :op "median" :field field :as (concat "mid_box_" field)))
                                               :groupby groupby)
                                         (list :filter (format "datum['%s'] >= datum['lower_fence_%s'] && datum['%s'] <= datum['upper_fence_%s']"
                                                               field field field field))
                                         (list :aggregate (vector (list :op "min" :field field :as (concat "lower_whisker_" field))
                                                                  (list :op "max" :field field :as (concat "upper_whisker_" field))
                                                                  (list :op "max" :field (concat "lower_box_" field) :as (concat "lower_box_" field))
                                                                  (list :op "max" :field (concat "upper_box_" field) :as (concat "upper_box_" field))
                                                                  (list :op "max" :field (concat "mid_box_" field) :as (concat "mid_box_" field)))
                                               :groupby groupby)))))))
         (style (cl-loop for k in '(:color :opacity :tooltip :clip) when (plist-member mark k) append (list k (plist-get mark k))))
         (boxes (list (list :transform stats :mark (list :type "rule") :encoding (funcall span "lower_whisker" "lower_box"))
                      (list :transform stats :mark (list :type "rule") :encoding (funcall span "upper_box" "upper_whisker"))
                      (list :transform stats :mark (append (list :type "bar" :size size :orient orient) style)
                            :encoding (funcall span "lower_box" "upper_box"))
                      (list :transform stats :mark (list :type "tick" :color "white" :size size :orient (if (eq channel :y) "horizontal" "vertical"))
                            :encoding (append enc (list channel (funcall pos "mid_box" t))))))
         (outliers (unless minmax
                     (list (list :transform (vconcat (append (plist-get node :transform) time-tx
                                                             (eas-composite-boxplot--fences field extent groupby)
                                                             (list (list :filter (format "datum['%s'] < datum['lower_fence_%s'] || datum['%s'] > datum['upper_fence_%s']"
                                                                                         field field field field)))))
                                 :mark (list :type "point" :style "boxplot-outliers")
                                 :encoding (append enc (list channel (append (list :title title) def))))))))
    (append (eas--plist-without (eas--plist-without (eas--plist-without node :mark) :encoding) :transform)
            (list :layer (vconcat (append outliers boxes))))))

(provide 'eas-composite-boxplot)
;;; eas-composite-boxplot.el ends here

;;; eas-composite.el --- errorbar and errorband, expanded as Vega-Lite does -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite's composite marks are macros over primitive
;; marks; `eas-composite-expand' rewrites them before compile the way
;; Vega-Lite normalizes them, so everything downstream (scales, hit
;; tests, both renderers) sees ordinary units:
;;
;;   errorbar   aggregate by the other channels' fields, then a rule
;;              from lower to upper along the continuous channel
;;   errorband  the same aggregate, then an area at opacity 0.3
;;
;; `extent' picks the interval: "stderr" (default) and "stdev" around
;; the mean, "ci" (bootstrap ci0..ci1), "iqr" (q1..q3).  Fields under a
;; timeUnit are grouped by their truncated value, as Vega-Lite does.

;;; Code:

(require 'eas-core)

(declare-function eas-composite-boxplot-expand "eas-composite-boxplot")

(defconst eas-composite-marks '("errorbar" "errorband")
  "Composite mark types `eas-composite-expand' rewrites.")

(defun eas-composite--merge (a b)
  "Encoding A with B's channels laid over it."
  (let ((out a))
    (cl-loop for (k v) on b by #'cddr do (setq out (eas-plist-put out k v)))
    out))

(defun eas-composite--continuous (encoding)
  "The channel of ENCODING the interval runs along: a quantitative field
that is not aggregated, y before x."
  (seq-find (lambda (ch)
              (let ((d (plist-get encoding ch)))
                (and (eas-object-p d) d (plist-get d :field) (not (plist-get d :aggregate))
                     (equal (or (plist-get d :type) "quantitative") "quantitative")
                     (not (plist-get d :timeUnit)))))
            (if (let ((x (plist-get encoding :x)))
                  (and (eas-object-p x) x (member (plist-get x :type) '("nominal" "ordinal" "temporal"))))
                '(:y :x) '(:x :y))))

(defun eas-composite--stats (extent field)
  "Transforms giving lower_FIELD and upper_FIELD for EXTENT, given groupby slot.
Return (AGGREGATE-OPS . CALCULATES)."
  (let ((lower (concat "lower_" field)) (upper (concat "upper_" field))
        (center (concat "center_" field)) (spread (concat "extent_" field)))
    (pcase extent
      ("ci" (cons (list (list :op "ci0" :field field :as lower) (list :op "ci1" :field field :as upper)) nil))
      ("iqr" (cons (list (list :op "q1" :field field :as lower) (list :op "q3" :field field :as upper)) nil))
      (_ (cons (list (list :op (if (equal extent "stdev") "stdev" "stderr") :field field :as spread)
                     (list :op "mean" :field field :as center))
               (list (list :calculate (format "datum['%s'] - datum['%s']" center spread) :as lower)
                     (list :calculate (format "datum['%s'] + datum['%s']" center spread) :as upper)))))))

(defun eas-composite--expand-unit (node inherited)
  "Primitive unit for composite NODE whose encoding extends INHERITED."
  (let* ((mark (plist-get node :mark))
         (type (plist-get mark :type))
         (encoding (eas-composite--merge inherited (plist-get node :encoding)))
         (channel (or (eas-composite--continuous encoding)
                      (eas-signal "INVALID_INPUT"
                                  (format "%s needs a quantitative field to span" type)
                                  :path "/encoding")))
         (def (plist-get encoding channel))
         (field (plist-get def :field))
         (partner (if (eq channel :x) :x2 :y2))
         (time-tx nil) (groupby nil) (enc nil))
    (cl-loop for (ch d) on encoding by #'cddr
             unless (memq ch (list channel partner))
             do (if (and (eas-object-p d) d (stringp (plist-get d :field)) (not (plist-get d :aggregate)))
                    (let* ((unit (plist-get d :timeUnit))
                           (as (if unit (format "%s_%s" unit (plist-get d :field)) (plist-get d :field))))
                      (when unit
                        (push (list :timeUnit unit :field (plist-get d :field) :as as) time-tx))
                      (push as groupby)
                      (setq enc (append enc (list ch (append (list :field as :type (or (plist-get d :type) (if unit "temporal" "nominal")))
                                                             (unless (plist-member d :title)
                                                               (list :title (if unit (format "%s (%s)" (plist-get d :field) unit)
                                                                              (plist-get d :field))))
                                                             (eas--plist-without (eas--plist-without (eas--plist-without d :field) :timeUnit) :type))))))
                  (setq enc (append enc (list ch d)))))
    (let ((stats (eas-composite--stats (or (plist-get mark :extent) "stderr") field)))
      (append
       (eas--plist-without (eas--plist-without node :mark) :encoding)
       (list :transform (vconcat (append (plist-get node :transform) (nreverse time-tx)
                                         (list (list :aggregate (vconcat (car stats)) :groupby (vconcat (delete-dups (nreverse groupby)))))
                                         (cdr stats)))
             :mark (append (if (equal type "errorband")
                               (list :type "area" :opacity 0.3)
                             (list :type "rule"))
                           (cl-loop for k in '(:color :opacity :interpolate :clip :tooltip)
                                    when (plist-member mark k) append (list k (plist-get mark k)))
                           (when (plist-get mark :thickness) (list :strokeWidth (plist-get mark :thickness))))
             :encoding (append enc
                               (list channel (append (list :field (concat "lower_" field) :type "quantitative")
                                                     (unless (plist-member def :title) (list :title field))
                                                     (eas--plist-without (eas--plist-without (eas--plist-without def :field) :type) :aggregate))
                                     partner (list :field (concat "upper_" field)))))))))

(defun eas-composite-expand (spec &optional inherited)
  "SPEC with composite marks rewritten into primitive units.
INHERITED is the encoding a layer passes down."
  (let ((mark (plist-get spec :mark)))
    (cond
     ((and mark (equal (if (stringp mark) mark (plist-get mark :type)) "boxplot"))
      ;; eas-composite-boxplot.el (fc-qx1.49)
      (eas-composite-expand (eas-composite-boxplot-expand
                             (plist-put (copy-sequence spec) :mark (if (stringp mark) (list :type mark) mark))
                             inherited)
                            inherited))
     ((and mark (member (if (stringp mark) mark (plist-get mark :type)) eas-composite-marks))
      (eas-composite--expand-unit (plist-put (copy-sequence spec) :mark (if (stringp mark) (list :type mark) mark))
                                  inherited))
     ((plist-get spec :layer)
      (let ((down (eas-composite--merge inherited (plist-get spec :encoding))))
        (plist-put (copy-sequence spec) :layer
                   (vconcat (mapcar (lambda (c) (eas-composite-expand c down)) (plist-get spec :layer))))))
     ((or (plist-get spec :vconcat) (plist-get spec :hconcat))
      (let ((key (if (plist-get spec :vconcat) :vconcat :hconcat)))
        (plist-put (copy-sequence spec) key
                   (vconcat (mapcar #'eas-composite-expand (plist-get spec key))))))
     (t spec))))

(provide 'eas-composite)
(require 'eas-composite-boxplot)
;;; eas-composite.el ends here

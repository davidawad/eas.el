;;; eas-parallel.el --- the parallel-coordinates domain transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Parallel coordinates put one vertical axis per field side by side and
;; draw each row as a polyline through its values.  Every axis has its
;; own scale, which Vega-Lite cannot express on one y channel, so the
;; "parallel-coordinates" transform does the scaling at resolve time:
;;
;;   {"x-eas:transform": "parallel-coordinates",
;;    "fields": ["Cylinders", "Displacement", ...]}
;;
;; folds each row into one row per field, {..., index, key, value,
;; norm, part: "line"}, where norm is the value's position (0 bottom, 1
;; top) on that field's linear domain (zero false, niced as Vega nices
;; it), and appends the axes as rows too: per field one {key, norm 0,
;; norm2 1, part: "axis"} and per tick {key, norm, value, label, part:
;; "tick"}, with ticks
;; and labels as a Vega linear axis of "ticks" ticks would draw them.
;; A template then draws lines, rules and text on one point x scale
;; and one [0, 1] y scale.  Rows missing a field's number are dropped,
;; as the polyline would break there.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-transform-domain)

(defun eas-parallel--domain (rows key nice zero)
  "Linear scale over the numbers of field KEY in ROWS, NICE and ZERO."
  (let ((nums (delq nil (mapcar (lambda (r) (let ((v (plist-get r key))) (and (numberp v) v))) rows))))
    (eas-scale-continuous "linear" (if nums (apply #'min nums) 0) (if nums (apply #'max nums) 1) [0 1]
                          :nice nice :zero zero)))

(defun eas-parallel--norm (scale v)
  "Position 0..1 of V on SCALE's domain."
  (let* ((d (plist-get scale :domain)) (span (- (aref d 1) (aref d 0))))
    (if (zerop span) 0.5 (/ (- v (aref d 0)) span))))

(defun eas-parallel--transform (rows params)
  "The parallel-coordinates domain transform of ROWS per PARAMS."
  (let* ((fields (append (plist-get params :fields) nil))
         (keys (mapcar #'eas-key fields))
         (as (plist-get params :as))
         (k-index (eas-key (aref as 0))) (k-key (eas-key (aref as 1)))
         (k-value (eas-key (aref as 2))) (k-norm (eas-key (aref as 3)))
         (nice (not (eq (plist-get params :nice) :false)))
         (zero (eq (plist-get params :zero) t))
         (count (plist-get params :ticks))
         (formats (plist-get params :format))
         (rows (seq-filter (lambda (r) (seq-every-p (lambda (k) (numberp (plist-get r k))) keys))
                           (append rows nil)))
         (scales (mapcar (lambda (k) (eas-parallel--domain rows k nice zero)) keys))
         (index -1) out)
    (dolist (row rows)
      (setq index (1+ index))
      (cl-loop for field in fields for key in keys for scale in scales
               for v = (plist-get row key)
               do (push (append row (list k-index index k-key field k-value v
                                          k-norm (eas-parallel--norm scale v) :part "line"))
                        out)))
    (cl-loop for field in fields for key in keys for scale in scales
             for format = (and (eas-object-p formats) (plist-get formats key))
             for fmt = (eas-scale-tick-format scale count (and (stringp format) format))
             do (push (list k-key field k-norm 0 :norm2 1 :part "axis") out)
             (when (> count 0)
               (dolist (tick (eas-scale-ticks scale count))
                 (push (list k-key field k-norm (eas-parallel--norm scale tick)
                             k-value tick :label (funcall fmt tick) :part "tick")
                       out))))
    (vconcat (nreverse out))))

(eas-register-transform
 "parallel-coordinates"
 :doc "Fold rows into one per field, scaled 0..1 on each field's own niced domain, plus axis tick rows."
 :schema '(:fields (:type "array" :required t :doc "numeric fields, one axis each, left to right")
           :as (:type "array" :default ["index" "key" "value" "norm"]
                :doc "output fields: row index, field name, value, 0..1 position")
           :nice (:type "boolean" :default t :doc "nice each field's domain")
           :zero (:type "boolean" :default :false :doc "include zero in each domain")
           :ticks (:type "integer" :default 10 :doc "tick count of each axis; 0 for none")
           :format (:type "object" :doc "d3 format per field for its tick labels"))
 :fn #'eas-parallel--transform)

(provide 'eas-parallel)
;;; eas-parallel.el ends here

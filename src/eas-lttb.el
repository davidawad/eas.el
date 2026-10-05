;;; eas-lttb.el --- largest-triangle-three-buckets decimation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; LTTB (Steinarsson 2013) keeps the visual shape of a series with
;; THRESHOLD points.  `eas-lttb-indices' returns indices into the
;; input, so callers keep datum back-references to the original rows.
;; Compile uses it when a series has more points than pixel columns;
;; the "lttb" domain transform exposes it to specs.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)
(require 'eas-time)

(defun eas-lttb-indices (xs ys threshold)
  "Return a vector of indices selecting at most THRESHOLD points.
XS and YS are equal-length vectors of numbers, XS ascending.  The first
and last points are always kept."
  (let ((n (length xs)))
    (if (or (<= n threshold) (< threshold 3))
        (vconcat (number-sequence 0 (1- n)))
      (let* ((every (/ (float (- n 2)) (- threshold 2)))
             (out (make-vector threshold 0))
             (a 0))
        (dotimes (i (- threshold 2))
          (let* ((avg-start (min n (1+ (floor (* (1+ i) every)))))
                 (avg-end (min n (1+ (floor (* (+ i 2) every)))))
                 (avg-end (if (= i (- threshold 3)) n avg-end))
                 (count (max 1 (- avg-end avg-start)))
                 (avg-x (/ (cl-loop for j from avg-start below avg-end sum (aref xs j)) (float count)))
                 (avg-y (/ (cl-loop for j from avg-start below avg-end sum (aref ys j)) (float count)))
                 (start (1+ (floor (* i every))))
                 (end (1+ (floor (* (1+ i) every))))
                 (ax (aref xs a)) (ay (aref ys a))
                 (best start) (best-area -1.0))
            (cl-loop for j from start below (min end (1- n))
                     for area = (abs (- (* (- ax avg-x) (- (aref ys j) ay))
                                        (* (- ax (aref xs j)) (- avg-y ay))))
                     when (> area best-area) do (setq best j best-area area))
            (aset out (1+ i) best)
            (setq a best)))
        (aset out (1- threshold) (1- n))
        out))))

(defun eas-lttb--transform (rows params)
  "The lttb domain transform: ROWS decimated per PARAMS."
  (let* ((x (eas-key (plist-get params :x)))
         (y (eas-key (plist-get params :y)))
         (threshold (plist-get params :threshold))
         (xs (vconcat (seq-map (lambda (r) (or (eas-time-parse (plist-get r x)) 0)) rows)))
         (ys (vconcat (seq-map (lambda (r) (let ((v (plist-get r y))) (if (numberp v) v 0))) rows))))
    (vconcat (seq-map (lambda (i) (aref rows i)) (eas-lttb-indices xs ys threshold)))))


(eas-register-transform
 "lttb"
 :doc "Largest-triangle-three-buckets decimation of an ordered series to at most threshold rows."
 :schema '(:x (:type "string" :required t :doc "ordered x field (number or date)")
           :y (:type "string" :required t :doc "numeric y field")
           :threshold (:type "integer" :default 1000 :doc "rows to keep"))
 :fn #'eas-lttb--transform)

(eas-register-transform
 "reference-band"
 :doc "Stub: add constant lo/hi reference columns; domain packages register range lookups."
 :schema '(:marker (:type "string" :doc "what the band is for, e.g. ldl-c")
           :lo (:type "number" :doc "lower bound") :hi (:type "number" :doc "upper bound")
           :as (:type "array" :default ["lo" "hi"] :doc "output column names"))
 :fn (lambda (rows params)
       (let ((as (plist-get params :as)))
         (seq-map (lambda (row)
                    (append row (list (eas-key (aref as 0)) (or (plist-get params :lo) :null)
                                      (eas-key (aref as 1)) (or (plist-get params :hi) :null))))
                  rows))))

(provide 'eas-lttb)
;;; eas-lttb.el ends here

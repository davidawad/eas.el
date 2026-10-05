;;; eas-transform.el --- the native Vega-Lite transforms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L1.  `eas-transform-run' applies a Vega-Lite transform array to
;; rows: filter, calculate, fold, timeUnit and bin here; aggregate,
;; joinaggregate and window in eas-transform-agg.el; domain
;; transforms through the registry.  Anything else is
;; UNSUPPORTED_FEATURE with the transform's JSON path.
;;
;; ENV is a plist of param values keyed by param name; expressions read
;; it, and `eas-transform-param-predicate' decides {"param": NAME}
;; filters (the runtime installs selection semantics there).

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-expr)
(require 'eas-transform-agg)
(require 'eas-transform-domain)
(require 'eas-transform-dist)

(defvar eas-transform-param-predicate
  (lambda (_param _row _env empty) empty)
  "Function (PARAM ROW ENV EMPTY) deciding a {\"param\": ...} predicate.
EMPTY is what an empty selection means.  The runtime replaces this
with Vega-Lite selection semantics.")

;;; filter

(defvar eas-transform-param-filter-function nil
  "When non-nil, a function (PARAM ROWS EMPTY) answering a bare param filter.
A bare filter is {\"param\": NAME} with at most \"empty\".  It returns
the passing ROWS in order, or nil to test row by row with
`eas-transform-param-predicate'.  The runtime indexes point
selections here (eas-params-index.el, fc-qx1.9).")

(defun eas-transform--filter (pred rows env)
  "ROWS satisfying filter PRED under ENV."
  (or (and eas-transform-param-filter-function
           (consp pred) (plist-get pred :param)
           (null (eas--plist-without (eas--plist-without pred :param) :empty))
           (funcall eas-transform-param-filter-function (plist-get pred :param) rows
                    (not (eq (plist-get pred :empty) :false))))
      (seq-filter (lambda (row) (eas-transform-predicate pred row env)) rows)))

(defun eas-transform--field-value (pred row)
  "Value of PRED's field in ROW, truncated by PRED's timeUnit if any."
  (let ((value (plist-get row (eas-key (plist-get pred :field)))))
    (if-let* ((unit (plist-get pred :timeUnit)))
        (eas-time-unit-floor unit value)
      value)))

(defun eas-transform--pred-value (pred key)
  "PRED's comparison value under KEY, truncated by PRED's timeUnit if any."
  (let ((v (plist-get pred key)) (unit (plist-get pred :timeUnit)))
    (cond ((null unit) v)
          ((vectorp v) (vconcat (mapcar (lambda (x) (eas-time-unit-floor unit x)) v)))
          (t (eas-time-unit-floor unit v)))))

(defun eas-transform-predicate (pred row env)
  "Non-nil when ROW satisfies Vega-Lite predicate PRED under ENV."
  (cond
   ((stringp pred) (eas-expr-truthy (eas-expr-evaluate pred row env)))
   ((plist-get pred :and) (seq-every-p (lambda (p) (eas-transform-predicate p row env))
                                       (plist-get pred :and)))
   ((plist-get pred :or) (seq-some (lambda (p) (eas-transform-predicate p row env))
                                   (plist-get pred :or)))
   ((plist-member pred :not) (not (eas-transform-predicate (plist-get pred :not) row env)))
   ((plist-get pred :param)
    (funcall eas-transform-param-predicate (plist-get pred :param) row env
             (not (eq (plist-get pred :empty) :false))))
   ((plist-get pred :field)
    (let ((v (eas-transform--field-value pred row)))
      (cond
       ((plist-member pred :equal) (eas-expr--equal v (eas-transform--pred-value pred :equal)))
       ((plist-member pred :lt) (and (numberp v) (< v (eas-transform--pred-value pred :lt))))
       ((plist-member pred :lte) (and (numberp v) (<= v (eas-transform--pred-value pred :lte))))
       ((plist-member pred :gt) (and (numberp v) (> v (eas-transform--pred-value pred :gt))))
       ((plist-member pred :gte) (and (numberp v) (>= v (eas-transform--pred-value pred :gte))))
       ((plist-member pred :range)
        (let ((r (plist-get pred :range)))
          (and (numberp v) (<= (aref r 0) v (aref r 1)))))
       ((plist-member pred :oneOf) (seq-some (lambda (x) (eas-expr--equal v x)) (plist-get pred :oneOf)))
       ((plist-member pred :valid)
        (eq (not (memq v '(nil :null))) (eas-true-p (plist-get pred :valid))))
       (t (eas-signal "UNSUPPORTED_FEATURE" "Field predicate needs equal, lt, lte, gt, gte, range, oneOf or valid"
                        :feature "predicate/other")))))
   (t (eas-signal "INVALID_INPUT" "A filter is an expression string or a predicate object"))))

;;; timeUnit

(defconst eas-time-unit-parts
  '("year" "quarter" "month" "date" "day" "hours" "minutes" "seconds" "milliseconds")
  "Vega-Lite time unit components eas supports (week is not supported).")

(defun eas-time-unit-components (unit)
  "Return the component list of time UNIT such as \"yearmonthdate\"."
  (let ((rest (string-remove-prefix "utc" unit)) parts)
    (while (not (string-empty-p rest))
      (let ((part (seq-find (lambda (p) (string-prefix-p p rest))
                            '("year" "quarter" "month" "date" "day" "hours" "minutes"
                              "seconds" "milliseconds"))))
        (unless part
          (eas-signal "UNSUPPORTED_FEATURE" (format "Time unit %s is not supported" unit)
                        :feature (concat "timeUnit/" unit)))
        (push part parts)
        (setq rest (substring rest (length part)))))
    (nreverse parts)))

(defun eas-time-unit-floor (unit value)
  "Truncate date VALUE to time UNIT; return epoch ms or `:null'.
Absent components take Vega's defaults (year 2012, January, day 1)."
  (let* ((ms (eas-time-parse value))
         (parts (eas-time-unit-components unit))
         ;; utc time units floor in UTC; the others in local time.
         (eas-time-zone (unless (string-prefix-p "utc" unit) eas-time-zone)))
    (if (null ms) :null
      (let* ((f (eas-time-fields ms))
             (has (lambda (p) (member p parts)))
             (month (cond ((funcall has "month") (plist-get f :month))
                          ((funcall has "quarter") (1+ (* 3 (/ (1- (plist-get f :month)) 3))))
                          (t 1))))
        (eas-time-ms (if (funcall has "year") (plist-get f :year) 2012)
                       month
                       (cond ((funcall has "date") (plist-get f :day))
                             ((funcall has "day") (1+ (plist-get f :weekday)))
                             (t 1))
                       (if (funcall has "hours") (plist-get f :hours) 0)
                       (if (funcall has "minutes") (plist-get f :minutes) 0)
                       (if (funcall has "seconds") (plist-get f :seconds) 0)
                       (if (funcall has "milliseconds") (plist-get f :milliseconds) 0))))))

;;; bin

(defun eas-bin-params (extent &optional opts)
  "Vega's bin parameters (:start :stop :step) for EXTENT and OPTS.
OPTS is a Vega-Lite bin object (:maxbins :step :nice :base :minstep)."
  (let* ((maxb (or (plist-get opts :maxbins) 10))
         (base (or (plist-get opts :base) 10))
         (logb (log base))
         (lo (float (aref extent 0))) (hi (float (aref extent 1)))
         (span (let ((s (- hi lo))) (if (zerop s) (if (zerop lo) 1.0 (abs lo)) s)))
         (minstep (or (plist-get opts :minstep) 0))
         (step (plist-get opts :step)))
    (unless step
      (let ((level (ceiling (/ (log maxb) logb))))
        (setq step (max minstep (expt (float base) (- (round (/ (log span) logb)) level))))
        (while (> (ceiling (/ span step)) maxb) (setq step (* step base)))
        (dolist (div '(5 2))
          (let ((v (/ step div)))
            (when (and (>= v minstep) (<= (/ span v) maxb)) (setq step v))))))
    (let* ((v (log step))
           (precision (if (>= v 0) 0 (1+ (truncate (/ (- v) logb)))))
           (eps (expt (float base) (- (1+ precision)))))
      (unless (eq (plist-get opts :nice) :false)
        (let ((floor-v (* (floor (+ (/ lo step) eps)) step)))
          (setq lo (if (< lo floor-v) (- floor-v step) floor-v)
                hi (* (ceiling (/ hi step)) step))))
      (list :start lo :stop (if (= hi lo) (+ lo step) hi) :step step))))

(defun eas-bin-value (params v)
  "Return the bin start for number V under bin PARAMS."
  (let ((start (plist-get params :start)) (stop (plist-get params :stop))
        (step (plist-get params :step)))
    (if (not (numberp v)) :null
      (let ((v (max start (min v (- stop step)))))
        (+ start (* step (floor (+ 1e-14 (/ (- v start) step)))))))))

(defun eas-transform-bin (tr rows)
  "Apply bin transform TR to ROWS."
  (let* ((field (eas-key (plist-get tr :field)))
         (opts (if (eas-object-p (plist-get tr :bin)) (plist-get tr :bin) nil))
         (as (plist-get tr :as))
         (as0 (eas-key (if (vectorp as) (aref as 0) as)))
         (as1 (eas-key (if (vectorp as) (aref as 1) (concat as "_end"))))
         (values (seq-filter #'numberp (seq-map (lambda (r) (plist-get r field)) rows)))
         (extent (or (plist-get opts :extent)
                     (if values (vector (apply #'min values) (apply #'max values)) [0 1])))
         (params (eas-bin-params extent opts)))
    (seq-map (lambda (row)
               (let ((b (eas-bin-value params (plist-get row field))))
                 (append row (list as0 b as1 (if (numberp b) (+ b (plist-get params :step)) :null)))))
             rows)))

;;; dispatch

(defun eas-transform-run (transforms rows &optional env path)
  "Apply the Vega-Lite TRANSFORMS array to ROWS and return new rows.
ENV is a plist of param values; PATH the array's JSON pointer."
  (let ((i -1) (rows (if (vectorp rows) rows (vconcat rows))))
    (seq-doseq (tr transforms)
      (setq i (1+ i))
      (let ((tpath (format "%s/%d" (or path "/transform") i)))
        (setq rows
              (vconcat
               (cond
                ((plist-get tr :x-eas:transform) (eas-transform-apply-domain tr rows tpath))
                ((plist-member tr :filter) (eas-transform--filter (plist-get tr :filter) rows env))
                ((plist-get tr :calculate)
                 (let ((as (eas-key (plist-get tr :as))) (expr (plist-get tr :calculate)))
                   (seq-map (lambda (row) (eas-plist-put row as (eas-expr-evaluate expr row env)))
                            rows)))
                ((plist-get tr :fold)
                 (let* ((as (or (plist-get tr :as) ["key" "value"]))
                        (k (eas-key (aref as 0))) (v (eas-key (aref as 1))))
                   (apply #'append
                          (seq-map (lambda (row)
                                     (seq-map (lambda (f) (append row (list k f v (plist-get row (eas-key f)))))
                                              (plist-get tr :fold)))
                                   rows))))
                ((plist-get tr :timeUnit)
                 (let ((field (eas-key (plist-get tr :field))) (as (eas-key (plist-get tr :as)))
                       (unit (plist-get tr :timeUnit)))
                   (seq-map (lambda (row) (eas-plist-put row as (eas-time-unit-floor
                                                                    unit (plist-get row field))))
                            rows)))
                ((plist-get tr :bin) (eas-transform-bin tr rows))
                ((plist-get tr :aggregate) (eas-transform-aggregate tr rows tpath))
                ((plist-get tr :joinaggregate) (eas-transform-joinaggregate tr rows tpath))
                ((plist-get tr :window) (eas-transform-window tr rows tpath))
                ((plist-get tr :flatten) (eas-transform-flatten tr rows))
                ((plist-get tr :density) (eas-transform-density tr rows))
                (t (eas-signal "UNSUPPORTED_FEATURE"
                                 (format "Transform %s is not in the native subset"
                                         (if (consp tr) (eas-key-name (car tr)) tr))
                                 :path tpath)))))))
    rows))

(provide 'eas-transform)
;;; eas-transform.el ends here

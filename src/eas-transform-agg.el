;;; eas-transform-agg.el --- aggregate, joinaggregate and window -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The grouping half of the native Vega-Lite transforms.  Aggregate ops
;; follow vega-statistics: quantiles are R-7 (d3.quantile), variance is
;; the sample variance, and null/undefined values are skipped by every
;; op except count and missing.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-transform-dist)

(defun eas-agg--valid (values)
  "VALUES without nulls, as numbers where possible."
  (seq-remove (lambda (v) (memq v '(nil :null))) values))

(defun eas-agg--numbers (values)
  "The numbers in VALUES; when there are none, its dates as epoch ms."
  (or (seq-filter #'numberp values)
      (delq nil (mapcar (lambda (v) (and (stringp v) (eas-time-parse v))) values))))

(defun eas-agg--quantile (values p)
  "R-7 quantile P of numeric VALUES."
  (let* ((sorted (vconcat (sort (seq-filter #'numberp values) #'<)))
         (n (length sorted)))
    (cond ((zerop n) :null)
          ((= n 1) (aref sorted 0))
          (t (let* ((i (* p (1- n))) (i0 (floor i)) (frac (- i i0)))
               (if (>= i0 (1- n)) (aref sorted (1- n))
                 (+ (aref sorted i0) (* frac (- (aref sorted (1+ i0)) (aref sorted i0))))))))))

(defun eas-agg--variance (values sample)
  "Variance of numeric VALUES; the SAMPLE variance when non-nil."
  (let* ((nums (seq-filter #'numberp values)) (n (length nums)))
    (if (< n (if sample 2 1)) :null
      (let* ((mean (/ (float (apply #'+ nums)) n))
             (ss (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) nums))))
        (/ ss (if sample (1- n) n))))))

(defconst eas-agg-ops
  `(("count" . ,#'length)
    ("valid" . ,(lambda (vs) (length (eas-agg--valid vs))))
    ("missing" . ,(lambda (vs) (- (length vs) (length (eas-agg--valid vs)))))
    ("distinct" . ,(lambda (vs) (length (delete-dups (copy-sequence vs)))))
    ("sum" . ,(lambda (vs) (apply #'+ (seq-filter #'numberp vs))))
    ("product" . ,(lambda (vs) (apply #'* (seq-filter #'numberp vs))))
    ("mean" . ,(lambda (vs) (let ((n (seq-filter #'numberp vs)))
                              (if n (/ (float (apply #'+ n)) (length n)) :null))))
    ("average" . ,(lambda (vs) (funcall (cdr (assoc "mean" eas-agg-ops)) vs)))
    ("median" . ,(lambda (vs) (eas-agg--quantile vs 0.5)))
    ("q1" . ,(lambda (vs) (eas-agg--quantile vs 0.25)))
    ("q3" . ,(lambda (vs) (eas-agg--quantile vs 0.75)))
    ("min" . ,(lambda (vs) (let ((n (eas-agg--numbers vs))) (if n (apply #'min n) :null))))
    ("max" . ,(lambda (vs) (let ((n (eas-agg--numbers vs))) (if n (apply #'max n) :null))))
    ("variance" . ,(lambda (vs) (eas-agg--variance vs t)))
    ("variancep" . ,(lambda (vs) (eas-agg--variance vs nil)))
    ("stdev" . ,(lambda (vs) (let ((v (eas-agg--variance vs t))) (if (numberp v) (sqrt v) v))))
    ("stdevp" . ,(lambda (vs) (let ((v (eas-agg--variance vs nil))) (if (numberp v) (sqrt v) v))))
    ("stderr" . eas-agg-stderr)
    ("ci0" . ,(lambda (vs) (or (car (eas-agg-bootstrap-ci vs)) :null)))
    ("ci1" . ,(lambda (vs) (or (cdr (eas-agg-bootstrap-ci vs)) :null))))
  "Aggregate operations: (NAME . FUNCTION of a list of values).")

(defun eas-agg-op (name path)
  "Return aggregate op NAME's function, or signal UNSUPPORTED_FEATURE at PATH."
  (or (cdr (assoc name eas-agg-ops))
      (eas-signal "UNSUPPORTED_FEATURE"
                    (format "Aggregate op %s is not supported; ops: %s" name
                            (mapconcat #'car eas-agg-ops " "))
                    :path path :feature (concat "aggregate/" (format "%s" name)))))

(defun eas-agg-apply (name values &optional path)
  "Apply aggregate op NAME to the list VALUES.
An unsupported NAME signals UNSUPPORTED_FEATURE at PATH."
  (funcall (eas-agg-op name path) values))

(defun eas-agg-default-as (op field)
  "Vega-Lite's default output name for OP over FIELD."
  (if (and (equal op "count") (null field)) "count" (format "%s_%s" op field)))

(defun eas-agg--groups (rows groupby)
  "Group ROWS by GROUPBY keys: list of (KEY-VALUES . ROWS) in first-seen order."
  (let ((table (make-hash-table :test 'equal)) order)
    (seq-doseq (row rows)
      (let* ((key (mapcar (lambda (k) (plist-get row k)) groupby))
             (group (gethash key table :none)))
        (when (eq group :none) (push key order) (setq group nil))
        (puthash key (cons row group) table)))
    (mapcar (lambda (key) (cons key (nreverse (gethash key table)))) (nreverse order))))

(defconst eas-agg-arg-ops '("argmin" "argmax")
  "Aggregate ops whose result is the group's row with the extreme field value.")

(defun eas-agg--arg (op field rows)
  "The first of ROWS whose FIELD is least (OP argmin) or greatest (argmax).
Date strings compare as their instants, as Vega's parsed dates do.  The
result is `:null' when no row has a value."
  (let (best best-v)
    (dolist (r rows)
      (let* ((v (plist-get r field)) (v (if (stringp v) (eas-time-parse v) v)))
        (when (and (numberp v) (or (null best-v) (if (equal op "argmin") (< v best-v) (> v best-v))))
          (setq best r best-v v))))
    (or best :null)))

(defun eas-agg--value (op field rows)
  "Aggregate OP over FIELD of the group ROWS."
  (if (member op eas-agg-arg-ops) (eas-agg--arg op field rows)
    (eas-agg-apply op (mapcar (lambda (r) (and field (plist-get r field))) rows))))

(defun eas-agg--specs (specs path)
  "Normalize aggregate SPECS into (OP FIELD-KEY AS-KEY) lists.
Errors point into PATH, the JSON path of SPECS."
  (seq-map-indexed
   (lambda (spec i)
     (let ((op (plist-get spec :op)) (field (plist-get spec :field)))
       (unless (member op eas-agg-arg-ops) (eas-agg-op op (format "%s/%d/op" path i)))
       (list op (and field (eas-key field))
             (eas-key (or (plist-get spec :as) (eas-agg-default-as op field))))))
   specs))

(defun eas-transform-aggregate (tr rows path)
  "Apply aggregate transform TR to ROWS.
PATH is TR's JSON path, for errors."
  (let ((groupby (mapcar #'eas-key (plist-get tr :groupby)))
        (specs (eas-agg--specs (plist-get tr :aggregate) (concat path "/aggregate"))))
    (vconcat
     (mapcar (lambda (group)
               (append (cl-loop for k in groupby for v in (car group) append (list k v))
                       (cl-loop for (op field as) in specs
                                append (list as (eas-agg--value op field (cdr group))))))
             (eas-agg--groups rows groupby)))))

(defun eas-transform-joinaggregate (tr rows path)
  "Apply joinaggregate transform TR to ROWS.
PATH is TR's JSON path, for errors."
  (let* ((groupby (mapcar #'eas-key (plist-get tr :groupby)))
         (specs (eas-agg--specs (plist-get tr :joinaggregate) (concat path "/joinaggregate")))
         (results (make-hash-table :test 'equal)))
    (dolist (group (eas-agg--groups rows groupby))
      (puthash (car group)
               (cl-loop for (op field as) in specs
                        append (list as (eas-agg--value op field (cdr group))))
               results))
    (seq-map (lambda (row)
               (append row (gethash (mapcar (lambda (k) (plist-get row k)) groupby) results)))
             rows)))

;;; window

(defun eas-agg--sort-pred (sort)
  "Return a row comparison predicate for Vega-Lite SORT field defs."
  (lambda (a b)
    (cl-loop for spec across sort
             for key = (eas-key (plist-get spec :field))
             for desc = (equal (plist-get spec :order) "descending")
             for x = (plist-get a key) for y = (plist-get b key)
             unless (equal x y)
             return (let ((lt (cond ((and (numberp x) (numberp y)) (< x y))
                                    ;; Vega's ascending: nulls first.
                                    ((memq x '(nil :null)) t)
                                    ((memq y '(nil :null)) nil)
                                    (t (string< (format "%s" x) (format "%s" y))))))
                      (if desc (not lt) lt))
             finally return nil)))

(defun eas-agg--rank-ops (op index sorted key-fn n param)
  "Return the value of ranking OP for position INDEX in SORTED (length N).
KEY-FN gives a row's sort key, ties sharing a rank; PARAM is ntile's
bucket count (default 1)."
  (let* ((key (funcall key-fn (aref sorted index)))
         (first (cl-loop for j downfrom index to 0
                         while (equal (funcall key-fn (aref sorted j)) key)
                         finally return (1+ j)))
         (last (cl-loop for j from index below n
                        while (equal (funcall key-fn (aref sorted j)) key)
                        finally return (1- j))))
    (pcase op
      ("row_number" (1+ index))
      ("rank" (1+ first))
      ("dense_rank" (1+ (length (delete-dups (cl-loop for j from 0 to index
                                                      collect (funcall key-fn (aref sorted j)))))))
      ("percent_rank" (if (= n 1) 0 (/ (float first) (1- n))))
      ("cume_dist" (/ (float (1+ last)) n))
      ("ntile" (1+ (floor (* (or param 1) index) n))))))

(defun eas-transform-window (tr rows path)
  "Apply window transform TR to ROWS, keeping the input order.
PATH is TR's JSON path, for errors."
  (let* ((groupby (mapcar #'eas-key (plist-get tr :groupby)))
         (sort (or (plist-get tr :sort) []))
         (frame (or (plist-get tr :frame) [:null 0]))
         (lo (aref frame 0)) (hi (aref frame 1))
         (key-fn (lambda (row) (mapcar (lambda (s) (plist-get row (eas-key (plist-get s :field))))
                                       sort)))
         (out (make-hash-table :test 'eq)))
    (dolist (group (eas-agg--groups rows groupby))
      (let* ((members (cdr group))
             (sorted (vconcat (if (> (length sort) 0)
                                  (sort (copy-sequence members) (eas-agg--sort-pred sort))
                                members)))
             (n (length sorted))
             ;; Without a sort Vega compares rows as all different: no peers.
             (key-fn (if (> (length sort) 0) key-fn
                       (let ((pos (make-hash-table :test 'eq)))
                         (dotimes (i n) (puthash (aref sorted i) i pos))
                         (lambda (row) (gethash row pos))))))
        (dotimes (i n)
          (let* ((row (aref sorted i))
                 (start (if (numberp lo) (max 0 (+ i lo)) 0))
                 (end (if (numberp hi) (min (1- n) (+ i hi)) (1- n)))
                 ;; Unless ignorePeers, the frame widens to rows sorting equal at either end.
                 (peers (and (> (length sort) 0) (not (eq (plist-get tr :ignorePeers) t))))
                 (start (if peers (let ((k (funcall key-fn (aref sorted start))))
                                    (while (and (> start 0) (equal k (funcall key-fn (aref sorted (1- start)))))
                                      (setq start (1- start)))
                                    start)
                          start))
                 (end (if peers (let ((k (funcall key-fn (aref sorted end))))
                                  (while (and (< end (1- n)) (equal k (funcall key-fn (aref sorted (1+ end)))))
                                    (setq end (1+ end)))
                                  end)
                        end))
                 (additions
                  (cl-loop
                   for spec across (plist-get tr :window)
                   for op = (plist-get spec :op)
                   for field = (and (plist-get spec :field) (eas-key (plist-get spec :field)))
                   for param = (plist-get spec :param)
                   for as = (eas-key (or (plist-get spec :as) (eas-agg-default-as op (plist-get spec :field))))
                   append
                   (list as
                         (pcase op
                           ((or "row_number" "rank" "dense_rank" "percent_rank" "cume_dist" "ntile")
                            (eas-agg--rank-ops op i sorted key-fn n param))
                           ("lag" (let ((j (- i (or param 1)))) (if (>= j 0) (plist-get (aref sorted j) field) :null)))
                           ("lead" (let ((j (+ i (or param 1)))) (if (< j n) (plist-get (aref sorted j) field) :null)))
                           ("first_value" (plist-get (aref sorted start) field))
                           ("last_value" (plist-get (aref sorted end) field))
                           ("nth_value" (let ((j (+ start (1- (or param 1)))))
                                          (if (<= j end) (plist-get (aref sorted j) field) :null)))
                           (_ (eas-agg-apply
                               op (cl-loop for j from start to end
                                           collect (and field (plist-get (aref sorted j) field)))
                               (format "%s/window" path))))))))
            (puthash row (append row additions) out)))))
    (seq-map (lambda (row) (gethash row out row)) rows)))

(provide 'eas-transform-agg)
;;; eas-transform-agg.el ends here

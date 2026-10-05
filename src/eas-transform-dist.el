;;; eas-transform-dist.el --- flatten and density transforms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1, Vega-Lite transforms for distributions, computed the way
;; Vega computes them so native charts match bin/chart:
;;
;; - flatten: one row per element of array fields, zipped; rows whose
;;   arrays are empty produce nothing (Vega's Flatten).
;; - density: Vega's KDE.  A Gaussian kernel density per group, with
;;   bandwidth from Scott's rule as Vega estimates it, sampled on a
;;   shared domain (the data's extent unless `extent' is given) in a
;;   uniform grid of `steps' (200), as Vega-Lite's default "shared"
;;   resolve does so that densities stack.  `counts' scales by group
;;   size, `cumulative' integrates.

;;; Code:

(require 'eas-core)
(require 'eas-memo)

;;; flatten

(defun eas-transform-flatten (tr rows)
  "Apply flatten transform TR to ROWS."
  (let* ((fields (mapcar #'eas-key (plist-get tr :flatten)))
         (as (if (vectorp (plist-get tr :as)) (mapcar #'eas-key (plist-get tr :as)) fields)))
    (apply #'append
           (seq-map (lambda (row)
                      (let* ((arrays (mapcar (lambda (f) (let ((v (plist-get row f))) (if (vectorp v) v [])))
                                             fields))
                             (m (apply #'max 0 (mapcar #'length arrays))))
                        (cl-loop for i below m
                                 collect (let ((out (copy-sequence row)))
                                           (cl-loop for a in arrays for k in as
                                                    do (setq out (eas-plist-put out k (if (< i (length a)) (aref a i) :null))))
                                           out))))
                    rows))))

;;; ci0 / ci1

(defconst eas-agg-bootstrap-samples 1000
  "Bootstrap resamples behind ci0 and ci1, as in Vega.")

(defvar eas-agg--bootstrap-memo (eas-memo-table)
  "Bootstrap intervals by their numbers: ci0 and ci1 share one resampling.")

(defun eas-agg-bootstrap-ci (values)
  "Vega's bootstrap 95% confidence interval of the mean of VALUES, as (LO . HI).
Vega resamples with Math.random(); a fixed-seed generator keeps native
charts reproducible.  nil when VALUES holds no numbers."
  (let ((nums (vconcat (seq-filter #'numberp values))))
    (when (> (length nums) 0)
      (eas-memo eas-agg--bootstrap-memo nums (eas-agg--bootstrap nums)))))

(defun eas-agg--bootstrap (nums)
  "`eas-agg-bootstrap-ci' of the non-empty vector of numbers NUMS."
  (let* ((n (length nums)) (seed 2463534242) (mu (make-vector eas-agg-bootstrap-samples 0.0)))
    (dotimes (j eas-agg-bootstrap-samples)
      (let ((a 0.0))
        (dotimes (_ n)
          ;; xorshift32
          (setq seed (logand #xffffffff (logxor seed (ash seed 13)))
                seed (logxor seed (ash seed -17))
                seed (logand #xffffffff (logxor seed (ash seed 5))))
          (setq a (+ a (aref nums (% seed n)))))
        (aset mu j (/ a n))))
    (let ((sorted (vconcat (sort (append mu nil) #'<))))
      (cons (eas-density--quantile sorted 0.025) (eas-density--quantile sorted 0.975)))))

(defun eas-agg-stderr (values)
  "Standard error of the mean of the numbers in VALUES, or :null."
  (let* ((nums (seq-filter #'numberp values)) (n (length nums)))
    (if (< n 2) :null
      (let* ((mean (/ (apply #'+ nums) (float n)))
             (var (/ (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) nums)) (1- n))))
        (sqrt (/ var n))))))

;;; density

(defun eas-density--quantile (sorted p)
  "Quantile P of the sorted vector SORTED (d3's R-7 rule)."
  (let* ((n (length sorted)) (i (* (1- n) p)) (lo (floor i)))
    (if (>= lo (1- n)) (aref sorted (1- n))
      (+ (aref sorted lo) (* (- i lo) (- (aref sorted (1+ lo)) (aref sorted lo)))))))

(defun eas-density-bandwidth (values)
  "Vega's estimateBandwidth for the numbers VALUES (Scott's rule)."
  (let* ((n (length values))
         (mean (/ (apply #'+ values) (float n)))
         (dev (if (> n 1) (sqrt (/ (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) values)) (1- n))) 0))
         (sorted (vconcat (sort (copy-sequence values) #'<)))
         (q1 (eas-density--quantile sorted 0.25)) (q3 (eas-density--quantile sorted 0.75))
         (h (/ (- q3 q1) 1.34))
         (v (let ((m (min dev h))) (cond ((/= m 0) m) ((/= dev 0) dev) ((/= q1 0) (abs q1)) (t 1)))))
    (* 1.06 v (expt n -0.2))))

(defun eas-density--pdf (values bw x)
  "Gaussian kernel density of VALUES with bandwidth BW at X."
  (let ((sum 0.0) (k (/ 1.0 (sqrt (* 2 float-pi)))))
    (dolist (v values) (let ((z (/ (- x v) bw))) (setq sum (+ sum (* k (exp (* -0.5 z z)))))))
    (/ sum (* bw (length values)))))

(defun eas-density--cdf (values bw x)
  "Gaussian kernel cumulative density of VALUES with bandwidth BW at X."
  (let ((sum 0.0))
    (dolist (v values) (setq sum (+ sum (* 0.5 (1+ (eas-density--erf (/ (- x v) (* bw (sqrt 2)))))))))
    (/ sum (length values))))

(defun eas-density--erf (x)
  "The error function at X (Abramowitz-Stegun 7.1.26, as Vega's erf)."
  (let* ((sign (if (< x 0) -1 1)) (x (abs x)) (tt (/ 1.0 (+ 1 (* 0.3275911 x))))
         (y (- 1 (* (+ (* (+ (* (+ (* (+ (* 1.061405429 tt) -1.453152027) tt) 1.421413741) tt) -0.284496736) tt)
                       0.254829592)
                    tt (exp (- (* x x)))))))
    (* sign y)))

(defun eas-transform-density (tr rows)
  "Apply density transform TR to ROWS: one row per sample per group."
  (let* ((field (eas-key (plist-get tr :density)))
         (groupby (mapcar #'eas-key (plist-get tr :groupby)))
         (as (if (vectorp (plist-get tr :as)) (plist-get tr :as) ["value" "density"]))
         (vk (eas-key (aref as 0))) (dk (eas-key (aref as 1)))
         (steps (or (plist-get tr :steps) (plist-get tr :maxsteps) 200))
         (cumulative (eq (plist-get tr :cumulative) t))
         (counts (eq (plist-get tr :counts) t))
         (groups nil) (all nil))
    (seq-doseq (row rows)
      (let ((v (plist-get row field)))
        (when (numberp v)
          (push v all)
          (let* ((key (mapcar (lambda (g) (plist-get row g)) groupby)) (cell (assoc key groups)))
            (if cell (push v (cdr cell)) (push (cons key (list v)) groups))))))
    (when all
      (let* ((extent (if (vectorp (plist-get tr :extent)) (plist-get tr :extent)
                       (vector (apply #'min all) (apply #'max all))))
             (lo (float (aref extent 0))) (hi (float (aref extent 1))) (span (- hi lo)))
        (apply #'append
               (mapcar (lambda (g)
                         (let* ((values (nreverse (cdr g)))
                                (bw (or (plist-get tr :bandwidth) (eas-density-bandwidth values)))
                                (bw (if (and (numberp bw) (> bw 0)) bw (eas-density-bandwidth values)))
                                (scale (if counts (length values) 1)))
                           (cl-loop for i from 0 to steps
                                    for x = (if (= i steps) hi (+ lo (* (/ (float i) steps) span)))
                                    for dens across (eas-density--samples values bw lo hi steps cumulative)
                                    collect (append (cl-loop for k in groupby for d in (car g) append (list k d))
                                                    (list vk x dk (* scale dens))))))
                       (nreverse groups)))))))

(defvar eas-density--memo (eas-memo-table)
  "Density samples by their inputs: a recompile does not sum the kernels again.")

(defun eas-density--samples (values bw lo hi steps cumulative)
  "Vector of the density of VALUES (bandwidth BW) at STEPS+1 points from LO
to HI, cumulative when CUMULATIVE."
  (eas-memo eas-density--memo (list values bw lo hi steps cumulative)
    (let ((span (- hi lo)))
      (vconcat (cl-loop for i from 0 to steps
                        for x = (if (= i steps) hi (+ lo (* (/ (float i) steps) span)))
                        collect (if cumulative (eas-density--cdf values bw x) (eas-density--pdf values bw x)))))))

(provide 'eas-transform-dist)
;;; eas-transform-dist.el ends here

;;; eas-expr-dist.el --- distribution functions for expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The rest of the Vega expression language's distribution functions,
;; ported from vega-statistics so native charts compute what Vega does:
;;
;;   densityNormal(x, mean=0, sd=1)       cumulativeNormal(x, mean=0, sd=1)
;;   densityLogNormal(x, mean=0, sd=1)    cumulativeLogNormal(x, mean=0, sd=1)
;;   quantileLogNormal(p, mean=0, sd=1)
;;   densityUniform(x, a=0, b=1)          cumulativeUniform(x, a=0, b=1)
;;
;; The normal CDF is West's (2005) double precision algorithm, as Vega
;; writes it; the log-normal quantile reuses `eas-expr-stats-quantile-normal'.

;;; Code:

(require 'eas-core)
(require 'eas-expr-stats)

(defconst eas-expr-dist--sqrt-2pi (sqrt (* 2 float-pi))
  "The square root of two pi.")

(defun eas-expr-dist-density-normal (x &optional mean stdev)
  "Density at X of the normal distribution with MEAN (0) and STDEV (1)."
  (let* ((s (or stdev 1)) (z (/ (- x (or mean 0)) (float s))))
    (/ (exp (* -0.5 z z)) (* s eas-expr-dist--sqrt-2pi))))

(defun eas-expr-dist-cumulative-normal (x &optional mean stdev)
  "Probability at most X under the normal distribution MEAN (0), STDEV (1)."
  (let* ((z (/ (- x (or mean 0)) (float (or stdev 1))))
         (zz (abs z))
         (cd (cond
              ((> zz 37) 0.0)
              ((< zz 7.07106781186547)
               (* (exp (/ (* (- zz) zz) 2))
                  (/ (eas-expr-stats--poly
                      zz '(3.52624965998911e-02 0.700383064443688 6.37396220353165 33.912866078383
                           112.079291497871 221.213596169931 220.206867912376))
                     (eas-expr-stats--poly
                      zz '(8.83883476483184e-02 1.75566716318264 16.064177579207 86.7807322029461
                           296.564248779674 637.333633378831 793.826512519948 440.413735824752)))))
              (t (let ((sum (+ zz 0.65)))
                   (dolist (k '(4 3 2 1)) (setq sum (+ zz (/ k sum))))
                   (/ (exp (/ (* (- zz) zz) 2)) sum 2.506628274631))))))
    (if (> z 0) (- 1 cd) cd)))

(defun eas-expr-dist-density-log-normal (x &optional mean stdev)
  "Density at X of the log-normal distribution MEAN (0), STDEV (1) of log X."
  (if (<= x 0) 0.0
    (let* ((s (or stdev 1)) (z (/ (- (log x) (or mean 0)) (float s))))
      (/ (exp (* -0.5 z z)) (* s eas-expr-dist--sqrt-2pi x)))))

(defun eas-expr-dist-cumulative-log-normal (x &optional mean stdev)
  "Probability at most X under the log-normal distribution MEAN, STDEV."
  (if (<= x 0) 0.0 (eas-expr-dist-cumulative-normal (log x) mean stdev)))

(defun eas-expr-dist-quantile-log-normal (p &optional mean stdev)
  "Quantile P of the log-normal distribution MEAN (0), STDEV (1) of log X."
  (if (or (< p 0) (> p 1)) 0.0e+NaN (exp (eas-expr-stats-quantile-normal p mean stdev))))

(defun eas-expr-dist-density-uniform (x &optional a b)
  "Density at X of the uniform distribution on [A, B] (default [0, 1])."
  (let ((a (or a 0)) (b (or b 1)))
    (if (and (<= a x) (<= x b)) (/ 1.0 (- b a)) 0.0)))

(defun eas-expr-dist-cumulative-uniform (x &optional a b)
  "Probability at most X under the uniform distribution on [A, B]."
  (let ((a (or a 0)) (b (or b 1)))
    (cond ((< x a) 0.0) ((> x b) 1.0) (t (/ (- x a) (float (- b a)))))))

(provide 'eas-expr-dist)
;;; eas-expr-dist.el ends here

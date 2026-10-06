;;; eas-scale-sequential.el --- domain and range of a continuous color scale -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A quantitative color channel compiles to a "sequential" scale over
;; the data's extent.  Vega-Lite's scale properties shape that extent
;; the way they shape a position domain: an explicit two-number
;; "domain" (or "domainMin"/"domainMax") replaces it, "zero" extends it
;; to include 0 and "nice" rounds it outward as d3's linear.nice does
;; (the legend gradient then runs over the niced domain).  "reverse"
;; flips the color range and "clamp" holds values outside the domain at
;; its end colors.  Vega-Lite leaves color domains unniced and
;; without zero by default, so these change nothing unless asked for.

;;; Code:

(require 'eas-core)
(require 'eas-scale)

(defun eas-scale-sequential-domain (sp nums)
  "Domain vector [LO HI] of a sequential color scale.
SP is the channel's scale object, NUMS the data values as numbers."
  (let* ((domain (plist-get sp :domain))
         ;; An explicit numeric domain sets the ends (its stops evenly spaced).
         (explicit (and (vectorp domain) (> (length domain) 1) (seq-every-p #'numberp domain)))
         (lo (cond (explicit (aref domain 0))
                   ((numberp (plist-get sp :domainMin)) (plist-get sp :domainMin))
                   (nums (apply #'min nums))
                   (t 0)))
         (hi (cond (explicit (aref domain (1- (length domain))))
                   ((numberp (plist-get sp :domainMax)) (plist-get sp :domainMax))
                   (nums (apply #'max nums))
                   (t 1))))
    (when (and (not explicit) (eq (plist-get sp :zero) t))
      (setq lo (min lo 0) hi (max hi 0)))
    (if (and (not explicit) (eq (plist-get sp :nice) t))
        (let ((n (eas-scale-nice-linear lo hi 10))) (vector (car n) (cdr n)))
      (vector lo hi))))

(defun eas-scale-sequential-range (sp range)
  "Return RANGE of colors, reversed when scale object SP has reverse."
  (if (and (eq (plist-get sp :reverse) t) (vectorp range))
      (reverse range)
    range))

(defun eas-scale-sequential-clamp (sp scale)
  "Return SCALE marked :clamp when scale object SP has clamp."
  (if (eq (plist-get sp :clamp) t) (append scale (list :clamp t)) scale))

(provide 'eas-scale-sequential)
;;; eas-scale-sequential.el ends here

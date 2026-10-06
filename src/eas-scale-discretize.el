;;; eas-scale-discretize.el --- quantize, quantile and threshold scales -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega's discretizing scales map a continuous domain onto
;; a few discrete outputs:
;;
;;   quantize   the domain [min, max] (zero: true includes 0) cut into
;;              equal segments, one per range value
;;   quantile   the sorted data cut at its quantiles (d3's R-7)
;;   threshold  explicit domain thresholds, one more output than them
;;
;; A scale is (:type T :domain D :thresholds [..] :range [..] :field F).
;; Ranges follow Vega-Lite: an explicit array; a size range of
;; config.scale.quantizeCount/quantileCount (4) or len(domain)+1 steps
;; from the mark's minimum size to its maximum; a color scheme ("ramp",
;; config.range.ramp, by default) sampled 5 times (len(domain)+1 for a
;; threshold), as Vega's quantizeInterpolator does.  Legends get one
;; entry per bucket, labelled "< t1", "t1 – t2", ..., "≥ tn".

;;; Code:

(require 'eas-core)
(require 'eas-scheme)
(require 'eas-format)

(defconst eas-scale-discretize-types '("quantize" "quantile" "threshold")
  "Discretizing scale types.")

(defun eas-scale-discretize-p (def)
  "Non-nil when channel DEF asks for a discretizing scale."
  (let ((sp (plist-get def :scale)))
    (and (eas-object-p sp) (member (plist-get sp :type) eas-scale-discretize-types))))

(defun eas-scale-discretize--quantile (sorted p)
  "The P quantile of the SORTED vector of numbers (d3's R-7)."
  (let* ((n (length sorted)) (h (* (1- n) p)) (i (floor h)))
    (if (>= (1+ i) n) (aref sorted (1- n))
      (+ (aref sorted i) (* (- h i) (- (aref sorted (1+ i)) (aref sorted i)))))))

(defun eas-scale-discretize--count (type sp config)
  "How many outputs a scale of TYPE with scale props SP has by default.
CONFIG may set the quantile and quantize counts."
  (pcase type
    ("threshold" (1+ (length (plist-get sp :domain))))
    (_ (or (plist-get (plist-get config :scale) (if (equal type "quantile") :quantileCount :quantizeCount)) 4))))

(defun eas-scale-discretize--colors (type sp config)
  "Color range of a discretizing scale of TYPE with scale props SP.
CONFIG supplies the default ramp."
  (let ((n (if (equal type "threshold") (1+ (length (plist-get sp :domain))) 5))
        (ramp (plist-get (plist-get config :range) :ramp)))
    (cond ((vectorp (plist-get sp :range)) (plist-get sp :range))
          ((plist-get sp :scheme) (eas-scheme-discrete-range (plist-get sp :scheme) n))
          ((and (vectorp ramp) (> (length ramp) 0)) ramp)
          ((and (eas-object-p ramp) (plist-get ramp :scheme)) (eas-scheme-discrete-range (plist-get ramp :scheme) n))
          (t (eas-scheme-discrete-range "blues" n)))))

(defun eas-scale-discretize--steps (lo hi n)
  "N values from LO to HI, evenly spaced (Vega-Lite's interpolateRange)."
  (if (<= n 1) (vector lo)
    (vconcat (cl-loop for i from 0 below n collect (+ lo (* i (/ (- hi lo) (float (1- n)))))))))

(defun eas-scale-discretize-make (def values channel config &optional mark-type)
  "Discretizing scale for channel DEF over VALUES (numbers), or nil.
CHANNEL is :size, :opacity or a color channel; CONFIG the theme; MARK-TYPE
the mark, for the size range's ends."
  (when (eas-scale-discretize-p def)
    (let* ((sp (plist-get def :scale)) (type (plist-get sp :type))
           (nums (vconcat (sort (seq-filter #'numberp values) #'<)))
           (range (cond ((memq channel '(:color :fill :stroke)) (eas-scale-discretize--colors type sp config))
                        ((vectorp (plist-get sp :range)) (plist-get sp :range))
                        (t (let ((n (eas-scale-discretize--count type sp config)))
                             (if (eq channel :opacity)
                                 (eas-scale-discretize--steps 0.3 0.8 n)
                               (eas-scale-discretize--steps (or (plist-get sp :rangeMin) (if (member mark-type '("bar" "tick")) 2 4))
                                                            (or (plist-get sp :rangeMax) 361) n))))))
           (n (length range))
           (lo (if (> (length nums) 0) (aref nums 0) 0))
           (hi (if (> (length nums) 0) (aref nums (1- (length nums))) 1))
           (lo (if (eq (plist-get sp :zero) t) (min 0 lo) lo))
           (hi (if (eq (plist-get sp :zero) t) (max 0 hi) hi))
           ;; An explicit [min, max] domain is what a quantize scale cuts.
           (explicit (let ((d (plist-get sp :domain)))
                       (and (equal type "quantize") (vectorp d) (= (length d) 2) (seq-every-p #'numberp d) d)))
           (lo (if explicit (aref explicit 0) lo))
           (hi (if explicit (aref explicit 1) hi))
           (thresholds
            (pcase type
              ("threshold" (plist-get sp :domain))
              ("quantile" (if (= (length nums) 0) []
                            (vconcat (cl-loop for i from 1 below n
                                              collect (eas-scale-discretize--quantile nums (/ i (float n)))))))
              (_ (vconcat (cl-loop for i from 1 below n collect (+ lo (* (- hi lo) (/ i (float n))))))))))
      (list :type type :domain (if (equal type "threshold") (plist-get sp :domain) (vector lo hi))
            :thresholds thresholds :range range :field (plist-get def :field)
            :values (and (equal type "quantile") nums)))))

(defun eas-scale-discretize-apply (scale value)
  "The output of discretizing SCALE for VALUE, or nil."
  (when (numberp value)
    (let ((th (plist-get scale :thresholds)) (range (plist-get scale :range)) (i 0))
      (while (and (< i (length th)) (>= value (aref th i))) (setq i (1+ i)))
      (and (> (length range) 0) (aref range (min i (1- (length range))))))))

;;; Legends

(defun eas-scale-discretize-merge-p (color-def size-def size-scale)
  "Return non-nil when SIZE-SCALE of SIZE-DEF joins COLOR-DEF's legend.
That is when SIZE-SCALE discretizes, both read the same field and the
color scale discretizes too."
  (and color-def size-def (member (plist-get size-scale :type) eas-scale-discretize-types)
       (eas-scale-discretize-p color-def)
       (equal (plist-get color-def :field) (plist-get size-def :field))))

(defun eas-scale-discretize--span-precision (span &optional count)
  "Fraction digits d3 prints for ticks over SPAN (COUNT ticks, default 30)."
  (if (<= span 0) 0
    (let* ((step0 (/ span (float (or count 30)))) (p10 (expt 10.0 (floor (log step0 10)))) (err (/ step0 p10))
           (step (* p10 (cond ((>= err (sqrt 50)) 10) ((>= err (sqrt 10)) 5) ((>= err (sqrt 2)) 2) (t 1)))))
      (max 0 (- (floor (log step 10)))))))

(defun eas-scale-discretize--formatter (scale fmt)
  "Label formatter for the thresholds of SCALE, under number format FMT.
Vega formats a quantize scale's labels by the span of its domain, a
quantile's by its closest quantiles, a threshold's as axis ticks."
  (if (stringp fmt) (lambda (v) (eas-format-number fmt v))
    (let* ((type (plist-get scale :type))
           (marks (pcase type ("quantile" (plist-get scale :thresholds)) (_ (plist-get scale :domain))))
           (gap (if (> (length marks) 1)
                    (cl-loop for i from 1 below (length marks) minimize (- (aref marks i) (aref marks (1- i))))
                  (if (> (length marks) 0) (aref marks 0) 1)))
           (digits (if (equal type "threshold")
                       (eas-scale-discretize--span-precision
                        (abs (- (aref marks (1- (length marks))) (aref marks 0))) 5)
                     (eas-scale-discretize--span-precision (abs gap)))))
      (lambda (v) (eas-format-number (format ",.%df" digits) v)))))

(defun eas-scale-discretize-entries (scale fmt color-of size-of)
  "Return the legend entries of discretizing SCALE, one per bucket.
Each is a plist (:value :label :color :size).  FMT is the legend's
number format; COLOR-OF and SIZE-OF map a bucket's value to its
symbol's color and area (or nil)."
  (let* ((th (append (plist-get scale :thresholds) nil))
         (f (eas-scale-discretize--formatter scale fmt))
         (values (cons -1.0e+INF th)))
    (vconcat
     (cl-loop for v in values for rest on values
              collect (let* ((hi (cadr rest))
                             (label (cond ((and (cl-plusp (length th)) (= v -1.0e+INF)) (concat "< " (funcall f hi)))
                                          (hi (concat (funcall f v) " – " (funcall f hi)))
                                          (t (concat "≥ " (funcall f v)))))
                             ;; A bucket's symbol shows the scales at its lower bound.
                             (probe (if (= v -1.0e+INF) (if th (1- (car th)) 0) v)))
                        ;; :formatted, the legend's format is in the label already.
                        (append (list :value probe :label label :formatted t)
                                (when color-of (list :color (funcall color-of probe)))
                                (when size-of (list :size (funcall size-of probe)))))))))

(defun eas-scale-discretize-gradient (scale fmt)
  "Return Vega's discrete gradient legend of discretizing color SCALE.
It gives the bar's value extent, its colors sampled along it and a
label per threshold, as (:domain [LO HI] :stops COLORS :entries
ENTRIES).  FMT is the legend's

number format.  A threshold scale's extent is its thresholds widened by
their mean gap on each side (Vega's labelFraction)."
  (let* ((type (plist-get scale :type)) (th (plist-get scale :thresholds))
         (marks (pcase type ("quantile" (plist-get scale :values)) (_ (plist-get scale :domain))))
         (lo (if (> (length marks) 0) (aref marks 0) 0))
         (hi (if (> (length marks) 0) (aref marks (1- (length marks))) 1))
         (adjust (if (equal type "threshold") (if (> (length marks) 1) (/ (- hi lo) (float (1- (length marks)))) 0.1) 0))
         (lo (- lo adjust)) (hi (+ hi adjust))
         (f (eas-scale-discretize--formatter scale fmt)))
    (list :domain (vector lo hi)
          :stops (vconcat (cl-loop for i from 0 to 127
                                   collect (eas-scale-discretize-apply scale (+ lo (* (- hi lo) (/ i 127.0))))))
          :entries (vconcat (mapcar (lambda (v) (list :value v :label (funcall f v))) th)))))

(provide 'eas-scale-discretize)
;;; eas-scale-discretize.el ends here

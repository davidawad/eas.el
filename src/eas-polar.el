;;; eas-polar.el --- theta/radius encodings and the arc mark -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite's polar channels: theta (angle) and radius,
;; with theta2/radius2, drawn about the plot's centre.  As in
;; Vega-Lite:
;;
;; - theta stacks by default for arcs (and wherever its stack is set):
;;   one stack, ordered by the order channel, else ascending by the
;;   color/fill/detail fields, else data order; "normalize" divides
;;   by the total.
;; - the theta scale is linear from zero onto [0, 2pi] (or its range),
;;   never niced; radius is linear (or its scale type) from zero onto
;;   [rangeMin or 0, rangeMax or min(width, height) / 2].
;; - an arc spans its stack start..end; its outer radius is radius,
;;   else mark.outerRadius, mark.radius or min(width, height) / 2; its
;;   inner radius is radius2, else mark.innerRadius.  Offsets
;;   (radiusOffset, thetaOffset) and padAngle come from the mark.
;; - other marks with theta or radius (text labels) sit at the middle
;;   of their stacked angle and at their radius.
;;
;; Arc items carry :cx :cy :innerRadius :outerRadius :startAngle
;; :endAngle (radians clockwise from 12 o'clock) and the centroid as
;; :x :y (see eas-arc.el).

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)
(require 'eas-compile-scales)
(require 'eas-marks)
(require 'eas-arc)

(defun eas-polar-unit-p (unit)
  "Non-nil when UNIT is positioned in polar coordinates."
  (let ((mark (plist-get unit :mark)) (enc (plist-get unit :encoding)))
    (or (equal (plist-get mark :type) "arc")
        (plist-get enc :theta) (plist-get enc :radius))))

;;; Stacking theta

(defun eas-polar--order (unit rows)
  "Indices of ROWS in UNIT's stacking order."
  (let* ((enc (plist-get unit :encoding))
         (order (plist-get enc :order))
         (by (seq-filter (lambda (d) (and d (eas-object-p d) (plist-get d :field) (eas-encode-discrete-p d)))
                         (mapcar (lambda (ch) (plist-get enc ch)) '(:color :fill :detail))))
         (keys (cond ((and (eas-object-p order) (plist-get order :field)) (list order)) (t by)))
         (desc (and (eas-object-p order) (equal (plist-get order :sort) "descending")))
         (idx (number-sequence 0 (1- (length rows)))))
    (if (null keys) idx
      (sort idx (lambda (a b)
                  (let ((ka (mapcar (lambda (d) (eas-encode-raw d (aref rows a))) keys))
                        (kb (mapcar (lambda (d) (eas-encode-raw d (aref rows b))) keys)))
                    (cl-loop for x in (if desc kb ka) for y in (if desc ka kb)
                             thereis (eas-compile--less x y)
                             until (eas-compile--less y x))))))))

(defun eas-polar-stack (unit)
  "UNIT with its theta channel stacked when Vega-Lite would."
  (let* ((enc (plist-get unit :encoding)) (def (plist-get enc :theta))
         (offset (and (eas-object-p def) (plist-get def :stack)))
         (arc (equal (plist-get (plist-get unit :mark) :type) "arc")))
    (if (or (not (eas-object-p def)) (not (plist-get def :field))
            (not (equal (plist-get def :type) "quantitative"))
            (plist-get enc :theta2) (memq offset '(:null :false))
            (and (null offset) (not arc)))
        unit
      (let* ((field (eas-encode-field def))
             (start (concat (plist-get def :field) "_start")) (end (concat (plist-get def :field) "_end"))
             (rows (plist-get unit :rows)) (out (copy-sequence rows)) (sum 0))
        (dolist (i (eas-polar--order unit rows))
          (let ((v (plist-get (aref rows i) field)))
            (when (numberp v)
              (aset out i (append (aref rows i) (list (eas-key start) sum (eas-key end) (+ sum v))))
              (setq sum (+ sum v)))))
        (when (and (equal offset "normalize") (/= sum 0))
          (dotimes (i (length out))
            (let ((row (aref out i)))
              (when (plist-get row (eas-key end))
                (aset out i (eas-plist-put (eas-plist-put row (eas-key start) (/ (plist-get row (eas-key start)) (float sum)))
                                           (eas-key end) (/ (plist-get row (eas-key end)) (float sum))))))))
        (thread-first unit
                      (plist-put :rows out)
                      (plist-put :encoding (eas-plist-put enc :theta
                                                          (append (list :field end :stack-start start
                                                                        :stack-field (plist-get def :field)
                                                                        :title (eas-encode-title def))
                                                                  (eas--plist-without def :field)))))))))

;;; Scales

(defun eas-polar--scale (units channel default-range)
  "Continuous scale for polar CHANNEL over UNITS, onto DEFAULT-RANGE unless
the scale sets a range, or nil when no unit encodes CHANNEL."
  (when-let* ((pairs (seq-filter (lambda (p) (plist-get (cdr p) :field)) (eas-compile--defs units channel))))
    (let* ((def (cdar pairs)) (sp (plist-get def :scale))
           (type (or (plist-get sp :type) "linear"))
           (explicit (and (vectorp (plist-get sp :domain)) (plist-get sp :domain)))
           (nums (seq-filter #'numberp (eas-compile--values pairs channel)))
           (lo (cond (explicit (aref explicit 0)) (nums (apply #'min nums)) (t 0)))
           (hi (cond (explicit (aref explicit 1)) (nums (apply #'max nums)) (t 1)))
           (zero (if (plist-member sp :zero) (eq (plist-get sp :zero) t) (not explicit)))
           (range (plist-get sp :range)))
      (unless (equal (plist-get def :type) "quantitative")
        (eas-signal "UNSUPPORTED_FEATURE" (format "%s needs a quantitative field natively" (eas-key-name channel))
                    :feature (concat "encoding/" (eas-key-name channel) "/" (or (plist-get def :type) "?"))))
      (unless (member type '("linear" "sqrt" "pow"))
        (eas-signal "UNSUPPORTED_FEATURE" (format "%s scale type %s" (eas-key-name channel) type)
                    :feature (concat "scale/" type)))
      (when zero (setq lo (min lo 0) hi (max hi 0)))
      (when (= lo hi) (setq hi (+ lo 1)))
      (append (list :type type :domain (vector (float lo) (float hi))
                    :range (if (and (vectorp range) (= (length range) 2) (seq-every-p #'numberp range))
                               range default-range)
                    :field (plist-get def :field))
              (when (plist-get sp :exponent) (list :exponent (plist-get sp :exponent)))
              (when (eq channel :radius)
                (list :rangeMin (plist-get sp :rangeMin) :rangeMax (plist-get sp :rangeMax)
                      :fitted (not (vectorp range))))))))

(defun eas-polar-scales (units)
  "The theta and radius scales UNITS share, as a plist (maybe empty)."
  (let ((theta (eas-polar--scale units :theta (vector 0 (* 2 float-pi))))
        (radius (eas-polar--scale units :radius [0 1])))
    (append (when theta (list :theta theta)) (when radius (list :radius radius)))))

(defun eas-polar-ranges (group)
  "Fit GROUP's radius scale to its placed plot; return GROUP."
  (let ((radius (plist-get (plist-get group :scales) :radius)))
    (when (and radius (plist-get radius :fitted))
      (let ((r (/ (min (plist-get group :w) (plist-get group :h)) 2.0)))
        (plist-put group :scales
                   (plist-put (plist-get group :scales) :radius
                              (plist-put (copy-sequence radius) :range
                                         (vector (or (plist-get radius :rangeMin) 0)
                                                 (or (plist-get radius :rangeMax) r)))))))
    group))

;;; Items

(defun eas-polar--value (unit scales channel row &optional mid)
  "Scaled polar CHANNEL of ROW in UNIT; the stack's middle when MID."
  (let* ((def (plist-get (plist-get unit :encoding) channel))
         (scale (plist-get scales (if (eq channel :radius2) :radius channel))))
    (cond
     ((not (eas-object-p def)) nil)
     ((plist-member def :value) (plist-get def :value))
     ((and scale (plist-get def :field))
      (let ((v (plist-get row (eas-encode-field def)))
            (s (and mid (plist-get def :stack-start) (plist-get row (eas-key (plist-get def :stack-start))))))
        (eas-scale-apply scale (if (and (numberp v) (numberp s)) (/ (+ v s) 2.0) v)))))))

(defun eas-polar--start (unit scales row)
  "Start angle of ROW's arc in UNIT: its stack start, theta2 or the range start."
  (let* ((enc (plist-get unit :encoding)) (def (plist-get enc :theta)) (scale (plist-get scales :theta)))
    (cond ((and (eas-object-p def) (plist-get def :stack-start) scale)
           (eas-scale-apply scale (plist-get row (eas-key (plist-get def :stack-start)))))
          ((plist-get enc :theta2)
           (let ((d (plist-get enc :theta2)))
             (if (plist-member d :value) (plist-get d :value)
               (and scale (eas-scale-apply scale (eas-encode-raw d row))))))
          (scale (aref (plist-get scale :range) 0))
          (t 0))))

(defun eas-polar--arc-row (unit scales bounds)
  "Row builder (ROW I -> item) for arc marks."
  (let* ((mark (plist-get unit :mark))
         (cx (+ (aref bounds 0) (/ (aref bounds 2) 2.0))) (cy (+ (aref bounds 1) (/ (aref bounds 3) 2.0)))
         (rmax (/ (min (aref bounds 2) (aref bounds 3)) 2.0))
         (num (lambda (k default) (let ((v (plist-get mark k))) (if (numberp v) v default)))))
    (lambda (row i)
      (let* ((a1 (or (eas-polar--value unit scales :theta row)
                     (funcall num :theta nil)
                     (* 2 float-pi)))
             (a0 (eas-polar--start unit scales row))
             (off (funcall num :thetaOffset 0))
             (r1 (+ (or (eas-polar--value unit scales :radius row)
                        (funcall num :outerRadius nil) (funcall num :radius nil) rmax)
                    (funcall num :radiusOffset 0)))
             (r0 (or (eas-polar--value unit scales :radius2 row) (funcall num :innerRadius 0))))
        (when (and (numberp a0) (numberp a1) (numberp r1))
          (let* ((a0 (+ a0 off)) (a1 (+ a1 off)) (am (/ (+ a0 a1) 2.0)) (rm (/ (+ r0 r1) 2.0))
                 (c (eas-arc-point cx cy rm am)))
            (append (list :datum i :x (car c) :y (cdr c) :cx cx :cy cy
                          :innerRadius r0 :outerRadius r1 :startAngle a0 :endAngle a1
                          :padAngle (funcall num :padAngle 0)
                          :strokeWidth (funcall num :strokeWidth 1))
                    (eas-marks--style unit scales row)
                    (eas-marks--extras unit row))))))))

(defun eas-polar--place (unit scales bounds items)
  "ITEMS of a non-arc UNIT moved to their polar positions."
  (let* ((mark (plist-get unit :mark)) (rows (plist-get unit :rows))
         (cx (+ (aref bounds 0) (/ (aref bounds 2) 2.0))) (cy (+ (aref bounds 1) (/ (aref bounds 3) 2.0)))
         (num (lambda (k default) (let ((v (plist-get mark k))) (if (numberp v) v default)))))
    (vconcat
     (delq nil
           (mapcar (lambda (item)
                     (let* ((row (aref rows (plist-get item :datum)))
                            (a (or (eas-polar--value unit scales :theta row t) (funcall num :theta 0)))
                            (r (or (eas-polar--value unit scales :radius row) (funcall num :radius 0))))
                       (when (and (numberp a) (numberp r))
                         (let ((p (eas-arc-point cx cy (+ r (funcall num :radiusOffset 0))
                                                 (+ a (funcall num :thetaOffset 0)))))
                           (plist-put (plist-put (copy-sequence item) :x (car p)) :y (cdr p))))))
                   items)))))

(defun eas-polar-items (unit scales bounds metrics)
  "Scene items of polar UNIT drawn with SCALES inside BOUNDS."
  (if (equal (plist-get (plist-get unit :mark) :type) "arc")
      (eas-marks--each unit (eas-polar--arc-row unit scales bounds))
    (eas-polar--place unit scales bounds
                      (eas-marks--each unit (or (eas-marks-row-fn unit scales bounds metrics)
                                                (eas-signal "UNSUPPORTED_FEATURE"
                                                            (format "%s marks in polar coordinates"
                                                                    (plist-get (plist-get unit :mark) :type))
                                                            :feature "encoding/theta"))))))

(provide 'eas-polar)
;;; eas-polar.el ends here

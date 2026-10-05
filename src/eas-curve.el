;;; eas-curve.el --- curved interpolation for line and area marks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  mark.interpolate "monotone" is d3's curveMonotoneX
;; (Steffen's monotone cubic: no overshoot between data points).  The
;; curve's cubic Bezier segments are sampled into the item's polyline,
;; so renderers and hit-testing stay polyline-only; the data points
;; remain as the item's :anchors.

;;; Code:

(require 'eas-core)
(require 'eas-curve-extra)

(defconst eas-curve-samples 12
  "Polyline segments per curve segment.")

(defconst eas-curve-modes (append '("monotone" "linear-closed") eas-curve-extra-modes)
  "Interpolation modes `eas-curve-apply' draws.")

(defun eas-curve--sign (x) "d3's sign: -1 below zero, else 1." (if (< x 0) -1 1))

(defun eas-curve--slope3 (p0 p1 p2)
  "d3 monotoneX tangent at P1 between neighbours P0 and P2."
  (let* ((h0 (- (car p1) (car p0))) (h1 (- (car p2) (car p1)))
         (s0 (if (/= h0 0) (/ (- (cadr p1) (cadr p0)) (float h0)) 0.0))
         (s1 (if (/= h1 0) (/ (- (cadr p2) (cadr p1)) (float h1)) 0.0))
         (p (if (/= (+ h0 h1) 0) (/ (+ (* s0 h1) (* s1 h0)) (float (+ h0 h1))) 0.0)))
    (* (+ (eas-curve--sign s0) (eas-curve--sign s1))
       (min (abs s0) (abs s1) (* 0.5 (abs p))))))

(defun eas-curve--slope2 (p0 p1 tangent)
  "d3 monotoneX end tangent on P0..P1 given the neighbouring TANGENT."
  (let ((h (- (car p1) (car p0))))
    (if (/= h 0) (/ (- (/ (* 3 (- (cadr p1) (cadr p0))) (float h)) tangent) 2) tangent)))

(defun eas-curve-monotone (points)
  "POINTS ((X Y) ...) sampled along d3's curveMonotoneX."
  (let* ((pts (vconcat points)) (n (length pts)))
    (if (< n 3) points
      (let ((ts (make-vector n 0.0)))
        (cl-loop for i from 1 below (1- n)
                 do (aset ts i (eas-curve--slope3 (aref pts (1- i)) (aref pts i) (aref pts (1+ i)))))
        (aset ts 0 (eas-curve--slope2 (aref pts 0) (aref pts 1) (aref ts 1)))
        (aset ts (1- n) (eas-curve--slope2 (aref pts (- n 2)) (aref pts (1- n)) (aref ts (- n 2))))
        (cons (aref pts 0)
              (cl-loop for i from 0 below (1- n)
                       for a = (aref pts i) for b = (aref pts (1+ i))
                       for dx = (/ (- (car b) (car a)) 3.0)
                       for c1 = (list (+ (car a) dx) (+ (cadr a) (* dx (aref ts i))))
                       for c2 = (list (- (car b) dx) (- (cadr b) (* dx (aref ts (1+ i)))))
                       append (cl-loop for k from 1 to eas-curve-samples
                                       for u = (/ k (float eas-curve-samples))
                                       for v = (- 1 u)
                                       collect (list (+ (* v v v (car a)) (* 3 v v u (car c1)) (* 3 v u u (car c2)) (* u u u (car b)))
                                                     (+ (* v v v (cadr a)) (* 3 v v u (cadr c1)) (* 3 v u u (cadr c2))
                                                        (* u u u (cadr b)))))))))))

(defun eas-curve-apply (points mode)
  "POINTS ((X Y) ...) drawn with interpolation MODE (others pass through)."
  (pcase mode
    ("monotone" (eas-curve-monotone points))
    ;; d3's curveLinearClosed: a line closes on its first point (fc-qx1.43).
    ("linear-closed" (if (cddr points) (append points (list (car points))) points))
    (_ (eas-curve-extra-apply points mode))))

(provide 'eas-curve)
;;; eas-curve.el ends here

;;; eas-color.el --- HCL color interpolation, as d3 does it -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega-Lite interpolates continuous color scales in HCL
;; ("interpolate": "hcl").  This ports d3-color's sRGB <-> CIELAB (D50)
;; <-> HCL conversions and d3-interpolate's interpolateHcl and
;; piecewise, so a ramp's colors match Vega's to the rounded byte.

;;; Code:

(require 'eas-core)

(defconst eas-color--xn 0.96422)
(defconst eas-color--zn 0.82521)
(defconst eas-color--t0 (/ 4.0 29))
(defconst eas-color--t1 (/ 6.0 29))
(defconst eas-color--t2 (* 3 (/ 6.0 29) (/ 6.0 29)))
(defconst eas-color--t3 (expt (/ 6.0 29) 3))

(defun eas-color--hex-rgb (hex)
  "HEX \"#rrggbb\" as a list of three numbers 0-255."
  (list (string-to-number (substring hex 1 3) 16)
        (string-to-number (substring hex 3 5) 16)
        (string-to-number (substring hex 5 7) 16)))

(defun eas-color--rgb2lrgb (x)
  "Linearize sRGB channel X (0-255)."
  (let ((x (/ x 255.0))) (if (<= x 0.04045) (/ x 12.92) (expt (/ (+ x 0.055) 1.055) 2.4))))

(defun eas-color--lrgb2rgb (x)
  "sRGB channel (0-255) of linear X."
  (* 255 (if (<= x 0.0031308) (* 12.92 x) (- (* 1.055 (expt x (/ 1 2.4))) 0.055))))

(defun eas-color--xyz2lab (tt)
  "CIELAB f(TT)."
  (if (> tt eas-color--t3) (expt tt (/ 1.0 3)) (+ (/ tt eas-color--t2) eas-color--t0)))

(defun eas-color--lab2xyz (tt)
  "Inverse of `eas-color--xyz2lab'."
  (if (> tt eas-color--t1) (* tt tt tt) (* eas-color--t2 (- tt eas-color--t0))))

(defvar eas-color--hcl-cache (make-hash-table :test 'equal)
  "HEX -> its hcl, as `eas-color-hcl' computed it (fc-qx1.43).")

(defun eas-color-hcl (hex)
  "HEX as d3's hcl: (H C L), H a NaN for grays."
  (or (gethash hex eas-color--hcl-cache)
      (puthash hex (eas-color--hcl hex) eas-color--hcl-cache)))

(defun eas-color--hcl (hex)
  "HEX as d3's hcl, computed (see `eas-color-hcl')."
  (pcase-let* ((`(,r ,g ,b) (mapcar #'eas-color--rgb2lrgb (eas-color--hex-rgb hex)))
               (y (eas-color--xyz2lab (+ (* 0.2225045 r) (* 0.7168786 g) (* 0.0606169 b))))
               (x (if (and (= r g) (= g b)) y
                    (eas-color--xyz2lab (/ (+ (* 0.4360747 r) (* 0.3850649 g) (* 0.1430804 b)) eas-color--xn))))
               (z (if (and (= r g) (= g b)) y
                    (eas-color--xyz2lab (/ (+ (* 0.0139322 r) (* 0.0971045 g) (* 0.7141733 b)) eas-color--zn))))
               (l (- (* 116 y) 16)) (a (* 500 (- x y))) (bb (* 200 (- y z))))
    (if (and (zerop a) (zerop bb))
        (list 0.0e+NaN (if (< 0 l 100) 0.0 0.0e+NaN) l)
      (let ((h (radians-to-degrees (atan bb a))))
        (list (if (< h 0) (+ h 360) h) (sqrt (+ (* a a) (* bb bb))) l)))))

(defun eas-color-hcl-hex (h c l)
  "The \"#rrggbb\" of d3's hcl(H, C, L)."
  (let* ((gray (isnan (float h)))
         (a (if gray 0 (* (cos (degrees-to-radians h)) c)))
         (b (if gray 0 (* (sin (degrees-to-radians h)) c)))
         (y (/ (+ l 16) 116.0))
         (x (* eas-color--xn (eas-color--lab2xyz (+ y (/ a 500.0)))))
         (z (* eas-color--zn (eas-color--lab2xyz (- y (/ b 200.0)))))
         (y (eas-color--lab2xyz y)))
    (apply #'format "#%02x%02x%02x"
           (mapcar (lambda (v) (let ((v (if (isnan (float v)) 0 v))) (max 0 (min 255 (floor (+ v 0.5))))))
                   (list (eas-color--lrgb2rgb (+ (* 3.1338561 x) (* -1.6168667 y) (* -0.4906146 z)))
                         (eas-color--lrgb2rgb (+ (* -0.9787684 x) (* 1.9161415 y) (* 0.0334540 z)))
                         (eas-color--lrgb2rgb (+ (* 0.0719453 x) (* -0.2289914 y) (* 1.4052427 z))))))))

(defun eas-color--lerp (a b tt &optional hue)
  "d3's interpolation from A to B at TT; HUE takes the shorter way round."
  (let ((d (- b a)))
    (cond ((isnan (float a)) b)
          ((or (isnan (float d)) (zerop d)) a)
          ((and hue (or (> d 180) (< d -180))) (+ a (* tt (- d (* 360 (round (/ d 360.0)))))))
          (t (+ a (* tt d))))))

(defun eas-color-interpolate-hcl (from to tt)
  "d3.interpolateHcl(FROM, TO)(TT) for hex colors."
  (pcase-let ((`(,h0 ,c0 ,l0) (eas-color-hcl from)) (`(,h1 ,c1 ,l1) (eas-color-hcl to)))
    (eas-color-hcl-hex (eas-color--lerp h0 h1 tt t) (eas-color--lerp c0 c1 tt) (eas-color--lerp l0 l1 tt))))

(defun eas-color-piecewise-hcl (colors tt)
  "d3.piecewise(interpolateHcl, COLORS)(TT): COLORS a vector of hex strings."
  (let* ((n (1- (length colors)))
         (i (max 0 (min (1- n) (floor (* tt n))))))
    (eas-color-interpolate-hcl (aref colors i) (aref colors (1+ i)) (- (* tt n) i))))

(provide 'eas-color)
;;; eas-color.el ends here

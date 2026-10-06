;;; eas-text-ramp.el --- a continuous color legend's ramp, in cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, terminal half (fc-qx1.51).  A gradient legend's :bar is
;; the SVG target's linear gradient over the scheme's :stops.  In text
;; each cell of the bar shows two samples of it: a half block in the
;; color of one half, on the other half's color as its background, so
;; a five-row bar reads as ten steps of the ramp, high values on top.

;;; Code:

(require 'eas-core)
(require 'eas-color)
(require 'eas-color-names)

(defun eas-text-ramp-color (stops tt)
  "Return the color at TT (0..1) of a linear RGB gradient through STOPS.
It is interpolated as SVG interpolates its <stop>s.  Unparsable
stops are returned as they are."
  (let* ((n (length stops)) (pos (* (max 0.0 (min 1.0 tt)) (max 0 (1- n))))
         (i (min (max 0 (1- (1- n))) (floor pos)))
         (a (aref stops i)) (b (aref stops (min (1- n) (1+ i)))))
    (if (not (and (eas-color-hex a) (eas-color-hex b))) a
      (let ((f (- pos i)))
        (apply #'format "#%02x%02x%02x"
               (cl-mapcar (lambda (x y) (round (+ x (* f (- y x)))))
                          (eas-color--hex-rgb (eas-color-hex a)) (eas-color--hex-rgb (eas-color-hex b))))))))

(defun eas-text-ramp-cells (bar stops cw ch &optional horizontal)
  "Return the cells of gradient BAR [X Y W H] over STOPS.
The grid has CW x CH pixel cells; each is (COL ROW CHAR FOREGROUND
BACKGROUND).  Vertical bars run
low to high upwards (▄ on the upper half's color), HORIZONTAL ones
left to right (▐ on the left half's color)."
  (let* ((x (aref bar 0)) (y (aref bar 1)) (w (max 1e-9 (aref bar 2))) (h (max 1e-9 (aref bar 3)))
         (at (lambda (tt) (eas-text-ramp-color stops tt))) out)
    (cl-loop for row from (floor y ch) to (floor (- (+ y h) 0.01) ch)
             do (cl-loop for col from (floor x cw) to (floor (- (+ x w) 0.01) cw)
                         do (push (if horizontal
                                      (list col row ?▐
                                            (funcall at (/ (- (* (+ col 0.75) cw) x) w))
                                            (funcall at (/ (- (* (+ col 0.25) cw) x) w)))
                                    (list col row ?▄
                                          (funcall at (- 1 (/ (- (* (+ row 0.75) ch) y) h)))
                                          (funcall at (- 1 (/ (- (* (+ row 0.25) ch) y) h)))))
                                  out)))
    (nreverse out)))

(provide 'eas-text-ramp)
;;; eas-text-ramp.el ends here

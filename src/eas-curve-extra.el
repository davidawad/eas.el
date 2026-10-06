;;; eas-curve-extra.el --- d3's other curves for line and area marks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, beside eas-curve.el's monotone.  mark.interpolate
;;
;;   basis basis-open bundle cardinal cardinal-open catmull-rom natural
;;
;; are ports of d3-shape's curve factories, as Vega draws them: each
;; walks the points exactly as d3's state machine does and emits moves,
;; lines and cubic Beziers, which are sampled into the item's polyline
;; (`eas-curve-samples' segments per Bezier) so renderers and hit tests
;; stay polyline-only.  mark.tension is Vega's parameter for the curve
;; that has one: cardinal's tension (default 0), bundle's beta (0.85)
;; and catmull-rom's alpha (0.5); `eas-curve-tension' carries it.  The
;; closed variants (and linear-closed) close a line on itself, which an
;; area cannot do; they stay unsupported.

;;; Code:

(require 'eas-core)

(defvar eas-curve-samples)

(defvar eas-curve-tension nil
  "The mark.tension of the series being drawn, or nil for the curve's default.")

(defconst eas-curve-extra-modes
  '("basis" "basis-open" "bundle" "cardinal" "cardinal-open" "catmull-rom" "natural")
  "Interpolation modes `eas-curve-extra-apply' draws.")

;;; A path that samples its Beziers

(defun eas-curve-extra--path ()
  "A fresh path accumulator: (POINTS-REVERSED . CURRENT-POINT)."
  (cons nil nil))

(defun eas-curve-extra--to (path x y)
  "Move or draw PATH to X, Y."
  (push (list x y) (car path))
  (setcdr path (list x y)))

(defun eas-curve-extra--bezier (path x1 y1 x2 y2 x y)
  "Draw a cubic Bezier on PATH through controls X1 Y1, X2 Y2 to X, Y."
  (let* ((p (cdr path)) (x0 (car p)) (y0 (cadr p)))
    (dotimes (k eas-curve-samples)
      (let* ((u (/ (1+ k) (float eas-curve-samples))) (v (- 1 u)))
        (push (list (+ (* v v v x0) (* 3 v v u x1) (* 3 v u u x2) (* u u u x))
                    (+ (* v v v y0) (* 3 v v u y1) (* 3 v u u y2) (* u u u y)))
              (car path))))
    (setcdr path (list x y))))

(defun eas-curve-extra--points (path)
  "PATH's points, first to last."
  (reverse (car path)))

;;; basis and bundle

(defun eas-curve-extra--basis-segment (path x0 y0 x1 y1 x y)
  "Extend PATH with d3's basis Bezier from points X0 Y0, X1 Y1 towards X, Y."
  (eas-curve-extra--bezier path (/ (+ (* 2 x0) x1) 3.0) (/ (+ (* 2 y0) y1) 3.0)
                           (/ (+ x0 (* 2 x1)) 3.0) (/ (+ y0 (* 2 y1)) 3.0)
                           (/ (+ x0 (* 4 x1) x) 6.0) (/ (+ y0 (* 4 y1) y) 6.0)))

(defun eas-curve-extra-basis (points &optional open)
  "POINTS ((X Y) ...) along d3's curveBasis (curveBasisOpen when OPEN)."
  (let ((path (eas-curve-extra--path)) (state 0) x0 y0 x1 y1)
    (dolist (p points)
      (let ((x (float (car p))) (y (float (cadr p))))
        (if open
            (pcase state
              (0 (setq state 1))
              (1 (setq state 2))
              (2 (setq state 3)
                 (eas-curve-extra--to path (/ (+ x0 (* 4 x1) x) 6.0) (/ (+ y0 (* 4 y1) y) 6.0)))
              (_ (setq state 4) (eas-curve-extra--basis-segment path x0 y0 x1 y1 x y)))
          (pcase state
            (0 (setq state 1) (eas-curve-extra--to path x y))
            (1 (setq state 2))
            (2 (setq state 3)
               (eas-curve-extra--to path (/ (+ (* 5 x0) x1) 6.0) (/ (+ (* 5 y0) y1) 6.0))
               (eas-curve-extra--basis-segment path x0 y0 x1 y1 x y))
            (_ (eas-curve-extra--basis-segment path x0 y0 x1 y1 x y))))
        (setq x0 x1 y0 y1 x1 x y1 y)))
    (unless open
      (when (= state 3) (eas-curve-extra--basis-segment path x0 y0 x1 y1 x1 y1))
      (when (memq state '(2 3)) (eas-curve-extra--to path x1 y1)))
    (eas-curve-extra--points path)))

(defun eas-curve-extra-bundle (points beta)
  "POINTS along d3's curveBundle with BETA: basis pulled towards the chord."
  (let* ((pts (vconcat points)) (j (1- (length pts))))
    (if (<= j 0) points
      (let* ((x0 (float (car (aref pts 0)))) (y0 (float (cadr (aref pts 0))))
             (dx (- (car (aref pts j)) x0)) (dy (- (cadr (aref pts j)) y0)))
        (eas-curve-extra-basis
         (cl-loop for i from 0 to j
                  for p = (aref pts i) for u = (/ i (float j))
                  collect (list (+ (* beta (car p)) (* (- 1 beta) (+ x0 (* u dx))))
                                (+ (* beta (cadr p)) (* (- 1 beta) (+ y0 (* u dy)))))))))))

;;; cardinal and catmull-rom

(defun eas-curve-extra-cardinal (points tension &optional open)
  "POINTS along d3's curveCardinal with TENSION (curveCardinalOpen when OPEN)."
  (let ((k (/ (- 1 tension) 6.0)) (path (eas-curve-extra--path)) (state 0)
        x0 y0 x1 y1 x2 y2)
    (cl-flet ((seg (x y) (eas-curve-extra--bezier path (+ x1 (* k (- x2 x0))) (+ y1 (* k (- y2 y0)))
                                                  (+ x2 (* k (- x1 x))) (+ y2 (* k (- y1 y))) x2 y2)))
      (dolist (p points)
        (let ((x (float (car p))) (y (float (cadr p))))
          (if open
              (pcase state
                (0 (setq state 1))
                (1 (setq state 2))
                (2 (setq state 3) (eas-curve-extra--to path x2 y2))
                (_ (setq state 4) (seg x y)))
            (pcase state
              (0 (setq state 1) (eas-curve-extra--to path x y))
              (1 (setq state 2 x1 x y1 y))
              (_ (setq state 3) (seg x y))))
          (setq x0 x1 x1 x2 x2 x y0 y1 y1 y2 y2 y)))
      (unless open
        (pcase state
          (2 (eas-curve-extra--to path x2 y2))
          (3 (seg x1 y1)))))
    (eas-curve-extra--points path)))

(defun eas-curve-extra-catmull-rom (points alpha)
  "POINTS along d3's curveCatmullRom with ALPHA (alpha 0 is cardinal 0)."
  (if (zerop alpha) (eas-curve-extra-cardinal points 0)
    (let ((path (eas-curve-extra--path)) (state 0) (eps 1e-12)
          x0 y0 x1 y1 x2 y2 (l01a 0) (l12a 0) (l23a 0) (l01 0) (l12 0) (l23 0))
      (cl-labels
          ((seg (x y)
             (let ((cx1 x1) (cy1 y1) (cx2 x2) (cy2 y2))
               (when (> l01a eps)
                 (let ((a (+ (* 2 l01) (* 3 l01a l12a) l12)) (n (* 3 l01a (+ l01a l12a))))
                   (setq cx1 (/ (+ (- (* x1 a) (* x0 l12)) (* x2 l01)) n)
                         cy1 (/ (+ (- (* y1 a) (* y0 l12)) (* y2 l01)) n))))
               (when (> l23a eps)
                 (let ((b (+ (* 2 l23) (* 3 l23a l12a) l12)) (m (* 3 l23a (+ l23a l12a))))
                   (setq cx2 (/ (- (+ (* x2 b) (* x1 l23)) (* x l12)) m)
                         cy2 (/ (- (+ (* y2 b) (* y1 l23)) (* y l12)) m))))
               (eas-curve-extra--bezier path cx1 cy1 cx2 cy2 x2 y2)))
           (pt (x y)
             (unless (zerop state)
               (let ((dx (- x2 x)) (dy (- y2 y)))
                 (setq l23 (expt (+ (* dx dx) (* dy dy)) alpha) l23a (sqrt l23))))
             (pcase state
               (0 (setq state 1) (eas-curve-extra--to path x y))
               (1 (setq state 2))
               (_ (setq state 3) (seg x y)))
             (setq l01a l12a l12a l23a l01 l12 l12 l23
                   x0 x1 x1 x2 x2 x y0 y1 y1 y2 y2 y)))
        (dolist (p points) (pt (float (car p)) (float (cadr p))))
        (pcase state
          (2 (eas-curve-extra--to path x2 y2))
          (3 (pt x2 y2))))
      (eas-curve-extra--points path))))

;;; natural

(defun eas-curve-extra--natural-controls (xs)
  "Return d3 curveNatural's control points for coordinates XS.
They come as (A . B), two vectors."
  (let* ((n (1- (length xs))) (a (make-vector n 0.0)) (b (make-vector n 0.0)) (r (make-vector n 0.0)))
    (aset b 0 2.0) (aset r 0 (+ (aref xs 0) (* 2 (aref xs 1))))
    (cl-loop for i from 1 below (1- n)
             do (aset a i 1.0) (aset b i 4.0) (aset r i (+ (* 4 (aref xs i)) (* 2 (aref xs (1+ i))))))
    (aset a (1- n) 2.0) (aset b (1- n) 7.0) (aset r (1- n) (+ (* 8 (aref xs (1- n))) (aref xs n)))
    (cl-loop for i from 1 below n
             for m = (/ (aref a i) (aref b (1- i)))
             do (aset b i (- (aref b i) m)) (aset r i (- (aref r i) (* m (aref r (1- i))))))
    (aset a (1- n) (/ (aref r (1- n)) (aref b (1- n))))
    (cl-loop for i from (- n 2) downto 0
             do (aset a i (/ (- (aref r i) (aref a (1+ i))) (aref b i))))
    (aset b (1- n) (/ (+ (aref xs n) (aref a (1- n))) 2.0))
    (cl-loop for i from 0 below (1- n)
             do (aset b i (- (* 2 (aref xs (1+ i))) (aref a (1+ i)))))
    (cons a b)))

(defun eas-curve-extra-natural (points)
  "POINTS along d3's curveNatural (a natural cubic spline)."
  (let* ((pts (vconcat points)) (n (length pts)))
    (if (< n 3) points
      (let* ((xs (vconcat (mapcar (lambda (p) (float (car p))) pts)))
             (ys (vconcat (mapcar (lambda (p) (float (cadr p))) pts)))
             (px (eas-curve-extra--natural-controls xs)) (py (eas-curve-extra--natural-controls ys))
             (path (eas-curve-extra--path)))
        (eas-curve-extra--to path (aref xs 0) (aref ys 0))
        (dotimes (i (1- n))
          (eas-curve-extra--bezier path (aref (car px) i) (aref (car py) i)
                                   (aref (cdr px) i) (aref (cdr py) i)
                                   (aref xs (1+ i)) (aref ys (1+ i))))
        (eas-curve-extra--points path)))))

;;; Dispatch

(defun eas-curve-extra-apply (points mode)
  "Draw POINTS ((X Y) ...) with interpolation MODE under `eas-curve-tension'.
MODE is one of `eas-curve-extra-modes'."
  (let ((tension (and (numberp eas-curve-tension) eas-curve-tension)))
    (pcase mode
      ("basis" (eas-curve-extra-basis points))
      ("basis-open" (eas-curve-extra-basis points t))
      ("bundle" (eas-curve-extra-bundle points (or tension 0.85)))
      ("cardinal" (eas-curve-extra-cardinal points (or tension 0)))
      ("cardinal-open" (eas-curve-extra-cardinal points (or tension 0) t))
      ("catmull-rom" (eas-curve-extra-catmull-rom points (or tension 0.5)))
      ("natural" (eas-curve-extra-natural points))
      (_ points))))

(provide 'eas-curve-extra)
;;; eas-curve-extra.el ends here

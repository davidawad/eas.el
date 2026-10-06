;;; eas-arc.el --- arc (wedge) geometry shared by the renderers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5.  An arc item is a wedge of an annulus: centre :cx :cy,
;; radii :innerRadius :outerRadius and angles :startAngle :endAngle in
;; radians, measured clockwise from 12 o'clock as in Vega (d3.arc),
;; with :padAngle trimmed off its sides.  Its :x :y is the centroid,
;; which hit-testing and tooltips use.  This file answers the geometric
;; questions renderers ask: the SVG path, a polygon for image :map hot
;; spots, and which text cells the wedge covers.

;;; Code:

(require 'eas-core)
(require 'eas-arc-d3)

(defconst eas-arc--tau (* 2 float-pi))

(defun eas-arc-point (cx cy r a)
  "The point at radius R and angle A (clockwise from 12 o'clock) about CX CY."
  (cons (+ cx (* r (sin a))) (- cy (* r (cos a)))))

(defun eas-arc-angles (item)
  "ITEM's (START . END) angles after its padAngle."
  (let* ((a0 (plist-get item :startAngle)) (a1 (plist-get item :endAngle))
         (pad (min (or (plist-get item :padAngle) 0) (abs (- a1 a0)))))
    (if (> pad 0) (cons (+ a0 (/ pad 2.0)) (- a1 (/ pad 2.0))) (cons a0 a1))))

(defun eas-arc--n (v)
  "V rounded to two decimals for path data."
  (let ((s (format "%.2f" (if (< (abs v) 0.005) 0.0 v))))
    (string-remove-suffix "." (replace-regexp-in-string "\\.?0+\\'" "" s))))

(defun eas-arc--xy (p)
  "Path coordinates of point P."
  (concat (eas-arc--n (car p)) "," (eas-arc--n (cdr p))))

(defun eas-arc-path (item)
  "SVG path data of arc ITEM, as d3.arc draws it.
Padded or rounded arcs take d3's own geometry (eas-arc-d3.el)."
  (if (eas-arc-d3-wanted-p item) (eas-arc-d3-path item) (eas-arc--wedge-path item)))

(defun eas-arc--wedge-path (item)
  "SVG path data of arc ITEM without padding or corners."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (ri (max 0 (or (plist-get item :innerRadius) 0))) (ro (max 0 (or (plist-get item :outerRadius) 0)))
         (r0 (min ri ro)) (r1 (max ri ro))
         (angles (eas-arc-angles item)) (a0 (car angles)) (a1 (cdr angles))
         (da (abs (- a1 a0))) (cw (if (>= a1 a0) 1 0))
         (pt (lambda (r a) (eas-arc--xy (eas-arc-point cx cy r a))))
         (arc (lambda (r large sweep to) (format "A%s,%s 0 %d %d %s" (eas-arc--n r) (eas-arc--n r) large sweep to))))
    (cond
     ((<= r1 0) "")
     ((>= da (- eas-arc--tau 1e-6))
      (concat "M" (funcall pt r1 a0) (funcall arc r1 1 1 (funcall pt r1 (+ a0 float-pi)))
              (funcall arc r1 1 1 (funcall pt r1 a0)) "Z"
              (when (> r0 0)
                (concat "M" (funcall pt r0 a0) (funcall arc r0 1 0 (funcall pt r0 (+ a0 float-pi)))
                        (funcall arc r0 1 0 (funcall pt r0 a0)) "Z"))))
     (t (let ((large (if (> da float-pi) 1 0)))
          (concat "M" (funcall pt r1 a0) (funcall arc r1 large cw (funcall pt r1 a1))
                  (if (> r0 0)
                      (concat "L" (funcall pt r0 a1) (funcall arc r0 large (- 1 cw) (funcall pt r0 a0)))
                    (concat "L" (eas-arc--xy (cons cx cy))))
                  "Z"))))))

(defun eas-arc-polygon (item &optional steps)
  "Return a vector [X0 Y0 X1 Y1 ...] of integers outlining arc ITEM.
STEPS is the number of segments per side."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (r0 (or (plist-get item :innerRadius) 0)) (r1 (or (plist-get item :outerRadius) 0))
         (angles (eas-arc-angles item)) (a0 (car angles)) (a1 (cdr angles))
         (n (or steps (max 2 (ceiling (/ (abs (- a1 a0)) 0.2)))))
         (side (lambda (r from to) (cl-loop for k from 0 to n
                                            collect (eas-arc-point cx cy r (+ from (* (- to from) (/ k (float n)))))))))
    (vconcat (mapcan (lambda (p) (list (round (car p)) (round (cdr p))))
                     (append (funcall side r1 a0 a1)
                             (if (> r0 0) (funcall side r0 a1 a0) (list (cons cx cy))))))))

(defun eas-arc-bounds (item)
  "Return the bounding box [X0 Y0 X1 Y1] of arc ITEM.
It spans the arc's ends, centre or inner ends, and every quarter-turn
extreme its angles sweep."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (r0 (or (plist-get item :innerRadius) 0)) (r1 (or (plist-get item :outerRadius) 0))
         (angles (eas-arc-angles item))
         (lo (min (car angles) (cdr angles))) (hi (max (car angles) (cdr angles)))
         (pts (append (list (eas-arc-point cx cy r1 lo) (eas-arc-point cx cy r1 hi)
                            (eas-arc-point cx cy r0 lo) (eas-arc-point cx cy r0 hi))
                      (cl-loop for k from (ceiling (/ lo (/ float-pi 2))) to (floor (/ hi (/ float-pi 2)))
                               collect (eas-arc-point cx cy r1 (* k (/ float-pi 2)))))))
    (vector (apply #'min (mapcar #'car pts)) (apply #'min (mapcar #'cdr pts))
            (apply #'max (mapcar #'car pts)) (apply #'max (mapcar #'cdr pts)))))

(defun eas-arc-contains-p (item x y)
  "Non-nil when point X Y lies inside arc ITEM."
  (let* ((dx (- x (plist-get item :cx))) (dy (- y (plist-get item :cy)))
         (r (sqrt (+ (* dx dx) (* dy dy))))
         (angles (eas-arc-angles item))
         (lo (min (car angles) (cdr angles))) (hi (max (car angles) (cdr angles)))
         ;; Angle of the point, clockwise from 12 o'clock, brought into [lo, lo + tau).
         (a (atan dx (- dy)))
         (a (+ lo (mod (- a lo) eas-arc--tau))))
    (and (<= (or (plist-get item :innerRadius) 0) r (or (plist-get item :outerRadius) 0))
         (<= a hi))))

(defun eas-arc-cells (item cw ch clip fn)
  "Call FN with (COL ROW) for each CW x CH text cell centred inside arc ITEM.
Only cells within CLIP [COL0 ROW0 COL1 ROW1) are visited."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy)) (r (or (plist-get item :outerRadius) 0)))
    (cl-loop for row from (max (aref clip 1) (floor (- cy r) ch)) below (min (aref clip 3) (ceiling (+ cy r) ch))
             do (cl-loop for col from (max (aref clip 0) (floor (- cx r) cw)) below (min (aref clip 2) (ceiling (+ cx r) cw))
                         when (eas-arc-contains-p item (* (+ col 0.5) cw) (* (+ row 0.5) ch))
                         do (funcall fn col row)))))

(provide 'eas-arc)
;;; eas-arc.el ends here

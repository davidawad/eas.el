;;; eas-arc-d3.el --- arcs with padding and rounded corners, as d3.arc draws them -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, beside eas-arc.el.  `eas-arc-d3-path' is a port of
;; d3-shape's arc generator (the one Vega's arc mark uses) for the
;; parts eas-arc.el's simple wedge leaves out: mark.padAngle padded at
;; d3's pad radius sqrt(innerRadius^2 + outerRadius^2), so the gap
;; between wedges has parallel sides, and mark.cornerRadius, rounding
;; the wedge's corners with the radius d3 restricts to what the wedge
;; can hold.  Path commands follow d3-path's arc(): a line to each
;; arc's start when the pen is elsewhere, then "A" segments.

;;; Code:

(require 'eas-core)

(defconst eas-arc-d3--eps 1e-12 "d3-shape's epsilon.")

(defconst eas-arc-d3--path-eps 1e-6 "d3-path's epsilon.")

(defun eas-arc-d3-wanted-p (item)
  "Non-nil when arc ITEM needs d3's padding or corners."
  (or (let ((rc (plist-get item :cornerRadius))) (and (numberp rc) (> rc 0)))
      (let ((pad (plist-get item :padAngle))) (and (numberp pad) (> pad 0)))))

(defun eas-arc-d3--intersect (x0 y0 x1 y1 x2 y2 x3 y3)
  "Intersection (X . Y) of lines X0Y0-X1Y1 and X2Y2-X3Y3, or nil."
  (let* ((x10 (- x1 x0)) (y10 (- y1 y0)) (x32 (- x3 x2)) (y32 (- y3 y2))
         (tt (- (* y32 x10) (* x32 y10))))
    (unless (< (* tt tt) eas-arc-d3--eps)
      (let ((tt (/ (- (* x32 (- y0 y2)) (* y32 (- x0 x2))) tt)))
        (cons (+ x0 (* tt x10)) (+ y0 (* tt y10)))))))

(defun eas-arc-d3--corner (x0 y0 x1 y1 r1 rc cw)
  "d3's cornerTangents: (CX CY X01 Y01 X11 Y11) of a corner of radius RC."
  (let* ((x01 (- x0 x1)) (y01 (- y0 y1))
         (lo (/ (if cw rc (- rc)) (sqrt (+ (* x01 x01) (* y01 y01)))))
         (ox (* lo y01)) (oy (* (- lo) x01))
         (x11 (+ x0 ox)) (y11 (+ y0 oy)) (x10 (+ x1 ox)) (y10 (+ y1 oy))
         (x00 (/ (+ x11 x10) 2)) (y00 (/ (+ y11 y10) 2))
         (dx (- x10 x11)) (dy (- y10 y11)) (d2 (+ (* dx dx) (* dy dy)))
         (r (- r1 rc)) (dd (- (* x11 y10) (* x10 y11)))
         (d (* (if (< dy 0) -1 1) (sqrt (max 0 (- (* r r d2) (* dd dd))))))
         (cx0 (/ (- (* dd dy) (* dx d)) d2)) (cy0 (/ (- (* (- dd) dx) (* dy d)) d2))
         (cx1 (/ (+ (* dd dy) (* dx d)) d2)) (cy1 (/ (+ (* (- dd) dx) (* dy d)) d2)))
    (when (> (+ (expt (- cx0 x00) 2) (expt (- cy0 y00) 2)) (+ (expt (- cx1 x00) 2) (expt (- cy1 y00) 2)))
      (setq cx0 cx1 cy0 cy1))
    (list cx0 cy0 (- ox) (- oy) (* cx0 (- (/ r1 r) 1)) (* cy0 (- (/ r1 r) 1)))))

(defun eas-arc-d3-path (item)
  "SVG path data of arc ITEM as d3.arc draws it, padding and corners included."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (ri (max 0 (or (plist-get item :innerRadius) 0))) (ro (max 0 (or (plist-get item :outerRadius) 0)))
         (r0 (float (min ri ro))) (r1 (float (max ri ro)))
         (hp (/ float-pi 2)) (tau (* 2 float-pi)) (eps eas-arc-d3--eps)
         (a0 (- (plist-get item :startAngle) hp)) (a1 (- (plist-get item :endAngle) hp))
         (da (abs (- a1 a0))) (cw (> a1 a0))
         (out nil) (pen nil)
         (n (lambda (v) (let ((s (format "%.2f" (if (< (abs v) 0.005) 0.0 v))))
                          (cond ((string-suffix-p ".00" s) (substring s 0 -3))
                                ((string-suffix-p "0" s) (substring s 0 -1))
                                (t s))))))
    (cl-labels
        ((emit (fmt &rest args) (push (apply #'format fmt args) out))
         (xy (x y) (concat (funcall n (+ cx x)) "," (funcall n (+ cy y))))
         (move (x y) (emit "M%s" (xy x y)) (setq pen (cons x y)))
         (line (x y) (emit "L%s" (xy x y)) (setq pen (cons x y)))
         (arc (x y r b0 b1 ccw)
           ;; d3-path's arc(): join the pen to the start, then one or two "A"s.
           (let* ((x0 (+ x (* r (cos b0)))) (y0 (+ y (* r (sin b0))))
                  (d (if ccw (- b0 b1) (- b1 b0))))
             (cond ((null pen) (move x0 y0))
                   ((or (> (abs (- (car pen) x0)) eas-arc-d3--path-eps)
                        (> (abs (- (cdr pen) y0)) eas-arc-d3--path-eps))
                    (line x0 y0)))
             (when (> r 0)
               ;; JavaScript's %, which truncates.
               (when (< d 0) (setq d (+ (- d (* tau (ftruncate (/ d tau)))) tau)))
               (cond
                ((> d (- tau eas-arc-d3--path-eps))
                 (emit "A%s,%s 0 1 %d %s" (funcall n r) (funcall n r) (if ccw 0 1) (xy (- x (- x0 x)) (- y (- y0 y))))
                 (emit "A%s,%s 0 1 %d %s" (funcall n r) (funcall n r) (if ccw 0 1) (xy x0 y0))
                 (setq pen (cons x0 y0)))
                ((> d eas-arc-d3--path-eps)
                 (let ((ex (+ x (* r (cos b1)))) (ey (+ y (* r (sin b1)))))
                   (emit "A%s,%s 0 %d %d %s" (funcall n r) (funcall n r) (if (>= d float-pi) 1 0) (if ccw 0 1) (xy ex ey))
                   (setq pen (cons ex ey)))))))))
      (cond
       ((not (> r1 eps)) (move 0 0))
       ((> da (- tau eps))
        (move (* r1 (cos a0)) (* r1 (sin a0)))
        (arc 0 0 r1 a0 a1 (not cw))
        (when (> r0 eps)
          (move (* r0 (cos a1)) (* r0 (sin a1)))
          (arc 0 0 r0 a1 a0 cw)))
       (t
        (let* ((a01 a0) (a11 a1) (a00 a0) (a10 a1) (da0 da) (da1 da)
               (ap (/ (or (plist-get item :padAngle) 0) 2.0))
               (rp (and (> ap eps) (sqrt (+ (* r0 r0) (* r1 r1)))))
               (rc (min (/ (abs (- r1 r0)) 2) (or (plist-get item :cornerRadius) 0)))
               (rc0 rc) (rc1 rc))
          (when (and rp (> rp eps))
            ;; asin beyond 1 is NaN in d3, which collapses that ring.
            (let* ((pad (lambda (r) (let ((v (and (> r 0) (* (/ rp r) (sin ap))))) (and v (<= v 1) (asin v)))))
                   (p0 (funcall pad r0)) (p1 (funcall pad r1)))
              (if (and p0 (> (setq da0 (- da0 (* 2 p0))) eps))
                  (let ((p0 (if cw p0 (- p0)))) (setq a00 (+ a00 p0) a10 (- a10 p0)))
                (setq da0 0 a00 (/ (+ a0 a1) 2) a10 a00))
              (if (and p1 (> (setq da1 (- da1 (* 2 p1))) eps))
                  (let ((p1 (if cw p1 (- p1)))) (setq a01 (+ a01 p1) a11 (- a11 p1)))
                (setq da1 0 a01 (/ (+ a0 a1) 2) a11 a01))))
          (let ((x01 (* r1 (cos a01))) (y01 (* r1 (sin a01)))
                (x10 (* r0 (cos a10))) (y10 (* r0 (sin a10)))
                (x11 (* r1 (cos a11))) (y11 (* r1 (sin a11)))
                (x00 (* r0 (cos a00))) (y00 (* r0 (sin a00))))
            (when (and (> rc eps) (< da float-pi))
              (if-let* ((oc (eas-arc-d3--intersect x01 y01 x00 y00 x11 y11 x10 y10)))
                  (let* ((ax (- x01 (car oc))) (ay (- y01 (cdr oc))) (bx (- x11 (car oc))) (by (- y11 (cdr oc)))
                         (kc (/ 1 (sin (/ (acos (max -1.0 (min 1.0 (/ (+ (* ax bx) (* ay by))
                                                                       (* (sqrt (+ (* ax ax) (* ay ay)))
                                                                          (sqrt (+ (* bx bx) (* by by))))))))
                                          2))))
                         (lc (sqrt (+ (* (car oc) (car oc)) (* (cdr oc) (cdr oc))))))
                    (setq rc0 (min rc (/ (- r0 lc) (- kc 1))) rc1 (min rc (/ (- r1 lc) (+ kc 1)))))
                (setq rc0 0 rc1 0)))
            ;; The outer ring.
            (cond
             ((not (> da1 eps)) (move x01 y01))
             ((> rc1 eps)
              (pcase-let ((`(,c0x ,c0y ,t0x01 ,t0y01 ,t0x11 ,t0y11) (eas-arc-d3--corner x00 y00 x01 y01 r1 rc1 cw))
                          (`(,c1x ,c1y ,t1x01 ,t1y01 ,t1x11 ,t1y11) (eas-arc-d3--corner x11 y11 x10 y10 r1 rc1 cw)))
                (move (+ c0x t0x01) (+ c0y t0y01))
                (if (< rc1 rc) (arc c0x c0y rc1 (atan t0y01 t0x01) (atan t1y01 t1x01) (not cw))
                  (arc c0x c0y rc1 (atan t0y01 t0x01) (atan t0y11 t0x11) (not cw))
                  (arc 0 0 r1 (atan (+ c0y t0y11) (+ c0x t0x11)) (atan (+ c1y t1y11) (+ c1x t1x11)) (not cw))
                  (arc c1x c1y rc1 (atan t1y11 t1x11) (atan t1y01 t1x01) (not cw)))))
             (t (move x01 y01) (arc 0 0 r1 a01 a11 (not cw))))
            ;; The inner ring, or the centre.
            (cond
             ((or (not (> r0 eps)) (not (> da0 eps))) (line x10 y10))
             ((> rc0 eps)
              (pcase-let ((`(,c0x ,c0y ,t0x01 ,t0y01 ,t0x11 ,t0y11) (eas-arc-d3--corner x10 y10 x11 y11 r0 (- rc0) cw))
                          (`(,c1x ,c1y ,t1x01 ,t1y01 ,t1x11 ,t1y11) (eas-arc-d3--corner x01 y01 x00 y00 r0 (- rc0) cw)))
                (line (+ c0x t0x01) (+ c0y t0y01))
                (if (< rc0 rc) (arc c0x c0y rc0 (atan t0y01 t0x01) (atan t1y01 t1x01) (not cw))
                  (arc c0x c0y rc0 (atan t0y01 t0x01) (atan t0y11 t0x11) (not cw))
                  (arc 0 0 r0 (atan (+ c0y t0y11) (+ c0x t0x11)) (atan (+ c1y t1y11) (+ c1x t1x11)) cw)
                  (arc c1x c1y rc0 (atan t1y11 t1x11) (atan t1y01 t1x01) (not cw)))))
             (t (arc 0 0 r0 a10 a00 cw))))))))
    (concat (apply #'concat (nreverse out)) "Z")))

(provide 'eas-arc-d3)
;;; eas-arc-d3.el ends here

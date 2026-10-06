;;; eas-geo-polyhedral.el --- the polyhedral butterfly projection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (eas-geo-raw.el registers it).  A port of
;; d3-geo-projection's polyhedral projections as the Vega gallery uses
;; them: the octahedron's eight faces, each a gnomonic projection about
;; its centroid, unfolded along a spanning tree of shared edges into
;; Cahill's butterfly (polyhedral/index.js, butterfly.js, matrix.js).
;; The sphere's outline walks the tree's free edges, each vertex nudged
;; towards its face's centre.

;;; Code:

(require 'cl-lib)
(require 'eas-geo-stream)

(declare-function eas-geo-proj--simple "eas-geo-proj")
(declare-function eas-geo-raw-gnomonic "eas-geo-raw")

(defconst eas-geo-polyhedral--octahedron
  (let ((v [[0 90] [-90 0] [0 0] [90 0] [180 0] [0 -90]]))
    (mapcar (lambda (f) (mapcar (lambda (i) (aref v i)) f))
            '((0 2 1) (0 3 2) (5 1 2) (5 2 3) (0 1 4) (0 4 3) (5 4 1) (5 3 4))))
  "The octahedron's faces, clockwise lists of [LON LAT] vertices.")

(defun eas-geo-polyhedral--centroid (points)
  "D3's geoCentroid of the MultiPoint POINTS ([LON LAT] degrees)."
  (let ((x 0.0) (y 0.0) (z 0.0))
    (dolist (p points)
      (let ((c (eas-geo-cartesian (* (aref p 0) eas-geo-rad) (* (aref p 1) eas-geo-rad))))
        (setq x (+ x (aref c 0)) y (+ y (aref c 1)) z (+ z (aref c 2)))))
    (vector (/ (atan y x) eas-geo-rad) (/ (asin (/ z (sqrt (+ (* x x) (* y y) (* z z))))) eas-geo-rad))))

(defun eas-geo-polyhedral--interpolate (a b tt)
  "D3's geoInterpolate(A, B)(TT), points in degrees."
  (let* ((x0 (* (aref a 0) eas-geo-rad)) (y0 (* (aref a 1) eas-geo-rad))
         (x1 (* (aref b 0) eas-geo-rad)) (y1 (* (aref b 1) eas-geo-rad))
         (cy0 (cos y0)) (sy0 (sin y0)) (cy1 (cos y1)) (sy1 (sin y1))
         (kx0 (* cy0 (cos x0))) (ky0 (* cy0 (sin x0))) (kx1 (* cy1 (cos x1))) (ky1 (* cy1 (sin x1)))
         (hav (lambda (v) (let ((s (sin (/ v 2)))) (* s s))))
         (d (* 2 (eas-geo-asin (sqrt (+ (funcall hav (- y1 y0)) (* cy0 cy1 (funcall hav (- x1 x0))))))))
         (k (sin d)))
    (if (= d 0) (vector (aref a 0) (aref a 1))
      (let* ((td (* tt d)) (B (/ (sin td) k)) (A (/ (sin (- d td)) k))
             (x (+ (* A kx0) (* B kx1))) (y (+ (* A ky0) (* B ky1))) (z (+ (* A sy0) (* B sy1))))
        (vector (/ (atan y x) eas-geo-rad) (/ (atan z (sqrt (+ (* x x) (* y y)))) eas-geo-rad))))))

;;; 2D affine matrices [a b c d e f]

(defun eas-geo-polyhedral--multiply (a b)
  "The product of affine matrices A and B."
  (vector (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 3)))
          (+ (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 4)))
          (+ (* (aref a 0) (aref b 2)) (* (aref a 1) (aref b 5)) (aref a 2))
          (+ (* (aref a 3) (aref b 0)) (* (aref a 4) (aref b 3)))
          (+ (* (aref a 3) (aref b 1)) (* (aref a 4) (aref b 4)))
          (+ (* (aref a 3) (aref b 2)) (* (aref a 4) (aref b 5)) (aref a 5))))

(defun eas-geo-polyhedral--matrix (a b)
  "The affine matrix taking segment B (two points) onto segment A."
  (let* ((u (vector (- (aref (nth 1 a) 0) (aref (nth 0 a) 0)) (- (aref (nth 1 a) 1) (aref (nth 0 a) 1))))
         (v (vector (- (aref (nth 1 b) 0) (aref (nth 0 b) 0)) (- (aref (nth 1 b) 1) (aref (nth 0 b) 1))))
         (phi (atan (- (* (aref u 0) (aref v 1)) (* (aref u 1) (aref v 0)))
                    (+ (* (aref u 0) (aref v 0)) (* (aref u 1) (aref v 1)))))
         (s (/ (sqrt (+ (expt (aref u 0) 2) (expt (aref u 1) 2))) (sqrt (+ (expt (aref v 0) 2) (expt (aref v 1) 2))))))
    (eas-geo-polyhedral--multiply
     (vector 1 0 (aref (nth 0 a) 0) 0 1 (aref (nth 0 a) 1))
     (eas-geo-polyhedral--multiply
      (vector s 0 0 0 s 0)
      (eas-geo-polyhedral--multiply
       (vector (cos phi) (sin phi) 0 (- (sin phi)) (cos phi) 0)
       (vector 1 0 (- (aref (nth 0 b) 0)) 0 1 (- (aref (nth 0 b) 1))))))))

;;; The face tree

(cl-defstruct (eas-geo-polyhedral--node (:constructor eas-geo-polyhedral--node-make (face project)) (:copier nil))
  "A face of the unfolding: vertices, gnomonic projection, transform, edges, children."
  face project (transform nil) (edges nil) (children nil))

(defun eas-geo-polyhedral--edges (face)
  "FACE's edges as (A . B) vertex pairs, from its last vertex round."
  (let ((a (car (last face))) out)
    (dolist (b face) (push (cons a b) out) (setq a b))
    (vconcat (nreverse out))))

(defun eas-geo-polyhedral--shared (a b)
  "The two vertices faces A and B share, in A's order."
  (let (found)
    (catch 'done
      (dolist (x a)
        (when (seq-some (lambda (y) (equal x y)) (reverse b))
          (if found (throw 'done (list found x)) (setq found x)))))))

(defun eas-geo-polyhedral--link (node parent)
  "Attach NODE under PARENT: transform and the shared edge, recursively."
  (setf (eas-geo-polyhedral--node-edges node) (eas-geo-polyhedral--edges (eas-geo-polyhedral--node-face node)))
  (when parent
    (let* ((shared (eas-geo-polyhedral--shared (eas-geo-polyhedral--node-face node) (eas-geo-polyhedral--node-face parent)))
           (on (lambda (n) (lambda (v) (funcall (eas-geo-polyhedral--node-project n) (aref v 0) (aref v 1)))))
           (m (eas-geo-polyhedral--matrix (mapcar (funcall on parent) shared) (mapcar (funcall on node) shared)))
           (pt (eas-geo-polyhedral--node-transform parent))
           (pe (eas-geo-polyhedral--node-edges parent)) (ne (eas-geo-polyhedral--node-edges node)))
      (setf (eas-geo-polyhedral--node-transform node) (if pt (eas-geo-polyhedral--multiply pt m) m))
      (dotimes (i (length pe))
        (let ((e (aref pe i)))
          (when (and (consp e) (or (and (equal (nth 0 shared) (cdr e)) (equal (nth 1 shared) (car e)))
                                   (and (equal (nth 0 shared) (car e)) (equal (nth 1 shared) (cdr e)))))
            (aset pe i node))))
      (dotimes (i (length ne))
        (let ((e (aref ne i)))
          (when (and (consp e) (or (and (equal (nth 0 shared) (car e)) (equal (nth 1 shared) (cdr e)))
                                   (and (equal (nth 0 shared) (cdr e)) (equal (nth 1 shared) (car e)))))
            (aset ne i parent))))))
  (dolist (c (eas-geo-polyhedral--node-children node)) (eas-geo-polyhedral--link c node)))

(defun eas-geo-polyhedral-butterfly ()
  "D3's polyhedralButterfly: (RAW . OUTLINE).
RAW maps (LAMBDA PHI) radians to [X Y]; OUTLINE streams the sphere's
outline (degrees) into a sink."
  (let* ((faces (vconcat (mapcar (lambda (f)
                                   (let ((c (eas-geo-polyhedral--centroid f)))
                                     (eas-geo-polyhedral--node-make
                                      f (plist-get (eas-geo-proj--simple #'eas-geo-raw-gnomonic :scale 1 :translate '(0 0)
                                                                         :rotate (list (- (aref c 0)) (- (aref c 1)) 0))
                                                   :point))))
                                 eas-geo-polyhedral--octahedron))))
    (seq-do-indexed (lambda (d i)
                      (when (>= d 0)
                        (let ((n (aref faces d)))
                          (setf (eas-geo-polyhedral--node-children n)
                                (append (eas-geo-polyhedral--node-children n) (list (aref faces i)))))))
                    [-1 0 0 1 0 1 4 5])
    (eas-geo-polyhedral--link (aref faces 0) nil)
    (cons
     (lambda (l p)
       (let* ((node (aref faces (cond ((< l (- eas-geo-half-pi)) (if (< p 0) 6 4))
                                      ((< l 0) (if (< p 0) 2 0))
                                      ((< l eas-geo-half-pi) (if (< p 0) 3 1))
                                      (t (if (< p 0) 7 5)))))
              (pt (funcall (eas-geo-polyhedral--node-project node) (/ l eas-geo-rad) (/ p eas-geo-rad)))
              (tm (eas-geo-polyhedral--node-transform node)))
         (if tm
             (vector (+ (* (aref tm 0) (aref pt 0)) (* (aref tm 1) (aref pt 1)) (aref tm 2))
                     (- (+ (* (aref tm 3) (aref pt 0)) (* (aref tm 4) (aref pt 1)) (aref tm 5))))
           (vector (aref pt 0) (- (aref pt 1))))))
     (lambda (sink)
       (eas-geo--call polygon-start sink)
       (eas-geo--call line-start sink)
       (eas-geo-polyhedral--outline sink (aref faces 0) nil)
       (eas-geo--call line-end sink)
       (eas-geo--call polygon-end sink)))))

(defun eas-geo-polyhedral--outline (sink node parent)
  "Stream NODE's free edges into SINK, descending into its children.
PARENT is the node it was reached from."
  (let* ((edges (eas-geo-polyhedral--node-edges node)) (n (length edges))
         (c (eas-geo-polyhedral--centroid (eas-geo-polyhedral--node-face node)))
         (inside nil) (j 0))
    (when parent
      (while (and (< j n) (not (eq (aref edges j) parent))) (setq j (1+ j))))
    (when parent (setq j (1+ j)))
    (dotimes (i n)
      (let ((e (aref edges (mod (+ i j) n))))
        (if (consp e)
            (progn
              (unless inside
                (let ((p (eas-geo-polyhedral--interpolate (car e) c eas-geo-eps))) (eas-geo-point sink (aref p 0) (aref p 1)))
                (setq inside t))
              (let ((p (eas-geo-polyhedral--interpolate (cdr e) c eas-geo-eps))) (eas-geo-point sink (aref p 0) (aref p 1))))
          (setq inside nil)
          (unless (eq e parent) (eas-geo-polyhedral--outline sink e node)))))))

(provide 'eas-geo-polyhedral)
;;; eas-geo-polyhedral.el ends here

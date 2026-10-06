;;; eas-contour-iso.el --- marching squares and the isocontour transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  `eas-contour-polygons' is d3-contour's contours():
;; marching squares over a grid (sample i, j at i + 0.5, j + 0.5),
;; isolines stitched into closed rings, each vertex moved along its
;; cell edge by linear interpolation when smoothing, rings of positive
;; area made polygons and the others assigned as holes to the first
;; polygon containing them.  The result is a GeoJSON MultiPolygon
;; whose region holds every point at or above the threshold.
;;
;; The "isocontour" domain transform is Vega's: thresholds given, or
;; LEVELS of them spread evenly over [min(0, lo), hi] of the grid values
;; (resolve "shared" spreads them over every grid's maximum), one
;; output row per grid and threshold with the contour (AS) and the
;; flat "threshold".  Coordinates are mapped by the grid's scale and
;; translate (or the transform's), keeping the winding when the map
;; flips an axis.  Grids come from a field (kde2d's) or from tidy
;; {x, y, value} rows (eas-contour-grid.el).

;;; Code:

(require 'eas-core)
(require 'eas-contour-grid)
(require 'eas-scale)
(require 'eas-transform-domain)

(defconst eas-contour--cases
  [nil
   (((1.0 1.5) (0.5 1.0)))
   (((1.5 1.0) (1.0 1.5)))
   (((1.5 1.0) (0.5 1.0)))
   (((1.0 0.5) (1.5 1.0)))
   (((1.0 1.5) (0.5 1.0)) ((1.0 0.5) (1.5 1.0)))
   (((1.0 0.5) (1.0 1.5)))
   (((1.0 0.5) (0.5 1.0)))
   (((0.5 1.0) (1.0 0.5)))
   (((1.0 1.5) (1.0 0.5)))
   (((0.5 1.0) (1.0 0.5)) ((1.5 1.0) (1.0 1.5)))
   (((1.5 1.0) (1.0 0.5)))
   (((0.5 1.0) (1.5 1.0)))
   (((1.0 1.5) (1.5 1.0)))
   (((0.5 1.0) (1.0 1.5)))
   nil]
  "Marching squares segments per case, as d3-contour's ((X0 Y0) (X1 Y1)).")

;; A fragment is [START END HEAD TAIL]: index keys of its ends and its
;; points as a list with its last cons, so both ends grow in O(1).

(defun eas-contour--above (v value)
  "1 when grid value V is at or above VALUE, else 0 (no data is below)."
  (if (and v (>= v value)) 1 0))

(defun eas-contour--isorings (values dx dy value callback)
  "Call CALLBACK with each closed ring of VALUES (DX x DY) at VALUE.
A ring is a list of [X Y] vectors whose first and last points agree."
  (let ((by-start (make-hash-table :test 'eql)) (by-end (make-hash-table :test 'eql))
        (x -1) (y -1) t0 t1 t2 t3)
    (cl-labels
        ((index (px py) (round (+ (* px 2) (* py (1+ dx) 4))))
         (frag (start end head tail) (vector start end head tail))
         (stitch (line)
           (let* ((start (vector (+ (car (car line)) x) (+ (cadr (car line)) y)))
                  (end (vector (+ (car (cadr line)) x) (+ (cadr (cadr line)) y)))
                  (si (index (aref start 0) (aref start 1))) (ei (index (aref end 0) (aref end 1)))
                  f g)
             (cond
              ((setq f (gethash si by-end))
               (if (setq g (gethash ei by-start))
                   (progn
                     (remhash (aref f 1) by-end) (remhash (aref g 0) by-start)
                     (if (eq f g)
                         (progn (setcdr (aref f 3) (list end)) (funcall callback (aref f 2)))
                       (setcdr (aref f 3) (aref g 2))
                       (let ((h (frag (aref f 0) (aref g 1) (aref f 2) (aref g 3))))
                         (puthash (aref f 0) h by-start) (puthash (aref g 1) h by-end))))
                 (remhash (aref f 1) by-end)
                 (let ((cell (list end))) (setcdr (aref f 3) cell) (aset f 3 cell))
                 (aset f 1 ei)
                 (puthash ei f by-end)))
              ((setq f (gethash ei by-start))
               (if (setq g (gethash si by-end))
                   (progn
                     (remhash (aref f 0) by-start) (remhash (aref g 1) by-end)
                     (if (eq f g)
                         (progn (setcdr (aref f 3) (list end)) (funcall callback (aref f 2)))
                       (setcdr (aref g 3) (aref f 2))
                       (let ((h (frag (aref g 0) (aref f 1) (aref g 2) (aref f 3))))
                         (puthash (aref g 0) h by-start) (puthash (aref f 1) h by-end))))
                 (remhash (aref f 0) by-start)
                 (aset f 2 (cons start (aref f 2)))
                 (aset f 0 si)
                 (puthash si f by-start)))
              (t (let* ((head (list start end)) (h (frag si ei head (cdr head))))
                   (puthash si h by-start) (puthash ei h by-end))))))
         (at (i) (eas-contour--above (aref values i) value))
         (run (case) (dolist (line (aref eas-contour--cases case)) (stitch line))))
      ;; The first row (y = -1, t2 = t3 = 0).
      (setq t1 (at 0))
      (run (ash t1 1))
      (while (< (setq x (1+ x)) (1- dx))
        (setq t0 t1 t1 (at (1+ x)))
        (run (logior t0 (ash t1 1))))
      (run t1)
      ;; The rows between.
      (while (< (setq y (1+ y)) (1- dy))
        (setq x -1 t1 (at (+ (* y dx) dx)) t2 (at (* y dx)))
        (run (logior (ash t1 1) (ash t2 2)))
        (while (< (setq x (1+ x)) (1- dx))
          (setq t0 t1 t1 (at (+ (* y dx) dx x 1))
                t3 t2 t2 (at (+ (* y dx) x 1)))
          (run (logior t0 (ash t1 1) (ash t2 2) (ash t3 3))))
        (run (logior t1 (ash t2 3))))
      ;; The last row (y = dy - 1, t0 = t1 = 0).
      (setq x -1 t2 (at (* y dx)))
      (run (ash t2 2))
      (while (< (setq x (1+ x)) (1- dx))
        (setq t3 t2 t2 (at (+ (* y dx) x 1)))
        (run (logior (ash t2 2) (ash t3 3))))
      (run (ash t2 3)))))

(defun eas-contour--valid (values i)
  "Grid VALUES at I, or negative infinity for no data (or past the end)."
  (let ((v (and (<= 0 i) (< i (length values)) (aref values i))))
    (if (and v (not (isnan v))) v -1.0e+INF)))

(defun eas-contour--smooth1 (x v0 v1 value)
  "Coordinate X moved toward where VALUE lies between samples V0 and V1."
  (let* ((a (- value v0)) (b (- v1 v0))
         (d (if (or (not (isnan (- a a))) (not (isnan (- b b))))
                (/ (float a) b)
              (/ (float (cl-signum a)) (cl-signum b)))))
    (if (isnan d) x (+ x d -0.5))))

(defun eas-contour--smooth (ring values dx dy value)
  "Move RING's vertices along their cell edges by linear interpolation.
VALUES is the DX x DY grid and VALUE the threshold."
  (dolist (p ring)
    (let* ((x (aref p 0)) (y (aref p 1)) (xt (floor x)) (yt (floor y))
           (v1 (eas-contour--valid values (+ (* yt dx) xt))))
      (when (and (> x 0) (< x dx) (= xt x))
        (aset p 0 (eas-contour--smooth1 x (eas-contour--valid values (+ (* yt dx) xt -1)) v1 value)))
      (when (and (> y 0) (< y dy) (= yt y))
        (aset p 1 (eas-contour--smooth1 y (eas-contour--valid values (+ (* (1- yt) dx) xt)) v1 value))))))

(defun eas-contour-ring-area (ring)
  "Twice the signed area of RING (a list of [X Y]), d3-contour's sign."
  (let* ((pts (vconcat ring)) (n (length pts))
         (a (- (* (aref (aref pts (1- n)) 1) (aref (aref pts 0) 0))
               (* (aref (aref pts (1- n)) 0) (aref (aref pts 0) 1)))))
    (cl-loop for i from 1 below n
             do (setq a (+ a (- (* (aref (aref pts (1- i)) 1) (aref (aref pts i) 0))
                                (* (aref (aref pts (1- i)) 0) (aref (aref pts i) 1))))))
    a))

(defun eas-contour--segment-contains (a b c)
  "Non-nil when point C lies on segment A B."
  (and (= (* (- (aref b 0) (aref a 0)) (- (aref c 1) (aref a 1)))
          (* (- (aref c 0) (aref a 0)) (- (aref b 1) (aref a 1))))
       (let ((i (if (= (aref a 0) (aref b 0)) 1 0)))
         (let ((p (aref a i)) (q (aref c i)) (r (aref b i)))
           (or (and (<= p q) (<= q r)) (and (<= r q) (<= q p)))))))

(defun eas-contour--ring-contains (ring point)
  "1 when POINT is inside RING (a vector of [X Y]), 0 on it, -1 outside."
  (let ((x (aref point 0)) (y (aref point 1)) (contains -1) (n (length ring)) (result nil))
    (cl-loop for i from 0 below n
             for j = (if (= i 0) (1- n) (1- i))
             for pi = (aref ring i) for pj = (aref ring j)
             do (cond
                 ((eas-contour--segment-contains pi pj point) (setq result 0) (cl-return))
                 ((and (not (eq (> (aref pi 1) y) (> (aref pj 1) y)))
                       (< x (+ (/ (* (- (aref pj 0) (aref pi 0)) (- y (aref pi 1))) (- (aref pj 1) (aref pi 1)))
                               (aref pi 0))))
                  (setq contains (- contains)))))
    (or result contains)))

(defun eas-contour--contains (ring hole)
  "Return 1 when HOLE lies inside RING (a vector), -1 outside, 0 undecided.
The first vertex of HOLE off RING's edge decides."
  (or (cl-loop for p in hole for c = (eas-contour--ring-contains ring p) unless (= c 0) return c) 0))

(defun eas-contour-polygons (values dx dy value &optional smooth)
  "The MultiPolygon coordinates of DX x DY grid VALUES at or above VALUE.
SMOOTH interpolates vertices along cell edges.  Return a list of
polygons, each a list of rings (the first its exterior), each ring a
list of [X Y] in grid units."
  (let (polygons holes)
    (eas-contour--isorings values dx dy value
                           (lambda (ring)
                             (when smooth (eas-contour--smooth ring values dx dy value))
                             (if (> (eas-contour-ring-area ring) 0) (push (list ring) polygons) (push ring holes))))
    (setq polygons (nreverse polygons))
    (when holes
      ;; (BOX RING . POLYGON): a hole whose first vertex lies outside an
      ;; exterior's bounding box is outside it, as the full test would say.
      (let ((exteriors (mapcar (lambda (poly)
                                 (let ((r (vconcat (car poly))))
                                   (cl-list* (vector (seq-min (mapcar (lambda (p) (aref p 0)) r))
                                                     (seq-min (mapcar (lambda (p) (aref p 1)) r))
                                                     (seq-max (mapcar (lambda (p) (aref p 0)) r))
                                                     (seq-max (mapcar (lambda (p) (aref p 1)) r)))
                                             r poly)))
                               polygons)))
        (dolist (hole (nreverse holes))
          (let* ((p (car hole))
                 (owner (seq-find (lambda (e)
                                    (let ((b (car e)))
                                      (and (<= (aref b 0) (aref p 0) (aref b 2)) (<= (aref b 1) (aref p 1) (aref b 3))
                                           (/= (eas-contour--contains (cadr e) hole) -1))))
                                  exteriors)))
            (when owner (nconc (cddr owner) (list hole)))))))
    polygons))

;;; Thresholds

(defun eas-contour-quantize (k nice zero values)
  "K thresholds over the extent of VALUES, as Vega's isocontour levels.
ZERO starts the extent at min(0, lo); NICE steps by d3's tickStep."
  (let* ((vals (delq nil (append values nil)))
         (lo (if vals (apply #'min vals) 0)) (hi (if vals (apply #'max vals) 0))
         (start (if zero (min lo 0) lo)) (span (- hi start))
         (step (if nice (let ((inc (eas-scale-tick-increment start hi k)))
                          (if (< inc 0) (/ -1.0 inc) (float inc)))
                 (/ span (float (1+ k))))))
    (when (> step 0)
      (cl-loop for i from 1 for v = (+ start (* i step)) while (< v hi) collect v))))

;;; Coordinates

(defun eas-contour--map (polygons grid scale translate)
  "POLYGONS in GRID units mapped to coordinates by SCALE and TRANSLATE.
Either may override the grid's own; return vectors of rings of [X Y]."
  (pcase-let* ((`(,gsx ,gsy ,gtx ,gty ,x1 ,y1) (eas-contour-grid-mapping grid))
               (s (eas-contour-grid--pair scale (cons gsx gsy)))
               (tr (eas-contour-grid--pair translate (cons gtx gty)))
               (sx (float (car s))) (sy (float (cdr s))) (tx (car tr)) (ty (cdr tr))
               (flip (< (* sx sy) 0)))
    (vconcat
     (mapcar (lambda (poly)
               (vconcat (mapcar (lambda (ring)
                                  (let ((pts (mapcar (lambda (p) (vector (+ tx (* sx (- (aref p 0) x1)))
                                                                         (+ ty (* sy (- (aref p 1) y1)))))
                                                     ring)))
                                    (vconcat (if flip (nreverse pts) pts))))
                                poly)))
             polygons))))

;; These loops touch every cell and vertex; interpreted (as when `make
;; test' loads the source) they take seconds per grid.
(dolist (f '(eas-contour--isorings eas-contour--smooth eas-contour--smooth1 eas-contour--valid
             eas-contour-ring-area eas-contour--segment-contains eas-contour--ring-contains
             eas-contour--contains eas-contour-polygons eas-contour--map))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

;;; The transform

(defun eas-contour--thresholds (params pairs)
  "The thresholds PARAMS give, or a function of a grid's values to them.
PAIRS are every (ROW . GRID), for resolve \"shared\"."
  (let ((given (plist-get params :thresholds)))
    (if (vectorp given)
        (append given nil)
      (let ((k (or (plist-get params :levels) 10)) (nice (eas-true-p (plist-get params :nice)))
            (zero (not (eq (plist-get params :zero) :false))))
        (if (equal (plist-get params :resolve) "shared")
            (eas-contour-quantize k nice zero (mapcar (lambda (p) (eas-contour-grid-max (cdr p))) pairs))
          (lambda (values) (eas-contour-quantize k nice zero values)))))))

(defun eas-contour-isocontour (rows params)
  "The isocontour transform: contour rows of the grids in ROWS under PARAMS."
  (let* ((pairs (eas-contour-grids rows params))
         (tz (eas-contour--thresholds params pairs))
         (smooth (not (eq (plist-get params :smooth) :false)))
         (as (eas-key (plist-get params :as)))
         out)
    (dolist (pair pairs)
      (let* ((grid (cdr pair)) (values (plist-get grid :values))
             (dx (plist-get grid :width)) (dy (plist-get grid :height)))
        (dolist (v (if (functionp tz) (funcall tz values) tz))
          (let ((coords (eas-contour--map (eas-contour-polygons values dx dy v smooth) grid
                                          (plist-get params :scale) (plist-get params :translate))))
            (push (append (car pair)
                          (list as (list :type "MultiPolygon" :value v :coordinates coords)
                                :threshold v))
                  out)))))
    (vconcat (nreverse out))))

(eas-register-transform
 "isocontour"
 :doc "Vega's isocontour: GeoJSON MultiPolygon contours of value grids at thresholds (marching squares)."
 :schema '(:field (:type "string" :doc "field holding a grid (kde2d's); else rows are x/y/value cells")
           :x (:type "string" :default "x" :doc "cell x field of tidy grid rows")
           :y (:type "string" :default "y" :doc "cell y field of tidy grid rows")
           :value (:type "string" :default "value" :doc "cell value field of tidy grid rows")
           :groupby (:type "array" :default [] :doc "fields making one grid per group of tidy rows")
           :thresholds (:type "array" :doc "contour thresholds; else levels of them")
           :levels (:type "integer" :default 10 :doc "number of thresholds when none are given")
           :nice (:type "boolean" :default :false :doc "step thresholds by round values")
           :zero (:type "boolean" :default t :doc "start the threshold extent at zero")
           :resolve (:type "string" :default "independent" :doc "shared: levels over every grid's maximum")
           :smooth (:type "boolean" :default t :doc "interpolate vertices along cell edges")
           :scale (:type "any" :doc "number or [sx sy] mapping grid units, else the grid's")
           :translate (:type "array" :doc "[tx ty] after scale, else the grid's")
           :as (:type "string" :default "contour" :doc "output geometry field; threshold holds its value"))
 :fn #'eas-contour-isocontour)

(provide 'eas-contour-iso)
;;; eas-contour-iso.el ends here

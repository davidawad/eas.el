;;; eas-geo-stream.el --- d3-geo streams: rotation, resampling, paths -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, the spherical half of geoshape (eas-geoshape.el).  A port
;; of d3-geo's stream pipeline, so a map draws the paths Vega draws:
;;
;;   GeoJSON -> degrees to radians -> 3-axis rotation -> preclip
;;   (antimeridian cut or small circle, eas-geo-clip.el) -> adaptive
;;   resampling in projected space -> postclip (rectangle) -> sink
;;
;; A stream is an `eas-geo-stream' of closures (point, lineStart, ...)
;; as in d3.  The path sink collects pixel rings, open lines and point
;; circles; the measure helpers give d3's planar path area and centroid
;; (Vega's geoArea and geoCentroid with a projection).  The graticule
;; is d3.geoGraticule's MultiLineString.

;;; Code:

(require 'cl-lib)
(require 'eas-core)

(defconst eas-geo-eps 1e-6 "D3-geo's epsilon.")
(defconst eas-geo-eps2 1e-12 "D3-geo's epsilon squared.")
(defconst eas-geo-half-pi (/ float-pi 2) "Half of pi.")
(defconst eas-geo-tau (* 2 float-pi) "Two pi.")
(defconst eas-geo-rad (/ float-pi 180) "Radians per degree.")
(defconst eas-geo-nan 0.0e+NaN "Not a number, d3's NaN.")

(cl-defstruct (eas-geo-stream (:constructor eas-geo-stream--make) (:copier nil))
  "A d3-geo stream: closures fed by the geometry, in order."
  (point #'ignore) (line-start #'ignore) (line-end #'ignore)
  (polygon-start #'ignore) (polygon-end #'ignore) (sphere #'ignore)
  (clean nil) (result nil) (rejoin nil))

(defmacro eas-geo--call (slot stream &rest args)
  "Call STREAM's SLOT closure with ARGS."
  `(funcall (,(intern (format "eas-geo-stream-%s" slot)) ,stream) ,@args))

(defun eas-geo-point (s x y &optional m)
  "Send point X Y (flag M) to stream S."
  (funcall (eas-geo-stream-point s) x y m))

(defun eas-geo-asin (x) "D3's clamped asin of X." (cond ((> x 1) eas-geo-half-pi) ((< x -1) (- eas-geo-half-pi)) (t (asin x))))
(defun eas-geo-acos (x) "D3's clamped acos of X." (cond ((> x 1) 0.0) ((< x -1) float-pi) (t (acos x))))
(defun eas-geo-sign (x) "Sign of X as d3 has it." (cond ((> x 0) 1) ((< x 0) -1) (t 0)))
(defun eas-geo-nan-p (x) "Non-nil when X is NaN." (and (floatp x) (isnan x)))
(defun eas-geo-rem (a b) "JavaScript's A % B." (- a (* b (ftruncate (/ a (float b))))))

;;; Cartesian helpers

(defun eas-geo-cartesian (lam phi)
  "Unit vector of spherical LAM PHI (radians)."
  (let ((c (cos phi))) (vector (* c (cos lam)) (* c (sin lam)) (sin phi))))

(defun eas-geo-spherical (v)
  "Spherical [lambda phi] of cartesian V."
  (vector (atan (aref v 1) (aref v 0)) (eas-geo-asin (aref v 2))))

(defun eas-geo-dot (a b) "Dot product of A and B."
  (+ (* (aref a 0) (aref b 0)) (* (aref a 1) (aref b 1)) (* (aref a 2) (aref b 2))))

(defun eas-geo-cross (a b) "Cross product of A and B."
  (vector (- (* (aref a 1) (aref b 2)) (* (aref a 2) (aref b 1)))
          (- (* (aref a 2) (aref b 0)) (* (aref a 0) (aref b 2)))
          (- (* (aref a 0) (aref b 1)) (* (aref a 1) (aref b 0)))))

(defun eas-geo-scale3 (v k) "V times K." (vector (* k (aref v 0)) (* k (aref v 1)) (* k (aref v 2))))
(defun eas-geo-add3 (a b) "A plus B." (vector (+ (aref a 0) (aref b 0)) (+ (aref a 1) (aref b 1)) (+ (aref a 2) (aref b 2))))

(defun eas-geo-normalize (v)
  "V scaled to unit length (V itself when null)."
  (let ((l (sqrt (eas-geo-dot v v)))) (if (> l 0) (eas-geo-scale3 v (/ 1.0 l)) v)))

(defun eas-geo-point-equal (a b)
  "Non-nil when points A and B coincide within epsilon."
  (and (< (abs (- (aref a 0) (aref b 0))) eas-geo-eps) (< (abs (- (aref a 1) (aref b 1))) eas-geo-eps)))

;;; Rotation (d3 rotation.js)

(defun eas-geo--wrap-lambda (lam)
  "LAM wrapped into [-pi, pi] as d3 does."
  (if (> (abs lam) float-pi) (- lam (* (fround (/ lam eas-geo-tau)) eas-geo-tau)) lam))

(defun eas-geo-rotation (dl dp dg)
  "D3's rotateRadians: (FORWARD . INVERSE), each (L P) -> [L P].
DL DP DG are the rotation angles in radians."
  (let* ((dl (eas-geo-rem dl eas-geo-tau))
         (cdp (cos dp)) (sdp (sin dp)) (cdg (cos dg)) (sdg (sin dg))
         (pg (and (or (/= dp 0) (/= dg 0))
                  (cons (lambda (l p)
                          (let* ((c (cos p)) (x (* (cos l) c)) (y (* (sin l) c)) (z (sin p))
                                 (k (+ (* z cdp) (* x sdp))))
                            (vector (atan (- (* y cdg) (* k sdg)) (- (* x cdp) (* z sdp)))
                                    (eas-geo-asin (+ (* k cdg) (* y sdg))))))
                        (lambda (l p)
                          (let* ((c (cos p)) (x (* (cos l) c)) (y (* (sin l) c)) (z (sin p))
                                 (k (- (* z cdg) (* y sdg))))
                            (vector (atan (+ (* y cdg) (* z sdg)) (+ (* x cdp) (* k sdp)))
                                    (eas-geo-asin (- (* k cdp) (* x sdp)))))))))
         (lam (lambda (d) (lambda (l p) (vector (eas-geo--wrap-lambda (+ l d)) p)))))
    (cond
     ((and (/= dl 0) pg)
      (cons (lambda (l p) (let ((a (funcall (funcall lam dl) l p))) (funcall (car pg) (aref a 0) (aref a 1))))
            (lambda (l p) (let ((a (funcall (cdr pg) l p))) (funcall (funcall lam (- dl)) (aref a 0) (aref a 1))))))
     ((/= dl 0) (cons (funcall lam dl) (funcall lam (- dl))))
     (pg pg)
     (t (let ((id (lambda (l p) (vector (eas-geo--wrap-lambda l) p)))) (cons id id))))))

;;; Transform streams

(defun eas-geo-transformer (sink point)
  "A stream passing everything to SINK but points, sent to POINT (S X Y M)."
  (eas-geo-stream--make
   :point (lambda (x y &optional m) (funcall point sink x y m))
   :line-start (lambda () (eas-geo--call line-start sink))
   :line-end (lambda () (eas-geo--call line-end sink))
   :polygon-start (lambda () (eas-geo--call polygon-start sink))
   :polygon-end (lambda () (eas-geo--call polygon-end sink))
   :sphere (lambda () (eas-geo--call sphere sink))))

(defun eas-geo-radians-rotate (rotate sink)
  "Stream converting degrees to radians, then ROTATE (L P -> [L P]), into SINK."
  (eas-geo-transformer sink (lambda (s x y _m)
                              (let ((r (funcall rotate (* x eas-geo-rad) (* y eas-geo-rad))))
                                (eas-geo-point s (aref r 0) (aref r 1))))))

;;; Adaptive resampling (d3 resample.js)

(defconst eas-geo--max-depth 16 "Resampling depth limit.")
(defconst eas-geo--cos-min-distance (cos (* 30 eas-geo-rad)) "Resampling angular limit.")

(defun eas-geo-resample (project delta2)
  "D3's resample: PROJECT (L P -> [X Y]) at precision DELTA2.
Return a function SINK -> stream."
  (if (not (> delta2 0))
      (lambda (sink) (eas-geo-transformer sink (lambda (s x y _m) (let ((p (funcall project x y))) (eas-geo-point s (aref p 0) (aref p 1))))))
    (lambda (sink) (eas-geo--resample-stream project delta2 sink))))

(defun eas-geo--resample-line (project delta2 x0 y0 l0 a0 b0 c0 x1 y1 l1 a1 b1 c1 depth sink)
  "Subdivide the projected segment X0 Y0 - X1 Y1 into SINK.
L0 A0 B0 C0 and L1 A1 B1 C1 are the ends' longitude and unit vector;
PROJECT and DELTA2 as in `eas-geo-resample'; DEPTH is what remains."
  (let* ((dx (- x1 x0)) (dy (- y1 y0)) (d2 (+ (* dx dx) (* dy dy))))
    (when (and (> d2 (* 4 delta2)) (> depth 0))
      (setq depth (1- depth))
      (let* ((a (+ a0 a1)) (b (+ b0 b1)) (c (+ c0 c1))
             (m (sqrt (+ (* a a) (* b b) (* c c))))
             (c (/ c m))
             (p2 (eas-geo-asin c))
             (l2 (if (or (< (abs (- (abs c) 1)) eas-geo-eps) (< (abs (- l0 l1)) eas-geo-eps))
                     (/ (+ l0 l1) 2) (atan b a)))
             (p (funcall project l2 p2)) (x2 (aref p 0)) (y2 (aref p 1))
             (dx2 (- x2 x0)) (dy2 (- y2 y0))
             (dz (- (* dy dx2) (* dx dy2))))
        (when (or (> (/ (* dz dz) d2) delta2)
                  (> (abs (- (/ (+ (* dx dx2) (* dy dy2)) d2) 0.5)) 0.3)
                  (< (+ (* a0 a1) (* b0 b1) (* c0 c1)) eas-geo--cos-min-distance))
          (let ((a (/ a m)) (b (/ b m)))
            (eas-geo--resample-line project delta2 x0 y0 l0 a0 b0 c0 x2 y2 l2 a b c depth sink)
            (eas-geo-point sink x2 y2)
            (eas-geo--resample-line project delta2 x2 y2 l2 a b c x1 y1 l1 a1 b1 c1 depth sink)))))))

(defun eas-geo--resample-stream (project delta2 sink)
  "The resampling stream of PROJECT at DELTA2 into SINK."
  (let ((l00 0) (x00 0) (y00 0) (a00 0) (b00 0) (c00 0)
        (l0 0) (x0 eas-geo-nan) (y0 eas-geo-nan) (a0 0) (b0 0) (c0 0)
        (s (eas-geo-stream--make)))
    (cl-labels ((point (x y &optional _m)
                  (let ((p (funcall project x y))) (eas-geo-point sink (aref p 0) (aref p 1))))
                (line-point (l p &optional _m)
                  (let ((c (eas-geo-cartesian l p)) (pr (funcall project l p)))
                    (eas-geo--resample-line project delta2 x0 y0 l0 a0 b0 c0
                                            (aref pr 0) (aref pr 1) l (aref c 0) (aref c 1) (aref c 2)
                                            eas-geo--max-depth sink)
                    (setq x0 (aref pr 0) y0 (aref pr 1) l0 l a0 (aref c 0) b0 (aref c 1) c0 (aref c 2))
                    (eas-geo-point sink x0 y0)))
                (line-start ()
                  (setq x0 eas-geo-nan)
                  (setf (eas-geo-stream-point s) #'line-point)
                  (eas-geo--call line-start sink))
                (line-end ()
                  (setf (eas-geo-stream-point s) #'point)
                  (eas-geo--call line-end sink))
                (ring-point (l p &optional _m)
                  (setq l00 l)
                  (line-point l p)
                  (setq x00 x0 y00 y0 a00 a0 b00 b0 c00 c0)
                  (setf (eas-geo-stream-point s) #'line-point))
                (ring-start ()
                  (line-start)
                  (setf (eas-geo-stream-point s) #'ring-point
                        (eas-geo-stream-line-end s) #'ring-end))
                (ring-end ()
                  (eas-geo--resample-line project delta2 x0 y0 l0 a0 b0 c0 x00 y00 l00 a00 b00 c00
                                          eas-geo--max-depth sink)
                  (setf (eas-geo-stream-line-end s) #'line-end)
                  (line-end)))
      (setf (eas-geo-stream-point s) #'point
            (eas-geo-stream-line-start s) #'line-start
            (eas-geo-stream-line-end s) #'line-end
            (eas-geo-stream-polygon-start s)
            (lambda () (eas-geo--call polygon-start sink) (setf (eas-geo-stream-line-start s) #'ring-start))
            (eas-geo-stream-polygon-end s)
            (lambda () (eas-geo--call polygon-end sink) (setf (eas-geo-stream-line-start s) #'line-start))
            (eas-geo-stream-sphere s) (lambda () (eas-geo--call sphere sink)))
      s)))

;;; GeoJSON into a stream (d3 stream.js)

(defun eas-geo--line (coords s closed)
  "Stream line COORDS into S, dropping the last point when CLOSED."
  (let ((n (- (length coords) (if closed 1 0))))
    (eas-geo--call line-start s)
    (dotimes (i n)
      (let ((c (aref coords i))) (eas-geo-point s (aref c 0) (aref c 1))))
    (eas-geo--call line-end s)))

(defun eas-geo--polygon (rings s)
  "Stream polygon RINGS into S."
  (eas-geo--call polygon-start s)
  (seq-doseq (ring rings) (eas-geo--line ring s t))
  (eas-geo--call polygon-end s))

(defun eas-geo-stream-geometry (g s)
  "Stream GeoJSON geometry G (a plist) into S."
  (let ((coords (plist-get g :coordinates)))
    (pcase (and (eas-object-p g) (plist-get g :type))
      ("Sphere" (eas-geo--call sphere s))
      ("Point" (when (vectorp coords) (eas-geo-point s (aref coords 0) (aref coords 1))))
      ("MultiPoint" (seq-doseq (c coords) (eas-geo-point s (aref c 0) (aref c 1))))
      ("LineString" (eas-geo--line coords s nil))
      ("MultiLineString" (seq-doseq (c coords) (eas-geo--line c s nil)))
      ("Polygon" (eas-geo--polygon coords s))
      ("MultiPolygon" (seq-doseq (c coords) (eas-geo--polygon c s)))
      ("GeometryCollection" (seq-doseq (c (plist-get g :geometries)) (eas-geo-stream-geometry c s))))))

(defun eas-geo-stream-object (o s)
  "Stream GeoJSON object O (feature, collection or geometry) into S."
  (pcase (and (eas-object-p o) (plist-get o :type))
    ("Feature" (eas-geo-stream-geometry (plist-get o :geometry) s))
    ("FeatureCollection" (seq-doseq (f (plist-get o :features)) (eas-geo-stream-geometry (plist-get f :geometry) s)))
    (_ (eas-geo-stream-geometry o s))))

;;; The path sink

(defun eas-geo-path-sink (radius)
  "A sink collecting projected geometry; RADIUS is a Point's circle radius.
`eas-geo-stream-result' returns (:paths PATHS :circles CIRCLES :polygons N):
PATHS a list of (CLOSED . FLAT-XY-VECTOR) in stream order, CIRCLES a list
of [CX CY R]."
  (let ((paths nil) (circles nil) (polygon nil) (line nil) (pts nil) (npoly 0)
        (s (eas-geo-stream--make)))
    (setf (eas-geo-stream-point s)
          (lambda (x y &optional _m)
            (if line (setcar pts (cons y (cons x (car pts)))) (push (vector x y radius) circles)))
          (eas-geo-stream-line-start s) (lambda () (setq line t) (push nil pts))
          (eas-geo-stream-line-end s)
          (lambda ()
            (setq line nil)
            (let ((flat (vconcat (nreverse (pop pts)))))
              (when (> (length flat) 0) (push (cons polygon flat) paths))))
          (eas-geo-stream-polygon-start s) (lambda () (setq polygon t))
          (eas-geo-stream-polygon-end s) (lambda () (setq polygon nil npoly (1+ npoly)))
          (eas-geo-stream-result s)
          (lambda () (list :paths (nreverse paths) :circles (nreverse circles) :polygons npoly)))
    s))

;;; Planar measures of a path (d3 path/area.js, path/centroid.js)

(defun eas-geo-ring-area (flat)
  "Signed shoelace area of closed ring FLAT [x0 y0 x1 y1 ...], as d3 sums it."
  (let ((n (/ (length flat) 2)) (sum 0.0))
    (when (> n 0)
      (dotimes (i n)
        (let* ((j (mod (1+ i) n))
               (x0 (aref flat (* 2 i))) (y0 (aref flat (1+ (* 2 i))))
               (x1 (aref flat (* 2 j))) (y1 (aref flat (1+ (* 2 j)))))
          (setq sum (+ sum (- (* x0 y1) (* y0 x1)))))))
    (/ sum 2)))

(defun eas-geo-path-area (result)
  "Planar area of a path RESULT (`eas-geo-path-sink'), d3's path.area.
Each closed ring's signed area summed, then made positive."
  (abs (apply #'+ 0.0 (mapcar (lambda (p) (if (car p) (eas-geo-ring-area (cdr p)) 0.0))
                              (plist-get result :paths)))))

(defun eas-geo-path-centroid (result)
  "Planar centroid [X Y] of a path RESULT, d3's path.centroid; nil if empty.
Rings weigh by area, else lines by length, else points count."
  (let ((x2 0.0) (y2 0.0) (z2 0.0) (x1 0.0) (y1 0.0) (z1 0.0) (x0 0.0) (y0 0.0) (z0 0))
    (dolist (p (plist-get result :paths))
      (let* ((flat (cdr p)) (n (/ (length flat) 2)))
        (dotimes (i n)
          (setq x0 (+ x0 (aref flat (* 2 i))) y0 (+ y0 (aref flat (1+ (* 2 i)))) z0 (1+ z0)))
        (dotimes (i (if (car p) n (1- n)))
          (let* ((j (mod (1+ i) n))
                 (ax (aref flat (* 2 i))) (ay (aref flat (1+ (* 2 i))))
                 (bx (aref flat (* 2 j))) (by (aref flat (1+ (* 2 j))))
                 (len (sqrt (+ (expt (- bx ax) 2) (expt (- by ay) 2))))
                 (z (- (* ay bx) (* ax by))))
            (setq x1 (+ x1 (* len (/ (+ ax bx) 2))) y1 (+ y1 (* len (/ (+ ay by) 2))) z1 (+ z1 len))
            (when (car p)
              (setq x2 (+ x2 (* z (+ ax bx))) y2 (+ y2 (* z (+ ay by))) z2 (+ z2 (* 3 z))))))))
    (dolist (c (plist-get result :circles))
      (setq x0 (+ x0 (aref c 0)) y0 (+ y0 (aref c 1)) z0 (1+ z0)))
    (cond ((/= z2 0) (vector (/ x2 z2) (/ y2 z2)))
          ((/= z1 0) (vector (/ x1 z1) (/ y1 z1)))
          ((/= z0 0) (vector (/ x0 z0) (/ y0 z0))))))

(defun eas-geo-bounds-sink ()
  "A sink measuring the planar bounds of what it receives (d3 path/bounds.js).
Its result is [X0 Y0 X1 Y1] (infinite when empty)."
  (let ((b (vector 1.0e+INF 1.0e+INF -1.0e+INF -1.0e+INF)))
    (eas-geo-stream--make
     :point (lambda (x y &optional _m)
              (when (< x (aref b 0)) (aset b 0 x)) (when (> x (aref b 2)) (aset b 2 x))
              (when (< y (aref b 1)) (aset b 1 y)) (when (> y (aref b 3)) (aset b 3 y)))
     :result (lambda () b))))

;;; Graticule (d3 graticule.js)

(defun eas-geo--range (start stop step)
  "D3's range START..STOP (exclusive) by STEP."
  (let ((n (max 0 (ceiling (/ (- stop start) (float step))))))
    (cl-loop for i below n collect (+ start (* i step)))))

(defun eas-geo-graticule (&optional params)
  "D3's graticule as a GeoJSON MultiLineString plist.
PARAMS may hold Vega-Lite's :extentMajor :extentMinor :stepMajor
:stepMinor :step (minor) :extent (both) and :precision."
  (let* ((ext (plist-get params :extent))
         (major (or (plist-get params :extentMajor) ext (vector (vector -180 (+ -90 eas-geo-eps)) (vector 180 (- 90 eas-geo-eps)))))
         (minor (or (plist-get params :extentMinor) ext (vector (vector -180 (- -80 eas-geo-eps)) (vector 180 (+ 80 eas-geo-eps)))))
         (smaj (or (plist-get params :stepMajor) [90 360]))
         (smin (or (plist-get params :stepMinor) (plist-get params :step) [10 10]))
         (precision (or (plist-get params :precision) 2.5))
         (X0 (aref (aref major 0) 0)) (Y0 (aref (aref major 0) 1)) (X1 (aref (aref major 1) 0)) (Y1 (aref (aref major 1) 1))
         (x0 (aref (aref minor 0) 0)) (y0 (aref (aref minor 0) 1)) (x1 (aref (aref minor 1) 0)) (y1 (aref (aref minor 1) 1))
         (DX (aref smaj 0)) (DY (aref smaj 1)) (dx (aref smin 0)) (dy (aref smin 1))
         (merid (lambda (ya yb step) (let ((ys (append (eas-geo--range ya (- yb eas-geo-eps) step) (list yb))))
                                       (lambda (x) (vconcat (mapcar (lambda (y) (vector x y)) ys))))))
         (paral (lambda (xa xb step) (let ((xs (append (eas-geo--range xa (- xb eas-geo-eps) step) (list xb))))
                                       (lambda (y) (vconcat (mapcar (lambda (x) (vector x y)) xs))))))
         (mx (funcall merid y0 y1 90)) (my (funcall paral x0 x1 precision))
         (MX (funcall merid Y0 Y1 90)) (MY (funcall paral X0 X1 precision)))
    (list :type "MultiLineString"
          :coordinates
          (vconcat
           (mapcar MX (eas-geo--range (* (fceiling (/ X0 (float DX))) DX) X1 DX))
           (mapcar MY (eas-geo--range (* (fceiling (/ Y0 (float DY))) DY) Y1 DY))
           (mapcar mx (seq-filter (lambda (x) (> (abs (eas-geo-rem x DX)) eas-geo-eps))
                                  (eas-geo--range (* (fceiling (/ x0 (float dx))) dx) x1 dx)))
           (mapcar my (seq-filter (lambda (y) (> (abs (eas-geo-rem y DY)) eas-geo-eps))
                                  (eas-geo--range (* (fceiling (/ y0 (float dy))) dy) y1 dy)))))))

(provide 'eas-geo-stream)
;;; eas-geo-stream.el ends here

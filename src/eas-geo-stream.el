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
(defconst eas-geo--quarter-pi (/ float-pi 4) "Quarter of pi.")
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

(defsubst eas-geo-point (s x y &optional m)
  "Send point X Y (flag M) to stream S."
  (funcall (eas-geo-stream-point s) x y m))

(defsubst eas-geo-asin (x) "D3's clamped asin of X." (cond ((> x 1) eas-geo-half-pi) ((< x -1) (- eas-geo-half-pi)) (t (asin x))))
(defsubst eas-geo-acos (x) "D3's clamped acos of X." (cond ((> x 1) 0.0) ((< x -1) float-pi) (t (acos x))))
(defsubst eas-geo-sign (x) "Sign of X as d3 has it." (cond ((> x 0) 1) ((< x 0) -1) (t 0)))
(defun eas-geo-nan-p (x) "Non-nil when X is NaN." (and (floatp x) (isnan x)))
(defsubst eas-geo-rem (a b) "JavaScript's A % B." (- a (* b (ftruncate (/ a (float b))))))

;;; Cartesian helpers

(defsubst eas-geo-cartesian (lam phi)
  "Unit vector of spherical LAM PHI (radians)."
  (let ((c (cos phi))) (vector (* c (cos lam)) (* c (sin lam)) (sin phi))))

(defun eas-geo-spherical (v)
  "Spherical [lambda phi] of cartesian V."
  (vector (atan (aref v 1) (aref v 0)) (eas-geo-asin (aref v 2))))

(defsubst eas-geo-dot (a b) "Dot product of A and B."
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

(defsubst eas-geo-point-equal (a b)
  "Non-nil when points A and B coincide within epsilon."
  (and (< (abs (- (aref a 0) (aref b 0))) eas-geo-eps) (< (abs (- (aref a 1) (aref b 1))) eas-geo-eps)))

;;; Rotation (d3 rotation.js)

(defsubst eas-geo--wrap-lambda (lam)
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
      ;; The longitude shifts are made once, not per point.
      (let ((fwd (funcall lam dl)) (back (funcall lam (- dl))) (pf (car pg)) (pb (cdr pg)))
        (cons (lambda (l p) (let ((a (funcall fwd l p))) (funcall pf (aref a 0) (aref a 1))))
              (lambda (l p) (let ((a (funcall pb l p))) (funcall back (aref a 0) (aref a 1)))))))
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
  (let ((s (eas-geo-transformer sink #'ignore)))
    ;; One closure per point, not the transformer's two.
    (setf (eas-geo-stream-point s)
          (lambda (x y &optional _m)
            (let ((r (funcall rotate (* x eas-geo-rad) (* y eas-geo-rad))))
              (eas-geo-point sink (aref r 0) (aref r 1)))))
    s))

;;; Adaptive resampling (d3 resample.js)

(defconst eas-geo--max-depth 16 "Resampling depth limit.")
(defconst eas-geo--cos-min-distance (cos (* 30 eas-geo-rad)) "Resampling angular limit.")

(defun eas-geo-resample (project delta2)
  "D3's resample: PROJECT (L P -> [X Y]) at precision DELTA2.
Return a function SINK -> stream."
  (if (not (> delta2 0))
      (lambda (sink) (eas-geo-transformer sink (lambda (s x y _m) (let ((p (funcall project x y))) (eas-geo-point s (aref p 0) (aref p 1))))))
    (lambda (sink) (eas-geo--resample-stream project delta2 sink))))

(defmacro eas-geo--resample-far (delta4 depth x0 y0 x1 y1)
  "Non-nil when `eas-geo--resample-line' would subdivide X0 Y0 - X1 Y1.
DELTA4 is four times its DELTA2 and DEPTH what remains: the test it
starts with, made inline by the hot callers so a short segment costs
no call.  The arguments are variables, evaluated more than once, and
not named dx or dy."
  `(and (> ,depth 0)
        (let ((dx (- ,x1 ,x0)) (dy (- ,y1 ,y0))) (> (+ (* dx dx) (* dy dy)) ,delta4))))

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
          (let ((a (/ a m)) (b (/ b m)) (delta4 (* 4 delta2)))
            (when (eas-geo--resample-far delta4 depth x0 y0 x2 y2)
              (eas-geo--resample-line project delta2 x0 y0 l0 a0 b0 c0 x2 y2 l2 a b c depth sink))
            (eas-geo-point sink x2 y2)
            (when (eas-geo--resample-far delta4 depth x2 y2 x1 y1)
              (eas-geo--resample-line project delta2 x2 y2 l2 a b c x1 y1 l1 a1 b1 c1 depth sink))))))))

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

;;; Recorded spherical streams

;; What reaches the resampler depends on the geometry, the rotation and
;; the preclip only, never on the raw projection, its scale or its
;; translate.  A grid of maps (24 projections of one world) records it
;; once per rotation and clip and replays it into each projection's
;; resampler: the same calls with the same numbers, so the same paths.

(defun eas-geo-recorder ()
  "A sink recording what it receives as events for `eas-geo-replay'.
Its result is a vector of events in order, or nil when a line was left
open: the symbols `polygon-start', `polygon-end' and `sphere'; a point
\[L P]; a line (L . FLAT), FLAT holding [L P A B C] per point, A B C
its unit vector (`eas-geo-cartesian')."
  (let ((events nil) (line nil) (open nil))
    (eas-geo-stream--make
     :point (lambda (l p &optional _m)
              (if open (push (cons l p) line) (push (vector l p) events)))
     :line-start (lambda () (setq open t line nil))
     :line-end (lambda ()
                 (let* ((n (length line)) (flat (make-vector (* 5 n) 0.0)) (i (* 5 n)))
                   (dolist (pt line)
                     (let* ((l (car pt)) (p (cdr pt)) (c (cos p)))
                       (setq i (- i 5))
                       (aset flat i l) (aset flat (+ i 1) p)
                       (aset flat (+ i 2) (* c (cos l))) (aset flat (+ i 3) (* c (sin l)))
                       (aset flat (+ i 4) (sin p))))
                   (push (cons 'line flat) events)
                   (setq open nil line nil)))
     :polygon-start (lambda () (push 'polygon-start events))
     :polygon-end (lambda () (push 'polygon-end events))
     :sphere (lambda () (push 'sphere events))
     :result (lambda () (and (not open) (vconcat (nreverse events)))))))

(defun eas-geo-replay (events project delta2 sink)
  "Replay recorded EVENTS through d3's resampling of PROJECT into SINK.
The stream `eas-geo-resample' makes of PROJECT at DELTA2 (> 0), fed
EVENTS (`eas-geo-recorder'), in one loop: the same calls into SINK with
the same numbers, without a closure call per point."
  (let ((l00 0) (x00 0) (y00 0) (a00 0) (b00 0) (c00 0)
        (l0 0) (x0 eas-geo-nan) (y0 eas-geo-nan) (a0 0) (b0 0) (c0 0)
        (ring nil) (delta4 (* 4 delta2)) (depth eas-geo--max-depth))
    (dotimes (e (length events))
      (let ((ev (aref events e)))
        (cond
         ((consp ev)
          (let* ((flat (cdr ev)) (n (length flat)) (i 0))
            (setq x0 eas-geo-nan)
            (eas-geo--call line-start sink)
            (while (< i n)
              (let* ((l (aref flat i)) (a (aref flat (+ i 2))) (b (aref flat (+ i 3))) (c (aref flat (+ i 4)))
                     (pr (funcall project l (aref flat (1+ i)))) (x (aref pr 0)) (y (aref pr 1)))
                (when (eas-geo--resample-far delta4 depth x0 y0 x y)
                  (eas-geo--resample-line project delta2 x0 y0 l0 a0 b0 c0 x y l a b c depth sink))
                (setq x0 x y0 y l0 l a0 a b0 b c0 c)
                (eas-geo-point sink x0 y0)
                (when (and ring (= i 0))
                  (setq l00 l x00 x0 y00 y0 a00 a0 b00 b0 c00 c0)))
              (setq i (+ i 5)))
            (when (and ring (eas-geo--resample-far delta4 depth x0 y0 x00 y00))
              (eas-geo--resample-line project delta2 x0 y0 l0 a0 b0 c0 x00 y00 l00 a00 b00 c00
                                      depth sink))
            (eas-geo--call line-end sink)))
         ((vectorp ev)
          (let ((p (funcall project (aref ev 0) (aref ev 1)))) (eas-geo-point sink (aref p 0) (aref p 1))))
         ((eq ev 'polygon-start) (eas-geo--call polygon-start sink) (setq ring t))
         ((eq ev 'polygon-end) (eas-geo--call polygon-end sink) (setq ring nil))
         ((eq ev 'sphere) (eas-geo--call sphere sink)))))))

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

(defconst eas-geo--simplify-run 6
  "Points a simplified line drops in a row at most (`eas-geo-path-sink').")

(defun eas-geo-path-sink (radius &optional tolerance)
  "A sink collecting projected geometry; RADIUS is a Point's circle radius.
`eas-geo-stream-result' returns (:paths PATHS :circles CIRCLES :polygons N):
PATHS a list of (CLOSED . FLAT-XY-VECTOR) in stream order, CIRCLES a list
of [CX CY R].

With TOLERANCE (pixels), a line drops a point B lying closer than it to
the segment from the last point kept to the point after B, at most
`eas-geo--simplify-run' in a row; its ends stay.  A ring left with
fewer than four points keeps them all when it had under 16."
  (let* ((paths nil) (circles nil) (polygon nil) (line nil) (pts nil) (npoly 0)
         (tol2 (and tolerance (> tolerance 0) (* tolerance tolerance)))
         (ax nil) (ay 0.0) (bx nil) (by 0.0) (run 0) (all nil) (nall 0)
         (s (eas-geo-stream--make)))
    (setf (eas-geo-stream-point s)
          (if tol2
              (lambda (x y &optional _m)
                (cond ((not line) (push (vector x y radius) circles))
                      (t
                       (when (< nall 16) (push x all) (push y all) (setq nall (1+ nall)))
                       (cond
                        ((null ax) (setcar pts (cons y (cons x (car pts)))) (setq ax x ay y))
                        ((null bx) (setq bx x by y))
                        ((and (< run eas-geo--simplify-run)
                              (let* ((dx (- x ax)) (dy (- y ay)) (ex (- bx ax)) (ey (- by ay))
                                     (z (- (* dx ey) (* dy ex))) (d2 (+ (* dx dx) (* dy dy))))
                                (if (> d2 0) (< (* z z) (* tol2 d2)) (< (+ (* ex ex) (* ey ey)) tol2))))
                         (setq bx x by y run (1+ run)))
                        (t (setcar pts (cons by (cons bx (car pts))))
                           (setq ax bx ay by bx x by y run 0))))))
            (lambda (x y &optional _m)
              (if line (setcar pts (cons y (cons x (car pts)))) (push (vector x y radius) circles))))
          (eas-geo-stream-line-start s)
          (lambda () (setq line t ax nil bx nil run 0 all nil nall 0) (push nil pts))
          (eas-geo-stream-line-end s)
          (lambda ()
            (setq line nil)
            (when bx (setcar pts (cons by (cons bx (car pts)))))
            (let ((flat (vconcat (nreverse (pop pts)))))
              (when (and tol2 polygon (< (length flat) 8) (< nall 16) (> (length all) (length flat)))
                (setq flat (vconcat (nreverse all))))
              (setq all nil)
              (when (> (length flat) 0) (push (cons polygon flat) paths))))
          (eas-geo-stream-polygon-start s) (lambda () (setq polygon t))
          (eas-geo-stream-polygon-end s) (lambda () (setq polygon nil npoly (1+ npoly)))
          (eas-geo-stream-result s)
          (lambda () (list :paths (nreverse paths) :circles (nreverse circles) :polygons npoly)))
    s))

;;; Planar measures of a path (d3 path/area.js, path/centroid.js)

(defun eas-geo-ring-area (flat)
  "Signed shoelace area of closed ring FLAT [x0 y0 x1 y1 ...], as d3 sums it."
  (let ((m (length flat)) (sum 0.0) (i 0))
    (when (> m 1)
      (let ((x0 (aref flat 0)) (y0 (aref flat 1)) x1 y1)
        (while (< i m)
          (setq i (+ i 2))
          (if (< i m) (setq x1 (aref flat i) y1 (aref flat (1+ i))) (setq x1 (aref flat 0) y1 (aref flat 1)))
          (setq sum (+ sum (- (* x0 y1) (* y0 x1))) x0 x1 y0 y1))))
    (/ sum 2)))

(defun eas-geo-path-area (result)
  "Planar area of a path RESULT (`eas-geo-path-sink'), d3's path.area.
Each closed ring's signed area summed, then made positive."
  (abs (apply #'+ 0.0 (mapcar (lambda (p) (if (car p) (eas-geo-ring-area (cdr p)) 0.0))
                              (plist-get result :paths)))))

(defun eas-geo-path-centroid (result)
  "Planar centroid [X Y] of a path RESULT, d3's path.centroid; nil if empty.
Rings weigh by area, else lines by length, else points count."
  (or (eas-geo--ring-centroid result) (eas-geo--path-centroid-1 result)))

(defun eas-geo--ring-centroid (result)
  "Area-weighted centroid [X Y] of path RESULT's rings, or nil.
The sums `eas-geo--path-centroid-1' makes for rings, in its order, so a
map's shapes skip measuring their lines (eas-b2s.8)."
  (let ((x2 0.0) (y2 0.0) (z2 0.0))
    (dolist (p (plist-get result :paths))
      (when (car p)
        (let* ((flat (cdr p)) (m (length flat)) (i 0))
          (when (> m 1)
            (let ((ax (aref flat 0)) (ay (aref flat 1)) bx by)
              (while (< i m)
                (setq i (+ i 2))
                (if (< i m) (setq bx (aref flat i) by (aref flat (1+ i))) (setq bx (aref flat 0) by (aref flat 1)))
                (let ((z (- (* ay bx) (* ax by))))
                  (setq x2 (+ x2 (* z (+ ax bx))) y2 (+ y2 (* z (+ ay by))) z2 (+ z2 (* 3 z)) ax bx ay by))))))))
    (and (/= z2 0) (vector (/ x2 z2) (/ y2 z2)))))

(defun eas-geo--path-centroid-1 (result)
  "Do `eas-geo-path-centroid' of RESULT, every sum."
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

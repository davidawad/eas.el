;;; eas-geo-clip.el --- d3-geo clipping: antimeridian, small circle, rectangle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (eas-geo-stream.el's pipeline).  A port of d3-geo's
;; clipping: the generic polygon clip with its rejoin of cut rings
;; along the clip edge (clip/index.js, clip/rejoin.js), the spherical
;; point-in-polygon test that decides whether a polygon holds the clip
;; region's start point (polygonContains.js), and the three clips
;; Vega's projections use:
;;
;;   antimeridian   cuts lines crossing longitude +-180 (most projections)
;;   circle         keeps a small circle around the centre (clipAngle;
;;                  orthographic, gnomonic, stereographic, azimuthals)
;;   rectangle      the planar postclip (clipExtent; mercator's own)
;;
;; Points travel as vectors [X Y FLAG], FLAG marking clip intersections.

;;; Code:

(require 'cl-lib)
(require 'eas-geo-stream)

;;; Buffer and rejoin

(defun eas-geo-clip-buffer ()
  "D3's clip buffer: collects lines of [X Y M] points."
  (let ((lines nil) (line nil))
    (eas-geo-stream--make
     :point (lambda (x y &optional m) (setcar line (cons (vector x y m) (car line))))
     :line-start (lambda () (setq line (list nil)) (push line lines))
     :rejoin (lambda ()
               (when (cdr lines)
                 ;; lines is newest first: last line = (car lines), first = (car (last lines)).
                 (let* ((first (car (last lines))) (lastl (car lines)))
                   (setcar lastl (append (car first) (car lastl)))
                   (setq lines (butlast lines)))))
     :result (lambda () (prog1 (nreverse (mapcar (lambda (l) (vconcat (reverse (car l)))) lines))
                          (setq lines nil line nil))))))

(cl-defstruct (eas-geo--ix (:constructor eas-geo--ix-make (x z o e)) (:copier nil) (:predicate nil))
  "A clip intersection: point X, segment Z, other O, entry E, then visited, next, previous."
  x z o e (visited nil) (next nil) (prev nil))

(defun eas-geo--link (items)
  "Link vector ITEMS into a ring."
  (let ((n (length items)))
    (when (> n 0)
      (dotimes (i n)
        (let ((a (aref items i)) (b (aref items (mod (1+ i) n))))
          (setf (eas-geo--ix-next a) b (eas-geo--ix-prev b) a))))))

(defun eas-geo-clip-rejoin (segments compare start-inside interpolate sink)
  "D3's clipRejoin of SEGMENTS (vectors of points) into SINK.
COMPARE orders intersections along the clip edge; START-INSIDE says
whether the clip region's start lies inside; INTERPOLATE (FROM TO DIR
SINK) walks the clip edge."
  (let (subject clip)
    (dolist (seg segments)
      (let ((n (1- (length seg))))
        (when (> n 0)
          (let ((p0 (aref seg 0)) (p1 (aref seg n)))
            (if (and (eas-geo-point-equal p0 p1) (not (aref p0 2)) (not (aref p1 2)))
                (progn (eas-geo--call line-start sink)
                       (dotimes (i n) (eas-geo-point sink (aref (aref seg i) 0) (aref (aref seg i) 1)))
                       (eas-geo--call line-end sink))
              (when (eas-geo-point-equal p0 p1)
                ;; handle degenerate cases by moving the point
                (aset p1 0 (+ (aref p1 0) (* 2 eas-geo-eps))))
              (let ((x (eas-geo--ix-make p0 seg nil t)))
                (push x subject)
                (push (setf (eas-geo--ix-o x) (eas-geo--ix-make p0 nil x nil)) clip))
              (let ((x (eas-geo--ix-make p1 seg nil nil)))
                (push x subject)
                (push (setf (eas-geo--ix-o x) (eas-geo--ix-make p1 nil x t)) clip)))))))
    (when subject
      (let ((subject (vconcat (nreverse subject)))
            (clip (vconcat (sort (nreverse clip) (lambda (a b) (< (funcall compare a b) 0))))))
        (eas-geo--link subject)
        (eas-geo--link clip)
        (cl-loop for c across clip do (setf (eas-geo--ix-e c) (setq start-inside (not start-inside))))
        (let ((start (aref subject 0)) (done nil))
          (while (not done)
            (let ((current start) (subj t) points)
              (while (and (not done) (eas-geo--ix-visited current))
                (setq current (eas-geo--ix-next current))
                (when (eq current start) (setq done t)))
              (unless done
                (setq points (eas-geo--ix-z current))
                (eas-geo--call line-start sink)
                (cl-loop
                 do (setf (eas-geo--ix-visited current) t (eas-geo--ix-visited (eas-geo--ix-o current)) t)
                 (if (eas-geo--ix-e current)
                     (progn
                       (if subj
                           (seq-doseq (pt points) (eas-geo-point sink (aref pt 0) (aref pt 1)))
                         (funcall interpolate (eas-geo--ix-x current) (eas-geo--ix-x (eas-geo--ix-next current)) 1 sink))
                       (setq current (eas-geo--ix-next current)))
                   (if subj
                       (let ((pts (eas-geo--ix-z (eas-geo--ix-prev current))))
                         (cl-loop for i downfrom (1- (length pts)) to 0
                                  do (eas-geo-point sink (aref (aref pts i) 0) (aref (aref pts i) 1))))
                     (funcall interpolate (eas-geo--ix-x current) (eas-geo--ix-x (eas-geo--ix-prev current)) -1 sink))
                   (setq current (eas-geo--ix-prev current)))
                 (setq current (eas-geo--ix-o current)
                       points (eas-geo--ix-z current)
                       subj (not subj))
                 until (eas-geo--ix-visited current))
                (eas-geo--call line-end sink)))))))))

;;; Spherical point in polygon (d3 polygonContains.js)

(defsubst eas-geo--longitude (pt)
  "Longitude of PT wrapped as d3 does."
  (let ((l (aref pt 0)))
    (if (<= (abs l) float-pi) l
      (* (eas-geo-sign l) (- (eas-geo-rem (+ (abs l) float-pi) eas-geo-tau) float-pi)))))

(defun eas-geo-polygon-contains (polygon point)
  "Return non-nil if POINT is inside spherical POLYGON.
POLYGON is a list of rings of [L P] vectors (radians)."
  (let* ((lam (eas-geo--longitude point)) (phi (aref point 1)) (sin-phi (sin phi))
         (normal (vector (sin lam) (- (cos lam)) 0))
         (angle 0.0) (winding 0) (sum 0.0))
    (cond ((= sin-phi 1) (setq phi (+ eas-geo-half-pi eas-geo-eps)))
          ((= sin-phi -1) (setq phi (- (- eas-geo-half-pi) eas-geo-eps))))
    (dolist (ring polygon)
      (let ((m (length ring)))
        (when (> m 0)
          (let* ((p0 (aref ring (1- m))) (l0 (eas-geo--longitude p0))
                 (ph0 (+ (/ (aref p0 1) 2) eas-geo--quarter-pi)) (s0 (sin ph0)) (c0 (cos ph0)))
            (dotimes (j m)
              (let* ((p1 (aref ring j)) (l1 (eas-geo--longitude p1))
                     (ph1 (+ (/ (aref p1 1) 2) eas-geo--quarter-pi)) (s1 (sin ph1)) (c1 (cos ph1))
                     (delta (- l1 l0)) (sign (if (>= delta 0) 1 -1)) (abs-delta (* sign delta))
                     (anti (> abs-delta float-pi)) (k (* s0 s1)))
                (setq sum (+ sum (atan (* k sign (sin abs-delta)) (+ (* c0 c1) (* k (cos abs-delta))))))
                (setq angle (+ angle (if anti (+ delta (* sign eas-geo-tau)) delta)))
                (when (not (eq (not (eq anti (>= l0 lam))) (>= l1 lam)))
                  (let* ((arc (eas-geo-normalize (eas-geo-cross (eas-geo-cartesian (aref p0 0) (aref p0 1))
                                                                (eas-geo-cartesian (aref p1 0) (aref p1 1)))))
                         (ix (eas-geo-normalize (eas-geo-cross normal arc)))
                         (flip (not (eq anti (>= delta 0))))
                         (phi-arc (* (if flip -1 1) (eas-geo-asin (aref ix 2)))))
                    (when (or (> phi phi-arc) (and (= phi phi-arc) (or (/= (aref arc 0) 0) (/= (aref arc 1) 0))))
                      (setq winding (+ winding (if flip 1 -1))))))
                (setq l0 l1 s0 s1 c0 c1 p0 p1)))))))
    (not (eq (or (< angle (- eas-geo-eps)) (and (< angle eas-geo-eps) (< sum (- eas-geo-eps2))))
             (= (logand winding 1) 1)))))

;;; The generic polygon clip (d3 clip/index.js)

(defun eas-geo--compare-intersection (a b)
  "D3's ordering of spherical clip intersections A and B along the edge."
  (let ((a (eas-geo--ix-x a)) (b (eas-geo--ix-x b)))
    (- (if (< (aref a 0) 0) (- (aref a 1) eas-geo-half-pi eas-geo-eps) (- eas-geo-half-pi (aref a 1)))
       (if (< (aref b 0) 0) (- (aref b 1) eas-geo-half-pi eas-geo-eps) (- eas-geo-half-pi (aref b 1))))))

(defun eas-geo-clip (visible clip-line interpolate start)
  "D3's clip: a function SINK -> stream.
VISIBLE (L P) tests points, CLIP-LINE (SINK) cuts lines, INTERPOLATE
walks the clip edge and START is a point of the clip region's edge."
  (lambda (sink)
    (let* ((line (funcall clip-line sink))
           (ring-buffer (eas-geo-clip-buffer))
           (ring-sink (funcall clip-line ring-buffer))
           (started nil) polygon segments ring
           (s (eas-geo-stream--make)))
      (cl-labels ((point (l p &optional _m) (when (funcall visible l p) (eas-geo-point sink l p)))
                  (point-line (l p &optional _m) (eas-geo-point line l p))
                  (line-start () (setf (eas-geo-stream-point s) #'point-line) (eas-geo--call line-start line))
                  (line-end () (setf (eas-geo-stream-point s) #'point) (eas-geo--call line-end line))
                  (point-ring (l p &optional _m) (push (vector l p) ring) (eas-geo-point ring-sink l p))
                  (ring-start () (eas-geo--call line-start ring-sink) (setq ring nil))
                  (start-polygon () (unless started (eas-geo--call polygon-start sink) (setq started t)))
                  (ring-end ()
                    (let ((first (car (last ring))))
                      (point-ring (aref first 0) (aref first 1)))
                    (eas-geo--call line-end ring-sink)
                    (let* ((clean (funcall (eas-geo-stream-clean ring-sink)))
                           (segs (funcall (eas-geo-stream-result ring-buffer)))
                           (n (length segs)))
                      (pop ring)
                      (push (vconcat (nreverse ring)) polygon)
                      (setq ring nil)
                      (when (> n 0)
                        (if (= (logand clean 1) 1)
                            (let* ((seg (car segs)) (m (1- (length seg))))
                              (when (> m 0)
                                (start-polygon)
                                (eas-geo--call line-start sink)
                                (dotimes (i m) (eas-geo-point sink (aref (aref seg i) 0) (aref (aref seg i) 1)))
                                (eas-geo--call line-end sink)))
                          (when (and (> n 1) (= (logand clean 2) 2))
                            (setq segs (append (butlast (cdr segs))
                                               (list (vconcat (car (last segs)) (car segs))))))
                          (push (seq-filter (lambda (sg) (> (length sg) 1)) segs) segments))))))
        (setf (eas-geo-stream-point s) #'point
              (eas-geo-stream-line-start s) #'line-start
              (eas-geo-stream-line-end s) #'line-end
              (eas-geo-stream-polygon-start s)
              (lambda ()
                (setf (eas-geo-stream-point s) #'point-ring (eas-geo-stream-line-start s) #'ring-start
                      (eas-geo-stream-line-end s) #'ring-end)
                (setq segments nil polygon nil))
              (eas-geo-stream-polygon-end s)
              (lambda ()
                (setf (eas-geo-stream-point s) #'point (eas-geo-stream-line-start s) #'line-start
                      (eas-geo-stream-line-end s) #'line-end)
                (let ((segs (apply #'append (nreverse segments)))
                      (inside (eas-geo-polygon-contains (nreverse polygon) start)))
                  (cond (segs (start-polygon)
                              (eas-geo-clip-rejoin segs #'eas-geo--compare-intersection inside interpolate sink))
                        (inside (start-polygon)
                                (eas-geo--call line-start sink)
                                (funcall interpolate nil nil 1 sink)
                                (eas-geo--call line-end sink)))
                  (when started (eas-geo--call polygon-end sink) (setq started nil))
                  (setq segments nil polygon nil)))
              (eas-geo-stream-sphere s)
              (lambda ()
                (eas-geo--call polygon-start sink)
                (eas-geo--call line-start sink)
                (funcall interpolate nil nil 1 sink)
                (eas-geo--call line-end sink)
                (eas-geo--call polygon-end sink)))
        s))))

;;; Antimeridian (d3 clip/antimeridian.js)

(defun eas-geo--antimeridian-intersect (l0 p0 l1 p1)
  "Latitude where the arc L0 P0 - L1 P1 crosses the antimeridian."
  (let ((s (sin (- l0 l1))))
    (if (> (abs s) eas-geo-eps)
        (let ((c0 (cos p0)) (c1 (cos p1)))
          (atan (/ (- (* (sin p0) c1 (sin l1)) (* (sin p1) c0 (sin l0))) (* c0 c1 s))))
      (/ (+ p0 p1) 2))))

(defun eas-geo--antimeridian-line (sink)
  "The antimeridian line cutter into SINK."
  (let ((l0 eas-geo-nan) (p0 eas-geo-nan) (sign0 eas-geo-nan) (clean 1))
    (eas-geo-stream--make
     :line-start (lambda () (eas-geo--call line-start sink) (setq clean 1))
     :point (lambda (l1 p1 &optional _m)
              (let ((sign1 (if (> l1 0) float-pi (- float-pi)))
                    (delta (abs (- l1 l0))))
                (cond
                 ((< (abs (- delta float-pi)) eas-geo-eps) ; crosses a pole
                  (setq p0 (if (> (/ (+ p0 p1) 2) 0) eas-geo-half-pi (- eas-geo-half-pi)))
                  (eas-geo-point sink l0 p0)
                  (eas-geo-point sink sign0 p0)
                  (eas-geo--call line-end sink)
                  (eas-geo--call line-start sink)
                  (eas-geo-point sink sign1 p0)
                  (eas-geo-point sink l1 p0)
                  (setq clean 0))
                 ((and (not (eql sign0 sign1)) (>= delta float-pi)) ; crosses the antimeridian
                  (when (< (abs (- l0 sign0)) eas-geo-eps) (setq l0 (- l0 (* sign0 eas-geo-eps))))
                  (when (< (abs (- l1 sign1)) eas-geo-eps) (setq l1 (- l1 (* sign1 eas-geo-eps))))
                  (setq p0 (eas-geo--antimeridian-intersect l0 p0 l1 p1))
                  (eas-geo-point sink sign0 p0)
                  (eas-geo--call line-end sink)
                  (eas-geo--call line-start sink)
                  (eas-geo-point sink sign1 p0)
                  (setq clean 0)))
                (eas-geo-point sink (setq l0 l1) (setq p0 p1))
                (setq sign0 sign1)))
     :line-end (lambda () (eas-geo--call line-end sink) (setq l0 eas-geo-nan p0 eas-geo-nan))
     :clean (lambda () (- 2 clean)))))

(defun eas-geo--antimeridian-interpolate (from to direction sink)
  "Walk the antimeridian clip edge FROM TO in DIRECTION into SINK."
  (cond
   ((null from)
    (let ((phi (* direction eas-geo-half-pi)) (pi float-pi))
      (dolist (pt (list (vector (- pi) phi) (vector 0 phi) (vector pi phi) (vector pi 0)
                        (vector pi (- phi)) (vector 0 (- phi)) (vector (- pi) (- phi))
                        (vector (- pi) 0) (vector (- pi) phi)))
        (eas-geo-point sink (aref pt 0) (aref pt 1)))))
   ((> (abs (- (aref from 0) (aref to 0))) eas-geo-eps)
    (let* ((lam (if (< (aref from 0) (aref to 0)) float-pi (- float-pi)))
           (phi (/ (* direction lam) 2)))
      (eas-geo-point sink (- lam) phi)
      (eas-geo-point sink 0 phi)
      (eas-geo-point sink lam phi)))
   (t (eas-geo-point sink (aref to 0) (aref to 1)))))

(defun eas-geo-clip-antimeridian ()
  "D3's clipAntimeridian, a function SINK -> stream."
  (eas-geo-clip (lambda (_l _p) t) #'eas-geo--antimeridian-line #'eas-geo--antimeridian-interpolate
                (vector (- float-pi) (- eas-geo-half-pi))))

;;; Small circle (d3 clip/circle.js, circle.js)

(defun eas-geo--circle-radius (cos-radius point)
  "Signed angle of POINT relative to [COS-RADIUS 0 0]."
  (let* ((p (eas-geo-cartesian (aref point 0) (aref point 1))))
    (aset p 0 (- (aref p 0) cos-radius))
    (setq p (eas-geo-normalize p))
    (let ((r (eas-geo-acos (- (aref p 1)))))
      (eas-geo-rem (- (+ (if (< (- (aref p 2)) 0) (- r) r) eas-geo-tau) eas-geo-eps) eas-geo-tau))))

(defun eas-geo-circle-stream (sink radius delta direction t0 t1)
  "D3's circleStream: the clip circle of RADIUS from T0 to T1 into SINK.
DELTA is the angular step, DIRECTION its sign; T0 nil means whole."
  (let* ((cr (cos radius)) (sr (sin radius)) (step (* direction delta)))
    (if (null t0)
        (setq t0 (+ radius (* direction eas-geo-tau)) t1 (- radius (/ step 2)))
      (setq t0 (eas-geo--circle-radius cr t0) t1 (eas-geo--circle-radius cr t1))
      (when (if (> direction 0) (< t0 t1) (> t0 t1)) (setq t0 (+ t0 (* direction eas-geo-tau)))))
    (let ((tt t0))
      (while (if (> direction 0) (> tt t1) (< tt t1))
        (let ((pt (eas-geo-spherical (vector cr (* (- sr) (cos tt)) (* (- sr) (sin tt))))))
          (eas-geo-point sink (aref pt 0) (aref pt 1)))
        (setq tt (- tt step))))))

(defun eas-geo-clip-circle (radius)
  "D3's clipCircle of RADIUS (radians), a function SINK -> stream."
  (let* ((cr (cos radius)) (delta (* 2 eas-geo-rad))
         (small (> cr 0)) (not-hemisphere (> (abs cr) eas-geo-eps)))
    (cl-labels
        ((visible (l p) (> (* (cos l) (cos p)) cr))
         (code (l p)
           (let ((r (if small radius (- float-pi radius))) (c 0))
             (cond ((< l (- r)) (setq c (logior c 1))) ((> l r) (setq c (logior c 2))))
             (cond ((< p (- r)) (setq c (logior c 4))) ((> p r) (setq c (logior c 8))))
             c))
         (intersect (a b &optional two)
           (let* ((pa (eas-geo-cartesian (aref a 0) (aref a 1)))
                  (pb (eas-geo-cartesian (aref b 0) (aref b 1)))
                  (n1 (vector 1 0 0)) (n2 (eas-geo-cross pa pb))
                  (n2n2 (eas-geo-dot n2 n2)) (n1n2 (aref n2 0))
                  (det (- n2n2 (* n1n2 n1n2))))
             (if (= det 0) (and (not two) a)
               (let* ((c1 (/ (* cr n2n2) det)) (c2 (/ (* (- cr) n1n2) det))
                      (u (eas-geo-cross n1 n2))
                      (A (eas-geo-add3 (eas-geo-scale3 n1 c1) (eas-geo-scale3 n2 c2)))
                      (w (eas-geo-dot A u)) (uu (eas-geo-dot u u))
                      (t2 (- (* w w) (* uu (- (eas-geo-dot A A) 1)))))
                 (unless (< t2 0)
                   (let* ((tt (sqrt t2))
                          (q (eas-geo-spherical (eas-geo-add3 (eas-geo-scale3 u (/ (- (- w) tt) uu)) A))))
                     (if (not two) q
                       (let ((l0 (aref a 0)) (l1 (aref b 0)) (p0 (aref a 1)) (p1 (aref b 1)))
                         (when (< l1 l0) (cl-rotatef l0 l1))
                         (let* ((d (- l1 l0)) (polar (< (abs (- d float-pi)) eas-geo-eps))
                                (meridian (or polar (< d eas-geo-eps))))
                           (when (and (not polar) (< p1 p0)) (cl-rotatef p0 p1))
                           (when (if meridian
                                     (if polar
                                         (not (eq (> (+ p0 p1) 0)
                                                  (< (aref q 1) (if (< (abs (- (aref q 0) l0)) eas-geo-eps) p0 p1))))
                                       (and (<= p0 (aref q 1)) (<= (aref q 1) p1)))
                                   (not (eq (> d float-pi) (and (<= l0 (aref q 0)) (<= (aref q 0) l1)))))
                             (list q (eas-geo-spherical (eas-geo-add3 (eas-geo-scale3 u (/ (+ (- w) tt) uu)) A)))))))))))))
         (interpolate (from to direction sink)
           (eas-geo-circle-stream sink radius delta direction from to))
         (clip-line (sink)
           (let (point0 c0 v0 v00 (clean 1))
             (eas-geo-stream--make
              :line-start (lambda () (setq v00 nil v0 nil clean 1))
              :point
              (lambda (l p &optional _m)
                (let* ((point1 (vector l p nil)) point2
                       (v (visible l p))
                       (c (if small (if v 0 (code l p))
                            (if v (code (+ l (if (< l 0) float-pi (- float-pi))) p) 0))))
                  (when (and (null point0) (setq v00 (setq v0 v))) (eas-geo--call line-start sink))
                  (unless (eq v v0)
                    (setq point2 (intersect point0 point1))
                    (when (or (null point2) (eas-geo-point-equal point0 point2) (eas-geo-point-equal point1 point2))
                      (aset point1 2 1)))
                  (cond
                   ((not (eq v v0))
                    (setq clean 0)
                    (if v
                        (progn (eas-geo--call line-start sink)
                               (setq point2 (intersect point1 point0))
                               (eas-geo-point sink (aref point2 0) (aref point2 1)))
                      (setq point2 (intersect point0 point1))
                      (eas-geo-point sink (aref point2 0) (aref point2 1) 2)
                      (eas-geo--call line-end sink))
                    (setq point0 point2))
                   ((and not-hemisphere point0 (not (eq small v)))
                    (let ((tt (and (= (logand c c0) 0) (intersect point1 point0 t))))
                      (when tt
                        (setq clean 0)
                        (if small
                            (progn (eas-geo--call line-start sink)
                                   (eas-geo-point sink (aref (car tt) 0) (aref (car tt) 1))
                                   (eas-geo-point sink (aref (cadr tt) 0) (aref (cadr tt) 1))
                                   (eas-geo--call line-end sink))
                          (eas-geo-point sink (aref (cadr tt) 0) (aref (cadr tt) 1))
                          (eas-geo--call line-end sink)
                          (eas-geo--call line-start sink)
                          (eas-geo-point sink (aref (car tt) 0) (aref (car tt) 1) 3))))))
                  (when (and v (or (null point0) (not (eas-geo-point-equal point0 point1))))
                    (eas-geo-point sink (aref point1 0) (aref point1 1)))
                  (setq point0 point1 v0 v c0 c)))
              :line-end (lambda () (when v0 (eas-geo--call line-end sink)) (setq point0 nil))
              :clean (lambda () (logior clean (if (and v00 v0) 2 0)))))))
      (eas-geo-clip #'visible #'clip-line #'interpolate
                    (if small (vector 0 (- radius)) (vector (- float-pi) (- radius float-pi)))))))

;;; Rectangle (d3 clip/rectangle.js, clip/line.js)

(defun eas-geo--clip-segment (a b x0 y0 x1 y1)
  "Clip segment A B (mutable [X Y]) to the box X0 Y0 X1 Y1 (Liang-Barsky).
Return nil when the segment lies outside."
  (let* ((ax (float (aref a 0))) (ay (float (aref a 1))) (dx (- (aref b 0) ax)) (dy (- (aref b 1) ay))
         (t0 0.0) (t1 1.0))
    (catch 'out
      (cl-flet ((edge (r d lower)
                  ;; lower: the edge bounds from below (x0/y0)
                  (if (= d 0)
                      (when (if lower (> r 0) (< r 0)) (throw 'out nil))
                    (let ((r (/ r d)))
                      (if (eq lower (< d 0))
                          (progn (when (< r t0) (throw 'out nil)) (when (< r t1) (setq t1 r)))
                        (when (> r t1) (throw 'out nil)) (when (> r t0) (setq t0 r)))))))
        (edge (- x0 ax) dx t)
        (edge (- x1 ax) dx nil)
        (edge (- y0 ay) dy t)
        (edge (- y1 ay) dy nil))
      (when (> t0 0) (aset a 0 (+ ax (* t0 dx))) (aset a 1 (+ ay (* t0 dy))))
      (when (< t1 1) (aset b 0 (+ ax (* t1 dx))) (aset b 1 (+ ay (* t1 dy))))
      t)))

(defun eas-geo-clip-rectangle (x0 y0 x1 y1)
  "D3's clipRectangle to X0 Y0 X1 Y1, a function SINK -> stream."
  (cl-labels
      ((visible (x y) (and (<= x0 x) (<= x x1) (<= y0 y) (<= y y1)))
       (corner (p direction)
         (cond ((< (abs (- (aref p 0) x0)) eas-geo-eps) (if (> direction 0) 0 3))
               ((< (abs (- (aref p 0) x1)) eas-geo-eps) (if (> direction 0) 2 1))
               ((< (abs (- (aref p 1) y0)) eas-geo-eps) (if (> direction 0) 1 0))
               (t (if (> direction 0) 3 2))))
       (compare-point (a b)
         (let ((ca (corner a 1)) (cb (corner b 1)))
           (cond ((/= ca cb) (- ca cb))
                 ((= ca 0) (- (aref b 1) (aref a 1)))
                 ((= ca 1) (- (aref a 0) (aref b 0)))
                 ((= ca 2) (- (aref a 1) (aref b 1)))
                 (t (- (aref b 0) (aref a 0))))))
       (interpolate (from to direction sink)
         (let ((a 0) (a1 0))
           (if (or (null from)
                   (/= (setq a (corner from direction)) (setq a1 (corner to direction)))
                   (not (eq (< (compare-point from to) 0) (> direction 0))))
               (cl-loop do (eas-geo-point sink (if (or (= a 0) (= a 3)) x0 x1) (if (> a 1) y1 y0))
                        (setq a (mod (+ a direction 4) 4))
                        until (= a a1))
             (eas-geo-point sink (aref to 0) (aref to 1))))))
    (lambda (sink)
      (let* ((active sink) (buffer (eas-geo-clip-buffer))
             segments polygon in-polygon x__ y__ v__ (x_ 0) (y_ 0) v_ first (clean t)
             (s (eas-geo-stream--make)))
        (cl-labels
            ((point (x y &optional _m) (when (visible x y) (eas-geo-point active x y)))
             (polygon-inside ()
               (let ((winding 0))
                 (dolist (r polygon)
                   (let* ((pts (vconcat (reverse r))) (m (length pts)))
                     (when (> m 0)
                       (let ((b0 (aref (aref pts 0) 0)) (b1 (aref (aref pts 0) 1)))
                         (cl-loop for j from 1 below m
                                  do (let ((a0 b0) (a1 b1))
                                       (setq b0 (aref (aref pts j) 0) b1 (aref (aref pts j) 1))
                                       (if (<= a1 y1)
                                           (when (and (> b1 y1) (> (* (- b0 a0) (- y1 a1)) (* (- b1 a1) (- x0 a0))))
                                             (cl-incf winding))
                                         (when (and (<= b1 y1) (< (* (- b0 a0) (- y1 a1)) (* (- b1 a1) (- x0 a0))))
                                           (cl-decf winding)))))))))
                 winding))
             (line-point (x y &optional _m)
               (let ((v (visible x y)))
                 (when in-polygon (setcar polygon (cons (vector x y) (car polygon))))
                 (if first
                     (progn (setq x__ x y__ y v__ v first nil)
                            (when v (eas-geo--call line-start active) (eas-geo-point active x y)))
                   (if (and v v_) (eas-geo-point active x y)
                     (let ((a (vector (max -1e9 (min 1e9 x_)) (max -1e9 (min 1e9 y_))))
                           (b (vector (max -1e9 (min 1e9 x)) (max -1e9 (min 1e9 y)))))
                       (setq x_ (aref a 0) y_ (aref a 1) x (aref b 0) y (aref b 1))
                       (cond
                        ((eas-geo--clip-segment a b x0 y0 x1 y1)
                         (unless v_ (eas-geo--call line-start active) (eas-geo-point active (aref a 0) (aref a 1)))
                         (eas-geo-point active (aref b 0) (aref b 1))
                         (unless v (eas-geo--call line-end active))
                         (setq clean nil))
                        (v (eas-geo--call line-start active) (eas-geo-point active x y) (setq clean nil))))))
                 (setq x_ x y_ y v_ v)))
             (line-start ()
               (setf (eas-geo-stream-point s) #'line-point)
               (when in-polygon (push nil polygon))
               (setq first t v_ nil x_ eas-geo-nan y_ eas-geo-nan))
             (line-end ()
               (when segments
                 (line-point x__ y__)
                 (when (and v__ v_) (funcall (eas-geo-stream-rejoin buffer)))
                 (setcar segments (append (car segments) (funcall (eas-geo-stream-result buffer)))))
               (setf (eas-geo-stream-point s) #'point)
               (when v_ (eas-geo--call line-end active))))
          (setf (eas-geo-stream-point s) #'point
                (eas-geo-stream-line-start s) #'line-start
                (eas-geo-stream-line-end s) #'line-end
                (eas-geo-stream-polygon-start s)
                ;; polygon holds each ring's points newest first.
                (lambda () (setq active buffer segments (list nil) polygon nil in-polygon t clean t))
                (eas-geo-stream-polygon-end s)
                (lambda ()
                  (let* ((start-inside (/= (polygon-inside) 0))
                         (clean-inside (and clean start-inside))
                         (segs (car segments)))
                    (when (or clean-inside segs)
                      (eas-geo--call polygon-start sink)
                      (when clean-inside
                        (eas-geo--call line-start sink)
                        (interpolate nil nil 1 sink)
                        (eas-geo--call line-end sink))
                      (when segs
                        (eas-geo-clip-rejoin segs (lambda (a b) (compare-point (eas-geo--ix-x a) (eas-geo--ix-x b)))
                                             start-inside #'interpolate sink))
                      (eas-geo--call polygon-end sink))
                    (setq active sink segments nil polygon nil in-polygon nil)))
                (eas-geo-stream-sphere s) (lambda () (eas-geo--call sphere sink)))
          s)))))

(provide 'eas-geo-clip)
;;; eas-geo-clip.el ends here

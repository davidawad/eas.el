;;; eas-geo-proj.el --- d3 projections: scale, translate, rotate, clip, fit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  `eas-geo-proj' builds a projection from a Vega-Lite
;; projection object whose values are plain (expressions already
;; evaluated), as d3's projectionMutator does: the raw projection of
;; eas-geo-raw.el, centred on "center", rotated by "rotate" (three
;; axes), scaled and translated, reflected; preclipped to the
;; antimeridian or to "clipAngle"; resampled at "precision"; and
;; postclipped to "clipExtent" (mercator and transverseMercator clip
;; to the world's square by themselves).  albersUsa is d3's composite of
;; three conics, each clipped to its inset.
;;
;; A projection is a plist: :stream (SINK -> stream of degrees),
;; :point (LON LAT -> [X Y] or nil, unclipped as d3's projection(point))
;; and :type.  `eas-geo-proj-fit' is d3's fitExtent: scale and translate
;; that fit GeoJSON objects into a box.

;;; Code:

(require 'cl-lib)
(require 'eas-geo-stream)
(require 'eas-geo-clip)
(require 'eas-geo-raw)
(require 'eas-geo-native)

(defun eas-geo-proj--num (v default)
  "V when a number, else DEFAULT."
  (if (numberp v) v default))

(defun eas-geo-proj--vec (v default)
  "V as a list of numbers when a non-empty array, else DEFAULT (a vector)."
  (append (if (and (vectorp v) (> (length v) 0) (seq-every-p #'numberp v)) v default) nil))

(defun eas-geo-proj--raw (type entry spec)
  "The raw function of TYPE (registry ENTRY) under SPEC's parallels."
  (cond
   ((plist-get entry :conic)
    (let ((par (eas-geo-proj--vec (plist-get spec :parallels) (plist-get entry :parallels))))
      (funcall (plist-get entry :conic) (* (nth 0 par) eas-geo-rad) (* (nth 1 par) eas-geo-rad))))
   ((plist-get entry :interrupt)
    (eas-geo-raw-interrupt (plist-get entry :interrupt) (cdr (assoc type eas-geo-raw-lobes))))
   (t (let ((raw (plist-get entry :raw))) (if (symbolp raw) (symbol-function raw) raw)))))

(defun eas-geo-proj--multiplex (streams)
  "A stream sending everything to each of STREAMS."
  (cl-flet ((to-each (slot) (lambda (&rest args) (dolist (s streams) (apply (funcall slot s) args)))))
    (eas-geo-stream--make :point (to-each #'eas-geo-stream-point) :line-start (to-each #'eas-geo-stream-line-start)
                          :line-end (to-each #'eas-geo-stream-line-end) :polygon-start (to-each #'eas-geo-stream-polygon-start)
                          :polygon-end (to-each #'eas-geo-stream-polygon-end) :sphere (to-each #'eas-geo-stream-sphere))))

(defun eas-geo-proj--berghaus-sphere (sink)
  "Stream d3's five-lobed Berghaus outline (degrees) into SINK."
  (let* ((lobes 5) (e 1e-2) (cr (- (cos (* e eas-geo-rad)))) (sr (sin (* e eas-geo-rad)))
         (delta (/ 360.0 lobes)) (delta0 (/ eas-geo-tau lobes)) (phi (- 90 (/ 180.0 lobes))) (phi0 eas-geo-half-pi))
    (eas-geo--call polygon-start sink)
    (eas-geo--call line-start sink)
    (dotimes (_ lobes)
      (eas-geo-point sink (/ (atan (* sr (cos phi0)) cr) eas-geo-rad) (/ (eas-geo-asin (* sr (sin phi0))) eas-geo-rad))
      (if (< phi -90)
          (progn (eas-geo-point sink -90 (- -180 phi e)) (eas-geo-point sink -90 (+ (- -180 phi) e)))
        (eas-geo-point sink 90 (+ phi e)) (eas-geo-point sink 90 (- phi e)))
      (setq phi (- phi delta) phi0 (- phi0 delta0)))
    (eas-geo--call line-end sink)
    (eas-geo--call polygon-end sink)))

(cl-defun eas-geo-proj--simple (raw &key (scale 150) (translate '(480 250)) (center '(0 0)) (rotate '(0 0 0))
                                    clip-angle clip-extent reclip (precision (sqrt 0.5)) reflect-x reflect-y (angle 0))
  "D3's projection of RAW with SCALE, TRANSLATE, CENTER, ROTATE (degrees).
CLIP-ANGLE (degrees) or the antimeridian preclips; CLIP-EXTENT
\[[X0 Y0] [X1 Y1]] postclips; RECLIP is mercator or transverse for their
automatic extent; PRECISION, REFLECT-X, REFLECT-Y and ANGLE (the planar
rotation, degrees) as in d3."
  (let* ((k (float scale)) (x (float (nth 0 translate))) (y (float (nth 1 translate)))
         (sx (if reflect-x -1 1)) (sy (if reflect-y -1 1))
         (lam (* (eas-geo-rem (nth 0 center) 360) eas-geo-rad)) (phi (* (eas-geo-rem (nth 1 center) 360) eas-geo-rad))
         (rot (eas-geo-rotation (* (eas-geo-rem (nth 0 rotate) 360) eas-geo-rad)
                                (* (eas-geo-rem (or (nth 1 rotate) 0) 360) eas-geo-rad)
                                (* (eas-geo-rem (or (nth 2 rotate) 0) 360) eas-geo-rad)))
         (alpha (* (eas-geo-rem angle 360) eas-geo-rad))
         (ca (* (cos alpha) k)) (sa (* (sin alpha) k))
         ;; d3's scaleTranslateRotate: (X Y) -> [ca X - sa Y + DX, DY - sa X - ca Y]
         (st (if (= alpha 0)
                 (lambda (r dx dy) (vector (+ dx (* k sx (aref r 0))) (- dy (* k sy (aref r 1)))))
               (lambda (r dx dy) (let ((rx (* sx (aref r 0))) (ry (* sy (aref r 1))))
                                   (vector (+ (- (* ca rx) (* sa ry)) dx) (- dy (* sa rx) (* ca ry)))))))
         (c (funcall st (funcall raw lam phi) 0 0))
         (dx (- x (aref c 0))) (dy (- y (aref c 1)))
         ;; (* k sx X) is (* (* k sx) X), and k times 1 or -1 is exact.
         (ksx (* k sx)) (ksy (* k sy))
         (project (if (= alpha 0)
                      ;; st inlined: the same arithmetic, a call fewer per point.
                      (lambda (l p) (let ((r (funcall raw l p)))
                                      (vector (+ dx (* ksx (aref r 0))) (- dy (* ksy (aref r 1))))))
                    (lambda (l p) (funcall st (funcall raw l p) dx dy))))
         (point (lambda (lon lat) (let ((r (funcall (car rot) (* lon eas-geo-rad) (* lat eas-geo-rad))))
                                    (funcall project (aref r 0) (aref r 1)))))
         (extent
          (if (not reclip) clip-extent
            ;; d3 projects rotation.invert([0, 0]), which rotates back to the origin.
            (let* ((kk (* float-pi k)) (tt (funcall project 0 0)))
              (cond ((null clip-extent)
                     (vector (vector (- (aref tt 0) kk) (- (aref tt 1) kk)) (vector (+ (aref tt 0) kk) (+ (aref tt 1) kk))))
                    ((eq reclip 'mercator)
                     (vector (vector (max (- (aref tt 0) kk) (aref (aref clip-extent 0) 0)) (aref (aref clip-extent 0) 1))
                             (vector (min (+ (aref tt 0) kk) (aref (aref clip-extent 1) 0)) (aref (aref clip-extent 1) 1))))
                    (t (vector (vector (aref (aref clip-extent 0) 0) (max (- (aref tt 1) kk) (aref (aref clip-extent 0) 1)))
                               (vector (aref (aref clip-extent 1) 0) (min (+ (aref tt 1) kk) (aref (aref clip-extent 1) 1)))))))))
         (preclip (if (and (numberp clip-angle) (> clip-angle 0))
                      (eas-geo-clip-circle (* clip-angle eas-geo-rad))
                    (eas-geo-clip-antimeridian)))
         (postclip (if extent
                       (eas-geo-clip-rectangle (aref (aref extent 0) 0) (aref (aref extent 0) 1)
                                               (aref (aref extent 1) 0) (aref (aref extent 1) 1))
                     #'identity))
         (resample (eas-geo-resample project (* precision precision))))
    (list :point point
          :stream (lambda (sink)
                    (eas-geo-radians-rotate (car rot) (funcall preclip (funcall resample (funcall postclip sink)))))
          ;; The halves `eas-geo-proj-stream' records and replays.
          :front (lambda (sink) (eas-geo-radians-rotate (car rot) (funcall preclip sink)))
          :front-key (list (float (eas-geo-rem (nth 0 rotate) 360)) (float (eas-geo-rem (or (nth 1 rotate) 0) 360))
                           (float (eas-geo-rem (or (nth 2 rotate) 0) 360))
                           (and (numberp clip-angle) (> clip-angle 0) (float clip-angle)))
          :project project :delta2 (* precision precision) :postclip postclip)))

(defun eas-geo-proj--albers-usa (k x y precision)
  "D3's albersUsa at scale K, translate X Y and PRECISION."
  (let* ((e eas-geo-eps)
         (sub (lambda (par rot center scale tx ty box)
                (eas-geo-proj--simple (eas-geo-raw-conic-equal-area (* (car par) eas-geo-rad) (* (cadr par) eas-geo-rad))
                                      :scale scale :translate (list tx ty) :rotate (list rot 0 0) :center center
                                      :precision precision
                                      :clip-extent (vector (vector (nth 0 box) (nth 1 box)) (vector (nth 2 box) (nth 3 box))))))
         (lower (funcall sub '(29.5 45.5) 96 '(-0.6 38.7) k x y
                         (list (- x (* 0.455 k)) (- y (* 0.238 k)) (+ x (* 0.455 k)) (+ y (* 0.238 k)))))
         (alaska (funcall sub '(55 65) 154 '(-2 58.5) (* 0.35 k) (- x (* 0.307 k)) (+ y (* 0.201 k))
                          (list (+ (- x (* 0.425 k)) e) (+ y (* 0.120 k) e) (- (- x (* 0.214 k)) e) (- (+ y (* 0.234 k)) e))))
         (hawaii (funcall sub '(8 18) 157 '(-3 19.9) k (- x (* 0.205 k)) (+ y (* 0.212 k))
                          (list (+ (- x (* 0.214 k)) e) (+ y (* 0.166 k) e) (- (- x (* 0.115 k)) e) (- (+ y (* 0.234 k)) e))))
         (subs (list (cons lower (list (- x (* 0.455 k)) (- y (* 0.238 k)) (+ x (* 0.455 k)) (+ y (* 0.238 k))))
                     (cons alaska (list (+ (- x (* 0.425 k)) e) (+ y (* 0.120 k) e) (- (- x (* 0.214 k)) e) (- (+ y (* 0.234 k)) e)))
                     (cons hawaii (list (+ (- x (* 0.214 k)) e) (+ y (* 0.166 k) e) (- (- x (* 0.115 k)) e) (- (+ y (* 0.234 k)) e))))))
    (list :point (lambda (lon lat)
                   (cl-loop for (p x0 y0 x1 y1) in subs
                            for q = (funcall (plist-get p :point) lon lat)
                            when (and (<= x0 (aref q 0) x1) (<= y0 (aref q 1) y1)) return q))
          :stream (lambda (sink) (eas-geo-proj--multiplex
                                  (mapcar (lambda (p) (funcall (plist-get (car p) :stream) sink)) subs))))))

(defun eas-geo-proj--plain (spec k tr)
  "SPEC with scale K and translate TR as built, for the scene and inversion."
  (eas-plist-put (eas-plist-put spec :scale k) :translate (vconcat tr)))

(defun eas-geo-proj-invert (proj x y &optional guess)
  "Longitude and latitude [LON LAT] that PROJ puts at pixel X Y, or nil.
Newton's method on PROJ's point function from GUESS (its centre by
default), so every projection inverts without a raw inverse."
  (let* ((f (plist-get proj :point))
         (c (or guess (let ((cc (plist-get (plist-get proj :spec) :center)))
                        (if (and (vectorp cc) (= (length cc) 2)) (append cc nil) '(0 0)))))
         (lon (float (nth 0 c))) (lat (float (nth 1 c))) (h 1e-4) (ok nil))
    (dotimes (_ 30)
      (unless ok
        (let* ((p (funcall f lon lat)) (px (funcall f (+ lon h) lat)) (py (funcall f lon (+ lat h)))
               (ex (- (aref p 0) x)) (ey (- (aref p 1) y))
               (a (/ (- (aref px 0) (aref p 0)) h)) (b (/ (- (aref py 0) (aref p 0)) h))
               (c2 (/ (- (aref px 1) (aref p 1)) h)) (d (/ (- (aref py 1) (aref p 1)) h))
               (det (- (* a d) (* b c2))))
          (if (or (= det 0) (isnan det)) (setq ok 'fail)
            (setq lon (- lon (/ (- (* d ex) (* b ey)) det))
                  lat (max -89.999 (min 89.999 (- lat (/ (- (* a ey) (* c2 ex)) det)))))
            (when (< (+ (abs ex) (abs ey)) 1e-6) (setq ok t))))))
    (and (not (eq ok 'fail)) (not (isnan lon)) (vector lon lat))))

(defun eas-geo-proj-supported-p (type)
  "Non-nil when projection TYPE is drawn natively."
  (and (stringp type) (member type (eas-geo-raw-names)) t))

(defun eas-geo-proj (spec &optional scale translate)
  "The projection Vega-Lite projection SPEC describes (plain values).
SCALE and TRANSLATE, when given, override SPEC's (a fit).  Signal
UNSUPPORTED_FEATURE for a type not drawn natively."
  (let* ((type (or (plist-get spec :type) "equalEarth"))
         (entry (eas-geo-raw-type type))
         (precision (eas-geo-proj--num (plist-get spec :precision) (sqrt 0.5))))
    (unless (or entry (equal type "albersUsa"))
      (eas-signal "UNSUPPORTED_FEATURE" (format "projection type %s is not drawn natively" type)
                  :feature (concat "projection/" type) :path "/projection/type"))
    (let* ((k (or scale (eas-geo-proj--num (plist-get spec :scale) (or (plist-get entry :scale) 1070))))
           (tr (or translate (eas-geo-proj--vec (plist-get spec :translate) (or (plist-get entry :translate) [480 250])))))
      (if (equal type "albersUsa")
          (append (eas-geo-proj--albers-usa (float k) (float (nth 0 tr)) (float (nth 1 tr)) precision)
                  (list :type type :spec (eas-geo-proj--plain spec k tr)
                        :native (vector type (float k) (float (nth 0 tr)) (float (nth 1 tr)) precision)))
        (let* ((transverse (eq (plist-get entry :reclip) 'transverse))
               (center (eas-geo-proj--vec (plist-get spec :center) (or (plist-get entry :center) [0 0])))
               (center (if transverse (list (- (nth 1 center)) (nth 0 center)) center))
               (rotate (eas-geo-proj--vec (plist-get spec :rotate) (or (plist-get entry :rotate) [0 0 0])))
               (rotate (list (nth 0 rotate) (or (nth 1 rotate) 0)
                             (+ (or (nth 2 rotate) 0) (if transverse 90 0))))
               (clip-angle (let ((a (plist-get spec :clipAngle))) (if (numberp a) a (plist-get entry :clip-angle))))
               (extent (let ((e (plist-get spec :clipExtent))) (and (vectorp e) (= (length e) 2) e)))
               (p (eas-geo-proj--simple (eas-geo-proj--raw type entry spec)
                                        :scale k :translate tr :center center :rotate rotate
                                        :clip-angle clip-angle :clip-extent extent
                                        :reclip (plist-get entry :reclip) :precision precision
                                        :angle (eas-geo-proj--num (plist-get spec :angle) (or (plist-get entry :angle) 0))
                                        :reflect-x (eas-true-p (plist-get spec :reflectX))
                                        :reflect-y (eas-true-p (plist-get spec :reflectY))))
               (outline (cond ((plist-get entry :interrupt)
                               (let ((g (eas-geo-raw-interrupt-sphere (cdr (assoc type eas-geo-raw-lobes)))))
                                 (lambda (s) (eas-geo-stream-geometry g s))))
                              ((eq (plist-get entry :sphere) 'berghaus) #'eas-geo-proj--berghaus-sphere)
                              ((eq (plist-get entry :sphere) 'armadillo) #'eas-geo-raw--armadillo-sphere)
                              ((eq (plist-get entry :sphere) 'polyhedral) #'eas-geo-raw--butterfly-sphere))))
          (when outline
            ;; The sphere's outline is drawn unrotated, as d3-geo-projection does.
            (let ((flat (eas-geo-proj--simple (eas-geo-proj--raw type entry spec)
                                              :scale k :translate tr :center center :rotate '(0 0 0)
                                              :clip-angle clip-angle :clip-extent extent :precision precision
                                              :angle (eas-geo-proj--num (plist-get spec :angle) (or (plist-get entry :angle) 0))))
                  (stream (plist-get p :stream)))
              (setq p (plist-put p :stream
                                 (lambda (sink)
                                   (let ((s (funcall stream sink)))
                                     (setf (eas-geo-stream-sphere s)
                                           (lambda () (funcall outline (funcall (plist-get flat :stream) sink))))
                                     s))))))
          (append p (list :type type :spec (eas-geo-proj--plain spec k tr)
                          :native (eas-geo-proj--native type entry spec k tr center rotate clip-angle extent
                                                        precision))))))))

(defun eas-geo-proj--native (type entry spec k tr center rotate clip-angle extent precision)
  "The parameters the native module builds projection TYPE from.
ENTRY is TYPE's registry entry and SPEC the projection; K, TR, CENTER,
ROTATE, CLIP-ANGLE, EXTENT and PRECISION as `eas-geo-proj' resolved
them for `eas-geo-proj--simple'."
  (let ((par (eas-geo-proj--vec (plist-get spec :parallels) (or (plist-get entry :parallels) [0 0]))))
    (vector type (nth 0 par) (nth 1 par) k (nth 0 tr) (nth 1 tr) (nth 0 center) (nth 1 center)
            (nth 0 rotate) (nth 1 rotate) (nth 2 rotate) (and (numberp clip-angle) clip-angle)
            (and extent (vector (aref (aref extent 0) 0) (aref (aref extent 0) 1)
                                (aref (aref extent 1) 0) (aref (aref extent 1) 1)))
            (pcase (plist-get entry :reclip) ('mercator 1) ('transverse 2) (_ 0))
            precision (eas-geo-proj--num (plist-get spec :angle) (or (plist-get entry :angle) 0))
            (eas-true-p (plist-get spec :reflectX)) (eas-true-p (plist-get spec :reflectY)))))

(defun eas-geo-proj-fit (spec objects extent)
  "D3's fitExtent: (SCALE . [X Y]) fitting OBJECTS into EXTENT [X0 Y0 X1 Y1].
SPEC is the Vega-Lite projection (plain values); OBJECTS GeoJSON."
  (let* ((p (eas-geo-proj (eas--plist-without spec :clipExtent) 150 '(0 0)))
         (b (or (eas-geo-native-fit (plist-get p :native) objects)
                (let ((sink (eas-geo-bounds-sink)) (stream nil))
                  ;; A shared stream is d3's: one resampler's state runs on across objects.
                  (dolist (o objects)
                    (if (eas-geo-proj--sphere-free-p o)
                        (eas-geo-proj-stream p o sink)
                      (eas-geo-stream-object o (or stream (setq stream (funcall (plist-get p :stream) sink))))))
                  (funcall (eas-geo-stream-result sink))))))
    (let* ((w (- (aref extent 2) (aref extent 0))) (h (- (aref extent 3) (aref extent 1)))
           (bw (- (aref b 2) (aref b 0))) (bh (- (aref b 3) (aref b 1))))
      (if (not (and (> bw 0) (> bh 0) (< bw 1.0e+INF)))
          (cons 150 (list (+ (aref extent 0) (/ w 2.0)) (+ (aref extent 1) (/ h 2.0))))
        (let ((k (min (/ w bw) (/ h bh))))
          (cons (* 150 k)
                (list (+ (aref extent 0) (/ (- w (* k (+ (aref b 2) (aref b 0)))) 2))
                      (+ (aref extent 1) (/ (- h (* k (+ (aref b 3) (aref b 1)))) 2)))))))))

(defvar eas-geo-proj--recorded (make-hash-table :test 'eq :weakness 'key)
  "Spherical streams recorded per GeoJSON object, by identity and weakly.
Keys are `eas-geo-proj--identity's; each value is an alist of
\((TYPE . FRONT-KEY) . EVENTS): what the object sends the resampler
under a rotation and preclip (`eas-geo-recorder').")

(defun eas-geo-proj--sphere-free-p (o)
  "Return non-nil when GeoJSON O has no Sphere (its outline may be special)."
  (and (eas-object-p o)
       (pcase (plist-get o :type)
         ("Sphere" nil)
         ("Feature" (eas-geo-proj--sphere-free-p (plist-get o :geometry)))
         ("FeatureCollection" (seq-every-p (lambda (f) (eas-geo-proj--sphere-free-p (plist-get f :geometry)))
                                           (plist-get o :features)))
         ("GeometryCollection" (seq-every-p #'eas-geo-proj--sphere-free-p (plist-get o :geometries)))
         (_ t))))

(defun eas-geo-proj--identity (o)
  "(KEY . TYPE): what GeoJSON O streams depends on, by identity.
A feature is its geometry, and a geometry its coordinates (rows copied
per view of a grid of maps share them); TYPE tells apart geometries
sharing coordinates."
  (let ((type (plist-get o :type)))
    (cond ((and (equal type "Feature") (eas-object-p (plist-get o :geometry)))
           (eas-geo-proj--identity (plist-get o :geometry)))
          ((vectorp (plist-get o :coordinates)) (cons (plist-get o :coordinates) type))
          (t (cons o type)))))

(defun eas-geo-proj--events (proj object)
  "OBJECT's recorded spherical stream under PROJ's front half, or nil."
  (let* ((id (eas-geo-proj--identity object))
         (key (cons (cdr id) (plist-get proj :front-key)))
         (known (gethash (car id) eas-geo-proj--recorded))
         (hit (assoc key known)))
    (if hit (cdr hit)
      (let ((rec (eas-geo-recorder)))
        (eas-geo-stream-object object (funcall (plist-get proj :front) rec))
        (let ((events (funcall (eas-geo-stream-result rec))))
          (puthash (car id) (cons (cons key events) known) eas-geo-proj--recorded)
          events)))))

(defun eas-geo-proj-stream (proj object sink)
  "Stream GeoJSON OBJECT under PROJ into SINK.
A shape's spherical half (rotation, preclip) is recorded once per
rotation and clip and replayed into PROJ's resampler; anything else
takes PROJ's :stream."
  (let ((events (and (plist-get proj :front) (> (plist-get proj :delta2) 0)
                     (eas-geo-proj--sphere-free-p object)
                     (eas-geo-proj--events proj object))))
    (if events
        (eas-geo-replay events (plist-get proj :project) (plist-get proj :delta2)
                        (funcall (plist-get proj :postclip) sink))
      (eas-geo-stream-object object (funcall (plist-get proj :stream) sink)))))

(defun eas-geo-proj-path (proj object &optional radius tolerance)
  "The projected path of GeoJSON OBJECT under PROJ (`eas-geo-path-sink').
RADIUS is a Point's circle radius (4.5, d3's default); TOLERANCE, in
pixels, drops a line's points closer than it to the last one kept."
  (let ((sink (eas-geo-path-sink (or radius 4.5) tolerance)))
    (eas-geo-proj-stream proj object sink)
    (funcall (eas-geo-stream-result sink))))

(provide 'eas-geo-proj)
;;; eas-geo-proj.el ends here

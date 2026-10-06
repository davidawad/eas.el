;;; eas-geo.el --- map projections for longitude/latitude point marks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1/L2.  A unit with a projection and longitude/latitude
;; fields draws its marks where d3-geo puts them.  `eas-geo-lower'
;; rewrites such a unit: an {"x-eas:transform": "project"} computes each
;; row's pixel position, and the x and y channels read it on fixed
;; linear scales without axes.  The projection is fitted to the rows as
;; Vega fits it to the data (fitSize to the view's width and height):
;; projected once at unit scale, then scaled by min(w/dx, h/dy) and
;; centred, which is exact because every supported projection is affine
;; in its scale and translate.  A point the projection drops (albersUsa
;; outside its three insets) sits at the origin, as Vega draws it.
;;
;; Projections: albersUsa (d3's composite: lower 48, Alaska at 0.35x
;; and Hawaii, each clipped to its inset), albers and conicEqualArea
;; (parallels, rotate), mercator, equirectangular and equalEarth.
;; Maps with geoshapes, graticules or other projection properties are
;; eas-geoshape.el's, which lowers them first.

;;; Code:

(require 'eas-core)
(require 'eas-transform)
(require 'eas-transform-domain)

(defconst eas-geo-projections '("albersUsa" "albers" "conicEqualArea")
  "Projection types this lowering draws.
The others (mercator, equirectangular, equalEarth) are drawn by
eas-projection.el (fc-qx1.40), which runs at compile.")

(defun eas-geo--rad (deg) "DEG in radians." (* deg (/ float-pi 180)))

(defun eas-geo--wrap (lam)
  "Longitude LAM (radians) wrapped into [-pi, pi], as d3's rotation does."
  (cond ((> lam float-pi) (- lam (* 2 float-pi))) ((< lam (- float-pi)) (+ lam (* 2 float-pi))) (t lam)))

(defun eas-geo--conic-raw (p0 p1)
  "Return d3's conicEqualAreaRaw for parallels P0 P1 (radians).
The result maps (L P) -> (X . Y)."
  (let* ((sy0 (sin p0)) (n (/ (+ sy0 (sin p1)) 2.0)))
    (if (< (abs n) 1e-6)
        (let ((cy0 (cos p0))) (lambda (l p) (cons (* l cy0) (/ (sin p) cy0))))
      (let* ((c (+ 1 (* sy0 (- (* 2 n) sy0)))) (r0 (/ (sqrt c) n)))
        (lambda (l p) (let ((r (/ (sqrt (max 0 (- c (* 2 n (sin p))))) n)))
                        (cons (* r (sin (* l n))) (- r0 (* r (cos (* l n)))))))))))

(defun eas-geo--equal-earth (l p)
  "Return d3's equalEarthRaw of L P (radians)."
  (let* ((a1 1.340264) (a2 -0.081106) (a3 0.000893) (a4 0.003796) (m (/ (sqrt 3) 2))
         (th (asin (* m (sin p)))) (t2 (* th th)) (t6 (* t2 t2 t2)))
    (cons (/ (* l (cos th)) (* m (+ a1 (* 3 a2 t2) (* t6 (+ (* 7 a3) (* 9 a4 t2))))))
          (* th (+ a1 (* a2 t2) (* t6 (+ a3 (* a4 t2))))))))

(defun eas-geo--sub (raw rotate center scale tx ty)
  "Return a d3 projection, a function (LON LAT) -> (X . Y) px.
RAW rotated by ROTATE degrees of longitude, CENTER (lon lat, rotated
frame) at TX TY, SCALE."
  (let* ((c (funcall raw (eas-geo--rad (aref center 0)) (eas-geo--rad (aref center 1))))
         (dl (eas-geo--rad rotate)))
    (lambda (lon lat)
      (let ((p (funcall raw (eas-geo--wrap (+ (eas-geo--rad lon) dl)) (eas-geo--rad lat))))
        (cons (+ tx (* scale (- (car p) (car c)))) (- ty (* scale (- (cdr p) (cdr c)))))))))

(defun eas-geo--albers-usa ()
  "Return d3's geoAlbersUsa at scale 1, translate 0.
The result maps (LON LAT) -> (X . Y) or nil."
  (let* ((e 1e-6)
         (lower (eas-geo--sub (eas-geo--conic-raw (eas-geo--rad 29.5) (eas-geo--rad 45.5)) 96 [-0.6 38.7] 1 0 0))
         (alaska (eas-geo--sub (eas-geo--conic-raw (eas-geo--rad 55) (eas-geo--rad 65)) 154 [-2 58.5] 0.35 -0.307 0.201))
         (hawaii (eas-geo--sub (eas-geo--conic-raw (eas-geo--rad 8) (eas-geo--rad 18)) 157 [-3 19.9] 1 -0.205 0.212))
         (insets (list (list lower -0.455 -0.238 0.455 0.238)
                       (list alaska (+ -0.425 e) (+ 0.120 e) (- -0.214 e) (- 0.234 e))
                       (list hawaii (+ -0.214 e) (+ 0.166 e) (- -0.115 e) (- 0.234 e)))))
    (lambda (lon lat)
      (cl-loop for (f x0 y0 x1 y1) in insets
               for p = (funcall f lon lat)
               when (and (<= x0 (car p) x1) (<= y0 (cdr p) y1)) return p))))

(defun eas-geo-projection (projection)
  "Return the unit-scale forward function of PROJECTION.
PROJECTION is a Vega-Lite projection object; the function maps
\(LON LAT) -> (X . Y) or nil.  Return nil for a type not drawn natively."
  (let* ((type (or (plist-get projection :type) "equalEarth"))
         (rot (let ((r (plist-get projection :rotate))) (if (vectorp r) (aref r 0) 0)))
         (par (plist-get projection :parallels)))
    (pcase type
      ("albersUsa" (eas-geo--albers-usa))
      ((or "albers" "conicEqualArea")
       (let ((albers (equal type "albers")))
         (eas-geo--sub (eas-geo--conic-raw (eas-geo--rad (if (vectorp par) (aref par 0) (if albers 29.5 0)))
                                           (eas-geo--rad (if (vectorp par) (aref par 1) (if albers 45.5 60))))
                       (if (plist-member projection :rotate) rot (if albers 96 0))
                       (if albers [-0.6 38.7] [0 0]) 1 0 0)))
      ("mercator" (eas-geo--sub (lambda (l p) (cons l (log (tan (/ (+ (/ float-pi 2) p) 2))))) rot [0 0] 1 0 0))
      ("equirectangular" (eas-geo--sub (lambda (l p) (cons l p)) rot [0 0] 1 0 0))
      ("equalEarth" (eas-geo--sub #'eas-geo--equal-earth rot [0 0] 1 0 0)))))

(defun eas-geo-project (rows params)
  "ROWS with PARAMS's :as fields holding their projected pixel position.
PARAMS: :projection, :longitude and :latitude fields, :size [W H]."
  (let* ((f (or (eas-geo-projection (plist-get params :projection))
                (eas-signal "UNSUPPORTED_FEATURE" "projection type not drawn natively" :feature "projection")))
         (lon (eas-key (plist-get params :longitude))) (lat (eas-key (plist-get params :latitude)))
         (as (plist-get params :as)) (kx (eas-key (aref as 0))) (ky (eas-key (aref as 1)))
         (size (plist-get params :size)) (w (aref size 0)) (h (aref size 1))
         (points (seq-map (lambda (r) (let ((a (plist-get r lon)) (b (plist-get r lat)))
                                        (and (numberp a) (numberp b) (funcall f a b))))
                          rows))
         (x0 1.0e+INF) (y0 1.0e+INF) (x1 -1.0e+INF) (y1 -1.0e+INF))
    (dolist (p points)
      (when p (setq x0 (min x0 (car p)) y0 (min y0 (cdr p)) x1 (max x1 (car p)) y1 (max y1 (cdr p)))))
    (let* ((ok (and (< x0 x1) (< y0 y1)))
           (k (if ok (min (/ w (- x1 x0)) (/ h (- y1 y0))) 1))
           (tx (if ok (/ (- w (* k (+ x1 x0))) 2) 0)) (ty (if ok (/ (- h (* k (+ y1 y0))) 2) 0)))
      (vconcat (cl-mapcar (lambda (r p)
                            (append r (list kx (if p (+ tx (* k (car p))) 0)
                                            ;; y grows up on the linear scale that reads it
                                            ky (- h (if p (+ ty (* k (cdr p))) 0)))))
                          rows points)))))

(eas-register-transform
 "project"
 :doc "Pixel positions of longitude/latitude rows under a Vega-Lite projection fitted to size."
 :schema '(:projection (:type "object" :required t :doc "the Vega-Lite projection")
           :longitude (:type "string" :required t :doc "longitude field")
           :latitude (:type "string" :required t :doc "latitude field")
           :size (:type "array" :required t :doc "[width height] the projection is fitted to")
           :as (:type "array" :default ["x_projected" "y_projected"] :doc "output fields"))
 :fn #'eas-geo-project)

;;; Lowering

(defvar eas-facet-keep)

(defun eas-geo-lower (spec)
  "SPEC with a projected longitude/latitude unit drawn on x and y.
A unit whose projection type is drawn natively and whose longitude and
latitude are fields gets the project transform; anything else is left
alone (and reported unsupported).  Export keeps the projection."
  (let* ((proj (plist-get spec :projection)) (enc (plist-get spec :encoding))
         (lon (plist-get enc :longitude)) (lat (plist-get enc :latitude)))
    (cond
     ((bound-and-true-p eas-facet-keep) spec)
     ((and (plist-get spec :mark) (eas-object-p proj) proj
           (member (or (plist-get proj :type) "equalEarth") eas-geo-projections)
           (eas-object-p lon) (stringp (plist-get lon :field)) (eas-object-p lat) (stringp (plist-get lat :field)))
      (let* ((w (let ((v (plist-get spec :width))) (if (numberp v) v 480)))
             (h (let ((v (plist-get spec :height))) (if (numberp v) v 300)))
             (pos (lambda (field size) (list :field field :type "quantitative" :axis :null
                                             :scale (list :domain (vector 0 size) :nice :false :zero :false)))))
        (thread-first spec
                      (eas--plist-without :projection)
                      (eas-plist-put :transform
                                     (vconcat (plist-get spec :transform)
                                              (list (list :x-eas:transform "project" :projection proj
                                                          :longitude (plist-get lon :field) :latitude (plist-get lat :field)
                                                          :size (vector w h) :as ["x_projected" "y_projected"]))))
                      (eas-plist-put :encoding
                                     (append (eas--plist-without (eas--plist-without enc :longitude) :latitude)
                                             (list :x (funcall pos "x_projected" w) :y (funcall pos "y_projected" h)))))))
     (t (let ((out spec))
          (dolist (key '(:layer :vconcat :hconcat))
            (when (vectorp (plist-get out key))
              (setq out (eas-plist-put out key (vconcat (mapcar #'eas-geo-lower (plist-get out key)))))))
          out)))))

(defun eas-geo-features (spec &optional path)
  "Return features of the projected units lowered in SPEC (at PATH).
The projection type, which check and supported.json see."
  (let ((path (or path "")) out)
    (seq-doseq (tr (plist-get spec :transform))
      (when (equal (plist-get tr :x-eas:transform) "project")
        ;; longitude and latitude stay unproven alone: drawn only through a projection.
        (setq out (append out (list (list :feature (concat "projection/" (or (plist-get (plist-get tr :projection) :type) "equalEarth"))
                                          :path (concat path "/projection")))))))
    (dolist (key '(:layer :vconcat :hconcat) out)
      (seq-do-indexed (lambda (c i) (setq out (append out (eas-geo-features c (format "%s/%s/%d" path (eas-key-name key) i)))))
                      (plist-get spec key)))))

(defvar eas-spec-rewrite-functions)
(add-hook 'eas-spec-rewrite-functions #'eas-geo-lower t)
(defvar eas-spec-feature-functions)
(add-hook 'eas-spec-feature-functions #'eas-geo-features)

(provide 'eas-geo)
;;; eas-geo.el ends here

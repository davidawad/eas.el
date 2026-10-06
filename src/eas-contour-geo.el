;;; eas-contour-geo.el --- geopath, geopoints and topojson for contours -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L0/L1.  Vega draws contours and map shapes as paths: its
;; geopath transform turns a GeoJSON geometry into SVG path data under
;; a projection.  Rows stay flat, and resolve has no scales, so:
;;
;; - "geopath" writes each row's geometry (FIELD, a GeoJSON object or
;;   its JSON text) as SVG path data in pixels (AS), projected into a
;;   SIZE plot, and drops the geometry.  A point mark draws the path
;;   as its shape (shape scale null) at x = y = 0 with size 4, where
;;   Vega-Lite (and Vega's custom symbols) scale a path by
;;   sqrt(size) / 2 = 1, so it stands where the projection put it.
;; - "geopoints" writes one row per vertex (x, y, a ring id and the
;;   vertex order) for line marks on the chart's own x and y scales,
;;   which suits stroked contours in data units (kde2d's).
;; - the "topojson" adapter turns a TopoJSON topology into flat rows,
;;   one per geometry of the object named, its GeoJSON as text.
;;
;; Projections: identity (reflectX, reflectY), naturalEarth1 and the
;; raw projections of eas-projection.el (equalEarth, mercator,
;; equirectangular), with scale, translate, center and a longitude
;; rotation.  Without scale and translate the projection is fitted to
;; the rows' geometry (d3's fitSize, Vega-Lite's default); with only
;; one, the other takes d3's default.  Polygons are not clipped at the
;; antimeridian and edges are not resampled.

;;; Code:

(require 'eas-core)
(require 'eas-data)
(require 'eas-adapters)
(require 'eas-projection)
(require 'eas-transform-domain)

;;; Projections

(defconst eas-contour-geo-projections
  '("identity" "naturalEarth1" "equalEarth" "mercator" "equirectangular")
  "Projection types geopath and geopoints draw.")

(defun eas-contour-geo--natural-earth1 (l p)
  "Return d3's naturalEarth1Raw of L P (radians), north up."
  (let* ((p2 (* p p)) (p4 (* p2 p2)))
    (cons (* l (+ 0.8707 (* -0.131979 p2) (* p4 (+ -0.013791 (* p4 (- (* 0.003971 p2) (* 0.001529 p4)))))))
          (* p (+ 1.007226 (* p2 (+ 0.015085 (* p4 (+ -0.044475 (* 0.028874 p2) (* -0.005916 p4))))))))))

(defun eas-contour-geo--default-scale (type)
  "The default scale d3 gives projection TYPE."
  (pcase type
    ("identity" 1) ("naturalEarth1" 175.295) ("equalEarth" 177.158)
    (_ (/ 961 (* 2 float-pi)))))

(defun eas-contour-geo--raw (proj)
  "The function (X Y) -> (U . V) of PROJ before scale and translate, y down."
  (let ((type (or (plist-get proj :type) "equalEarth")))
    (unless (member type eas-contour-geo-projections)
      (eas-signal "UNSUPPORTED_FEATURE" (format "projection %s is not drawn natively" type)
                  :feature (concat "projection/" type) :path "/projection/type"))
    (if (equal type "identity")
        (let ((fx (if (eas-true-p (plist-get proj :reflectX)) -1 1))
              (fy (if (eas-true-p (plist-get proj :reflectY)) -1 1)))
          (lambda (x y) (cons (* fx x) (* fy y))))
      (let ((dl (let ((r (plist-get proj :rotate))) (if (and (vectorp r) (> (length r) 0)) (aref r 0) 0))))
        (lambda (lon lat)
          (let* ((l (degrees-to-radians (+ lon dl)))
                 (l (cond ((> l float-pi) (- l (* 2 float-pi))) ((< l (- float-pi)) (+ l (* 2 float-pi))) (t l)))
                 (xy (if (equal type "naturalEarth1")
                         (eas-contour-geo--natural-earth1 l (degrees-to-radians lat))
                       (eas-projection-raw type (radians-to-degrees l) lat))))
            (cons (car xy) (- (cdr xy)))))))))

(defun eas-contour-geo--bounds (raw geometries)
  "[X0 Y0 X1 Y1] of every vertex of GEOMETRIES under RAW, or nil."
  (let ((b nil))
    (dolist (g geometries)
      (eas-contour-geo--each-point
       g (lambda (p) (let ((q (funcall raw (aref p 0) (aref p 1))))
                       (if b (setq b (vector (min (aref b 0) (car q)) (min (aref b 1) (cdr q))
                                             (max (aref b 2) (car q)) (max (aref b 3) (cdr q))))
                         (setq b (vector (car q) (cdr q) (car q) (cdr q))))))))
    b))

(defun eas-contour-geo-projection (proj size geometries)
  "The function (X Y) -> (PX . PY) of projection PROJ for a SIZE plot.
Without scale and translate it is fitted to GEOMETRIES."
  (let* ((proj (or proj '(:type "identity")))
         (raw (eas-contour-geo--raw proj))
         (w (aref size 0)) (h (aref size 1))
         (scale (plist-get proj :scale)) (translate (plist-get proj :translate))
         (center (plist-get proj :center)) k tx ty)
    (if (or scale translate)
        (let ((c (if (vectorp center) (funcall raw (aref center 0) (aref center 1)) '(0 . 0))))
          (setq k (or scale (eas-contour-geo--default-scale (plist-get proj :type)))
                tx (- (if (vectorp translate) (aref translate 0) (if (equal (plist-get proj :type) "identity") 0 480))
                      (* k (car c)))
                ty (- (if (vectorp translate) (aref translate 1) (if (equal (plist-get proj :type) "identity") 0 250))
                      (* k (cdr c)))))
      (let* ((b (or (eas-contour-geo--bounds raw geometries) [0 0 1 1]))
             (dx (- (aref b 2) (aref b 0))) (dy (- (aref b 3) (aref b 1))))
        (setq k (cond ((and (> dx 0) (> dy 0)) (min (/ w dx) (/ h dy))) ((> dx 0) (/ w dx)) ((> dy 0) (/ h dy)) (t 1))
              tx (/ (- w (* k (+ (aref b 0) (aref b 2)))) 2.0)
              ty (/ (- h (* k (+ (aref b 1) (aref b 3)))) 2.0))))
    (lambda (x y) (let ((q (funcall raw x y))) (cons (+ tx (* k (car q))) (+ ty (* k (cdr q))))))))

;;; Geometry

(defun eas-contour-geo-geometry (value)
  "VALUE as a GeoJSON geometry plist: parsed from text, unwrapped from features."
  (let ((g (if (stringp value) (eas-json-parse value) value)))
    (pcase (and (eas-object-p g) (plist-get g :type))
      ("Feature" (eas-contour-geo-geometry (plist-get g :geometry)))
      ("FeatureCollection" (list :type "GeometryCollection"
                                 :geometries (vconcat (mapcar #'eas-contour-geo-geometry (plist-get g :features)))))
      ((pred stringp) g))))

(defun eas-contour-geo--lines (g)
  "The rings and lines of geometry G, as a list of (CLOSED . POINTS)."
  (let ((c (plist-get g :coordinates)))
    (pcase (plist-get g :type)
      ("Polygon" (mapcar (lambda (r) (cons t r)) c))
      ("MultiPolygon" (cl-loop for poly across c append (mapcar (lambda (r) (cons t r)) poly)))
      ("LineString" (list (cons nil c)))
      ("MultiLineString" (mapcar (lambda (l) (cons nil l)) c))
      ("GeometryCollection" (cl-loop for x across (plist-get g :geometries) append (eas-contour-geo--lines x))))))

(defun eas-contour-geo--each-point (g fn)
  "Call FN with every vertex [X Y] of geometry G."
  (dolist (line (eas-contour-geo--lines g)) (seq-doseq (p (cdr line)) (funcall fn p))))

(defun eas-contour-geo--n (v)
  "V with at most two decimals, as SVG path text."
  (let ((s (format "%.2f" v)))
    (setq s (replace-regexp-in-string "\\.?0+\\'" "" s))
    (if (equal s "-0") "0" s)))

(defun eas-contour-geo-path (g project)
  "SVG path data of geometry G under PROJECT, or \"\" when it draws nothing."
  (mapconcat (lambda (line)
               (let ((pts (cdr line)))
                 (if (< (length pts) 2) ""
                   (concat "M" (mapconcat (lambda (p) (let ((q (funcall project (aref p 0) (aref p 1))))
                                                    (concat (eas-contour-geo--n (car q)) "," (eas-contour-geo--n (cdr q)))))
                                      pts "L")
                           (if (car line) "Z" "")))))
             (eas-contour-geo--lines g)
             ""))

(defun eas-contour-geo--rows (rows params)
  "ROWS as (ROW-WITHOUT-FIELD . GEOMETRY) pairs of PARAMS's field."
  (let ((key (eas-key (plist-get params :field))))
    (cl-loop for row across (vconcat rows)
             for g = (eas-contour-geo-geometry (plist-get row key))
             when g collect (cons (eas--plist-without row key) g))))

(defun eas-contour-geopath (rows params)
  "The geopath transform: SVG path data of the geometry of ROWS under PARAMS."
  (let* ((pairs (eas-contour-geo--rows rows params))
         (project (eas-contour-geo-projection (plist-get params :projection) (plist-get params :size)
                                              (mapcar #'cdr pairs)))
         (as (eas-key (plist-get params :as))))
    (vconcat (cl-loop for (row . g) in pairs
                      for d = (eas-contour-geo-path g project)
                      unless (string-empty-p d) collect (append row (list as d))))))

(eas-register-transform
 "geopath"
 :doc "Vega's geopath: SVG path data in pixels of each row's GeoJSON geometry under a projection."
 :schema '(:field (:type "string" :default "geometry" :doc "GeoJSON geometry, feature or its JSON text")
           :projection (:type "object" :doc "Vega-Lite projection; identity by default")
           :size (:type "array" :required t :doc "[width height] of the plot in pixels")
           :as (:type "string" :default "path" :doc "output path field; rows drawing nothing are dropped"))
 :fn #'eas-contour-geopath)

(defun eas-contour-geo--clip-segment (a b box)
  "Segment A B (conses) clipped to BOX [X0 Y0 X1 Y1] (Liang-Barsky), or nil."
  (let* ((dx (- (car b) (car a))) (dy (- (cdr b) (cdr a))) (t0 0.0) (t1 1.0) (ok t))
    (cl-loop for (p q) in (list (list (- dx) (- (car a) (aref box 0))) (list dx (- (aref box 2) (car a)))
                                (list (- dy) (- (cdr a) (aref box 1))) (list dy (- (aref box 3) (cdr a))))
             while ok
             do (if (= p 0) (when (< q 0) (setq ok nil))
                  (let ((r (/ (float q) p)))
                    (if (< p 0) (when (> r t0) (setq t0 r)) (when (< r t1) (setq t1 r)))
                    (when (> t0 t1) (setq ok nil)))))
    (when ok
      (cons (cons (+ (car a) (* t0 dx)) (+ (cdr a) (* t0 dy)))
            (cons (+ (car a) (* t1 dx)) (+ (cdr a) (* t1 dy)))))))

(defun eas-contour-geo--clip-line (points box)
  "Clip POINTS (conses) to BOX; return the pieces left inside it."
  (if (null box) (list points)
    (let (runs run)
      (cl-loop for (a b) on points while b
               for seg = (eas-contour-geo--clip-segment a b box)
               do (cond ((null seg) (when (cdr run) (push (nreverse run) runs)) (setq run nil))
                        ((and run (equal (car run) (car seg))) (push (cdr seg) run))
                        (t (when (cdr run) (push (nreverse run) runs)) (setq run (list (cdr seg) (car seg))))))
      (when (cdr run) (push (nreverse run) runs))
      (nreverse runs))))

(defun eas-contour-geo--box (clip row)
  "The [X0 Y0 X1 Y1] box CLIP gives for ROW, or nil.
CLIP holds four numbers or field names."
  (when (and (vectorp clip) (= (length clip) 4))
    (let ((v (vconcat (mapcar (lambda (c) (if (stringp c) (plist-get row (eas-key c)) c)) clip))))
      (when (seq-every-p #'numberp v)
        (vector (min (aref v 0) (aref v 2)) (min (aref v 1) (aref v 3))
                (max (aref v 0) (aref v 2)) (max (aref v 1) (aref v 3)))))))

(defun eas-contour-geopoints (rows params)
  "The geopoints transform: one row per vertex of the geometry of ROWS.
PARAMS name the geometry field, a projection, a clip box and the outputs."
  (let* ((pairs (eas-contour-geo--rows rows params))
         (proj (plist-get params :projection))
         (project (if proj (eas-contour-geo-projection proj (plist-get params :size) (mapcar #'cdr pairs))
                    #'cons))
         (as (plist-get params :as)) (kx (eas-key (aref as 0))) (ky (eas-key (aref as 1)))
         (kr (eas-key (plist-get params :ring))) (ko (eas-key (plist-get params :order)))
         (ring 0) out)
    (dolist (pair pairs)
      (let ((box (eas-contour-geo--box (plist-get params :clip) (car pair))))
        (dolist (line (eas-contour-geo--lines (cdr pair)))
          (dolist (run (eas-contour-geo--clip-line
                        (mapcar (lambda (p) (funcall project (aref p 0) (aref p 1))) (cdr line)) box))
            (let ((k 0))
              (dolist (q run)
                (push (append (car pair) (list kr ring ko k kx (car q) ky (cdr q))) out)
                (setq k (1+ k))))
            (setq ring (1+ ring))))))
    (vconcat (nreverse out))))

(eas-register-transform
 "geopoints"
 :doc "One row per vertex of each row's GeoJSON geometry (ring id and order), for line marks."
 :schema '(:field (:type "string" :default "geometry" :doc "GeoJSON geometry, feature or its JSON text")
           :projection (:type "object" :doc "Vega-Lite projection; else coordinates stay as they are")
           :size (:type "array" :doc "[width height] in pixels, for a projection")
           :clip (:type "array" :doc "[x0 y0 x1 y1] (numbers or field names) the lines are cut to")
           :ring (:type "string" :default "ring" :doc "output ring id field (one per ring or line)")
           :order (:type "string" :default "order" :doc "output vertex order field")
           :as (:type "array" :default ["x" "y"] :doc "output coordinate fields"))
 :fn #'eas-contour-geopoints)

;; Per-vertex loops, compiled when the source is loaded interpreted.
(dolist (f '(eas-contour-geo--n eas-contour-geo-path eas-contour-geo--clip-segment eas-contour-geo--clip-line))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

;;; TopoJSON

(defun eas-contour-geo--arcs (topology)
  "TOPOLOGY's arcs decoded (delta and quantization) to vectors of [X Y]."
  (let* ((tr (plist-get topology :transform))
         (s (or (plist-get tr :scale) [1 1])) (d (or (plist-get tr :translate) [0 0])))
    (vconcat
     (mapcar (lambda (arc)
               (if (null tr) (vconcat (mapcar (lambda (p) (vector (aref p 0) (aref p 1))) arc))
                 (let ((x 0) (y 0))
                   (vconcat (mapcar (lambda (p)
                                      (setq x (+ x (aref p 0)) y (+ y (aref p 1)))
                                      (vector (+ (* x (aref s 0)) (aref d 0)) (+ (* y (aref s 1)) (aref d 1))))
                                    arc)))))
             (plist-get topology :arcs)))))

(defun eas-contour-geo--line (arcs indexes)
  "The points of the arcs INDEXES (negative for reversed) of ARCS, joined."
  (let (points)
    (seq-doseq (i indexes)
      (let ((a (append (aref arcs (if (< i 0) (lognot i) i)) nil)))
        (when points (pop points))
        (dolist (p (if (< i 0) (reverse a) a)) (push p points))))
    (nreverse points)))

(defun eas-contour-geo--ring (arcs indexes)
  "The closed ring of ARCS INDEXES."
  (let ((pts (eas-contour-geo--line arcs indexes)))
    (vconcat (if (< (length pts) 4) (append pts (list (car pts))) pts))))

(defun eas-contour-geo--topo-geometry (o arcs tr)
  "GeoJSON of TopoJSON geometry object O with decoded ARCS and transform TR."
  (let ((pt (lambda (p) (if tr (vector (+ (* (aref p 0) (aref (plist-get tr :scale) 0)) (aref (plist-get tr :translate) 0))
                                       (+ (* (aref p 1) (aref (plist-get tr :scale) 1)) (aref (plist-get tr :translate) 1)))
                          p)))
        (a (plist-get o :arcs)))
    (pcase (plist-get o :type)
      ("Polygon" (list :type "Polygon" :coordinates (vconcat (mapcar (lambda (r) (eas-contour-geo--ring arcs r)) a))))
      ("MultiPolygon" (list :type "MultiPolygon"
                            :coordinates (vconcat (mapcar (lambda (poly) (vconcat (mapcar (lambda (r) (eas-contour-geo--ring arcs r)) poly)))
                                                          a))))
      ("LineString" (list :type "LineString" :coordinates (vconcat (eas-contour-geo--line arcs a))))
      ("MultiLineString" (list :type "MultiLineString"
                               :coordinates (vconcat (mapcar (lambda (l) (vconcat (eas-contour-geo--line arcs l))) a))))
      ("Point" (list :type "Point" :coordinates (funcall pt (plist-get o :coordinates))))
      ("MultiPoint" (list :type "MultiPoint" :coordinates (vconcat (mapcar pt (plist-get o :coordinates))))))))

(defun eas-contour-geo-topojson-rows (topology &optional object)
  "Flat rows of TOPOLOGY's geometries (of OBJECT, else every object).
Each row holds object, id, the geometry's scalar properties and
geometry, its GeoJSON as JSON text."
  (unless (and (eas-object-p topology) (equal (plist-get topology :type) "Topology"))
    (eas-shape-invalid "A TopoJSON topology needs \"type\": \"Topology\", objects and arcs" nil "type"))
  (let* ((arcs (eas-contour-geo--arcs topology)) (tr (plist-get topology :transform))
         (objects (plist-get topology :objects)) out)
    (when (and object (not (plist-member objects (eas-key object))))
      (eas-shape-invalid (format "The topology has no object %s; it has %s" object
                                 (mapconcat #'eas-key-name (eas-plist-keys objects) ", "))
                         nil "object"))
    (cl-loop for (key o) on objects by #'cddr
             when (or (null object) (equal (eas-key-name key) object))
             do (dolist (g (if (equal (plist-get o :type) "GeometryCollection")
                               (append (plist-get o :geometries) nil)
                             (list o)))
                  (when-let* ((geo (eas-contour-geo--topo-geometry g arcs tr)))
                    (push (append (list :object (eas-key-name key))
                                  (when (plist-member g :id) (list :id (plist-get g :id)))
                                  (cl-loop for (pk pv) on (plist-get g :properties) by #'cddr
                                           when (or (numberp pv) (stringp pv)) append (list pk pv))
                                  (list :geometry (eas-json-encode geo)))
                          out))))
    (vconcat (nreverse out))))

(defun eas-contour-geo--adapt (input)
  "Convert INPUT to data/v1: a topology or {topology, object}, its text or file."
  (let ((value (if (eas-object-p input) input (eas-json-parse (eas-adapters--text input)))))
    (eas-data-from "plist" (if (plist-get value :topology)
                               (eas-contour-geo-topojson-rows (plist-get value :topology) (plist-get value :object))
                             (eas-contour-geo-topojson-rows value)))))

(eas-register-adapter
 "topojson"
 :doc "A TopoJSON topology, or {topology, object}: one row per geometry, its GeoJSON as text."
 :convert #'eas-contour-geo--adapt
 :example '(:type "Topology" :arcs [[[0 0] [1 0] [0 1] [-1 -1]]]
            :objects (:shapes (:type "GeometryCollection" :geometries [(:type "Polygon" :arcs [[0]] :id 1)]))))

(provide 'eas-contour-geo)
;;; eas-contour-geo.el ends here

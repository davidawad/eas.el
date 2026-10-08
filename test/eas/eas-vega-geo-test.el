;;; eas-vega-geo-test.el --- Vega gallery maps: projections, geoshape, templates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for eas-7r1.6: the d3-geo port (eas-geo-*.el), TopoJSON,
;; geoshape marks and the eight map templates of templates/vega/.
;; golden/vega-geo-d3.json holds d3-geo's (and d3-geo-projection's)
;; own paths of a few test geometries under every projection eas
;; draws, made by d3 with the same scale and translate; the paths must
;; agree to 0.01 px.  The template renders compare with the Vega
;; gallery's reference PNGs where rsvg-convert is installed.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-test-support)
(require 'eas-text-check)

(defconst eas-vega-geo-templates
  '(("world-map" . "pass") ("county-unemployment" . "pass") ("earthquakes" . "pass")
    ("projections" . "partial") ("zoomable-world-map" . "pass") ("distortion-comparison" . "pass")
    ("map-with-tooltip" . "pass") ("earthquakes-globe" . "partial"))
  "The map templates of templates/vega/ and the status each records.")

(defun eas-vega-geo-template (name)
  "Template NAME of templates/vega/, loaded."
  (eas-template-get (eas-template-load (eas-test-file "templates" "vega" (concat name ".json")))))

(defun eas-vega-geo-resolve (name)
  "Template NAME resolved with its example bindings."
  (let* ((tpl (eas-vega-geo-template name)) (default-directory eas-test-root))
    (eas-resolve tpl (eas-template-read-bindings (eas-template-example-file tpl)))))

(defun eas-vega-geo-marks (scene type)
  "SCENE's marks of TYPE, across its views."
  (seq-mapcat (lambda (v) (seq-filter (lambda (m) (equal (plist-get m :mark) type)) (plist-get v :marks)))
              (plist-get scene :views)))

(defun eas-vega-geo-world (&optional n)
  "The first N (all by default) country features of world-110m."
  (let ((rows (eas-topojson-features (eas-json-read-file (eas-test-file "test" "vega-examples" "data" "world-110m.json"))
                                     "countries")))
    (if n (seq-take rows n) rows)))

;;; Projections against d3

(defun eas-vega-geo--flat (path)
  "Absolute XY lists of PATH's rings and circles, as the golden holds them."
  (append (mapcar (lambda (p) (list (if (car p) t :false) (append (cdr p) nil))) (plist-get path :paths))
          (mapcar (lambda (c) (list :circle (append c nil))) (plist-get path :circles))))

(defun eas-vega-geo--golden-flat (paths)
  "The golden's PATHS in `eas-vega-geo--flat' form."
  (mapcar (lambda (p) (if (plist-get p :circle) (list :circle (append (plist-get p :circle) nil))
                        (list (if (eq (plist-get p :c) t) t :false) (append (plist-get p :p) nil))))
          paths))

(defun eas-vega-geo--near (pts line closed tol)
  "Non-nil when every point of flat PTS lies within TOL of polyline LINE.
LINE is a flat list too, a ring when CLOSED."
  (let* ((lv (vconcat line)) (n (/ (length lv) 2)))
    (cl-loop for (x y) on pts by #'cddr
             always (cl-loop for i from 0 below (if closed n (max 1 (1- n)))
                             for j = (mod (1+ i) n)
                             thereis (let* ((ax (aref lv (* 2 i))) (ay (aref lv (1+ (* 2 i))))
                                            (bx (aref lv (* 2 j))) (by (aref lv (1+ (* 2 j))))
                                            (dx (- bx ax)) (dy (- by ay)) (l2 (+ (* dx dx) (* dy dy)))
                                            (tt (if (> l2 0) (max 0 (min 1 (/ (+ (* (- x ax) dx) (* (- y ay) dy)) l2))) 0))
                                            (px (+ ax (* tt dx))) (py (+ ay (* tt dy))))
                                       (< (sqrt (+ (expt (- x px) 2) (expt (- y py) 2))) tol))))))

(defun eas-vega-geo--same-path (a b)
  "Non-nil when paths A and B (`eas-vega-geo--flat' entries) agree.
Rings and lines within 0.75 px of each other both ways (d3 resamples to
0.7 px; libm and V8 trigonometry may round a borderline subdivision
differently), circles to 0.01 px."
  (and (eq (car a) (car b))
       (if (or (eq (car a) :circle) (= (length (cadr a)) (length (cadr b))))
           (cl-every (lambda (u v) (< (abs (- u v)) 0.011)) (cadr a) (cadr b))
         (and (eas-vega-geo--near (cadr a) (cadr b) (eq (car a) t) 0.75)
              (eas-vega-geo--near (cadr b) (cadr a) (eq (car a) t) 0.75)))))

(defun eas-vega-geo--sort (paths)
  "PATHS with circles last, as eas groups them."
  (append (seq-remove (lambda (p) (eq (car p) :circle)) paths) (seq-filter (lambda (p) (eq (car p) :circle)) paths)))

(ert-deftest eas-vega-geo-paths-equal-d3 ()
  "Every projection draws d3's paths: antimeridian cuts, a polar ring,
a hole, a resampled meridian, the sphere and a point."
  (let* ((golden (eas-json-read-file (eas-test-file "test" "eas" "golden" "vega-geo-d3.json")))
         (geoms (plist-get golden :geometries)) (problems nil))
    (cl-loop for (type-key byg) on (plist-get golden :paths) by #'cddr
             for type = (eas-key-name type-key)
             for proj = (eas-geo-proj (list :type type :scale 60 :translate [200 150]))
             do (cl-loop for (g want) on byg by #'cddr
                         for got = (eas-vega-geo--flat (eas-geo-proj-path proj (plist-get geoms g)))
                         for exp = (eas-vega-geo--sort (eas-vega-geo--golden-flat (append want nil)))
                         unless (and (= (length got) (length exp)) (cl-every #'eas-vega-geo--same-path got exp))
                         do (push (format "%s %s" type (eas-key-name g)) problems)))
    (should (equal problems nil))
    (should (= (length (eas-plist-keys (plist-get golden :paths))) (length (eas-geo-raw-names))))))

;; eas-gzi: shapes' spherical streams are recorded once per rotation
;; and clip and replayed into each projection's resampler.
(ert-deftest eas-vega-geo-replayed-paths-equal-streamed ()
  "A replayed path is the path d3's stream draws, number for number:
every projection, a rotation, a clip angle and a clip extent, over the
d3 test geometries, a graticule and countries that cross the
antimeridian or hold a pole."
  (let* ((topo (eas-json-read-file (eas-test-file "examples" "data" "vega" "world-110m.json")))
         (countries (seq-filter (lambda (f) (memq (plist-get f :id) '(10 242 643)))
                                (eas-topojson-features topo "countries")))
         (golden (eas-json-read-file (eas-test-file "test" "eas" "golden" "vega-geo-d3.json")))
         (small (cl-loop for (_ g) on (plist-get golden :geometries) by #'cddr
                         unless (equal (plist-get g :type) "Sphere") collect g))
         (some '("equalEarth" "guyou" "interruptedMollweide" "polyhedralButterfly" "stereographic"))
         (specs (append (mapcar (lambda (ty) (list :type ty :scale 60 :translate [200 150])) (eas-geo-raw-names))
                        '((:type "orthographic" :scale 80 :translate [100 90] :rotate [30 -20 10])
                          (:type "equirectangular" :scale 60 :translate [200 150] :clipAngle 70 :rotate [-40 0])
                          (:type "mercator" :scale 60 :translate [200 150]
                                 :clipExtent [[20 30] [300 200]] :precision 0.3))))
         (problems nil))
    (should (= (length countries) 3))
    (dolist (spec specs)
      (let ((proj (eas-geo-proj spec)))
        (dolist (o (if (or (member (plist-get spec :type) some) (plist-get spec :rotate))
                       (append countries (list (eas-geo-graticule)) small)
                     small))
          (let ((want (let ((sink (eas-geo-path-sink 4.5)))
                        (eas-geo-stream-object o (funcall (plist-get proj :stream) sink))
                        (funcall (eas-geo-stream-result sink)))))
            ;; Twice: recorded, then replayed from the record.
            (dotimes (_ 2)
              (unless (equal (eas-geo-proj-path proj o) want)
                (push (format "%s %s" (plist-get spec :type) (or (plist-get o :id) (plist-get o :type))) problems)))))))
    (should (equal problems nil))))

(ert-deftest eas-vega-geo-graticule-is-d3s ()
  (let* ((g (eas-geo-graticule)) (lines (plist-get g :coordinates)))
    (should (equal (plist-get g :type) "MultiLineString"))
    ;; 4 major meridians, the equator, 32 minor meridians and 16 minor parallels.
    (should (= (length lines) 53))
    ;; d3's major meridians run from -90 + epsilon in steps of 90.
    (should (equal (aref lines 0) (vector (vector -180.0 (+ -90 eas-geo-eps)) (vector -180.0 (+ -90 eas-geo-eps 90))
                                          (vector -180.0 (- 90 eas-geo-eps)))))
    (should (= (length (plist-get (eas-geo-graticule '(:step [15 15])) :coordinates)) (+ 4 1 (- 24 4) (- 11 1))))))

(ert-deftest eas-vega-geo-fit-and-invert ()
  (let* ((objs (list '(:type "Polygon" :coordinates [[[-10 -10] [-10 10] [10 10] [10 -10] [-10 -10]]])))
         (fit (eas-geo-proj-fit '(:type "equirectangular") objs [0 0 200 100]))
         (proj (eas-geo-proj '(:type "equirectangular") (car fit) (cdr fit)))
         (b (let ((s (eas-geo-bounds-sink))) (eas-geo-stream-object (car objs) (funcall (plist-get proj :stream) s))
                 (funcall (eas-geo-stream-result s)))))
    ;; d3's fitExtent, bounds of the resampled path (its edges are great
    ;; circles, which bulge poleward): d3 gives scale 286.4789 and translate
    ;; [100, 50]; the box fills the height, centred.
    (should (< (abs (- (car fit) 286.4788975654116)) 1e-6))
    (should (< (abs (- (nth 0 (cdr fit)) 100)) 1e-6))
    (should (< (abs (- (nth 1 (cdr fit)) 50)) 1e-6))
    ;; d3's own path bounds at the fitted scale: [[50, -0.7554], [150, 100.7554]]
    (should (< (abs (- (aref b 0) 50)) 1e-6))
    (should (< (abs (- (aref b 1) -0.7554085552406704)) 1e-6))
    (should (< (abs (- (aref b 3) 100.75540855524068)) 1e-6))
    (let* ((p (eas-geo-proj '(:type "orthographic" :scale 100 :rotate [30 -20 0] :translate [50 50])))
           (xy (funcall (plist-get p :point) 12 34))
           (ll (eas-geo-proj-invert p (aref xy 0) (aref xy 1))))
      (should (< (abs (- (aref ll 0) 12)) 1e-6))
      (should (< (abs (- (aref ll 1) 34)) 1e-6)))))

(ert-deftest eas-vega-geo-path-measures ()
  (let* ((p (eas-geo-proj '(:type "equirectangular" :scale 1 :translate [0 0])))
         ;; a clockwise square: 10x10 px after projection, the y axis flipped
         (path (eas-geo-proj-path p '(:type "Polygon" :coordinates [[[0 0] [0 10] [10 10] [10 0] [0 0]]]) 0)))
    (should (= (length (plist-get path :paths)) 1))
    (should (< (abs (- (eas-geo-path-area path) (* (* 10 eas-geo-rad) (* 10 eas-geo-rad)))) 1e-9))
    (let ((c (eas-geo-path-centroid path)))
      (should (< (abs (- (aref c 0) (* 5 eas-geo-rad))) 1e-6))
      (should (< (abs (+ (aref c 1) (* 5 eas-geo-rad))) 1e-3)))))

;;; TopoJSON

(ert-deftest eas-vega-geo-topojson-features-and-mesh ()
  (let* ((topo (eas-json-read-file (eas-test-file "test" "vega-examples" "data" "world-110m.json")))
         (rows (eas-topojson-features topo "countries"))
         (first (aref rows 0)) (geom (plist-get first :geometry)))
    (should (= (length rows) 177))
    (should (equal (plist-get first :type) "Feature"))
    (should (equal (plist-get first :id) 4))
    (should (equal (plist-get geom :type) "Polygon"))
    ;; topojson-client's first vertex of Afghanistan, and its ring's length
    (let ((ring (aref (plist-get geom :coordinates) 0)))
      (should (equal (aref ring 0) [61.20961209612096 35.64924568531417]))
      (should (= (length ring) 69)))
    (let ((mesh (aref (eas-topojson-mesh topo "countries") 0)))
      (should (equal (plist-get mesh :type) "MultiLineString"))
      (should (> (length (plist-get mesh :coordinates)) 0)))
    (eas-test-should-code "NOT_FOUND" (eas-topojson-features topo "nope"))))

(ert-deftest eas-vega-geo-geojson-adapter ()
  (let ((default-directory eas-test-root))
    (should (= (length (plist-get (eas-data-from "geojson" '(:url "test/vega-examples/data/world-110m.json" :feature "countries")) :rows))
               177))
    (should (> (length (plist-get (eas-data-from "geojson" '(:url "test/vega-examples/data/earthquakes.json")) :rows)) 100)))
  (should (= (length (plist-get (eas-data-from "geojson" (plist-get (eas-adapter "geojson") :example)) :rows)) 1))
  (eas-test-should-code "SHAPE_INVALID" (eas-data-from "geojson" 42)))

;;; Vega-Lite maps

(defconst eas-vega-geo--square
  '(:type "Feature" :id "sq" :properties (:v 1)
    :geometry (:type "Polygon" :coordinates [[[0 0] [0 10] [10 10] [10 0] [0 0]]]))
  "A small clockwise square feature.")

(ert-deftest eas-vega-geo-geoshape-compiles-natively ()
  (let* ((spec (list :width 200 :height 100 :padding 0
                     :data (list :values (vector eas-vega-geo--square))
                     :projection '(:type "mercator" :scale 100 :translate [100 50])
                     :mark '(:type "geoshape" :stroke "red")
                     :encoding '(:tooltip [(:field "id" :type "nominal")])))
         (scene (eas-compile spec))
         (marks (eas-vega-geo-marks scene "geoshape"))
         (item (aref (plist-get (car marks) :items) 0)))
    (should (null (eas-spec-unsupported spec)))
    (should (= (length marks) 1))
    (should (equal (plist-get item :stroke) "red"))
    (should (eq (aref (aref (eas-geoshape-paths item) 0) 0) t))
    ;; the square spans 100 * 10 degrees in radians, east and north of the centre
    (let ((box (eas-geoshape-item-box item)))
      (should (< (abs (- (aref box 0) 100)) 0.6))
      (should (< (abs (- (aref box 2) (+ 100 (* 100 10 eas-geo-rad)))) 0.6)))
    (should (string-match-p "<path d=\"M[0-9.]+,[0-9.]+L" (eas-svg-render scene)))
    ;; hit test: inside the square, at distance zero
    (let ((hit (eas-hit scene nil (vector (+ 100 8) (- 50 8)))))
      (should (equal (plist-get hit :datum) 0))
      (should (< (plist-get hit :distance) 1e-6)))
    ;; the view is no cell: no frame
    (should (eq (plist-get (aref (plist-get scene :views) 0) :cell) :false))))

(ert-deftest eas-vega-geo-default-projection-fits ()
  "A geoshape without a projection is equalEarth fitted to the view."
  (let* ((scene (eas-compile (list :width 200 :height 100 :padding 0 :data (list :values (vector eas-vega-geo--square))
                                   :mark "geoshape")))
         (item (aref (plist-get (car (eas-vega-geo-marks scene "geoshape")) :items) 0))
         (box (eas-geoshape-item-box item)))
    ;; d3's fitSize: scale 494.9455, translate [62.788, 100]
    (should (equal (plist-get scene :size) '(:w 200 :h 100 :cell [7 14])))
    (should (< (abs (- (aref box 0) 62.78793388036829)) 1e-6))
    (should (< (abs (- (aref box 1) 0)) 1e-6))
    (should (< (abs (- (aref box 3) 100)) 1e-6))))

(ert-deftest eas-vega-geo-graticule-and-sphere-data ()
  (let* ((spec '(:width 300 :height 300 :padding 0 :projection (:type "orthographic")
                 :layer [(:data (:sphere t) :mark (:type "geoshape" :fill "aliceblue"))
                         (:data (:graticule (:step [30 30])) :mark "geoshape")]))
         (scene (eas-compile spec))
         (marks (eas-vega-geo-marks scene "geoshape"))
         (sphere (aref (plist-get (nth 0 marks) :items) 0))
         (grat (aref (plist-get (nth 1 marks) :items) 0)))
    (should (equal (plist-get sphere :fill) "aliceblue"))
    ;; a graticule's geoshape is a stroke, as in Vega-Lite
    (should (equal (plist-get grat :fill) "none"))
    (should (not (equal (plist-get grat :stroke) "none")))
    ;; the fitted sphere fills the 300 px square
    (let ((b (eas-geoshape-item-box sphere)))
      (should (< (abs (- (- (aref b 2) (aref b 0)) 300)) 1.5)))))

(ert-deftest eas-vega-geo-points-follow-the-projection ()
  "Longitude/latitude marks under a geoshape's projection share it."
  (let* ((spec '(:width 400 :height 400 :padding 0
                 :projection (:type "orthographic" :scale 100 :rotate [-20 0 0] :translate [200 200])
                 :layer [(:data (:sphere t) :mark "geoshape")
                         (:data (:values [(:lon 20 :lat 0) (:lon 50 :lat 30)])
                          :mark "circle"
                          :encoding (:longitude (:field "lon" :type "quantitative")
                                     :latitude (:field "lat" :type "quantitative")))]))
         (scene (eas-compile spec))
         (items (plist-get (car (eas-vega-geo-marks scene "circle")) :items))
         (p (funcall (plist-get (eas-geo-proj '(:type "orthographic" :scale 100 :rotate [-20 0 0] :translate [200 200])) :point)
                     50 30)))
    (should (< (abs (- (plist-get (aref items 0) :x) 200)) 1e-6))
    (should (< (abs (- (plist-get (aref items 0) :y) 200)) 1e-6))
    (should (< (abs (- (plist-get (aref items 1) :x) (aref p 0))) 1e-6))
    (should (< (abs (- (plist-get (aref items 1) :y) (aref p 1))) 1e-6))))

(ert-deftest eas-vega-geo-text-backend-draws-maps ()
  (let* ((spec (list :width 200 :height 100 :padding 0 :projection '(:type "equirectangular")
                     :layer (vector (list :data (list :values (vector eas-vega-geo--square)) :mark "geoshape")
                                    '(:data (:graticule t) :mark "geoshape"))))
         (text (eas-text-render (eas-compile (plist-put spec :projection '(:type "equirectangular" :scale 300 :translate [100 50]))
                                             :target 'text :size '(:cols 40 :rows 12)))))
    (should (string-match-p "█" text))
    (should (string-match-p "[⠁-⣿]" text))))

(ert-deftest eas-vega-geo-unsupported-projection-is-a-finding ()
  (let ((spec '(:data (:values []) :mark "geoshape" :projection (:type "bonne"))))
    (should (equal (mapcar (lambda (f) (plist-get f :feature)) (eas-spec-unsupported spec)) '("projection/bonne")))))

(ert-deftest eas-vega-geo-measure-transform ()
  (let* ((rows (eas-transform-apply-domain
                (list :x-eas:transform "geo-measure" :projection '(:type "equirectangular" :scale 100)
                      :size [200 100] :as ["cx" "cy" "area"])
                (vector eas-vega-geo--square) ""))
         (row (aref rows 0)) (k (* 100 10 eas-geo-rad)))
    (should (< (abs (- (plist-get row :area) (* k k))) 1e-6))
    (should (< (abs (- (plist-get row :cx) (+ 100 (/ k 2)))) 1e-3))
    (should (< (abs (- (plist-get row :cy) (- 50 (/ k 2)))) 1e-3))))

;;; Templates

(ert-deftest eas-vega-geo-templates-declare-their-status ()
  (dolist (entry eas-vega-geo-templates)
    (let* ((tpl (eas-vega-geo-template (car entry))) (meta (plist-get tpl :meta)))
      (should (equal (plist-get tpl :name) (car entry)))
      (should (equal (plist-get (plist-get meta :vega) :status) (cdr entry)))
      (should (stringp (plist-get (plist-get meta :vega) :note)))
      (should (file-exists-p (eas-template-example-file tpl))))))

(ert-deftest eas-vega-geo-world-map-renders-and-zooms ()
  (let* ((spec (eas-vega-geo-resolve "world-map"))
         (scene (eas-compile spec))
         (marks (eas-vega-geo-marks scene "geoshape")))
    (should (null (eas-spec-unsupported spec)))
    (should (equal (plist-get scene :size) '(:w 900 :h 500 :cell [7 14])))
    (should (= (length marks) 2))
    (should (= (length (plist-get (nth 1 marks) :items)) 177))
    ;; the type, scale and rotation are params: a bound input re-projects.
    (let* ((tpl (eas-vega-geo-template "world-map"))
           (eas-views (make-hash-table :test 'equal))
           (view (let ((default-directory eas-test-root))
                   (eas-view-open (plist-get tpl :name) :bindings (eas-template-read-bindings (eas-template-example-file tpl))))))
      (eas-dispatch view '(:type "param" :param "type" :value "orthographic"))
      (let* ((m (car (eas-vega-geo-marks (eas-view-scene view) "geoshape"))))
        (should (equal (plist-get (plist-get (plist-get m :geo) :resolved) :type) "orthographic")))
      ;; hovering a country outlines it in firebrick
      (let* ((scene (eas-view-scene view)) (marks (eas-vega-geo-marks scene "geoshape"))
             (item (seq-find (lambda (i) (and (> (plist-get i :x) 450)
                                              (eas-geoshape--inside-p i (plist-get i :x) (plist-get i :y))))
                             (plist-get (nth 1 marks) :items)))
             (px (vector (plist-get item :x) (plist-get item :y))))
        (eas-dispatch view (list :type "pointermove" :px px))
        (let ((now (plist-get (nth 1 (eas-vega-geo-marks (eas-view-scene view) "geoshape")) :items)))
          (should (equal (mapcar (lambda (i) (plist-get i :datum))
                                 (seq-filter (lambda (i) (equal (plist-get i :stroke) "firebrick")) now))
                         (list (plist-get item :datum)))))))))

(ert-deftest eas-vega-geo-zoomable-map-wheel-and-drag ()
  (let* ((tpl (eas-vega-geo-template "zoomable-world-map"))
         (eas-views (make-hash-table :test 'equal))
         (view (let ((default-directory eas-test-root))
                 (eas-view-open (plist-get tpl :name) :bindings (eas-template-read-bindings (eas-template-example-file tpl)))))
         (params (lambda () (plist-get (eas-view-state view) :params))))
    (eas-dispatch view '(:type "wheel" :px [450 250] :delta -1))
    (should (< (abs (- (plist-get (funcall params) :scale) (* 150 eas-zoom-wheel-step))) 1e-9))
    ;; Vega's handler: rotateX gains the dragged longitude, centerY the latitude.
    (eas-dispatch view '(:type "drag" :from [450 250] :to [500 250]))
    (let ((k (* 150 eas-zoom-wheel-step)))
      (should (< (abs (- (plist-get (funcall params) :rotateX) (/ 50 k eas-geo-rad))) 1e-6)))
    (eas-dispatch view '(:type "wheel" :px [450 250] :delta -100))
    (should (= (plist-get (funcall params) :scale) 3000))
    (eas-dispatch view '(:type "drag" :from [450 250] :to [450 -200000]))
    (should (= (plist-get (funcall params) :centerY) -60))
    ;; the scene follows the params
    (let ((m (car (eas-vega-geo-marks (eas-view-scene view) "geoshape"))))
      (should (= (plist-get (plist-get (plist-get m :geo) :resolved) :scale) 3000)))))

(ert-deftest eas-vega-geo-globe-rotates-and-pauses ()
  "The globe turns a degree a tick (180 follows -180); space pauses it."
  (let* ((tpl (eas-vega-geo-template "earthquakes-globe"))
         (eas-views (make-hash-table :test 'equal)) (eas-plays (make-hash-table :test 'equal))
         (view (let ((default-directory eas-test-root))
                 (eas-view-open (plist-get tpl :name) :bindings (eas-template-read-bindings (eas-template-example-file tpl)))))
         (angle (lambda () (plist-get (eas-compile--env (eas-view-spec view) (eas-view-state view)) :angle))))
    (eas-play-tick view 100.0)
    (eas-play-tick view 200.0)
    (should (= (funcall angle) -2))
    (eas-play-key view "space")
    (eas-play-tick view 300.0)
    (should (= (funcall angle) -2))
    (eas-play-key view "space")
    (eas-dispatch view '(:type "param" :param "angle" :value -180))
    (eas-play-tick view 400.0)
    (should (= (funcall angle) 180))))

(ert-deftest eas-vega-geo-choropleth-joins-and-quantizes ()
  (let* ((tpl (eas-vega-geo-template "county-unemployment"))
         (shapes (vector (append '(:type "Feature" :id 1) (cddr (cddr eas-vega-geo--square)))
                         (append '(:type "Feature" :id 2) (cddr (cddr eas-vega-geo--square)))
                         (append '(:type "Feature" :id 3) (cddr (cddr eas-vega-geo--square)))))
         (spec (eas-resolve tpl (list :shapes shapes :rates "id\trate\n1\t0.01\n2\t0.149\n"
                                      :projection '(:type "equirectangular"))))
         (scene (eas-compile spec))
         (items (plist-get (car (eas-vega-geo-marks scene "geoshape")) :items)))
    ;; the shape with no rate is filtered out, as Vega's example does
    (should (= (length items) 2))
    ;; quantize over the explicit [0, 0.15] into 7 blues: the ends' colors
    (should (equal (mapcar (lambda (i) (plist-get i :fill)) items)
                   (list (aref (eas-scheme-discrete-range "blues" 7) 0) (aref (eas-scheme-discrete-range "blues" 7) 6))))))

(defun eas-vega-geo-small-bindings (name)
  "Template NAME's example bindings with less data, for a fast render.
Twelve countries; for the choropleths, the shapes of a few counties."
  (let* ((tpl (eas-vega-geo-template name))
         (b (let ((default-directory eas-test-root)) (eas-template-read-bindings (eas-template-example-file tpl)))))
    (when (plist-get b :world) (setq b (plist-put b :world (eas-vega-geo-world 12))))
    (when (plist-get b :projections) (setq b (plist-put b :projections (seq-take (plist-get b :projections) 4))))
    (when (plist-get b :shapes)
      (setq b (plist-put b :shapes (seq-take (eas-topojson-features
                                              (eas-json-read-file (eas-test-file "test" "vega-examples" "data" "us-10m.json"))
                                              "counties")
                                             60))))
    (when (plist-get b :rates)
      (setq b (plist-put b :rates (list :file (eas-test-file "test" "vega-examples" "data" "unemployment.tsv")))))
    (when (plist-get b :quakes)
      (setq b (plist-put b :quakes (list :url (eas-test-file "test" "vega-examples" "data" "earthquakes.json")))))
    (cons tpl b)))

(ert-deftest eas-vega-geo-templates-render-natively ()
  "Every map template renders in SVG and text, natively (with less data)."
  (dolist (entry eas-vega-geo-templates)
    (let* ((tb (eas-vega-geo-small-bindings (car entry)))
           (spec (eas-resolve (car tb) (cdr tb))))
      (should (null (eas-spec-unsupported spec)))
      (should (eas-vega-geo-marks (eas-compile spec) "geoshape"))
      (should (string-match-p "<path d=\"M" (eas-svg-render (eas-compile spec))))
      (should (stringp (eas-text-render (eas-compile spec :target 'text :size '(:cols 80 :rows 24))))))))

(ert-deftest eas-vega-geo-text-check-holds ()
  "Each map template's text rendering passes `eas-text-check' (less data)."
  (dolist (entry eas-vega-geo-templates)
    (let* ((tb (eas-vega-geo-small-bindings (car entry)))
           (scene (eas-compile (eas-resolve (car tb) (cdr tb)) :target 'text :size '(:cols 60 :rows 20))))
      (should (equal (cons (car entry) (eas-text-check scene 60)) (list (car entry)))))))

(defconst eas-vega-geo-ratios
  '(("world-map" . 0.03) ("county-unemployment" . 0.03) ("earthquakes" . 0.03) ("zoomable-world-map" . 0.03)
    ("distortion-comparison" . 0.03) ("map-with-tooltip" . 0.01) ("earthquakes-globe" . 0.03) ("projections" . 0.09))
  "Pixel ratio each template's render stays within against its reference.
earthquakes-globe's title room is Vega's titleLayout union plus its
offset 4 and -dy 10 (eas-7r1.13): 658 px tall as the reference.
Measured against the vg2png (node-canvas) references with rsvg-convert
and Arimo (eas-7r1.12): world-map 0.0000, county-unemployment 0.0078,
earthquakes 0.0067, zoomable-world-map 0.0000, distortion-comparison
0.0000, earthquakes-globe 0.0170 (partial) and projections 0.0791
against its 360 px thumbnail (partial); map-with-tooltip 0.0008 with
Vega's legend look (eas-7r1.12b; 0.0073 before).")

(ert-deftest eas-vega-geo-templates-match-their-references ()
  "Each map template against test/vega-examples/ref/NAME.png (needs rsvg-convert)."
  (unless (executable-find "rsvg-convert")
    (eas-test-skip "rsvg-convert not on PATH; install librsvg to compare the map templates with their Vega references"))
  (let (problems)
    (dolist (entry eas-vega-geo-ratios)
      (let* ((name (car entry))
             (svg (make-temp-file "eas-vega-geo" nil ".svg")) (png (make-temp-file "eas-vega-geo" nil ".png"))
             (ref (eas-test-file "test" "vega-examples" "ref" (concat name ".png"))))
        (unwind-protect
            (progn
              (with-temp-file svg (insert (eas-svg-render (eas-compile (eas-vega-geo-resolve name)))))
              ;; the projections reference is the Vega site's 360 px wide thumbnail
              (apply #'call-process "rsvg-convert" nil nil nil
                     (append (and (equal name "projections") '("-w" "360")) (list "-o" png svg)))
              (let ((r (eas-png-compare (eas-png-read png) (eas-png-read ref))))
                (when (> (plist-get r :ratio) (cdr entry))
                  (push (format "%s: ratio %.4f > %s" name (plist-get r :ratio) (cdr entry)) problems))))
          (delete-file svg) (delete-file png))))
    (should (equal problems nil))))

(ert-deftest eas-vega-geo-examples-render ()
  "Each map template's own example renders natively, all of its data.
The 3000-county choropleths and the 24-map grid render theirs in
`eas-vega-geo-templates-match-their-references' (seconds byte-compiled,
too slow interpreted for make test); here they render with less data."
  (dolist (entry eas-vega-geo-templates)
    (unless (member (car entry) '("county-unemployment" "map-with-tooltip" "projections"))
      (let* ((spec (eas-vega-geo-resolve (car entry))) (scene (eas-compile spec)))
        (should (null (eas-spec-unsupported spec)))
        (should (eas-vega-geo-marks scene "geoshape"))))))

;;; Precision and simplification (eas-gzi): visually lossless

(defun eas-vega-geo--full-rings (x y paths)
  "PATHS moved by X Y as SVG path data, every number as `eas-svg--n' writes it."
  (mapconcat (lambda (p)
               (let ((flat (aref p 1)) (out nil))
                 (dotimes (i (/ (length flat) 2))
                   (push (format "%s%s,%s" (if (= i 0) "M" "L") (eas-svg--n (+ x (aref flat (* 2 i))))
                                 (eas-svg--n (+ y (aref flat (1+ (* 2 i))))))
                         out))
                 (concat (apply #'concat (nreverse out)) (if (eq (aref p 0) t) "Z" ""))))
             paths ""))

(defun eas-vega-geo--png (svg zoom)
  "SVG string rasterized by rsvg-convert at ZOOM, decoded (`eas-png-read')."
  (let ((in (make-temp-file "eas-geo-raster" nil ".svg")) (out (make-temp-file "eas-geo-raster" nil ".png")))
    (unwind-protect
        (progn (with-temp-file in (insert svg))
               (call-process "rsvg-convert" nil nil nil "-b" "white" "-z" (number-to-string zoom) "-o" out in)
               (eas-png-read out))
      (delete-file in) (delete-file out))))

(ert-deftest eas-vega-geo-gallery-simplified-paths-rasterize-as-the-full-paths ()
  "Each map template, drawn with sub-pixel simplified paths printed to
a tenth of a pixel, differs from its full paths printed to hundredths
in at most 0.1% of its pixels at 1x and 2x (needs rsvg-convert).
About a minute byte-compiled, so it runs with the gallery."
  :tags '(:gallery)
  (unless (executable-find "rsvg-convert")
    (eas-test-skip "rsvg-convert not on PATH; install librsvg to rasterize the map templates"))
  (require 'eas-png)
  (let (problems)
    (dolist (entry eas-vega-geo-templates)
      (let* ((name (car entry)) (spec (eas-vega-geo-resolve name))
             (new (progn (eas-geoshape-forget) (eas-svg-render (eas-compile spec))))
             (old (progn (eas-geoshape-forget)
                         ;; The Elisp backend: the native one prints its own path data.
                         (cl-letf (((symbol-function 'eas-geoshape--svg-rings) #'eas-vega-geo--full-rings))
                           (let ((eas-geoshape-tolerance nil) (eas-geo-backend 'lisp))
                             (eas-svg-render (eas-compile spec)))))))
        (eas-geoshape-forget)
        (dolist (zoom '(1 2))
          (let* ((a (eas-vega-geo--png old zoom)) (b (eas-vega-geo--png new zoom))
                 (count (eas-png--count a b 0 0 (eas-png--background a)
                                        (* 35215 eas-png-pixel-threshold eas-png-pixel-threshold)))
                 (ratio (/ (car count) (float (cdr count)))))
            (when (> ratio 0.001)
              (push (format "%s at %dx: %.4f%% of pixels differ" name zoom (* 100 ratio)) problems))))))
    (should (equal problems nil))))

(ert-deftest eas-vega-geo-svg-string-prints-as-the-node ()
  "`eas-geoshape-svg-string' is the text `eas-geoshape-svg''s node prints to."
  (let ((base (list :x 10.5 :y 20.25 :paths (vector (vector t [0.0 0.0 5.0 0.0 5.0 5.0]) (vector :false [1.0 1.0 2.0 3.0]))
                    :circles nil :box [0.0 0.0 5.0 5.0])))
    (dolist (extra '(() (:fill "#4c78a8") (:fill "#4c78a8" :stroke "white" :strokeWidth 0.5)
                     (:stroke "none" :strokeWidth 2) (:stroke "black" :outline 2) (:outline 1.5)
                     (:fill "a&b" :fillOpacity 0.5 :strokeOpacity 0.25 :strokeDashOffset 1 :strokeMiterLimit 4)
                     (:stroke "red" :strokeCap "round" :strokeJoin "bevel" :strokeDash [4 2] :blend "multiply")
                     (:circles [[1.0 2.0 3.0]] :fill "red")))
      (let* ((item (append extra base))
             (fill (plist-get item :fill)) (stroke (plist-get item :stroke)))
        (dolist (opacity '(nil 0.5))
          (should (equal (eas-geoshape-svg-string item fill stroke opacity)
                         (eas-svg-retain-string (list (eas-geoshape-svg item fill stroke opacity))))))))))

(provide 'eas-vega-geo-test)
;;; eas-vega-geo-test.el ends here

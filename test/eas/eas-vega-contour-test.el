;;; eas-vega-contour-test.el --- Vega gallery: contours, density heatmaps, rasters, vectors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.7: the kde2d, isocontour, heatmap, geopath and geopoints
;; transforms, the grid and topojson adapters, the character grid's
;; path and image cells, and the five Vega gallery templates they make
;; possible (templates/vega/: contour-plot, density-heatmaps,
;; volcano-contours, annual-precipitation, wind-vectors).  Expected
;; numbers come from d3-contour, d3-geo, topojson-client and Vega 6
;; run on the same inputs.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-contour)
(require 'eas-png)
(require 'eas-chart)

(defun eas-vega-contour--near (a b &optional eps)
  "Non-nil when A and B differ by less than EPS (default 1e-6)."
  (< (abs (- a b)) (or eps 1e-6)))

(defun eas-vega-contour--relative (a b &optional eps)
  "Non-nil when A and B agree to relative EPS (default 1e-6)."
  (< (abs (- a b)) (* (or eps 1e-6) (max (abs a) (abs b) 1e-12))))

(defun eas-vega-contour--rings (coords)
  "COORDS (vectors of polygons of rings of [X Y]) as nested lists of numbers."
  (mapcar (lambda (poly) (mapcar (lambda (ring) (mapcar (lambda (p) (list (aref p 0) (aref p 1))) ring)) poly))
          coords))

(defun eas-vega-contour--same-rings (a b)
  "Non-nil when nested ring lists A and B agree to 1e-9."
  (and (= (length a) (length b))
       (cl-every (lambda (x y)
                   (if (numberp x) (eas-vega-contour--near x y 1e-9)
                     (eas-vega-contour--same-rings x y)))
                 a b)))

(defmacro eas-vega-contour--with-templates (&rest body)
  "Run BODY with templates/vega/ registered under the vega namespace only."
  (declare (indent 0))
  `(let* ((dir (file-name-as-directory (eas-test-file "templates/vega")))
          (eas-template-directories (list dir))
          (eas-template-namespaces (list (cons dir "vega")))
          (eas--templates nil)
          (eas-template-load-errors nil))
     ,@body))

(defconst eas-vega-contour--names
  '("contour-plot" "density-heatmaps" "volcano-contours" "annual-precipitation" "wind-vectors")
  "The Vega gallery examples this bead covers.")

(defvar eas-vega-contour--resolved (make-hash-table :test 'equal)
  "Example name -> its resolved spec, so the slow resolves run once.")

(defun eas-vega-contour--resolve (name)
  "The resolved Vega-Lite spec of template vega/NAME with its example."
  (or (gethash name eas-vega-contour--resolved)
      (puthash name (eas-vega-contour--with-templates
                      (eas-resolve (concat "vega/" name) (eas-template-example (concat "vega/" name))))
               eas-vega-contour--resolved)))

(defun eas-vega-contour--meta (name)
  "The x-eas.vega block of template NAME."
  (plist-get (plist-get (eas-json-read-file (eas-test-file "templates/vega" (concat name ".json"))) :x-eas) :vega))

;;; Marching squares and isocontour

(defconst eas-vega-contour--ring-grid
  [0 0 0 0 0 0  0 5 5 5 5 0  0 5 1 1 5 0  0 5 5 5 5 0  0 0 0 0 0 0]
  "A 6x5 grid: a ring of 5 around a hollow of 1.")

(ert-deftest eas-vega-contour-marching-squares-match-d3 ()
  "A ring with a hole, smoothed and not, as d3-contour's contours() draws it."
  (let ((values (vconcat (mapcar #'float eas-vega-contour--ring-grid))))
    (should (eas-vega-contour--same-rings
             (eas-vega-contour--rings (eas-contour-polygons values 6 5 3 t))
             '((((4.9 3.5) (4.9 2.5) (4.9 1.5) (4.5 1.1) (3.5 1.1) (2.5 1.1) (1.5 1.1) (1.1 1.5) (1.1 2.5)
                 (1.1 3.5) (1.5 3.9000000000000004) (2.5 3.9000000000000004) (3.5 3.9000000000000004)
                 (4.5 3.9000000000000004) (4.9 3.5))
                ((3.5 3) (2.5 3) (2 2.5) (2.5 2) (3.5 2) (4 2.5) (3.5 3))))))
    (should (eas-vega-contour--same-rings
             (eas-vega-contour--rings (eas-contour-polygons values 6 5 3 nil))
             '((((5 3.5) (5 2.5) (5 1.5) (4.5 1) (3.5 1) (2.5 1) (1.5 1) (1 1.5) (1 2.5) (1 3.5) (1.5 4)
                 (2.5 4) (3.5 4) (4.5 4) (5 3.5))
                ((3.5 3) (2.5 3) (2 2.5) (2.5 2) (3.5 2) (4 2.5) (3.5 3))))))
    ;; Nothing at or above the threshold: no polygon.
    (should-not (eas-contour-polygons values 6 5 9 t))))

(ert-deftest eas-vega-contour-volcano-rings-match-d3 ()
  "The volcano grid's rings per polygon at four thresholds, and a first vertex run."
  (let* ((g (eas-contour-grid-normalize (eas-json-read-file (eas-test-file "test/vega-examples/data/volcano.json"))))
         (res (mapcar (lambda (v) (eas-contour-polygons (plist-get g :values) 87 61 v t)) '(100 130 160 190))))
    (should (equal (mapcar (lambda (c) (mapcar (lambda (p) (mapcar #'length p)) c)) res)
                   '(((303)) ((259)) ((165 31)) ((49)))))
    (should (eas-vega-contour--same-rings
             (mapcar (lambda (p) (list (aref p 0) (aref p 1))) (seq-take (car (car (nth 1 res))) 4))
             '((25.833333333333332 58.5) (26.5 57.833333333333336) (27 57.5) (27.5 57.166666666666664))))))

(ert-deftest eas-vega-contour-isocontour-levels-and-mapping ()
  "Levels spread over [0, max] as Vega's quantize; a flipping map keeps the winding."
  (should (equal (eas-contour-quantize 3 nil t [0 4 8]) '(2.0 4.0 6.0)))
  (should (equal (eas-contour-quantize 4 t t [0 4 8]) '(2.0 4.0 6.0)))
  (let* ((rows (eas-contour-grid-rows (list :width 6 :height 5 :values eas-vega-contour--ring-grid
                                            :scale [1 -1] :translate [-3 2])))
         (out (eas-contour-isocontour rows '(:x "x" :y "y" :value "value" :thresholds [3] :as "contour")))
         (c (plist-get (aref out 0) :contour)) (outer (aref (aref (plist-get c :coordinates) 0) 0)))
    (should (= (length out) 1))
    (should (equal (plist-get c :type) "MultiPolygon"))
    (should (= (plist-get (aref out 0) :threshold) 3))
    ;; x = c - 3, y = 2 - c: the first smoothed vertex (4.9 3.5) of the
    ;; reversed ring is now its last.
    (should (eas-vega-contour--near (aref (aref outer (1- (length outer))) 0) 1.9 1e-9))
    (should (eas-vega-contour--near (aref (aref outer (1- (length outer))) 1) -1.5 1e-9))
    ;; Flipping y and reversing the ring keeps its area's sign: still an exterior.
    (should (> (eas-contour-ring-area (append outer nil)) 0))))

(ert-deftest eas-vega-contour-grid-adapter-round-trips ()
  "A Vega grid object becomes tidy cell rows and back, with its mapping."
  (let* ((data (eas-data-from "grid" '(:width 3 :height 2 :values [1 2 3 4 5 :null] :scale [2 -1] :translate [10 5])))
         (rows (plist-get data :rows))
         (grid (eas-contour-grid-from-rows (append rows nil) "x" "y" "value")))
    (should (= (length rows) 6))
    (should (equal (aref rows 0) '(:x 11.0 :y 4.5 :value 1.0)))
    (should (equal (plist-get grid :values) [1.0 2.0 3.0 4.0 5.0 nil]))
    (should (equal (eas-contour-grid-mapping grid) '(2.0 -1.0 10.0 5.0 0 0))))
  (should (equal (plist-get (eas-test-should-code "SHAPE_INVALID" (eas-data-from "grid" '(:width 2 :height 2 :values [1])))
                            :field)
                 "values")))

;;; kde2d

(ert-deftest eas-vega-contour-kde2d-matches-vega ()
  "cars.json by Origin on 500x400 pixels, counts: Vega's grids, sums and maxima."
  (let* ((rows (eas-json-read-file (eas-test-file "test/vega-examples/data/cars.json")))
         (out (eas-kde2d rows '(:x "Horsepower" :y "Miles_per_Gallon" :size [500 400] :groupby ["Origin"]
                                :cellSize 4 :bandwidth [-1 -1] :counts t :as "grid")))
         ;; Vega 6's density grids (Float32): width height x1 y1 sum max.
         (expected '(("USA" 143 112 9 6 15.32375477023414 0.013590357266366482)
                     ("Japan" 137 114 6 7 4.938736672652951 0.008795162662863731)
                     ("Europe" 135 112 5 6 4.250000168430688 0.008410168811678886))))
    (should (= (length out) 3))
    (cl-loop for row across out for (origin w h x1 y1 sum max) in expected
             for g = (plist-get row :grid)
             do (should (equal (plist-get row :Origin) origin))
             (should (equal (list (plist-get g :width) (plist-get g :height) (plist-get g :x1) (plist-get g :y1))
                            (list w h x1 y1)))
             (should (eas-vega-contour--relative (cl-reduce #'+ (plist-get g :values)) sum 1e-6))
             (should (eas-vega-contour--relative (eas-contour-grid-max g) max 1e-6))
             ;; The nice zero domains the chart's scales take.
             (should (equal (list (plist-get row :grid_x0) (plist-get row :grid_x1)
                                  (plist-get row :grid_y0) (plist-get row :grid_y1))
                            '(0.0 240.0 0.0 50.0))))
    ;; Vega's shared levels: 3 over every grid's maximum.
    (let ((c (eas-contour-isocontour out '(:field "grid" :levels 3 :resolve "shared" :as "contour"))))
      (should (= (length c) 9))
      (should (eas-vega-contour--relative (plist-get (aref c 0) :threshold) 0.0033975893165916204 1e-6))
      (should (equal (mapcar (lambda (r) (length (plist-get (plist-get r :contour) :coordinates))) c)
                     '(1 1 2 1 1 0 1 1 0))))))

(ert-deftest eas-vega-contour-kde2d-bandwidth-is-scotts-rule ()
  "vega-statistics estimateBandwidth: 1.06 min(sd, IQR/1.34) n^-1/5."
  (should (eas-vega-contour--near (eas-kde2d-bandwidth '(1 2 3 4 5)) (* 1.06 (/ 2 1.34) (expt 5 -0.2)) 1e-12))
  (should (eas-vega-contour--near (eas-kde2d-bandwidth '(3 3 3)) (* 1.06 3 (expt 3 -0.2)) 1e-12)))

;;; heatmap

(ert-deftest eas-vega-contour-png-round-trips ()
  "The native PNG encoder writes what eas-png.el reads back."
  (let* ((rgba (unibyte-string 255 0 0 255  0 255 0 128  0 0 255 0  10 20 30 40  1 2 3 4  250 251 252 253))
         (file (make-temp-file "eas-vega-contour" nil ".png")))
    (unwind-protect
        (progn
          (with-temp-file file (set-buffer-multibyte nil) (insert (eas-contour-png 3 2 rgba)))
          (let ((img (eas-png-read file)))
            (should (equal (list (plist-get img :w) (plist-get img :h)) '(3 2)))
            (should (equal (plist-get img :rgba) rgba))))
      (delete-file file))))

(ert-deftest eas-vega-contour-heatmap-colors-and-opacity ()
  "A constant color fades with value / max; a scheme maps it; opacity fixes alpha."
  (let* ((rows [(:g "a" :x 0 :y 0 :value 0) (:g "a" :x 1 :y 0 :value 2) (:g "a" :x 0 :y 1 :value 4) (:g "a" :x 1 :y 1 :value 1)])
         (img (lambda (params)
                (let ((url (plist-get (aref (eas-contour-heatmap rows (append params '(:as "image" :groupby ["g"]))) 0) :image)))
                  (eas-contour-text--png-data url)))))
    (let ((d (funcall img '(:color "#ff0000"))))
      (should (equal (list (plist-get d :w) (plist-get d :h)) '(2 2)))
      (should (equal (append (plist-get d :rgba) nil) '(255 0 0 0  255 0 0 127  255 0 0 255  255 0 0 63))))
    (let ((d (funcall img '(:color (:scheme "viridis") :opacity 1))))
      (should (equal (append (substring (plist-get d :rgba) 0 4) nil) '(#x44 #x01 #x54 255)))
      (should (equal (append (substring (plist-get d :rgba) 8 12) nil) '(#xfd #xe7 #x25 255))))))

;;; geopath, geopoints, topojson

(ert-deftest eas-vega-contour-natural-earth1-matches-d3 ()
  "geoNaturalEarth1().scale(110).translate([300,150]) of three points."
  (let ((p (eas-contour-geo-projection '(:type "naturalEarth1" :scale 110 :translate [300 150]) [600 300] nil)))
    (cl-loop for (lon lat x y) in '((0 0 300 150) (100 45 450.5770173102453 62.764346614621815)
                                    (-170 -60 67.28613214738624 264.90272917033013))
             for q = (funcall p lon lat)
             do (should (eas-vega-contour--near (car q) x 1e-6))
             (should (eas-vega-contour--near (cdr q) y 1e-6)))))

(ert-deftest eas-vega-contour-geopath-writes-svg-path-data ()
  "Rings close with Z, every subpath starts with M; fitted identity; empty rows drop."
  (let* ((square '(:type "MultiPolygon" :coordinates [[[[0 0] [10 0] [10 10] [0 0]] [[2 2] [2 4] [4 2] [2 2]]]]))
         (out (eas-contour-geopath (vector (list :k 1 :g square) (list :k 2 :g '(:type "MultiPolygon" :coordinates [])))
                                   '(:field "g" :size [20 20] :as "path"))))
    (should (= (length out) 1))
    (should (equal (aref out 0) '(:k 1 :path "M0,0L20,0L20,20L0,0ZM4,4L4,8L8,4L4,4Z")))
    (should (equal (eas-contour-geo-path '(:type "LineString" :coordinates [[0 0] [1.005 2]]) #'cons)
                   "M0,0L1,2"))))

(ert-deftest eas-vega-contour-geopoints-cut-lines-to-the-box ()
  "Vertex rows per ring, and a line leaving the box breaks into runs."
  (let* ((line '(:type "LineString" :coordinates [[0 5] [5 5] [15 5] [15 8] [5 8]]))
         (out (eas-contour-geopoints (vector (list :geometry line)) '(:field "geometry" :clip [0 0 10 10]
                                                                        :ring "ring" :order "order" :as ["x" "y"]))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :ring) (plist-get r :order) (plist-get r :x) (plist-get r :y)))
                           out)
                   '((0 0 0.0 5.0) (0 1 5.0 5.0) (0 2 10.0 5.0) (1 0 10.0 8.0) (1 1 5.0 8.0))))))

(ert-deftest eas-vega-contour-topojson-matches-topojson-client ()
  "world-110m's countries: 177 rows, the first (id 4) as topojson-client decodes it."
  (let* ((rows (plist-get (eas-data-from "topojson" (list :object "countries"
                                                          :topology (eas-json-read-file (eas-test-file "test/vega-examples/data/world-110m.json"))))
                          :rows))
         (g (eas-json-parse (plist-get (aref rows 0) :geometry)))
         (p (aref (aref (plist-get g :coordinates) 0) 0)))
    (should (= (length rows) 177))
    (should (equal (plist-get (aref rows 0) :object) "countries"))
    (should (= (plist-get (aref rows 0) :id) 4))
    (should (equal (plist-get g :type) "Polygon"))
    (should (eas-vega-contour--near (aref p 0) 61.20961209612096 1e-9))
    (should (eas-vega-contour--near (aref p 1) 35.64924568531417 1e-9)))
  (should (equal (plist-get (eas-test-should-code "SHAPE_INVALID"
                              (eas-data-from "topojson" '(:object "lakes" :topology (:type "Topology" :arcs [] :objects (:land nil)))))
                            :field)
                 "object")))

;;; The character grid

(ert-deftest eas-vega-contour-text-fills-path-cells ()
  "A path symbol covers the cells whose centres it holds; holes stay empty."
  (let ((cells (eas-contour-text-path-cells "M0,0L40,0L40,40L0,40ZM10,10L10,30L30,30L30,10Z" 0 0 4 10 10)))
    (should (= (length cells) 12))
    (should (member '(0 . 0) cells))
    (should-not (member '(1 . 1) cells)))
  (should (eq (eas-contour-text-path-cells "M100,100L101,100L101,101Z" 0 0 4 10 10) 'empty))
  (should-not (eas-contour-text-path-cells "circle" 0 0 4 10 10))
  ;; Curves make a symbol icon (an isotype's): it keeps its one glyph.
  (should-not (eas-contour-text-path-cells "M1.7 -1.7h-0.8c0.3 -0.2 0.6 -0.5 0.6 -0.9Z" 0 0 400 10 10))
  ;; Stroked only: the cells its edges cross.
  (should (equal (length (eas-contour-text-path-cells "M0,0L40,0" 0 0 4 10 10 t)) 5)))

(ert-deftest eas-vega-contour-text-samples-png-images ()
  "An image of PNG data colors each cell with the pixel under its centre."
  (let* ((url (eas-contour-png-url 2 1 (unibyte-string 255 0 0 255  0 0 255 128)))
         (cells (eas-contour-text-image-cells url 0 0 40 10 10 10)))
    (should (equal (mapcar (lambda (c) (seq-take c 3)) cells)
                   '((0 0 "#ff0000") (1 0 "#ff0000") (2 0 "#0000ff") (3 0 "#0000ff"))))
    (should (eas-vega-contour--near (nth 3 (nth 3 cells)) (/ 128 255.0)))
    (should-not (eas-contour-text-image-cells "pic.png" 0 0 40 10 10 10))))

;;; Engine fixes the templates need

(ert-deftest eas-vega-contour-shape-scale-null-draws-path-data ()
  "shape with scale null passes each row's SVG path through, as Vega-Lite."
  (let* ((spec '(:width 100 :height 100 :data (:values [(:p "M0,0L50,0L50,50Z")])
                 :mark (:type "point" :filled t :opacity 1)
                 :encoding (:x (:value 0) :y (:value 0) :size (:value 4)
                            :shape (:field "p" :type "nominal" :scale :null))))
         (svg (let ((eas-spec-supported-function nil)) (eas-svg-render (eas-compile spec)))))
    (should (string-match-p "<path d=\"M0,0L50,0L50,50Z\" transform=\"translate([0-9.]+,[0-9.]+) scale(1)\"" svg))))

(ert-deftest eas-vega-contour-explicit-color-domains-hold ()
  "Sequential and quantize color scales keep an explicit domain; constant data draws."
  (let ((legend (lambda (scale values)
                  (let* ((spec `(:data (:values ,(vconcat (cl-loop for v in values for i from 0 collect (list :x i :d v))))
                                 :mark "point"
                                 :encoding (:x (:field "x" :type "quantitative")
                                            :color (:field "d" :type "quantitative" :scale ,scale))))
                         (view (aref (plist-get (let ((eas-spec-supported-function nil)) (eas-compile spec)) :views) 0)))
                    (aref (plist-get view :legends) 0)))))
    (should (equal (plist-get (funcall legend '(:domain [0 1]) '(0.5 1)) :domain) [0 1]))
    (should (equal (plist-get (funcall legend '(:domain [0 1]) '(1 1)) :domain) [0 1]))
    (should (plist-get (funcall legend nil '(1 1)) :entries))
    (should (equal (mapcar (lambda (e) (plist-get e :label))
                           (plist-get (funcall legend '(:type "quantize" :domain [0 3000] :scheme (:name "bluepurple" :count 6))
                                               '(500 2500))
                                      :fixed-entries))
                   '("500" "1,000" "1,500" "2,000" "2,500")))))

(ert-deftest eas-vega-contour-horizontal-gradient-title-on-the-left ()
  "A horizontal gradient legend with titleOrient left puts its bar after the title."
  (let* ((spec '(:data (:values [(:x 1 :d 0) (:x 2 :d 10)]) :mark "point"
                 :encoding (:x (:field "x" :type "quantitative")
                            :color (:field "d" :type "quantitative" :title "A long legend title"
                                    :legend (:orient "bottom" :direction "horizontal" :titleOrient "left" :titlePadding 10)))))
         (l (aref (plist-get (aref (plist-get (let ((eas-spec-supported-function nil)) (eas-compile spec)) :views) 0) :legends) 0))
         (bar (plist-get l :bar)) (title (plist-get l :title-mark)))
    (should (> (aref bar 0) (+ (plist-get title :x) 60)))
    ;; Centred on the bar to within the title's baseline rounding.
    (should (<= (abs (- (+ (plist-get title :y) 5) (+ (aref bar 1) (/ (aref bar 3) 2.0)))) 3))))

;; eas-7r1.12a: what density-heatmaps and annual-precipitation needed.

(ert-deftest eas-vega-contour-legend-array-title-is-multi-line ()
  "A Vega-Lite array title draws one line each and pushes the gradient down."
  (let* ((legend (lambda (title target)
                   (let ((spec `(:data (:values [(:x 1 :d 0) (:x 2 :d 10)]) :mark "point"
                                 :encoding (:x (:field "x" :type "quantitative")
                                            :color (:field "d" :type "quantitative" :title ,title)))))
                     (aref (plist-get (aref (plist-get (eas-compile spec :target target) :views) 0) :legends) 0))))
         (one (funcall legend "Local Density" 'svg))
         (two (funcall legend ["Local Density" "(Normalized)"] 'svg)))
    (should (equal (plist-get two :title) "Local Density\n(Normalized)"))
    ;; Vega's line height: font size + 2.
    (should (= (- (aref (plist-get two :bar) 1) (aref (plist-get one :bar) 1)) 13))
    (should (string-match-p "<tspan[^>]*>(Normalized)</tspan>"
                            (eas-svg-render (eas-compile '(:data (:values [(:x 1 :d 0)]) :mark "point"
                                                           :encoding (:x (:field "x" :type "quantitative")
                                                                      :color (:field "d" :type "quantitative"
                                                                              :title ["A" "(Normalized)"])))))))
    ;; One line on the character grid.
    (should (equal (plist-get (funcall legend ["A" "B"] 'text) :title) "A B"))))

(ert-deftest eas-vega-contour-legend-layout-anchor ()
  "config.legend.layout's anchor centres or ends a bottom legend under the view."
  (let* ((x (lambda (layout)
              (let* ((spec `(:width 400 :data (:values [(:x 1 :d 0) (:x 2 :d 10)]) :mark "point"
                             :encoding (:x (:field "x" :type "quantitative")
                                        :color (:field "d" :type "quantitative" :legend (:orient "bottom")))
                             :config (:legend (:layout ,layout))))
                     (view (aref (plist-get (eas-compile spec) :views) 0))
                     (l (aref (plist-get view :legends) 0)))
                (list (- (aref (plist-get l :box) 0) (aref (plist-get view :bounds) 0))
                      (- (aref (plist-get l :box) 2) (aref (plist-get l :box) 0))))))
         (start (funcall x :null)) (w (cadr start)))
    (should (= (car start) 0))
    (should (= (car (funcall x '(:expr "{anchor: 'middle'}"))) (/ (- 400 w) 2.0)))
    (should (= (car (funcall x '(:bottom (:anchor "end")))) (- 400 w)))
    (should (equal (eas-legend-layout-anchor "top" '(:legend (:layout (:bottom (:anchor "end"))))) "start"))))

;;; The templates

(ert-deftest eas-vega-contour-templates-load-with-examples ()
  "Every template loads as vega/NAME with its example and a recorded verdict."
  (eas-vega-contour--with-templates
    (dolist (name eas-vega-contour--names)
      (should (eas-template-p (concat "vega/" name)))
      (should (file-exists-p (eas-template-example-file (eas-template-get (concat "vega/" name)))))
      (should (member (plist-get (eas-vega-contour--meta name) :status) '("pass" "partial" "unsupported"))))
    (should-not eas-template-load-errors)))

(ert-deftest eas-vega-contour-templates-render-natively ()
  "Each example resolves to pure Vega-Lite that draws natively in svg and text."
  (dolist (name eas-vega-contour--names)
    (let* ((spec (eas-vega-contour--resolve name))
           (findings (seq-remove (lambda (f) (plist-get f :property)) (eas-spec-check spec)))
           (scene (eas-compile spec))
           (svg (eas-svg-render scene))
           (text (eas-text-render (eas-compile spec :target 'text))))
      (should-not (string-match-p "x-eas" (eas-json-encode spec)))
      (should (equal (list name findings) (list name nil)))
      (should (string-prefix-p "<svg" svg))
      (should (> (length text) 100)))))

(ert-deftest eas-vega-contour-templates-draw-their-marks ()
  "The examples draw what the gallery shows: counts of items per mark."
  (let ((items (lambda (name)
                 (let ((scene (eas-compile (eas-vega-contour--resolve name))))
                   (cl-loop for view across (plist-get scene :views)
                            append (cl-loop for m across (plist-get view :marks)
                                            collect (cons (plist-get m :mark) (length (plist-get m :items)))))))))
    ;; 21 levels, one path each.
    (should (equal (funcall items "volcano-contours") '(("point" . 21))))
    ;; 4800 wedges.
    (should (equal (funcall items "wind-vectors") '(("point" . 4800))))
    ;; Points, three heatmaps, and contour rings.
    (let ((m (funcall items "contour-plot")))
      (should (equal (mapcar #'car m) '("point" "image" "line")))
      (should (equal (cdr (nth 1 m)) 3))
      (should (>= (cdr (nth 2 m)) 7)))
    ;; Three facet cells, each an extent rect and one image.
    (should (equal (seq-filter (lambda (c) (equal (car c) "image")) (funcall items "density-heatmaps"))
                   '(("image" . 1) ("image" . 1) ("image" . 1))))
    ;; 177 countries under five contour levels.
    (should (equal (funcall items "annual-precipitation") '(("point" . 177) ("point" . 5))))))

(ert-deftest eas-vega-contour-terminal-draws-filled-contours ()
  "On the character grid the volcano's levels fill their cells in their colors."
  (let* ((text (eas-text-render (eas-compile (eas-vega-contour--resolve "volcano-contours") :target 'text)))
         (colors (delete-dups (cl-loop for i below (length text)
                                       for face = (get-text-property i 'face text)
                                       when (and (consp face) (eq (aref text i) ?█)) collect (plist-get face :foreground)))))
    (should (>= (length colors) 15))
    (should (= (cl-count ?█ text) (- (length text) (cl-count ?\n text))))))

(defun eas-vega-contour--rasterize (svg png compare)
  "Rasterize SVG to PNG with rsvg-convert, scaled as COMPARE says."
  (let ((in (make-temp-file "eas-vega-contour" nil ".svg" svg)))
    (unwind-protect
        (should (zerop (apply #'call-process eas-chart-rsvg-program nil nil nil
                              (append (cond ((plist-get compare :width) (list "-w" (number-to-string (plist-get compare :width))))
                                            ((plist-get compare :height) (list "-h" (number-to-string (plist-get compare :height)))))
                                      (list "-o" png in)))))
      (delete-file in))))

(ert-deftest eas-vega-contour-templates-match-the-references ()
  "Native renderings stay within their recorded threshold of the Vega references."
  :tags '(:gallery)
  (unless (and (executable-find eas-chart-rsvg-program) (zlib-available-p))
    (eas-test-skip (format "%s not on PATH; install librsvg's rsvg-convert to compare with test/vega-examples/ref"
                           eas-chart-rsvg-program)))
  (dolist (name eas-vega-contour--names)
    (let* ((meta (eas-vega-contour--meta name))
           (png (make-temp-file "eas-vega-contour" nil ".png")))
      (unwind-protect
          (progn
            (eas-vega-contour--rasterize (eas-svg-render (eas-compile (eas-vega-contour--resolve name))) png
                                         (plist-get meta :compare))
            (let ((r (eas-png-compare (eas-png-read png)
                                      (eas-png-read (eas-test-file "test/vega-examples/ref" (concat name ".png"))))))
              (should (equal (list name (<= (plist-get r :ratio) (plist-get meta :threshold)))
                             (list name t)))))
        (delete-file png)))))

(provide 'eas-vega-contour-test)
;;; eas-vega-contour-test.el ends here

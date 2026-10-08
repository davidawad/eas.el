;;; eas-geoshape.el --- geoshape marks, graticules and projections -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Parts of L2 and L4: Vega-Lite maps.  `eas-geoshape-lower' (an
;; `eas-spec-rewrite-functions' entry) takes over every view whose
;; projection eas-geo.el and eas-projection.el do not already draw: one
;; with a geoshape mark, graticule or sphere data, a projection type
;; beyond theirs, or projection properties (scale, translate, center,
;; rotate, clipAngle, ...).  It
;;
;;   - inlines data {"graticule": ...} (d3's MultiLineString; a
;;     geoshape of it is unfilled, as in Vega-Lite) and {"sphere": true};
;;   - moves the projection onto each unit, under x-eas.geo with the
;;     path of the view that declared it, which is the fit's scope;
;;   - turns longitude/latitude encodings into x and y on fixed linear
;;     scales without axes, filled at compile ("geo-point" transform).
;;
;; At compile, `eas-geoshape-ranges' resolves each projection once per
;; layered view: expressions ({"expr": ...}) read the params; with
;; neither scale nor translate the projection is fitted to every unit's
;; shapes and points (d3's fitSize to width and height), else translate
;; defaults to the view's centre, as Vega-Lite compiles it.  Geoshape
;; items carry their projected path (eas-geo-stream.el) relative to an
;; anchor (the path's centroid), so moving an item moves its shape;
;; points get their pixel position.
;;
;; The "geo-measure" transform adds each feature's projected centroid
;; and area (Vega's geoCentroid and geoArea with a projection), which
;; Vega-Lite cannot compute.

;;; Code:

(require 'cl-lib)
(require 'eas-core)
(require 'eas-expr)
(require 'eas-transform)
(require 'eas-transform-domain)
(require 'eas-nested)
(require 'eas-geo-stream)
(require 'eas-geo-proj)
(require 'eas-topojson)
(require 'eas-geo)
(require 'eas-projection)
(require 'eas-geoshape-render)
(require 'eas-gc)

(defconst eas-geoshape-x "eas_geo_x" "Field holding a projected point's x.")
(defconst eas-geoshape-y "eas_geo_y" "Field holding a projected point's y (up from the bottom).")

(defvar eas-facet-keep)

;;; Data generators

(defun eas-geoshape--data (data)
  "DATA with graticule, sphere and inline topojson made values.
Return (DATA . GRATICULE), GRATICULE non-nil for a graticule source."
  (cond
   ((not (and (eas-object-p data) data)) (cons data nil))
   ((plist-get data :graticule)
    (let ((g (plist-get data :graticule)))
      (cons (list :values (vector (eas-geoshape--graticule (and (eas-object-p g) g)))) t)))
   ((plist-get data :sphere) (cons (list :values (vector (list :type "Sphere"))) nil))
   ((and (equal (plist-get (plist-get data :format) :type) "topojson")
         (eas-object-p (plist-get data :values)) (plist-get data :values))
    (cons (list :values (eas-topojson-rows (plist-get data :values) (plist-get data :format))) nil))
   (t (cons data nil))))

(defvar eas-geoshape--graticules nil
  "Graticules made: alist of (PARAMS . GEOJSON), newest first.
The same object every compile lets its projected lines be retained
\=(`eas-geoshape--projected').")

(defun eas-geoshape--graticule (params)
  "`eas-geo-graticule' of PARAMS, made once per distinct PARAMS."
  (or (cdr (assoc params eas-geoshape--graticules))
      (let ((g (eas-geo-graticule params)))
        (push (cons (copy-tree params t) g) eas-geoshape--graticules)
        (when (> (length eas-geoshape--graticules) 8)
          (setcdr (nthcdr 7 eas-geoshape--graticules) nil))
        g)))

;;; Lowering

(defun eas-geoshape--mark-type (node)
  "The mark type of NODE, or nil."
  (let ((m (plist-get node :mark))) (if (stringp m) m (and (eas-object-p m) (plist-get m :type)))))

(defun eas-geoshape--ours-p (node proj)
  "Non-nil when this file draws NODE's subtree under projection PROJ."
  (let ((type (or (and (eas-object-p proj) (plist-get proj :type)) "equalEarth"))
        (keys (and (eas-object-p proj) (eas-plist-keys proj))))
    (or (cl-labels ((geo (n) (or (equal (eas-geoshape--mark-type n) "geoshape")
                                 (let ((d (plist-get n :data))) (and (eas-object-p d) (or (plist-get d :graticule) (plist-get d :sphere))))
                                 (seq-some #'geo (plist-get n :layer)))))
          (geo node))
        (not (or (and (member type eas-projection-types) (null (remq :type keys)))
                 (and (member type eas-geo-projections) (null (cl-set-difference keys '(:type :rotate :parallels)))))))))

(defun eas-geoshape--unit (node proj key graticule)
  "Unit NODE drawn under PROJ, declared by the view at path KEY.
GRATICULE is non-nil when its data is a graticule."
  (let* ((enc (plist-get node :encoding)) (mark (plist-get node :mark))
         (mark (if (stringp mark) (list :type mark) mark))
         (lon (plist-get enc :longitude)) (lat (plist-get enc :latitude))
         (shape (plist-get enc :shape))
         (geo (list :projection proj :key key))
         (out node))
    (when (equal (plist-get mark :type) "geoshape")
      (when (and graticule (not (plist-member mark :filled)))
        (setq out (eas-plist-put out :mark (eas-plist-put mark :filled :false))))
      (when (and (eas-object-p shape) (plist-get shape :field))
        (setq geo (append geo (list :shape (plist-get shape :field)))
              enc (eas--plist-without enc :shape))))
    (when (and (eas-object-p lon) (stringp (plist-get lon :field)) (eas-object-p lat) (stringp (plist-get lat :field)))
      (let ((pos (lambda (field) (list :field field :type "quantitative" :axis :null
                                       :scale (list :domain [0 1] :nice :false :zero :false)))))
        (setq geo (append geo (list :longitude (plist-get lon :field) :latitude (plist-get lat :field)))
              enc (append (eas--plist-without (eas--plist-without enc :longitude) :latitude)
                          (list :x (funcall pos eas-geoshape-x) :y (funcall pos eas-geoshape-y))))
        (setq out (eas-plist-put out :transform
                                 (vconcat (plist-get out :transform)
                                          (list (list :x-eas:transform "geo-point"
                                                      :longitude (plist-get lon :field)
                                                      :latitude (plist-get lat :field))))))))
    (when enc (setq out (eas-plist-put out :encoding enc)))
    (eas-plist-put out :x-eas (eas-plist-put (plist-get out :x-eas) :geo geo))))

(defun eas-geoshape--walk (node proj key graticule path)
  "NODE (at PATH) lowered under inherited projection PROJ declared at KEY.
GRATICULE is non-nil when NODE inherits graticule data."
  (let* ((d (eas-geoshape--data (plist-get node :data)))
         (node (if (plist-get node :data) (eas-plist-put node :data (car d)) node))
         (graticule (if (plist-get node :data) (cdr d) graticule))
         (own (plist-get node :projection)))
    (when (and own (eas-geoshape--ours-p node own))
      (setq proj own key path node (eas--plist-without node :projection)))
    (cond
     ((plist-get (plist-get node :x-eas) :geo) node) ; lowered already
     ((and own (not (eq proj own))) node)        ; drawn by eas-geo.el or eas-projection.el
     ((plist-get node :mark)
      (cond (proj (eas-geoshape--unit node proj key graticule))
            ;; A geoshape without a projection uses Vega-Lite's default, equalEarth.
            ((equal (eas-geoshape--mark-type node) "geoshape")
             (eas-geoshape--unit node (list :type "equalEarth") path graticule))
            (t node)))
     (t (let ((out node))
          (dolist (k '(:layer :vconcat :hconcat))
            (when (vectorp (plist-get out k))
              (setq out (eas-plist-put
                         out k (vconcat (seq-map-indexed
                                         (lambda (c i)
                                           (eas-geoshape--walk c (and (eq k :layer) proj) (and (eq k :layer) key)
                                                               (and (eq k :layer) graticule)
                                                               (format "%s/%s/%d" path (eas-key-name k) i)))
                                         (plist-get out k)))))))
          out)))))

(defun eas-geoshape-lower (spec)
  "SPEC with its maps lowered for compile (see the commentary).
Resolve (`eas-facet-keep') keeps Vega-Lite's own form for export, and a
template (registered through parse too) keeps it for resolve."
  (if (or (bound-and-true-p eas-facet-keep) (not (eas-object-p spec)) (null spec)
          (plist-get (plist-get spec :x-eas) :template))
      spec
    (eas-geoshape--walk spec nil nil nil "")))

(defun eas-geoshape-features (spec &optional path)
  "Features of the maps lowered in SPEC (at PATH): projection/TYPE."
  (let* ((path (or path ""))
         (geo (plist-get (plist-get spec :x-eas) :geo))
         (type (and geo (let ((t0 (plist-get (plist-get geo :projection) :type))) (if (stringp t0) t0 (and (null t0) "equalEarth")))))
         ;; A type given as {"expr": ...} is checked once evaluated, at compile.
         (out (when type
                (list (list :feature (concat "projection/" type) :path (concat path "/projection")
                            :unknown (not (eas-geo-proj-supported-p type)))))))
    (dolist (key '(:layer :vconcat :hconcat) out)
      (seq-do-indexed (lambda (c i) (setq out (append out (eas-geoshape-features c (format "%s/%s/%d" path (eas-key-name key) i)))))
                      (plist-get spec key)))))

;;; Compile: resolving projections

(defun eas-geoshape--eval (v env)
  "V with every {\"expr\": E} evaluated under ENV, recursively."
  (cond ((and (eas-object-p v) v (plist-get v :expr) (stringp (plist-get v :expr)))
         (eas-expr-evaluate (plist-get v :expr) nil env))
        ((vectorp v) (vconcat (mapcar (lambda (x) (eas-geoshape--eval x env)) v)))
        ((and (eas-object-p v) v)
         (cl-loop for (k x) on v by #'cddr append (list k (eas-geoshape--eval x env))))
        (t v)))

(defun eas-geoshape--geo (unit)
  "UNIT's x-eas.geo, or nil."
  (plist-get (plist-get (plist-get unit :node) :x-eas) :geo))

(defun eas-geoshape--get (row field)
  "ROW's FIELD, reading nested paths (\"geometry.coordinates[0]\")."
  (let ((path (eas-nested-path field)))
    (if path (eas-nested-get row path) (plist-get row (eas-key field)))))

(defun eas-geoshape--shape (geo row)
  "The GeoJSON ROW draws under x-eas.geo GEO."
  (if (plist-get geo :shape) (eas-geoshape--get row (plist-get geo :shape)) row))

(defun eas-geoshape--fit-objects (units)
  "GeoJSON objects the projection of UNITS fits: shapes and points."
  (let (out)
    (dolist (u units)
      (let ((geo (eas-geoshape--geo u)))
        (seq-doseq (row (plist-get u :rows))
          (if (plist-get geo :longitude)
              (let ((a (eas-geoshape--get row (plist-get geo :longitude)))
                    (b (eas-geoshape--get row (plist-get geo :latitude))))
                (when (and (numberp a) (numberp b)) (push (list :type "Point" :coordinates (vector a b)) out)))
            (let ((s (eas-geoshape--shape geo row))) (when (eas-object-p s) (push s out)))))))
    (nreverse out)))

(defun eas-geoshape-resolve (spec units w h &optional sw sh)
  "The projection SPEC (plain values) for UNITS in a W x H view.
Vega-Lite fits it when it has neither scale nor translate; otherwise
translate defaults to the view's centre.  SW SH, the size the spec
declares, differ from W H when the view follows its window (text, a
container): the fixed scale and translate then shrink or grow by
min(W/SW, H/SH), the map centred, so the same map fills the window."
  (if (or (plist-get spec :scale) (plist-get spec :translate))
      (let* ((sw (if (numberp sw) sw w)) (sh (if (numberp sh) sh h))
             (k (min (/ w (float sw)) (/ h (float sh))))
             (tr (eas-geo-proj--vec (plist-get spec :translate) (vector (/ sw 2.0) (/ sh 2.0))))
             (spec (if (and (plist-get spec :scale) (/= k 1))
                       (plist-put (copy-sequence spec) :scale (* k (plist-get spec :scale)))
                     spec)))
        (eas-geo-proj spec (and (/= k 1) (null (plist-get spec :scale))
                                (* k (or (plist-get (eas-geo-raw-type (or (plist-get spec :type) "equalEarth")) :scale) 1070)))
                      (list (+ (/ (- w (* k sw)) 2) (* k (nth 0 tr))) (+ (/ (- h (* k sh)) 2) (* k (nth 1 tr))))))
    (let ((fit (eas-geo-proj-fit spec (eas-geoshape--fit-objects units) (vector 0 0 w h))))
      (eas-geo-proj spec (car fit) (cdr fit)))))

(declare-function eas-compile-set-range "eas-compile-scales")

(defun eas-geoshape-group-p (group)
  "Non-nil when GROUP draws a map of this file's."
  (seq-some #'eas-geoshape--geo (plist-get group :units)))

(defun eas-geoshape-ranges (group)
  "Resolve GROUP's projections and place its projected points."
  (let ((w (float (plist-get group :w))) (h (float (plist-get group :h))) (done nil))
    (dolist (u (plist-get group :units))
      (when-let* ((geo (eas-geoshape--geo u)))
        (let* ((key (plist-get geo :key))
               (proj (or (cdr (assoc key done))
                         (let* ((spec (eas-geoshape--eval (plist-get geo :projection) (plist-get u :env)))
                                (p (eas-geoshape-resolve
                                    spec (seq-filter (lambda (v) (equal (plist-get (eas-geoshape--geo v) :key) key))
                                                     (plist-get group :units))
                                    w h (plist-get group :spec-w) (plist-get group :spec-h))))
                           (push (cons key p) done)
                           p))))
          (plist-put u :geo-proj proj)
          (when (plist-get geo :longitude)
            (let ((rows (copy-sequence (plist-get u :rows))) (kx (eas-key eas-geoshape-x)) (ky (eas-key eas-geoshape-y)))
              (dotimes (i (length rows))
                (let* ((row (aref rows i))
                       (a (eas-geoshape--get row (plist-get geo :longitude)))
                       (b (eas-geoshape--get row (plist-get geo :latitude)))
                       (p (and (numberp a) (numberp b) (funcall (plist-get proj :point) a b))))
                  (aset rows i (eas-plist-put (eas-plist-put row kx (if p (aref p 0) 0)) ky (- h (if p (aref p 1) 0))))))
              (plist-put u :rows rows))
            (let ((scales (plist-get group :scales)))
              (dolist (c (list (list :x w (vector (plist-get group :x0) (+ (plist-get group :x0) w)))
                               (list :y h (vector (+ (plist-get group :y0) h) (plist-get group :y0)))))
                (when-let* ((s (plist-get scales (car c))))
                  (setq scales (plist-put scales (car c)
                                          (eas-compile-set-range (plist-put (copy-sequence s) :domain (vector 0 (nth 1 c)))
                                                                 (nth 2 c))))))
              (plist-put group :scales scales))))))))

;;; Compile: items

(declare-function eas-marks--style "eas-marks")
(declare-function eas-marks--extras "eas-marks")

(defun eas-geoshape--relative (path ax ay)
  "PATH's coordinates relative to anchor AX AY, with its bounding box.
Return (PATHS CIRCLES BOX): PATHS a vector of [CLOSED XY...] vectors."
  (let ((x0 1.0e+INF) (y0 1.0e+INF) (x1 -1.0e+INF) (y1 -1.0e+INF))
    (cl-flet ((see (x y) (setq x0 (min x0 x) y0 (min y0 y) x1 (max x1 x) y1 (max y1 y))))
      (list
       (vconcat (mapcar (lambda (p)
                          (let* ((flat (cdr p)) (n (length flat)) (rel (make-vector n 0.0)))
                            (cl-loop for i from 0 below n by 2
                                     do (see (aref flat i) (aref flat (1+ i)))
                                     (aset rel i (- (aref flat i) ax)) (aset rel (1+ i) (- (aref flat (1+ i)) ay)))
                            (vector (if (car p) t :false) rel)))
                        (plist-get path :paths)))
       (vconcat (mapcar (lambda (c)
                          (let ((r (aref c 2)))
                            (see (- (aref c 0) r) (- (aref c 1) r)) (see (+ (aref c 0) r) (+ (aref c 1) r))
                            (vector (- (aref c 0) ax) (- (aref c 1) ay) r)))
                        (plist-get path :circles)))
       (vector (- x0 ax) (- y0 ay) (- x1 ax) (- y1 ay))))))

(defun eas-geoshape--anchor (path)
  "Where an item of PATH is anchored: its largest ring's centroid.
A shape cut in two (Fiji across the antimeridian) is anchored on a part
of it, not between the parts; a path without rings uses its centroid."
  (let ((best nil) (best-a 0))
    (dolist (p (plist-get path :paths))
      (when (car p)
        (let ((a (abs (eas-geo-ring-area (cdr p)))))
          (when (> a best-a) (setq best p best-a a)))))
    (eas-geo-path-centroid (if best (list :paths (list best)) path))))

;;; Compile: projected shapes, retained

(defvar eas-geoshape-tolerance 0.06
  "Pixels within which a geoshape item's path drops points, or nil.
A path point closer than this to the segment around it is not drawn
\=(`eas-geo-path-sink').  Rasterized at 1x and 2x, the projections
grid then differs from its full paths in under 0.1% of its pixels
\=(`eas-vega-geo-gallery-simplified-paths-rasterize-as-the-full-paths')
and its SVG is a third smaller.  The geo-measure transform measures
every point.")

(defvar eas-geoshape--projected nil
  "Projected shapes per projection: alist of (SPEC . TABLE), newest first.
SPEC is a projection's plain values (its :spec); TABLE maps a geometry
\=(`eas-geoshape--identity'), by identity and weakly, to (TYPE . VALUE),
VALUE as `eas-geoshape--project' returns it.")

(defun eas-geoshape-forget ()
  "Forget every projected shape and recorded spherical stream.
The next compile of a map projects it as a fresh Emacs would."
  (setq eas-geoshape--projected nil)
  (clrhash eas-geo-proj--recorded))

(defvar eas-geoshape-projected-max 32
  "Projections whose shapes `eas-geoshape--projected' keeps.
A grid of maps (the projections template draws 24) needs one each.")

(defun eas-geoshape--projected-table (proj)
  "The table of shapes projected under PROJ, made if new."
  (let* ((spec (plist-get proj :spec))
         (hit (assoc spec eas-geoshape--projected)))
    (if hit
        (progn (unless (eq hit (car eas-geoshape--projected))
                 (setq eas-geoshape--projected (cons hit (delq hit eas-geoshape--projected))))
               (cdr hit))
      (let ((table (make-hash-table :test 'eq :weakness 'key)))
        (push (cons spec table) eas-geoshape--projected)
        (when (> (length eas-geoshape--projected) eas-geoshape-projected-max)
          (setcdr (nthcdr (1- eas-geoshape-projected-max) eas-geoshape--projected) nil))
        table))))

(defun eas-geoshape--identity (shape)
  "(KEY . TYPE): what SHAPE's projected path depends on, by identity.
A row copied by a transform keeps its geometry and coordinates objects,
so a feature or geometry is known again by them in a later compile.
TYPE tells geometries sharing coordinates apart."
  (let ((type (plist-get shape :type)))
    (cond ((equal type "Feature")
           (let ((g (plist-get shape :geometry)))
             (if (eas-object-p g) (eas-geoshape--identity g) (cons shape type))))
          ((equal type "Sphere") (cons 'sphere type))
          ((vectorp (plist-get shape :coordinates)) (cons (plist-get shape :coordinates) type))
          ((vectorp (plist-get shape :geometries)) (cons (plist-get shape :geometries) type))
          (t (cons shape type)))))

(defun eas-geoshape--project (proj shape &optional table)
  "SHAPE projected under PROJ as (ANCHOR . (PATHS CIRCLES BOX)), or nil.
ANCHOR is [X Y] in plot pixels, the rest relative to it
\=(`eas-geoshape--relative').  With TABLE (`eas-geoshape--projected-table')
the value is retained per geometry (`eas-geoshape--identity'): a frame
that only restyles a map (a hover) re-projects nothing, nor does a
compile of the same data."
  (let* ((id (and table (eas-geoshape--identity shape)))
         (hit (and id (gethash (car id) table)))
         (hit (and hit (equal (car hit) (cdr id)) (cdr hit))))
    (if hit (and (consp hit) hit)
      (let* ((path (eas-geo-proj-path proj shape nil eas-geoshape-tolerance))
             (value (if (and path (or (plist-get path :paths) (plist-get path :circles)))
                        (let ((c (or (eas-geoshape--anchor path) [0 0])))
                          (cons c (eas-geoshape--relative path (aref c 0) (aref c 1))))
                      'none)))
        (when id (puthash (car id) (cons (cdr id) value) table))
        (and (consp value) value)))))

(defun eas-geoshape-row-fn (unit scales bounds)
  "Return the function (ROW I) -> item or nil for geoshape UNIT.
It draws with SCALES in plot BOUNDS [X Y W H]; a hover patches one
item through it (`eas-patch--items').  Call it inside
`eas-marks-with-cache'."
  (let* ((geo (eas-geoshape--geo unit))
         (proj (or (plist-get unit :geo-proj)
                   (eas-geoshape-resolve (eas-geoshape--eval (plist-get geo :projection) (plist-get unit :env))
                                         (list unit) (aref bounds 2) (aref bounds 3))))
         (mark (plist-get unit :mark))
         (stroked (eq (plist-get mark :filled) :false))
         (sw (plist-get mark :strokeWidth))
         (table (and (plist-get proj :spec) (eas-geoshape--projected-table proj)))
         (ox (aref bounds 0)) (oy (aref bounds 1)))
    (lambda (row i)
      (let* ((shape (eas-geoshape--shape geo row))
             (projected (and (eas-object-p shape) shape (eas-geoshape--project proj shape table))))
        (when projected
          (let* ((c (car projected))
                 (rel (cdr projected))
                 (style (eas-marks--style unit scales row)))
            (append (list :datum i :x (+ ox (aref c 0)) :y (+ oy (aref c 1))
                          :paths (nth 0 rel) :circles (nth 1 rel) :box (nth 2 rel))
                    style
                    (unless (plist-get style :strokeWidth)
                      (cond ((numberp sw) (list :strokeWidth sw)) (stroked (list :strokeWidth 1))))
                    (cl-loop for k in '(:strokeDash :strokeCap :strokeJoin)
                             for v = (plist-get mark k)
                             when (and v (not (plist-get style k))) append (list k v))
                    (eas-marks--extras unit row))))))))

(defun eas-geoshape-items (unit scales bounds _metrics)
  "Scene items of geoshape UNIT with SCALES in plot BOUNDS [X Y W H].
A map's first compile conses hundreds of megabytes of floats (the
projections grid about 550 MB), so collection waits until Emacs is
idle (`eas-gc-defer'), as during an interaction."
  (eas-gc-defer)
  (let ((row-fn (eas-geoshape-row-fn unit scales bounds)) out)
    (seq-do-indexed (lambda (row i) (let ((item (funcall row-fn row i))) (when item (push item out))))
                    (plist-get unit :rows))
    (vconcat (nreverse out))))

(defun eas-geoshape-item-box (item)
  "ITEM's bounding box [X0 Y0 X1 Y1] in pixels, a painted stroke included."
  (let ((b (plist-get item :box)) (x (plist-get item :x)) (y (plist-get item :y))
        (r (if (eas-geoshape--visible-stroke-p item) (/ (or (plist-get item :strokeWidth) 1) 2.0) 0)))
    ;; Rounded to a micropixel: a fitted map's -1e-14 overhang grows no canvas.
    (and b (vconcat (mapcar (lambda (v) (/ (fround (* v 1e6)) 1e6))
                            (list (- (+ x (aref b 0)) r) (- (+ y (aref b 1)) r) (+ x (aref b 2) r) (+ y (aref b 3) r)))))))

;;; Transforms

(defun eas-geoshape--point (rows params)
  "ROWS with placeholder projected x and y where PARAMS's lon/lat are numbers.
`eas-geoshape-ranges' fills them once the projection is known; rows
without a position keep none and drop out, as Vega-Lite's do."
  (let ((lon (plist-get params :longitude)) (lat (plist-get params :latitude))
        (kx (eas-key eas-geoshape-x)) (ky (eas-key eas-geoshape-y)))
    (seq-map (lambda (row)
               (if (and (numberp (eas-geoshape--get row lon)) (numberp (eas-geoshape--get row lat)))
                   (eas-plist-put (eas-plist-put row kx 0) ky 0)
                 row))
             rows)))

(eas-register-transform
 "geo-point"
 :doc "Projected x and y of longitude/latitude rows (Vega-Lite projection, lowered; placed at compile)."
 :schema '(:longitude (:type "string" :required t) :latitude (:type "string" :required t))
 :fn #'eas-geoshape--point)

(defun eas-geoshape--measure (rows params)
  "ROWS with PARAMS's :as fields: projected centroid x, y and area.
PARAMS: :projection (Vega-Lite), :size [W H] for its fit or centre,
:field (the GeoJSON field; the row itself when absent)."
  (let* ((size (or (plist-get params :size) [960 500]))
         (field (plist-get params :field))
         (shape (lambda (row) (if field (eas-geoshape--get row field) row)))
         (spec (plist-get params :projection))
         (proj (eas-geoshape-resolve
                spec (list (list :rows rows :node (list :x-eas (list :geo (list :shape field)))))
                (aref size 0) (aref size 1)))
         (as (or (plist-get params :as) ["centroid_x" "centroid_y" "area"])))
    (seq-map (lambda (row)
               (let* ((path (eas-geo-proj-path proj (funcall shape row)))
                      (c (eas-geo-path-centroid path)))
                 (append row (list (eas-key (aref as 0)) (if c (aref c 0) :null)
                                   (eas-key (aref as 1)) (if c (aref c 1) :null)
                                   (eas-key (aref as 2)) (eas-geo-path-area path)))))
             rows)))

(eas-register-transform
 "geo-measure"
 :doc "Projected centroid and area of each feature (Vega's geoCentroid and geoArea with a projection)."
 :schema '(:projection (:type "object" :required t :doc "the Vega-Lite projection (plain values)")
           :size (:type "array" :default [960 500] :doc "[width height]: the fit, or the default translate's centre")
           :field (:type "string" :doc "the GeoJSON field; the row itself when absent")
           :as (:type "array" :default ["centroid_x" "centroid_y" "area"] :doc "output fields"))
 :fn #'eas-geoshape--measure)

(defvar eas-spec-rewrite-functions)
;; Before eas-geo.el's lowering (depth 90), after eas-vl-lower's data inlining.
(add-hook 'eas-spec-rewrite-functions #'eas-geoshape-lower 80)
(defvar eas-spec-feature-functions)
(add-hook 'eas-spec-feature-functions #'eas-geoshape-features)

(provide 'eas-geoshape)
;;; eas-geoshape.el ends here

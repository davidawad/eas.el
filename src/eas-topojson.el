;;; eas-topojson.el --- TopoJSON and GeoJSON data for geoshape -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L0.  Maps read GeoJSON features, one row each, as Vega-Lite
;; reads them:
;;
;;   format {"type": "topojson", "feature": NAME}   the object's features
;;   format {"type": "topojson", "mesh": NAME}      one MultiLineString row
;;
;; `eas-topojson-features' is topojson-client's feature(): quantized,
;; delta-encoded arcs decoded, rings stitched from arcs (reversed ones
;; for negative indices) and closed.  A mesh keeps every arc once.
;;
;; The "geojson" adapter binds map data to a template slot: an array
;; of features or rows, a FeatureCollection, a topology with "feature"
;; (or "mesh"), or {"url": FILE, "feature": NAME} naming a TopoJSON or
;; GeoJSON file (relative to `default-directory', else the repository).
;; Rows keep their nested geometry; a feature's properties stay under
;; "properties", as Vega-Lite reads them ("properties.name").

;;; Code:

(require 'eas-core)
(require 'eas-data)

(defun eas-topojson--arcs (topology)
  "The decoded arcs of TOPOLOGY, a vector of point vectors."
  (let* ((tr (plist-get topology :transform))
         (sx (and tr (aref (plist-get tr :scale) 0))) (sy (and tr (aref (plist-get tr :scale) 1)))
         (tx (and tr (aref (plist-get tr :translate) 0))) (ty (and tr (aref (plist-get tr :translate) 1)))
         (in (let ((a (plist-get topology :arcs))) (if (vectorp a) a (vconcat a))))
         (out (make-vector (length in) nil)))
    (dotimes (k (length in))
      (let* ((arc (let ((a (aref in k))) (if (vectorp a) a (vconcat a)))) (n (length arc)) (pts (make-vector n nil)) (x 0) (y 0))
        (dotimes (i n)
          (let ((p (aref arc i)))
            (aset pts i (if tr
                            (progn (setq x (+ x (aref p 0)) y (+ y (aref p 1)))
                                   (vector (+ (* x sx) tx) (+ (* y sy) ty)))
                          (vector (aref p 0) (aref p 1))))))
        (aset out k pts)))
    out))

(defun eas-topojson--position (topology p)
  "Position P of a Point geometry in TOPOLOGY, untransformed."
  (let ((tr (plist-get topology :transform)))
    (if tr (vector (+ (* (aref p 0) (aref (plist-get tr :scale) 0)) (aref (plist-get tr :translate) 0))
                   (+ (* (aref p 1) (aref (plist-get tr :scale) 1)) (aref (plist-get tr :translate) 1)))
      p)))

(defun eas-topojson--line (arcs indices)
  "The line stitched from ARCS at INDICES (negative: reversed ~i).
Each arc after the first replaces the last point so far with its own
first.  A line of one point repeats it; of none, it is [nil]."
  (let ((size 0))
    (seq-doseq (i indices) (setq size (+ size (length (aref arcs (if (< i 0) (lognot i) i))))))
    (let ((line (make-vector size nil)) (k 0))
      (seq-doseq (i indices)
        (let* ((arc (aref arcs (if (< i 0) (lognot i) i))) (m (length arc)))
          (when (> k 0) (setq k (1- k)))
          (if (< i 0)
              (dotimes (j m) (aset line (+ k j) (aref arc (- m j 1))))
            (dotimes (j m) (aset line (+ k j) (aref arc j))))
          (setq k (+ k m))))
      (cond ((= k 0) (vector nil))
            ((= k 1) (vector (aref line 0) (aref line 0)))
            ((= k size) line)
            (t (substring line 0 k))))))

(defun eas-topojson--ring (arcs indices)
  "The closed ring stitched from ARCS at INDICES."
  (let ((line (eas-topojson--line arcs indices)))
    (if (>= (length line) 4) line
      (vconcat line (make-vector (- 4 (length line)) (aref line 0))))))

(defun eas-topojson--geometry (topology arcs o)
  "GeoJSON geometry of TopoJSON object O in TOPOLOGY (decoded ARCS)."
  (let ((a (plist-get o :arcs)) (c (plist-get o :coordinates)))
    (pcase (plist-get o :type)
      ("GeometryCollection"
       (list :type "GeometryCollection"
             :geometries (vconcat (seq-map (lambda (g) (eas-topojson--geometry topology arcs g))
                                           (plist-get o :geometries)))))
      ("Point" (list :type "Point" :coordinates (eas-topojson--position topology c)))
      ("MultiPoint" (list :type "MultiPoint" :coordinates (vconcat (seq-map (lambda (p) (eas-topojson--position topology p)) c))))
      ("LineString" (list :type "LineString" :coordinates (eas-topojson--line arcs a)))
      ("MultiLineString" (list :type "MultiLineString" :coordinates (vconcat (seq-map (lambda (l) (eas-topojson--line arcs l)) a))))
      ("Polygon" (list :type "Polygon" :coordinates (vconcat (seq-map (lambda (r) (eas-topojson--ring arcs r)) a))))
      ("MultiPolygon" (list :type "MultiPolygon"
                            :coordinates (vconcat (seq-map (lambda (p) (vconcat (seq-map (lambda (r) (eas-topojson--ring arcs r)) p))) a))))
      (_ :null))))

(defun eas-topojson--feature (topology arcs o)
  "The GeoJSON Feature of TopoJSON object O in TOPOLOGY (decoded ARCS)."
  (append (list :type "Feature")
          (when (plist-member o :id) (list :id (plist-get o :id)))
          (list :properties (plist-get o :properties)
                :geometry (eas-topojson--geometry topology arcs o))))

(defun eas-topojson--object (topology name)
  "The object NAME of TOPOLOGY, or signal NOT_FOUND."
  (or (plist-get (plist-get topology :objects) (eas-key name))
      (eas-signal "NOT_FOUND" (format "TopoJSON has no object %s; it has %s" name
                                      (mapconcat #'eas-key-name (eas-plist-keys (plist-get topology :objects)) ", "))
                  :path "/data/format/feature")))

(defun eas-topojson-features (topology name)
  "The rows of TopoJSON TOPOLOGY's object NAME: one GeoJSON Feature each."
  (let* ((arcs (eas-topojson--arcs topology)) (o (eas-topojson--object topology name)))
    (if (equal (plist-get o :type) "GeometryCollection")
        (vconcat (seq-map (lambda (g) (eas-topojson--feature topology arcs g)) (plist-get o :geometries)))
      (vector (eas-topojson--feature topology arcs o)))))

(defun eas-topojson-mesh (topology name)
  "TOPOLOGY's object NAME as one MultiLineString row, each arc once."
  (let* ((arcs (eas-topojson--arcs topology)) (seen (make-hash-table)) lines)
    (cl-labels ((walk (o)
                  (let ((a (plist-get o :arcs)))
                    (pcase (plist-get o :type)
                      ("GeometryCollection" (seq-do #'walk (plist-get o :geometries)))
                      ((or "LineString" "Polygon" "MultiLineString" "MultiPolygon") (collect a)))))
                (collect (a)
                  (if (numberp a)
                      (let ((i (if (< a 0) (lognot a) a)))
                        (unless (gethash i seen) (puthash i t seen) (push (aref arcs i) lines)))
                    (seq-do #'collect a))))
      (walk (eas-topojson--object topology name)))
    (vector (list :type "MultiLineString" :coordinates (vconcat (nreverse lines))))))

(defun eas-topojson-rows (value format)
  "Rows of parsed TopoJSON VALUE under Vega-Lite FORMAT (feature or mesh)."
  (cond ((plist-get format :feature) (eas-topojson-features value (plist-get format :feature)))
        ((plist-get format :mesh) (eas-topojson-mesh value (plist-get format :mesh)))
        (t (eas-signal "INVALID_INPUT" "A topojson format needs a feature or a mesh object name"
                       :path "/data/format"))))

;;; The geojson adapter

(defvar eas-template--root)

(defun eas-topojson--file (file)
  "FILE, relative to `default-directory' or else the repository root."
  (let ((here (expand-file-name file)))
    (if (or (file-readable-p here) (not (boundp 'eas-template--root))) here
      (expand-file-name file eas-template--root))))

(defvar eas-topojson--files (make-hash-table :test 'equal)
  "(FILE MTIME REST) -> features read from a map file by the adapter.
As `eas-data-url--cache': every resolve of a template shares the rows,
so their shapes are projected once (`eas-geoshape--projected').")

(defun eas-topojson--geojson-rows (value)
  "Rows (features) of geojson adapter VALUE; see the commentary."
  (cond
   ((vectorp value) value)
   ((and (listp value) (not (eas-object-p value))) (vconcat value))
   ((plist-get value :url)
    (let ((file (eas-topojson--file (plist-get value :url))))
      (unless (file-readable-p file)
        (eas-shape-invalid (format "No readable map file %s" file) nil "url"))
      (let* ((rest (eas--plist-without value :url))
             (key (list file (file-attribute-modification-time (file-attributes file)) rest)))
        (or (gethash key eas-topojson--files)
            (puthash key (eas-topojson--geojson-rows (append (list :topology (eas-json-read-file file)) rest))
                     eas-topojson--files)))))
   ((plist-get value :topology)
    (let ((top (plist-get value :topology)))
      (if (equal (plist-get top :type) "Topology")
          (eas-topojson-rows top value)
        (eas-topojson--geojson-rows top))))
   ((equal (plist-get value :type) "Topology") (eas-topojson-rows value value))
   ((equal (plist-get value :type) "FeatureCollection") (plist-get value :features))
   ((plist-get value :type) (vector value))
   (t (eas-shape-invalid "Give map data as features, a FeatureCollection, a topology with a feature, or {url, feature}" nil))))

(defun eas-topojson--adapter (value)
  "The geojson adapter: VALUE to data/v1 rows of features."
  (let ((rows (eas-topojson--geojson-rows value)))
    (seq-do-indexed (lambda (row i) (unless (eas-object-p row)
                                      (eas-shape-invalid (format "Map row %d is not an object" i) i)))
                    rows)
    (list :schema (vector (list :name "geometry" :type "geojson")) :rows rows)))

(eas-register-adapter
 "geojson" :doc "Map rows: GeoJSON features, a FeatureCollection, or TopoJSON {topology|url, feature|mesh}."
 :convert #'eas-topojson--adapter
 :example '(:type "FeatureCollection"
            :features [(:type "Feature" :id 1 :properties (:name "a")
                        :geometry (:type "Polygon" :coordinates [[[0 0] [10 0] [10 10] [0 10] [0 0]]]))]))

(provide 'eas-topojson)
;;; eas-topojson.el ends here

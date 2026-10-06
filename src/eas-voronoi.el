;;; eas-voronoi.el --- the x-eas voronoi transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  {"x-eas:transform": "voronoi", "x": F, "y": G} gives
;; every row the Voronoi cell of its point within "extent", as Vega's
;; voronoi transform does with d3-delaunay: the region of the plane
;; nearer that point than any other.  "as" (default "path") receives
;; the cell as SVG path data, "M x,y L x,y ... Z", the field Vega's
;; path marks read; "polygon", when named, receives the same cell as
;; an array of [x, y] vertices, which a line mark draws after a
;; flatten (one vertex per row, detail by the row).
;;
;; A cell is the extent clipped by the half-plane of each other point
;; (Sutherland-Hodgman), nearest points first; a point farther than
;; twice the cell's reach cannot clip it, which ends the search.  As
;; in d3-delaunay, a row without a numeric point, and every repeat of
;; a point already seen, gets a null cell.  "key" makes rows sharing
;; that field's value one site (its first row's point), so a table of
;; routes can carry the cell of its origin.  "transform" holds
;; Vega-Lite transforms applied to the rows first (a template's domain
;; transforms precede its native ones).

;;; Code:

(require 'eas-core)
(require 'eas-transform)
(require 'eas-transform-domain)

(defun eas-voronoi--clip (poly ax ay c)
  "POLY (a list of (X . Y)) clipped to the half-plane AX*x + AY*y <= C."
  (let ((out nil) (n (length poly)))
    (when (> n 0)
      (let ((prev (car (last poly))))
        (dolist (cur poly)
          (let ((dp (- (+ (* ax (car prev)) (* ay (cdr prev))) c))
                (dc (- (+ (* ax (car cur)) (* ay (cdr cur))) c)))
            (when (or (and (<= dp 0) (> dc 0)) (and (> dp 0) (<= dc 0)))
              (let ((u (/ dp (- dp dc))))
                (push (cons (+ (car prev) (* u (- (car cur) (car prev))))
                            (+ (cdr prev) (* u (- (cdr cur) (cdr prev)))))
                      out)))
            (when (<= dc 0) (push cur out))
            (setq prev cur)))))
    (nreverse out)))

(defun eas-voronoi-cells (xs ys extent)
  "Voronoi cells of the points XS YS (vectors of floats or nil) in EXTENT.
EXTENT is [[X0 Y0] [X1 Y1]].  Return a vector holding, per point, its
cell as a list of (X . Y) vertices in order, or nil (no point, or a
repeat of an earlier one)."
  (let* ((n (length xs))
         (x0 (float (aref (aref extent 0) 0))) (y0 (float (aref (aref extent 0) 1)))
         (x1 (float (aref (aref extent 1) 0))) (y1 (float (aref (aref extent 1) 1)))
         (seen (make-hash-table :test 'equal))
         (sites nil)
         (cells (make-vector n nil)))
    (dotimes (i n)
      (let ((x (aref xs i)) (y (aref ys i)))
        (when (and x y (not (gethash (cons x y) seen)))
          (puthash (cons x y) i seen)
          (push i sites))))
    (setq sites (vconcat (nreverse sites)))
    (seq-doseq (i sites)
      (let* ((xi (aref xs i)) (yi (aref ys i))
             (others (sort (seq-filter (lambda (j) (/= j i)) (append sites nil))
                           (lambda (a b)
                             (< (+ (expt (- (aref xs a) xi) 2) (expt (- (aref ys a) yi) 2))
                                (+ (expt (- (aref xs b) xi) 2) (expt (- (aref ys b) yi) 2))))))
             (poly (list (cons x0 y0) (cons x1 y0) (cons x1 y1) (cons x0 y1)))
             (reach2 (lambda ()
                       (let ((r 0.0))
                         (dolist (p poly r)
                           (setq r (max r (+ (expt (- (car p) xi) 2) (expt (- (cdr p) yi) 2)))))))))
        (catch 'done
          (let ((limit (* 4 (funcall reach2))))
            (dolist (j others)
              (let* ((xj (aref xs j)) (yj (aref ys j))
                     (d2 (+ (expt (- xj xi) 2) (expt (- yj yi) 2))))
                (when (> d2 limit) (throw 'done nil))
                (setq poly (eas-voronoi--clip poly (- xj xi) (- yj yi)
                                              (/ (- (+ (* xj xj) (* yj yj)) (+ (* xi xi) (* yi yi))) 2)))
                (setq limit (* 4 (funcall reach2)))))))
        (aset cells i poly)))
    cells))

(defun eas-voronoi--number (v)
  "V as a float, or nil when it is not a number."
  (and (numberp v) (float v)))

(defun eas-voronoi--fmt (v)
  "V for SVG path data: at most 3 decimals."
  (let ((s (format "%.3f" v)))
    (string-remove-suffix "." (replace-regexp-in-string "\\.?0+\\'" "" s))))

(defun eas-voronoi-path (cell)
  "SVG path data of CELL (a list of (X . Y)), as d3-delaunay renders it."
  (and cell
       (concat "M" (mapconcat (lambda (p) (concat (eas-voronoi--fmt (car p)) "," (eas-voronoi--fmt (cdr p))))
                              cell "L")
               "Z")))

(defun eas-voronoi-transform (rows params)
  "The voronoi transform: ROWS with the cells PARAMS describe."
  (let* ((rows (vconcat (if-let* ((pre (plist-get params :transform)))
                            (eas-transform-run pre rows nil "/transform")
                          rows)))
         (fx (eas-key (plist-get params :x))) (fy (eas-key (plist-get params :y)))
         (key (and (stringp (plist-get params :key)) (eas-key (plist-get params :key))))
         (n (length rows))
         (site-of (make-vector n nil))
         (firsts (make-hash-table :test 'equal))
         (xs (make-vector n nil)) (ys (make-vector n nil)))
    (dotimes (i n)
      (let* ((row (aref rows i))
             (k (if key (plist-get row key) i))
             (first (or (gethash k firsts) (puthash k i firsts))))
        (aset site-of i first)
        (when (= first i)
          (aset xs i (eas-voronoi--number (plist-get row fx)))
          (aset ys i (eas-voronoi--number (plist-get row fy))))))
    (let* ((cells (eas-voronoi-cells xs ys (or (plist-get params :extent) [[-100000 -100000] [100000 100000]])))
           (as (eas-key (or (plist-get params :as) "path")))
           (polygon (and (stringp (plist-get params :polygon)) (eas-key (plist-get params :polygon)))))
      (vconcat
       (seq-map-indexed
        (lambda (row i)
          (let* ((cell (aref cells (aref site-of i)))
                 (out (eas-plist-put row as (or (eas-voronoi-path cell) :null))))
            (if polygon
                (eas-plist-put out polygon
                               (if cell (vconcat (mapcar (lambda (p) (vector (car p) (cdr p)))
                                                         (append cell (list (car cell)))))
                                 :null))
              out)))
        rows)))))

(eas-register-transform
 "voronoi"
 :doc "Voronoi cells of x/y points (Vega's voronoi transform): SVG path data, optionally vertices."
 :schema '(:x (:type "string" :required t :doc "field of a point's x")
           :y (:type "string" :required t :doc "field of a point's y")
           :extent (:type "array" :default [[-100000 -100000] [100000 100000]]
                    :doc "[[x0, y0], [x1, y1]] the cells are clipped to")
           :as (:type "string" :default "path" :doc "field receiving the cell's SVG path data")
           :polygon (:type "string" :doc "field receiving the cell's closed [x, y] vertex array")
           :key (:type "string" :doc "rows sharing this field's value are one site")
           :transform (:type "array" :doc "Vega-Lite transforms applied to the rows first"))
 :fn #'eas-voronoi-transform)

(dolist (f '(eas-voronoi--clip eas-voronoi-cells))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(provide 'eas-voronoi)
;;; eas-voronoi.el ends here

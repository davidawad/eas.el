;;; eas-kde2d.el --- two-dimensional kernel density estimates on a grid -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  The "kde2d" domain transform is Vega's kde2d (vega-geo's
;; density2D, after d3-contour's contourDensity): each point of a
;; group lands in a cell of a WIDTH x HEIGHT pixel raster binned by
;; cellSize, and three box blurs per axis, of a radius from the
;; bandwidth (Scott's rule per axis when -1), approximate a Gaussian
;; kernel.  Values are probability densities summing to 1, or with
;; counts, points per square pixel.
;;
;; Vega places the points with the chart's own scales.  At resolve
;; there are no scales yet, so the transform takes the scales' domains
;; itself: the data's extent with zero, made nice as Vega-Lite's
;; default linear scale makes it (`eas-scale-continuous'), unless
;; xExtent / yExtent fix them.  The grid it writes maps its cells back
;; into those data units (scale and translate), so isocontour's rings
;; and heatmap's image fall on the chart's x and y scales, and it
;; writes the extent (AS_x0 AS_x1 AS_y0 AS_y1) so a layer can span the
;; grid exactly.  Pixels round as Vega's round: true scales do.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-transform-domain)

(defun eas-kde2d--quantile (sorted p)
  "The P quantile of SORTED numbers (a vector), d3.quantile's R-7."
  (let* ((n (length sorted)) (h (* (1- n) p)) (i (floor h)))
    (if (>= (1+ i) n) (aref sorted (1- n))
      (+ (aref sorted i) (* (- h i) (- (aref sorted (1+ i)) (aref sorted i)))))))

(defun eas-kde2d-bandwidth (values)
  "Scott's-rule bandwidth of VALUES, as vega-statistics estimateBandwidth."
  (let* ((n (length values))
         (mean (/ (apply #'+ values) (float n)))
         (d (if (> n 1) (sqrt (/ (apply #'+ (mapcar (lambda (v) (expt (- v mean) 2)) values)) (1- n))) 0.0))
         (sorted (vconcat (sort (copy-sequence values) #'<)))
         (q0 (eas-kde2d--quantile sorted 0.25)) (q2 (eas-kde2d--quantile sorted 0.75))
         (h (/ (- q2 q0) 1.34))
         (v (let ((m (min d h))) (cond ((/= m 0) m) ((/= d 0) d) ((/= q0 0) (abs q0)) (t 1.0)))))
    (* 1.06 v (expt n -0.2))))

(defun eas-kde2d--radius (bw values)
  "Blur radius in pixels for bandwidth BW (estimated from VALUES when negative)."
  (let ((v (if (>= bw 0) bw (eas-kde2d-bandwidth values))))
    (eas-kde2d--round (/ (- (sqrt (+ (* 4 v v) 1)) 1) 2))))

(defun eas-kde2d--round (v)
  "V rounded half up, as JavaScript's Math.round."
  (floor (+ v 0.5)))

(defun eas-kde2d--blur-x (n m source target r)
  "Box-blur the N x M SOURCE into TARGET along x with radius R."
  (let ((w (1+ (* 2 r))))
    (dotimes (j m)
      (let ((sr 0.0) (row (* j n)))
        (dotimes (i (+ n r))
          (when (< i n) (setq sr (+ sr (aref source (+ i row)))))
          (when (>= i r)
            (when (>= i w) (setq sr (- sr (aref source (+ (- i w) row)))))
            (aset target (+ (- i r) row) (/ sr (min (1+ i) (- (+ n -1 w) i) w)))))))))

(defun eas-kde2d--blur-y (n m source target r)
  "Box-blur the N x M SOURCE into TARGET along y with radius R."
  (let ((w (1+ (* 2 r))))
    (dotimes (i n)
      (let ((sr 0.0))
        (dotimes (j (+ m r))
          (when (< j m) (setq sr (+ sr (aref source (+ i (* j n))))))
          (when (>= j r)
            (when (>= j w) (setq sr (- sr (aref source (+ i (* (- j w) n))))))
            (aset target (+ i (* (- j r) n)) (/ sr (min (1+ j) (- (+ m -1 w) j) w)))))))))

(defun eas-kde2d-density (points width height &rest opts)
  "Density grid of POINTS, a list of (PX PY WEIGHT) pixel positions.
The raster is WIDTH x HEIGHT pixels.  OPTS: :cell-size (default 4),
:bandwidth [BX BY] (-1 estimates), :counts.  Return a grid plist
whose x1..x2 by y1..y2 is the raster inside its blur padding."
  (let* ((k (floor (log (max 1 (or (plist-get opts :cell-size) 4)) 2)))
         (cell (ash 1 k))
         (bw (or (plist-get opts :bandwidth) [-1 -1]))
         (rx (ash (eas-kde2d--radius (aref bw 0) (mapcar #'car points)) (- k)))
         (ry (ash (eas-kde2d--radius (aref bw 1) (mapcar #'cadr points)) (- k)))
         (ox (if (> rx 0) (+ rx 2) 0)) (oy (if (> ry 0) (+ ry 2) 0))
         (n (+ (* 2 ox) (ash width (- k)))) (m (+ (* 2 oy) (ash height (- k))))
         (v0 (make-vector (* n m) 0.0)) (v1 (make-vector (* n m) 0.0))
         (values v0))
    (dolist (p points)
      (let ((xi (+ ox (floor (nth 0 p) cell))) (yi (+ oy (floor (nth 1 p) cell))))
        (when (and (<= 0 xi) (< xi n) (<= 0 yi) (< yi m))
          (aset v0 (+ xi (* yi n)) (+ (aref v0 (+ xi (* yi n))) (nth 2 p))))))
    (cond
     ((and (> rx 0) (> ry 0))
      (dotimes (_ 3) (eas-kde2d--blur-x n m v0 v1 rx) (eas-kde2d--blur-y n m v1 v0 ry)))
     ((> rx 0)
      (eas-kde2d--blur-x n m v0 v1 rx) (eas-kde2d--blur-x n m v1 v0 rx) (eas-kde2d--blur-x n m v0 v1 rx)
      (setq values v1))
     ((> ry 0)
      (eas-kde2d--blur-y n m v0 v1 ry) (eas-kde2d--blur-y n m v1 v0 ry) (eas-kde2d--blur-y n m v0 v1 ry)
      (setq values v1)))
    (let* ((sum (cl-reduce #'+ values))
           (s (if (plist-get opts :counts) (expt 2.0 (* -2 k)) (if (> sum 0) (/ 1.0 sum) 1.0))))
      (dotimes (i (* n m)) (aset values i (* s (aref values i))))
      (list :width n :height m :values values :cell cell
            :x1 ox :y1 oy :x2 (+ ox (ash width (- k))) :y2 (+ oy (ash height (- k)))))))

(defun eas-kde2d--extent (rows key fixed)
  "The domain [LO HI] of KEY in ROWS: FIXED, else zero-including and nice."
  (if (and (vectorp fixed) (= (length fixed) 2))
      (vector (float (aref fixed 0)) (float (aref fixed 1)))
    (let ((vals (delq nil (mapcar (lambda (r) (let ((v (plist-get r key))) (and (numberp v) v))) rows))))
      (plist-get (eas-scale-continuous "linear" (if vals (apply #'min vals) 0) (if vals (apply #'max vals) 1)
                                       [0 1] :zero t :nice t)
                 :domain))))

(defun eas-kde2d (rows params)
  "The kde2d transform: one density grid per group of ROWS under PARAMS."
  (let* ((kx (eas-key (plist-get params :x))) (ky (eas-key (plist-get params :y)))
         (kw (and (plist-get params :weight) (eas-key (plist-get params :weight))))
         (size (plist-get params :size)) (w (aref size 0)) (h (aref size 1))
         (rows (seq-filter (lambda (r) (and (numberp (plist-get r kx)) (numberp (plist-get r ky)))) (append rows nil)))
         (ex (eas-kde2d--extent rows kx (plist-get params :xExtent)))
         (ey (eas-kde2d--extent rows ky (plist-get params :yExtent)))
         (spanx (- (aref ex 1) (aref ex 0))) (spany (- (aref ey 1) (aref ey 0)))
         (as (plist-get params :as))
         (groupby (mapcar #'eas-key (eas-seq-list (plist-get params :groupby))))
         (groups (make-hash-table :test 'equal)) (order nil))
    (unless (and (integerp w) (integerp h) (> w 0) (> h 0))
      (eas-signal "INVALID_INPUT" "kde2d size must be [width height] in whole pixels" :path "/size"))
    (dolist (r rows)
      (let ((g (mapcar (lambda (k) (plist-get r k)) groupby)))
        (unless (gethash g groups) (push g order))
        (push r (gethash g groups))))
    (vconcat
     (mapcar
      (lambda (g)
        (let* ((points (mapcar (lambda (r)
                                 (list (eas-kde2d--round (* w (/ (- (plist-get r kx) (aref ex 0)) spanx)))
                                       (eas-kde2d--round (* h (- 1 (/ (- (plist-get r ky) (aref ey 0)) spany))))
                                       (if kw (let ((v (plist-get r kw))) (if (numberp v) v 0)) 1)))
                               (nreverse (gethash g groups))))
               (grid (eas-kde2d-density points w h :cell-size (plist-get params :cellSize)
                                        :bandwidth (plist-get params :bandwidth)
                                        :counts (eas-true-p (plist-get params :counts))))
               (cell (plist-get grid :cell)))
          (append (cl-loop for k in groupby for v in g append (list k v))
                  (list (eas-key as)
                        (append grid (list :scale (vector (/ (* cell spanx) w) (- (/ (* cell spany) h)))
                                           :translate (vector (aref ex 0) (aref ey 1))))
                        (eas-key (concat as "_x0")) (aref ex 0) (eas-key (concat as "_x1")) (aref ex 1)
                        (eas-key (concat as "_y0")) (aref ey 0) (eas-key (concat as "_y1")) (aref ey 1)))))
      (nreverse order)))))

;; Per-cell loops, compiled when the source is loaded interpreted.
(dolist (f '(eas-kde2d--blur-x eas-kde2d--blur-y eas-kde2d-density eas-kde2d-bandwidth))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(eas-register-transform
 "kde2d"
 :doc "Vega's kde2d: a 2D kernel density grid per group, on the chart's nice zero x/y domains."
 :schema '(:x (:type "string" :required t :doc "x field")
           :y (:type "string" :required t :doc "y field")
           :size (:type "array" :required t :doc "[width height] of the plot in pixels")
           :groupby (:type "array" :default [] :doc "fields making one grid per group")
           :weight (:type "string" :doc "point weight field (default 1)")
           :cellSize (:type "number" :default 4 :doc "pixels per grid cell, a power of two")
           :bandwidth (:type "array" :default [-1 -1] :doc "kernel bandwidths in pixels; -1 estimates")
           :counts (:type "boolean" :default :false :doc "points per square pixel, not probability")
           :xExtent (:type "array" :doc "fixed x domain [lo hi], else zero-including and nice")
           :yExtent (:type "array" :doc "fixed y domain [lo hi], else zero-including and nice")
           :as (:type "string" :default "grid" :doc "grid field; AS_x0 AS_x1 AS_y0 AS_y1 hold its extent"))
 :fn #'eas-kde2d)

(provide 'eas-kde2d)
;;; eas-kde2d.el ends here

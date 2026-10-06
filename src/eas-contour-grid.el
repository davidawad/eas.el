;;; eas-contour-grid.el --- value grids for contours and heatmaps -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L0/L1.  A grid is what Vega's kde2d makes and its isocontour
;; and heatmap read: a plist
;;
;;   (:width N :height M :values V [:x1 :y1 :x2 :y2] [:scale S] [:translate T])
;;
;; with V a row-major vector of N*M numbers (nil for no data), row 0 on
;; top.  x1..x2 by y1..y2 is the part a heatmap draws (all of it by
;; default).  A grid point C (in cells, sample i at i + 0.5) stands for
;; ((C - x1) * sx + tx, (C - y1) * sy + ty), Vega's isocontour mapping.
;;
;; Rows stay flat (data/v1), so a grid travels as tidy rows {x, y,
;; value}, one per cell at the cell's centre.  The "grid" adapter turns
;; a Vega grid object ({"width", "height", "values"} plus optional
;; "scale" and "translate", as in volcano.json or annual-precip.json)
;; into such rows; `eas-contour-grid-from-rows' builds the grid back:
;; the distinct x values in order of appearance are its columns, the y
;; values its rows, evenly spaced from the first to the last.  A domain
;; transform's output (kde2d's "grid" field) may hold a grid itself.

;;; Code:

(require 'eas-core)
(require 'eas-data)
(require 'eas-adapters)

(defun eas-contour-grid-p (value)
  "Non-nil when VALUE is a grid plist (or a Vega grid object)."
  (and (eas-object-p value) value (integerp (plist-get value :width)) (integerp (plist-get value :height))
       (vectorp (plist-get value :values))))

(defun eas-contour-grid--number (v)
  "V as a float, or nil when it is no number."
  (and (numberp v) (float v)))

(defun eas-contour-grid-normalize (grid)
  "GRID (a grid plist or a parsed Vega grid object) with float values."
  (unless (eas-contour-grid-p grid)
    (eas-signal "INVALID_INPUT" "A grid needs integer width and height and a values array"))
  (let ((n (* (plist-get grid :width) (plist-get grid :height))))
    (unless (= (length (plist-get grid :values)) n)
      (eas-signal "INVALID_INPUT" (format "A %dx%d grid needs %d values, not %d"
                                          (plist-get grid :width) (plist-get grid :height) n
                                          (length (plist-get grid :values)))))
    (eas-plist-put grid :values (vconcat (mapcar #'eas-contour-grid--number (plist-get grid :values))))))

(defun eas-contour-grid--pair (v default)
  "V (a number or [A B]) as (A . B), or DEFAULT when V is absent."
  (cond ((numberp v) (cons v v))
        ((and (vectorp v) (= (length v) 2)) (cons (aref v 0) (aref v 1)))
        (t default)))

(defun eas-contour-grid-mapping (grid)
  "The (SX SY TX TY X1 Y1) that place GRID's points, as Vega's isocontour."
  (let ((s (eas-contour-grid--pair (plist-get grid :scale) '(1 . 1)))
        (tr (eas-contour-grid--pair (plist-get grid :translate) '(0 . 0))))
    (list (car s) (cdr s) (car tr) (cdr tr) (or (plist-get grid :x1) 0) (or (plist-get grid :y1) 0))))

(defun eas-contour-grid-rows (grid)
  "Tidy rows (:x :y :value) of GRID, one per cell at its centre, row 0 first."
  (let* ((grid (eas-contour-grid-normalize grid))
         (w (plist-get grid :width)) (h (plist-get grid :height)) (vals (plist-get grid :values)))
    (pcase-let ((`(,sx ,sy ,tx ,ty ,x1 ,y1) (eas-contour-grid-mapping grid)))
      (let ((out (make-vector (* w h) nil)))
        (dotimes (j h)
          (dotimes (i w)
            (let ((v (aref vals (+ i (* j w)))))
              (aset out (+ i (* j w))
                    (list :x (+ tx (* sx (- (+ i 0.5) x1))) :y (+ ty (* sy (- (+ j 0.5) y1)))
                          :value (or v :null))))))
        out))))

(defun eas-contour-grid--axis (values)
  "Distinct VALUES in order of appearance, as a vector, and an index hash."
  (let ((index (make-hash-table :test 'eql)) (order nil) (n 0))
    (dolist (v values)
      (unless (gethash v index) (puthash v n index) (push v order) (setq n (1+ n))))
    (cons (vconcat (nreverse order)) index)))

(defun eas-contour-grid-from-rows (rows x y value)
  "The grid of tidy ROWS whose X, Y and VALUE fields hold its cells.
Columns follow the distinct x values in order of appearance and rows
the distinct y values, evenly spaced; a missing cell has no data."
  (let* ((kx (eas-key x)) (ky (eas-key y)) (kv (eas-key value))
         (rows (seq-filter (lambda (r) (and (numberp (plist-get r kx)) (numberp (plist-get r ky)))) rows))
         (xs (eas-contour-grid--axis (mapcar (lambda (r) (float (plist-get r kx))) rows)))
         (ys (eas-contour-grid--axis (mapcar (lambda (r) (float (plist-get r ky))) rows)))
         (w (length (car xs))) (h (length (car ys)))
         (vals (make-vector (* w h) nil))
         (step (lambda (axis) (if (> (length axis) 1)
                                  (/ (- (aref axis (1- (length axis))) (aref axis 0)) (1- (length axis)))
                                1.0))))
    (when (zerop (* w h))
      (eas-signal "INVALID_INPUT" (format "No rows with numeric %s and %s to make a grid of" x y)))
    (dolist (r rows)
      (aset vals (+ (gethash (float (plist-get r kx)) (cdr xs)) (* w (gethash (float (plist-get r ky)) (cdr ys))))
            (eas-contour-grid--number (plist-get r kv))))
    (let ((sx (funcall step (car xs))) (sy (funcall step (car ys))))
      (list :width w :height h :values vals
            :scale (vector sx sy)
            :translate (vector (- (aref (car xs) 0) (* 0.5 sx)) (- (aref (car ys) 0) (* 0.5 sy)))))))

(defun eas-contour-grids (rows params)
  "The grids PARAMS name in ROWS, as a list of (ROW . GRID).
With :field each row holds a grid there (ROW keeps its other fields);
else the rows are tidy cells (:x, :y and :value fields, default x, y
and value), one grid per :groupby group whose ROW holds the group's
fields."
  (let ((field (plist-get params :field)))
    (if (stringp field)
        (let ((key (eas-key field)))
          (cl-loop for row across (vconcat rows)
                   for grid = (plist-get row key)
                   when (eas-contour-grid-p grid)
                   collect (cons (eas--plist-without row key) (eas-contour-grid-normalize grid))))
      (let* ((groupby (mapcar #'eas-key (eas-seq-list (plist-get params :groupby))))
             (groups (make-hash-table :test 'equal)) (order nil))
        (seq-doseq (row rows)
          (let ((g (mapcar (lambda (k) (plist-get row k)) groupby)))
            (unless (gethash g groups) (push g order))
            (push row (gethash g groups))))
        (mapcar (lambda (g)
                  (cons (cl-loop for k in groupby for v in g append (list k v))
                        (eas-contour-grid-from-rows (nreverse (gethash g groups))
                                                    (or (plist-get params :x) "x") (or (plist-get params :y) "y")
                                                    (or (plist-get params :value) "value"))))
                (nreverse order))))))

;; Grids have tens of thousands of cells; compile the per-cell loops
;; when the source is loaded interpreted (as `make test' does).
(dolist (f '(eas-contour-grid-normalize eas-contour-grid-rows eas-contour-grid--axis
             eas-contour-grid-from-rows eas-contour-grids))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(defun eas-contour-grid-max (grid)
  "The largest value of GRID, or 0."
  (let ((m nil))
    (seq-doseq (v (plist-get grid :values)) (when (and v (or (null m) (> v m))) (setq m v)))
    (or m 0)))

;;; The grid adapter

(defun eas-contour-grid--adapt (input)
  "Convert INPUT (a Vega grid object, its JSON text or file, or rows) to data/v1."
  (let ((value (if (or (vectorp input) (and (eas-object-p input) (plist-get input :values)))
                   input
                 (eas-json-parse (eas-adapters--text input)))))
    (cond
     ((vectorp value) (eas-data-from "plist" value))
     ((eas-contour-grid-p value)
      (condition-case err
          (eas-data-from "plist" (eas-contour-grid-rows value))
        (eas-error (eas-shape-invalid (car (cdr err)) nil "values"))))
     (t (eas-shape-invalid "A grid is {\"width\": N, \"height\": M, \"values\": [N*M numbers]} or tidy {x, y, value} rows" nil)))))

(eas-register-adapter
 "grid"
 :doc "A Vega value grid {width, height, values[, scale, translate]} as tidy {x, y, value} cell rows."
 :convert #'eas-contour-grid--adapt
 :example '(:width 2 :height 2 :values [1 2 3 4]))

(provide 'eas-contour-grid)
;;; eas-contour-grid.el ends here

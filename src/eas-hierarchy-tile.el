;;; eas-hierarchy-tile.el --- space-filling layouts: treemap and partition -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's treemap and partition transforms, as x-eas
;; domain transforms over id/parent rows (eas-hierarchy.el):
;;
;;   {"x-eas:transform": "treemap", "field": "size", "method": "squarify",
;;    "ratio": 1.618, "size": [W, H], "round": false, "padding": 0,
;;    "paddingInner", "paddingOuter", "paddingTop", ... ,
;;    "as": ["x0", "y0", "x1", "y1", "depth", "children"]}
;;
;;   {"x-eas:transform": "partition", "field": "size", "size": [W, H],
;;    "padding": 0, "round": false,
;;    "as": ["x0", "y0", "x1", "y1", "depth", "children"]}
;;
;; Treemap methods are d3's tilings: squarify (and resquarify, the same
;; for one layout) with the golden ratio by default, binary, dice,
;; slice and slicedice.  Partition stacks the depths into bands of
;; equal height (an icicle); read x as an angle and y as a radius for a
;; sunburst.  Values are summed from FIELD, so internal rows usually
;; leave it empty; sort {"field": "value"} orders siblings as Vega does.

;;; Code:

(require 'eas-core)
(require 'eas-hierarchy)

(defconst eas-hierarchy-phi (/ (+ 1 (sqrt 5)) 2) "The golden ratio, squarify's default.")

(defmacro eas-hierarchy--box (node x0 y0 x1 y1)
  "Set NODE's rectangle to X0 Y0 X1 Y1."
  `(setf (eas-hierarchy-node-x0 ,node) ,x0 (eas-hierarchy-node-y0 ,node) ,y0
         (eas-hierarchy-node-x1 ,node) ,x1 (eas-hierarchy-node-y1 ,node) ,y1))

;;; Tilings

(defun eas-hierarchy-dice (nodes value x0 y0 x1 y1)
  "Lay NODES (summing to VALUE) side by side along x in X0 Y0 X1 Y1."
  (let ((k (if (and value (/= value 0)) (/ (- x1 x0) (float value)) 0)))
    (dolist (n nodes)
      (eas-hierarchy--box n x0 y0 (setq x0 (+ x0 (* (eas-hierarchy-node-value n) k))) y1))))

(defun eas-hierarchy-slice (nodes value x0 y0 x1 y1)
  "Lay NODES (summing to VALUE) one above the other along y in X0 Y0 X1 Y1."
  (let ((k (if (and value (/= value 0)) (/ (- y1 y0) (float value)) 0)))
    (dolist (n nodes)
      (eas-hierarchy--box n x0 y0 x1 (setq y0 (+ y0 (* (eas-hierarchy-node-value n) k)))))))

(defun eas-hierarchy-squarify (ratio parent x0 y0 x1 y1)
  "Squarify PARENT's children into X0 Y0 X1 Y1 at RATIO, as d3 does."
  (let* ((nodes (vconcat (eas-hierarchy-node-children parent))) (n (length nodes))
         (value (eas-hierarchy-node-value parent)) (i0 0) (i1 0))
    (while (< i0 n)
      (let* ((dx (- x1 x0)) (dy (- y1 y0)) sum)
        ;; The next non-empty node starts the row.
        (setq sum (eas-hierarchy-node-value (aref nodes i1)) i1 (1+ i1))
        (while (and (= sum 0) (< i1 n))
          (setq sum (eas-hierarchy-node-value (aref nodes i1)) i1 (1+ i1)))
        (let* ((lo sum) (hi sum)
               (alpha (/ (max (/ dy (float dx)) (/ dx (float dy))) (* value ratio)))
               (beta (* sum sum alpha))
               (best (max (/ hi beta) (/ beta lo))))
          (catch 'done
            (while (< i1 n)
              (let* ((v (eas-hierarchy-node-value (aref nodes i1))))
                (setq sum (+ sum v))
                (when (< v lo) (setq lo v))
                (when (> v hi) (setq hi v))
                (setq beta (* sum sum alpha))
                (let ((r (max (/ hi beta) (/ beta lo))))
                  (when (> r best) (setq sum (- sum v)) (throw 'done nil))
                  (setq best r)))
              (setq i1 (1+ i1))))
          (let ((row (append (seq-subseq nodes i0 i1) nil)))
            (if (< dx dy)
                (eas-hierarchy-dice row sum x0 y0 x1
                                    (if (/= value 0) (setq y0 (+ y0 (/ (* dy sum) (float value)))) y1))
              (eas-hierarchy-slice row sum x0 y0
                                   (if (/= value 0) (setq x0 (+ x0 (/ (* dx sum) (float value)))) x1)
                                   y1)))
          (setq value (- value sum) i0 i1))))))

(defun eas-hierarchy-binary (parent x0 y0 x1 y1)
  "Tile PARENT's children into X0 Y0 X1 Y1 by d3's binary method."
  (let* ((nodes (vconcat (eas-hierarchy-node-children parent))) (n (length nodes))
         (sums (make-vector (1+ n) 0)))
    (dotimes (i n) (aset sums (1+ i) (+ (aref sums i) (eas-hierarchy-node-value (aref nodes i)))))
    (cl-labels
        ((part (i j value x0 y0 x1 y1)
           (if (>= i (1- j))
               (eas-hierarchy--box (aref nodes i) x0 y0 x1 y1)
             (let* ((offset (aref sums i)) (target (+ (/ value 2.0) offset)) (k (1+ i)) (hi (1- j)))
               (while (< k hi)
                 (let ((mid (ash (+ k hi) -1)))
                   (if (< (aref sums mid) target) (setq k (1+ mid)) (setq hi mid))))
               (when (and (< (- target (aref sums (1- k))) (- (aref sums k) target)) (< (1+ i) k))
                 (setq k (1- k)))
               (let* ((left (- (aref sums k) offset)) (right (- value left)))
                 (if (> (- x1 x0) (- y1 y0))
                     (let ((xk (if (/= value 0) (/ (+ (* x0 right) (* x1 left)) (float value)) x1)))
                       (part i k left x0 y0 xk y1)
                       (part k j right xk y0 x1 y1))
                   (let ((yk (if (/= value 0) (/ (+ (* y0 right) (* y1 left)) (float value)) y1)))
                     (part i k left x0 y0 x1 yk)
                     (part k j right x0 yk x1 y1))))))))
      (when (> n 0) (part 0 n (eas-hierarchy-node-value parent) x0 y0 x1 y1)))))

(defun eas-hierarchy-tile (method ratio)
  "The tiling function (PARENT X0 Y0 X1 Y1) of treemap METHOD at RATIO."
  (pcase method
    ((or "squarify" "resquarify") (lambda (p x0 y0 x1 y1) (eas-hierarchy-squarify ratio p x0 y0 x1 y1)))
    ("binary" #'eas-hierarchy-binary)
    ("dice" (lambda (p x0 y0 x1 y1) (eas-hierarchy-dice (eas-hierarchy-node-children p) (eas-hierarchy-node-value p) x0 y0 x1 y1)))
    ("slice" (lambda (p x0 y0 x1 y1) (eas-hierarchy-slice (eas-hierarchy-node-children p) (eas-hierarchy-node-value p) x0 y0 x1 y1)))
    ("slicedice" (lambda (p x0 y0 x1 y1)
                   (funcall (if (cl-oddp (eas-hierarchy-node-depth p)) #'eas-hierarchy-slice #'eas-hierarchy-dice)
                            (eas-hierarchy-node-children p) (eas-hierarchy-node-value p) x0 y0 x1 y1)))
    (_ (eas-signal "INVALID_INPUT"
                   (format "treemap method %S is not squarify, resquarify, binary, dice, slice or slicedice" method)
                   :path "method"))))

(defun eas-hierarchy--round-boxes (root)
  "Round every rectangle under ROOT half up, as d3's round option does."
  (eas-hierarchy-each-before
   root (lambda (n) (eas-hierarchy--box n (eas-hierarchy-round (eas-hierarchy-node-x0 n))
                                        (eas-hierarchy-round (eas-hierarchy-node-y0 n))
                                        (eas-hierarchy-round (eas-hierarchy-node-x1 n))
                                        (eas-hierarchy-round (eas-hierarchy-node-y1 n))))))

;;; treemap

(defun eas-hierarchy-treemap (root params)
  "Lay ROOT out as d3's treemap under PARAMS.
PARAMS holds size, method, ratio, the paddings and round."
  (let* ((size (or (plist-get params :size) [1 1]))
         (tile (eas-hierarchy-tile (plist-get params :method) (plist-get params :ratio)))
         (pad (lambda (k) (let ((v (plist-get params k)))
                            (cond ((numberp v) v) ((numberp (plist-get params :padding)) (plist-get params :padding))
                                  (t 0)))))
         (inner (funcall pad :paddingInner)) (outer (funcall pad :paddingOuter))
         (side (lambda (k) (let ((v (plist-get params k))) (if (numberp v) v outer))))
         (top (funcall side :paddingTop)) (right (funcall side :paddingRight))
         (bottom (funcall side :paddingBottom)) (left (funcall side :paddingLeft))
         (stack (make-hash-table)))
    (puthash 0 0 stack)
    (eas-hierarchy--box root 0 0 (aref size 0) (aref size 1))
    (eas-hierarchy-each-before
     root (lambda (n)
            (let* ((p (gethash (eas-hierarchy-node-depth n) stack 0))
                   (x0 (+ (eas-hierarchy-node-x0 n) p)) (y0 (+ (eas-hierarchy-node-y0 n) p))
                   (x1 (- (eas-hierarchy-node-x1 n) p)) (y1 (- (eas-hierarchy-node-y1 n) p)))
              (when (< x1 x0) (setq x0 (/ (+ x0 x1) 2.0) x1 x0))
              (when (< y1 y0) (setq y0 (/ (+ y0 y1) 2.0) y1 y0))
              (eas-hierarchy--box n x0 y0 x1 y1)
              (when (eas-hierarchy-node-children n)
                (let ((q (/ inner 2.0)))
                  (puthash (1+ (eas-hierarchy-node-depth n)) q stack)
                  (setq x0 (+ x0 (- left q)) y0 (+ y0 (- top q)) x1 (- x1 (- right q)) y1 (- y1 (- bottom q)))
                  (when (< x1 x0) (setq x0 (/ (+ x0 x1) 2.0) x1 x0))
                  (when (< y1 y0) (setq y0 (/ (+ y0 y1) 2.0) y1 y0))
                  (funcall tile n x0 y0 x1 y1))))))
    (when (eq (plist-get params :round) t) (eas-hierarchy--round-boxes root))))

(defconst eas-hierarchy--box-fields
  (list (cons "x0" #'eas-hierarchy-node-x0) (cons "y0" #'eas-hierarchy-node-y0)
        (cons "x1" #'eas-hierarchy-node-x1) (cons "y1" #'eas-hierarchy-node-y1)
        (cons "depth" #'eas-hierarchy-node-depth) (cons "children" #'eas-hierarchy-children-count))
  "The fields treemap and partition write.")

(eas-register-transform
 "treemap"
 :doc "Treemap layout of id/parent rows: squarify, resquarify, binary, dice, slice or slicedice (Vega's treemap)."
 :schema (append eas-hierarchy-common-schema
                 `(:method (:type "string" :default "squarify" :doc "the tiling")
                   :ratio (:type "number" :default ,eas-hierarchy-phi :doc "squarify's target aspect ratio")
                   :size (:type "array" :doc "[width height] (default [1 1])")
                   :round (:type "boolean" :default :false :doc "round the rectangles to whole pixels")
                   :padding (:type "number" :doc "every padding below")
                   :paddingInner (:type "number" :doc "between siblings")
                   :paddingOuter (:type "number" :doc "inside a parent, on all four sides")
                   :paddingTop (:type "number") :paddingRight (:type "number")
                   :paddingBottom (:type "number") :paddingLeft (:type "number")
                   :as (:type "array" :default ["x0" "y0" "x1" "y1" "depth" "children"] :doc "output fields")))
 :fn (lambda (rows params) (eas-hierarchy-layout rows params #'eas-hierarchy-treemap eas-hierarchy--box-fields)))

;;; partition

(defun eas-hierarchy-partition (root params)
  "Lay ROOT out as d3's partition under PARAMS (size, padding, round)."
  (let* ((size (or (plist-get params :size) [1 1])) (dx (aref size 0)) (dy (float (aref size 1)))
         (padding (or (plist-get params :padding) 0))
         (n (1+ (eas-hierarchy-node-height root))))
    (eas-hierarchy--box root padding padding dx (/ dy n))
    (eas-hierarchy-each-before
     root (lambda (node)
            (when (eas-hierarchy-node-children node)
              (eas-hierarchy-dice (eas-hierarchy-node-children node) (eas-hierarchy-node-value node)
                                  (eas-hierarchy-node-x0 node) (/ (* dy (1+ (eas-hierarchy-node-depth node))) n)
                                  (eas-hierarchy-node-x1 node) (/ (* dy (+ 2 (eas-hierarchy-node-depth node))) n)))
            (let ((x0 (eas-hierarchy-node-x0 node)) (y0 (eas-hierarchy-node-y0 node))
                  (x1 (- (eas-hierarchy-node-x1 node) padding)) (y1 (- (eas-hierarchy-node-y1 node) padding)))
              (when (< x1 x0) (setq x0 (/ (+ x0 x1) 2.0) x1 x0))
              (when (< y1 y0) (setq y0 (/ (+ y0 y1) 2.0) y1 y0))
              (eas-hierarchy--box node x0 y0 x1 y1))))
    (when (eq (plist-get params :round) t) (eas-hierarchy--round-boxes root))))

(eas-register-transform
 "partition"
 :doc "Adjacency (icicle) layout of id/parent rows; read x as angle and y as radius for a sunburst (Vega's partition)."
 :schema (append eas-hierarchy-common-schema
                 '(:size (:type "array" :doc "[width height] (default [1 1])")
                   :padding (:type "number" :default 0 :doc "gap between neighbouring cells")
                   :round (:type "boolean" :default :false :doc "round the cells to whole pixels")
                   :as (:type "array" :default ["x0" "y0" "x1" "y1" "depth" "children"] :doc "output fields")))
 :fn (lambda (rows params) (eas-hierarchy-layout rows params #'eas-hierarchy-partition eas-hierarchy--box-fields)))

(provide 'eas-hierarchy-tile)
;;; eas-hierarchy-tile.el ends here

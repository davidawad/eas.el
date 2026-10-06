;;; eas-compile-grid.el --- grid-aligned concatenation (repeat) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 layout.  A row-and-column repeat is laid out by Vega-Lite
;; as a grid (align "all"): every column is as wide as its widest cell
;; and every plot in a column starts at the same x, every row as tall
;; as its tallest cell.  `eas-repeat-expand' marks the vconcat of
;; hconcats it produces with x-eas.grid, `eas-vl-lower' a column
;; repeat's hconcat (one row); `eas-place-arrange-grid' places such a
;; node.

;;; Code:

(require 'eas-core)

(defun eas-place--grid-rows (node)
  "NODE's rows, each a list of cell nodes.
That is one row for an hconcat of single views (a column repeat), else
its children's cells."
  (if (and (equal (plist-get node :concat) "h")
           (seq-every-p (lambda (c) (plist-get c :group)) (plist-get node :children)))
      (list (plist-get node :children))
    (mapcar (lambda (row) (plist-get row :children)) (plist-get node :children))))
(declare-function eas-facet-layout-min-plot "eas-facet-layout")
(declare-function eas-layout-text-p "eas-layout")

(defun eas-place-grid-p (node)
  "Non-nil when layout NODE is a grid: rows of single-view cells."
  (and (plist-get node :grid)
       (or (and (equal (plist-get node :concat) "h")
                (seq-every-p (lambda (c) (plist-get c :group)) (plist-get node :children)))
           (seq-every-p (lambda (row) (and (plist-get row :concat)
                                           (seq-every-p (lambda (c) (plist-get c :group)) (plist-get row :children))))
                        (plist-get node :children)))))

(defun eas-place-arrange-grid (node ox oy metrics)
  "Place grid NODE's cells with its top-left at OX OY; return (W . H).
Vega's gridLayout with align \"all\" and bounds \"full\": each cell's
box is its plot with its chrome; columns are as wide as the widest box
plus the spacing and the largest left chrome of the later columns, rows
likewise, so the spacing separates the boxes and plots line up.
METRICS gives the default spacing."
  (let* ((spacing (or (plist-get node :spacing) (plist-get metrics :spacing)))
         (rows (mapcar (lambda (row) (mapcar (lambda (c) (plist-get c :group)) row))
                       (eas-place--grid-rows node)))
         (chrome (lambda (g k) (or (plist-get (plist-get g :chrome) k) 0)))
         (x2 (lambda (g) (ceiling (+ (plist-get g :w) (funcall chrome g :right)))))
         (y2 (lambda (g) (ceiling (max (+ (plist-get g :h) (funcall chrome g :bottom)) (funcall chrome g :legend-h)))))
         (cells (apply #'append rows))
         (xmax (apply #'max 0 (mapcar x2 cells)))
         (ymax (apply #'max 0 (mapcar y2 cells)))
         (offx (apply #'max 0 (cl-loop for r in rows append (mapcar (lambda (g) (+ spacing (ceiling (funcall chrome g :left)))) (cdr r)))))
         (offy (apply #'max 0 (mapcar (lambda (g) (+ spacing (ceiling (funcall chrome g :top)))) (apply #'append (cdr rows)))))
         ;; align "all": the first column and row line up with the widest chrome of any cell.
         (left (apply #'max 0 (mapcar (lambda (g) (funcall chrome g :left)) cells)))
         (top (apply #'max 0 (mapcar (lambda (g) (funcall chrome g :top)) cells))))
    (cl-loop for r in rows for i from 0
             do (cl-loop for g in r for j from 0
                         do (plist-put g :x0 (+ ox left (* j (+ xmax offx))))
                         (plist-put g :y0 (+ oy top (* i (+ ymax offy))))))
    (cons (+ left (* (1- (apply #'max (mapcar #'length rows))) (+ xmax offx)) xmax)
          (+ top (* (1- (length rows)) (+ ymax offy)) ymax))))

(defun eas-place-fit-grid (node width height metrics)
  "Resize grid NODE's plots so the grid is WIDTH by HEIGHT under METRICS."
  (let* ((spacing (or (plist-get node :spacing) (plist-get metrics :spacing)))
         (rows (mapcar (lambda (row) (mapcar (lambda (c) (plist-get c :group)) row))
                       (eas-place--grid-rows node)))
         (cells (apply #'append rows))
         (most (lambda (k) (apply #'max 0 (mapcar (lambda (g) (plist-get (plist-get g :chrome) k)) cells))))
         (ncol (apply #'max (mapcar #'length rows)))
         (cw (/ (- width (* spacing (1- ncol))) (float ncol)))
         (ch (/ (- height (* spacing (1- (length rows)))) (float (length rows))))
         (w (max (* 4 (aref (plist-get metrics :cell) 0)) (- cw (funcall most :left) (funcall most :right))))
         (h (max (* 2 (aref (plist-get metrics :cell) 1)) (- ch (funcall most :top) (funcall most :bottom)))))
    ;; Never so small that a cell's axis labels collide (the canvas grows instead).
    (when (fboundp 'eas-facet-layout-min-plot)
      (dolist (g cells)
        (let ((least (eas-facet-layout-min-plot g metrics)))
          (setq w (max w (car least)) h (max h (cdr least))))))
    ;; Text plots span whole cells: one ending mid-cell put its axis
    ;; labels on its axis line.
    (when (eas-layout-text-p metrics)
      (let ((cell (plist-get metrics :cell)))
        (setq w (* (aref cell 0) (max 4 (floor w (aref cell 0))))
              h (* (aref cell 1) (max 2 (floor h (aref cell 1)))))))
    (dolist (g cells) (plist-put g :w w) (plist-put g :h h))))

(provide 'eas-compile-grid)
;;; eas-compile-grid.el ends here

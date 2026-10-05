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
  "NODE's rows, each a list of cell nodes: one row for an hconcat of
single views (a column repeat), else its children's cells."
  (if (and (equal (plist-get node :concat) "h")
           (seq-every-p (lambda (c) (plist-get c :group)) (plist-get node :children)))
      (list (plist-get node :children))
    (mapcar (lambda (row) (plist-get row :children)) (plist-get node :children))))

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
Vega-Lite's align \"all\": every cell is as wide and as tall as the
largest, and every plot sits at the same offset inside its cell."
  (let* ((spacing (plist-get metrics :spacing))
         (rows (mapcar (lambda (row) (mapcar (lambda (c) (plist-get c :group)) row)) (eas-place--grid-rows node)))
         (cells (apply #'append rows))
         (most (lambda (fn) (apply #'max 0 (mapcar fn cells))))
         (chrome (lambda (g k) (plist-get (plist-get g :chrome) k)))
         (left (funcall most (lambda (g) (funcall chrome g :left))))
         (top (funcall most (lambda (g) (funcall chrome g :top))))
         (width (+ left (funcall most (lambda (g) (+ (plist-get g :w) (funcall chrome g :right))))))
         (height (+ top (funcall most (lambda (g) (max (+ (plist-get g :h) (funcall chrome g :bottom))
                                                       (or (funcall chrome g :legend-h) 0)))))))
    (cl-loop for r in rows for i from 0
             do (cl-loop for g in r for j from 0
                         do (plist-put g :x0 (+ ox (* j (+ width spacing)) left))
                         (plist-put g :y0 (+ oy (* i (+ height spacing)) top))))
    (cons (- (* (apply #'max (mapcar #'length rows)) (+ width spacing)) spacing)
          (- (* (length rows) (+ height spacing)) spacing))))

(defun eas-place-fit-grid (node width height metrics)
  "Resize grid NODE's plots so the grid is WIDTH by HEIGHT."
  (let* ((spacing (plist-get metrics :spacing))
         (rows (mapcar (lambda (row) (mapcar (lambda (c) (plist-get c :group)) row)) (eas-place--grid-rows node)))
         (cells (apply #'append rows))
         (most (lambda (k) (apply #'max 0 (mapcar (lambda (g) (plist-get (plist-get g :chrome) k)) cells))))
         (ncol (apply #'max (mapcar #'length rows)))
         (cw (/ (- width (* spacing (1- ncol))) (float ncol)))
         (ch (/ (- height (* spacing (1- (length rows)))) (float (length rows))))
         (w (max (* 4 (aref (plist-get metrics :cell) 0)) (- cw (funcall most :left) (funcall most :right))))
         (h (max (* 2 (aref (plist-get metrics :cell) 1)) (- ch (funcall most :top) (funcall most :bottom)))))
    (dolist (g cells) (plist-put g :w w) (plist-put g :h h))))

(provide 'eas-compile-grid)
;;; eas-compile-grid.el ends here

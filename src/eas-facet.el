;;; eas-facet.el --- row and column facets as concatenated cells -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2/L4.  A facet (row, column or wrapped, in the encoding or
;; at the top level) is lowered by eas-facet-grid.el into a vconcat of
;; hconcat rows, one cell per level, and laid out by eas-facet-layout.el
;; as Vega's trellis.  This file registers the lowering as a spec
;; rewrite (so parse, check and compile all see it) and places the
;; one-line cell headers a terminal draws above each cell.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-facet-title)
(require 'eas-facet-grid)
(require 'eas-concat-title)
(require 'eas-geo)
(require 'eas-facet-layout)
(require 'eas-concat-flush)

;;; Lowering

(defvar eas-facet-keep nil
  "Non-nil while resolving for export: facets stay Vega-Lite facets.")

(defun eas-facet-lower (spec)
  "SPEC with every facet over inline data lowered to a grid of cells.
Row, column and wrapped facets, in the encoding or at the top level,
become a vconcat of hconcat rows (`eas-facet-grid-lower'); a facet whose
data is not inline is left alone and reported unsupported.  While
`eas-facet-keep' is non-nil (export) SPEC is returned as it is."
  (if eas-facet-keep spec (eas-facet-grid-lower spec)))

(defvar eas-spec-rewrite-functions)
(add-hook 'eas-spec-rewrite-functions #'eas-facet-lower t)

(defvar eas-spec-feature-functions)
(add-hook 'eas-spec-feature-functions #'eas-facet-grid-features)

(defun eas-facet-expand (spec)
  "SPEC with any facet left after parsing lowered to a grid of cells."
  (eas-facet-grid-lower spec))

(defconst eas-facet-header-size 10 "Vega-Lite's header labelFontSize.")
(defconst eas-facet-header-padding 10 "Vega-Lite's header labelPadding.")

;;; Headers

(defun eas-facet-header-extent (header metrics)
  "Space HEADER (and its facet title) needs beside its plot: (SIDE . PIXELS).
METRICS gives the target."
  (let ((e (eas-facet--label-extent header metrics)))
    (cons (car e) (+ (cdr e) (eas-facet-title-band header metrics)))))

(defun eas-facet--size (header)
  "HEADER's label font size (labelFontSize, Vega-Lite's 10)."
  (or (plist-get header :fontSize) eas-facet-header-size))

(defun eas-facet--pad (header)
  "HEADER's label padding (labelPadding, Vega-Lite's 10)."
  (or (plist-get header :padding) eas-facet-header-padding))

(defun eas-facet--label-extent (header metrics)
  "Space HEADER's label needs beside its plot under METRICS: (SIDE . PIXELS)."
  (cond
   ((eas-layout-text-p metrics)
    (cons :top (plist-get metrics :label-size)))
   ;; A horizontal row label takes its widest text.
   ((and (equal (plist-get header :orient) "left") (eql (plist-get header :angle) 0))
    (cons :left (+ (eas-facet--pad header)
                   (apply #'max 0 (mapcar (lambda (l) (eas-layout-text-width metrics l (eas-facet--size header)))
                                          (append (plist-get header :labels) nil))))))
   (t (cons (if (equal (plist-get header :orient) "top") :top :left)
            (+ (eas-facet--pad header) (eas-facet--size header) 1)))))

(defun eas-facet-header-place (header bounds inset metrics)
  "HEADER placed beside plot BOUNDS [X Y W H], outside INSET pixels of axes.
Return (:text :x :y :angle :align :baseline :fontSize) and the label's
own :color :fontWeight :font :fontStyle.  METRICS gives the target."
  (let* ((top (equal (plist-get header :orient) "top"))
         (x0 (- (aref bounds 0) (if top 0 inset))) (y0 (- (aref bounds 1) (if top inset 0)))
         (w (aref bounds 2)) (h (aref bounds 3))
         (text (plist-get header :text))
         (size (eas-facet--size header)) (pad (eas-facet--pad header))
         (look (cl-loop for k in '(:color :fontWeight :font :fontStyle)
                        when (plist-get header k) append (list k (plist-get header k)))))
    (append
     (cond
      ((eas-layout-text-p metrics)
       ;; Cut to the cell (and the gap to the next), so neighbours stay legible.
       (list :text (eas-layout-truncate metrics text (plist-get metrics :label-size)
                                        (+ w (plist-get metrics :spacing) (- (plist-get metrics :char-w))))
             :x x0 :y (- y0 (plist-get metrics :label-size)) :angle 0 :align "left" :baseline "top"
             :fontSize (plist-get metrics :label-size)))
      ((equal (plist-get header :orient) "top")
       (list :text text :x (+ x0 (/ w 2.0)) :y (- y0 pad) :angle 0
             :align "center" :baseline "bottom" :fontSize size))
      ((eql (plist-get header :angle) 0)
       (list :text text :x (- x0 pad) :y (+ y0 (/ h 2.0)) :angle 0
             :align "right" :baseline "middle" :fontSize size))
      (t (list :text text :x (- x0 pad) :y (+ y0 (/ h 2.0)) :angle -90
               :align "center" :baseline "bottom" :fontSize size)))
     (unless (eas-layout-text-p metrics) look))))

(provide 'eas-facet)
;;; eas-facet.el ends here

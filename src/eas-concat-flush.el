;;; eas-concat-flush.el --- concatenation with bounds "flush" -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 layout.  Vega-Lite's "bounds": "flush" lays a concat's
;; views out by their plot sizes alone (Vega's gridLayout with a flush
;; bounding box): the spacing separates plots, not their axes and
;; legends, and every plot starts at the same x (vconcat) or y
;; (hconcat).  Axes then overhang the gaps, which is what marginal
;; histograms want.  A nested concat counts as 0x0 there, as in Vega
;; (its group has no size of its own), so the next view starts at it.  `eas-concat-flush-arrange' places such a node; the
;; chrome around the plots still counts for the block's size.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(declare-function eas-place-arrange "eas-compile-place")
(declare-function eas-place--groups "eas-compile-place")

(defun eas-concat-flush-p (node)
  "Non-nil when layout NODE is a concat with bounds \"flush\"."
  (and (plist-get node :concat) (equal (plist-get node :bounds) "flush")))

(defun eas-concat-flush--plan (node metrics)
  "Plan flush NODE under METRICS: its plot origins and its flush size.
Return (ORIGINS (W . H)), ORIGINS ((G X . Y) ...) relative to the first
plot."
  (let* ((vertical (equal (plist-get node :concat) "v"))
         (spacing (or (plist-get node :spacing) (plist-get metrics :spacing)))
         (cursor 0) (cross 0) out)
    (dolist (child (plist-get node :children))
      (let* ((sub (cond ((plist-get child :group)
                         (let ((g (plist-get child :group)))
                           (list (list (cons g (cons 0 0))) (cons (plist-get g :w) (plist-get g :h)))))
                        ((eas-concat-flush-p child) (eas-concat-flush--plan child metrics))
                        ;; Any other block: its own placement, its first plot at 0,0.
                        (t (let* ((size (eas-place-arrange child 0 0 metrics))
                                  (groups (eas-place--groups child))
                                  (g0 (car groups)) (x0 (plist-get g0 :x0)) (y0 (plist-get g0 :y0)))
                             (list (mapcar (lambda (g) (cons g (cons (- (plist-get g :x0) x0) (- (plist-get g :y0) y0))))
                                           groups)
                                   (cons (- (car size) x0) (- (cdr size) y0)))))))
             (dx (if vertical 0 cursor)) (dy (if vertical cursor 0)))
        (dolist (p (car sub))
          (push (cons (car p) (cons (+ dx (cadr p)) (+ dy (cddr p)))) out))
        ;; Vega's flush box of a nested concat is its group's width and
        ;; height, which it does not set: the next view starts right after it.
        (let ((size (if (plist-get child :group) (cadr sub) '(0 . 0))))
          (setq cursor (+ cursor (if vertical (cdr size) (car size)) spacing)
                cross (max cross (if vertical (car size) (cdr size)))))))
    (setq cursor (max 0 (- cursor spacing)))
    (list (nreverse out) (if vertical (cons cross cursor) (cons cursor cross)))))

(defun eas-concat-flush--extent (origins)
  "[LEFT TOP RIGHT BOTTOM] of ORIGINS' groups with their chrome."
  (let ((l 0) (tp 0) (r 0) (b 0))
    (pcase-dolist (`(,g ,x . ,y) origins)
      (let ((c (plist-get g :chrome)))
        (setq l (min l (- x (plist-get c :left))) tp (min tp (- y (plist-get c :top)))
              r (max r (+ x (plist-get g :w) (plist-get c :right)))
              b (max b (+ y (max (+ (plist-get g :h) (plist-get c :bottom)) (or (plist-get c :legend-h) 0)))))))
    (vector l tp r b)))

(defun eas-concat-flush-arrange (node ox oy metrics)
  "Place flush NODE with its block's top-left at OX OY; return (W . H).
Nil when NODE is not flush or METRICS target text."
  (when (and (eas-concat-flush-p node) (not (eas-layout-text-p metrics)))
    (let* ((origins (car (eas-concat-flush--plan node metrics)))
           (e (eas-concat-flush--extent origins)))
      (pcase-dolist (`(,g ,x . ,y) origins)
        (plist-put g :x0 (+ ox (- (aref e 0)) x))
        (plist-put g :y0 (+ oy (- (aref e 1)) y)))
      (cons (ceiling (- (aref e 2) (aref e 0))) (ceiling (- (aref e 3) (aref e 1)))))))

(defun eas-concat-flush-lead (node key)
  "Chrome on side KEY before flush NODE's first plot, or nil."
  (let ((metrics (plist-get node :flush-metrics)))
    (when (and metrics (eas-concat-flush-p node))
      (let ((e (eas-concat-flush--extent (car (eas-concat-flush--plan node metrics)))))
        (- (aref e (if (eq key :left) 0 1)))))))

(defun eas-concat-flush-prepare (tree metrics)
  "Remember METRICS on TREE's flush nodes (for `eas-concat-flush-lead')."
  (unless (plist-get tree :group)
    (when (eas-concat-flush-p tree)
      (if (eas-layout-text-p metrics) (plist-put tree :bounds nil) (plist-put tree :flush-metrics metrics)))
    (dolist (c (plist-get tree :children)) (eas-concat-flush-prepare c metrics))))

(defvar eas-place-arrange-functions)
(defvar eas-place-lead-functions)
(add-hook 'eas-place-arrange-functions #'eas-concat-flush-arrange)
(add-hook 'eas-place-lead-functions #'eas-concat-flush-lead)

(provide 'eas-concat-flush)
;;; eas-concat-flush.el ends here

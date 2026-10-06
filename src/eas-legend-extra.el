;;; eas-legend-extra.el --- horizontal gradient legends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A gradient legend with legend.direction "horizontal"
;; lays its bar out left to right, gradientLength long (default 200)
;; and gradientThickness tall, under its title, with labels below the
;; bar: the first aligned left, the last right, the rest centred, as
;; Vega's legend guide does.  With titleOrient "left" the title sits
;; beside the bar instead, cut to titleLimit (180), and the bar starts
;; titlePadding past it.  The text target keeps vertical legends.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(declare-function eas-legend--title "eas-legend")

(defun eas-legend-extra-horizontal-p (legend metrics)
  "Non-nil when LEGEND is a horizontal gradient under svg METRICS."
  (and (equal (plist-get legend :type) "gradient") (equal (plist-get legend :direction) "horizontal")
       (not (eas-layout-text-p metrics))))

(defconst eas-legend-extra-title-limit 180
  "Vega's legend titleLimit: the widest a legend title is drawn, in pixels.")

(defun eas-legend-extra--side-title (legend x y metrics)
  "LEGEND's title left of its bar (titleOrient \"left\") at X Y, or nil.
Return ((MARK . Y) . SHIFT): the title, cut to titleLimit, and how far
right of X the bar starts (the title's width plus titlePadding).
METRICS are the layout's."
  (when-let* ((text (and (equal (plist-get legend :title-orient) "left") (plist-get legend :title))))
    (let* ((size (plist-get metrics :legend-title-size))
           (text (eas-layout-truncate metrics text size eas-legend-extra-title-limit))
           (w (eas-layout-text-width metrics text size (plist-get metrics :legend-title-weight))))
      (cons (cons (list :text text :x x :y y :align "left" :baseline "top") y)
            (+ (min w eas-legend-extra-title-limit) (plist-get metrics :legend-title-pad))))))

(defun eas-legend-extra-place-horizontal (legend x y metrics)
  "Horizontal gradient LEGEND with its top-left at X Y, sized by METRICS."

  (let* ((fs (plist-get metrics :legend-label-size))
         (side (eas-legend-extra--side-title legend x y metrics))
         (title (or (car side) (eas-legend--title legend x y metrics)))
         (x0 x) (x (+ x (or (cdr side) 0)))
         (by (cdr title)) (thick (plist-get metrics :gradient-thickness))
         ;; Vega-Lite: clamp(plot width, 100, 200).
         (glen (or (plist-get legend :gradient-length) (max 100 (min 200 (or (plist-get legend :plot-w) 200)))))
         (d (plist-get legend :domain)) (span (max 1e-9 (- (aref d 1) (aref d 0))))
         (ly (+ by thick (plist-get metrics :legend-label-offset)))
         (box (vector x by (+ x glen) (+ by thick)))
         (entries (mapcar (lambda (e)
                            (let* ((perc (/ (- (plist-get e :value) (aref d 0)) (float span)))
                                   (align (cond ((<= perc 0) "left") ((>= perc 1) "right") (t "center")))
                                   (lx (+ x (* glen perc)))
                                   (b (eas-layout-text-bounds metrics (plist-get e :label) fs lx ly align "top")))
                              (setq box (eas-layout-union box b))
                              (append e (list :lx lx :ly ly :align align :baseline "top" :sx lx :sy by
                                              :bounds (vector (aref b 0) by (- (aref b 2) (aref b 0)) (+ thick fs))))))
                          (plist-get legend :entries))))
    (when (car title)
      (setq box (eas-layout-union box (eas-layout-text-bounds metrics (plist-get (car title) :text)
                                                                  (plist-get metrics :legend-title-size) x0 y "left" "top"
                                                                  0 (plist-get metrics :legend-title-weight)))))
    (append (eas--plist-without legend :entries)
            (list :x x0 :y y :width (ceiling (- (aref box 2) x0)) :font-size fs
                  :bar (vector x by glen thick)
                  :box (vector x0 y (+ x0 (ceiling (- (aref box 2) x0))) (+ y (ceiling (- (aref box 3) y))))
                  :entries (vconcat entries))
            (when (car title) (list :title-mark (car title))))))

(provide 'eas-legend-extra)
;;; eas-legend-extra.el ends here

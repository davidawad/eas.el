;;; eas-legend-extra.el --- horizontal gradient legends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A gradient legend with legend.direction "horizontal"
;; lays its bar out left to right, gradientLength long (default 200)
;; and gradientThickness tall, under its title, with labels below the
;; bar: the first aligned left, the last right, the rest centred, as
;; Vega's legend guide does.  The text target keeps vertical legends.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(declare-function eas-legend--title "eas-legend")

(defun eas-legend-extra-horizontal-p (legend metrics)
  "Non-nil when LEGEND is a horizontal gradient under svg METRICS."
  (and (equal (plist-get legend :type) "gradient") (equal (plist-get legend :direction) "horizontal")
       (not (eas-layout-text-p metrics))))

(defun eas-legend-extra-place-horizontal (legend x y metrics)
  "Horizontal gradient LEGEND with its top-left at X Y."
  (let* ((fs (plist-get metrics :legend-label-size))
         (title (eas-legend--title legend x y metrics))
         (by (cdr title)) (thick (plist-get metrics :gradient-thickness))
         (glen (or (plist-get legend :gradient-length) 200))
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
      (setq box (eas-layout-union box (eas-layout-text-bounds metrics (plist-get legend :title)
                                                                  (plist-get metrics :legend-title-size) x y "left" "top"
                                                                  0 (plist-get metrics :legend-title-weight)))))
    (append (eas--plist-without legend :entries)
            (list :x x :y y :width (ceiling (- (aref box 2) x)) :font-size fs
                  :bar (vector x by glen thick)
                  :box (vector x y (+ x (ceiling (- (aref box 2) x))) (+ y (ceiling (- (aref box 3) y))))
                  :entries (vconcat entries))
            (when (car title) (list :title-mark (car title))))))

(provide 'eas-legend-extra)
;;; eas-legend-extra.el ends here

;;; eas-legend-row.el --- symbol legends laid out in a row -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A legend with orient "top" or "bottom" is horizontal in
;; Vega: its symbol entries sit in one row, each its own width (rounded
;; up) plus columnPadding (10) after the last, and its title sits above
;; the row, or left of it (titleOrient "left", titlePadding 5 before
;; the entries, centred on the row).  The shared legends of a
;; composition (eas-compile-shared.el) are placed this way when they
;; are oriented top or bottom; each entry keeps the look and hot spot
;; the vertical layout gives it.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-legend)

(defun eas-legend-row-p (legend metrics)
  "Non-nil when LEGEND is a symbol legend to lay out in a row under METRICS."
  (and (eq (plist-get legend :row) t) (not (eas-layout-text-p metrics))
       (equal (plist-get legend :type) "symbol")))

(defun eas-legend-row-orient (legend metrics)
  "Return where a LEGEND sits against a composition under METRICS.
That is \"top\" or \"bottom\" for a legend placed above or below it
\(svg only), else nil."
  (and (not (eas-layout-text-p metrics)) (car (member (plist-get legend :orient) '("top" "bottom")))))

(defun eas-legend-row-place (legend x y metrics)
  "Symbol LEGEND in one row with its top-left at X Y.
METRICS are the layout's."
  (let* ((vertical (eas-legend--place-symbols (eas--plist-without legend :title) 0 0 metrics))
         (entries (append (plist-get vertical :entries) nil))
         (row-h (apply #'max 0 (mapcar (lambda (e) (aref (plist-get e :bounds) 3)) entries)))
         (title (plist-get legend :title))
         (tsize (plist-get metrics :legend-title-size))
         (left (and title (equal (plist-get legend :title-orient) "left")))
         (tw (if title (ceiling (eas-layout-text-width metrics title tsize (plist-get metrics :legend-title-weight))) 0))
         (ex (if left (+ x tw (or (plist-get legend :title-padding) 5)) x))
         (ey (if (and title (not left)) (+ y tsize (plist-get metrics :legend-title-pad)) y))
         (pad (or (plist-get legend :column-padding) 10))
         (cy (+ ey (/ row-h 2.0)))
         (cursor ex)
         (placed (mapcar (lambda (e)
                           (let* ((b (plist-get e :bounds)) (dx (- cursor (aref b 0))) (dy (- cy (plist-get e :sy))))
                             (prog1 (append (list :sx (+ (plist-get e :sx) dx) :sy cy
                                                  :lx (+ (plist-get e :lx) dx) :ly (+ (plist-get e :ly) dy)
                                                  :bounds (vector cursor ey (aref b 2) row-h))
                                            (when (plist-get e :clip)
                                              (let ((c (plist-get e :clip)))
                                                (list :clip (vector (+ (aref c 0) dx) (+ (aref c 1) dy) (aref c 2) (aref c 3)))))
                                            (cl-loop for (k v) on e by #'cddr
                                                     unless (memq k '(:sx :sy :lx :ly :bounds :clip)) append (list k v)))
                               (setq cursor (+ cursor (ceiling (aref b 2)) pad)))))
                         entries))
         (right (max (+ x tw) (- cursor pad)))
         (bottom (max (+ ey row-h) (if (and title left) (+ y tsize) y))))
    (append (cl-loop for (k v) on vertical by #'cddr
                     unless (memq k '(:entries :title-mark :title :x :y :width :box)) append (list k v))
            (list :x x :y y :width (ceiling (- right x))
                  :box (vector x y (ceiling right) (ceiling bottom))
                  :entries (vconcat placed))
            (when title
              ;; The renderer sets legend titles top-aligned: lift a centred one.
              (list :title (plist-get legend :title)
                    :title-mark (list :text title :x x :y (if left (- cy (* 0.49 tsize)) y)
                                      :align "left" :baseline "top"))))))

(provide 'eas-legend-row)
;;; eas-legend-row.el ends here

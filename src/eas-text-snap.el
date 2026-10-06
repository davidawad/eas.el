;;; eas-text-snap.el --- y bands on whole character rows -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, text target.  A discrete y scale whose step is not a
;; whole number of character rows puts its bands on uneven rows: one
;; bar takes two rows, the next one, a blank row falls between them and
;; the labels drift off their bars.  `eas-text-snap-bands' shortens such
;; a scale's range so the step is the largest whole number of rows
;; that fits, keeping the paddings, and gives the rows left over to the
;; top of the plot, away from the x axis.  A step under one row is left
;; alone: it cannot be made whole.

;;; Code:

(require 'eas-core)
(require 'eas-scale)

(defun eas-text-snap--band (scale ch)
  "SCALE (band or point, ranged) with its step snapped to rows of CH."
  (let* ((step (plist-get scale :step))
         (whole (* ch (floor (+ 1e-9 (/ step (float ch)))))))
    (if (or (null step) (< step ch) (< (abs (- step whole)) 1e-6))
        scale
      (let* ((range (plist-get scale :range))
             (r0 (aref range 0)) (r1 (aref range 1))
             (len (abs (- r1 r0)))
             (n (length (plist-get scale :domain)))
             (point (equal (plist-get scale :type) "point"))
             (inner (if point 1.0 (- 1 (/ (plist-get scale :bandwidth) step))))
             (outer (/ (- (/ len step) (- n inner)) 2.0))
             (new-len (* len (/ whole (float step))))
             ;; A hair down: a band centred on a row boundary (an even
             ;; number of rows) labels its lower middle row, never by luck.
             (hi (+ (max r0 r1) 0.01))
             (new (if (< r0 r1) (vector (- hi new-len) hi) (vector hi (- hi new-len)))))
        (append (eas-scale-band (plist-get scale :type) (plist-get scale :domain) new inner outer)
                (list :field (plist-get scale :field)))))))

(defun eas-text-snap-bands (group metrics)
  "Snap GROUP's discrete y scale to whole rows of the text METRICS."
  (let* ((scales (plist-get group :scales)) (y (plist-get scales :y)))
    (when (and y (member (plist-get y :type) '("band" "point")))
      (plist-put group :scales (plist-put scales :y (eas-text-snap--band y (plist-get metrics :row)))))))

(provide 'eas-text-snap)
;;; eas-text-snap.el ends here

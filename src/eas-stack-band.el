;;; eas-stack-band.el --- bandPosition along a stacked span -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A stacked point-like mark (text, point, tick) sits at
;; the end of its stack segment, as Vega-Lite places it.  The
;; channel's bandPosition slides it along the segment instead: 0 is its
;; start, 0.5 the middle (Vega-Lite's way of centring labels in a
;; stacked bar), 1 the end.  Bars, rects and areas span the segment
;; and ignore it.

;;; Code:

(require 'eas-core)
(require 'eas-scale)

(defun eas-stack-band-pos (unit scales channel row p)
  "P, CHANNEL's pixel position for ROW in UNIT, slid by bandPosition.
SCALES are the view's scales.  Only stacked channels of marks other
than bar, rect and area move."
  (let* ((def (plist-get (plist-get unit :encoding) channel))
         (band (and (eas-object-p def) (plist-get def :bandPosition)))
         (start (and (numberp band) (numberp p) (plist-get def :stack-start)
                     (not (member (plist-get (plist-get unit :mark) :type) '("bar" "rect" "area")))
                     (plist-get row (eas-key (plist-get def :stack-start)))))
         (s (and (numberp start) (eas-scale-apply (plist-get scales channel) start))))
    (if (numberp s) (+ s (* band (- p s))) p)))

(provide 'eas-stack-band)
;;; eas-stack-band.el ends here

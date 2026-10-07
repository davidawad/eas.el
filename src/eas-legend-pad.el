;;; eas-legend-pad.el --- legend padding -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, for eas-legend.el.  A legend's padding (its own, else
;; config.legend's; Vega's default 0) is the room between the legend
;; group's edge and its content, as Vega's legend layout leaves it: the
;; title and entries start padding right of and below the legend's
;; corner, and the legend's box, which the chart sizes itself by, grows
;; by padding on every side.  The text target has no padding.

;;; Code:

(require 'eas-core)
(require 'eas-theme)
(require 'eas-layout)

(defun eas-legend-pad (legend metrics)
  "LEGEND's padding in pixels under METRICS: 0 in text or when unset."
  (if (eas-layout-text-p metrics) 0
    (let ((own (plist-get (plist-get legend :props) :padding)))
      (cond ((numberp own) own)
            ((numberp (eas-theme-get (plist-get metrics :config) :legend :padding))
             (eas-theme-get (plist-get metrics :config) :legend :padding))
            (t 0)))))

(defun eas-legend-pad-box (placed pad)
  "PLACED legend (content placed PAD pixels in) with its :box grown by PAD."
  (let ((b (plist-get placed :box)))
    (if (not (and (vectorp b) (> pad 0))) placed
      (eas-plist-put placed :box (vector (- (aref b 0) pad) (- (aref b 1) pad)
                                         (+ (aref b 2) pad) (+ (aref b 3) pad))))))

(provide 'eas-legend-pad)
;;; eas-legend-pad.el ends here

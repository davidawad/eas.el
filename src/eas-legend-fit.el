;;; eas-legend-fit.el --- legends cut to the height a fitted chart has -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 layout.  At its own size a chart's canvas grows to hold a
;; long legend, as Vega's does.  Fitted to a window, it cannot: a
;; symbol legend taller than the room beside its plot would spill off
;; the canvas and squeeze the plot.  `eas-legend-fit' keeps the first
;; entries that fit and says so in the title ("series, 9 of 14"), so
;; the cut is visible to people and to agents reading the scene
;; (:truncated holds the full count).

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-legend)

(defun eas-legend-fit--height (legend metrics)
  "Height LEGEND takes when placed under METRICS."
  (if (eas-layout-text-p metrics)
      (cdr (eas-legend-size legend metrics))
    (let ((b (plist-get (eas-legend-place legend 0 0 metrics) :box)))
      (- (aref b 3) (aref b 1)))))

(defun eas-legend-fit--cut (legend k)
  "LEGEND keeping its first K entries, titled with the count kept."
  (let ((n (length (plist-get legend :entries))) (title (plist-get legend :title)))
    (thread-first legend
                  (eas-plist-put :entries (seq-take (plist-get legend :entries) k))
                  (eas-plist-put :title (format "%s%d of %d" (if title (concat title ", ") "") k n))
                  (eas-plist-put :truncated n))))

(defun eas-legend-fit (legend max-h metrics)
  "LEGEND cut to the entries that fit in MAX-H pixels (nil: no limit).
Only symbol legends are cut; at least one entry always stays."
  (if (or (null max-h) (not (equal (plist-get legend :type) "symbol"))
          (<= (eas-legend-fit--height legend metrics) max-h))
      legend
    (let ((n (length (plist-get legend :entries))))
      (or (cl-loop for k downfrom (1- n) to 1
                   for cut = (eas-legend-fit--cut legend k)
                   when (<= (eas-legend-fit--height cut metrics) max-h) return cut)
          (eas-legend-fit--cut legend 1)))))

(provide 'eas-legend-fit)
;;; eas-legend-fit.el ends here

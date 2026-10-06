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
;;
;; A terminal canvas cannot grow wider than its window either:
;; `eas-legend-fit-width' ends a legend label or title that would run
;; past the canvas's last column with an ellipsis (:full keeps the
;; whole text), rather than let the text be moved back over its swatch
;; or cut by the window's truncation glyph (fc-qx1.52).

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

(defun eas-legend-fit--clip (mark xk tk cols cw)
  "MARK with its text at TK, anchored left at pixel XK, cut to COLS cells."
  (let* ((text (plist-get mark tk)) (x (plist-get mark xk))
         (room (and (stringp text) (numberp x) (- cols (round (/ x (float cw)))))))
    (if (and room (> (string-width text) room))
        ;; Text wholly past the edge is left out, not moved back in.
        (eas-plist-put (eas-plist-put mark tk (if (< room 1) "" (truncate-string-to-width text room nil nil "…")))
                       :full text)
      mark)))

(defun eas-legend-fit-width (view width metrics)
  "VIEW with its legends' labels and titles cut to a text canvas WIDTH
pixels wide under METRICS; VIEW itself when METRICS is not text."
  (if (not (and (eas-layout-text-p metrics) (plist-get view :legends)))
      view
    (let ((cw (aref (plist-get metrics :cell) 0)))
      (eas-plist-put
       view :legends
       (vconcat
        (mapcar (lambda (legend)
                  (let* ((cols (round (/ (float width) cw)))
                         (legend (eas-plist-put legend :entries
                                                (vconcat (mapcar (lambda (e) (eas-legend-fit--clip e :lx :label cols cw))
                                                                 (plist-get legend :entries))))))
                    (if-let* ((tm (plist-get legend :title-mark)))
                        (eas-plist-put legend :title-mark (eas-legend-fit--clip tm :x :text cols cw))
                      legend)))
                (plist-get view :legends)))))))

(provide 'eas-legend-fit)
;;; eas-legend-fit.el ends here

;;; eas-marks-props.el --- mark properties of text, line and rule marks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, for eas-marks.el (fc-qx1.43).  Vega-Lite mark properties
;; the item builders read through these helpers:
;;
;;   line, rule   the size channel, else the mark's size, else its
;;                strokeWidth, is the stroke width (Vega-Lite encodes
;;                size as strokeWidth for these marks, after the rest)
;;   text         fill (channel, then color, then mark fill, then mark
;;                color), font, fontStyle, angle, lineHeight, and limit
;;                with its ellipsis, cut as Vega truncates
;;   bar          strokeWidth and strokeDash
;;   rule, tick   strokeCap
;;
;; Fonts other than the theme's are drawn by name; layout still measures
;; them with the oracle's Arial metrics.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(declare-function eas-marks--channel "eas-marks")
(declare-function eas-marks--mark-value "eas-marks")

(defun eas-marks-props-stroke-width (unit scales row fallback)
  "Return the stroke width of ROW in line or rule UNIT under SCALES.
That is the size channel, the mark's size, then its strokeWidth, else
FALLBACK."
  (let ((mark (plist-get unit :mark))
        (size (and (plist-get (plist-get unit :encoding) :size) (eas-marks--channel unit scales :size row))))
    (cond ((numberp size) size)
          ((numberp (plist-get mark :size)) (plist-get mark :size))
          ((numberp (plist-get mark :strokeWidth)) (plist-get mark :strokeWidth))
          (t fallback))))

(defun eas-marks-props-text-fill (unit scales row)
  "Fill of text ROW in UNIT: fill or color channel, then mark fill or color.
SCALES map the channels."
  (let ((mark (plist-get unit :mark)))
    (or (eas-marks--channel unit scales :fill row) (eas-marks--channel unit scales :color row)
        (let ((f (plist-get mark :fill))) (and (stringp f) f))
        (plist-get mark :color) "black")))

(defun eas-marks-props-truncate (metrics text size limit ellipsis &optional font)
  "TEXT cut to LIMIT px at font SIZE, ending in ELLIPSIS (default \"…\").
METRICS measures TEXT.  A monospace FONT measures 0.6 em a character,
as its canvas does."

  (let ((eas-font-family font))
    (if (or (not (numberp limit)) (<= limit 0) (string-search "\n" text)
            (<= (eas-layout-text-width metrics text size) limit))
        text
      (let* ((ell (if (stringp ellipsis) ellipsis "…"))
             (room (- limit (eas-layout-text-width metrics ell size))) (n (length text)))
        (while (and (> n 0) (> (eas-layout-text-width metrics (substring text 0 n) size) room))
          (setq n (1- n)))
        (concat (substring text 0 n) ell)))))

(defun eas-marks-props-text (unit row metrics text size)
  "Item properties of text ROW in UNIT beyond position and fill.
TEXT is its string at font SIZE under METRICS; returns (:text T ...)."
  (let ((mark (plist-get unit :mark)) (svg (not (eas-layout-text-p metrics))))
    (append
     (list :text (if svg (eas-marks-props-truncate metrics text size (eas-marks--mark-value unit :limit row)
                                                   (plist-get mark :ellipsis) (plist-get mark :font))
                   text))
     (when svg
       (append
        (let ((f (plist-get mark :font))) (when (stringp f) (list :font f)))
        (let ((s (plist-get mark :fontStyle))) (when (stringp s) (list :fontStyle s)))
        (let ((a (eas-marks--mark-value unit :angle row))) (when (and (numberp a) (/= a 0)) (list :angle a)))
        (let ((h (plist-get mark :lineHeight))) (when (numberp h) (list :lineHeight h))))))))

(defun eas-marks-props-bar (unit)
  "Stroke properties of a bar UNIT's items from its mark: strokeWidth, strokeDash."
  (let ((mark (plist-get unit :mark)))
    (append (when (numberp (plist-get mark :strokeWidth)) (list :strokeWidth (plist-get mark :strokeWidth)))
            (when (vectorp (plist-get mark :strokeDash)) (list :strokeDash (plist-get mark :strokeDash))))))

(defun eas-marks-props-rule (unit)
  "Stroke properties of a rule or tick UNIT's items from its mark: strokeCap."
  (let ((cap (plist-get (plist-get unit :mark) :strokeCap)))
    (when (member cap '("butt" "round" "square")) (list :strokeCap cap))))

(provide 'eas-marks-props)
;;; eas-marks-props.el ends here

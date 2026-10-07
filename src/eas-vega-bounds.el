;;; eas-vega-bounds.el --- autosize none and mitered stroke bounds -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, beside eas-marks-bounds.el.  Two rules by which Vega
;; sizes its canvas around the marks:
;;
;; - Autosize "none" keeps the canvas at the spec's width and height
;;   plus padding; marks that overhang it are cut, not made room for.
;; - vega-scenegraph 5 bounds a stroked line or path by half its stroke
;;   width, or, with a miter join (the default), by miterLimit / 2
;;   widths, which is two widths at the default limit 4.  eas widens
;;   other marks by the full width, so a mitered line widens by two.

;;; Code:

(require 'eas-core)

(defun eas-vega-bounds-autosize-none-p (spec)
  "Non-nil when SPEC's autosize type is \"none\"."
  (let ((a (plist-get spec :autosize)))
    (equal (if (eas-object-p a) (plist-get a :type) a) "none")))

(defun eas-vega-bounds-line (box item)
  "BOX, the points' bounds of line ITEM, widened by its stroke.
A miter join (the default) widens it by twice the stroke width, any
other join by the width."
  (let ((stroke (plist-get item :stroke)) (join (plist-get item :strokeJoin)))
    (if (and box (stringp stroke) (not (equal stroke "none")))
        (let ((sw (* (or (plist-get item :strokeWidth) 1)
                     (if (or (null join) (equal join "miter")) 2 1))))
          (vector (- (aref box 0) sw) (- (aref box 1) sw) (+ (aref box 2) sw) (+ (aref box 3) sw)))
      box)))

(provide 'eas-vega-bounds)
;;; eas-vega-bounds.el ends here

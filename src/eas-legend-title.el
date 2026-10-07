;;; eas-legend-title.el --- multi-line legend titles -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, around eas-legend.el.  Vega-Lite's title is Text, a
;; string or an array of strings, one line each.  A legend's array
;; title becomes one string with its lines joined by newlines, which
;; the layout measures line by line (`eas-layout-text-width',
;; `eas-layout-text-extra-height') and the SVG renderer draws as
;; tspans.  The character grid keeps one line: there the lines are
;; joined by spaces.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(defun eas-legend-title-text (title metrics)
  "TITLE (a string or a vector of strings) as one string, or nil.
Lines join with newlines, or with spaces on the text target of METRICS."
  (cond ((stringp title) title)
        ((and (vectorp title) (> (length title) 0) (seq-every-p #'stringp title))
         (mapconcat #'identity title (if (eas-layout-text-p metrics) " " "\n")))))

(defun eas-legend-title-extra (legend metrics)
  "Height LEGEND's title lines take beyond its first, under METRICS."
  (let ((title (plist-get legend :title)))
    (if (and (stringp title) (not (eas-layout-text-p metrics)))
        (eas-layout-text-extra-height metrics title (plist-get metrics :legend-title-size))
      0)))

(provide 'eas-legend-title)
;;; eas-legend-title.el ends here

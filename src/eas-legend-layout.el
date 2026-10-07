;;; eas-legend-layout.el --- config.legend.layout anchors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, around eas-legend-orient.el.  Vega-Lite 6.4.1 passes
;; config.legend.layout (an ExprRef) through to Vega's legend layout:
;; an object such as {anchor: 'middle'}, or one per orient such as
;; {bottom: {anchor: 'end'}}.  Vega lines the legends of a top or bottom
;; orient up in a row and anchors that row at the start, middle or end of
;; the view's width; those of a left orient stack in a column anchored
;; along its height.  `eas-legend-layout-shift' gives the offset that
;; anchor puts before the first legend.  Only anchor is read.

;;; Code:

(require 'eas-core)
(require 'eas-theme)
(require 'eas-expr)

(defun eas-legend-layout--object (config)
  "CONFIG's config.legend.layout as a plist, or nil.
An ExprRef {expr: ...} is evaluated; a plain object is taken as is."
  (let ((layout (eas-theme-get config :legend :layout)))
    (cond ((and (eas-object-p layout) (stringp (plist-get layout :expr)))
           (let ((v (condition-case nil (eas-expr-evaluate (plist-get layout :expr) nil)
                      (error nil))))
             (and (eas-object-p v) v)))
          ((eas-object-p layout) layout))))

(defun eas-legend-layout-anchor (side config)
  "The anchor (\"start\", \"middle\" or \"end\") of SIDE's legends under CONFIG.
SIDE is an orient string; its own layout block wins over the common one."
  (let* ((layout (eas-legend-layout--object config))
         (own (plist-get layout (eas-key side)))
         (anchor (or (and (eas-object-p own) (plist-get own :anchor)) (plist-get layout :anchor))))
    (if (member anchor '("middle" "end")) anchor "start")))

(defun eas-legend-layout-shift (side extent span config)
  "Offset of SIDE's legends that take EXTENT pixels along a view SPAN long.
CONFIG gives the anchor; the start anchor and a row longer than SPAN
shift nothing."
  (let ((free (- span extent)))
    (pcase (eas-legend-layout-anchor side config)
      ((guard (<= free 0)) 0)
      ("middle" (/ free 2.0))
      ("end" free)
      (_ 0))))

(provide 'eas-legend-layout)
;;; eas-legend-layout.el ends here

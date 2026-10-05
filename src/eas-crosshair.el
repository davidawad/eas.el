;;; eas-crosshair.el --- crosshair readout: the snapped datum's tooltip fields -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.2).  A crosshair is Vega-Lite's own idiom, with
;; no engine API of its own:
;;
;;   {"params": [{"name": "crosshair", "select": {"type": "point",
;;     "on": "pointermove", "nearest": true, "encodings": ["x"]}}], ...}
;;   {"transform": [{"filter": {"param": "crosshair", "empty": false}}],
;;    "mark": "rule", ...}
;;
;; The reducer snaps the pointer to the nearest x through the hit index
;; and fills the selection; compile-patch rebuilds only the rule.  This
;; file answers the readout: every field of encoding.tooltip for the
;; snapped datum, in encoding order.  The hovered mark may carry no
;; tooltip (the rule, or a point layer holding the param), so another
;; mark of the same view at the same x supplies it; with no tooltip
;; anywhere the readout is the row's own fields.  Pure: scene, plan and
;; hover in, data out.

;;; Code:

(require 'eas-core)
(require 'eas-hit)
(require 'eas-params)
(require 'eas-tip)
(require 'eas-view)

(defconst eas-crosshair-snap 0.5
  "Pixels two marks' data may differ in x and still be one crosshair position.")

(defun eas-crosshair--row-readout (row)
  "ROW's own fields as readout pairs, without the row identity."
  (cl-loop for (k v) on (eas--plist-without row eas-params-row-key) by #'cddr
           collect (list :title (eas-key-name k) :value (format "%s" v))))

(defun eas-crosshair--sibling (scene plan hit)
  "Tooltip of another mark in HIT's view whose datum sits at HIT's x.
SCENE was compiled from PLAN."
  (when-let* ((view (seq-find (lambda (v) (equal (plist-get v :id) (plist-get hit :view)))
                              (plist-get scene :views))))
    (seq-some
     (lambda (mark)
       (unless (or (plist-get mark :interactive-off) (equal (plist-get mark :id) (plist-get hit :mark)))
         (when-let* ((c (eas-hit-mark mark (plist-get hit :x) (plist-get hit :y) t))
                     ((<= (plist-get c :distance) eas-crosshair-snap)))
           (eas-tip-tooltip scene plan
                              (append (list :view (plist-get hit :view)) c
                                      (list :row (aref (plist-get mark :rows) (plist-get c :datum))))))))
     (plist-get view :marks))))

(defun eas-crosshair-readout (scene plan hit)
  "Readout of HIT (an `eas-hit' result) as [(:title T :value V) ...], or nil.
SCENE was compiled from PLAN.  The fields are HIT's encoding.tooltip,
else a sibling mark's at the same x, else HIT's row."
  (when hit
    (vconcat (or (eas-tip-tooltip scene plan hit)
                 (eas-crosshair--sibling scene plan hit)
                 (eas-crosshair--row-readout (plist-get hit :row))))))

(defun eas-crosshair-view-readout (view)
  "Readout of live VIEW's hovered datum, or nil when nothing is hovered."
  (let ((view (eas-view-get view)))
    (eas-crosshair-readout (eas-view-scene view) (eas-view-plan view)
                             (plist-get (eas-view-state view) :hover))))

(defun eas-crosshair-format (readout)
  "READOUT pairs on one line: \"title=value\", two spaces apart."
  (mapconcat (lambda (p) (format "%s=%s" (plist-get p :title) (plist-get p :value))) readout "  "))

(provide 'eas-crosshair)
;;; eas-crosshair.el ends here

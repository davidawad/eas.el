;;; eas-demo-crosshair.el --- demo: crosshair -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A nearest point selection on pointermove plus rules filtered by it:
;; the rule and point snap to the datum nearest the pointer, and the
;; values strip under the plot reads it.  Financial: the line template's
;; crosshair slot over a price history.  Non-financial: the multi
;; template over weekly KPIs, with the same crosshair layered on.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-crosshair--multi (rows title)
  "The multi template over ROWS, layered with a nearest-x crosshair rule."
  (let ((spec (eas-demo-spec "multi" (list :data rows :title title))))
    (eas-demo-spec "multi" (list :data rows :title title)
                   :mark nil :encoding nil :params nil :name nil
                   :layer (vector (list :name "series" :params (plist-get spec :params)
                                        :mark (plist-get spec :mark) :encoding (plist-get spec :encoding))
                                  '(:params [(:name "hair" :select (:type "point" :on "pointermove" :nearest t
                                                                          :encodings ["x"]))]
                                    :mark (:type "rule" :opacity 0)
                                    :encoding (:x (:field "x" :type "quantitative")))
                                  '(:transform [(:filter (:param "hair" :empty :false))]
                                    :mark (:type "rule" :color "gray")
                                    :encoding (:x (:field "x" :type "quantitative")))))))

(defun eas-demo-crosshair--sweep (view)
  "Steps sweeping the pointer across VIEW's plot and resting."
  (cl-loop for fx in '(0.1 0.35 0.6 0.85)
           append (list (list :glide (eas-demo-at view fx 0.4) :frames 8) '(:hold 8))))

(eas-demo-run
 "crosshair"
 (list (list :title "ACME close (line template, crosshair slot)"
             :source "line" :bindings (list :data (eas-demo-prices) :y "close" :crosshair t :title "ACME close")
             :steps #'eas-demo-crosshair--sweep)
       (list :title "Weekly KPIs (multi template)"
             :source (eas-demo-crosshair--multi (eas-demo-xy (eas-demo-kpis) :week :value :kpi) "Weekly KPIs")
             :steps #'eas-demo-crosshair--sweep)))

;;; eas-demo-crosshair.el ends here

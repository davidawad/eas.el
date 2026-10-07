;;; eas-demo-legend.el --- demo: legend toggle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A point selection bound to the legend (`bind: "legend"'): clicking an
;; entry keeps that series opaque and fades the others; clicking it
;; again clears the selection.  Financial: the multi template over three
;; tickers.  Non-financial: the same template over weekly KPIs.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-legend--spec (rows title x-type)
  "The multi template over ROWS (x of X-TYPE) with a legend-bound selection."
  (eas-demo-spec "multi" (list :data rows :title title :x_type x-type)
                 :params (lambda (old)
                           (vconcat old (vector '(:name "pick" :select (:type "point" :fields ["series"])
                                                        :bind "legend"))))
                 :encoding (lambda (enc)
                             (plist-put (copy-sequence enc) :opacity
                                        '(:condition (:param "pick" :value 1) :value 0.15)))))

(defun eas-demo-legend--steps (labels)
  "Steps clicking each legend entry of LABELS, then the last again."
  (lambda (view)
    (append (cl-loop for l in labels append (eas-demo-click-steps (eas-demo-legend-px view l) 10))
            (eas-demo-click-steps (eas-demo-legend-px view (car (last labels))) 10))))

(eas-demo-run
 "legend"
 (list (list :title "Three tickers (multi template)"
             :source (eas-demo-legend--spec (eas-demo-xy (eas-demo-tickers) :date :close :symbol) "Closes" "temporal")
             :steps (eas-demo-legend--steps '("GLOBX" "INIT")))
       (list :title "Weekly KPIs (multi template)"
             :source (eas-demo-legend--spec (eas-demo-xy (eas-demo-kpis) :week :value :kpi) "Weekly KPIs" "quantitative")
             :steps (eas-demo-legend--steps '("active" "tickets")))))

;;; eas-demo-legend.el ends here

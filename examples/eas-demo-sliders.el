;;; eas-demo-sliders.el --- demo: sliders and params -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Variable params bound to input widgets (`bind: {input: range}'): the
;; widget row under the chart shows each value, and a param event (what
;; dragging the slider sends) recomputes the chart.  Financial: the
;; multi template's normalize param, bound to a slider, rebases three
;; tickers.  Non-financial: the air-traffic template's month slider over
;; U.S. air travel (its example data).
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-sliders--multi ()
  "The multi template with its normalize param bound to a range slider."
  (eas-demo-spec "multi" (list :data (eas-demo-xy (eas-demo-tickers) :date :close :symbol)
                               :x_type "temporal" :normalize 0 :title "Closes, rebased")
                 :params [(:name "normalize" :value 0
                                 :bind (:input "range" :min 0 :max 200 :step 50 :name "rebase to "))]))

(defun eas-demo-sliders--steps (param values)
  "Steps setting PARAM to each of VALUES, holding after each."
  (cl-loop for v in values
           append (list (list :type "param" :param param :value v) '(:hold 8))))

(eas-demo-run
 "sliders"
 (list (list :title "Three tickers, rebased (multi template)"
             :source (eas-demo-sliders--multi)
             :steps (cons '(:hold 6) (eas-demo-sliders--steps "normalize" '(50 100 150 200 0))))
       (list :title "U.S. air travel by month (air-traffic template)"
             :source "air-traffic" :bindings (eas-template-example "air-traffic")
             :steps (eas-demo-sliders--steps "month" '(6 12 18 24 30 36 42 48)))))

;;; eas-demo-sliders.el ends here

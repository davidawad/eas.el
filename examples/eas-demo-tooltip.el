;;; eas-demo-tooltip.el --- demo: hover tooltips -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The pointer rests on data; the tooltip of the datum under it shows
;; (help-echo in a GUI frame, the echo area in a terminal) and
;; inspect's hover names it.  Financial: the area template over a
;; price history.  Non-financial: the bars template over average steps
;; per weekday.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(eas-demo-run
 "tooltip"
 (list (list :title "ACME close (area template)"
             :source "area"
             :bindings (list :data (eas-demo-xy (eas-demo-prices) :date :close) :x_type "temporal"
                             :x_title "date" :y_title "close" :title "ACME close")
             :steps (lambda (view)
                      (cl-loop for fx in '(0.15 0.4 0.62 0.9)
                               for px = (eas-demo-item-px view "line" "x" (eas-demo--date (round (* fx 59))))
                               when px append (list (list :glide px :frames 8) '(:hold 10)))))
       (list :title "Average steps per weekday (bars template)"
             :source "bars" :bindings (list :data (eas-demo-weekly-steps) :title "Steps per weekday")
             :steps '((:visit (:mark "bar" :key "category" :values ["Tue" "Thu" "Sat" "Sun"])
                       :glide 8 :hold 10)))))

;;; eas-demo-tooltip.el ends here

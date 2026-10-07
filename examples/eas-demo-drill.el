;;; eas-demo-drill.el --- demo: drill -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The drill action: a click on a bar opens a detail view of the rows
;; behind it, drawn by another template (`eas-register-drill' gives
;; the rows, `eas-action-bind' binds the click).  The click answers
;; the detail view's id, which an agent then inspects; the recording
;; follows into it.  Financial: monthly volume (bars) drills into that
;; month's daily closes (series-line).  Non-financial: weekly steps
;; (bars) drill into that week's days (line).
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-drill--month (row)
  "The month (\"2026-01\") of ROW's date."
  (substring (plist-get row :date) 0 7))

(eas-register-drill "demo-month-closes"
                    :doc "The daily closes of a month bar."
                    :fn (lambda (target _view _args)
                          (eas-demo-xy (seq-filter (lambda (r) (equal (eas-demo-drill--month r)
                                                                      (plist-get (plist-get target :row) :category)))
                                                   (eas-demo-prices))
                                       :date :close)))

(eas-register-drill "demo-week-days"
                    :doc "The days of a week bar."
                    :fn (lambda (target _view _args)
                          (let ((w (string-to-number (substring (plist-get (plist-get target :row) :category) 1))))
                            (seq-subseq (eas-demo-heart) (* 7 (1- w)) (* 7 w)))))

(defun eas-demo-drill--monthly ()
  "Total ACME volume per month."
  (vconcat (cl-loop for m in '("2026-01" "2026-02" "2026-03")
                    collect (list :category m
                                  :value (apply #'+ (cl-loop for r across (eas-demo-prices)
                                                             when (equal (eas-demo-drill--month r) m)
                                                             collect (plist-get r :volume)))))))

(defun eas-demo-drill--weekly ()
  "Total steps per week."
  (vconcat (cl-loop for w from 1 to 8
                    collect (list :category (format "W%d" w)
                                  :value (apply #'+ (mapcar (lambda (r) (plist-get r :steps))
                                                            (seq-subseq (eas-demo-heart) (* 7 (1- w)) (* 7 w))))))))

(defun eas-demo-drill--steps (category)
  "Steps clicking the bar CATEGORY, then following into the detail view."
  (lambda (view)
    (append (eas-demo-click-steps (eas-demo-item-px view "bar" "category" category) 8)
            (list (list :call (lambda (view)
                                (eas-view-get (plist-get (plist-get (eas-inspect view) :click) :result))))
                  '(:hold 16)))))

(eas-demo-run
 "drill"
 (list (list :title "ACME monthly volume (bars), drill to days (series-line)"
             :source "bars" :bindings (list :data (eas-demo-drill--monthly) :title "ACME volume by month")
             :setup (lambda (view)
                      (eas-action-bind view "*" '(:action "drill" :provider "demo-month-closes" :template "series-line"
                                                          :bindings (:x_type "temporal" :title "ACME closes, drilled"))))
             :steps (eas-demo-drill--steps "2026-02"))
       (list :title "Steps per week (bars), drill to days (line)"
             :source "bars" :bindings (list :data (eas-demo-drill--weekly) :title "Steps per week")
             :setup (lambda (view)
                      (eas-action-bind view "*" '(:action "drill" :provider "demo-week-days" :template "line"
                                                          :bindings (:y "steps" :points t :title "Steps, drilled"))))
             :steps (eas-demo-drill--steps "W3"))))

;;; eas-demo-drill.el ends here

;;; eas-demo-click.el --- demo: click actions and callbacks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A click on a datum runs the action bound to it (`eas-action-bind'):
;; here a Lisp callback that receives the clicked row, and the
;; built-in copy-row.  inspect's click records the target and what the
;; action answered.  Financial: the bars template over sector returns.
;; Non-financial: the line template over resting heart rate, points on.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defvar eas-demo-click-log nil "Rows the click callback received.")

(defun eas-demo-click--callback (target _view)
  "Note TARGET's row; the answer lands in inspect's click."
  (push (plist-get target :row) eas-demo-click-log)
  (message "callback: %S" (plist-get target :row))
  (list :noted (length eas-demo-click-log)))

(eas-demo-run
 "click"
 (list (list :title "Sector returns (bars template)"
             :source "bars" :bindings (list :data (eas-demo-sector-returns) :title "YTD return by sector, %")
             :setup (lambda (view) (eas-action-bind view "*" #'eas-demo-click--callback))
             :steps (lambda (view)
                      (append (eas-demo-click-steps (eas-demo-item-px view "bar" "category" "Tech"))
                              (eas-demo-click-steps (eas-demo-item-px view "bar" "category" "Energy")))))
       (list :title "Resting heart rate (line template)"
             :source "line" :bindings (list :data (eas-demo-heart 30) :y "bpm" :points t :title "Resting heart rate")
             :setup (lambda (view) (eas-action-bind view "*" "copy-row"))
             :steps (lambda (view)
                      (append (eas-demo-click-steps (eas-demo-item-px view "point" "date" (eas-demo--day 9)))
                              (eas-demo-click-steps (eas-demo-item-px view "point" "date" (eas-demo--day 21))))))))

;;; eas-demo-click.el ends here

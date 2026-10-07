;;; eas-title-extra-test.el --- title dy and the title's room -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.13: a title's dy moves the plots by -dy, as Vega's autosize
;; grows the canvas by the title group's bounds, the lines shifted by dy
;; included; the lines keep the canvas top.

;;; Code:

(require 'ert)
(require 'eas)

(defconst eas-title-extra-test--spec
  '(:data (:values [(:a 1 :b 2) (:a 2 :b 3)]) :mark "point"
    :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")))
  "A small chart to title.")

(defun eas-title-extra-test--chart (title &rest opts)
  "The test chart titled TITLE, compiled with OPTS."
  (apply #'eas-compile (append (list :title title) eas-title-extra-test--spec) opts))

(ert-deftest eas-title-extra-dy-moves-the-plots ()
  "A negative dy grows the room by -dy and keeps both lines in place."
  (let* ((base '(:text "T" :subtitle "S" :fontSize 18 :subtitlePadding 5 :offset 4))
         (plain (eas-title-extra-test--chart base))
         (up (eas-title-extra-test--chart (append '(:dy -10) base)))
         (down (eas-title-extra-test--chart (append '(:dy 5) base)))
         (h (lambda (s) (plist-get (plist-get s :size) :h)))
         (y (lambda (s) (plist-get (plist-get s :title) :y)))
         (sy (lambda (s) (plist-get (plist-get (plist-get s :title) :subtitle) :y))))
    (should (= (funcall h up) (+ (funcall h plain) 10)))
    (should (= (funcall h down) (- (funcall h plain) 5)))
    (dolist (s (list up down))
      (should (= (funcall y s) (funcall y plain)))
      (should (= (funcall sy s) (funcall sy plain))))))

(ert-deftest eas-title-extra-dy-room-floors-at-zero ()
  "A dy past the room puts the lines over the plots, with no room above."
  (let* ((metrics (eas-layout-metrics 'svg nil eas-theme-default))
         (title '(:text "T" :fontSize 13 :offset 4))
         (h (eas-title-height (list :title title) metrics))
         (deep (eas-title-extra-test--chart (append '(:dy 100) title)))
         (plain (eas-title-extra-test--chart title)))
    (should (= (eas-title-height (list :title (append '(:dy 100) title)) metrics) 0))
    (should (= (plist-get (plist-get deep :title) :y)
               (+ (plist-get (plist-get plain :title) :y) (- 100 h))))))

(ert-deftest eas-title-extra-dy-ignored-on-text ()
  "The text target gives a title with dy the room it gives one without."
  (let ((size '(:cols 40 :rows 16)))
    (should (equal (plist-get (eas-title-extra-test--chart '(:text "T" :dy -10) :target 'text :size size) :size)
                   (plist-get (eas-title-extra-test--chart '(:text "T") :target 'text :size size) :size)))))

(provide 'eas-title-extra-test)
;;; eas-title-extra-test.el ends here

;;; eas-reliability-diagram-test.el --- reliability diagram template -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The generic probability calibration surface: observations are binned,
;; summarized, and rendered with a reference diagonal and optional Brier parts.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defun eas-reliability-diagram-test--scene (&optional target)
  "Compile the reliability diagram example for TARGET."
  (eas-compile (eas-resolve "reliability-diagram"
                            (eas-template-example "reliability-diagram"))
               :target target :size '(:cols 72 :rows 22)))

(ert-deftest eas-reliability-diagram-text-and-svg-goldens ()
  "The example calibration plot has stable text and SVG surfaces."
  (eas-test-golden "reliability-diagram.txt"
                   (eas-text-render (eas-reliability-diagram-test--scene 'text)))
  (eas-test-golden "reliability-diagram.svg"
                   (eas-svg-render (eas-reliability-diagram-test--scene 'svg))))

(ert-deftest eas-reliability-diagram-includes-diagonal-points-and-caption ()
  "The resolved scene contains its reference line, calibration bins and caption."
  (let* ((scene (eas-reliability-diagram-test--scene 'svg))
         (marks (plist-get (aref (plist-get scene :views) 0) :marks)))
    (should (= (length marks) 3))
    (should (equal (plist-get (aref marks 0) :mark) "rule"))
    (should (= (length (plist-get (aref marks 0) :items)) 1))
    (should (> (length (plist-get (aref marks 1) :items)) 0))
    (should (= (length (plist-get (aref marks 2) :items)) 1))))

(ert-deftest eas-reliability-diagram-can-hide-brier-caption ()
  "The caption toggle removes the Brier parts text mark."
  (let* ((bindings (plist-put (eas-template-example "reliability-diagram")
                              :show_brier_parts :false))
         (scene (eas-compile (eas-resolve "reliability-diagram" bindings) :target 'svg))
         (marks (plist-get (aref (plist-get scene :views) 0) :marks)))
    (should (= (length (plist-get (aref marks 2) :items)) 0))))

(provide 'eas-reliability-diagram-test)
;;; eas-reliability-diagram-test.el ends here

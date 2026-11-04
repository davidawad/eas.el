;;; eas-dumbbell-test.el --- dumbbell template -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Tests for the paired-value dumbbell template and its two renderers.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defun eas-dumbbell-test--spec (&optional bindings)
  "Resolve the dumbbell example with BINDINGS when supplied."
  (eas-resolve "dumbbell" (or bindings (eas-template-example "dumbbell"))))

(ert-deftest eas-dumbbell-template-resolves-and-draws-both-surfaces ()
  "The example resolves to native Vega-Lite and draws as SVG and text."
  (let* ((spec (eas-dumbbell-test--spec))
         (scene (eas-compile spec))
         (svg (eas-svg-render scene))
         (text (eas-text-render (eas-compile spec :target 'text :size '(:cols 80 :rows 24)))))
    (should-not (plist-get spec :x-eas))
    (should-not (eas-spec-check spec))
    (should (string-prefix-p "<svg" svg))
    (should (> (length (string-trim text)) 0))
    (should (= (length (plist-get (eas-scene-mark scene "main/0") :items)) 5))
    (should (= (length (plist-get (eas-scene-mark scene "main/1") :items)) 5))
    (should (= (length (plist-get (eas-scene-mark scene "main/2") :items)) 5))))

(ert-deftest eas-dumbbell-template-binds-field-slots-and-sorts-by-gap ()
  "Callers can choose the three fields and gaps determine row order."
  (let* ((rows [(:name "small" :before 0.4 :after 0.5)
                (:name "large" :before 0.1 :after 0.9)])
         (spec (eas-dumbbell-test--spec
                (list :data rows :label "name" :stated "before" :coherent "after")))
         (scene (eas-compile spec))
         (mark (eas-scene-mark scene "main/0"))
         (scale (plist-get (plist-get (aref (plist-get scene :views) 0) :scales) :y)))
    (should-not (eas-spec-check spec))
    (should (equal (plist-get scale :domain) '["large" "small"]))
    (should (< (abs (- (plist-get (aref (plist-get mark :rows) 0) :gap) 0.1)) 1e-9))
    (should (= (plist-get (aref (plist-get mark :rows) 1) :gap) 0.8))))

(ert-deftest eas-dumbbell-template-goldens ()
  "The example text and SVG stay stable."
  (let ((spec (eas-dumbbell-test--spec)))
    (eas-test-golden "text-dumbbell.txt"
                     (eas-text-render (eas-compile spec :target 'text :size '(:cols 80 :rows 24))))
    (eas-test-golden "svg-dumbbell.svg" (eas-svg-render (eas-compile spec)))))

(provide 'eas-dumbbell-test)
;;; eas-dumbbell-test.el ends here

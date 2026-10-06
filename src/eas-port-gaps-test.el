;;; eas-port-gaps-test.el --- gaps found porting a domain package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Regression tests for the gaps the health-charts.el port onto eas
;; found (eas-agt.3), one test per gap.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-text)
(require 'eas-agent)
(require 'eas-agent-cli)

(defun eas-port-gaps-test--marks (scene type)
  "Every mark of TYPE across SCENE's views."
  (cl-loop for v across (vconcat (plist-get scene :views))
           append (cl-loop for m across (vconcat (plist-get v :marks))
                           when (equal (plist-get m :mark) type) collect m)))

(ert-deftest eas-port-gaps-null-channel-cancels-an-inherited-one ()
  ;; A mean rule across a layer: x: null drops the x the layer inherits.
  (let* ((spec '(:data (:values [(:a 1 :b 2) (:a 2 :b 4)])
                 :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative"))
                 :layer [(:mark "line")
                         (:mark "rule" :encoding (:x :null :y (:aggregate "mean" :field "b")))]))
         (rule (car (eas-port-gaps-test--marks (eas-compile spec) "rule")))
         (items (plist-get rule :items)))
    (should (eq (plist-get (eas-agent "check" (eas-json-encode spec)) :ok) t))
    (should (= (length items) 1))
    (let ((item (aref items 0)))
      (should (= (plist-get item :y1) (plist-get item :y2)))
      (should (< (plist-get item :x1) (plist-get item :x2))))))

(ert-deftest eas-port-gaps-json-encode-returns-characters ()
  (let ((json (eas-json-encode '(:title "Pulse ♥ é"))))
    (should (multibyte-string-p json))
    (should (equal json "{\"title\":\"Pulse ♥ é\"}"))
    (should (equal (eas-json-parse json) '(:title "Pulse ♥ é"))))
  ;; The hash is still over the UTF-8 bytes.
  (should (equal (eas-content-hash '(:t "é"))
                 (concat "sha256:" (secure-hash 'sha256 "{\"t\":\"\303\251\"}")))))

(ert-deftest eas-port-gaps-cli-prints-a-non-ascii-title ()
  (let ((spec (eas-json-encode '(:title "Pulse ♥" :data (:values [(:a 1)]) :mark "point"
                                 :encoding (:x (:field "a" :type "quantitative"))))))
    (dolist (raw '(nil ("--raw")))
      (let ((out (cdr (eas-agent-cli-run
                       (append (list "render" spec "--cols" "30" "--rows" "8") raw)))))
        (should (string-match-p "Pulse ♥" out))))))

(provide 'eas-port-gaps-test)
;;; eas-port-gaps-test.el ends here

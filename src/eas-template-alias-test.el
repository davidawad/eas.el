;;; eas-template-alias-test.el --- gallery templates by plain name -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The templates of templates/vega/ register under plain names
;; (treemap), and their old "vega/NAME" names resolve as deprecated
;; aliases through x-eas.aliases (eas-7r1.14).

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-test-support)

(defun eas-template-alias-test--manifest ()
  "The examples of test/vega-examples/manifest.json."
  (append (plist-get (eas-json-read-file (eas-test-file "test/vega-examples/manifest.json"))
                     :examples)
          nil))

(ert-deftest eas-template-alias-gallery-templates-take-plain-names ()
  (let ((names (eas-template-names)))
    (should-not (seq-find (lambda (n) (string-prefix-p "vega/" n)) names))
    (should-not (seq-find (lambda (e) (string-match-p "already defined" (plist-get e :message)))
                          eas-template-load-errors))
    (dolist (example (eas-template-alias-test--manifest))
      (let* ((name (plist-get example :template))
             (template (eas-template-get name)))
        (should (member name names))
        (should (string-suffix-p (concat "templates/vega/" name ".json") (plist-get template :path)))
        ;; The old name still finds it.
        (should (equal (plist-get (eas-template-get (concat "vega/" (plist-get example :name))) :name)
                       name))
        (should (eas-template-p (concat "vega/" (plist-get example :name))))))))

(ert-deftest eas-template-alias-eas-keeps-its-own-names ()
  ;; The gallery's heatmap and histogram yield the plain name.
  (should (equal (eas-test-gallery-template "heatmap") "hourly-heatmap"))
  (should (equal (eas-test-gallery-template "histogram") "rug-histogram"))
  (should (equal (eas-test-gallery-template "treemap") "treemap"))
  (dolist (name '("heatmap" "histogram"))
    (should (string-suffix-p (concat "templates/" name ".json")
                             (plist-get (eas-template-get name) :path))))
  (should (equal (plist-get (eas-template-get "vega/heatmap") :name) "hourly-heatmap"))
  (should (equal (plist-get (eas-template-get "vega/histogram") :name) "rug-histogram"))
  (should-not (eas-template-p "vega/no-such-chart"))
  (eas-test-should-code "NOT_FOUND" (eas-template-get "vega/no-such-chart")))

(ert-deftest eas-template-alias-from-x-eas ()
  (let ((eas--templates (progn (eas-template-names) (copy-sequence eas--templates))))
    (eas-template-register '(:x-eas (:template "alias-new" :version "1" :aliases ["alias-old"]
                                     :slots (:data (:shape "json")))
                             :data (:name "data") :mark "bar"
                             :encoding (:x (:field "a" :type "nominal"))))
    (should (equal (plist-get (eas-template-get "alias-old") :name) "alias-new"))
    (should (eas-template-p "alias-old"))
    (should-not (member "alias-old" (eas-template-names)))
    (should (equal (eas-resolve "alias-old" '(:data [(:a "x")]))
                   (eas-resolve "alias-new" '(:data [(:a "x")]))))))

(provide 'eas-template-alias-test)
;;; eas-template-alias-test.el ends here

;;; eas-core-test.el --- tests for eas-core and eas-time -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas-core)
(require 'eas-time)

(ert-deftest eas-core-json-round-trips-null-false-and-empty ()
  (let ((text "{\"a\":[1,null,false,{}],\"b\":true,\"x-eas:transform\":\"lttb\"}"))
    (should (equal (eas-json-parse text)
                   '(:a [1 :null :false nil] :b t :x-eas:transform "lttb")))
    (should (equal (eas-json-encode (eas-json-parse text)) text))))

(ert-deftest eas-core-parse-error-is-data ()
  (let ((plist (eas-test-should-code "PARSE_ERROR" (eas-json-parse "{"))))
    (should (string-match-p "fix the syntax" (plist-get plist :message)))))

(ert-deftest eas-core-every-code-is-an-eas-error-child ()
  (dolist (entry eas-reason-codes)
    (let ((err (should-error (eas-signal (car entry) "m" :x 1) :type 'eas-error)))
      (should (eq (car err) (nth 1 entry)))
      (should (equal (eas-error-plist err)
                     (list :code (car entry) :message "m" :x 1))))))

(ert-deftest eas-core-foreign-errors-become-engine-failed ()
  (should (equal (plist-get (eas-error-plist '(wrong-type-argument numberp "a")) :code)
                 "ENGINE_FAILED")))

(ert-deftest eas-core-content-hash-is-sha256-of-canonical-json ()
  ;; sha256('{"a":2,"b":1}'), computed independently.
  (should (equal (eas-content-hash '(:b 1 :a 2))
                 "sha256:d3626ac30a87e6f7a6428233b3c68299976865fa5508e4267c5415c76af7a772"))
  (should (equal (eas-content-hash '(:b 1 :a 2)) (eas-content-hash '(:a 2 :b 1)))))

(ert-deftest eas-core-pretty-is-sorted-and-stable ()
  (should (equal (eas-json-pretty '(:z [1 2] :a (:k "v") :m [(:n [1 (:deep 1)])]))
                 "{\n  \"a\": {\"k\": \"v\"},\n  \"m\": [\n    {\n      \"n\": [\n        1,\n        {\"deep\": 1}\n      ]\n    }\n  ],\n  \"z\": [1, 2]\n}\n")))

(ert-deftest eas-time-parse-is-utc-and-honours-offsets ()
  (should (= (eas-time-parse "2026-03-01") 1772323200000))
  (should (= (eas-time-parse "2026-03-01T10:30:00Z") 1772361000000))
  (should (= (eas-time-parse "2026-03-01T10:30:00-05:00") 1772379000000))
  (should (= (eas-time-parse "1969-12-31") -86400000))
  (should (= (eas-time-parse 42) 42))
  (should-not (eas-time-parse "March 1")))

(ert-deftest eas-time-fields-and-ms-round-trip ()
  (let ((ms (eas-time-parse "2024-02-29T23:59:59.5Z")))
    (should (equal (eas-time-fields ms)
                   '(:year 2024 :month 2 :day 29 :hours 23 :minutes 59 :seconds 59
                           :milliseconds 500 :weekday 4)))
    (should (= (eas-time-ms 2024 2 29 23 59 59 500) ms))
    (should (= (eas-time-ms 2026 14 1) (eas-time-parse "2027-02-01")))))

(ert-deftest eas-has-no-references-to-its-host-package ()
  "Extraction guard (fc-qx1.11): src/eas names no host package."
  (let ((needle (concat "financial" "-chart"))
        (dir (eas-test-file "src/eas")))
    (dolist (file (directory-files dir t "\\.el\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (should-not (and (search-forward needle nil t) file))))))

(provide 'eas-core-test)
;;; eas-core-test.el ends here

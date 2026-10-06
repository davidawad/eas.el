;;; eas-data-test.el --- tests for data/v1 adapters -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(ert-deftest eas-data-plist-infers-schema ()
  (should (equal (eas-data-from "plist" '((:d "2026-01-01" :v 1 :s "a") (:d "2026-01-02" :v :null :s "b")))
                 '(:schema [(:name "d" :type "temporal") (:name "v" :type "quantitative")
                            (:name "s" :type "nominal")]
                           :rows [(:d "2026-01-01" :v 1 :s "a") (:d "2026-01-02" :v :null :s "b")]))))

(ert-deftest eas-data-plist-failures-carry-index-and-field ()
  (should (equal (eas-data-check "plist" [(:a 1) 3])
                 '(:code "SHAPE_INVALID"
                         :message "Row 1 is not an object; give each row as {\"field\": value, ...}"
                         :index 1 :field nil)))
  (let ((failure (eas-data-check "plist" [(:a 1) (:a [1 2])])))
    (should (equal (plist-get failure :index) 1))
    (should (equal (plist-get failure :field) "a"))))

(ert-deftest eas-data-unknown-adapter-is-not-found ()
  (eas-test-should-code "NOT_FOUND" (eas-data-from "xlsx" "x")))

(ert-deftest eas-data-json-accepts-rows-values-and-data-v1 ()
  (dolist (text '("[{\"x\":1}]" "{\"values\":[{\"x\":1}]}"
                  "{\"schema\":[{\"name\":\"x\",\"type\":\"quantitative\"}],\"rows\":[{\"x\":1}]}"))
    (should (equal (eas-data-rows (eas-data-from "json" text)) [(:x 1)])))
  (should (equal (plist-get (eas-data-check "json" "{\"a\":1}") :code) "SHAPE_INVALID"))
  (should (equal (plist-get (eas-data-check "json" "[1,") :code) "PARSE_ERROR")))

(ert-deftest eas-data-json-reads-files ()
  (let ((file (make-temp-file "eas" nil ".json" "[{\"x\": 2}]")))
    (unwind-protect
        (progn (should (equal (eas-data-rows (eas-data-from "json" file)) [(:x 2)]))
               (should (equal (eas-data-rows (eas-data-from "json" (list :file file))) [(:x 2)])))
      (delete-file file))))

(ert-deftest eas-data-csv-handles-quotes-and-types ()
  (should (equal (eas-data-from "csv" "a,b,c\n1,\"x,y\",\n-2.5e1,\"he said \"\"hi\"\"\",3\n")
                 '(:schema [(:name "a" :type "quantitative") (:name "b" :type "nominal")
                            (:name "c" :type "quantitative")]
                           :rows [(:a 1 :b "x,y" :c :null) (:a -25.0 :b "he said \"hi\"" :c 3)])))
  (should (equal (eas-data-rows (eas-data-from "tsv" "a\tb\n1\tx y\n")) [(:a 1 :b "x y")]))
  (let ((failure (eas-data-check "csv" "a,b\n1,2\n3\n")))
    (should (equal (plist-get failure :code) "SHAPE_INVALID"))
    (should (equal (plist-get failure :index) 1))))

(ert-deftest eas-data-csv-reads-the-repo-example ()
  (let ((data (eas-data-from "csv" (eas-test-file "test/vl-examples/data/stocks.csv"))))
    (should (> (length (eas-data-rows data)) 10))
    (should (equal (eas-data-field-type data "price") "quantitative"))))

(ert-deftest eas-data-bar-v1-validates-and-lowers ()
  (let ((data (eas-data-from "bar/v1" '((:open 1 :high 2 :low 0.5 :close 1.5 :volume 10 :time 1700000000000)))))
    (should (equal (eas-data-rows data)
                   [(:time 1700000000000 :open 1 :high 2 :low 0.5 :close 1.5 :volume 10)]))
    (should (equal (eas-data-field-type data "time") "temporal")))
  (dolist (case '(((:open 1 :high 2 :low 0 :close "x") 0 "close")
                  ((:open 1 :high 2 :low 0 :close 1 :volume -1) 0 "volume")
                  ((:open 1 :high 0 :low 2 :close 1) 0 "high")))
    (let ((failure (eas-data-check "bar/v1" (list (car case)))))
      (should (equal (list (plist-get failure :code) (plist-get failure :index) (plist-get failure :field))
                     (list "SHAPE_INVALID" (nth 1 case) (nth 2 case)))))))

(ert-deftest eas-data-org-table-babel-named-and-at-point ()
  (should (equal (eas-data-rows (eas-data-from "org-table" '(("x" "y") hline (1 "2") ("a" ""))))
                 [(:x 1 :y 2) (:x "a" :y :null)]))
  (with-temp-buffer
    (insert "#+name: t1\n| a | b |\n|---+---|\n| 1 | q |\n| 2 | r |\n")
    (org-mode)
    (should (equal (eas-data-rows (eas-data-from "org-table" '(:name "t1")))
                   [(:a 1 :b "q") (:a 2 :b "r")]))
    (goto-char (point-max))
    (forward-line -1)
    (should (equal (length (eas-data-rows (eas-data-from "org-table" '(:at-point t)))) 2))
    (eas-test-should-code "NOT_FOUND" (eas-data-from "org-table" '(:name "nope"))))
  (should (equal (plist-get (eas-data-check "org-table" '(("x" "y") (1))) :index) 0)))

(ert-deftest eas-data-append-checks-the-schema ()
  (let ((data (eas-data-from "plist" '((:t "2026-01-01" :v 1)))))
    (should (equal (eas-data-rows (eas-data-append data [(:t "2026-01-02" :v 2)]))
                   [(:t "2026-01-01" :v 1) (:t "2026-01-02" :v 2)]))
    (let ((failure (should-error (eas-data-append data [(:t "2026-01-02" :v 2) (:t 5 :w 1)])
                                 :type 'eas-shape-invalid)))
      (should (equal (plist-get (eas-error-plist failure) :index) 1))
      (should (equal (plist-get (eas-error-plist failure) :field) "w")))
    (should (equal (plist-get (eas-error-plist
                               (should-error (eas-data-append data [(:v "high")])
                                             :type 'eas-shape-invalid))
                              :field)
                   "v"))))

(ert-deftest eas-data-adapters-accept-their-examples ()
  (dolist (entry eas-adapters)
    (should-not (eas-data-check (car entry) (plist-get (cdr entry) :example)))))

(provide 'eas-data-test)
;;; eas-data-test.el ends here

;;; eas-template-each-test.el --- x-eas:each and x-eas:item in templates -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-template-each-test--spec
  '(:x-eas (:template "test-each" :version "1.0.0"
		      :slots (:data (:shape "plist" :required t)
				    :series (:type "array" :default [])
				    :typed (:enum ["quantitative" "temporal"] :default "quantitative")))
	   :data (:name "data")
	   :layer [(:name "base" :mark "point"
			  :encoding (:x (:field "a" :type (:x-eas:slot "typed") :scale (:type "linear"))))
		   (:x-eas:each "series"
				:spec (:mark (:type "line" :color (:x-eas:item "color"))
					     :encoding (:y (:field (:x-eas:item "field" :default (:x-eas:item "."))
								   :type "quantitative"
								   :title (:x-eas:item "title" :default (:x-eas:item "field"
														     :default (:x-eas:item "."))))
							   :x (:field "a" :type "quantitative"))))])
  "A template with one layer per item of its series slot.")

(defmacro eas-template-each-test--with (&rest body)
  "Run BODY with the test template registered in a private registry."
  (declare (indent 0))
  `(let ((eas--templates (progn (eas-template-names) (copy-sequence eas--templates))))
     (eas-template-register eas-template-each-test--spec)
     ,@body))

(defun eas-template-each-test--layers (series)
  "The resolved layers of the test template with SERIES."
  (plist-get (eas-resolve "test-each" (list :data [(:a 1 :b 2 :c 3)] :series series)) :layer))

(ert-deftest eas-template-each-expands-one-spec-per-item ()
  (eas-template-each-test--with
   (let ((layers (eas-template-each-test--layers [])))
     (should (= (length layers) 1))
     (should (equal (plist-get (aref layers 0) :name) "base")))
   (let ((layers (eas-template-each-test--layers ["b" (:field "c" :title "C" :color "red")])))
     (should (= (length layers) 3))
     ;; A string item is "." itself; a missing key with no default drops the key.
     (should (equal (plist-get (aref layers 1) :mark) '(:type "line")))
     (should (equal (plist-get (plist-get (plist-get (aref layers 1) :encoding) :y) :field) "b"))
     (should (equal (plist-get (plist-get (plist-get (aref layers 1) :encoding) :y) :title) "b"))
     (should (equal (plist-get (aref layers 2) :mark) '(:type "line" :color "red")))
     (should (equal (plist-get (plist-get (plist-get (aref layers 2) :encoding) :y) :title) "C"))
     ;; Placeholders never reach the resolved spec.
     (should-not (string-match-p "x-eas" (eas-json-encode layers))))))

(ert-deftest eas-template-each-checks-its-slot-and-scope ()
  (eas-test-should-code "SLOT_TYPE"
			(eas-resolve-spec '(:data (:values [(:a 1)])
						  :layer [(:x-eas:each "n" :spec (:mark "point"))])
					  '(:n 3)))
  (eas-test-should-code "INVALID_INPUT"
			(eas-resolve-spec '(:data (:values [(:a 1)]) :mark (:type "point" :color (:x-eas:item "c"))))))

(ert-deftest eas-template-slot-placeholders-in-type-positions-validate ()
  ;; An encoding or scale type may be a slot: the template registers and
  ;; resolves to the slot's value.
  (eas-template-each-test--with
   (let ((layer (aref (plist-get (eas-resolve "test-each" '(:data [(:a 1)] :typed "temporal")) :layer) 0)))
     (should (equal (plist-get (plist-get (plist-get layer :encoding) :x) :type) "temporal"))))
  (should-not (seq-find (lambda (f) (plist-get f :invalid))
                        (eas-spec-features '(:mark "point" :encoding (:x (:field "a" :type (:x-eas:slot "t")
										 :scale (:type (:x-eas:slot "s")))))))))

(provide 'eas-template-each-test)
;;; eas-template-each-test.el ends here

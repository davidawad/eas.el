;;; eas-spec-test.el --- tests for chart/v1, templates and resolve -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defun eas-spec-test--codes (findings)
  "Return (CODE . PATH) pairs of FINDINGS."
  (mapcar (lambda (f) (cons (plist-get f :code) (plist-get f :path))) findings))

(ert-deftest eas-spec-parse-expands-mark-shorthand ()
  (should (equal (eas-spec-parse "{\"mark\":\"bar\"}") '(:mark (:type "bar"))))
  (should (equal (eas-spec-parse '(:layer [(:mark "line")]))
                 '(:layer [(:mark (:type "line"))]))))

(ert-deftest eas-spec-parse-rejects-non-objects ()
  (eas-test-should-code "INVALID_INPUT" (eas-spec-parse "[1]"))
  (eas-test-should-code "PARSE_ERROR" (eas-spec-parse "{\"mark\":")))

(ert-deftest eas-spec-check-names-each-problem-by-path ()
  (should (equal
           (eas-spec-test--codes
            (eas-spec-check
             "{\"mark\":\"hexbin\",\"facet\":{},\"transform\":[{\"sample\":5}],
               \"encoding\":{\"latitude\":{\"field\":\"a\"},
                             \"x\":{\"field\":\"a\",\"type\":\"bogus\"},
                             \"y\":{\"field\":\"b\",\"scale\":{\"type\":\"symlog\"}}}}"))
           '(("UNSUPPORTED_FEATURE" . "/facet")
             ("UNSUPPORTED_FEATURE" . "/mark")
             ("INVALID_INPUT" . "/encoding/x/type")
             ("UNSUPPORTED_FEATURE" . "/encoding/y/scale/type")
             ("UNSUPPORTED_FEATURE" . "/transform/0")))))

(ert-deftest eas-spec-check-accepts-the-native-vocabulary ()
  (should-not
   (eas-spec-check
    '(:data (:values [(:a 1 :b 2)])
      :params [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")
               (:name "hover" :select (:type "point" :on "pointermove" :nearest t))]
      :transform [(:filter "datum.a > 0") (:calculate "datum.a * 2" :as "c")]
      :layer [(:mark "line" :encoding (:x (:field "a" :type "quantitative")
                                       :y (:field "b" :type "quantitative" :scale (:type "log"))))
              (:mark (:type "rule") :encoding (:x (:field "a" :type "quantitative")
                                                  :opacity (:condition (:param "hover" :value 1)
                                                                       :value 0)))]))))

(ert-deftest eas-spec-check-flags-input-binds-and-expr-params ()
  (should (equal (eas-spec-test--codes
                  (eas-spec-check
                   '(:mark "point"
                     :params [(:name "p" :bind (:input "range"))
                              (:name "q" :expr "1 + 1")])))
                 ;; Input binds are native now (fc-qx1.33); expression params are not.
                 '(("UNSUPPORTED_FEATURE" . "/params/1")))))

(ert-deftest eas-spec-check-restricts-to-supported-list ()
  (let ((eas-spec-supported-function (lambda () '("mark/line"))))
    (should (equal (eas-spec-test--codes (eas-spec-check '(:mark "bar")))
                   '(("UNSUPPORTED_FEATURE" . "/mark"))))
    (should-not (eas-spec-check '(:mark "line")))))

(ert-deftest eas-spec-view-without-mark-is-invalid ()
  (should (equal (eas-spec-test--codes (eas-spec-check '(:layer [(:encoding nil)])))
                 '(("INVALID_INPUT" . "/layer/0")))))

;;; Templates

(ert-deftest eas-template-registry-autoloads-templates-dir ()
  (eas-template-reload)
  (should (member "line" (eas-template-names)))
  (should (member "bars" (eas-template-names)))
  (let ((described (eas-template-describe "line")))
    (should (equal (plist-get described :version) "1.0.0"))
    (should (plist-get (plist-get described :slots) :data))
    (should (file-exists-p (plist-get described :example)))
    (should (string-suffix-p "templates/line.json" (plist-get described :path)))))

(ert-deftest eas-template-unknown-is-not-found ()
  (eas-test-should-code "NOT_FOUND" (eas-template-get "no-such-template")))

(ert-deftest eas-template-bind-names-the-slot ()
  (should (equal (plist-get (eas-test-should-code "SLOT_MISSING"
                              (eas-resolve "line" nil))
                            :slot)
                 "data"))
  (should (equal (plist-get (eas-test-should-code "SLOT_TYPE"
                              (eas-resolve "line" '(:data [(:date 1 :value 2)] :points 3)))
                            :slot)
                 "points"))
  (let ((err (eas-test-should-code "FIELD_MISSING"
               (eas-resolve "line" '(:data [(:t 1 :v 2)])))))
    (should (equal (plist-get err :slot) "x"))
    (should (equal (plist-get err :field) "date")))
  (let ((err (eas-test-should-code "SHAPE_INVALID"
               (eas-resolve "line" '(:data [(:date 1) 7])))))
    (should (equal (plist-get err :slot) "data"))
    (should (equal (plist-get err :index) 1)))
  (eas-test-should-code "INVALID_INPUT"
    (eas-resolve "line" '(:data [(:date 1 :value 2)] :colour "red"))))

(ert-deftest eas-template-enum-and-array-items ()
  (eas-template-register
   '(:x-eas (:template "test-enum" :version "1.0.0"
               :slots (:style (:enum ["a" "b"] :default "a")
                       :ids (:type "array" :items "integer" :default [])))
     :mark (:type (:x-eas:slot "style"))))
  (should (equal (eas-template-bind (eas-template-get "test-enum") '(:ids (1 2)))
                 '(:style "a" :ids [1 2])))
  (eas-test-should-code "SLOT_TYPE"
    (eas-template-bind (eas-template-get "test-enum") '(:style "c")))
  (should (equal (plist-get (eas-test-should-code "SLOT_TYPE"
                              (eas-template-bind (eas-template-get "test-enum")
                                                   '(:ids [1 "x"])))
                            :index)
                 1)))

;;; Resolve

(ert-deftest eas-resolve-template-goldens ()
  (dolist (name '("line" "bars"))
    (eas-test-golden (format "resolve-%s.json" name)
                       (eas-json-pretty (eas-resolve name (eas-template-example name))))))

(ert-deftest eas-resolve-when-and-strip ()
  (let ((resolved (eas-resolve "line" (plist-put (eas-template-example "line") :points t))))
    (should (= (length (plist-get resolved :layer)) 2))
    (should-not (plist-get resolved :x-eas))
    (should (equal (plist-get resolved :$schema) eas-spec-schema-url))
    (should (vectorp (plist-get (plist-get resolved :data) :values))))
  (should (= (length (plist-get (eas-resolve "line" (eas-template-example "line")) :layer))
             1)))

(ert-deftest eas-resolve-is-deterministic ()
  (let ((a (eas-resolve "bars" (eas-template-example "bars")))
        (b (eas-resolve "bars" (eas-template-example "bars"))))
    (should (equal (eas-resolve-hash a) (eas-resolve-hash b)))
    (should (string-prefix-p "sha256:" (eas-resolve-hash a)))))

(ert-deftest eas-resolve-materializes-domain-transforms ()
  (let ((eas-transforms nil))
    (eas-register-transform
     "double" :schema '(:field (:type "string" :required t) :as (:type "string" :default "d"))
     :fn (lambda (rows params)
           (let ((in (eas-key (plist-get params :field)))
                 (out (eas-key (plist-get params :as))))
             (seq-map (lambda (row) (append row (list out (* 2 (plist-get row in))))) rows))))
    (let ((resolved (eas-resolve-spec
                     '(:data (:values [(:a 1) (:a 2)])
                       :layer [(:transform [(:x-eas:transform "double" :field "a")
                                            (:filter "datum.d > 2")]
                                :mark "point")]))))
      (should (equal (plist-get (plist-get resolved :data) :values) [(:a 1 :d 2) (:a 2 :d 4)]))
      (should (equal (plist-get (aref (plist-get resolved :layer) 0) :transform)
                     [(:filter "datum.d > 2")])))
    (eas-test-should-code "UNSUPPORTED_FEATURE"
      (eas-resolve-spec '(:data (:values [(:a 1)])
                            :transform [(:filter "true") (:x-eas:transform "double" :field "a")]
                            :mark "point")))
    (eas-test-should-code "INVALID_INPUT"
      (eas-resolve-spec '(:data (:values [(:a 1)])
                            :transform [(:x-eas:transform "double")] :mark "point")))
    (eas-test-should-code "TRANSFORM_UNKNOWN"
      (eas-resolve-spec '(:data (:values [(:a 1)])
                            :transform [(:x-eas:transform "nope")] :mark "point")))))

(defun eas-spec-test--eas-only-p (name)
  "Non-nil when template NAME declares \"export\": {\"vega-lite\": false}.
Such a template uses a capability Vega-Lite lacks and says why in
\"reason\"; it renders natively but bin/chart cannot export it."
  (let ((export (plist-get (plist-get (eas-template-get name) :meta) :export)))
    (and export (eq (plist-get export :vega-lite) :false)
         (stringp (plist-get export :reason)))))

(ert-deftest eas-template-eas-only-exports-say-why ()
  ;; Every eas-only template names its reason; keep the list short.
  (should (equal (seq-filter #'eas-spec-test--eas-only-p (eas-template-names))
                 '("projections"))))

(ert-deftest eas-resolve-output-passes-bin-chart-check ()
  (eas-test-require-chart)
  (dolist (name (seq-remove #'eas-spec-test--eas-only-p (eas-template-names)))
    ;; Gallery reproductions leave the title empty to match their
    ;; untitled references; a caller supplies one, so the check does.
    (let* ((example (eas-template-example name))
           (bindings (if (and (member (plist-get example :title) '(nil ""))
                              (plist-get (plist-get (plist-get (eas-template-get name) :meta) :slots) :title))
                         (plist-put (copy-sequence example) :title name)
                       example))
           (file (make-temp-file "eas-resolved" nil ".vl.json"
                                 (eas-json-encode (eas-resolve name bindings)))))
      (unwind-protect
          (should (zerop (call-process eas-test-chart-program nil nil nil "check" file)))
        (delete-file file)))))




(ert-deftest eas-describe-lists-registries ()
  (let ((all (eas-describe)))
    (should (equal (plist-get all :vega-lite) "6.4.1"))
    (should (seq-find (lambda (tpl) (equal (plist-get tpl :name) "line")) (plist-get all :templates)))
    (should (seq-find (lambda (tr) (equal (plist-get tr :name) "lttb")) (plist-get all :transforms)))
    (should (seq-find (lambda (ad) (equal (plist-get ad :name) "csv")) (plist-get all :adapters)))
    (should (stringp (eas-json-encode all))))
  (should (equal (eas-plist-keys (eas-describe "adapters")) '(:adapters)))
  (eas-test-should-code "NOT_FOUND" (eas-describe 'nope)))

(provide 'eas-spec-test)
;;; eas-spec-test.el ends here

;;; eas-air-traffic-test.el --- the animated air-traffic template -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; templates/air-traffic.json with examples/air-traffic.data.json (the
;; BTS Air Traffic tables, 2021-2024, read from CSV): it resolves to
;; schema-valid Vega-Lite, draws as SVG and as text, and its x-eas
;; timer, keys and month slider move the current month.  The full
;; JSON-schema check needs node, ajv and the Vega-Lite schema (named by
;; EAS_VL_SCHEMA) and skips, with the reason, without them.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-play)

(defmacro eas-air-traffic-with (&rest body)
  "Run BODY with fresh view and play registries, in UTC."
  (declare (indent 0))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-plays (make-hash-table :test 'equal))
         (eas-time-zone nil))
     ,@body))

(defun eas-air-traffic-open (&optional bindings)
  "Open the air-traffic template with BINDINGS (default its example)."
  (eas-view-open "air-traffic" :bindings (or bindings (eas-template-example "air-traffic"))
                 :id "air-traffic"))

(defun eas-air-traffic-param (view name)
  "The value of param NAME in VIEW."
  (plist-get (eas-compile--env (eas-view-spec view) (eas-view-state view)) (eas-key name)))

(defun eas-air-traffic-rows (view panel index)
  "Rows of layer INDEX of PANEL (0 left, 1 right) in VIEW's scene."
  (let* ((id (format "hconcat_%d" panel))
         (v (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get (eas-view-scene view) :views)))
         (m (seq-find (lambda (m) (equal (plist-get m :id) (format "%s/%d" id index))) (plist-get v :marks))))
    (plist-get m :rows)))

(defun eas-air-traffic--faults (node path)
  "Vega-Lite schema faults under NODE at PATH, as a list of strings.
A fault is a leftover x-eas key or placeholder, a null encoding channel
or mark property, or a DateTime month outside 1-12."
  (cond
   ((and (consp node) (keywordp (car node)))
    (cl-loop for (k v) on node by #'cddr
             for name = (substring (symbol-name k) 1)
             for p = (concat path "." name)
             when (string-prefix-p "x-eas" name) collect (concat p " is an x-eas key")
             when (and (memq v '(nil :null))
                       (string-match-p "\\.\\(encoding\\|mark\\)\\.[^.]+\\'" p))
             collect (concat p " is null")
             when (and (eq k :month) (plist-member node :year) (numberp v) (not (<= 1 v 12)))
             collect (concat p " is not a month")
             append (eas-air-traffic--faults v p)))
   ((vectorp node)
    (cl-loop for e across node append (eas-air-traffic--faults e (concat path "[]"))))))

(ert-deftest eas-air-traffic-resolves ()
  "The example reads the BTS CSV and resolves to schema-valid Vega-Lite."
  (let* ((bindings (eas-template-example "air-traffic"))
         (spec (eas-resolve "air-traffic" bindings)))
    ;; 24 tables x 48 months.
    (should (equal (plist-get bindings :data)
                   (list :file (eas-test-file "examples/data/air-traffic/air-traffic-2021-2024.csv"))))
    (should (= (length (plist-get (plist-get spec :data) :values)) 1152))
    (should (equal (plist-get spec :$schema) eas-spec-schema-url))
    (should (= (length (plist-get spec :hconcat)) 2))
    (should (equal (mapcar (lambda (p) (plist-get p :name)) (plist-get spec :params)) '("month" "playing")))
    (should (equal (plist-get (plist-get (aref (plist-get spec :params) 0) :bind) :max) 48))
    (should (equal (eas-air-traffic--faults spec "") nil))
    (should (equal (eas-play-config "air-traffic")
                   (eas-play-config (plist-get (eas-template-get "air-traffic") :spec))))))

(ert-deftest eas-air-traffic-resolves-json-schema-valid ()
  "The resolved example validates against the Vega-Lite JSON schema."
  (let ((schema (getenv "EAS_VL_SCHEMA")))
    (unless (and schema (file-readable-p schema))
      (eas-test-skip "EAS_VL_SCHEMA does not name vega-lite-schema.json (npm vega-lite@6.4.1 build/)"))
    (unless (executable-find "node")
      (eas-test-skip "node is not on PATH"))
    (unless (zerop (call-process "node" nil nil nil "-e" "require('ajv')"))
      (eas-test-skip "node cannot require ajv; put ajv@8 on NODE_PATH"))
    (let ((file (make-temp-file "eas-air-traffic" nil ".vl.json"
                                (eas-json-encode (eas-resolve "air-traffic" (eas-template-example "air-traffic"))))))
      (unwind-protect
          (with-temp-buffer
            (let ((status (call-process
                           "node" nil t nil "-e"
                           (concat "const A=require('ajv'),fs=require('fs');"
                                   "const v=new A({strict:false,allowUnionTypes:true,logger:false})"
                                   ".compile(JSON.parse(fs.readFileSync(process.argv[1])));"
                                   "if(!v(JSON.parse(fs.readFileSync(process.argv[2]))))"
                                   "{console.log(JSON.stringify(v.errors));process.exit(1)}")
                           schema file)))
              (should (equal (cons status (buffer-string)) (cons 0 "")))))
        (delete-file file)))))

(ert-deftest eas-air-traffic-takes-other-tables ()
  "Data, fields, series and labels are slots: any monthly table animates."
  (eas-air-traffic-with
    (let* ((rows (vconcat (cl-loop for m from 1 to 6
                                   append (list (list :s "a" :y 2020 :m m :v (* 10 m))
                                                (list :s "b" :y 2020 :m m :v m)))))
           (view (eas-air-traffic-open (list :data rows :series "s" :year "y" :month "m" :value "v"
                                             :left_series ["a"] :right_series ["b"] :labels ["A"]
                                             :start_year 2020 :months 6 :left_domain [0 60]
                                             :right_domain [0 6]))))
      (should (equal (eas-view-warnings view) nil))
      (should (= (length (eas-air-traffic-rows view 0 1)) 6))
      (should (equal (plist-get (aref (eas-air-traffic-rows view 1 2) 0) :label) "A"))
      (eas-play-tick view 1.0)
      (should (= (eas-air-traffic-param view "month") 1))))
  (eas-test-should-code "FIELD_MISSING"
    (eas-resolve "air-traffic" (list :data [(:a 1)]))))

(ert-deftest eas-air-traffic-renders-svg-and-text ()
  "The example opens as an interactive view and draws as SVG and text."
  (eas-air-traffic-with
    (let ((view (eas-air-traffic-open)))
      (should (eq (eas-view-interactive view) t))
      (should (equal (eas-view-warnings view) nil))
      (let ((svg (eas-svg-render (eas-view-scene view))))
        (should (string-prefix-p "<svg" svg))
        (should (string-match-p "December 2024" svg))
        (should (string-match-p "Load factor" svg)))
      (let ((text (eas-text-render
                   (eas-compile (eas-view-spec view) :rows (plist-get (eas-view-data view) :rows)
                                :target 'text :size '(:cols 120 :rows 36)))))
        (should (string-match-p "December 2024" text))
        (should (string-match-p "2021" text))
        (should (string-match-p "International" text))))))

(ert-deftest eas-air-traffic-timer-advances-the-month ()
  "Each tick draws one more month and moves the highlight; the last
month wraps to the first; space pauses, the arrows and the slider step."
  (eas-air-traffic-with
    (let* ((view (eas-air-traffic-open))
           (current (lambda () (plist-get (aref (eas-air-traffic-rows view 0 2) 0) :frame))))
      ;; Opened at the last month, the whole series shows.
      (should (= (eas-air-traffic-param view "month") 48))
      (should (= (length (eas-air-traffic-rows view 0 1)) 144))
      (eas-play-tick view 1.0)
      (should (= (eas-air-traffic-param view "month") 1))
      (should (= (length (eas-air-traffic-rows view 0 1)) 3))
      (eas-play-tick view 2.0)
      (eas-play-tick view 3.0)
      (should (= (eas-air-traffic-param view "month") 3))
      (should (= (length (eas-air-traffic-rows view 0 1)) 9))
      (should (= (funcall current) 3))
      ;; Three highlighted points per panel, at March 2021.
      (should (= (length (eas-air-traffic-rows view 1 2)) 3))
      (should (equal (plist-get (aref (eas-air-traffic-rows view 0 4) 0) :now) "March 2021"))
      (should (equal (car (last (mapcar (lambda (e) (plist-get e :summary))
                                        (append (eas-view-log-entries view) nil))))
                     "param month = 3"))
      ;; Paused, the timer leaves the month alone.
      (eas-play-key view "space")
      (should (eq (eas-air-traffic-param view "playing") :false))
      (eas-play-tick view 4.0)
      (should (= (eas-air-traffic-param view "month") 3))
      (eas-play-key view "right")
      (should (= (funcall current) 4))
      (eas-play-key view "left")
      (eas-play-key view "left")
      (should (= (eas-air-traffic-param view "month") 2))
      ;; The slider sets the month as a "param" event.
      (eas-dispatch view '(:type "param" :param "month" :value 30))
      (should (= (funcall current) 30))
      (eas-play-key view "space")
      (eas-play-tick view 5.0)
      (should (= (eas-air-traffic-param view "month") 31)))))

(ert-deftest eas-air-traffic-domain-exprs-evaluate ()
  "An explicit scale domain's {\"expr\": E} entries are evaluated."
  (let* ((scene (eas-compile (eas-resolve-spec
                              '(:data (:values [(:a 1)]) :params [(:name "top" :value 8)]
                                :mark "point"
                                :encoding (:x (:field "a" :type "quantitative"
                                                  :scale (:domain [0 (:expr "top * 2")] :nice :false)))))))
         (x (plist-get (plist-get (aref (plist-get scene :views) 0) :scales) :x)))
    (should (equal (append (plist-get x :domain) nil) '(0.0 16.0)))))

(provide 'eas-air-traffic-test)
;;; eas-air-traffic-test.el ends here

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
(require 'eas-agent-health)

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

;;; Templates

(defmacro eas-port-gaps-test--registry (&rest body)
  "Run BODY with a private template registry and load-error list."
  (declare (indent 0))
  `(let ((eas--templates (progn (eas-template-names) (copy-sequence eas--templates)))
         (eas-template-directories eas-template-directories)
         (eas-template-namespaces nil)
         (eas-template-load-errors nil))
     ,@body))

(defconst eas-port-gaps-test--rows [(:k "a" :g "x" :v 1) (:k "b" :g "y" :v 2) (:k "c" :g "x" :v 3)]
  "Rows the template tests bind.")

(defun eas-port-gaps-test--template (name slots &rest body)
  "A template NAME with SLOTS (plus a json data slot) and BODY keys."
  (append (list :x-eas (list :template name :version "1"
                             :slots (append '(:data (:shape "json" :required t)) slots))
                :data '(:name "data"))
          body))

(defun eas-port-gaps-test--text (spec &optional cols rows)
  "SPEC drawn as text, COLS by ROWS."
  (substring-no-properties
   (eas-text-render (eas-compile spec :target 'text :size (list :cols (or cols 50) :rows (or rows 14))))))

(defconst eas-port-gaps-test--bar
  '(:mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
  "A bar view over the test rows.")

(ert-deftest eas-port-gaps-template-may-hold-a-facet ()
  (eas-port-gaps-test--registry
    (eas-template-register
     (eas-port-gaps-test--template "gap-facet" '(:by (:type "field" :default "g"))
                                   :facet '(:field (:x-eas:slot "by") :type "nominal")
                                   :columns 2 :spec eas-port-gaps-test--bar))
    (let ((spec (eas-resolve "gap-facet" (list :data eas-port-gaps-test--rows))))
      ;; Export keeps the facet; compile lowers it to one cell per level.
      (should (equal (plist-get spec :facet) '(:field "g" :type "nominal")))
      (should (null (eas-spec-unsupported spec)))
      (should (= (length (eas-port-gaps-test--marks (eas-compile spec) "bar")) 2)))))

(ert-deftest eas-port-gaps-concat-wraps-into-rows ()
  (let* ((spec (list :data (list :values eas-port-gaps-test--rows) :columns 2
                     :concat (vconcat (make-list 3 eas-port-gaps-test--bar))))
         (parsed (eas-spec-parse spec)))
    (should (null (eas-spec-check spec)))
    (should (= (length (plist-get parsed :vconcat)) 2))
    (should (= (length (plist-get (aref (plist-get parsed :vconcat) 0) :hconcat)) 2))
    (should (= (length (plist-get (aref (plist-get parsed :vconcat) 1) :hconcat)) 1))
    (should (= (length (plist-get (eas-compile spec) :views)) 3)))
  ;; No columns: one row, as in Vega-Lite.
  (should (= (length (plist-get (eas-spec-parse (list :data (list :values eas-port-gaps-test--rows)
                                                      :concat (vector eas-port-gaps-test--bar eas-port-gaps-test--bar)))
                                :hconcat))
             2)))

(ert-deftest eas-port-gaps-template-concat-wraps-after-each-expands ()
  (eas-port-gaps-test--registry
    (eas-template-register
     (eas-port-gaps-test--template
      "gap-concat" '(:fields (:type "array" :default ["v" "v" "v"]))
      :columns 2
      :concat `[(:x-eas:each "fields"
                 :spec (:mark "bar" :encoding (:x (:field "k" :type "nominal")
                                               :y (:field (:x-eas:item ".") :type "quantitative"))))]))
    (let ((spec (eas-resolve "gap-concat" (list :data eas-port-gaps-test--rows))))
      (should (= (length (plist-get spec :concat)) 3))
      (should (= (length (plist-get (eas-compile spec) :views)) 3)))))

(ert-deftest eas-port-gaps-each-nests ()
  (eas-port-gaps-test--registry
    (eas-template-register
     (eas-port-gaps-test--template
      "gap-nest" '(:panes (:type "array" :default [(:name "one" :lines ["v"]) (:name "two" :lines ["v" "v"])]))
      :vconcat
      [(:x-eas:each "panes"
        :spec (:name (:x-eas:item "name")
               :layer [(:x-eas:each (:x-eas:item "lines")
                        :spec (:mark "line" :description (:x-eas:item "../name")
                               :encoding (:x (:field "k" :type "nominal")
                                          :y (:field (:x-eas:item ".") :type "quantitative"))))]))]))
    (let* ((panes (plist-get (eas-resolve "gap-nest" (list :data eas-port-gaps-test--rows)) :vconcat)))
      (should (equal (mapcar (lambda (p) (length (plist-get p :layer))) panes) '(1 2)))
      (should (equal (plist-get (aref (plist-get (aref panes 1) :layer) 1) :description) "two")))))

(ert-deftest eas-port-gaps-slot-reaches-into-objects-and-strings ()
  (eas-port-gaps-test--registry
    (eas-template-register
     (eas-port-gaps-test--template
      "gap-strings" '(:range (:type "object" :default (:lo 1 :hi 3)) :metric (:type "string" :default "Pulse"))
      :title '(:x-eas:text "Average {{metric}} ({{range.lo}}-{{range.hi}})")
      :transform [(:filter (:x-eas:expr "datum.v >= {{range.lo}} && datum.k != {{metric}}"))]
      :mark "bar"
      :encoding '(:x (:field "k" :type "nominal")
                  :y (:field "v" :type "quantitative"
                      :scale (:domain [(:x-eas:slot "range" :key "lo") (:x-eas:slot "range" :key "hi")]
                              :zero (:x-eas:slot "range" :key "zero" :default :false))))))
    (let ((spec (eas-resolve "gap-strings" (list :data eas-port-gaps-test--rows))))
      (should (equal (plist-get spec :title) "Average Pulse (1-3)"))
      (should (equal (plist-get (aref (plist-get spec :transform) 0) :filter)
                     "datum.v >= 1 && datum.k != \"Pulse\""))
      (should (equal (plist-get (plist-get (plist-get spec :encoding) :y) :scale)
                     '(:domain [1 3] :zero :false))))
    (eas-test-should-code "SLOT_MISSING"
      (eas-resolve-spec '(:mark "bar" :title (:x-eas:text "{{nope}}")) '(:a 1)))))

(ert-deftest eas-port-gaps-null-slot-leaves-the-property-out ()
  (eas-port-gaps-test--registry
    (eas-template-register
     (eas-port-gaps-test--template
      "gap-null" '(:ticks (:type "number" :default :null) :domain (:type "array" :default :null))
      :mark "bar"
      :encoding '(:x (:field "k" :type "nominal")
                  :y (:field "v" :type "quantitative" :axis (:tickCount (:x-eas:slot "ticks"))
                      :scale (:domain (:x-eas:slot "domain"))))))
    (let ((y (plist-get (plist-get (eas-resolve "gap-null" (list :data eas-port-gaps-test--rows)) :encoding) :y)))
      ;; An empty object, which Vega-Lite reads as no axis properties.
      (should (equal (plist-get y :axis) nil))
      (should (equal (plist-get y :scale) nil)))
    (let ((y (plist-get (plist-get (eas-resolve "gap-null" (list :data eas-port-gaps-test--rows :ticks 3)) :encoding) :y)))
      (should (equal (plist-get y :axis) '(:tickCount 3))))))

(ert-deftest eas-port-gaps-tick-count-may-be-a-time-interval ()
  (let* ((spec (lambda (tc)
                 (list :data '(:values [(:t "2024-01-01" :v 1) (:t "2024-12-31" :v 3)])
                       :mark "line"
                       :encoding (list :x (list :field "t" :type "temporal" :axis (list :tickCount tc))
                                       :y '(:field "v" :type "quantitative")))))
         (labels (lambda (tc)
                   (let* ((view (aref (vconcat (plist-get (eas-compile (funcall spec tc)) :views)) 0))
                          (axis (seq-find (lambda (a) (equal (plist-get a :channel) "x")) (plist-get view :axes))))
                     (mapcar (lambda (tk) (plist-get tk :label)) (plist-get axis :ticks))))))
    (should (equal (funcall labels '(:interval "month" :step 3)) '("2024" "April" "July" "October")))
    (should (= (length (funcall labels "month")) 12))
    (should (null (eas-spec-check (funcall spec '(:interval "month" :step 3)))))))

(ert-deftest eas-port-gaps-a-bad-template-file-is-skipped-and-reported ()
  (let ((dir (make-temp-file "eas-gaps" t)))
    (unwind-protect
        (eas-port-gaps-test--registry
          (with-temp-file (expand-file-name "a-broken.json" dir) (insert "{\"x-eas\": "))
          (with-temp-file (expand-file-name "b-nometa.json" dir) (insert "{\"mark\": \"bar\"}"))
          (with-temp-file (expand-file-name "c-good.json" dir)
            (insert (eas-json-encode (eas-port-gaps-test--template "gap-good" nil :mark "bar"))))
          (setq eas-template-directories (append eas-template-directories (list dir)))
          (let ((names (eas-template-reload)))
            (should (member "gap-good" names))
            (should (member "line" names)))
          (should (equal (mapcar (lambda (e) (file-name-nondirectory (plist-get e :file)))
                                 eas-template-load-errors)
                         '("a-broken.json" "b-nometa.json")))
          (should (equal (mapcar (lambda (e) (plist-get e :code)) eas-template-load-errors)
                         '("PARSE_ERROR" "INVALID_INPUT")))
          (should (seq-find (lambda (r) (equal (plist-get r :name) "template-file:a-broken.json"))
                            (eas-agent-doctor-rows))))
      (delete-directory dir t))))

(ert-deftest eas-port-gaps-templates-have-namespaces ()
  (let ((one (make-temp-file "eas-gaps-a" t)) (two (make-temp-file "eas-gaps-b" t)))
    (unwind-protect
        (eas-port-gaps-test--registry
          (dolist (dir (list one two))
            (with-temp-file (expand-file-name "trend.json" dir)
              (insert (eas-json-encode (eas-port-gaps-test--template "trend" nil :mark "bar")))))
          (with-temp-file (expand-file-name "solo.json" two)
            (insert (eas-json-encode (eas-port-gaps-test--template "solo" nil :mark "bar"))))
          (eas-template-add-directory one "health")
          (eas-template-add-directory two "money")
          (let ((names (eas-template-names)))
            (should (member "health/trend" names))
            (should (member "money/trend" names))
            (should (member "money/solo" names))
            (should (member "line" names)))
          (should (null eas-template-load-errors))
          ;; A bare name finds a namespaced template only when it is unique.
          (should (equal (plist-get (eas-template-get "solo") :name) "money/solo"))
          (should (eas-template-p "solo"))
          (should-not (eas-template-p "trend"))
          (eas-test-should-code "NOT_FOUND" (eas-template-get "trend")))
      (delete-directory one t)
      (delete-directory two t))))

;;; Legends and the text target

(defconst eas-port-gaps-test--color-and-fill
  '(:data (:values [(:a 1 :b 2 :c "x" :d "p") (:a 2 :b 3 :c "y" :d "q")])
    :layer [(:mark "line" :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")
                                     :color (:field "c" :type "nominal")))
            (:mark (:type "point" :filled t)
             :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")
                        :fill (:field "d" :type "nominal" :scale (:range ["red" "green"]))))])
  "A color legend on the lines and a fill legend of another field on the points.")

(ert-deftest eas-port-gaps-fill-legend-beside-a-color-legend ()
  (let* ((scene (eas-compile eas-port-gaps-test--color-and-fill))
         (view (aref (vconcat (plist-get scene :views)) 0))
         (legends (append (plist-get view :legends) nil)))
    (should (equal (mapcar (lambda (l) (plist-get l :channel)) legends) '("color" "fill")))
    (should (equal (mapcar (lambda (e) (plist-get e :color)) (plist-get (nth 1 legends) :entries))
                   '("red" "green")))
    (should (equal (mapcar (lambda (i) (plist-get i :fill))
                           (plist-get (car (eas-port-gaps-test--marks scene "point")) :items))
                   '("red" "green"))))
  (let ((text (eas-port-gaps-test--text eas-port-gaps-test--color-and-fill 60 12)))
    (should (string-match-p "● p" text))
    (should (string-match-p "━ x" text))))

(ert-deftest eas-port-gaps-fill-of-the-color-field-shares-its-legend ()
  (let* ((spec '(:data (:values [(:a 1 :c "x") (:a 2 :c "y")]) :mark "bar"
                 :encoding (:x (:field "c" :type "nominal") :y (:field "a" :type "quantitative")
                            :color (:field "c" :type "nominal") :fill (:field "c" :type "nominal"))))
         (view (aref (vconcat (plist-get (eas-compile spec) :views)) 0)))
    (should (= (length (plist-get view :legends)) 1))))

(provide 'eas-port-gaps-test)
;;; eas-port-gaps-test.el ends here

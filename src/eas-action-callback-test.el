;;; eas-action-callback-test.el --- tests for click callbacks set up ahead of time -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.10: global callbacks, function bindings, :when predicates and
;; axis, title and background click targets, driven headlessly through
;; `eas-dispatch' in SVG scenes (through their :map areas) and text
;; buffers (through RET at point).

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-action-callback-test--spec
  '(:data (:values [(:fruit "apple" :amount 1) (:fruit "banana" :amount 5) (:fruit "cherry" :amount 3)])
    :title "Harvest" :width 240 :height 120 :mark "bar"
    :encoding (:x (:field "fruit" :type "nominal") :y (:field "amount" :type "quantitative")))
  "Three bars, the first short, under a title.")

(defmacro eas-action-callback-test--with-view (var source &rest body)
  "Open SOURCE (a template name or spec) as VAR (id \"t\"), then run BODY.
The view registry and the global bindings are fresh."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-action-default-bindings nil)
         (eas-action-inhibit nil)
         (eas-action-drill-show-function #'ignore))
     (let ((,var (if (stringp ,source)
                     (eas-view-open ,source :bindings (eas-template-example ,source) :id "t")
                   (eas-view-open ,source :id "t"))))
       ,@body)))

(defun eas-action-callback-test--centre (view mark-id i)
  "Pixel centre of item I of MARK-ID in VIEW's scene."
  (let ((item (aref (plist-get (eas-scene-mark (eas-view-scene view) mark-id) :items) i)))
    (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))))

(defun eas-action-callback-test--area (view id)
  "Centre of VIEW's SVG :map area named ID."
  (let* ((area (seq-find (lambda (a) (eq (nth 1 a) (intern id))) (eas-svg-hot-spots (eas-view-scene view))))
         (rect (cdr (car area))))
    (should area)
    (vector (/ (+ (caar rect) (cadr rect)) 2.0) (/ (+ (cdar rect) (cddr rect)) 2.0))))

(defun eas-action-callback-test--mouse-1 (view px)
  "Press and release at PX in VIEW, as a GUI mouse-1 does; return the click."
  (eas-dispatch view (list :type "pointerdown" :px px))
  (plist-get (eas-dispatch view (list :type "pointerup" :px px)) :click))

(defmacro eas-action-callback-test--in-text (view &rest body)
  "Show VIEW as text in a temporary eas buffer, then run BODY there."
  (declare (indent 1))
  `(progn
     (eas-view-resize ,view '(:cols 60 :rows 18) 'text)
     (with-temp-buffer
       (eas-view-mode)
       (setq eas-mode--view ,view)
       (setf (eas-view-buffer ,view) (current-buffer))
       (let ((inhibit-read-only t)) (insert (eas-text-render (eas-view-scene ,view))))
       ,@body)))

(defun eas-action-callback-test--ret-on (regexp &optional nth)
  "Move point to the NTH (default 1) match of REGEXP and press RET.
Return the view's click."
  (goto-char (point-min))
  (should (re-search-forward regexp nil t (or nth 1)))
  (goto-char (match-beginning 0))
  (eas-mode-click-at-point)
  (plist-get (eas-inspect eas-mode--view) :click))

(defun eas-action-callback-test--ret-at-px (px)
  "Move point to the text cell holding scene pixel PX and press RET."
  (let ((cell (plist-get (plist-get (eas-view-scene eas-mode--view) :size) :cell)))
    (goto-char (point-min))
    (forward-line (floor (aref px 1) (aref cell 1)))
    (move-to-column (floor (aref px 0) (aref cell 0)))
    (eas-mode-click-at-point)
    (plist-get (eas-inspect eas-mode--view) :click)))

;;; Global callbacks and function bindings

(ert-deftest eas-action-callback-defined-before-the-view-exists ()
  (let ((eas-action-default-bindings nil) (seen nil))
    ;; As in init.el: no view, no template instance yet.
    (eas-define-callback "bars" "main/0" (lambda (target view) (push (list target (eas-view-id view)) seen) "hi")
                         :doc "Say hi.")
    (eas-define-callback nil "main/0" #'ignore)
    (should (= (length eas-action-default-bindings) 2))
    ;; Re-evaluating the init file replaces rather than duplicates.
    (eas-define-callback "bars" "main/0" (lambda (_target _view) (push 'second seen) "again"))
    (should (= (length eas-action-default-bindings) 2))
    (let ((bindings eas-action-default-bindings))
      (eas-action-callback-test--with-view v "bars"
        (setq eas-action-default-bindings bindings)
        (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-callback-test--centre v "main/0" 1)))
                                :click)))
          (should (equal (plist-get click :action) "lambda"))
          (should (equal (plist-get click :result) "again"))
          (should (equal seen '(second))))
        ;; Any-template entries run when the template has none.
        (eas-remove-callback "bars" "main/0")
        (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                   :px (eas-action-callback-test--centre v "main/0" 1)))
                                             :click)
                                  :action)
                       "ignore"))))))

(ert-deftest eas-action-callback-target-carries-datum-fields-and-data-space ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (let (got)
      (eas-define-callback nil "main/0" (lambda (target view) (setq got (list target view)) nil))
      (eas-dispatch v (list :type "click" :px (eas-action-callback-test--centre v "main/0" 1)))
      (let ((target (car got)))
        (should (eq (cadr got) v))
        (should (equal (plist-get target :view) "main"))
        (should (equal (plist-get target :mark) "main/0"))
        (should (equal (plist-get target :datum) 1))
        (should (equal (plist-get target :row) '(:fruit "banana" :amount 5)))
        (should (equal (plist-get target :x) "banana"))
        (should (< 0 (plist-get target :y) 5))
        (should (equal (plist-get target :fields) '(:x "fruit" :y "amount")))))))

(ert-deftest eas-action-callback-bind-accepts-functions ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (defalias 'eas-action-callback-test--fn (lambda (target _view) (plist-get (plist-get target :row) :fruit)))
    (eas-action-bind v "main/0" 'eas-action-callback-test--fn)
    (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-callback-test--centre v "main/0" 2)))
                            :click)))
      (should (equal (plist-get click :action) "eas-action-callback-test--fn"))
      (should (equal (plist-get click :result) "cherry")))
    (eas-action-bind v "main/0" (list :fn (lambda (target _view) (format "%S" (plist-get target :args))) :depth 2))
    (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                               :px (eas-action-callback-test--centre v "main/0" 2)))
                                         :click)
                              :result)
                   "(:depth 2)"))
    (eas-test-should-code "INVALID_INPUT" (eas-action-bind v "main/0" 'eas-action-callback-test--unbound))
    (eas-test-should-code "INVALID_INPUT" (eas-define-callback nil "main/0" 3))
    (eas-test-should-code "INVALID_INPUT" (eas-define-callback nil "main/0" #'ignore :when 3))))

(ert-deftest eas-action-callback-errors-are-recorded-not-raised ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (eas-action-bind v "main/0" (lambda (_target _view) (error "Boom")))
    (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-action-callback-test--centre v "main/0" 0)))
                            :click)))
      (should (eq (plist-get click :ran) :false))
      (should (equal (plist-get (plist-get click :error) :code) "ENGINE_FAILED"))
      (should (string-match-p "Boom" (plist-get (plist-get click :error) :message))))
    ;; Inhibited and replayed clicks record the callback without running it.
    (let ((eas-action-inhibit t))
      (should (eq (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                              :px (eas-action-callback-test--centre v "main/0" 0)))
                                        :click)
                             :ran)
                  :false)))))

(ert-deftest eas-action-callback-precedence ()
  (let ((eas--templates (copy-sequence (progn (eas-template-names) eas--templates))))
    (eas-template-register
     (eas-json-parse
      "{\"x-eas\":{\"template\":\"test-cb\",\"version\":\"1.0.0\",
         \"slots\":{\"data\":{\"shape\":\"plist\",\"required\":true}},
         \"actions\":{\"main/0\":[{\"action\":\"copy-row\",\"when\":\"datum.amount > 4\"},
                                {\"action\":\"echo\",\"when\":\"datum.amount < 2\"}]}},
        \"data\":{\"name\":\"data\"},\"width\":200,\"height\":100,\"mark\":\"bar\",
        \"encoding\":{\"x\":{\"field\":\"fruit\",\"type\":\"nominal\"},
                    \"y\":{\"field\":\"amount\",\"type\":\"quantitative\"}}}"))
    (eas-action-callback-test--with-view v eas-action-callback-test--spec
      (setq v (eas-view-open "test-cb" :bindings (list :data (plist-get (plist-get eas-action-callback-test--spec :data)
                                                                       :values))
                               :id "t"))
      (cl-flet ((action (i) (let ((inhibit-message t) (kill-ring nil))
                              (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                          :px (eas-action-callback-test--centre v "main/0" i)))
                                                    :click)
                                         :action))))
        ;; The template's list: its :when picks per bar; none holds for cherry.
        (eas-define-callback nil "main/0" #'ignore)
        (should (equal (mapcar #'action '(0 1 2)) '("echo" "copy-row" "ignore")))
        ;; Global entries keyed by the template outrank its own actions.
        (eas-define-callback "test-cb" "main/0" #'identity :when "datum.fruit === 'banana'")
        (should (equal (mapcar #'action '(0 1 2)) '("echo" "identity" "ignore")))
        ;; The view's own binding outranks everything.
        (eas-action-bind v "main/0" #'list)
        (should (equal (mapcar #'action '(0 1 2)) '("list" "list" "list")))))))

;;; :when

(ert-deftest eas-action-callback-when-splits-one-mark-into-regions ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (eas-define-callback nil "main/0" (lambda (_t _v) "low") :when "datum.amount < 2")
    (eas-define-callback nil "main/0" (lambda (_t _v) "high")
                         :when (lambda (target _view) (> (plist-get (plist-get target :row) :amount) 4)))
    ;; A :when that fails to evaluate does not hold.
    (eas-define-callback nil "main/0" (lambda (_t _v) "never") :when "nosuchname > 1")
    (cl-flet ((result (i) (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                      :px (eas-action-callback-test--centre v "main/0" i)))
                                                :click)
                                     :result)))
      (should (equal (mapcar #'result '(0 1 2)) '("low" "high" nil))))
    ;; `target' names the whole target in an expression.
    (eas-define-callback nil "main/0" (lambda (_t _v) "mid") :when "target.datum == 2")
    (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                               :px (eas-action-callback-test--centre v "main/0" 2)))
                                         :click)
                              :result)
                   "mid"))))

;; eas-7r1.18: a catch-all defined first no longer shadows a later :when.
(ert-deftest eas-action-callback-when-outranks-unconditional ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (cl-flet ((result (i) (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                      :px (eas-action-callback-test--centre v "main/0" i)))
                                                :click)
                                     :result)))
      (eas-define-callback nil "*" (lambda (_t _v) "any"))
      (eas-define-callback nil "*" (lambda (_t _v) "big") :when "datum.amount > 4")
      (eas-define-callback nil "*" (lambda (_t _v) "mid") :when "datum.amount > 2")
      ;; :when entries first, in definition order; then the catch-all.
      (should (equal (mapcar #'result '(0 1 2)) '("any" "big" "mid")))
      ;; Redefining the catch-all replaces it and keeps the order.
      (eas-define-callback nil "*" (lambda (_t _v) "other"))
      (should (equal (mapcar #'result '(0 1 2)) '("other" "big" "mid")))
      ;; Same within one view binding.
      (eas-action-bind v "main/0" (vector (lambda (_t _v) "view-any")
                                          (list :fn (lambda (_t _v) "view-low") :when "datum.amount < 2")))
      (should (equal (mapcar #'result '(0 1 2)) '("view-low" "view-any" "view-any"))))))

(ert-deftest eas-action-callback-when-order-keeps-scope-precedence ()
  (eas-action-callback-test--with-view v "bars"
    (cl-flet ((result () (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                     :px (eas-action-callback-test--centre v "main/0" 1)))
                                               :click)
                                    :result)))
      ;; An any-view :when does not outrank the template's catch-all.
      (eas-define-callback nil "main/0" (lambda (_t _v) "global-when") :when "true")
      (eas-define-callback "bars" "main/0" (lambda (_t _v) "template-any"))
      (should (equal (result) "template-any"))
      ;; A mark key still outranks "*", however conditional "*" is.
      (eas-remove-callback "bars" "main/0")
      (eas-define-callback "bars" "*" (lambda (_t _v) "star-when") :when "true")
      (should (equal (result) "global-when")))))

;;; Area targets: GUI (:map areas, mouse-1)

(ert-deftest eas-action-callback-axis-title-and-background-in-svg ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (let ((bg (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :bounds)))
      ;; Unbound, a background click still clears :click.
      (eas-dispatch v (list :type "click" :px (eas-action-callback-test--centre v "main/0" 1)))
      (should (eq (plist-get (eas-action-callback-test--mouse-1 v (vector (+ (aref bg 0) 3) (+ (aref bg 1) 3))) :area)
                  nil))
      (should (eq (plist-get (eas-inspect v) :click) :null))
      (eas-define-callback nil "axis:x" (lambda (target _v) (format "x=%s" (plist-get target :value))))
      (eas-define-callback nil "axis" (lambda (target _v) (format "%s %s" (plist-get target :axis)
                                                                 (plist-get target :part))))
      (eas-define-callback nil "title" (lambda (target _v) (plist-get target :title)))
      (eas-define-callback nil "background" (lambda (target _v) (format "%s %.1f" (plist-get target :x)
                                                                       (plist-get target :y))))
      (eas-define-callback nil "*" (lambda (_t _v) "star"))
      (let ((click (eas-action-callback-test--mouse-1 v (eas-action-callback-test--area v "eas-axis:main|x|1"))))
        (should (equal (plist-get click :area) "axis"))
        (should (equal (plist-get click :axis) "x"))
        (should (equal (plist-get click :value) "banana"))
        (should (equal (plist-get click :result) "x=banana")))
      (let ((click (eas-action-callback-test--mouse-1 v (eas-action-callback-test--area v "eas-axis:main|y|title"))))
        (should (equal (plist-get click :title) "amount"))
        (should (equal (plist-get click :result) "y title")))
      (let ((click (eas-action-callback-test--mouse-1 v (eas-action-callback-test--area v "eas-axis:main|y|2"))))
        (should (equal (plist-get click :part) "label"))
        (should (equal (format "%s" (plist-get click :value)) (concat (plist-get click :label) ".0"))))
      (should (equal (plist-get (eas-action-callback-test--mouse-1 v (eas-action-callback-test--area v "eas-title:title"))
                                :result)
                     "Harvest"))
      ;; Background: data space through the scales; "*" never answers areas.
      (let* ((px (vector (+ (aref bg 0) 3) (+ (aref bg 1) 3)))
             (click (eas-action-callback-test--mouse-1 v px)))
        (should (equal (plist-get click :area) "background"))
        (should (equal (plist-get click :x) "apple"))
        (should (< 4.5 (plist-get click :y) 5.0))
        (should (equal (plist-get click :fields) '(:x "fruit" :y "amount")))
        (should (string-prefix-p "apple 4." (plist-get click :result))))
      ;; A datum still wins over the background beneath it.
      (should (equal (plist-get (eas-action-callback-test--mouse-1 v (eas-action-callback-test--centre v "main/0" 1))
                                :result)
                     "star"))
      ;; The areas reach Emacs as :map hot spots with help-echo.
      (let ((spot (seq-find (lambda (a) (eq (nth 1 a) 'eas-axis:main|x|1)) (eas-svg-hot-spots (eas-view-scene v)))))
        (should (equal (plist-get (nth 2 spot) 'help-echo) "banana"))
        (should (eq (plist-get (nth 2 spot) 'pointer) 'hand))))))

(ert-deftest eas-action-callback-background-when-picks-regions ()
  (eas-action-callback-test--with-view v '(:data (:values [(:x 0 :y 0) (:x 10 :y 10)]) :width 200 :height 100
                                           :mark "point"
                                           :encoding (:x (:field "x" :type "quantitative")
                                                      :y (:field "y" :type "quantitative")))
    (eas-define-callback nil "background" (lambda (_t _v) "left") :when "datum.x < 5")
    (eas-define-callback nil "background" (lambda (_t _v) "right") :when "datum.x >= 5")
    (let ((b (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :bounds)))
      (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                 :px (vector (+ (aref b 0) (* 0.25 (aref b 2)))
                                                                             (+ (aref b 1) (* 0.25 (aref b 3))))))
                                           :click)
                                :result)
                     "left"))
      (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click"
                                                                 :px (vector (+ (aref b 0) (* 0.75 (aref b 2)))
                                                                             (+ (aref b 1) (* 0.25 (aref b 3))))))
                                           :click)
                                :result)
                     "right")))))

;;; Area targets: terminal (RET at point)

(ert-deftest eas-action-callback-areas-with-ret-in-a-text-buffer ()
  (eas-action-callback-test--with-view v eas-action-callback-test--spec
    (eas-define-callback nil "axis" (lambda (target _v) (format "%s %s %s" (plist-get target :axis)
                                                               (plist-get target :part)
                                                               (or (plist-get target :title) (plist-get target :value)))))
    (eas-define-callback nil "title" (lambda (target _v) (plist-get target :title)))
    (eas-define-callback nil "background" (lambda (target _v) (format "bg %s" (plist-get target :x))))
    (eas-define-callback nil "main/0" (lambda (target _v) (plist-get (plist-get target :row) :fruit)))
    (eas-action-callback-test--in-text v
      (should (equal (plist-get (eas-action-callback-test--ret-on "Harvest") :result) "Harvest"))
      (should (equal (plist-get (eas-action-callback-test--ret-on "cherry") :result) "x label cherry"))
      (should (equal (plist-get (eas-action-callback-test--ret-on "fruit *$") :result) "x title fruit"))
      (should (equal (plist-get (eas-action-callback-test--ret-on "^amount") :result) "y title amount"))
      (let ((click (eas-action-callback-test--ret-on "^ *4")))
        (should (equal (plist-get click :axis) "y"))
        (should (equal (plist-get click :value) 4.0)))
      ;; Above the short apple bar: the background, in data space.
      (let* ((b (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :bounds))
             (cw (aref (plist-get (plist-get (eas-view-scene v) :size) :cell) 0))
             (click (eas-action-callback-test--ret-at-px (vector (+ (aref b 0) (* 2 cw)) (+ (aref b 1) 2)))))
        (should (equal (plist-get click :area) "background"))
        (should (equal (plist-get click :x) "apple"))
        (should (equal (plist-get click :result) "bg apple")))
      ;; On a bar: the datum.
      (should (equal (plist-get (eas-action-callback-test--ret-at-px (eas-action-callback-test--centre v "main/0" 1))
                                :result)
                     "banana")))))

(ert-deftest eas-action-callback-legend-takes-function-bindings ()
  (eas-action-callback-test--with-view v '(:data (:values [(:x 1 :y 3 :c "u") (:x 2 :y 5 :c "v")])
                                           :width 200 :height 100
                                           :params [(:name "series" :select (:type "point" :fields ["c"])
                                                           :bind "legend")]
                                           :mark "point"
                                           :encoding (:x (:field "x" :type "quantitative")
                                                      :y (:field "y" :type "quantitative")
                                                      :color (:field "c" :type "nominal")))
    (eas-define-callback nil "legend" (lambda (target _v) (format "only %s" (plist-get target :value)))
                         :when "datum.value === 'v'")
    (should (equal (plist-get (eas-action-callback-test--mouse-1 v (eas-action-callback-test--area v "eas-legend:main|color|1"))
                              :result)
                   "only v"))
    (should (eq (plist-get (eas-action-callback-test--mouse-1 v (eas-action-callback-test--area v "eas-legend:main|color|0"))
                           :action)
                :null))))

(provide 'eas-action-callback-test)
;;; eas-action-callback-test.el ends here

;;; eas-tip-test.el --- tests for tooltips, click targets and actions -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.1: encoding.tooltip and encoding.href on the runtime, driven
;; headlessly through `eas-dispatch' and `eas-replay'.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defmacro eas-tip-test--with-view (var source &rest body)
  "Open SOURCE (a template name or spec) as VAR in a fresh registry; run BODY.
A template name opens with its example bindings."
  (declare (indent 2))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-action-inhibit nil))
     (let ((,var (if (stringp ,source)
                     (eas-view-open ,source :bindings (eas-template-example ,source) :id "t")
                   (eas-view-open ,source :id "t"))))
       ,@body)))

(defun eas-tip-test--item (view mark-id i)
  "Item I of MARK-ID in VIEW's scene."
  (aref (plist-get (eas-scene-mark (eas-view-scene view) mark-id) :items) i))

(defun eas-tip-test--centre (view mark-id i)
  "Pixel centre of item I of MARK-ID in VIEW's scene."
  (let ((item (eas-tip-test--item view mark-id i)))
    (if (plist-member item :w)
        (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0)) (+ (plist-get item :y) (/ (plist-get item :h) 2.0)))
      (vector (plist-get item :x) (plist-get item :y)))))

(defun eas-tip-test--anchor (view mark-id k)
  "Pixel of datum K on the first series of MARK-ID in VIEW's scene."
  (let ((p (aref (plist-get (eas-tip-test--item view mark-id 0) :points) k)))
    (vector (aref p 0) (aref p 1))))

(defun eas-tip-test--titles (tooltip)
  "TOOLTIP as an alist of (TITLE . VALUE)."
  (mapcar (lambda (p) (cons (plist-get p :title) (plist-get p :value))) tooltip))

(defconst eas-tip-test--href-spec
  '(:data (:values [(:k "a" :v 3 :url "https://example.com/a") (:k "b" :v 5 :url "https://example.com/b")])
    :width 200 :height 100
    :layer [(:mark "bar" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                    :href (:field "url")))
            (:mark "line" :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                     :href (:field "url")
                                     :tooltip [(:field "k" :type "nominal") (:field "v" :type "quantitative")]))])
  "Bars and a line, both linking each datum to its url.")

;;; Hover

(ert-deftest eas-tip-bars-hover-reports-the-bar-tooltip ()
  (eas-tip-test--with-view v "bars"
    (let ((inspect (eas-dispatch v (list :type "pointermove" :px (eas-tip-test--centre v "main/0" 1)))))
      (should (equal (eas-tip-test--titles (plist-get (plist-get inspect :hover) :tooltip))
                     '(("category" . "Tue") ("value" . "10400"))))
      (eas-dispatch v '(:type "pointerleave"))
      (should (eq (plist-get (eas-inspect v) :hover) :null)))))

(ert-deftest eas-tip-series-tooltip-comes-from-the-hit-index ()
  ;; One path per series: the datum is found by bisecting x, and its
  ;; tooltip is encoded from the plan, not read off the item.
  (eas-tip-test--with-view v "line"
    (should-not (plist-get (eas-tip-test--item v "main/0" 0) :tooltip))
    (let* ((px (eas-tip-test--anchor v "main/0" 4))
           (hover (plist-get (eas-dispatch v (list :type "pointermove" :px (vector (+ 2 (aref px 0)) (aref px 1))))
                             :hover)))
      (should (equal (plist-get hover :datum) 4))
      (should (equal (eas-tip-test--titles (plist-get hover :tooltip))
                     '(("date" . "Mar 06, 2026") ("value" . "108.9")))))))

(ert-deftest eas-tip-hot-spots-and-text-cells-carry-help-echo ()
  (eas-tip-test--with-view v "bars"
    (let ((areas (eas-svg-hot-spots (eas-view-scene v))))
      (should (= (length areas) 5))
      (should (equal (plist-get (nth 2 (car areas)) 'help-echo) "category: Mon\nvalue: 8200"))))
  (eas-tip-test--with-view v "bars"
    (eas-view-resize v '(:cols 60 :rows 16) 'text)
    (let ((text (eas-text-render (eas-view-scene v))))
      (should (seq-some (lambda (i) (let ((h (get-text-property i 'help-echo text)))
                                      (and h (string-match-p "category: Wed\nvalue: 6100" h))))
                        (number-sequence 0 (1- (length text))))))))

;;; Click targets and actions

(ert-deftest eas-tip-click-records-the-target ()
  (eas-tip-test--with-view v "bars"
    (let* ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-tip-test--centre v "main/0" 2))) :click)))
      (should (equal (plist-get click :mark) "main/0"))
      (should (equal (plist-get click :row) '(:category "Wed" :value 6100)))
      (should (equal (cdr (assoc "value" (eas-tip-test--titles (plist-get click :tooltip)))) "6100"))
      (should (eq (plist-get click :href) :null))
      ;; Nothing is bound and there is no href: recorded, nothing runs.
      (should (eq (plist-get click :action) :null)))
    ;; A click on empty plot space clears it; other events leave it.
    (eas-dispatch v (list :type "click" :px (eas-tip-test--centre v "main/0" 2)))
    (eas-dispatch v (list :type "pointermove" :px [5 5]))
    (should-not (eq (plist-get (eas-inspect v) :click) :null))
    (let* ((b (plist-get (aref (plist-get (eas-view-scene v) :views) 0) :bounds)))
      (eas-dispatch v (list :type "click" :px (vector (+ (aref b 0) 2) (+ (aref b 1) 2)))))
    (should (eq (plist-get (eas-inspect v) :click) :null))))

(ert-deftest eas-tip-actions-run-on-mouse-1 ()
  ;; mouse-1 is pointerdown + pointerup; the bars are bound to echo.
  (eas-tip-test--with-view v "bars"
    (eas-action-bind v "main/0" "echo")
    (let* ((px (eas-tip-test--centre v "main/0" 0)) (inhibit-message t))
      (eas-dispatch v (list :type "pointerdown" :px px))
      (let ((click (plist-get (eas-dispatch v (list :type "pointerup" :px px)) :click)))
        (should (equal (plist-get click :mark) "main/0"))
        (should (equal (plist-get click :action) "echo"))
        (should (eq (plist-get click :ran) t))
        (should (string-match-p "category: Mon\nvalue: 8200" (plist-get click :result))))
      ;; A press that moves past the click slop is a drag, not a click.
      (eas-dispatch v '(:type "click" :px [0 0]))
      (eas-dispatch v (list :type "pointerdown" :px px))
      (eas-dispatch v (list :type "pointerup" :px (vector (+ 10 (aref px 0)) (aref px 1))))
      (should (eq (plist-get (eas-inspect v) :click) :null)))))

(ert-deftest eas-tip-href-opens-for-bars-and-series ()
  (eas-tip-test--with-view v eas-tip-test--href-spec
    (let* ((opened nil) (eas-action-browse-function (lambda (url) (push url opened))))
      (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-tip-test--centre v "main/0" 1))) :click)))
        (should (equal (plist-get click :href) "https://example.com/b"))
        (should (equal (plist-get click :action) "open-href"))
        (should (equal (plist-get click :result) "https://example.com/b")))
      ;; Off the bars but near the line's first datum: the series hit.
      (let* ((px (eas-tip-test--anchor v "main/1" 0))
             (click (plist-get (eas-dispatch v (list :type "click" :px (vector (aref px 0) (- (aref px 1) 8))))
                               :click)))
        (should (equal (plist-get click :mark) "main/1"))
        (should (equal (plist-get click :href) "https://example.com/a"))
        (should (equal (eas-tip-test--titles (plist-get click :tooltip)) '(("k" . "a") ("v" . "3")))))
      (should (equal opened '("https://example.com/a" "https://example.com/b"))))))

(ert-deftest eas-tip-actions-bind-by-mark-param-and-wildcard ()
  (eas-tip-test--with-view v '(:data (:values [(:k "a" :v 3) (:k "b" :v 5)]) :width 200 :height 100
                                 :params [(:name "pick" :select (:type "point" :on "click"))]
                                 :mark "bar"
                                 :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
    (let ((px (eas-tip-test--centre v "main/0" 0)) (calls nil))
      (eas-action-define "test-note" (lambda (target _view) (push (plist-get target :row) calls) "noted"))
      (eas-action-bind v "*" "copy-row")
      (let ((kill-ring nil))
        (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :action) "copy-row"))
        (should (equal (car kill-ring) "{\"k\":\"a\",\"v\":3}")))
      (eas-action-bind v "pick" "test-note")
      (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :result) "noted"))
      (eas-action-bind v "main/0" "echo")
      (let ((inhibit-message t))
        (should (equal (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :action) "echo")))
      (should (equal calls '((:k "a" :v 3))))
      (should (equal (plist-get (eas-test-should-code "NOT_FOUND" (eas-action-bind v "*" "nope")) :action)
                     "nope"))
      (should (member "open-href" (mapcar (lambda (a) (plist-get a :name)) (eas-action-describe)))))))

(ert-deftest eas-tip-failing-action-is-data-not-an-error ()
  (eas-tip-test--with-view v "bars"
    (eas-action-define "test-boom" (lambda (_target _view) (error "Kaboom")))
    (eas-action-bind v "*" "test-boom")
    (let ((click (plist-get (eas-dispatch v (list :type "click" :px (eas-tip-test--centre v "main/0" 0))) :click)))
      (should (eq (plist-get click :ran) :false))
      (should (equal (plist-get (plist-get click :error) :code) "ENGINE_FAILED"))
      (should (string-match-p "Kaboom" (plist-get (plist-get click :error) :message))))))

(ert-deftest eas-tip-replay-and-inhibit-record-without-running ()
  (eas-tip-test--with-view v "bars"
    (let ((runs 0) (px (eas-tip-test--centre v "main/0" 3)))
      (eas-action-define "test-count" (lambda (_target _view) (cl-incf runs)))
      (eas-action-bind v "*" "test-count")
      (eas-dispatch v (list :type "click" :px px))
      (should (= runs 1))
      (let ((eas-action-inhibit t))
        (should (eq (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :ran) :false)))
      (let ((log (eas-view-log v)))
        (eas-tip-test--with-view w "bars"
          (eas-action-bind w "*" "test-count")
          (let ((click (plist-get (eas-replay w log) :click)))
            (should (equal (plist-get click :action) "test-count"))
            (should (eq (plist-get click :ran) :false))
            (should (equal (plist-get click :row) '(:category "Thu" :value 9300))))))
      (should (= runs 1)))))

;;; Glue: echo area and RET in buffers

(ert-deftest eas-tip-hover-echoes-series-tooltips-in-buffers ()
  (eas-tip-test--with-view v "line"
    (let* ((shown nil)
           (eas-mode-tip-display-function (lambda (text _view) (push text shown))))
      (with-temp-buffer
        (setf (eas-view-buffer v) (current-buffer))
        (eas-dispatch v (list :type "pointermove" :px (eas-tip-test--anchor v "main/0" 1)))
        (eas-dispatch v (list :type "pointermove" :px (eas-tip-test--anchor v "main/0" 1)))
        (eas-dispatch v '(:type "pointerleave"))
        (should (equal (reverse shown) '("date: Mar 03, 2026\nvalue: 104.1" nil))))))
  ;; GUI discrete marks: the :map area's help-echo is Emacs's job.
  (eas-tip-test--with-view v "bars"
    (let* ((shown nil)
           (eas-mode-tip-display-function (lambda (text _view) (push text shown))))
      (with-temp-buffer
        (setf (eas-view-buffer v) (current-buffer))
        (eas-dispatch v (list :type "pointermove" :px (eas-tip-test--centre v "main/0" 0)))
        (should-not shown)
        (eas-view-resize v '(:cols 60 :rows 16) 'text)
        (eas-dispatch v '(:type "pointerleave"))
        (eas-dispatch v (list :type "pointermove" :px (eas-tip-test--centre v "main/0" 0)))
        (should (equal shown '("category: Mon\nvalue: 8200"))))))
  ;; The default display falls back to the echo area off a GUI frame.
  (eas-tip-test--with-view v "line"
    (with-temp-buffer
      (let ((inhibit-message t))
        (eas-mode-tip-display "date: x" v))
      (should (equal (current-message) nil))
      (should eas-mode-tip--shown))))

(ert-deftest eas-tip-ret-at-point-clicks-in-a-text-buffer ()
  (eas-tip-test--with-view v "bars"
    (eas-view-resize v '(:cols 60 :rows 16) 'text)
    (eas-action-bind v "*" "copy-row")
    (with-temp-buffer
      (eas-view-mode)
      (setq eas-mode--view v)
      (setf (eas-view-buffer v) (current-buffer))
      (let ((inhibit-read-only t)) (insert (eas-text-render (eas-view-scene v))))
      (goto-char (point-min))
      (while (not (equal (get-text-property (point) 'eas-datum) 4)) (forward-char 1))
      (let ((kill-ring nil))
        (eas-mode-click-at-point)
        (should (equal (car kill-ring) "{\"category\":\"Fri\",\"value\":12050}")))
      (should (equal (plist-get (plist-get (eas-inspect v) :click) :mark) "main/0")))))

(provide 'eas-tip-test)
;;; eas-tip-test.el ends here

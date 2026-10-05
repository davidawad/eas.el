;;; eas-brush-test.el --- tests for brush select (fc-qx1.4) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-brush-test--spec
  '(:data (:values [(:t "2026-03-01" :p 10) (:t "2026-03-02" :p 12) (:t "2026-03-03" :p 9)
                    (:t "2026-03-04" :p 15) (:t "2026-03-05" :p 14) (:t "2026-03-06" :p 20)])
    :width 300 :height 150
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
    :mark "line"
    :encoding (:x (:field "t" :type "temporal") :y (:field "p" :type "quantitative")))
  "A temporal line with an x brush.")

(defmacro eas-brush-test--with-view (var &rest body)
  "Open the brush spec as VAR in a fresh registry, quiet hooks, and run BODY."
  (declare (indent 1))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-brush-functions nil))
     (let ((,var (eas-view-open eas-brush-test--spec :id "b")))
       ,@body)))

(defun eas-brush-test--px (view date)
  "Scene pixel [X Y] of DATE mid-plot in VIEW."
  (let* ((sv (aref (plist-get (eas-view-scene view) :views) 0))
         (b (plist-get sv :bounds)))
    (vector (eas-scale-apply (plist-get (plist-get sv :scales) :x) date)
            (+ (aref b 1) (/ (aref b 3) 2.0)))))

(defun eas-brush-test--domain (view)
  "VIEW's x domain as reported by inspect."
  (plist-get (plist-get (aref (plist-get (eas-inspect view) :views) 0) :domains) :x))

(ert-deftest eas-brush-drag-summary-counts-the-selection ()
  (eas-brush-test--with-view v
    (eas-dispatch v (list :type "drag" :from (eas-brush-test--px v "2026-03-05T12:00:00Z")
                            :to (eas-brush-test--px v "2026-03-01T12:00:00Z")))
    (let ((s (eas-brush-summary v)))
      (should (equal (plist-get s :param) "brush"))
      (should (equal (plist-get s :field) "p"))
      (should (= (plist-get s :n) 4))
      (should (equal (list (plist-get s :min) (plist-get s :max) (plist-get s :first) (plist-get s :last))
                     '(9 15 12 14)))
      (should (= (plist-get s :change) 2))
      (should (= (plist-get s :change-pct) 16.67))
      (should (string-match-p "\\`brush 2026-03-01T12:00:00Z\\.\\.2026-03-05T12:00:00Z: n=4  p min 9 max 15 first 12 last 14 change 2 (\\+16\\.67%)\\'"
                              (eas-brush-format s))))))

(ert-deftest eas-brush-event-between-is-a-data-space-brush ()
  "The terminal's mark..point becomes one readable brush event, either direction."
  (eas-brush-test--with-view v
    (let* ((scene (eas-view-scene v))
           (a (eas-brush-test--px v "2026-03-02")) (b (eas-brush-test--px v "2026-03-04"))
           (event (eas-brush-event-between scene b a)))
      (should (equal event '(:type "brush" :param "brush" :x ["2026-03-02" "2026-03-04"])))
      (eas-dispatch v event)
      (should (equal (eas-selection v "brush") [(:t "2026-03-02" :p 12) (:t "2026-03-03" :p 9) (:t "2026-03-04" :p 15)]))
      (should (equal (plist-get (aref (eas-view-log-entries v) 0) :summary) "brush x 2026-03-02..2026-03-04"))
      ;; Off the plot's right edge clamps to the domain's end.
      (should (equal (plist-get (eas-brush-event-between scene a (vector 10000 (aref a 1))) :x)
                     ["2026-03-02" "2026-03-06"]))
      (should (equal (plist-get (eas-test-should-code "EVENT_INVALID"
                                  (eas-brush-event-between scene [-50 -50] [-60 -60]))
                                :field)
                     "param")))))

(ert-deftest eas-brush-z-zooms-into-the-brush-and-history-undoes ()
  (eas-brush-test--with-view v
    (should (equal (eas-brush-test--domain v) ["2026-03-01" "2026-03-06"]))
    (eas-dispatch v '(:type "brush" :x ["2026-03-02" "2026-03-04"]))
    (let ((inspect (eas-dispatch v '(:type "key" :key "z"))))
      (should (equal (eas-brush-test--domain v) ["2026-03-02" "2026-03-04"]))
      (should (eq (plist-get (aref (plist-get inspect :views) 0) :zoomed) t))
      (should (= (plist-get (plist-get (aref (plist-get inspect :views) 0) :visible) :n) 3))
      (should-not (plist-get (plist-get (eas-view-state v) :params) :brush))
      (should-not (eas-brush-summary v)))
    ;; Brushing inside the zoom and zooming again narrows further.
    (eas-dispatch v '(:type "brush" :x ["2026-03-03" "2026-03-04"]))
    (eas-dispatch v '(:type "key" :key "z"))
    (should (equal (eas-brush-test--domain v) ["2026-03-03" "2026-03-04"]))
    (eas-dispatch v '(:type "key" :key "["))
    (should (equal (eas-brush-test--domain v) ["2026-03-02" "2026-03-04"]))
    (eas-dispatch v '(:type "key" :key "["))
    (should (equal (eas-brush-test--domain v) ["2026-03-01" "2026-03-06"]))
    ;; z with nothing brushed changes nothing and records no history.
    (let ((state (eas-view-state v)))
      (eas-dispatch v '(:type "key" :key "z"))
      (should (equal (eas-view-state v) state)))))

(ert-deftest eas-brush-emit-sends-rows-org-json-to-echo-kill-ring-or-callback ()
  (eas-brush-test--with-view v
    (should (equal (plist-get (eas-test-should-code "NOT_FOUND" (eas-brush-emit v)) :view) "b"))
    (eas-dispatch v '(:type "brush" :x ["2026-03-05" "2026-03-06"]))
    (should (equal (plist-get (eas-brush-emit v) :selection) [(:t "2026-03-05" :p 14) (:t "2026-03-06" :p 20)]))
    (should (equal (plist-get (eas-brush-emit v :as 'json) :selection)
                   "[{\"t\":\"2026-03-05\",\"p\":14},{\"t\":\"2026-03-06\",\"p\":20}]"))
    (let ((kill-ring nil) (kill-ring-yank-pointer nil) (interprogram-cut-function nil))
      (eas-brush-emit v :to 'kill-ring)
      (should (equal (car kill-ring) "| t | p |\n|---+---|\n| 2026-03-05 | 14 |\n| 2026-03-06 | 20 |\n"))
      (eas-brush-emit v :as 'rows :to 'kill-ring)
      (should (string-prefix-p "[{\"t\":\"2026-03-05\"" (car kill-ring))))
    (let (got)
      (eas-brush-emit v :as "org" :to (lambda (data summary) (setq got (list data summary))))
      (should (string-prefix-p "| t | p |" (car got)))
      (should (equal (plist-get (cadr got) :change-pct) 42.86)))
    (let (echoed)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq echoed (apply #'format fmt args)))))
        (eas-brush-emit v :to 'echo))
      (should (string-match-p "\\`brush 2026-03-05\\.\\.2026-03-06: n=2 " echoed)))
    (eas-test-should-code "INVALID_INPUT" (eas-brush-emit v :to 'printer))))

(ert-deftest eas-brush-functions-run-when-a-brush-changes ()
  (eas-brush-test--with-view v
    (let (calls)
      (setq eas-brush-functions (list (lambda (view param summary)
                                          (push (list (eas-view-id view) param (and summary (plist-get summary :n)))
                                                calls))))
      (eas-dispatch v '(:type "brush" :x ["2026-03-01" "2026-03-02"]))
      (eas-dispatch v '(:type "pointermove" :px [100 60]))
      (eas-dispatch v '(:type "brush" :x ["2026-03-01" "2026-03-03"]))
      (eas-dispatch v '(:type "key" :key "escape"))
      (should (equal (reverse calls) '(("b" "brush" 2) ("b" "brush" 3) ("b" "brush" nil)))))))

(ert-deftest eas-brush-replays-from-the-log ()
  (let ((eas-views (make-hash-table :test 'equal)) (eas-brush-functions nil))
    (let ((a (eas-view-open eas-brush-test--spec :id "a"))
          (b (eas-view-open eas-brush-test--spec :id "b")))
      (eas-dispatch a (list :type "drag" :from (eas-brush-test--px a "2026-03-02")
                              :to (eas-brush-test--px a "2026-03-05")))
      (eas-dispatch a '(:type "key" :key "z"))
      (eas-dispatch a '(:type "brush" :x ["2026-03-03" "2026-03-04"]))
      (eas-replay b (eas-view-log a))
      (should (equal (eas-view-state a) (eas-view-state b)))
      (should (equal (eas-brush-summary a) (eas-brush-summary b)))
      (should (equal (eas-scene-to-json (eas-view-scene a)) (eas-scene-to-json (eas-view-scene b)))))))

(ert-deftest eas-brush-terminal-mark-and-point-brush-then-copy ()
  (let ((eas-views (make-hash-table :test 'equal)) (eas-brush-functions nil))
    (let* ((view (eas-view-open eas-brush-test--spec :id "tty"))
           (buffer (eas-show view 'text)))
      (unwind-protect
          (with-current-buffer buffer
            (should (eq (lookup-key eas-view-mode-map "b") #'eas-brush-region))
            (should-error (let ((mark-ring nil)) (set-marker (mark-marker) nil) (eas-brush-region)) :type 'user-error)
            (let ((from (text-property-any (point-min) (point-max) 'eas-datum 1))
                  (to (text-property-any (point-min) (point-max) 'eas-datum 4)))
              (should (and from to))
              (set-mark from)
              (goto-char to)
              (eas-brush-region)
              (let ((s (eas-brush-summary view)))
                (should (equal (plist-get s :param) "brush"))
                (should (<= 3 (plist-get s :n) 4)))
              (should (text-property-not-all (point-min) (point-max) 'eas-brush nil))
              (let ((kill-ring nil) (kill-ring-yank-pointer nil) (interprogram-cut-function nil)
                    (inhibit-message t))
                (eas-brush-copy)
                (should (string-prefix-p "| t | p |" (car kill-ring))))
              (execute-kbd-macro "z")
              (should (eq (plist-get (aref (plist-get (eas-inspect view) :views) 0) :zoomed) t))
              (should-not (text-property-not-all (point-min) (point-max) 'eas-brush nil))))
        (kill-buffer buffer)))))

(provide 'eas-brush-test)
;;; eas-brush-test.el ends here

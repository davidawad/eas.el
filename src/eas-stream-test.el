;;; eas-stream-test.el --- tests for live streaming (x-eas.stream) -*- lexical-binding: t; -*-

;;; Commentary:

;; Every case runs headlessly: the clock is a variable, timers are off
;; and frames are taken by `eas-stream-tick' at chosen times.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-stream-test--spec
  '(:data (:values [(:t 1 :p 10) (:t 2 :p 12)])
    :width 200 :height 100
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
    :mark "point"
    :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")))
  "Points with an x brush.")

(defvar eas-stream-test--now 0.0 "The test clock.")

(defmacro eas-stream-test--with (var config &rest body)
  "Open the test spec as streamed view VAR with CONFIG in fresh registries."
  (declare (indent 2))
  `(let* ((eas-views (make-hash-table :test 'equal))
          (eas-streams (make-hash-table :test 'equal))
          (eas-stream-use-timers nil)
          (eas-stream-test--now 0.0)
          (eas-stream-clock (lambda () eas-stream-test--now))
          (,var (eas-stream-open eas-stream-test--spec :id "s" :stream ,config)))
     ,@body))

(defun eas-stream-test--at (time)
  "Set the test clock to TIME."
  (setq eas-stream-test--now (float time)))

(defun eas-stream-test--ts (view)
  "The t column of VIEW's rows."
  (seq-map (lambda (r) (plist-get r :t)) (plist-get (eas-view-data view) :rows)))

(defun eas-stream-test--rows (from to)
  "Rows t = FROM..TO."
  (vconcat (mapcar (lambda (i) (list :t i :p (* 2 i))) (number-sequence from to))))

(ert-deftest eas-stream-config-is-validated ()
  (should (equal (eas-stream-check '(:window 10)) (list :max-fps eas-stream-default-max-fps :window 10)))
  (dolist (case '(((:max-fps 0) . "/x-eas/stream/max-fps") ((:max-fps 120) . "/x-eas/stream/max-fps")
                  ((:window -1) . "/x-eas/stream/window") ((:window 2.5) . "/x-eas/stream/window")
                  ((:fps 5) . "/x-eas/stream/fps") ("fast" . "/x-eas/stream")))
    (should (equal (plist-get (eas-test-should-code "INVALID_INPUT" (eas-stream-check (car case))) :path)
                   (cdr case))))
  (should (equal (eas-stream-config "{\"x-eas\": {\"stream\": {\"max-fps\": 2}}, \"mark\": \"point\"}")
                 '(:max-fps 2)))
  (let ((eas-views (make-hash-table :test 'equal)) (eas-streams (make-hash-table :test 'equal)))
    (eas-test-should-code "INVALID_INPUT" (eas-stream-open eas-stream-test--spec :id "plain"))
    (should-not (eas-view-ids))
    (should (eas-stream-get (eas-stream-open (append '(:x-eas (:stream (:window 3))) eas-stream-test--spec)
                                                 :id "own")))))

(ert-deftest eas-stream-push-events-validate-window ()
  (should (equal (plist-get (eas-test-should-code "EVENT_INVALID"
                              (eas-event-parse '(:type "push" :rows [] :window 0)))
                            :field)
                 "window"))
  (should (equal (eas-event-describe '(:type "push" :rows [1 2] :window 9)) "push 2 rows (window 9)")))

(ert-deftest eas-stream-frames-are-capped ()
  (eas-stream-test--with v '(:max-fps 5)
    (eas-push v (eas-stream-test--rows 3 3))
    (should (equal (eas-stream-test--ts v) '(1 2 3)))
    (eas-stream-test--at 0.05)
    (eas-push v (eas-stream-test--rows 4 4))
    (eas-stream-test--at 0.1)
    (let ((inspect (eas-push v (eas-stream-test--rows 5 6))))
      (should (= (plist-get inspect :rows) 3)))
    (should (= (plist-get (eas-stream-inspect v) :queued) 3))
    (should-not (eas-stream-tick v 0.19))
    (should (eas-stream-tick v 0.2))
    (should (equal (eas-stream-test--ts v) '(1 2 3 4 5 6)))
    (let ((s (eas-stream-inspect v)))
      (should (equal (list (plist-get s :frames) (plist-get s :pushes) (plist-get s :queued)) '(2 3 0))))
    ;; Three pushes, two frames: the log holds exactly what was drawn.
    (should (equal (mapcar (lambda (e) (plist-get e :summary)) (eas-view-log-entries v))
                   '("push 1 rows" "push 3 rows")))
    (should-not (eas-stream-tick v 5))))

(ert-deftest eas-stream-window-keeps-the-newest-rows ()
  (eas-stream-test--with v '(:max-fps 10 :window 4)
    (eas-push v (eas-stream-test--rows 3 5))
    (should (equal (eas-stream-test--ts v) '(2 3 4 5)))
    (let ((ends (lambda () (let ((sum (plist-get (aref (plist-get (eas-inspect v) :views) 0) :visible)))
                             (list (plist-get sum :first) (plist-get sum :last))))))
      (should (equal (funcall ends) '(12 10)))
      (eas-stream-test--at 1)
      (eas-push v (eas-stream-test--rows 6 6))
      ;; Same row count, new rows: the scene must still follow.
      (should (equal (eas-stream-test--ts v) '(3 4 5 6)))
      (should (equal (funcall ends) '(6 12))))
    (eas-stream-test--at 1.01)
    (eas-push v (eas-stream-test--rows 7 20))
    (should (equal (eas-stream-test--ts v) '(3 4 5 6)))
    (should (eas-stream-tick v 1.1))
    (should (equal (eas-stream-test--ts v) '(17 18 19 20)))
    (should (equal (plist-get (car (last (append (eas-view-log-entries v) nil))) :summary)
                   "push 4 rows (window 4)"))))

(ert-deftest eas-stream-pauses-while-the-pointer-is-in-the-chart ()
  (eas-stream-test--with v '(:max-fps 5)
    (let ((eas-stream-hover-hold 2.0))
      (eas-stream-test--at 1)
      (eas-dispatch v '(:type "pointermove" :px [100 50]))
      (eas-stream-test--at 1.5)
      (eas-push v (eas-stream-test--rows 3 3))
      (should (equal (plist-get (eas-stream-inspect v) :held) "pointer"))
      (should-not (eas-stream-tick v 2.9))
      (should (equal (eas-stream-test--ts v) '(1 2)))
      ;; Leaving catches up at once.
      (eas-stream-test--at 3)
      (eas-dispatch v '(:type "pointerleave"))
      (should (equal (eas-stream-test--ts v) '(1 2 3)))
      ;; A pointer that stops moving releases the stream after the hold.
      (eas-stream-test--at 10)
      (eas-dispatch v '(:type "pointermove" :px [100 50]))
      (eas-push v (eas-stream-test--rows 4 4))
      (should-not (eas-stream-tick v 11.9))
      (should (eas-stream-tick v 12.0))
      (should (equal (eas-stream-test--ts v) '(1 2 3 4))))
    (let ((eas-stream-hover-hold nil))
      (eas-stream-test--at 20)
      (eas-dispatch v '(:type "pointermove" :px [100 50]))
      (eas-push v (eas-stream-test--rows 5 5))
      (should-not (eas-stream-tick v 1000))
      (should (equal (eas-stream-flush v) (eas-inspect v)))
      (should (equal (eas-stream-test--ts v) '(1 2 3 4 5))))))

(ert-deftest eas-stream-pauses-during-a-brush-then-catches-up ()
  (eas-stream-test--with v '(:max-fps 5)
    (let ((eas-stream-hover-hold 0)
          (x (lambda (value) (eas-scale-apply (plist-get (plist-get (aref (plist-get (eas-view-scene v) :views) 0)
                                                                     :scales)
                                                          :x)
                                               value))))
      (eas-stream-test--at 1)
      (eas-dispatch v (list :type "pointerdown" :px (vector (funcall x 1.2) 50)))
      (eas-dispatch v (list :type "pointermove" :px (vector (funcall x 1.8) 50)))
      (eas-push v (eas-stream-test--rows 3 4))
      (should (equal (plist-get (eas-stream-inspect v) :held) "drag"))
      (should-not (eas-stream-tick v 5))
      (eas-stream-test--at 6)
      (eas-dispatch v (list :type "pointerup" :px (vector (funcall x 1.8) 50)))
      (should (equal (eas-stream-test--ts v) '(1 2 3 4)))
      (should (plist-get (plist-get (eas-view-state v) :params) :brush)))))

(ert-deftest eas-stream-rows-fail-at-push-time ()
  (eas-stream-test--with v '(:max-fps 5)
    (eas-push v (eas-stream-test--rows 3 3))
    (let ((err (eas-test-should-code "SHAPE_INVALID" (eas-push v [(:t 4 :p 1) (:t 5 :q 1)]))))
      (should (equal (list (plist-get err :index) (plist-get err :field)) '(1 "q"))))
    (should (= (plist-get (eas-stream-inspect v) :queued) 0))))

(ert-deftest eas-stream-replays-what-was-drawn ()
  (eas-stream-test--with a '(:max-fps 5 :window 3)
    (dotimes (i 6)
      (eas-stream-test--at (* i 0.07))
      (eas-push a (eas-stream-test--rows (+ 3 i) (+ 3 i))))
    (eas-stream-tick a 1)
    (let ((b (eas-view-open eas-stream-test--spec :id "b")))
      (eas-replay b (eas-view-log a))
      (should (equal (eas-view-data a) (eas-view-data b)))
      (should (equal (eas-scene-to-json (eas-view-scene a)) (eas-scene-to-json (eas-view-scene b)))))))

(ert-deftest eas-stream-template-attaches-on-first-push ()
  (let ((eas--templates (progn (eas-template-names) (copy-sequence eas--templates)))
        (eas-views (make-hash-table :test 'equal))
        (eas-streams (make-hash-table :test 'equal))
        (eas-stream-use-timers nil)
        (eas-stream-test--now 0.0)
        (eas-stream-clock (lambda () eas-stream-test--now)))
    (eas-template-register
     '(:x-eas (:template "test-stream" :version "1.0.0"
                 :slots (:data (:shape "plist" :required t))
                 :stream (:max-fps 2 :window 2))
       :data (:name "data") :mark "line"
       :encoding (:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative"))))
    (should (equal (eas-stream-config "test-stream") '(:max-fps 2 :window 2)))
    (let ((v (eas-view-open "test-stream" :bindings (list :data (eas-stream-test--rows 1 2)))))
      (should-not (eas-stream-get v))
      (eas-push v (eas-stream-test--rows 3 3))
      (should (equal (plist-get (eas-stream-inspect v) :window) 2))
      (should (equal (eas-stream-test--ts v) '(2 3)))
      (eas-stream-detach v)
      (should (eq (eas-stream-inspect v) :null))
      (eas-stream-attach v)
      (eas-view-close v)
      (should-not (eas-stream-get (eas-view-open "test-stream" :bindings (list :data (eas-stream-test--rows 1 2))))))
    (let ((plain (eas-view-open "line" :bindings (eas-template-example "line"))))
      (should (= (plist-get (eas-push plain [(:date "2026-02-01" :value 1)]) :rows)
                 (1+ (length (plist-get (eas-template-example "line") :data)))))
      (should-not (eas-stream-get plain)))
    (should (equal (plist-get (plist-get (eas-describe 'stream) :stream) :key) "x-eas.stream"))))

(ert-deftest eas-stream-timers-take-the-deferred-frame ()
  (let ((eas-views (make-hash-table :test 'equal))
        (eas-streams (make-hash-table :test 'equal)))
    (let ((v (eas-stream-open eas-stream-test--spec :id "live" :stream '(:max-fps 20))))
      ;; Both pushes must land within one 50 ms frame: a collection
      ;; pause between them is not what this test measures.
      (let ((gc-cons-threshold most-positive-fixnum))
        (eas-push v (eas-stream-test--rows 3 3))
        (eas-push v (eas-stream-test--rows 4 4)))
      (should (= (plist-get (eas-stream-inspect v) :queued) 1))
      (with-timeout (2 (ert-fail "the deferred frame never ran"))
        (while (> (plist-get (eas-stream-inspect v) :queued) 0) (sleep-for 0.01)))
      (should (equal (eas-stream-test--ts v) '(1 2 3 4))))))

(provide 'eas-stream-test)
;;; eas-stream-test.el ends here

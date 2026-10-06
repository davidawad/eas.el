;;; eas-keyed-test.el --- tests for keyed pushes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Keyed pushes replace or delete rows by a key column, directly and
;; through the stream coalescer.  Everything runs headlessly with the
;; stream test clock.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-keyed-test--spec
  '(:data (:values [(:price 100 :size 5 :side "bid") (:price 101 :size 3 :side "ask")])
    :width 200 :height 100
    :params [(:name "brush" :select (:type "interval" :encodings ["x"]))]
    :mark "bar"
    :encoding (:x (:field "price" :type "quantitative") :y (:field "size" :type "quantitative")
               :color (:field "side" :type "nominal")))
  "A two-level book.")

(defvar eas-keyed-test--now 0.0 "The test clock.")

(defun eas-keyed-test--at (time)
  "Set the test clock to TIME."
  (setq eas-keyed-test--now (float time)))

(defun eas-keyed-test--book (view)
  "VIEW's rows as (PRICE . SIZE) pairs."
  (seq-map (lambda (r) (cons (plist-get r :price) (plist-get r :size)))
           (plist-get (eas-view-data view) :rows)))

(defmacro eas-keyed-test--with (var config &rest body)
  "Open the book spec as view VAR, streamed with CONFIG when non-nil."
  (declare (indent 2))
  `(let* ((eas-views (make-hash-table :test 'equal))
          (eas-streams (make-hash-table :test 'equal))
          (eas-stream-use-timers nil)
          (eas-keyed-test--now 0.0)
          (eas-stream-clock (lambda () eas-keyed-test--now))
          (,var (if ,config (eas-stream-open eas-keyed-test--spec :id "k" :stream ,config)
                  (eas-view-open eas-keyed-test--spec :id "k"))))
     ,@body))

(ert-deftest eas-keyed-apply-replaces-appends-and-deletes ()
  (let ((data (eas-data-make [(:k "a" :v 1) (:k "b" :v 2) (:k "c" :v 3)]
                             [(:name "k" :type "nominal") (:name "v" :type "quantitative")])))
    (should (equal (plist-get (eas-keyed-apply data [(:k "b" :v 20) (:k "d" :v 4) (:k "a" :_eas_delete t)
                                                     (:k "b" :v 21) (:k "z" :_eas_delete t)]
                                               "k")
                              :rows)
                   [(:k "b" :v 21) (:k "c" :v 3) (:k "d" :v 4)]))
    (should (equal (eas-keyed-coalesce [(:k "a" :v 1) (:k "b" :v 2) (:k "a" :v 3)] "k")
                   [(:k "a" :v 3) (:k "b" :v 2)]))
    ;; False markers are dropped; deletes need only the key.
    (should (equal (eas-keyed-check data [(:k "a" :v 1 :_eas_delete :false) (:k "b" :_eas_delete t)] "k")
                   [(:k "a" :v 1) (:k "b" :_eas_delete t)]))
    (should (equal (plist-get (eas-test-should-code "EVENT_INVALID" (eas-keyed-check data [] "nope")) :field)
                   "key"))
    (let ((err (eas-test-should-code "SHAPE_INVALID" (eas-keyed-check data [(:k "a" :v 1) (:v 2)] "k"))))
      (should (equal (list (plist-get err :index) (plist-get err :field)) '(1 "k"))))
    (let ((err (eas-test-should-code "SHAPE_INVALID" (eas-keyed-check data [(:k "a" :v "x")] "k"))))
      (should (equal (list (plist-get err :index) (plist-get err :field)) '(0 "v"))))
    (eas-test-should-code "SHAPE_INVALID" (eas-keyed-check data [(:k "a" :_eas_delete 1)] "k"))
    ;; A plain push knows no delete marker.
    (eas-test-should-code "SHAPE_INVALID" (eas-keyed-check data [(:k "a" :_eas_delete t)] nil))))

(ert-deftest eas-keyed-events-validate-and-describe ()
  (should (equal (plist-get (eas-test-should-code "EVENT_INVALID"
                              (eas-event-parse '(:type "push" :rows [] :key 3)))
                            :field)
                 "key"))
  (should (equal (eas-event-describe '(:type "push" :rows [1 2] :key "price" :window 9))
                 "push 2 rows by price (window 9)")))

(ert-deftest eas-keyed-push-updates-a-view-in-place ()
  (eas-keyed-test--with v nil
    (eas-push v [(:price 101 :size 7 :side "ask") (:price 102 :size 1 :side "ask")] :key "price")
    (should (equal (eas-keyed-test--book v) '((100 . 5) (101 . 7) (102 . 1))))
    (let ((inspect (eas-push-delete v "price" '(100))))
      (should (= (plist-get inspect :rows) 2)))
    (should (equal (eas-keyed-test--book v) '((101 . 7) (102 . 1))))
    (should (equal (mapcar (lambda (e) (plist-get e :summary)) (eas-view-log-entries v))
                   '("push 2 rows by price" "push 1 rows by price")))
    ;; The scene follows a replace that keeps the row count.
    (let ((before (eas-scene-to-json (eas-view-scene v))))
      (eas-push v [(:price 102 :size 9 :side "ask")] :key "price")
      (should (equal (eas-keyed-test--book v) '((101 . 7) (102 . 9))))
      (should-not (equal before (eas-scene-to-json (eas-view-scene v)))))
    (let ((b (eas-view-open eas-keyed-test--spec :id "b")))
      (eas-replay b (eas-view-log v))
      (should (equal (eas-view-data v) (eas-view-data b)))
      (should (equal (eas-scene-to-json (eas-view-scene v)) (eas-scene-to-json (eas-view-scene b)))))
    (eas-test-should-code "SHAPE_INVALID" (eas-push v [(:size 1)] :key "price"))))

(ert-deftest eas-keyed-stream-keeps-the-latest-row-per-key ()
  (eas-keyed-test--with v '(:max-fps 5)
    (eas-push v [(:price 100 :size 6 :side "bid")] :key "price")
    (should (equal (eas-keyed-test--book v) '((100 . 6) (101 . 3))))
    (eas-keyed-test--at 0.05)
    (dotimes (i 50)
      (eas-push v (vector (list :price 101 :size i :side "ask")) :key "price"))
    (eas-push v [(:price 99 :size 2 :side "bid")] :key "price")
    (eas-push-delete v "price" '(100))
    ;; Fifty deltas to one level queue as one row.
    (should (= (plist-get (eas-stream-inspect v) :queued) 3))
    (should-not (eas-stream-tick v 0.1))
    (should (eas-stream-tick v 0.2))
    (should (equal (eas-keyed-test--book v) '((101 . 49) (99 . 2))))
    (should (equal (mapcar (lambda (e) (plist-get e :summary)) (eas-view-log-entries v))
                   '("push 1 rows by price" "push 3 rows by price")))
    ;; Schema failures reach the caller and queue nothing.
    (eas-keyed-test--at 1)
    (eas-test-should-code "SHAPE_INVALID" (eas-push v [(:price 98 :size "big")] :key "price"))
    (should (= (plist-get (eas-stream-inspect v) :queued) 0))))

(ert-deftest eas-keyed-stream-mixes-plain-pushes-in-order ()
  (eas-keyed-test--with v '(:max-fps 5 :window 3)
    (eas-push v [(:price 102 :size 1 :side "ask")])
    (eas-keyed-test--at 0.05)
    (eas-push v [(:price 101 :size 8 :side "ask")] :key "price")
    (eas-push v [(:price 101 :size 9 :side "ask")] :key "price")
    (eas-push v [(:price 103 :size 4 :side "ask")])
    (should (= (plist-get (eas-stream-inspect v) :queued) 2))
    (should (eas-stream-tick v 0.2))
    ;; One frame, two batches, the window applied after each.
    (should (equal (eas-keyed-test--book v) '((101 . 9) (102 . 1) (103 . 4))))
    (should (equal (mapcar (lambda (e) (plist-get e :summary)) (eas-view-log-entries v))
                   '("push 1 rows (window 3)" "push 1 rows by price (window 3)" "push 1 rows (window 3)")))
    (should (= (plist-get (eas-stream-inspect v) :frames) 2))))

(ert-deftest eas-keyed-stream-pauses-under-the-pointer ()
  (eas-keyed-test--with v '(:max-fps 5)
    (let ((eas-stream-hover-hold 2.0))
      (eas-keyed-test--at 1)
      (eas-dispatch v '(:type "pointermove" :px [100 50]))
      (dotimes (i 10)
        (eas-push v (vector (list :price 100 :size i :side "bid")) :key "price"))
      (should (equal (plist-get (eas-stream-inspect v) :held) "pointer"))
      (should (= (plist-get (eas-stream-inspect v) :queued) 1))
      (should-not (eas-stream-tick v 2.9))
      (should (equal (eas-keyed-test--book v) '((100 . 5) (101 . 3))))
      (eas-keyed-test--at 3)
      (eas-dispatch v '(:type "pointerleave"))
      (should (equal (eas-keyed-test--book v) '((100 . 9) (101 . 3)))))))

(provide 'eas-keyed-test)
;;; eas-keyed-test.el ends here

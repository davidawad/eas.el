;;; eas-live-test.el --- invisible views, frame budget, incremental push -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.19: a hidden view takes no frames and catches up once when
;; shown; a slow view skips ticks; a push patches the compile plan
;; (eas-compile-rows.el) to the scene a full compile makes, for less;
;; a text redraw rewrites only the lines that changed.

;;; Code:

(require 'ert)
(require 'eas-live)
(require 'eas-stream)
(require 'eas-play)
(require 'eas-compile-rows)
(require 'eas-mode)
(require 'eas-text)

(defun eas-live-test--book (seed levels)
  "A LEVELS-per-side order book as rows, sizes drawn from SEED."
  (let ((state seed))
    (vconcat
     (cl-loop for side in '("ask" "bid")
              append (cl-loop for i below levels
                              do (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                              collect (list :side side :level i
                                            :price (if (equal side "ask") (+ 100.01 (* 0.01 i))
                                                     (- 99.99 (* 0.01 i)))
                                            :size (+ 1 (% (/ state 7) 20))))))))

(defconst eas-live-test--ladder
  (list :width 400 :height 300
        :encoding (list :y (list :field "price" :type "ordinal" :sort "descending"
                                 :axis (list :format ".2f")))
        :layer (vector (list :mark (list :type "bar")
                             :encoding (list :x (list :field "size" :type "quantitative"
                                                      :scale (list :domain [0 40]))
                                             :color (list :field "side" :type "nominal")))
                       (list :mark (list :type "text" :align "left")
                             :encoding (list :x (list :field "size" :type "quantitative")
                                             :text (list :field "size" :type "quantitative")))))
  "A 2x25-level ladder: fixed size domain, one band per price.")

(defmacro eas-live-test--with (&rest body)
  "Run BODY with fresh registries, no timers and a stepped clock `clock'."
  (declare (indent 0) (debug t))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-streams (make-hash-table :test 'equal))
         (eas-plays (make-hash-table :test 'equal))
         (eas-live--hidden (make-hash-table :test 'equal))
         (eas-live--costs (make-hash-table :test 'equal))
         (eas-live--starts (make-hash-table :test 'equal))
         (eas-stream-use-timers nil)
         (clock 100.0))
     (let ((eas-stream-clock (lambda () clock))
           (eas-play-clock (lambda () clock)))
       ,@body)))

(defun eas-live-test--compiles (thunk)
  "Call THUNK; return how many times a view compiled meanwhile."
  (let* ((n 0) (count (lambda (&rest _) (cl-incf n))))
    (advice-add 'eas-view--compile :before count)
    (unwind-protect (funcall thunk) (advice-remove 'eas-view--compile count))
    n))

(ert-deftest eas-live-hidden-stream-takes-no-frames ()
  "A hidden view does zero compiles over N pushes, then draws the latest once."
  (eas-live-test--with
    (let* ((visible nil)
           (eas-live-visible-function (lambda (_) visible))
           (view (eas-view-open (eas-json-encode eas-live-test--ladder) :id "book" :size '(400 . 300)
                                :rows (eas-live-test--book 1 25)))
           (stream (eas-stream-attach view '(:max-fps 4 :window 50))))
      (should (= 0 (eas-live-test--compiles
                    (lambda ()
                      (dotimes (i 40)
                        (setq clock (+ clock 1.0))
                        (eas-push view (eas-live-test--book (+ 2 i) 25)))))))
      ;; Pushes coalesce into one bounded batch.
      (should (= (eas-stream-queued stream) 50))
      (should (= (eas-stream-frames stream) 0))
      (should (gethash "book" eas-live--hidden))
      (setq visible t)
      (should (= 1 (eas-live-test--compiles #'eas-live-wake)))
      (should (= (eas-stream-frames stream) 1))
      (should (equal (plist-get (eas-view-data view) :rows) (eas-live-test--book 41 25)))
      (should-not (gethash "book" eas-live--hidden)))))

(ert-deftest eas-live-hidden-play-pauses-its-timer ()
  "A hidden play cancels its timer and restarts it when shown."
  (eas-live-test--with
    (let* ((visible nil)
           (eas-play-visible-function (lambda (_) visible))
           (eas-live-visible-function (lambda (_) visible))
           (view (eas-view-open "clock" :bindings (eas-template-example "clock") :id "clock"))
           (play (eas-play-attach view)))
      (eas-play-start view)
      (should (eas-play-timer play))
      (should (= 0 (eas-live-test--compiles (lambda () (eas-play--timer-fire "clock")))))
      (should-not (eas-play-timer play))
      (should (= (eas-play-skipped play) 1))
      (setq visible t)
      (eas-live-wake)
      (should (eas-play-timer play))
      (eas-play-stop view))))

(ert-deftest eas-live-slow-frames-skip-ticks ()
  "A tick that took longer than the interval skips ticks until it is paid."
  (eas-live-test--with
    (let* ((eas-play-visible-function #'always)
           (view (eas-view-open "clock" :bindings (eas-template-example "clock") :id "clock"))
           (play (eas-play-attach view)))
      ;; The last tick started at 100 and took 3.5 s of a 1 s interval.
      (puthash "clock" 100.0 eas-live--starts)
      (puthash "clock" 3.5 eas-live--costs)
      (dolist (at '(101.0 102.0 103.0))
        (setq clock at)
        (eas-play--timer-fire "clock"))
      (should (= (eas-play-ticks play) 0))
      (should (= (eas-play-skipped play) 3))
      (setq clock 104.0)
      (eas-play--timer-fire "clock")
      (should (= (eas-play-ticks play) 1))
      ;; A stream's next frame waits out a slow frame too.
      (puthash "clock" 2.0 eas-live--costs)
      (should (= (eas-live-interval "clock" 0.25) 2.0))
      (should (= (eas-live-interval "other" 0.25) 0.25)))))

(defun eas-live-test--full (view)
  "The scene a full compile makes of VIEW's spec, rows, size and state."
  (let ((state (eas-view-state view)))
    (eas-params-with-state state
      (eas-compile-scene (eas-compile-plan (eas-view-spec view) :rows (plist-get (eas-view-data view) :rows)
                                           :size (eas-view-size view) :target (eas-view-target view)
                                           :state state)
                         state))))

(ert-deftest eas-live-incremental-push-equals-full-compile ()
  "Over random push sequences, the patched scene equals a full compile."
  (let ((specs (list eas-live-test--ladder
                     ;; A free domain: most pushes move it and compile in full.
                     (list :width 300 :height 200 :mark "line"
                           :encoding (list :x (list :field "level" :type "quantitative")
                                           :y (list :field "size" :type "quantitative")
                                           :color (list :field "side" :type "nominal")))
                     (list :width 300 :height 200 :mark "point"
                           :encoding (list :x (list :field "price" :type "quantitative" :scale (list :zero :false))
                                           :y (list :field "size" :type "quantitative" :scale (list :domain [0 25]))
                                           :tooltip (list :field "size" :type "quantitative"))))))
    (eas-live-test--with
      (let ((eas-compile-rows-stats (list :patched 0 :full 0 :units 0 :reused 0)))
        (cl-loop
         for spec in specs for k from 0 do
         (dolist (target '(svg text))
           (let* ((size (if (eq target 'text) '(:cols 80 :rows 30) '(400 . 300)))
                  (view (eas-view-open (eas-json-encode spec) :id (format "p%d" k) :size size :target target
                                       :rows (eas-live-test--book 1 10)))
                  (seed (+ 7 k)))
             (dotimes (_ 15)
               (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
               (let* ((rows (eas-live-test--book seed 10))
                      ;; Sometimes only a few levels change, sometimes none.
                      (rows (pcase (% seed 3)
                              (0 rows)
                              (1 (let ((old (copy-sequence (plist-get (eas-view-data view) :rows))))
                                   (aset old (% seed 20) (aref rows (% seed 20)))
                                   old))
                              (_ (plist-get (eas-view-data view) :rows)))))
                 (eas-dispatch view (list :type "push" :rows rows :window 20))
                 (should (equal (eas-view-scene view) (eas-live-test--full view)))
                 (should (equal (eas-text-render (eas-view-scene view))
                                (eas-text-render (eas-live-test--full view))))))
             (eas-view-close view))))
        (should (> (plist-get eas-compile-rows-stats :patched) 20))
        (should (> (plist-get eas-compile-rows-stats :reused) 0))))))

(ert-deftest eas-live-order-book-push-recomputes-5x-less ()
  "A 25-level order-book push costs at least 5x less than a full compile.
Each push updates 10 levels.  Cost is what the work allocates, with
time (best of several alternating runs) as a looser guard."
  (eas-live-test--with
    (let* ((view (eas-view-open (eas-json-encode eas-live-test--ladder) :id "book" :size '(400 . 300)
                                :rows (eas-live-test--book 1 25)))
           (gc-cons-threshold (* 64 1024 1024))
           (allocated (lambda () (apply #'+ (memory-use-counts))))
           (timed (lambda (fn)
                    (garbage-collect)
                    (let ((t0 (float-time)) (a0 (funcall allocated)))
                      (dotimes (i 10) (funcall fn i))
                      (cons (- (float-time) t0) (- (funcall allocated) a0)))))
           (seed 1)
           ;; Each frame carries 10 level updates, as a feed's delta batch;
           ;; the feed's rows are made before timing, only the push is timed.
           (frames (let ((rows (plist-get (eas-view-data view) :rows)))
                     (cl-loop for f below 50
                              collect (let ((fresh (eas-live-test--book (+ 2 f) 25)))
                                        (setq rows (copy-sequence rows))
                                        (dotimes (_ 10)
                                          (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
                                          (aset rows (% seed 50) (aref fresh (% seed 50))))
                                        rows))))
           (push-fn (lambda (_) (eas-dispatch view (list :type "push" :rows (pop frames) :window 50))))
           (full-fn (lambda (_) (eas-live-test--full view)))
           (patched 1e9) (full 1e9) (patched-alloc 0) (full-alloc 0))
      ;; Rounds alternate, so a change in machine load hits both sides.
      (dotimes (_ 5)
        (let ((p (funcall timed push-fn)) (f (funcall timed full-fn)))
          (setq patched (min patched (car p)) full (min full (car f))
                patched-alloc (+ patched-alloc (cdr p)) full-alloc (+ full-alloc (cdr f)))))
      (message "order-book push: full %.2f ms, patched %.2f ms, %.1fx; allocation %.1fx"
               (* 100 full) (* 100 patched) (/ full patched) (/ (float full-alloc) patched-alloc))
      ;; Allocation counts the work done whatever else the machine runs;
      ;; time, which earlier tests' warm caches shift, only guards.
      (should (>= (/ (float full-alloc) patched-alloc) 5.0))
      (should (>= (/ full patched) 4.0)))))

(ert-deftest eas-live-text-redraw-touches-only-changed-lines ()
  "A text redraw after a one-level push rewrites only the lines that changed."
  (eas-live-test--with
    (let* ((rows (eas-live-test--book 1 25))
           (view (eas-view-open (eas-json-encode eas-live-test--ladder) :id "book" :target 'text
                                :size '(:cols 80 :rows 60) :rows rows))
           (touched nil))
      (with-temp-buffer
        (eas-mode-patch-text (eas-text-render (eas-view-scene view)))
        (let ((before (split-string (buffer-string) "\n")))
          (let ((next (copy-sequence rows)))
            (aset next 3 (plist-put (copy-sequence (aref next 3)) :size 19))
            (eas-dispatch view (list :type "push" :rows next :window 50)))
          (let ((after-change-functions
                 (list (lambda (beg end _) (push (cons (line-number-at-pos beg) (line-number-at-pos end))
                                                 touched)))))
            (eas-mode-patch-text (eas-text-render (eas-view-scene view))))
          (let* ((now (split-string (buffer-string) "\n"))
                 (changed (cl-loop for a in before for b in now for i from 1
                                   unless (equal a b) collect i)))
            (should changed)
            (should (< (length changed) 5))
            (dolist (span touched)
              (should (member (car span) changed))
              (should (member (cdr span) changed)))))))))

(provide 'eas-live-test)
;;; eas-live-test.el ends here

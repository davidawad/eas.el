;;; eas-bench-test.el --- tests for the performance bead (fc-qx1.9) -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-bench)
(require 'eas-agent)

;; The calibration's best-of-five matters for CI numbers, not for tests.
(setq eas-bench-calibration-runs 1)

;;; Indexed param filters agree with the row-by-row test

(defconst eas-bench-test--values
  [1 1.0 2 -0.0 0 "2026-01-02" "2026-01-02T00:00:00Z" 1767312000000 "abc" "2" :null 2.5]
  "Values that `eas-params--same' treats as equal in non-obvious ways.")

(defun eas-bench-test--rows ()
  "Rows mixing numbers, floats, dates and strings in fields a and b."
  (let ((vs eas-bench-test--values))
    (vconcat (cl-loop for i below 36
                      collect (list :a (aref vs (% i (length vs))) :b (aref vs (% (* i 7) (length vs)))
                                    :_eas_row i)))))

(defun eas-bench-test--stores ()
  "Point stores over several field sets, and an interval store."
  (append
   (cl-loop for v across eas-bench-test--values
            collect (list :type "point" :fields ["a"] :values (vector (vector v))))
   (list (list :type "point" :fields ["a" "b"] :values [[1 "2026-01-02"] [2.5 :null]])
         (list :type "point" :fields ["_eas_row"] :values [[3] [41] [3]])
         (list :type "interval" :fields '(:x "a") :x [0 2]))))

(ert-deftest eas-bench-index-filter-equals-row-test ()
  (let ((rows (eas-bench-test--rows)))
    (dolist (store (eas-bench-test--stores))
      (let ((indexed (eas-params-index-filter store rows nil))
            (scanned (vconcat (seq-filter (lambda (r) (eas-params-contains store r)) rows))))
        (if (equal (plist-get store :type) "interval")
            (should-not indexed)
          (should (equal indexed scanned)))))
    (should (equal (eas-params-index-filter nil rows t) rows))
    (should (equal (eas-params-index-filter nil rows nil) []))))

(ert-deftest eas-bench-index-falls-back-past-exact-floats ()
  (let ((rows (vector (list :a (1+ (expt 2 60))) (list :a (expt 2 60)))))
    (should-not (eas-params-index-filter '(:type "point" :fields ["a"] :values [[1]]) rows nil))
    (should (eq (eas-params-index-changed rows '(("p" . nil)) nil
                                            '(:params (:p (:type "point" :fields ["a"] :values [[1]]))))
                'all))))

(ert-deftest eas-bench-index-changed-covers-every-membership-change ()
  (let ((rows (eas-bench-test--rows)) (stores (cons nil (butlast (eas-bench-test--stores)))))
    (dolist (old stores)
      (dolist (new stores)
        (dolist (empty '(nil t))
          (let* ((so (and old (list :params (list :p old)))) (sn (and new (list :params (list :p new))))
                 (changed (eas-params-index-changed rows (list (cons "p" empty)) so sn)))
            (dotimes (i (length rows))
              (unless (eq (not (eas-params-test so "p" (aref rows i) empty))
                          (not (eas-params-test sn "p" (aref rows i) empty)))
                (should (or (eq changed 'all) (memq i changed)))))))))))

;;; Hover patches equal full compiles at ladder scale

(defun eas-bench-test--zoomable (spec)
  "SPEC with an x zoom (an interval bound to scales) at the top."
  (let ((spec (copy-sequence spec)))
    (plist-put spec :params (vconcat [(:name "zoom" :select (:type "interval" :encodings ["x"]) :bind "scales")]
                                     (plist-get spec :params)))))

(ert-deftest eas-bench-hover-patches-equal-full-compiles ()
  (let* ((rows (eas-bench-rows 400))
         (dated (vconcat (seq-map (lambda (r) (list :t (eas-time-iso (* 86400000 (plist-get r :t)))
                                                    :p (plist-get r :p)))
                                  rows)))
         (cond-spec (list :data (list :values rows)
                          :params [(:name "h" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
                          :mark "point"
                          :encoding '(:x (:field "t" :type "quantitative") :y (:field "p" :type "quantitative")
                                      :size (:condition (:param "h" :empty :false :value 80) :value 10))))
         (date-spec (eas-bench-line-spec dated)))
    (setf (plist-get (plist-get (aref (plist-get date-spec :layer) 0) :encoding) :x)
          '(:field "t" :type "temporal"))
    (setf (plist-get (plist-get (aref (plist-get date-spec :layer) 1) :encoding) :x)
          '(:field "t" :type "temporal"))
    (dolist (spec (mapcar #'eas-bench-test--zoomable (list (eas-bench-line-spec rows) cond-spec date-spec)))
      (let* ((eas-views (make-hash-table :test 'equal))
             (v (eas-view-open spec :id "b" :size '(400 . 200))))
        (eas-replay v (vconcat (cl-loop for x from 50 to 380 by 47
                                          collect (list :type "pointermove" :px (vector x 100)))
                                 '((:type "key" :key "+") (:type "pointermove" :px [200 100])
                                   (:type "pointerleave"))))
        (dolist (x '(90 91 300))
          (eas-dispatch v (list :type "pointermove" :px (vector x 100)))
          (should (equal (eas-scene-to-json (eas-view-scene v))
                         (eas-scene-to-json
                          (eas-params-with-state (eas-view-state v)
                            (eas-compile (eas-view-spec v) :size '(400 . 200)
                                           :state (eas-view-state v)))))))
        ;; The crosshair param holds the last move, and "+" zoomed.
        (should (seq-some (lambda (p) (and (not (equal (plist-get p :name) "zoom"))
                                           (plist-get (plist-get (eas-view-state v) :params)
                                                      (eas-key (plist-get p :name)))))
                          (eas-params-of (eas-view-scene v))))
        (should (plist-get (eas-view-state v) :domains))))))

(ert-deftest eas-bench-crosshair-rule-follows-hover ()
  (let* ((eas-views (make-hash-table :test 'equal))
         (v (eas-view-open (eas-bench-line-spec (eas-bench-rows 2000)) :id "c" :size '(800 . 400))))
    (dolist (x '(100 400 700))
      (eas-dispatch v (list :type "pointermove" :px (vector x 200)))
      (let* ((scene (eas-view-scene v))
             (rule (aref (plist-get (aref (plist-get scene :views) 0) :marks) 1))
             (hover (plist-get (eas-inspect v) :hover)))
        (should (= (length (plist-get rule :items)) 1))
        (should (equal (plist-get (aref (plist-get rule :rows) 0) :t)
                       (plist-get (plist-get hover :row) :t)))))))

;;; Grid hit-test equals the linear scan

(ert-deftest eas-bench-grid-hit-equals-scan ()
  (let* ((rows (vconcat (cl-loop for i below 800
                                 collect (list :t (% (* i 7919) 300) :p (% (* i 104729) 97)))))
         (scene (eas-compile (eas-bench-points-spec rows) :size '(400 . 300)))
         (mark (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0)))
    (should (equal (plist-get (plist-get mark :index) :kind) "grid"))
    (dotimes (k 100)
      (let ((px (- (* (% (* k 7) 47) 10.0) 30)) (py (- (* (% (* k 13) 37) 10.0) 20)))
        (dolist (x-only '(nil t))
          (should (equal (eas-hit-mark mark px py x-only) (eas-hit--scan mark px py x-only))))))))

;;; Visible summary cache

(ert-deftest eas-bench-visible-summary-follows-zoom ()
  (let* ((eas-views (make-hash-table :test 'equal))
         (v (eas-view-open (eas-bench-test--zoomable (eas-bench-line-spec (eas-bench-rows 500)))
                             :id "s" :size '(800 . 400)))
         (n (lambda () (plist-get (plist-get (aref (plist-get (eas-inspect v) :views) 0) :visible) :n))))
    (should (= (funcall n) 500))
    (eas-dispatch v '(:type "pointermove" :px [300 200]))
    (should (= (funcall n) 500))
    (eas-dispatch v '(:type "key" :key "+"))
    (let ((zoomed (funcall n)))
      (should (< zoomed 500))
      (should (equal (eas-view--visible-summary (aref (plist-get (eas-view-scene v) :views) 0))
                     (let ((sv (aref (plist-get (eas-view-scene v) :views) 0)))
                       (eas-view--summarize (plist-get (plist-get sv :scales) :x)
                                              (plist-get (plist-get sv :scales) :y)
                                              (aref (plist-get sv :marks) 0))))))
    (eas-dispatch v '(:type "key" :key "0"))
    (should (= (funcall n) 500))))

;;; GC deferral

(ert-deftest eas-bench-gc-defers-until-idle-and-restores ()
  (let ((gc-cons-threshold 800000) (noninteractive nil)
        (eas-gc-cons-threshold (* 64 1024 1024)) (eas-gc--saved nil) (eas-gc--timer nil))
    (unwind-protect
        (progn
          (eas-gc-defer)
          (should (= gc-cons-threshold (* 64 1024 1024)))
          (should (timerp eas-gc--timer))
          (let ((timer eas-gc--timer))
            (eas-gc-defer)
            (should (eq eas-gc--timer timer)))
          (eas-gc-collect)
          (should (= gc-cons-threshold 800000))
          (should-not eas-gc--timer)
          ;; Someone else changed it meanwhile: leave theirs.
          (eas-gc-defer)
          (setq gc-cons-threshold 123456789)
          (eas-gc-collect)
          (should (= gc-cons-threshold 123456789))
          ;; A larger user value is never lowered; nil leaves GC alone.
          (eas-gc-defer)
          (should (= gc-cons-threshold 123456789))
          (should-not eas-gc--saved)
          (let ((eas-gc-cons-threshold nil) (gc-cons-threshold 800000))
            (eas-gc-defer)
            (should (= gc-cons-threshold 800000))))
      (when (timerp eas-gc--timer) (cancel-timer eas-gc--timer)))))

(ert-deftest eas-bench-gc-leaves-batch-alone ()
  (let ((gc-cons-threshold 800000) (noninteractive t) (eas-gc--saved nil) (eas-gc--timer nil))
    (eas-gc-defer)
    (should (= gc-cons-threshold 800000))
    (should-not eas-gc--timer)))

;;; The ladder, the budget and the verb

(defconst eas-bench-test--result
  '(:contract "eas-bench/v1" :compiled t :calibration-ms 20.0
    :ladder [(:points 1000 :hover (:mean 1.0) :compile-svg (:mean 10.0))
             (:points 10000 :hover (:mean 30.0) :compile-svg (:mean 100.0))])
  "A synthetic ladder result.")

(defconst eas-bench-test--budget
  '(:compiled t :calibration-ms 10.0 :tolerance 2.0 :floor-ms 1
    :targets [(:points 10000 :stage "hover" :ms 50) (:points 100000 :stage "hover" :ms 50)]
    :reference [(:points 1000 :stages (:hover 0.2 :compile-svg 2.0))
                (:points 10000 :stages (:hover 10.0 :compile-svg 30.0))])
  "A budget for it: the result's machine is twice as slow.")

(ert-deftest eas-bench-check-scales-limits-by-calibration ()
  (let ((v (eas-bench-check eas-bench-test--result eas-bench-test--budget)))
    (should (equal (plist-get v :status) "fail"))
    (should (= (plist-get v :factor) 2.0))
    (should (= (plist-get v :checked) 4))
    ;; 1k compile 10 > 2x2x2 = 8; 10k compile 100 > 120? no; hover 30 < 40; 1k hover 1.0 < floor 2.
    (should (equal (mapcar (lambda (x) (list (plist-get x :points) (plist-get x :stage) (plist-get x :limit)))
                           (plist-get v :violations))
                   '((1000 "compile-svg" 8.0))))
    (should (equal (mapcar (lambda (x) (plist-get x :met)) (plist-get v :targets)) '(t :null)))))

(ert-deftest eas-bench-check-skips-unlike-builds-and-passes-its-own-references ()
  (should (equal (plist-get (eas-bench-check (plist-put (copy-sequence eas-bench-test--result) :compiled :false)
                                               eas-bench-test--budget)
                            :status)
                 "skipped"))
  (let ((own (eas-bench-budget-from eas-bench-test--result eas-bench-test--budget)))
    (should (equal (plist-get own :tolerance) 2.0))
    (should (equal (plist-get (eas-bench-check eas-bench-test--result own) :status) "pass"))
    (should (equal (eas-json-parse (eas-json-encode own)) (eas-json-parse (eas-json-encode own))))))

(ert-deftest eas-bench-ladder-measures-every-stage ()
  (let* ((result (eas-bench-ladder :points '(100) :reps 1))
         (rung (aref (plist-get result :ladder) 0)))
    (should (equal (plist-get result :contract) "eas-bench/v1"))
    (should (memq (plist-get result :compiled) '(t :false)))
    (should (> (plist-get result :calibration-ms) 0))
    (should (= (plist-get rung :points) 100))
    (dolist (s eas-bench-stages)
      (let ((timing (plist-get rung (intern (concat ":" s)))))
        (should (numberp (plist-get timing :mean)))
        (should (>= (plist-get timing :max) (plist-get timing :mean)))))
    (should (eas-json-encode result))))

(ert-deftest eas-bench-verb-runs-the-ladder-and-fails-over-budget ()
  (let ((file (make-temp-file "budget" nil ".json")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert (eas-json-encode
                     (list :compiled (if (eas-bench-compiled-p) t :false) :calibration-ms 1000000
                           :tolerance 1 :floor-ms 0 :targets []
                           :reference [(:points 100 :stages (:compile-svg 0.000001))]))))
          (let ((env (eas-agent "bench" :points "100" :n 1 :budget-file file)))
            (should (eq (plist-get env :ok) :false))
            (should (equal (plist-get env :reason) "BUDGET_EXCEEDED"))
            (should (equal (plist-get (plist-get env :evidence) :field) "compile-svg"))
            (should (equal (plist-get (plist-get (plist-get env :data) :budget) :status) "fail"))
            (should (= (plist-get (aref (plist-get (plist-get env :data) :ladder) 0) :points) 100))
            (should (member "make bench" (append (plist-get env :next) nil)))))
      (delete-file file)))
  (dolist (bad '((:points "0") (:points "1k") ("line" :points "300") (:gc "never")))
    (let ((env (apply #'eas-agent "bench" bad)))
      (should (equal (plist-get env :reason) "INVALID_INPUT")))))

(ert-deftest eas-bench-shipped-budget-covers-the-ladder ()
  (let ((budget (eas-bench-read-budget)))
    (should (equal (plist-get budget :contract) "eas-bench-budget/v1"))
    (should (> (plist-get budget :calibration-ms) 0))
    (dolist (n eas-bench-points)
      (let ((ref (eas-bench--reference budget n)))
        (dolist (s eas-bench-stages)
          (should (numberp (plist-get ref (intern (concat ":" s))))))))))

(provide 'eas-bench-test)
;;; eas-bench-test.el ends here

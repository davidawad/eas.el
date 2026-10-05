;;; eas-transform-test.el --- tests for native and domain transforms -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-transform-test--rows
  [(:k "a" :v 1 :d "2026-01-15") (:k "b" :v 4 :d "2026-02-10")
   (:k "a" :v 3 :d "2026-04-01") (:k "b" :v :null :d "2026-04-20")]
  "Rows with a group key, a nullable value and a date.")

(defun eas-transform-test--run (&rest transforms)
  "Run TRANSFORMS over the test rows."
  (eas-transform-run (vconcat transforms) eas-transform-test--rows))

(ert-deftest eas-transform-filter-expression-and-predicates ()
  (should (= (length (eas-transform-test--run '(:filter "datum.v > 1"))) 2))
  (should (equal (seq-map (lambda (r) (plist-get r :v))
                          (eas-transform-test--run '(:filter (:field "k" :equal "a")))) '(1 3)))
  (should (= (length (eas-transform-test--run '(:filter (:field "v" :range [2 4])))) 2))
  (should (= (length (eas-transform-test--run '(:filter (:field "k" :oneOf ["b"])))) 2))
  (should (= (length (eas-transform-test--run '(:filter (:field "v" :valid t)))) 3))
  (should (= (length (eas-transform-test--run
                      '(:filter (:and [(:field "k" :equal "a") (:not (:field "v" :lt 2))])))) 1))
  (should (= (length (eas-transform-test--run
                      '(:filter (:field "d" :timeUnit "month" :equal "2026-04-01")))) 2)))

(ert-deftest eas-transform-filter-param-uses-the-runtime-predicate ()
  (should (= (length (eas-transform-test--run '(:filter (:param "brush")))) 4))
  (should (= (length (eas-transform-test--run '(:filter (:param "brush" :empty :false)))) 0))
  (let ((eas-transform-param-predicate
         (lambda (param row _env _empty) (and (equal param "brush") (equal (plist-get row :k) "b")))))
    (should (= (length (eas-transform-test--run '(:filter (:param "brush")))) 2))))

(ert-deftest eas-transform-calculate-fold-timeunit ()
  (should (equal (plist-get (aref (eas-transform-test--run '(:calculate "datum.v * 10" :as "w")) 1) :w)
                 40))
  (should (equal (aref (eas-transform-run [(:fold ["x" "y"])] [(:x 1 :y 2)]) 1)
                 '(:x 1 :y 2 :key "y" :value 2)))
  (should (equal (seq-map (lambda (r) (eas-time-iso (plist-get r :m)))
                          (eas-transform-test--run '(:timeUnit "yearmonth" :field "d" :as "m")))
                 '("2026-01-01" "2026-02-01" "2026-04-01" "2026-04-01")))
  (should (equal (eas-time-iso (eas-time-unit-floor "quarter" "2026-05-17")) "2012-04-01"))
  (should (equal (eas-time-iso (eas-time-unit-floor "utcyearmonthdate" "2026-05-17T10:00:00Z"))
                 "2026-05-17"))
  (eas-test-should-code "UNSUPPORTED_FEATURE" (eas-time-unit-floor "week" "2026-05-17")))

(ert-deftest eas-transform-bin-matches-vega ()
  (should (equal (eas-bin-params [0 100]) '(:start 0.0 :stop 100.0 :step 10.0)))
  (should (equal (eas-bin-params [1.3 9.7]) '(:start 1.0 :stop 10.0 :step 1.0)))
  (should (equal (eas-bin-params [0 1] '(:maxbins 5)) '(:start 0.0 :stop 1.0 :step 0.2)))
  (should (equal (eas-bin-params [3 3]) '(:start 3.0 :stop 3.5 :step 0.5)))
  (let ((out (eas-transform-run [(:bin t :field "v" :as "b")] [(:v 0) (:v 37) (:v 100)])))
    (should (equal (seq-map (lambda (r) (list (plist-get r :b) (plist-get r :b_end))) out)
                   '((0.0 10.0) (30.0 40.0) (90.0 100.0))))))

(ert-deftest eas-transform-aggregate-ops ()
  (should (equal (eas-transform-test--run
                  '(:aggregate [(:op "sum" :field "v" :as "s") (:op "count" :as "n")
                                (:op "mean" :field "v" :as "m") (:op "missing" :field "v" :as "miss")]
                    :groupby ["k"]))
                 [(:k "a" :s 4 :n 2 :m 2.0 :miss 0) (:k "b" :s 4 :n 2 :m 4.0 :miss 1)]))
  (let ((vs '(1 2 3 4 :null)))
    (should (equal (mapcar (lambda (op) (eas-agg-apply op vs))
                           '("median" "q1" "q3" "min" "max" "variance" "variancep" "distinct" "valid"))
                   (list 2.5 1.75 3.25 1 4 (/ 5.0 3) 1.25 5 4))))
  (eas-test-should-code "UNSUPPORTED_FEATURE"
    (eas-transform-test--run '(:aggregate [(:op "argmax" :field "v" :as "x")]))))

(ert-deftest eas-transform-joinaggregate-and-window ()
  (should (equal (seq-map (lambda (r) (plist-get r :total))
                          (eas-transform-test--run
                           '(:joinaggregate [(:op "sum" :field "v" :as "total")] :groupby ["k"])))
                 '(4 4 4 4)))
  (let ((out (eas-transform-run
              [(:window [(:op "row_number" :as "rn") (:op "sum" :field "v" :as "cum")
                         (:op "lag" :field "v" :as "prev") (:op "rank" :as "rk")]
                :sort [(:field "v")])]
              [(:v 3) (:v 1) (:v 3) (:v 2)])))
    (should (equal (seq-map (lambda (r) (list (plist-get r :rn) (plist-get r :cum)
                                              (plist-get r :prev) (plist-get r :rk)))
                            out)
                   '((3 6 2 3) (1 1 :null 1) (4 9 3 3) (2 3 1 2)))))
  (let ((out (eas-transform-run [(:window [(:op "mean" :field "v" :as "ma")] :frame [-1 1])]
                                  [(:v 1) (:v 2) (:v 6)])))
    (should (equal (seq-map (lambda (r) (plist-get r :ma)) out) (list 1.5 3.0 4.0)))))

(ert-deftest eas-transform-unsupported-names-the-path ()
  (should (equal (plist-get (eas-test-should-code "UNSUPPORTED_FEATURE"
                              (eas-transform-run [(:filter "true") (:lookup "x")] [] nil "/layer/0/transform"))
                            :path)
                 "/layer/0/transform/1")))

(ert-deftest eas-transform-lttb-keeps-shape-and-endpoints ()
  (let* ((n 1000)
         (xs (vconcat (number-sequence 0 (1- n))))
         (ys (vconcat (mapcar (lambda (i) (if (= i 500) 100.0 (sin (/ i 50.0)))) (number-sequence 0 (1- n)))))
         (idx (eas-lttb-indices xs ys 50)))
    (should (= (length idx) 50))
    (should (= (aref idx 0) 0))
    (should (= (aref idx 49) (1- n)))
    (should (seq-contains-p idx 500))
    (should (equal (append idx nil) (sort (append idx nil) #'<))))
  (should (equal (eas-lttb-indices [0 1 2] [1 2 3] 10) [0 1 2]))
  (let ((rows (eas-transform-run [(:x-eas:transform "lttb" :x "t" :y "v" :threshold 3)]
                                   [(:t 0 :v 0) (:t 1 :v 5) (:t 2 :v 1) (:t 3 :v 0)])))
    (should (equal rows [(:t 0 :v 0) (:t 1 :v 5) (:t 3 :v 0)]))))

(ert-deftest eas-transform-reference-band-stub ()
  (should (equal (eas-transform-run [(:x-eas:transform "reference-band" :marker "ldl-c" :lo 0 :hi 100)]
                                      [(:v 1)])
                 [(:v 1 :lo 0 :hi 100)])))

(ert-deftest eas-transform-domain-schema-is-checked ()
  (should (equal (plist-get (eas-test-should-code "INVALID_INPUT"
                              (eas-transform-run [(:x-eas:transform "lttb" :x "t")] []))
                            :path)
                 "/transform/0/y"))
  (eas-test-should-code "INVALID_INPUT"
    (eas-transform-run [(:x-eas:transform "lttb" :x "t" :y "v" :threshold "many")] [])))

(provide 'eas-transform-test)
;;; eas-transform-test.el ends here

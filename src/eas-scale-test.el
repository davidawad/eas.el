;;; eas-scale-test.el --- tests for scales, nice domains and ticks -*- lexical-binding: t; -*-

;;; Code:

(require 'eas-test-support)
(require 'eas-scale)

(ert-deftest eas-scale-linear-ticks-match-d3 ()
  ;; Expected values are d3-array 3 ticks().
  (should (equal (eas-scale-linear-ticks 0 10 10) '(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0)))
  (should (equal (eas-scale-linear-ticks 0 1 5) '(0.0 0.2 0.4 0.6 0.8 1.0)))
  (should (equal (eas-scale-linear-ticks -0.3 1.7 4) '(0.0 0.5 1.0 1.5)))
  (should (equal (eas-scale-linear-ticks 0 8200 5) '(0.0 2000.0 4000.0 6000.0 8000.0)))
  (should (equal (eas-scale-linear-ticks 10 0 2) '(10.0 5.0 0.0))))

(ert-deftest eas-scale-nice-matches-d3 ()
  (should (equal (eas-scale-nice-linear 0.123 9.87) '(0.0 . 10.0)))
  (should (equal (eas-scale-nice-linear 101.8 110.4) '(101.0 . 111.0)))
  (should (equal (eas-scale-nice-linear 0 12050) '(0.0 . 13000.0)))
  (should (equal (eas-scale-nice-linear -0.42 0.0) '(-0.45 . 0.0))))

(ert-deftest eas-scale-continuous-zero-nice-and-degenerate ()
  (should (equal (plist-get (eas-scale-continuous "linear" 3 9 [0 1] :zero t :nice t) :domain) [0.0 9.0]))
  (should (equal (plist-get (eas-scale-continuous "linear" 3 9.3 [0 1] :zero t :nice t) :domain) [0.0 10.0]))
  (should (equal (plist-get (eas-scale-continuous "linear" -3 -1 [0 1] :zero t) :domain) [-3.0 0.0]))
  (should (equal (plist-get (eas-scale-continuous "linear" 5 5 [0 1]) :domain) [2.5 7.5]))
  (should (equal (plist-get (eas-scale-continuous "log" 3 900 [0 1] :nice t) :domain) [1.0 1000.0])))

(ert-deftest eas-scale-apply-and-invert-round-trip ()
  (let ((s (eas-scale-continuous "linear" 0 10 [100 300])))
    (should (= (eas-scale-apply s 2.5) 150.0))
    (should (= (eas-scale-invert s 150) 2.5)))
  (let ((s (eas-scale-continuous "log" 1 100 [0 200])))
    (should (= (eas-scale-apply s 10) 100.0))
    (should (< (abs (- (eas-scale-invert s 100) 10)) 1e-9)))
  (let ((s (eas-scale-continuous "time" (eas-time-parse "2026-01-01") (eas-time-parse "2026-01-11") [0 100])))
    (should (= (eas-scale-apply s "2026-01-06") 50.0))
    (should (equal (eas-time-iso (eas-scale-invert s 50)) "2026-01-06")))
  (let ((s (eas-scale-continuous "linear" 0 10 [200 0])))
    (should (= (eas-scale-apply s 10) 0.0))
    (should (= (eas-scale-invert s 0) 10.0))))

(ert-deftest eas-scale-band-and-point-match-d3 ()
  (let ((band (eas-scale-band "band" ["a" "b" "c" "d" "e"] [0 300])))
    (should (= (plist-get band :step) 60.0))
    (should (= (plist-get band :bandwidth) 54.0))
    (should (= (eas-scale-apply band "a") 3.0))
    (should (= (eas-scale-apply band "e") 243.0))
    (should (equal (eas-scale-invert band 250) "e"))
    (should-not (eas-scale-apply band "z")))
  (let ((point (eas-scale-band "point" ["a" "b"] [0 40])))
    (should (= (eas-scale-apply point "a") 10.0))
    (should (= (eas-scale-apply point "b") 30.0))
    (should (= (plist-get point :bandwidth) 0.0)))
  (let ((rev (eas-scale-band "band" ["a" "b"] [100 0] 0 0)))
    (should (= (eas-scale-apply rev "a") 50.0))))

(ert-deftest eas-scale-color-scales ()
  (let ((ord (eas-scale-ordinal ["x" "y"] eas-scale-tableau10)))
    (should (equal (eas-scale-apply ord "y") "#f58518")))
  (let ((ramp (list :type "sequential" :domain [0 10] :range eas-scale-blues)))
    (should (equal (eas-scale-apply ramp 0) "#cfe1f2"))
    (should (equal (eas-scale-apply ramp 10) "#0a4a90"))
    (should (equal (eas-scale-apply ramp 5) "#5ba3cf"))))

(ert-deftest eas-scale-tick-formats ()
  (let ((s (eas-scale-continuous "linear" 0 12000 [0 1])))
    (should (equal (funcall (eas-scale-tick-format s 5) 10000.0) "10,000")))
  (let ((s (eas-scale-continuous "linear" 0 1 [0 1])))
    (should (equal (funcall (eas-scale-tick-format s 5) 0.4) "0.4"))
    (should (equal (funcall (eas-scale-tick-format s 5 ".0%") 0.4) "40%")))
  (should (equal (eas-scale-format-number -12345.678 1) "−12,345.7")))

(ert-deftest eas-scale-time-ticks-match-d3 ()
  (cl-flet ((fmt (a b n) (mapcar #'eas-scale-time-multi-format
                                 (eas-scale-time-ticks (eas-time-parse a) (eas-time-parse b) n))))
    (should (equal (fmt "2026-03-02" "2026-03-11" 10)
                   '("Mon 02" "Tue 03" "Wed 04" "Thu 05" "Fri 06" "Sat 07" "Mar 08" "Mon 09" "Tue 10" "Wed 11")))
    (should (equal (fmt "2025-01-15" "2026-06-11" 5) '("April" "July" "October" "2026" "April")))
    (should (equal (fmt "2026-03-02T09:00:00Z" "2026-03-02T17:00:00Z" 4)
                   '("09 AM" "12 PM" "03 PM")))
    (should (equal (fmt "2000-01-01" "2030-01-01" 4) '("2000" "2010" "2020" "2030")))))

(ert-deftest eas-scale-log-ticks ()
  ;; d3-scale log.ticks: every k*10^i while decades < count.
  (should (equal (eas-scale-log-ticks 1.0 100.0 10)
                 '(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 20.0 30.0 40.0 50.0 60.0 70.0 80.0 90.0 100.0)))
  (should (equal (eas-scale-log-ticks 1e-3 1e12 10)
                 (mapcar (lambda (e) (expt 10.0 e)) '(-2.0 0.0 2.0 4.0 6.0 8.0 10.0 12.0))))
  ;; Labels: only small mantissas, like d3's log tickFormat (1, 2, 10, 20, ...).
  (let* ((s (eas-scale-continuous "log" 1 10000 [0 1]))
         (fmt (eas-scale-tick-format s 8)))
    (should (equal (mapcar fmt '(1.0 2.0 3.0 10.0 20.0 50.0 1000.0 2000.0 10000.0))
                   '("1" "2" "" "10" "20" "" "1,000" "2,000" "10,000")))))

(provide 'eas-scale-test)
;;; eas-scale-test.el ends here

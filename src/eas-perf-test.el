;;; eas-perf-test.el --- tests for the performance suite (eas-b2s.4) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Fast checks of the suite's machinery: metrics, the gate, recording
;; and the report.  The heavy workloads run only under make bench.

;;; Code:

(require 'ert)
(require 'eas-test-support)
(require 'eas-perf)
(require 'eas-perf-gate)

(defvar eas-perf-test--sink nil "Where test frames leave what they allocate.")

(defun eas-perf-test--metrics (bytes &rest calls)
  "Metrics with BYTES per frame and CALLS.
CALLS are compiles, patches, scenes, renders and repaints."
  (append (list :frames 20 :alloc-bytes bytes :conses (/ bytes 20))
          (cl-loop for m in eas-perf-call-metrics for c in (append calls '(0 0 0 0 0))
                   append (list (intern (format ":%s" m)) c))
          (list :ms-median 1.5 :ms-p95 2.5)))

(defun eas-perf-test--run (mode &rest workloads)
  "A run in JSON shape of MODE with WORKLOADS, each (NAME SVG TEXT)."
  (list :contract "eas-perf-run/v1" :mode mode :emacs emacs-version
        :frames '(:live 20 :warmup 3 :render 2)
        :workloads (vconcat (mapcar (lambda (w) (list :name (nth 0 w) :svg (nth 1 w) :text (nth 2 w)))
                                    workloads))))

(defun eas-perf-test--baseline ()
  "A baseline with a byte entry of two workloads."
  (eas-perf-record nil (eas-perf-test--run "byte"
                                           (list "ladder-25" (eas-perf-test--metrics 1000 0 1 1 1 0)
                                                 (eas-perf-test--metrics 2000 0 1 1 1 1))
                                           (list "render/bars" (eas-perf-test--metrics 500 1 0 1 1 0)
                                                 (eas-perf-test--metrics 600 1 0 1 1 1)))
                   "2026-10-07"))

(defun eas-perf-test--statuses (verdict)
  "(NAME TARGET STATUS) of each row of VERDICT."
  (mapcar (lambda (r) (list (plist-get r :name) (plist-get r :target) (plist-get r :status)))
          (plist-get verdict :rows)))

(ert-deftest eas-perf-alloc-bytes-weighs-every-count ()
  (should (= (eas-perf--alloc-bytes '(0 0 0 0 0 0 0) '(1 1 1 1 1 1 1)) (+ 16 16 8 48 1 56 32)))
  (should (= (eas-perf--alloc-bytes '(10 0 0 0 5 0 0) '(12 0 0 0 9 0 0)) 36)))

(ert-deftest eas-perf-percentile-is-nearest-rank ()
  (let ((ms '(5 1 4 2 3)))
    (should (= (eas-perf--percentile ms 0.5) 3))
    (should (= (eas-perf--percentile ms 0.95) 5))
    (should (equal ms '(5 1 4 2 3)))))

(ert-deftest eas-perf-run-frames-counts-allocation-and-calls ()
  (let* ((n 0)
         (m (eas-perf-run-frames (lambda (_i _buffer)
                                   (cl-incf n)
                                   (setq eas-perf-test--sink (make-list 1000 nil))
                                   (ignore-errors (eas-text-render nil)))
                                 4 2)))
    (should (= n 6))
    (should (= (plist-get m :frames) 4))
    ;; 1000 conses per frame, plus what the render and the counting cost.
    (should (>= (plist-get m :conses) 1000))
    (should (>= (plist-get m :alloc-bytes) 16000))
    (should (= (plist-get m :renders) 1.0))
    (should (= (plist-get m :compiles) 0.0))
    (should (numberp (plist-get m :ms-p95)))
    ;; The counting advice is gone afterwards.
    (dolist (entry eas-perf-counted)
      (should-not (advice--p (symbol-function (car entry)))))))

(ert-deftest eas-perf-allocation-is-deterministic ()
  (let ((step (lambda (i _buffer)
                (setq eas-perf-test--sink (list (make-list (+ 100 (% i 2)) i)
                                                (vconcat (number-sequence 1 50)))))))
    (should (equal (plist-get (eas-perf-run-frames step 4 1) :alloc-bytes)
                   (plist-get (eas-perf-run-frames step 4 1) :alloc-bytes)))))

(ert-deftest eas-perf-live-workload-runs-and-counts ()
  (let ((eas-views (make-hash-table :test 'equal))
        (eas-perf-frames 1) (eas-perf-warmup 0))
    (dolist (target '(svg text))
      (let ((m (eas-perf-measure "ladder-25" (cdr (assoc "ladder-25" (eas-perf-live-workloads))) target)))
        (should-not (plist-get m :error))
        (should (> (plist-get m :alloc-bytes) 0))
        (should (= (plist-get m :renders) 1.0))
        ;; A keyed push patches the plan's rows; it does not compile.
        (should (= (+ (plist-get m :compiles) (plist-get m :patches)) 1.0))
        (should (= (plist-get m :repaints) (if (eq target 'text) 1.0 0.0)))))
    (should (= (hash-table-count eas-views) 0))))

(ert-deftest eas-perf-workloads-cover-the-uses ()
  (let ((names (mapcar #'car (eas-perf-workloads))))
    (dolist (n '("ladder-25" "ladder-100" "depth-25" "depth-100" "candles" "clock" "pacman"
                 "pi-monte-carlo" "hover-airports" "hover-counties" "resize"))
      (should (member n names)))
    (dolist (template (eas-template-names))
      (should (member (concat "render/" template) names)))))

(ert-deftest eas-perf-failing-workload-is-data ()
  (let ((m (eas-perf-measure "boom" (lambda (_) (error "Kaboom")) 'svg)))
    (should (equal (plist-get m :error) "Kaboom"))))

(ert-deftest eas-perf-gate-passes-within-tolerance ()
  (let* ((run (eas-perf-test--run "byte"
                                  (list "ladder-25" (eas-perf-test--metrics 1099 0 1 1 1 0)
                                        (eas-perf-test--metrics 1500 0 1 1 1 1))
                                  (list "render/bars" (eas-perf-test--metrics 500 1 0 1 1 0)
                                        (eas-perf-test--metrics 600 1 0 1 1 1))))
         (verdict (eas-perf-compare (eas-perf-test--baseline) run)))
    (should (equal (plist-get verdict :status) "pass"))
    (should (equal (eas-perf-test--statuses verdict)
                   '(("ladder-25" "svg" "pass") ("ladder-25" "text" "pass")
                     ("render/bars" "svg" "pass") ("render/bars" "text" "pass"))))))

(ert-deftest eas-perf-gate-fails-past-tolerance ()
  (let* ((run (eas-perf-test--run "byte"
                                  ;; 11% more allocation on SVG, one more compile on text.
                                  (list "ladder-25" (eas-perf-test--metrics 1110 0 1 1 1 0)
                                        (eas-perf-test--metrics 2000 1 1 1 1 1))
                                  (list "render/bars" (eas-perf-test--metrics 500 1 0 1 1 0)
                                        (list :error "Kaboom"))))
         (verdict (eas-perf-compare (eas-perf-test--baseline) run))
         (text (eas-perf-format-verdict verdict)))
    (should (equal (plist-get verdict :status) "fail"))
    (should (equal (eas-perf-test--statuses verdict)
                   '(("ladder-25" "svg" "fail") ("ladder-25" "text" "fail")
                     ("render/bars" "svg" "pass") ("render/bars" "text" "error"))))
    (should (equal (mapcar (lambda (o) (plist-get o :metric))
                           (plist-get (nth 1 (plist-get verdict :rows)) :over))
                   '(:compiles)))
    (should (string-match-p "| ladder-25 *| svg *| 1\\.0 *| 1\\.1 *| \\+11\\.0% .*| fail (alloc-bytes) |" text))
    (should (string-match-p "FAIL -- 1 pass, 2 fail, 1 error, 0 new, 0 missing" text))))

(ert-deftest eas-perf-gate-reports-new-missing-and-skipped ()
  (let* ((run (eas-perf-test--run "byte"
                                  (list "ladder-25" (eas-perf-test--metrics 900 0 1 1 1 0)
                                        (eas-perf-test--metrics 1900 0 1 1 1 1))
                                  (list "fresh" (eas-perf-test--metrics 1 0 0 0 0 0)
                                        (eas-perf-test--metrics 1 0 0 0 0 0))))
         (verdict (eas-perf-compare (eas-perf-test--baseline) run)))
    (should (equal (plist-get verdict :status) "pass"))
    (should (equal (mapcar #'caddr (eas-perf-test--statuses verdict))
                   '("pass" "pass" "new" "new" "missing" "missing"))))
  (let ((verdict (eas-perf-compare (eas-perf-test--baseline) (eas-perf-test--run "native"))))
    (should (equal (plist-get verdict :status) "skipped"))
    (should (string-match-p "no native baseline" (eas-perf-format-verdict verdict)))))

(ert-deftest eas-perf-gate-notes-a-different-frame-count ()
  (let* ((run (eas-perf-test--run "byte" (list "ladder-25" (eas-perf-test--metrics 1000 0 1 1 1 0)
                                               (eas-perf-test--metrics 2000 0 1 1 1 1))))
         (same (eas-perf-compare (eas-perf-test--baseline) run))
         (fewer (eas-perf-compare (eas-perf-test--baseline)
                                  (plist-put (copy-sequence run) :frames '(:live 5 :warmup 3 :render 2)))))
    (should-not (plist-get same :note))
    (should (string-match-p "note: this run's frames" (eas-perf-format-verdict fewer)))))

(ert-deftest eas-perf-history-keeps-a-row-per-version-and-mode ()
  (let* ((baseline (eas-perf-test--baseline))
         (h1 (eas-perf-history-add [] baseline "byte"))
         (h2 (eas-perf-history-add h1 baseline "byte"))
         (row (aref h2 0)))
    (should (= (length h2) 1))
    (should (equal (plist-get row :mode) "byte"))
    (should (equal (plist-get row :version) (eas-perf-version)))
    (should (equal (mapcar (lambda (l) (plist-get l :name)) (plist-get row :live)) '("ladder-25")))
    (should (= (plist-get (plist-get row :render) :svg) 500))
    (let ((older (vconcat (list (plist-put (copy-sequence row) :version "0.0.1")) h2)))
      (should (= (length (eas-perf-history-add older baseline "byte")) 2)))))

(ert-deftest eas-perf-report-has-tables-and-trend ()
  (let* ((baseline (eas-perf-test--baseline))
         (md (eas-perf-report baseline (eas-perf-history-add [] baseline "byte"))))
    (should (string-match-p "^## byte-compiled$" md))
    (should (string-match-p "^| ladder-25 *| svg *| 1\\.0 *| 50 *| 0/1/1/1/0 *| 1\\.5/2\\.5 *|$" md))
    (should (string-match-p "First render (1 templates)" md))
    (should (string-match-p "^## Trend per release$" md))
    (should (string-match-p (concat "^| " (regexp-quote (eas-perf-version)) " *| byte *| 2026-10-07 *| 1\\.0 / 2\\.0 *| 0\\.5 / 0\\.6 *|$") md))
    (should (string-match-p "more than 10%" md))))

(ert-deftest eas-perf-record-replaces-one-mode-and-round-trips ()
  (let* ((baseline (eas-perf-test--baseline))
         (native (eas-perf-test--run "native" (list "ladder-25" (eas-perf-test--metrics 7 0 0 0 0 0)
                                                    (eas-perf-test--metrics 8 0 0 0 0 0))))
         (both (eas-perf-record baseline native "2026-10-08"))
         (again (eas-perf-record both (eas-perf-test--run "byte") "2026-10-09"))
         (file (make-temp-file "eas-perf" nil ".json")))
    (should (equal (eas-plist-keys (plist-get both :modes)) '(:byte :native)))
    (should (equal (plist-get both :tolerance) eas-perf-default-tolerance))
    (should (equal (eas-plist-keys (plist-get again :modes)) '(:native :byte)))
    (should (equal (plist-get (plist-get (eas-perf--mode again "byte") :recorded) :date) "2026-10-09"))
    (unwind-protect
        (progn (eas-perf-write file both)
               (let ((verdict (eas-perf-compare (eas-perf-read file) native)))
                 (should (equal (plist-get verdict :status) "pass"))
                 (should (equal (mapcar #'caddr (eas-perf-test--statuses verdict)) '("pass" "pass")))))
      (delete-file file))))

(ert-deftest eas-perf-skips-workloads-this-emacs-cannot-run ()
  (cl-letf (((symbol-function 'eas-perf-native-p) (lambda () nil))
            ((symbol-function 'eas-perf-zlib-p) (lambda () nil)))
    (should (eas-perf-unavailable-p "cold-native/projections"))
    (should (eas-perf-unavailable-p "render/contour-plot"))
    (should-not (eas-perf-unavailable-p "cold/projections"))
    (should-not (eas-perf-unavailable-p "ladder-25"))
    (should-not (cl-some (lambda (w) (string-prefix-p "cold-native/" (car w))) (eas-perf-workloads))))
  (cl-letf (((symbol-function 'eas-perf-native-p) (lambda () t))
            ((symbol-function 'eas-perf-zlib-p) (lambda () t)))
    (should-not (eas-perf-unavailable-p "cold-native/projections"))
    (should-not (eas-perf-unavailable-p "render/contour-plot"))))

(provide 'eas-perf-test)
;;; eas-perf-test.el ends here

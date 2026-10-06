;;; eas-agent-health.el --- agent verbs bench and doctor -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;   bench [--points 1000,10000,100000] [--budget]   the ladder (eas-bench.el)
;;   bench SOURCE [--data B] [--n N]   measured resolve, compile, render and hover ms
;;   doctor                            eager (:name :status :detail :remediation) rows
;;
;; bench measures in the Emacs it runs in (batch for bin/eas), so
;; its numbers are the Lisp half only; a GUI frame adds rasterization
;; (docs/design/engine-spikes.md).  With no SOURCE it runs the fixed
;; ladder; --budget compares that with bench-budget.json and fails with
;; BUDGET_EXCEEDED naming each stage over its limit.  doctor status is pass, fail or
;; skip; a skip names what is missing and never fails the envelope.

;;; Code:

(require 'eas-agent-core)
(require 'eas-agent-verbs)
(require 'eas-view)
(require 'eas-bench)

;;; bench

(defun eas-agent--time (n fn)
  "Run FN N times; return (:mean MS :max MS)."
  (let ((total 0.0) (worst 0.0))
    (dotimes (_ n)
      (let ((start (float-time)))
        (funcall fn)
        (let ((ms (* 1000.0 (- (float-time) start))))
          (setq total (+ total ms) worst (max worst ms)))))
    (list :mean (eas-scene-round (/ total n)) :max (eas-scene-round worst))))

(defun eas-agent--hover-points (scene n)
  "N pointer positions sweeping across SCENE's first view."
  (let* ((b (plist-get (aref (plist-get scene :views) 0) :bounds))
         (y (+ (aref b 1) (/ (aref b 3) 2.0))))
    (cl-loop for i below n
             collect (vector (+ (aref b 0) (* (aref b 2) (/ (+ i 0.5) n))) y))))

(defun eas-agent--bench-hover (spec n)
  "Mean/max ms of N pointermove dispatches on a scratch view of SPEC."
  (let* ((eas-views (make-hash-table :test 'equal))
         (view (eas-view-open spec :id "bench"))
         (points (and (eas-view-interactive view)
                      (eas-agent--hover-points (eas-view-scene view) n))))
    (if (null points) :null
      (eas-agent--time n (lambda () (eas-dispatch view (list :type "pointermove" :px (pop points))))))))

(defun eas-agent--points (opts)
  "OPTS's --points as a list of positive integers, or nil when absent."
  (let ((v (plist-get opts :points)))
    (when v
      (let ((ns (cond ((numberp v) (list v))
                      ((vectorp v) (append v nil))
                      ((stringp v) (mapcar (lambda (w) (and (string-match-p "\\`[0-9]+\\'" w) (string-to-number w)))
                                           (split-string v "[, ]+" t))))))
        (unless (and ns (seq-every-p (lambda (n) (and (integerp n) (> n 1))) ns))
          (eas-signal "INVALID_INPUT" (format "--points is a comma list of integers above 1, got %S" v)
                        :option "points"))
        ns))))

(defun eas-agent--bench-ladder (opts)
  "Answer bench with no SOURCE: the ladder under OPTS, checked with --budget."
  (let* ((points (eas-agent--points opts))
         (reps (eas-agent-arg-number opts :n))
         (gc (intern (eas-agent-arg-choice opts :gc '("deferred" "default") "deferred")))
         (file (plist-get opts :budget-file))
         (budget (and (or (plist-get opts :budget) file) (eas-bench-read-budget file)))
         (result (eas-bench-ladder :points points :reps (and reps (max 1 reps)) :gc gc))
         (verdict (and budget (eas-bench-check result budget)))
         (data (if verdict (append result (list :budget verdict)) result))
         (again (apply #'eas-agent-cmd "bench"
                       (append (and points (list "--points" (mapconcat #'number-to-string points ",")))
                               (list "--budget")))))
    (cond
     ((and verdict (equal (plist-get verdict :status) "fail"))
      (let ((v (plist-get verdict :violations)))
        (eas-agent-fail
         "BUDGET_EXCEEDED"
         (list :message (format "%d stage(s) over budget: %s" (length v)
                                (mapconcat (lambda (x) (format "%s at %d points %.1f ms > %.1f ms"
                                                               (plist-get x :stage) (plist-get x :points)
                                                               (plist-get x :ms) (plist-get x :limit)))
                                           v "; "))
               :path (or file eas-bench-budget-file) :field (plist-get (aref v 0) :stage)
               :points (plist-get (aref v 0) :points))
         data again "make bench" "make bench-budget")))
     ((not (eq (plist-get result :compiled) t))
      (eas-agent-ok data "make bench" again))
     (t (eas-agent-ok data (unless budget again))))))

(defun eas-agent-bench (pos opts)
  "Answer bench [SOURCE] (first of POS) under OPTS: latency per stage.
Without SOURCE, the fixed ladder at 1k, 10k and 100k points."
  (if (null pos) (eas-agent--bench-ladder opts)
    (dolist (key '(:points :budget :budget-file :gc))
      (when (plist-get opts key)
        (eas-signal "INVALID_INPUT" (format "--%s is for the ladder: bench with no SOURCE" (eas-key-name key))
                      :option (eas-key-name key))))
    (eas-agent--bench-source pos opts)))

(defun eas-agent--bench-source (pos opts)
  "Answer bench SOURCE (first of POS) under OPTS: latency per stage."
  (let* ((n (max 1 (or (eas-agent-arg-number opts :n) 10)))
         (src (eas-agent-resolve-source "bench" pos opts))
         (spec (plist-get src :spec))
         (svg-scene (eas-agent-compile spec opts "svg"))
         (text-scene (eas-agent-compile spec opts "text")))
    (eas-agent-ok
     (list :n n :emacs emacs-version :batch (if noninteractive t :false)
           :template (or (plist-get src :template) :null)
           :rows (length (plist-get (plist-get spec :data) :values))
           :items (apply #'+ (cl-loop for v across (plist-get svg-scene :views)
                                      append (cl-loop for m across (plist-get v :marks)
                                                      collect (length (plist-get m :items)))))
           :ms (list :resolve (if (plist-get src :template)
                                  (eas-agent--time n (lambda () (eas-agent-resolve-source "bench" pos opts)))
                                :null)
                     :compile-svg (eas-agent--time n (lambda () (eas-agent-compile spec opts "svg")))
                     :render-svg (eas-agent--time n (lambda () (eas-svg-render svg-scene)))
                     :compile-text (eas-agent--time n (lambda () (eas-agent-compile spec opts "text")))
                     :render-text (eas-agent--time n (lambda () (eas-text-render text-scene)))
                     :hover (eas-agent--bench-hover spec n)))
     (eas-agent-next "explain" src "--stage" "compile"))))

;;; doctor

(defun eas-agent--row (name status detail &optional remediation)
  "A doctor row: NAME, STATUS, DETAIL and REMEDIATION."
  (list :name name :status status :detail detail :remediation (or remediation :null)))

(defun eas-agent--template-row (name)
  "Doctor row for template NAME: its example resolves, compiles, renders."
  (condition-case err
      (let* ((spec (eas-resolve name (eas-template-example name)))
             (text (eas-text-render (eas-compile spec :target 'text))))
        (eas-agent--row (concat "template:" name) "pass"
                          (format "example renders (%d text lines)" (length (split-string text "\n")))))
    (error (let ((e (eas-error-plist err)))
             (eas-agent--row (concat "template:" name) "fail"
                               (format "%s: %s" (plist-get e :code) (plist-get e :message))
                               (eas-agent-cmd "check" name "--data"
                                                (or (ignore-errors (eas-template-example-file
                                                                    (eas-template-get name)))
                                                    "BINDINGS.json")))))))

(defun eas-agent-doctor-rows ()
  "Every health check, evaluated now."
  (append
   (list
    (if (version<= "30.1" emacs-version)
        (eas-agent--row "emacs" "pass" (format "GNU Emacs %s" emacs-version))
      (eas-agent--row "emacs" "fail" (format "GNU Emacs %s; eas needs 30.1" emacs-version)
                        "Install GNU Emacs 30.1 or newer"))
    (if (and (fboundp 'json-serialize) (fboundp 'json-parse-string))
        (eas-agent--row "json" "pass" "native JSON")
      (eas-agent--row "json" "fail" "this Emacs lacks native JSON" "Build Emacs with libjansson (29) or use 30+"))
    (let ((names (ignore-errors (eas-template-names))))
      (if names (eas-agent--row "templates" "pass"
                                  (format "%d: %s" (length names) (string-join names ", ")))
        (eas-agent--row "templates" "fail" (format "none in %s" (string-join eas-template-directories ", "))
                          "Point eas-template-directories at the repo's templates/"))))
   (mapcar #'eas-agent--template-row (ignore-errors (eas-template-names)))
   (list
    (if (file-exists-p eas-conformance-supported-file)
        (eas-agent--row "supported.json" "pass" eas-conformance-supported-file)
      (eas-agent--row "supported.json" "skip" "absent: every recognised feature counts as native"
                        "Generate it with (eas-conformance-generate)"))
    (cond (noninteractive (eas-agent--row "svg-display" "skip" "batch: no frame; the text backend is available"))
          ((image-type-available-p 'svg) (eas-agent--row "svg-display" "pass" "SVG images display"))
          (t (eas-agent--row "svg-display" "skip" "this Emacs cannot display SVG; views use the text backend"
                               "Build Emacs with librsvg for SVG views")))
    (if (eas-chart-available-p)
        (eas-agent--row "bin/chart" "pass" (executable-find eas-chart-program))
      (eas-agent--row "bin/chart" "skip" (eas-chart-missing-reason)
                        "Optional: put bin/chart on PATH for static export, conformance refs and opt-in eas-static-fallback"))
    (if (executable-find eas-chart-rsvg-program)
        (eas-agent--row "rsvg-convert" "pass" (executable-find eas-chart-rsvg-program))
      (eas-agent--row "rsvg-convert" "skip" (format "%s not on PATH" eas-chart-rsvg-program)
                        "Install librsvg2-bin for conformance diffs")))))

(defun eas-agent-doctor (_pos _opts)
  "Answer doctor: health rows; fail when any row fails."
  (let* ((rows (eas-agent-doctor-rows))
         (failed (seq-filter (lambda (r) (equal (plist-get r :status) "fail")) rows))
         (data (list :rows (vconcat rows)
                     :pass (seq-count (lambda (r) (equal (plist-get r :status) "pass")) rows)
                     :fail (length failed)
                     :skip (seq-count (lambda (r) (equal (plist-get r :status) "skip")) rows))))
    (if failed
        (apply #'eas-agent-fail "ENGINE_FAILED"
               (list :message (format "%d health check(s) failed: %s" (length failed)
                                      (mapconcat (lambda (r) (plist-get r :name)) failed ", ")))
               data
               (append (seq-filter (lambda (c) (and (stringp c) (string-prefix-p eas-agent-shell-program c)))
                                   (mapcar (lambda (r) (plist-get r :remediation)) failed))
                       (list (eas-agent-cmd "doctor"))))
      (eas-agent-ok data (eas-agent-cmd "describe")))))

(eas-agent-register-verb
 "bench" #'eas-agent-bench
 :doc "Measured latency (ms): the 1k/10k/100k ladder, or one SOURCE's resolve, compile, render and hover"
 :usage "bench [--points 1000,10000,100000] [--n N] [--gc deferred|default] [--budget] [--budget-file F] | bench SOURCE [--data B] [--n N] [--width W --height H]"
 :options (append '(:n :points :gc :budget :budget-file) eas-agent--source-options)
 :flags '(:budget))
(eas-agent-register-verb
 "doctor" #'eas-agent-doctor :doc "Eager health rows (:name :status pass|fail|skip :detail :remediation)"
 :usage "doctor")

(provide 'eas-agent-health)
;;; eas-agent-health.el ends here

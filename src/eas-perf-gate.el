;;; eas-perf-gate.el --- performance baselines, the regression gate, the report -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.4.  A run of the suite (eas-perf.el) is compared with the
;; committed baseline, bench/baseline.json:
;;
;;   {"contract": "eas-perf-baseline/v1",
;;    "tolerance": {"alloc": 0.1, "calls": 0},
;;    "modes": {"byte":   {"recorded": {...}, "workloads": [W ...]},
;;              "native": {...}}}
;;
;; where each W is {"name": N, "svg": METRICS, "text": METRICS}.  Only
;; hardware-independent metrics gate: a workload fails when its bytes
;; allocated per frame grow past the baseline by more than the alloc
;; tolerance, or one of its call counts per frame (compiles, patches,
;; scenes, renders, repaints) by more than the calls tolerance.  A
;; workload that errors fails too.  Wall-clock medians and p95s are
;; printed beside them, never gated.  A run in a mode the baseline does
;; not have is skipped, and says so.
;;
;; Recording a run replaces its mode in the baseline and its row in
;; bench/history.json (one per release version and mode), from which
;; `eas-perf-report' writes docs/perf.md: the recorded numbers and the
;; trend per release.

;;; Code:

(require 'cl-lib)
(require 'lisp-mnt)
(require 'eas-core)
(require 'eas-perf)

(defconst eas-perf-default-tolerance '(:alloc 0.1 :calls 0)
  "Tolerances of a new baseline: fractions of the baseline value.")

(defconst eas-perf-gated '(:alloc-bytes :compiles :patches :scenes :renders :repaints :slices)
  "Metrics the gate compares.")

;;; Files

(defun eas-perf-read (file)
  "The JSON object in FILE as a plist, or nil when FILE does not exist."
  (and (file-exists-p file) (eas-json-read-file file)))

(defun eas-perf-write (file object)
  "Write OBJECT to FILE as pretty JSON."
  (with-temp-file file (insert (eas-json-pretty object) "\n")))

(defun eas-perf-version ()
  "The package version in src/eas.el's header."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "eas.el" (file-name-directory (locate-library "eas"))))
    (or (lm-header "Version") "unknown")))

;;; Shapes

(defun eas-perf-run-json (run)
  "RUN (from `eas-perf-run') in its JSON shape."
  (list :contract "eas-perf-run/v1" :mode (plist-get run :mode) :emacs (plist-get run :emacs)
        :frames (list :live eas-perf-frames :warmup eas-perf-warmup :render eas-perf-render-frames
                      :heavy eas-perf-heavy-frames)
        :workloads (vconcat (mapcar (lambda (w) (cons :name w)) (plist-get run :workloads)))))

(defun eas-perf--find (workloads name)
  "The workload called NAME in WORKLOADS (a sequence), or nil."
  (seq-find (lambda (w) (equal (plist-get w :name) name)) workloads))

(defun eas-perf--mode (baseline mode)
  "BASELINE's entry for MODE (a string), or nil."
  (plist-get (plist-get baseline :modes) (intern (concat ":" mode))))

;;; The gate

(defun eas-perf--limit (metric base tolerance)
  "The highest passing value of METRIC over BASE with TOLERANCE plist."
  (* base (+ 1 (or (plist-get tolerance (if (eq metric :alloc-bytes) :alloc :calls)) 0))))

(defun eas-perf--compare-metrics (base now tolerance)
  "Gated metrics of NOW past BASE's limits under TOLERANCE, as a list.
Each is (:metric KEY :base B :now N :limit L)."
  (cl-loop for m in eas-perf-gated
           for b = (plist-get base m) for n = (plist-get now m)
           when (and (numberp b) (numberp n) (> n (+ 1e-9 (eas-perf--limit m b tolerance))))
           collect (list :metric m :base b :now n :limit (eas-perf--limit m b tolerance))))

(defun eas-perf-compare (baseline run)
  "Compare RUN (JSON shape) with BASELINE; return the verdict plist.
:status is \"pass\", \"fail\" or \"skipped\"; :rows has one plist per
workload and target with :status \"pass\", \"fail\", \"error\", \"new\"
or \"missing\"."
  (let* ((mode (plist-get run :mode))
         (entry (eas-perf--mode baseline mode))
         (tolerance (or (plist-get baseline :tolerance) eas-perf-default-tolerance))
         (base-ws (plist-get entry :workloads))
         (rows nil))
    (if (not entry)
        (list :status "skipped" :mode mode :rows nil
              :reason (format "bench/baseline.json has no %s baseline; run make bench-record" mode))
      (seq-doseq (w (plist-get run :workloads))
        (let ((b (eas-perf--find base-ws (plist-get w :name))))
          (dolist (target '(:svg :text))
            (let* ((now (plist-get w target)) (base (plist-get b target))
                   (over (and base now (not (plist-get now :error)) (not (plist-get base :error))
                              (eas-perf--compare-metrics base now tolerance))))
              ;; tiles/ workloads run on one target only.
              (when (or now base)
                (push (list :name (plist-get w :name) :target (substring (symbol-name target) 1)
                          :base base :now now :over over
                          :status (cond ((plist-get now :error) "error")
                                        ((or (null base) (plist-get base :error)) "new")
                                        (over "fail")
                                        (t "pass")))
                      rows))))))
      (seq-doseq (b base-ws)
        (unless (or (eas-perf--find (plist-get run :workloads) (plist-get b :name))
                    (eas-perf-unavailable-p (plist-get b :name)))
          (dolist (target '("svg" "text"))
            (when (plist-get b (intern (concat ":" target)))
              (push (list :name (plist-get b :name) :target target :status "missing"
                          :base (plist-get b (intern (concat ":" target))))
                    rows)))))
      (setq rows (nreverse rows))
      (list :status (if (seq-some (lambda (r) (member (plist-get r :status) '("fail" "error"))) rows)
                        "fail" "pass")
            :mode mode :tolerance tolerance :rows rows
            :note (and (plist-get run :frames) (plist-get entry :frames)
                       (not (equal (plist-get run :frames) (plist-get entry :frames)))
                       (format "this run's frames %S differ from the baseline's %S; allocation per frame may differ"
                               (plist-get run :frames) (plist-get entry :frames)))))))

;;; Tables

(defun eas-perf--kb (bytes)
  "BYTES as a KB string, one decimal."
  (if (numberp bytes) (format "%.1f" (/ bytes 1024.0)) "-"))

(defun eas-perf--calls (m)
  "The call counts of metrics M as compiles/patches/scenes/renders/repaints.
Tiles re-rasterized out of tiles shown follow when M counts them."
  (if (and m (not (plist-get m :error)))
      (concat (mapconcat (lambda (k) (let ((v (plist-get m (intern (format ":%s" k)))))
                                       (if (and (numberp v) (= v (round v))) (format "%d" v) (format "%s" v))))
                         eas-perf-call-metrics "/")
              (if (plist-get m :slices)
                  (format ", tiles %s of %s (%s%% px)" (plist-get m :slices) (plist-get m :slice-tiles)
                          (plist-get m :slice-area))
                ""))
    "-"))

(defun eas-perf--delta (base now)
  "Relative change from BASE to NOW as a signed percentage string."
  (if (and (numberp base) (numberp now) (> base 0))
      (format "%+.1f%%" (* 100.0 (/ (- now base) (float base))))
    ""))

(defun eas-perf--ms (m)
  "Median/p95 of metrics M."
  (if (and m (plist-get m :ms-median)) (format "%.1f/%.1f" (plist-get m :ms-median) (plist-get m :ms-p95)) "-"))

(defun eas-perf--table-row (row)
  "Cells of a comparison ROW."
  (let ((b (plist-get row :base)) (n (plist-get row :now)))
    (list (plist-get row :name) (plist-get row :target)
          (eas-perf--kb (plist-get b :alloc-bytes)) (eas-perf--kb (plist-get n :alloc-bytes))
          (eas-perf--delta (plist-get b :alloc-bytes) (plist-get n :alloc-bytes))
          (eas-perf--calls b) (eas-perf--calls n)
          (eas-perf--ms b) (eas-perf--ms n)
          (if (plist-get n :error) (format "error: %s" (plist-get n :error))
            (concat (plist-get row :status)
                    (and (plist-get row :over)
                         (concat " (" (mapconcat (lambda (o) (substring (symbol-name (plist-get o :metric)) 1))
                                                 (plist-get row :over) ",")
                                 ")")))))))

(defun eas-perf-markdown-table (header rows)
  "A Markdown table of HEADER (strings) and ROWS (lists of strings)."
  (let* ((all (cons header rows))
         (widths (cl-loop for i below (length header)
                          collect (apply #'max (mapcar (lambda (r) (string-width (or (nth i r) ""))) all))))
         (line (lambda (cells)
                 (concat "| " (string-join (cl-mapcar (lambda (c w) (string-pad (or c "") w)) cells widths) " | ")
                         " |\n"))))
    (concat (funcall line header)
            "|" (mapconcat (lambda (w) (make-string (+ 2 w) ?-)) widths "|") "|\n"
            (mapconcat line rows ""))))

(defconst eas-perf--compare-header
  '("workload" "target" "KB before" "KB after" "alloc" "calls before" "calls after"
    "ms before" "ms after" "status")
  "Columns of the comparison table; calls are c/p/s/r/r, ms median/p95.")

(defun eas-perf-format-verdict (verdict)
  "The comparison VERDICT as text: a table and a summary line."
  (if (equal (plist-get verdict :status) "skipped")
      (format "eas-perf %s: skipped: %s\n" (plist-get verdict :mode) (plist-get verdict :reason))
    (let* ((rows (plist-get verdict :rows))
           (count (lambda (s) (seq-count (lambda (r) (equal (plist-get r :status) s)) rows)))
           (tol (plist-get verdict :tolerance)))
      (concat
       (format "eas-perf %s: alloc KB/frame; calls per frame are compiles/patches/scenes/renders/repaints; ms median/p95 (reported, not gated)\n\n"
               (plist-get verdict :mode))
       (eas-perf-markdown-table eas-perf--compare-header (mapcar #'eas-perf--table-row rows))
       (format "\neas-perf %s: %s -- %d pass, %d fail, %d error, %d new, %d missing (tolerance: alloc +%g%%, calls +%g%%)\n"
               (plist-get verdict :mode) (upcase (plist-get verdict :status))
               (funcall count "pass") (funcall count "fail") (funcall count "error")
               (funcall count "new") (funcall count "missing")
               (* 100 (plist-get tol :alloc)) (* 100 (plist-get tol :calls)))
       (and (plist-get verdict :note) (format "eas-perf %s: note: %s\n" (plist-get verdict :mode)
                                              (plist-get verdict :note)))))))

;;; Recording

(defun eas-perf-record (baseline run &optional date)
  "BASELINE with RUN (JSON shape) recorded as its mode, on DATE."
  (let* ((mode (plist-get run :mode))
         (modes (eas--plist-without (plist-get baseline :modes) (intern (concat ":" mode))))
         (entry (list :recorded (list :date (or date (format-time-string "%F" nil t))
                                      :version (eas-perf-version)
                                      :emacs (plist-get run :emacs)
                                      :system (symbol-name system-type))
                      :frames (plist-get run :frames)
                      :workloads (plist-get run :workloads))))
    (list :contract "eas-perf-baseline/v1"
          :note (or (plist-get baseline :note)
                    "Recorded by make bench-record; gated by make bench-check (alloc and calls only).")
          :tolerance (or (plist-get baseline :tolerance) eas-perf-default-tolerance)
          :modes (append modes (list (intern (concat ":" mode)) entry)))))

(defun eas-perf--summary (workloads)
  "Per live workload of WORKLOADS, its alloc and median ms on each target.
Each is (:name N :svg B :text B :svg-ms MS :text-ms MS)."
  (vconcat
   (cl-loop for w across (vconcat workloads)
            unless (string-prefix-p "render/" (plist-get w :name))
            collect (list :name (plist-get w :name)
                          :svg (or (plist-get (plist-get w :svg) :alloc-bytes) :null)
                          :text (or (plist-get (plist-get w :text) :alloc-bytes) :null)
                          :svg-ms (or (plist-get (plist-get w :svg) :ms-median) :null)
                          :text-ms (or (plist-get (plist-get w :text) :ms-median) :null)))))

(defun eas-perf--render-total (workloads target key)
  "Sum of metric KEY over the render/ WORKLOADS on TARGET."
  (cl-loop for w across (vconcat workloads)
           for m = (plist-get w target)
           when (and (string-prefix-p "render/" (plist-get w :name)) (numberp (plist-get m key)))
           sum (plist-get m key)))

(defun eas-perf-history-add (history baseline mode)
  "HISTORY (a vector) with BASELINE's MODE as the row of its version.
A row for the same version and mode is replaced."
  (let* ((entry (eas-perf--mode baseline mode))
         (rec (plist-get entry :recorded))
         (ws (plist-get entry :workloads))
         (row (list :version (plist-get rec :version) :mode mode :date (plist-get rec :date)
                    :live (eas-perf--summary ws)
                    :render (list :svg (eas-perf--render-total ws :svg :alloc-bytes)
                                  :text (eas-perf--render-total ws :text :alloc-bytes)
                                  :svg-ms (eas-scene-round (eas-perf--render-total ws :svg :ms-median))
                                  :text-ms (eas-scene-round (eas-perf--render-total ws :text :ms-median))))))
    (vconcat (seq-remove (lambda (r) (and (equal (plist-get r :version) (plist-get row :version))
                                          (equal (plist-get r :mode) mode)))
                         history)
             (list row))))

;;; The report

(defun eas-perf--mode-section (mode entry)
  "The docs/perf.md section of baseline ENTRY for MODE."
  (let* ((rec (plist-get entry :recorded)) (ws (append (plist-get entry :workloads) nil))
         (live (seq-remove (lambda (w) (string-prefix-p "render/" (plist-get w :name))) ws))
         (renders (seq-filter (lambda (w) (string-prefix-p "render/" (plist-get w :name))) ws))
         (cells (lambda (w target)
                  (let ((m (plist-get w target)))
                    (if (plist-get m :error) (list "error" "" "" "")
                      (list (eas-perf--kb (plist-get m :alloc-bytes))
                            (format "%s" (or (plist-get m :conses) "-"))
                            (eas-perf--calls m) (eas-perf--ms m))))))
         (table (lambda (rows)
                  (eas-perf-markdown-table
                   '("workload" "target" "KB/frame" "conses/frame" "calls" "ms median/p95")
                   (cl-loop for w in rows
                            append (cl-loop for target in '(:svg :text)
                                            when (plist-get w target)
                                            collect (append (list (plist-get w :name) (substring (symbol-name target) 1))
                                                            (funcall cells w target))))))))
    (concat
     (format "## %s-compiled\n\nRecorded %s, eas %s, Emacs %s on %s.\n\n"
             mode (plist-get rec :date) (plist-get rec :version) (plist-get rec :emacs) (plist-get rec :system))
     "### Live workloads\n\n" (funcall table live)
     (format "\n### First render (%d templates)\n\n" (length renders))
     (format "Totals: SVG %s KB, %.0f ms; text %s KB, %.0f ms.  Heaviest by SVG allocation:\n\n"
             (eas-perf--kb (eas-perf--render-total ws :svg :alloc-bytes))
             (eas-perf--render-total ws :svg :ms-median)
             (eas-perf--kb (eas-perf--render-total ws :text :alloc-bytes))
             (eas-perf--render-total ws :text :ms-median))
     (funcall table (seq-take (sort (copy-sequence renders)
                                    (lambda (a b) (> (or (plist-get (plist-get a :svg) :alloc-bytes) 0)
                                                     (or (plist-get (plist-get b :svg) :alloc-bytes) 0))))
                              10))
     "\nEvery template is in bench/baseline.json.\n\n")))

(defun eas-perf--trend (history)
  "The trend tables of HISTORY rows."
  (let* ((rows (append history nil))
         (names (delete-dups (cl-loop for r in rows
                                      append (mapcar (lambda (l) (plist-get l :name)) (plist-get r :live)))))
         (cell (lambda (r name a b fmt)
                 (let ((l (seq-find (lambda (l) (equal (plist-get l :name) name)) (plist-get r :live))))
                   (if l (format "%s / %s" (funcall fmt (plist-get l a)) (funcall fmt (plist-get l b))) "-"))))
         (num (lambda (v) (if (numberp v) (format "%.1f" v) "-")))
         (kb (lambda (v) (if (numberp v) (eas-perf--kb v) "-")))
         (render (lambda (r a b fmt) (let ((x (plist-get r :render)))
                                       (format "%s / %s" (funcall fmt (plist-get x a)) (funcall fmt (plist-get x b)))))))
    (if (null rows) "No release has been recorded yet.\n"
      (concat
       "Allocation, KB per frame (SVG / text):\n\n"
       (eas-perf-markdown-table
        (append '("version" "mode" "date") names '("first render, all"))
        (mapcar (lambda (r) (append (list (plist-get r :version) (plist-get r :mode) (plist-get r :date))
                                    (mapcar (lambda (n) (funcall cell r n :svg :text kb)) names)
                                    (list (funcall render r :svg :text kb))))
                rows))
       "\nWall clock, median ms per frame (SVG / text), on the recording machine:\n\n"
       (eas-perf-markdown-table
        (append '("version" "mode" "date") names '("first render, all"))
        (mapcar (lambda (r) (append (list (plist-get r :version) (plist-get r :mode) (plist-get r :date))
                                    (mapcar (lambda (n) (funcall cell r n :svg-ms :text-ms num)) names)
                                    (list (funcall render r :svg-ms :text-ms num))))
                rows))))))

(defun eas-perf-report (baseline history)
  "The Markdown of docs/perf.md from BASELINE and HISTORY."
  (let ((tol (or (plist-get baseline :tolerance) eas-perf-default-tolerance)))
    (concat
     "# eas performance\n\n"
     "<!-- Generated by make bench-report from bench/baseline.json and bench/history.json; do not edit. -->\n\n"
     "The standing performance suite (src/eas-perf.el).  A frame is one update (a push, a\n"
     "timer tick, a pointermove, a resize, or opening a template) plus the redraw: the SVG\n"
     "string, or the text grid patched into a buffer.  Calls per frame are\n"
     "compiles/patches/scenes/renders/repaints.\n\n"
     (format "`make bench-check` fails when bytes allocated per frame grow more than %g%% or a\n"
             (* 100 (plist-get tol :alloc)))
     (format "call count more than %g%% over the baseline.  Wall-clock times are reported only.\n\n"
             (* 100 (plist-get tol :calls)))
     "```sh\nmake bench          # run the suite, compare with the baseline, never fail\n"
     "make bench-check    # the regression gate (CI: byte-compiled)\n"
     "make bench-record   # re-record bench/baseline.json, bench/history.json and this file\n"
     "make bench-report   # regenerate this file from the committed baseline\n```\n\n"
     (cl-loop for (key entry) on (plist-get baseline :modes) by #'cddr
              concat (eas-perf--mode-section (substring (symbol-name key) 1) entry))
     "## Trend per release\n\n"
     (eas-perf--trend history))))

;;; Batch entry points (scripts/eas-perf.sh)

(defun eas-perf-batch-run ()
  "Run the suite; write the run to the file named by the first argument.
A second argument is a regexp selecting workloads."
  (let* ((out (pop command-line-args-left)) (only (pop command-line-args-left))
         (frames (getenv "EAS_PERF_FRAMES")))
    (when (and frames (> (length frames) 0)) (setq eas-perf-frames (string-to-number frames)))
    (eas-perf-write out (eas-perf-run-json (eas-perf-run :only (and only (> (length only) 0) only)
                                                         :progress t)))))

(defun eas-perf-batch-check ()
  "Compare the run file (second argument) with the baseline (first).
Print the table; exit 1 on a regression, or when the baseline has no
entry for the run's mode, unless the third argument is \"report\".
\"partial\" (a run of some workloads) leaves out the ones not run."
  (let* ((baseline (eas-perf-read (pop command-line-args-left)))
         (run (eas-perf-read (pop command-line-args-left)))
         (how (pop command-line-args-left))
         (verdict (eas-perf-compare baseline run)))
    (when (equal how "partial")
      (setq verdict (plist-put verdict :rows (seq-remove (lambda (r) (equal (plist-get r :status) "missing"))
                                                         (plist-get verdict :rows)))))
    (princ (eas-perf-format-verdict verdict))
    (kill-emacs (if (or (equal how "report") (equal (plist-get verdict :status) "pass")) 0 1))))

(defun eas-perf-batch-record ()
  "Record the run (third argument) into the baseline and history files.
The arguments are BASELINE HISTORY RUN."
  (let* ((bfile (pop command-line-args-left)) (hfile (pop command-line-args-left))
         (run (eas-perf-read (pop command-line-args-left)))
         (baseline (eas-perf-record (or (eas-perf-read bfile) nil) run)))
    (eas-perf-write bfile baseline)
    (eas-perf-write hfile (eas-perf-history-add (or (eas-perf-read hfile) []) baseline (plist-get run :mode)))
    (message "eas-perf: recorded %s into %s and %s" (plist-get run :mode) bfile hfile)))

(defun eas-perf-batch-report ()
  "Write docs/perf.md (third argument) from BASELINE and HISTORY files."
  (let ((baseline (eas-perf-read (pop command-line-args-left)))
        (history (or (eas-perf-read (pop command-line-args-left)) []))
        (out (pop command-line-args-left)))
    (with-temp-file out (insert (eas-perf-report baseline history)))
    (message "eas-perf: wrote %s" out)))

(provide 'eas-perf-gate)
;;; eas-perf-gate.el ends here

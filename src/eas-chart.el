;;; eas-chart.el --- the static door: bin/chart build and diff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; bin/chart (chart-runtime) renders any resolved Vega-Lite
;; spec statically.  It is never a runtime dependency: eas draws every
;; chart in Lisp.  bin/chart is (a) the conformance oracle, dev/CI
;; only (its builds are the committed reference images), and (b) the
;; static export door (`export --vl', babel .png/.pdf).  Showing a
;; spec outside the native subset as a bin/chart image is opt-in
;; (`eas-static-fallback'); by default such a view shows its
;; UNSUPPORTED_FEATURE findings as text instead.  The command lines are
;; variables because they are an assumption here: adjust
;; `eas-chart-build-args' and `eas-chart-theme-args' if bin/chart's
;; flags differ.  Images are compared by `eas-png-compare', not
;; `bin/chart diff', which scores any canvas size mismatch as total.

;;; Code:

(require 'eas-core)

(defgroup eas-chart nil
  "The optional bin/chart static door of the eas chart engine."
  :group 'tools)

(defcustom eas-static-fallback nil
  "Non-nil to draw specs outside the native subset with bin/chart.
When nil (the default), eas never runs bin/chart to display a chart:
a view whose spec uses unsupported Vega-Lite features is static and
shows its UNSUPPORTED_FEATURE findings as text, and `render --backend
svg' fails with UNSUPPORTED_FEATURE.  Explicit static exports (babel
.png/.pdf) and the conformance oracle use bin/chart regardless."
  :type 'boolean
  :group 'eas-chart)

(defvar eas-chart-program "chart"
  "The bin/chart executable (name on PATH or absolute file).")

(defvar eas-chart-build-args
  (lambda (spec-file out-file) (list "build" spec-file "--out" out-file "--force"))
  "Function (SPEC-FILE OUT-FILE) -> argument list rendering a spec statically.
The output format follows OUT-FILE's extension (.svg or .png).  OUT-FILE
already exists (`make-temp-file' creates it), hence --force.")

(defvar eas-chart-theme-args '("theme" "--json")
  "Arguments printing bin/chart's default theme as {config, hash, source}.")

(defvar eas-chart-rsvg-program "rsvg-convert"
  "Rasterizer turning native SVG into PNG for oracle diffs.")

(defun eas-chart-available-p ()
  "Non-nil when bin/chart can be run."
  (and (executable-find eas-chart-program) t))

(defun eas-chart-missing-reason ()
  "Why the static door is unavailable, or nil."
  (cond ((not (executable-find eas-chart-program))
         (format "bin/chart (%s) is not on PATH; install bin/chart" eas-chart-program))
        (t nil)))

(defun eas-static-fallback-error (err)
  "The UNSUPPORTED_FEATURE plist a view shows instead of a static image.
ERR is the `eas-unsupported-feature' condition; the message adds how
to opt in to the bin/chart picture."
  (let ((plist (eas-error-plist err)))
    (plist-put plist :message
               (format "%s; set `eas-static-fallback' to t (with bin/chart on PATH) to draw a static image"
                       (plist-get plist :message)))))

(defun eas-chart--run (args &optional stdout-only)
  "Run bin/chart with ARGS; return (EXIT . OUTPUT).
OUTPUT includes stderr unless STDOUT-ONLY."
  (with-temp-buffer
    (let ((exit (apply #'call-process eas-chart-program nil (if stdout-only '(t nil) t) nil args)))
      (cons exit (buffer-string)))))

(defun eas-chart-build (spec format)
  "Render resolved SPEC statically with bin/chart as FORMAT (\"svg\" or \"png\").
Return the image data as a unibyte string.  Signals NOT_FOUND when
bin/chart is absent and ENGINE_FAILED when it fails."
  (when-let* ((reason (eas-chart-missing-reason)))
    (eas-signal "NOT_FOUND" reason :program eas-chart-program))
  (let ((in (make-temp-file "eas-spec" nil ".vl.json"))
        (out (make-temp-file "eas-static" nil (concat "." format))))
    (unwind-protect
        (progn
          (with-temp-file in (insert (eas-json-encode spec)))
          (let ((result (eas-chart--run (funcall eas-chart-build-args in out))))
            (unless (and (zerop (car result)) (> (file-attribute-size (file-attributes out)) 0))
              (eas-signal "ENGINE_FAILED" (format "bin/chart build failed: %s" (string-trim (cdr result)))
                            :exit (car result)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally out)
              (buffer-string))))
      (delete-file in)
      (delete-file out))))

(defun eas-chart-theme ()
  "bin/chart's default theme, parsed: (:config CONFIG :hash HASH ...).
Signals NOT_FOUND when bin/chart is absent and ENGINE_FAILED when it fails."
  (when-let* ((reason (eas-chart-missing-reason)))
    (eas-signal "NOT_FOUND" reason :program eas-chart-program))
  (let ((result (eas-chart--run eas-chart-theme-args t)))
    (unless (zerop (car result))
      (eas-signal "ENGINE_FAILED" (format "bin/chart theme failed: %s" (string-trim (cdr result)))
                    :exit (car result)))
    ;; `chart theme --json' prints the chart/v1 envelope:
    ;; {"data": {"config": {...}, "config_hash": "sha256:..."}}.
    (let* ((parsed (eas-json-parse (cdr result)))
           (data (or (plist-get parsed :data) parsed)))
      (list :config (plist-get data :config)
            :hash (or (plist-get data :config_hash) (plist-get data :hash))))))

(defun eas-chart-rasterize (svg png-file)
  "Rasterize SVG (a string) to PNG-FILE with `eas-chart-rsvg-program'."
  (let ((in (make-temp-file "eas-native" nil ".svg" svg)))
    (unwind-protect
        (unless (zerop (call-process eas-chart-rsvg-program nil nil nil "-o" png-file in))
          (eas-signal "ENGINE_FAILED" (format "%s failed on native SVG" eas-chart-rsvg-program)))
      (delete-file in))))

(provide 'eas-chart)
;;; eas-chart.el ends here

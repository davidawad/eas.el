;;; eas-agent-verbs.el --- stateless agent verbs: describe to export -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The verbs that need no live Emacs, so bin/eas runs them in batch:
;;
;;   describe [SECTION]                 registries, verbs, events, reasons
;;   example TEMPLATE                   bindings that render as-is
;;   check SOURCE [--data B]            reason codes, before drawing
;;   explain SOURCE --stage S           the artifact at resolve|compile|scene
;;   render SOURCE --backend text|svg   the chart (text: the agent's eyes)
;;   export SOURCE --vl                 resolved pure Vega-Lite for bin/chart
;;
;; SOURCE is a template name (bound with --data) or a chart/v1 spec
;; (inline JSON, a .json file, or "-" for stdin).

;;; Code:

(require 'eas-agent-core)
(require 'eas-describe)
(require 'eas-resolve)
(require 'eas-spec-props)
(require 'eas-compile)
(require 'eas-scene)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-event)
(require 'eas-chart)
(require 'eas-conformance)

(defconst eas-agent-backends '("text" "svg") "Render backends.")

(defconst eas-agent-stages '("resolve" "compile" "scene") "Stages of explain.")

(defconst eas-agent--source-options '(:data :backend :width :height :cols :rows)
  "Options shared by the verbs that take a SOURCE.")

(defun eas-agent-resolve-source (verb pos opts)
  "Resolve VERB's SOURCE (first of POS) with OPTS's bindings.
Return (:spec RESOLVED :template NAME-or-nil :label LABEL :words W),
W being how next[] commands name the source and its bindings."
  (let ((source (eas-agent-arg-required pos 0 "a template name or a chart/v1 spec" verb)))
    (if (eas-agent-template-name-p source)
        (list :spec (eas-resolve source (eas-agent-bindings opts)) :template source :label source
              :words (list source "--data" (eas-agent-data-word (plist-get opts :data))))
      (when (plist-get opts :data)
        (eas-signal "INVALID_INPUT"
                      (format "--data binds template slots, but %s is not a template; templates: %s"
                              (eas-agent-source-label source)
                              (string-join (eas-template-names) ", "))
                      :option "data"))
      (list :spec (let ((eas-spec-source-directory
                         ;; A spec file's data.url is relative to the file, as bin/chart reads it.
                         (if (and (stringp source) (not (eas-agent-json-text-p source))
                                  (not (equal source "-")) (file-readable-p source))
                             (file-name-directory (expand-file-name source))
                           eas-spec-source-directory)))
                    (eas-resolve-spec (eas-agent-arg-json source)))
            :template nil
            :label (eas-agent-source-label source)
            :words (list (eas-agent-source-label source))))))

(defun eas-agent-data-word (data)
  "How next[] commands pass bindings DATA: its file, else BINDINGS.json."
  (if (and (stringp data) (not (equal data "-")) (not (eas-agent-json-text-p data)))
      data "BINDINGS.json"))

(defun eas-agent-next (verb src &rest words)
  "A next[] command running VERB on SRC's source (and bindings) with WORDS."
  (apply #'eas-agent-cmd verb (append (plist-get src :words) words)))

(defun eas-agent-size (opts backend)
  "The compile SIZE that OPTS ask for under BACKEND, or nil (spec's own)."
  (let ((w (eas-agent-arg-number opts :width)) (h (eas-agent-arg-number opts :height))
        (c (eas-agent-arg-number opts :cols)) (r (eas-agent-arg-number opts :rows)))
    (cond ((and c r (equal backend "text")) (list :cols c :rows r))
          ((and w h) (cons w h))
          ((or w h c r)
           (eas-signal "INVALID_INPUT"
                         (if (equal backend "text") "Give --cols and --rows together (or --width and --height)"
                           "Give --width and --height together; --cols/--rows are for --backend text")
                         :option "size")))))

(defun eas-agent-compile (spec opts backend)
  "Compile resolved SPEC for BACKEND at OPTS's size; return the scene."
  (eas-compile spec :size (eas-agent-size opts backend) :target (intern backend)))

(defun eas-agent-draw (scene backend)
  "SCENE drawn by BACKEND as a plain string."
  (if (equal backend "text") (substring-no-properties (eas-text-render scene))
    (eas-svg-render scene)))

;;; describe

(defun eas-agent--verbs-describe ()
  "Return every verb as describe data."
  (vconcat (mapcar (lambda (name)
                     (let ((e (eas-agent-verb name)))
                       (list :name name :doc (plist-get e :doc) :usage (plist-get e :usage)
                             :live (if (plist-get e :live) t :false)
                             :options (vconcat (mapcar #'eas-key-name (plist-get e :options))))))
                   (eas-agent-verb-names))))

(defun eas-agent--describe-extra ()
  "The agent surface's own describe sections."
  (list :contract eas-agent-contract
        :verbs (eas-agent--verbs-describe)
        :events (list :types (vconcat eas-event-types) :keys (vconcat eas-event-keys))
        :reasons (vconcat (mapcar (lambda (e) (list :code (car e) :title (nth 2 e)))
                                  eas-reason-codes))))

(defun eas-agent-describe (pos _opts)
  "Answer describe [SECTION] (POS): registries, verbs, events, reasons."
  (let* ((section (car pos))
         (extra (eas-agent--describe-extra))
         (key (and section (eas-key (format "%s" section))))
         (first (car (eas-template-names))))
    (eas-agent-ok (cond ((null section) (append (eas-describe) extra))
                          ((plist-member extra key) (list key (plist-get extra key)))
                          (t (eas-describe section)))
                    (and first (eas-agent-cmd "example" first))
                    (eas-agent-cmd "check" "SPEC.json"))))

;;; example

(defun eas-agent-example (pos _opts)
  "Answer example TEMPLATE (first of POS): bindings that render as-is."
  (let* ((name (eas-agent-arg-required pos 0 "a template name" "example"))
         (bindings (eas-template-example name))
         (file (eas-template-example-file (eas-template-get name))))
    (eas-agent-ok bindings
                    (eas-agent-cmd "check" name "--data" file)
                    (eas-agent-cmd "render" name "--data" file "--backend" "text"))))

;;; check

(defun eas-agent--check-findings (spec)
  "SPEC's check findings as (ERRORS . UNSUPPORTED)."
  (let (errors unsupported)
    (dolist (f (eas-spec-check spec))
      (if (equal (plist-get f :code) "UNSUPPORTED_FEATURE") (push f unsupported) (push f errors)))
    (cons (nreverse errors) (nreverse unsupported))))

(defun eas-agent-check (pos opts)
  "Answer check SOURCE (POS, bound with OPTS :data) without drawing.
Resolve, validate and compile.
Unsupported features are not failures: the chart still opens, as a
static view (a bin/chart image only when `eas-static-fallback' is
non-nil), so they come back as warnings with native false."
  (let* ((src (eas-agent-resolve-source "check" pos opts))
         (spec (plist-get src :spec))
         (findings (eas-agent--check-findings spec))
         (errors (car findings)) (unsupported (cdr findings))
         (data (list :template (or (plist-get src :template) :null)
                     :hash (eas-resolve-hash spec)
                     :native (if unsupported :false t)
                     ;; Properties drawn without (eas-spec-props.el) warn but stay native.
                     :warnings (vconcat unsupported (eas-spec-props-findings spec)))))
    (cond
     (errors
      (eas-agent-fail (plist-get (car errors) :code) (car errors)
                        (append data (list :findings (vconcat errors)))
                        (eas-agent-next "explain" src "--stage" "resolve")
                        (eas-agent-cmd "describe" "supported")))
     (unsupported
      (eas-agent-ok data (eas-agent-next "export" src "--vl")
                      (eas-agent-cmd "describe" "supported")))
     (t
      (eas-agent-compile spec opts "text")
      (eas-agent-ok data
                      (eas-agent-next "render" src "--backend" "text")
                      (eas-agent-next "explain" src "--stage" "scene"))))))

;;; explain

(defun eas-agent--scene-compiled (scene)
  "Return the compile stage of SCENE: views, scales and item counts."
  (let ((summary (eas-scene-summary scene)))
    (eas-scene-round
     (list :size (plist-get summary :size)
           :views (vconcat
                   (seq-mapn (lambda (sv v)
                               (append sv (list :scales
                                                (cl-loop for (ch s) on (plist-get v :scales) by #'cddr
                                                         append (list ch s)))))
                             (plist-get summary :views) (plist-get scene :views)))
           :params (vconcat (mapcar (lambda (p) (list :name (plist-get p :name) :view (plist-get p :view)))
                                    (plist-get scene :params)))))))

(defun eas-agent-explain (pos opts)
  "Answer explain SOURCE (POS) with OPTS :stage: one layer's artifact.
The stage is resolve, compile or scene."
  (let* ((stage (eas-agent-arg-choice opts :stage eas-agent-stages "resolve"))
         (backend (eas-agent-arg-choice opts :backend eas-agent-backends "svg"))
         (src (eas-agent-resolve-source "explain" pos opts))
         (spec (plist-get src :spec))
         (artifact (pcase stage
                     ("resolve" spec)
                     ("compile" (eas-agent--scene-compiled (eas-agent-compile spec opts backend)))
                     (_ (eas-scene-round (eas-agent-compile spec opts backend))))))
    (eas-agent-ok (list :stage stage :backend backend :template (or (plist-get src :template) :null)
                          :hash (eas-resolve-hash spec) :artifact artifact)
                    (eas-agent-next "render" src "--backend" "text")
                    (unless (equal stage "scene") (eas-agent-next "explain" src "--stage" "scene")))))

;;; render

(defun eas-agent-render (pos opts)
  "Answer render SOURCE (POS) with OPTS :backend text or svg.
A spec outside the native subset fails with UNSUPPORTED_FEATURE; with
`eas-static-fallback' non-nil and bin/chart installed, an svg render
comes from bin/chart instead."
  (let* ((backend (eas-agent-arg-choice opts :backend eas-agent-backends "text"))
         (src (eas-agent-resolve-source "render" pos opts))
         (spec (plist-get src :spec))
         (out (condition-case err
                  (let ((scene (eas-agent-compile spec opts backend)))
                    (list :output (eas-agent-draw scene backend) :size (plist-get scene :size)
                          :static :false))
                (eas-unsupported-feature
                 (if (and eas-static-fallback (equal backend "svg") (eas-chart-available-p))
                     (list :output (eas-chart-build spec "svg") :size :null :static "bin/chart")
                   (signal (car err) (cdr err)))))))
    (eas-agent-ok (append (list :backend backend :template (or (plist-get src :template) :null)
                                  :hash (eas-resolve-hash spec))
                            out)
                    (eas-agent-next "export" src "--vl")
                    (eas-agent-next "explain" src "--stage" "scene")
                    (and (plist-get src :template)
                         (eas-agent-live-cmd "open" (plist-get src :template)
                                               :data (nth 2 (plist-get src :words)))))))

;;; export

(defun eas-agent-export (pos opts)
  "Answer export SOURCE (POS, OPTS): the resolved, standalone Vega-Lite."
  (let* ((src (eas-agent-resolve-source "export" pos opts))
         (spec (plist-get src :spec)))
    (eas-agent-ok spec
                    "chart check SPEC.vl.json"
                    "chart build SPEC.vl.json --out chart.svg")))

(eas-agent-register-verb
 "describe" #'eas-agent-describe
 :doc "Templates with slots and examples, transforms, adapters, supported features, verbs, events, reasons"
 :usage "describe [templates|transforms|adapters|supported|verbs|events|reasons]")
(eas-agent-register-verb
 "example" #'eas-agent-example :doc "Bindings for TEMPLATE that render as-is"
 :usage "example TEMPLATE")
(eas-agent-register-verb
 "check" #'eas-agent-check :doc "Resolve, validate and compile without drawing; reason codes with paths"
 :usage "check SOURCE [--data BINDINGS]" :options '(:data))
(eas-agent-register-verb
 "explain" #'eas-agent-explain :doc "The intermediate artifact at one layer"
 :usage "explain SOURCE [--data B] --stage resolve|compile|scene [--backend svg|text] [--width W --height H]"
 :options (cons :stage eas-agent--source-options))
(eas-agent-register-verb
 "render" #'eas-agent-render :doc "The chart as text (deterministic) or SVG"
 :usage "render SOURCE [--data B] [--backend text|svg] [--cols C --rows R | --width W --height H]"
 :options eas-agent--source-options)
(eas-agent-register-verb
 "export" #'eas-agent-export :doc "Resolved pure Vega-Lite for bin/chart and documents"
 :usage "export SOURCE [--data B] --vl" :options '(:data :vl) :flags '(:vl))

(provide 'eas-agent-verbs)
;;; eas-agent-verbs.el ends here

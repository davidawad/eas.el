;;; eas-babel.el --- org-babel: #+begin_src eas -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L7 (fc-qx1.13).  One org chart block type:
;;
;;   #+begin_src eas :template ohlc :data tbl
;;   {"title": "TSM"}
;;   #+end_src
;;
;; :template names a template; the body, when present, is a JSON
;; object of extra bindings (or, without :template, a whole chart/v1
;; spec).  :data binds the template's data slot from an org table, a
;; named table or babel result (through the org-table adapter), or a
;; .json/.csv/.tsv file.  Any other header named like a slot binds it
;; (:title "TSM", :points true), and :var NAME=REF binds slot NAME.
;;
;; What the block yields:
;;
;;   :file F :results file   F by extension: .svg native SVG, .json
;;                           resolved pure Vega-Lite, .txt the text
;;                           chart, .png/.pdf through bin/chart.
;;   :as view (default)      the text chart as the result, and a live
;;                           view inline (eas-babel-inline.el): the
;;                           SVG over it in GUI frames, the text itself
;;                           in terminals.  Clicking a mark jumps to its
;;                           source row (eas-babel-source.el).
;;   :as text                the text chart only.
;;   :as vl                  resolved pure Vega-Lite; with
;;                           :wrap src vega-lite it feeds ob-vega, and it
;;                           is what render's ::: {.chart} and bin/chart
;;                           take.
;;
;; While exporting no view opens, and :as defaults to
;; `eas-babel-export-as'.  :cols/:rows size the text chart and
;; :width/:height the SVG; both default to the spec's own size.

;;; Code:

(require 'eas)
(require 'ob-core)

(declare-function org-babel-ref-resolve "ob-ref" (ref))

(defvar org-export-current-backend)

(defvar org-babel-default-header-args:eas '((:results . "verbatim"))
  "Default header arguments for eas blocks.")

(defvar eas-babel-export-as "text"
  "What an eas block without :file yields while exporting: text or vl.")

(defvar eas-babel-target nil
  "Target of block views: nil picks svg in graphic frames, else text.")

(defconst eas-babel--reserved
  '(:data :template :as :cols :rows :width :height :id :var :file :results :exports
    :result-params :result-type :session :cache :noweb :tangle :hlines :colnames
    :rownames :wrap :post :output-dir :file-desc :file-ext :file-mode :eval :dir :sep)
  "Header arguments that never bind a template slot.")

(defvar eas-babel-after-open-functions nil
  "Hook run with VIEW, SOURCE and BLOCK after a block opens a live view.
SOURCE is (:name REF :buffer B) when :data named an org table or
result, else nil; BLOCK is a marker at the block, or nil.")

;;; Data

(defun eas-babel--file-adapter (file)
  "The adapter name for data FILE by extension, or nil."
  (pcase (downcase (or (file-name-extension file) ""))
    ("json" "json") ("csv" "csv") ("tsv" "tsv")))

(defun eas-babel--resolve-ref (ref)
  "The value of org REF (a table, named table, result or src block name)."
  (require 'ob-ref)
  (condition-case err
      (org-babel-ref-resolve ref)
    (error (eas-signal "NOT_FOUND"
                         (format "No org table, result or block named %s here (%s); add #+name: %s above it"
                                 ref (error-message-string err) ref)
                         :table ref))))

(defun eas-babel-data (value)
  "VALUE (a :data or :var value) as data/v1.
A string names a .json/.csv/.tsv file or an org reference; a list of
rows (header first, `hline' allowed) is an org table or babel result;
a list of plists is plist rows."
  (cond
   ((eas-data-p value) value)
   ((and (stringp value) (eas-babel--file-adapter value))
    (unless (file-exists-p value)
      (eas-signal "NOT_FOUND" (format "No data file %s (from %s); fix the :data path"
                                        value default-directory)
                    :file value))
    (eas-data-from (eas-babel--file-adapter value) (list :file (expand-file-name value))))
   ((stringp value) (eas-babel-data (eas-babel--resolve-ref value)))
   ((and (consp value) (eas-object-p (car value))) (eas-data-from "plist" value))
   ((and (consp value) (or (listp (car value)) (eq (car value) 'hline)))
    (eas-data-from "org-table" value))
   (t (eas-signal "INVALID_INPUT"
                    (format "Cannot read chart data from %S; give :data a table name or a .json/.csv file"
                            value)
                    :header "data"))))

(defun eas-babel--slot-value (def value)
  "VALUE for a data slot with DEF: rows, or the raw table for org-table slots."
  (if (equal (plist-get def :shape) "org-table") value
    (plist-get (eas-babel-data value) :rows)))

(defun eas-babel-data-slot (template)
  "Name (a keyword) of TEMPLATE's data slot: the first required shape slot."
  (let ((slots (eas-template-slots template)) first)
    (or (cl-loop for (slot def) on slots by #'cddr
                 when (plist-get def :shape)
                 do (setq first (or first slot))
                 and when (eas-true-p (plist-get def :required)) return slot)
        first
        (eas-signal "INVALID_INPUT"
                      (format "Template %s has no data slot for :data" (plist-get template :name))
                      :header "data"))))

;;; Bindings

(defun eas-babel--scalar (def value)
  "Header VALUE (as org read it) coerced to slot DEF's type."
  (pcase (plist-get def :type)
    ("boolean" (cond ((memq value '(t :false)) value)
                     ((member (format "%s" value) '("true" "yes" "t" "1")) t)
                     (t :false)))
    ("string" (format "%s" value))
    (_ value)))

(defun eas-babel--body (body)
  "BODY parsed as JSON, or nil when blank."
  (let ((text (string-trim (or body ""))))
    (unless (string-empty-p text)
      (condition-case err
          (eas-json-parse text)
        (eas-error (eas-signal "PARSE_ERROR"
                                   (format "The block body is not JSON (%s); give bindings as a JSON object"
                                           (plist-get (eas-error-plist err) :message))
                                   :path "body"))))))

(defun eas-babel-bindings (template params body)
  "Bindings for TEMPLATE (a plist) from org PARAMS and the JSON BODY."
  (let* ((slots (eas-template-slots template))
         (bindings (eas-babel--body body)))
    (unless (eas-object-p bindings)
      (eas-signal "INVALID_INPUT" "With :template, the block body is a JSON object of bindings"
                    :path "body"))
    (pcase-dolist (`(,key . ,value) params)
      (cond
       ((and (eq key :var) (consp value) (plist-member slots (eas-key (car value))))
        (let* ((slot (eas-key (car value))) (def (plist-get slots slot)))
          (setq bindings (eas-plist-put bindings slot
                                          (if (plist-get def :shape)
                                              (eas-babel--slot-value def (cdr value))
                                            (cdr value))))))
       ((and (not (memq key eas-babel--reserved)) (plist-member slots key))
        (setq bindings (eas-plist-put bindings key (eas-babel--scalar (plist-get slots key) value))))))
    (when-let* ((data (cdr (assq :data params))))
      (let ((slot (eas-babel-data-slot template)))
        (setq bindings (eas-plist-put bindings slot
                                        (eas-babel--slot-value (plist-get slots slot) data)))))
    bindings))

(defun eas-babel-spec (params body)
  "Return the block's resolved Vega-Lite and template name: (SPEC . NAME).
PARAMS are the block's header arguments.  NAME is nil when BODY is a
whole chart/v1 spec."

  (let ((name (cdr (assq :template params))))
    (if name
        (let* ((name (format "%s" name)) (template (eas-template-get name)))
          (cons (eas-resolve template (eas-babel-bindings template params body)) name))
      (let ((spec (eas-babel--body body)))
        (unless spec
          (eas-signal "INVALID_INPUT"
                        (format "Give :template (one of %s) or a chart/v1 spec as the body"
                                (string-join (eas-template-names) ", "))
                        :header "template"))
        (cons (eas-resolve-spec spec) nil)))))

(defun eas-babel--rows (params)
  "Root rows from :data in PARAMS for a plain spec, or nil."
  (when-let* ((data (cdr (assq :data params))))
    (plist-get (eas-babel-data data) :rows)))

;;; Output

(defun eas-babel--number (params key)
  "Header KEY of PARAMS as a number, or nil."
  (let ((v (cdr (assq key params))))
    (cond ((numberp v) v)
          ((and (stringp v) (string-match-p "\\`[0-9]+\\'" v)) (string-to-number v)))))

(defun eas-babel-size (params target)
  "Compile size PARAMS ask for under TARGET (text or svg), or nil."
  (let ((c (eas-babel--number params :cols)) (r (eas-babel--number params :rows))
        (w (eas-babel--number params :width)) (h (eas-babel--number params :height)))
    (if (eq target 'text)
        (and c r (list :cols c :rows r))
      (and w h (cons w h)))))

(defun eas-babel-text (spec params &optional rows)
  "SPEC (with root ROWS) drawn as the deterministic text chart.
PARAMS give the size."
  (substring-no-properties
   (eas-text-render (eas-compile spec :rows rows :target 'text
                                     :size (eas-babel-size params 'text)))))

(defun eas-babel--write (file data &optional binary)
  "Write DATA to FILE (unibyte when BINARY)."
  (let ((coding-system-for-write (if binary 'binary 'utf-8)))
    (write-region data nil file nil 'silent)))

(defun eas-babel-write-file (file spec params &optional rows)
  "Write SPEC (with root ROWS) to FILE in the format its extension names."
  (let ((ext (downcase (or (file-name-extension file) ""))))
    (pcase ext
      ("svg" (eas-babel--write file (eas-svg-render
                                       (eas-compile spec :rows rows
                                                      :size (eas-babel-size params 'svg)))))
      ("json" (eas-babel--write file (concat (eas-json-pretty spec) "\n")))
      ("txt" (eas-babel--write file (eas-babel-text spec params rows)))
      ((or "png" "pdf") (eas-babel--write file (eas-chart-build spec ext) t))
      (_ (eas-signal "INVALID_INPUT"
                       (format "Cannot write %s; use a .svg, .json, .txt, .png or .pdf :file" file)
                       :header "file")))))

(defun eas-babel--exporting-p ()
  "Non-nil while org exports."
  (and (boundp 'org-export-current-backend) org-export-current-backend t))

(defun eas-babel-as (params)
  "What the block with header PARAMS yields: \"view\", \"text\" or \"vl\"."
  (let ((as (format "%s" (or (cdr (assq :as params))
                             (if (eas-babel--exporting-p) eas-babel-export-as "view")))))
    (unless (member as '("view" "text" "vl"))
      (eas-signal "INVALID_INPUT" (format ":as %s is not one of view, text, vl" as) :header "as"))
    (if (and (equal as "view") (eas-babel--exporting-p)) "text" as)))

(defun eas-babel-source (params)
  "Where the block's :data in PARAMS came from: (:name REF :buffer B), or nil."
  (let ((data (cdr (assq :data params))))
    (when (and (stringp data) (not (eas-babel--file-adapter data)))
      (list :name data :buffer (current-buffer)))))

(defun eas-babel--block ()
  "Marker at the block being executed, or nil."
  (let ((loc (and (boundp 'org-babel-current-src-block-location)
                  org-babel-current-src-block-location)))
    (cond ((markerp loc) loc)
          ((integerp loc) (copy-marker loc)))))

(defun eas-babel-view-id (template params)
  "Id of a block's view: TEMPLATE:NAME, NAME its :id, #+name or :data.
PARAMS are the block's header arguments."
  (format "%s:%s" (or template "chart")
          (or (cdr (assq :id params))
              (when-let* ((block (eas-babel--block)))
                (with-current-buffer (marker-buffer block)
                  (save-excursion
                    (goto-char block)
                    (nth 4 (org-babel-get-src-block-info 'no-eval)))))
              (let ((d (cdr (assq :data params)))) (and (stringp d) (file-name-base d)))
              "org")))

(defun eas-babel-open (spec template params rows)
  "Open a live view of resolved SPEC (with root ROWS) for a block.
TEMPLATE and PARAMS name the view as in `eas-babel-view-id'.
Re-running the block replaces its view.  Return the view."

  (let ((id (eas-babel-view-id template params)))
    (when (gethash id eas-views) (eas-view-close id))
    (let* ((target (or eas-babel-target
                       (if (and (display-graphic-p) (image-type-available-p 'svg)) 'svg 'text)))
           (view (eas-view-open spec :id id :rows rows :target target
                                  :size (eas-babel-size params target))))
      (setf (eas-view-template view) template)
      (run-hook-with-args 'eas-babel-after-open-functions view (eas-babel-source params)
                          (eas-babel--block))
      view)))

;;;###autoload
(defun org-babel-execute:eas (body params)
  "Execute an eas block: JSON BODY under header PARAMS.
See the commentary of eas-babel.el for the header arguments."
  (let ((file (cdr (assq :file params))))
    (when (and file (not (member "file" (cdr (assq :result-params params)))))
      (eas-signal "INVALID_INPUT" ":file needs :results file (as with ob-dot)" :header "file"))
    (eas-babel--execute body params file)))

(defun eas-babel--execute (body params file)
  "Run a block (BODY, PARAMS) writing FILE when non-nil; return its result."
  (pcase-let* ((as (eas-babel-as params))
               (`(,spec . ,template) (eas-babel-spec params body))
               (rows (and (null template) (eas-babel--rows params))))
    (cond
     (file
      (eas-babel-write-file file spec params rows)
      nil)
     (t
      (pcase as
        ("vl" (eas-json-pretty spec))
        ("text" (eas-babel-text spec params rows))
        (_ (let ((view (eas-babel-open spec template params rows)))
             (cond ((not (eas-view-interactive view))
                    (mapconcat (lambda (w) (format "Static chart: %s unsupported at %s"
                                                   (or (plist-get w :feature) (plist-get w :code))
                                                   (plist-get w :path)))
                               (eas-view-warnings view) "\n"))
                   ((eq (eas-view-target view) 'text)
                    (substring-no-properties (eas-text-render (eas-view-scene view))))
                   (t (eas-babel-text spec params rows))))))))))

(provide 'eas-babel)
;;; eas-babel.el ends here

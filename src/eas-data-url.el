;;; eas-data-url.el --- data.url, data.sequence and format.parse, inlined -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L0 for Vega-Lite's own data sources.  Compile reads only inline
;; data.values, so `eas-data-url-inline' (an `eas-spec-rewrite-functions'
;; entry, run by `eas-spec-parse') turns every
;;
;;   {"url": "data/x.csv", "format": {"type": "csv", "parse": {...}}}
;;   {"sequence": {"start": 0, "stop": 10, "step": 1, "as": "x"}}
;;
;; in a spec into {"values": ROWS}.  URLs are local files: absolute, or
;; relative to `eas-data-url-directory' (the spec file's directory when
;; parse read a file, else `default-directory'), the way bin/chart
;; resolves them next to the spec.  The format type comes from
;; format.type or the extension (csv, tsv, json); cells that read as
;; numbers become numbers, as Vega-Lite's inferred parse does.
;; format.parse converts named fields: "number", "boolean", "date",
;; "date:'FMT'" (local time) and "utc:'FMT'" (UTC), d3 time formats,
;; dates becoming epoch milliseconds.  Parsed files are cached by
;; name, modification time, format and zone.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-adapters)

(defvar eas-spec-source-directory)
(declare-function eas-topojson-rows "eas-topojson")

(defvar eas-data-url-directory nil
  "Directory relative data URLs resolve against; nil means `default-directory'.")

(defvar eas-data-url--cache (make-hash-table :test 'equal)
  "(FILE MTIME FORMAT ZONE) -> rows vector.")

(defun eas-data-url--file (url)
  "The local file URL names, or signal NOT_FOUND."
  (when (string-match-p "\\`[a-z]+://" (replace-regexp-in-string "\\`file://" "" url))
    (eas-signal "UNSUPPORTED_FEATURE"
                (format "data.url %s is remote; eas reads local files only, save it next to the spec" url)
                :feature "data/url-remote" :path "/data/url"))
  (let ((file (expand-file-name (replace-regexp-in-string "\\`file://" "" url)
                                (or eas-data-url-directory eas-spec-source-directory default-directory))))
    (unless (file-readable-p file)
      (eas-signal "NOT_FOUND" (format "data.url %s: no readable file %s; the path is relative to the spec" url file)
                  :path "/data/url"))
    file))

(defun eas-data-url--type (url format)
  "The format type of URL under FORMAT: csv, tsv or json."
  (or (plist-get format :type)
      (pcase (downcase (or (file-name-extension url) ""))
        ("csv" "csv") ((or "tsv" "txt") "tsv") (_ "json"))))

;;; d3 time format parsing

(defconst eas-data-url--months
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
  "Month abbreviations, as %b and %B read them.")

(defconst eas-data-url--directives
  '((?Y . "\\([-+]?[0-9]\\{1,4\\}\\)") (?y . "\\([0-9]\\{2\\}\\)")
    (?m . "\\([0-9]\\{1,2\\}\\)") (?d . "\\([0-9]\\{1,2\\}\\)") (?e . " ?\\([0-9]\\{1,2\\}\\)")
    (?H . "\\([0-9]\\{1,2\\}\\)") (?M . "\\([0-9]\\{1,2\\}\\)") (?S . "\\([0-9]\\{1,2\\}\\)")
    (?L . "\\([0-9]\\{3\\}\\)") (?b . "\\([A-Za-z]\\{3\\}\\)") (?B . "\\([A-Za-z]+\\)"))
  "D3 time-format directives `eas-data-url-parse-time' reads.")


(defun eas-data-url--format-regexp (format)
  "(REGEXP . DIRECTIVES) matching d3 time FORMAT."
  (let ((regexp "\\`") (fields nil) (i 0) (n (length format)))
    (while (< i n)
      (let ((c (aref format i)))
        (if (and (eq c ?%) (< (1+ i) n))
            (let ((d (aref format (1+ i))))
              ;; Padding modifiers (%-d, %_d, %0d) read the same digits.
              (when (and (memq d '(?- ?_ ?0)) (< (+ 2 i) n))
                (setq i (1+ i) d (aref format (1+ i))))
              (let ((re (cdr (assq d eas-data-url--directives))))
                (unless re
                  (eas-signal "UNSUPPORTED_FEATURE" (format "Time format directive %%%c is not supported" d)
                              :feature "format/parse"))
                (setq regexp (concat regexp re) fields (cons d fields) i (+ i 2))))
          (setq regexp (concat regexp (regexp-quote (string c))) i (1+ i)))))
    (cons (concat regexp "\\'") (nreverse fields))))

(defun eas-data-url-parse-time (format value utc)
  "Parse string VALUE with d3 time FORMAT; epoch ms, or `:null' on mismatch.
Local time (`eas-time-zone') unless UTC is non-nil."
  (let ((re (eas-data-url--format-regexp format)))
    (if (not (and (stringp value) (string-match (car re) value))) :null
      (let ((year 1900) (month 1) (day 1) (h 0) (mi 0) (s 0) (ms 0) (k 0))
        (dolist (d (cdr re))
          (setq k (1+ k))
          (let* ((m (match-string k value)) (v (string-to-number m)))
            (pcase d
              (?Y (setq year v))
              (?y (setq year (+ v (if (< v 69) 2000 1900))))
              (?m (setq month v))
              ((or ?d ?e) (setq day v))
              (?H (setq h v)) (?M (setq mi v)) (?S (setq s v)) (?L (setq ms v))
              ((or ?b ?B) (setq month (1+ (or (seq-position eas-data-url--months (downcase (substring m 0 3))) 0)))))))
        (let ((eas-time-zone (unless utc eas-time-zone)))
          (eas-time-ms year month day h mi s ms))))))

(defun eas-data-url--parse-value (how value)
  "VALUE converted per format.parse HOW."
  (cond
   ((memq value '(nil :null)) :null)
   ((equal how "number")
    (if (numberp value) value
      (let ((s (string-trim (format "%s" value))))
        (if (string-match-p eas-adapters--number-regexp s) (string-to-number s) :null))))
   ((equal how "boolean") (if (member value '("true" t 1 "1")) t :false))
   ((equal how "date") (or (eas-time-parse value) :null))
   ((and (stringp how) (string-match "\\`\\(date\\|utc\\):'\\(.*\\)'\\'" how))
    (eas-data-url-parse-time (match-string 2 how) (format "%s" value) (equal (match-string 1 how) "utc")))
   ((eq how :null) value)
   (t (eas-signal "UNSUPPORTED_FEATURE" (format "format.parse %S is not supported" how)
                  :feature "format/parse" :path "/data/format/parse"))))

(defun eas-data-url--apply-parse (rows parse)
  "ROWS with format.parse PARSE (an object of FIELD -> HOW) applied."
  (if (not (and parse (eas-object-p parse))) rows
    (seq-map (lambda (row)
               (let ((out row))
                 (cl-loop for (k how) on parse by #'cddr
                          when (plist-member out k)
                          do (setq out (eas-plist-put out k (eas-data-url--parse-value how (plist-get out k)))))
                 out))
             rows)))

(defun eas-data-url--read (file url format)
  "Rows of FILE (named by URL) read per FORMAT, before parsing."
  (pcase (eas-data-url--type url format)
    ("csv" (plist-get (eas-data-from "csv" (list :file file)) :rows))
    ("tsv" (plist-get (eas-data-from "tsv" (list :file file)) :rows))
    ("json" (let ((v (eas-json-read-file file)))
              (when-let* ((prop (plist-get format :property)))
                (dolist (k (split-string prop "\\.")) (setq v (plist-get v (eas-key k)))))
              (unless (vectorp v)
                (eas-signal "SHAPE_INVALID" (format "data.url %s is not a JSON array of rows" url)
                            :path "/data/url"))
              v))
    ;; Maps (eas-topojson.el): the features or mesh of one object.
    ("topojson" (eas-topojson-rows (eas-json-read-file file) format))
    (type (eas-signal "UNSUPPORTED_FEATURE" (format "data format %s is not supported natively" type)
                      :feature (concat "data/" type) :path "/data/format/type"))))

(defun eas-data-url-rows (data)
  "The rows of Vega-Lite DATA (:url U [:format F]) as a vector."
  (let* ((url (plist-get data :url)) (format (plist-get data :format))
         (file (eas-data-url--file url))
         (key (list file (file-attribute-modification-time (file-attributes file)) format eas-time-zone)))
    (or (gethash key eas-data-url--cache)
        (puthash key (vconcat (eas-data-url--apply-parse (eas-data-url--read file url format)
                                                         (plist-get format :parse)))
                 eas-data-url--cache))))

(defun eas-data-sequence-rows (seq)
  "Rows of a data.sequence generator SEQ (:start :stop [:step] [:as])."
  (let* ((start (or (plist-get seq :start) 0)) (stop (plist-get seq :stop))
         (step (or (plist-get seq :step) 1)) (as (eas-key (or (plist-get seq :as) "data")))
         (n (max 0 (ceiling (/ (- stop start) (float step))))))
    (vconcat (cl-loop for i below n collect (list as (+ start (* i step)))))))

(defun eas-data-url--inline-data (data)
  "DATA with url or sequence sources replaced by values."
  (cond
   ((not (and data (eas-object-p data))) data)
   ((stringp (plist-get data :url)) (list :values (eas-data-url-rows data)))
   ((and (plist-get data :sequence) (eas-object-p (plist-get data :sequence)))
    (list :values (eas-data-sequence-rows (plist-get data :sequence))))
   ((and (vectorp (plist-get data :values)) (plist-get (plist-get data :format) :parse))
    (list :values (vconcat (eas-data-url--apply-parse (plist-get data :values)
                                                      (plist-get (plist-get data :format) :parse)))))
   (t data)))

(defun eas-data-url-inline (spec)
  "SPEC with every data.url and data.sequence (at any level) made inline."
  (if (not (and spec (eas-object-p spec))) spec
    (let ((out spec))
      (when (plist-get spec :data)
        (setq out (eas-plist-put out :data (eas-data-url--inline-data (plist-get spec :data)))))
      (dolist (key '(:layer :vconcat :hconcat :concat))
        (when (vectorp (plist-get spec key))
          (setq out (eas-plist-put out key (vconcat (mapcar #'eas-data-url-inline (plist-get spec key)))))))
      (when (and (plist-get spec :spec) (eas-object-p (plist-get spec :spec)))
        (setq out (eas-plist-put out :spec (eas-data-url-inline (plist-get spec :spec)))))
      out)))

(provide 'eas-data-url)
;;; eas-data-url.el ends here

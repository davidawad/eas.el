;;; eas-adapters.el --- json, csv, tsv and bar/v1 adapters; appends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L0 adapters beyond plist rows.  Text adapters accept the text itself,
;; a file name, "-" for stdin, or (:file F) / (:text S) to be explicit.
;; `eas-data-append' is the streaming contract: live data and batch
;; data go through the same schema check.

;;; Code:

(require 'eas-data)

(defun eas-adapters--text (input)
  "Return the text INPUT designates: itself, a file's contents or stdin."
  (cond
   ((and (eas-object-p input) (plist-get input :text)) (plist-get input :text))
   ((and (eas-object-p input) (plist-get input :file))
    (eas-adapters--file (plist-get input :file)))
   ((equal input "-") (eas-adapters--file "/dev/stdin"))
   ((and (stringp input) (not (string-match-p "[\n{[]" input)) (file-exists-p input))
    (eas-adapters--file input))
   ((stringp input) input)
   (t (eas-shape-invalid "Give text, a file name, \"-\", (:file F) or (:text S)" nil))))

(defun eas-adapters--file (file)
  "Return FILE's contents or signal NOT_FOUND."
  (unless (file-readable-p file)
    (eas-signal "NOT_FOUND" (format "No readable file %s" file) :path file))
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;; json

(defun eas-adapters--json (input)
  "Convert JSON INPUT (rows array, {\"values\": rows} or data/v1) to data/v1."
  (let ((value (if (or (vectorp input) (and (eas-object-p input) (plist-get input :values)))
                   input
                 (eas-json-parse (eas-adapters--text input)))))
    (cond
     ((vectorp value) (eas-data-from "plist" value))
     ((eas-data-p value) (eas-data-from "data/v1" value))
     ((vectorp (plist-get value :values)) (eas-data-from "plist" (plist-get value :values)))
     (t (eas-shape-invalid
         "JSON data must be an array of row objects, {\"values\": [...]} or data/v1" nil)))))

(eas-register-adapter
 "json" :doc "JSON text, file or stdin: an array of rows, {\"values\": rows} or data/v1."
 :convert #'eas-adapters--json
 :example "[{\"x\": 1, \"y\": 2}, {\"x\": 2, \"y\": 3}]")

;;; csv / tsv

(defconst eas-adapters--number-regexp
  "\\`[-+]?\\(?:[0-9]+\\.?[0-9]*\\|\\.[0-9]+\\)\\(?:[eE][-+]?[0-9]+\\)?\\'"
  "A cell that reads as a number.")

(defun eas-adapters-parse-delimited (text separator)
  "Parse TEXT into a list of records (lists of strings) split on SEPARATOR.
Double-quoted cells may contain SEPARATOR, newlines and \"\" escapes."
  (let ((records nil) (record nil) (cell nil) (i 0) (n (length text)) (quoted nil))
    (cl-flet ((end-cell () (push (apply #'string (nreverse cell)) record) (setq cell nil))
              (end-record () (push (nreverse record) records) (setq record nil)))
      (while (< i n)
        (let ((c (aref text i)))
          (cond
           (quoted
            (cond ((and (eq c ?\") (< (1+ i) n) (eq (aref text (1+ i)) ?\"))
                   (push ?\" cell) (setq i (1+ i)))
                  ((eq c ?\") (setq quoted nil))
                  (t (push c cell))))
           ((and (eq c ?\") (null cell)) (setq quoted t))
           ((eq c separator) (end-cell))
           ((eq c ?\r))
           ((eq c ?\n) (end-cell) (end-record))
           (t (push c cell))))
        (setq i (1+ i)))
      (when (or cell record) (end-cell) (end-record)))
    (seq-remove (lambda (r) (equal r '(""))) (nreverse records))))

(defun eas-adapters--cell (text)
  "Return cell TEXT as a number, `:null' when empty, else the string."
  (cond ((string-empty-p text) :null)
        ((string-match-p eas-adapters--number-regexp text) (string-to-number text))
        (t text)))

(defun eas-adapters--delimited (input separator)
  "Convert delimited INPUT with a header row, split on SEPARATOR, to data/v1."
  (let* ((records (eas-adapters-parse-delimited (eas-adapters--text input) separator))
         (header (car records))
         (keys (mapcar #'eas-key header))
         (index 0) rows)
    (unless header (eas-shape-invalid "Delimited data needs a header row" nil))
    (dolist (record (cdr records))
      (unless (= (length record) (length keys))
        (eas-shape-invalid
         (format "Row %d has %d cells but the header has %d; quote cells containing the separator"
                 index (length record) (length keys))
         index))
      (push (cl-loop for key in keys for cell in record
                     append (list key (eas-adapters--cell cell)))
            rows)
      (setq index (1+ index)))
    (eas-data-make (vconcat (nreverse rows)))))

(eas-register-adapter
 "csv" :doc "Comma-separated text, file or stdin with a header row; numeric cells become numbers."
 :convert (lambda (input) (eas-adapters--delimited input ?,))
 :example "x,y\n1,2\n2,3\n")

(eas-register-adapter
 "tsv" :doc "Tab-separated text, file or stdin with a header row."
 :convert (lambda (input) (eas-adapters--delimited input ?\t))
 :example "x\ty\n1\t2\n2\t3\n")

;;; bar/v1

(defconst eas-adapters--bar-schema
  [(:name "time" :type "temporal") (:name "open" :type "quantitative")
   (:name "high" :type "quantitative") (:name "low" :type "quantitative")
   (:name "close" :type "quantitative") (:name "volume" :type "quantitative")]
  "Schema of bar/v1 rows.")

(defun eas-adapters--bars (input)
  "Convert bar/v1 INPUT (OHLCV plists, oldest first) to data/v1."
  (unless (or (listp input) (vectorp input))
    (eas-shape-invalid "bar/v1 data is a list of (:open :high :low :close ...) plists" nil))
  (let ((index 0) rows)
    (seq-doseq (bar input)
      (unless (eas-object-p bar)
        (eas-shape-invalid (format "Bar %d is not a plist" index) index))
      (dolist (key '(:open :high :low :close))
        (unless (numberp (plist-get bar key))
          (eas-shape-invalid (format "Bar %d needs a numeric %s" index key)
                               index (eas-key-name key))))
      (let ((volume (plist-get bar :volume)) (time (plist-get bar :time)))
        (when (and volume (not (and (numberp volume) (>= volume 0))))
          (eas-shape-invalid (format "Bar %d volume must be a non-negative number" index)
                               index "volume"))
        (when (and time (not (or (numberp time) (eas-time-string-p time))))
          (eas-shape-invalid (format "Bar %d time must be epoch ms or an ISO date" index)
                               index "time"))
        (when (< (plist-get bar :high) (plist-get bar :low))
          (eas-shape-invalid (format "Bar %d has high below low" index) index "high"))
        (push (append (list :time (or time index))
                      (cl-loop for key in '(:open :high :low :close)
                               append (list key (plist-get bar key)))
                      (list :volume (or volume :null)))
              rows))
      (setq index (1+ index)))
    (eas-data-make (vconcat (nreverse rows)) eas-adapters--bar-schema)))

(eas-register-adapter
 "bar/v1" :doc "OHLCV bars: (:open :high :low :close [:volume] [:time epoch-ms]) oldest first."
 :convert #'eas-adapters--bars
 :example '((:open 100 :high 103 :low 99 :close 102 :volume 12000 :time 1700000000000)
            (:open 102 :high 104 :low 101 :close 101.5 :volume 9500 :time 1700086400000)))

;;; Streaming

(defun eas-data--value-fits (type value)
  "Non-nil when VALUE fits schema TYPE."
  (or (memq value '(nil :null))
      (pcase type
        ("quantitative" (numberp value))
        ("temporal" (or (numberp value) (eas-time-string-p value)))
        (_ (or (stringp value) (numberp value) (memq value '(t :false)))))))

(defun eas-data-append (data rows)
  "Return DATA (data/v1) with ROWS appended, after a schema check.
Each new row must be a flat object using only schema columns with
values of the column's type; failures are SHAPE_INVALID whose :index
counts within ROWS."
  (let ((schema (plist-get data :schema)) (index 0))
    (seq-doseq (row rows)
      (unless (and row (eas-object-p row))
        (eas-shape-invalid (format "Pushed row %d is not an object" index) index))
      (cl-loop for (key value) on row by #'cddr
               for name = (eas-key-name key)
               for type = (eas-data-field-type data name)
               do (cond
                   ((null type)
                    (eas-shape-invalid
                     (format "Pushed row %d has column %s, which the schema lacks; columns: %s"
                             index name (mapconcat (lambda (c) (plist-get c :name)) schema ", "))
                     index name))
                   ((not (eas-data--value-fits type value))
                    (eas-shape-invalid
                     (format "Pushed row %d column %s must be %s" index name type)
                     index name))))
      (setq index (1+ index)))
    (list :schema schema :rows (vconcat (plist-get data :rows) rows))))

(provide 'eas-adapters)
;;; eas-adapters.el ends here

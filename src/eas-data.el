;;; eas-data.el --- data/v1: tidy rows and the adapter registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L0.  data/v1 is (:schema [(:name N :type T [:unit U]) ...] :rows [ROW ...])
;; where every ROW is a flat plist with keyword keys and TYPE is one of
;; quantitative, temporal, ordinal, nominal.
;;
;; Adapters turn caller data into data/v1.  Each is registered once
;; with `eas-register-adapter' and fails as data: its validator
;; signals SHAPE_INVALID whose props carry :index and :field.
;; `eas-data-check' returns that failure as a plist instead.

;;; Code:

(require 'eas-core)
(require 'eas-time)

(defvar eas-adapters nil
  "Registered adapters: alist of (NAME . PLIST).
PLIST has :doc, :convert (VALUE -> data/v1) and optionally :example.")

(cl-defun eas-register-adapter (name &key doc convert example)
  "Register adapter NAME (a string), a converter of caller data to data/v1.
CONVERT is called with the caller's value and returns data/v1, or
signals SHAPE_INVALID with :index and :field.  DOC is one line for
describe.  EXAMPLE is a small input value that CONVERT accepts."
  (unless (and (stringp name) (functionp convert))
    (eas-signal "INVALID_INPUT" "An adapter needs a string NAME and a :convert function"
                  :adapter name))
  (setf (alist-get name eas-adapters nil nil #'equal)
        (list :name name :doc doc :convert convert :example example))
  name)

(defun eas-adapter (name)
  "Return the adapter plist for NAME or signal NOT_FOUND."
  (or (alist-get name eas-adapters nil nil #'equal)
      (eas-signal "NOT_FOUND"
                    (format "No adapter %S; registered adapters: %s" name
                            (mapconcat #'car eas-adapters ", "))
                    :adapter name)))

(defun eas-data-from (name value)
  "Convert VALUE to data/v1 with adapter NAME."
  (funcall (plist-get (eas-adapter name) :convert) value))

(defun eas-data-check (name value)
  "Return nil when adapter NAME accepts VALUE, else the failure plist.
The plist is (:code :message :index :field ...), never signalled."
  (condition-case err
      (progn (eas-data-from name value) nil)
    (eas-error (eas-error-plist err))))

(defun eas-shape-invalid (message index &optional field &rest props)
  "Signal SHAPE_INVALID with MESSAGE for element INDEX and FIELD.
PROPS are further error properties."
  (apply #'eas-signal "SHAPE_INVALID" message :index index :field field props))

;;; data/v1

(defun eas-data-p (value)
  "Return non-nil when VALUE has the shape of data/v1."
  (and (eas-object-p value) (plist-member value :rows) (plist-member value :schema)))

(defun eas-data-rows (data)
  "Return the rows vector of DATA."
  (plist-get data :rows))

(defun eas-data-make (rows &optional schema)
  "Return data/v1 for ROWS (a vector or list of plists).
SCHEMA, when nil, is inferred from the rows."
  (let ((rows (if (vectorp rows) rows (vconcat rows))))
    (list :schema (or schema (eas-data-infer-schema rows)) :rows rows)))

(defun eas-data-infer-type (values)
  "Return the measurement type that fits every non-null value in VALUES."
  (let ((values (seq-remove (lambda (v) (memq v '(nil :null))) values)))
    (cond ((null values) "nominal")
          ((seq-every-p #'numberp values) "quantitative")
          ((seq-every-p #'eas-time-string-p values) "temporal")
          (t "nominal"))))

(defun eas-data-columns (rows)
  "Return the column keys of ROWS in first-seen order."
  (let (keys)
    (seq-doseq (row rows)
      (dolist (key (eas-plist-keys row))
        (unless (memq key keys) (push key keys))))
    (nreverse keys)))

(defun eas-data-infer-schema (rows)
  "Return a schema vector inferred from ROWS."
  (vconcat
   (mapcar (lambda (key)
             (list :name (eas-key-name key)
                   :type (eas-data-infer-type
                          (seq-map (lambda (row) (plist-get row key)) rows))))
           (eas-data-columns rows))))

(defun eas-data-field-type (data field)
  "Return the schema type of FIELD (a string) in DATA, or nil."
  (seq-some (lambda (col) (and (equal (plist-get col :name) field) (plist-get col :type)))
            (plist-get data :schema)))

;;; Built-in adapters

(defun eas-data--plist-rows (value)
  "Convert VALUE, a list or vector of row objects, to data/v1.
VALUE may instead be (:file F): F's rows, read as CSV, TSV or JSON by
its extension."
  (if (and (eas-object-p value) (stringp (plist-get value :file)))
      (eas-data-from (pcase (downcase (or (file-name-extension (plist-get value :file)) ""))
                       ("csv" "csv") ("tsv" "tsv") (_ "json"))
                     value)
    (eas-data--plist-rows-1 value)))

(defun eas-data--plist-rows-1 (value)
  "Convert VALUE, a list or vector of row objects, to data/v1."
  (unless (or (vectorp value) (listp value))
    (eas-shape-invalid "Rows must be a list or array of objects" nil))
  (let ((index 0))
    (seq-doseq (row value)
      (unless (and (eas-object-p row) row)
        (eas-shape-invalid
         (format "Row %d is not an object; give each row as {\"field\": value, ...}" index)
         index))
      (dolist (key (eas-plist-keys row))
        (let ((v (plist-get row key)))
          (unless (or (numberp v) (stringp v) (memq v '(t :false :null nil)))
            (eas-shape-invalid
             (format "Row %d field %s is nested; rows must be flat" index (eas-key-name key))
             index (eas-key-name key)))))
      (setq index (1+ index))))
  (eas-data-make value))

(defun eas-data--passthrough (value)
  "Accept VALUE that is already data/v1, checking its rows."
  (unless (eas-data-p value)
    (eas-shape-invalid "data/v1 needs :schema and :rows" nil))
  (let ((rows (plist-get (eas-data--plist-rows (plist-get value :rows)) :rows)))
    (list :schema (plist-get value :schema) :rows rows)))

(eas-register-adapter
 "plist" :doc "List or array of flat row objects (Lisp plists or JSON objects)."
 :convert #'eas-data--plist-rows
 :example '((:x 1 :y 2) (:x 2 :y 3)))

(eas-register-adapter
 "data/v1" :doc "Already tidy: (:schema [...] :rows [...])."
 :convert #'eas-data--passthrough
 :example '(:schema [(:name "x" :type "quantitative")] :rows [(:x 1)]))

(provide 'eas-data)
;;; eas-data.el ends here

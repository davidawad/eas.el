;;; eas-core.el --- eas errors, JSON values and hashing -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The conventions every eas layer shares.
;;
;; JSON values in Lisp: objects are plists with keyword keys, arrays
;; are vectors, JSON null is `:null' and JSON false is `:false'.  So
;; `nil' is the empty object, never an empty array, and every value
;; round-trips through `eas-json-parse' / `eas-json-encode'.
;;
;; Failures are data: `eas-signal' raises an `eas-error' child
;; whose data is (MESSAGE :code CODE ...), and the MESSAGE says how to
;; fix the input.  The codes are the design's section 5 reason codes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(define-error 'eas-error "eas error")

(defconst eas-reason-codes
  '(("INVALID_INPUT" eas-invalid-input "Invalid input")
    ("PARSE_ERROR" eas-parse-error "Parse error")
    ("NOT_FOUND" eas-not-found "Not found")
    ("SLOT_MISSING" eas-slot-missing "Template slot missing")
    ("SLOT_TYPE" eas-slot-type "Template slot has the wrong type")
    ("SHAPE_INVALID" eas-shape-invalid "Data does not match its shape")
    ("FIELD_MISSING" eas-field-missing "Field missing from data")
    ("UNSUPPORTED_FEATURE" eas-unsupported-feature "Unsupported feature")
    ("TRANSFORM_UNKNOWN" eas-transform-unknown "Unknown transform")
    ("VIEW_NOT_FOUND" eas-view-not-found "View not found")
    ("EVENT_INVALID" eas-event-invalid "Invalid event")
    ("ENGINE_FAILED" eas-engine-failed "Engine failed")
    ("BUDGET_EXCEEDED" eas-budget-exceeded "Benchmark over its regression budget"))
  "Stable reason codes: (CODE ERROR-SYMBOL TITLE).
Codes shared with bin/chart mean the same thing in both.")

(dolist (entry eas-reason-codes)
  (define-error (nth 1 entry) (nth 2 entry) 'eas-error))

(defun eas-signal (code message &rest props)
  "Signal the `eas-error' child for CODE with MESSAGE and PROPS.
CODE is a reason code string from `eas-reason-codes'.  The error
data is (MESSAGE :code CODE . PROPS)."
  (let ((entry (assoc code eas-reason-codes)))
    (unless entry
      (error "Unknown eas reason code %S" code))
    (signal (nth 1 entry) (cons message (cons :code (cons code props))))))

(defun eas-error-plist (err)
  "Return the condition ERR as a plist (:code :message ...).
ERR is the value bound by `condition-case'.  Errors that are not
`eas-error' children are reported as ENGINE_FAILED."
  (let ((data (cdr err)))
    (if (and (stringp (car-safe data)) (plist-get (cdr data) :code))
        (append (list :code (plist-get (cdr data) :code) :message (car data))
                (eas--plist-without (cdr data) :code))
      (list :code "ENGINE_FAILED" :message (error-message-string err)))))

(defun eas--plist-without (plist key)
  "Return a copy of PLIST with KEY removed."
  (let (out)
    (while plist
      (unless (eq (car plist) key)
        (setq out (cons (cadr plist) (cons (car plist) out))))
      (setq plist (cddr plist)))
    (nreverse out)))

;;; JSON values

(defun eas-json-parse (string)
  "Parse JSON STRING into an eas JSON value.
Signals PARSE_ERROR naming the position when STRING is not JSON."
  (condition-case err
      (json-parse-string string :object-type 'plist :array-type 'array
                         :null-object :null :false-object :false)
    (json-error
     (eas-signal "PARSE_ERROR"
                   (format "Not valid JSON (%s); fix the syntax and retry"
                           (error-message-string err))
                   :detail (cdr err)))))

(defun eas-json-read-file (file)
  "Parse the JSON in FILE.  Signals NOT_FOUND or PARSE_ERROR."
  (unless (file-readable-p file)
    (eas-signal "NOT_FOUND" (format "No readable file %s; check the path" file)
                  :path file))
  (condition-case err
      (with-temp-buffer
        (insert-file-contents file)
        (eas-json-parse (buffer-string)))
    (eas-parse-error
     (signal (car err) (append (cdr err) (list :path file))))))

(defun eas-json-encode (value)
  "Encode the eas JSON VALUE as a compact JSON string.
The result holds characters, not UTF-8 bytes: `json-serialize' returns
a unibyte string, which would turn into raw bytes once concatenated
with text or printed."
  (decode-coding-string
   (json-serialize value :null-object :null :false-object :false) 'utf-8 t))

(defun eas-json-pretty (value)
  "Encode VALUE as indented JSON with sorted keys and a trailing newline.
Arrays of scalars stay on one line.  Used for goldens and humans."
  (concat (eas-json--pretty (eas-json-canonical value) "") "\n"))

(defun eas-json--scalar-p (value)
  "Non-nil when VALUE encodes as a JSON scalar."
  (not (or (vectorp value) (and (consp value) (keywordp (car value))))))

(defun eas-json--pretty (value indent)
  "Pretty-print VALUE at INDENT."
  (let ((inner (concat indent "  ")))
    (cond
     ((and (vectorp value) (seq-every-p #'eas-json--scalar-p value))
      (concat "[" (mapconcat #'eas-json-encode value ", ") "]"))
     ((vectorp value)
      (concat "[\n" inner
              (mapconcat (lambda (v) (eas-json--pretty v inner)) value (concat ",\n" inner))
              "\n" indent "]"))
     ((and (consp value) (keywordp (car value))
           (cl-loop for (_ v) on value by #'cddr always (eas-json--scalar-p v))
           (< (length (eas-json-encode value)) 100))
      (concat "{"
              (mapconcat (lambda (pair)
                           (concat (eas-json-encode (eas-key-name (car pair))) ": "
                                   (eas-json-encode (cdr pair))))
                         (cl-loop for (k v) on value by #'cddr collect (cons k v))
                         ", ")
              "}"))
     ((and (consp value) (keywordp (car value)))
      (concat "{\n" inner
              (mapconcat (lambda (pair)
                           (concat (eas-json-encode (eas-key-name (car pair))) ": "
                                   (eas-json--pretty (cdr pair) inner)))
                         (cl-loop for (k v) on value by #'cddr collect (cons k v))
                         (concat ",\n" inner))
              "\n" indent "}"))
     ((null value) "{}")
     (t (eas-json-encode value)))))

(defun eas-object-p (value)
  "Non-nil when VALUE is a JSON object (a keyword plist, or nil)."
  (or (null value) (and (consp value) (keywordp (car value)))))

(defun eas-json-canonical (value)
  "Return VALUE with every object's keys sorted, recursively."
  (cond
   ((vectorp value) (vconcat (mapcar #'eas-json-canonical value)))
   ((and (consp value) (keywordp (car value)))
    (let (pairs)
      (while value
        (push (cons (car value) (eas-json-canonical (cadr value))) pairs)
        (setq value (cddr value)))
      (cl-loop for (k . v) in (sort pairs (lambda (a b)
                                            (string< (symbol-name (car a))
                                                     (symbol-name (car b)))))
               append (list k v))))
   (t value)))

(defun eas-content-hash (value)
  "Return \"sha256:HEX\" of VALUE's canonical compact JSON encoding."
  (concat "sha256:"
          (secure-hash 'sha256
                       (encode-coding-string
                        (eas-json-encode (eas-json-canonical value)) 'utf-8))))

(defun eas-true-p (value)
  "Non-nil when JSON VALUE is truthy (not nil, `:false', `:null', 0 or \"\")."
  (not (or (memq value '(nil :false :null)) (eql value 0) (equal value ""))))

(defvar eas--key-cache (make-hash-table :test 'equal :size 512)
  "Field name -> keyword, so hot loops never intern.")

(defun eas-key (name)
  "Return the plist keyword for JSON key or field NAME (a string or symbol)."
  (cond ((keywordp name) name)
        ((symbolp name) (intern (concat ":" (symbol-name name))))
        (t (or (gethash name eas--key-cache)
               (puthash name (intern (concat ":" name)) eas--key-cache)))))

(defun eas-key-name (key)
  "Return the JSON name of plist KEY."
  (substring (symbol-name key) 1))

(defun eas-plist-keys (plist)
  "Return the keys of PLIST in order."
  (cl-loop for (k _) on plist by #'cddr collect k))

(defun eas-plist-put (plist key value)
  "Return a copy of PLIST with KEY set to VALUE (appended when new)."
  (if (plist-member plist key)
      (cl-loop for (k v) on plist by #'cddr
               append (list k (if (eq k key) value v)))
    (append plist (list key value))))

(defun eas-seq-list (value)
  "Return VALUE (a vector or list) as a list."
  (if (vectorp value) (append value nil) value))

(defun eas-path (&rest parts)
  "Join PARTS into a JSON pointer such as \"/layer/0/mark\"."
  (mapconcat (lambda (part) (format "/%s" part)) parts ""))

(provide 'eas-core)
;;; eas-core.el ends here

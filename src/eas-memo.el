;;; eas-memo.el --- bounded memo tables for pure, costly computations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Some transforms are pure functions of a few numbers yet cost
;; milliseconds in Elisp: a bootstrap interval resamples a group 1000
;; times (and ci0 and ci1 each ask for it), a density sums a Gaussian
;; per value per sample.  A view compiles its spec again on every
;; resize and for each backend, so the same inputs come back.
;; `eas-memo' caches such a result under an `equal' key in a table
;; that is emptied when it holds its limit of entries; results are
;; exact, never approximations.  `eas-memo-clear' empties
;; every table (the gallery bench measures cold compiles with it).

;;; Code:

(require 'eas-core)

(defvar eas-memo-limit 256
  "Entries a memo table holds before it is emptied, unless it has its own.")

(defvar eas-memo--tables nil
  "Every memo table, for `eas-memo-clear'.")

(defun eas-memo-table (&optional limit)
  "A new memo table holding up to LIMIT entries (default `eas-memo-limit')."
  (car (push (cons limit (make-hash-table :test 'equal)) eas-memo--tables)))

(defun eas-memo-clear ()
  "Empty every memo table."
  (dolist (tb eas-memo--tables) (clrhash (cdr tb))))

(defmacro eas-memo (table key &rest body)
  "The value of BODY, cached in TABLE under KEY (compared with `equal').
BODY must be a pure function of KEY.  A nil value is not cached."
  (declare (indent 2) (debug t))
  (let ((tb (make-symbol "table")) (k (make-symbol "key")))
    `(let ((,tb ,table) (,k ,key))
       (or (gethash ,k (cdr ,tb))
           (progn (when (>= (hash-table-count (cdr ,tb)) (or (car ,tb) eas-memo-limit)) (clrhash (cdr ,tb)))
                  (puthash ,k (progn ,@body) (cdr ,tb)))))))

(provide 'eas-memo)
;;; eas-memo.el ends here

;;; eas-data-org.el --- org-table adapter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The org-table adapter accepts the table at point (:at-point t), a
;; named table (:name "tbl" [:buffer B]) or a babel result value (a
;; list of rows, first row the header, `hline' separators allowed).
;; org is loaded only when a table has to be read from a buffer.

;;; Code:

(require 'eas-data)
(require 'eas-adapters)

(declare-function org-table-to-lisp "org-table" (&optional txt))
(declare-function org-at-table-p "org-table" (&optional table-type))

(defun eas-data-org--cell (cell)
  "Return org CELL as a number, `:null' when empty, else a string."
  (cond ((numberp cell) cell)
        ((not (stringp cell)) (format "%s" cell))
        (t (let ((text (string-trim (substring-no-properties cell))))
             (cond ((string-empty-p text) :null)
                   ((string-match-p eas-adapters--number-regexp text)
                    (string-to-number text))
                   (t text))))))

(defun eas-data-org--from-lisp (table)
  "Convert TABLE (rows of cells with optional `hline') to data/v1."
  (let* ((rows (seq-remove (lambda (r) (eq r 'hline)) table))
         (keys (mapcar (lambda (h) (eas-key (format "%s" (eas-data-org--cell h))))
                       (car rows)))
         (index 0) out)
    (unless keys (eas-shape-invalid "The org table needs a header row" nil))
    (dolist (row (cdr rows))
      (unless (and (listp row) (= (length row) (length keys)))
        (eas-shape-invalid (format "Table row %d has %d cells, header has %d"
                                     index (if (listp row) (length row) 0) (length keys))
                             index))
      (push (cl-loop for key in keys for cell in row
                     append (list key (eas-data-org--cell cell)))
            out)
      (setq index (1+ index)))
    (eas-data-make (vconcat (nreverse out)))))

(defun eas-data-org--named (name buffer)
  "Return the lisp form of the table named NAME in BUFFER."
  (require 'org-table)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward
               (format "^[ \t]*#\\+name:[ \t]*%s[ \t]*$" (regexp-quote name)) nil t)
        (eas-signal "NOT_FOUND" (format "No org table named %s in %s" name (buffer-name))
                      :table name))
      (forward-line 1)
      (unless (org-at-table-p)
        (eas-signal "NOT_FOUND" (format "#+name: %s is not followed by a table" name)
                      :table name))
      (org-table-to-lisp))))

(defun eas-data-org--convert (input)
  "Convert org-table INPUT to data/v1."
  (cond
   ((and (eas-object-p input) (plist-get input :at-point))
    (require 'org-table)
    (unless (org-at-table-p)
      (eas-signal "NOT_FOUND" "Point is not in an org table; move into one or name it"))
    (eas-data-org--from-lisp (org-table-to-lisp)))
   ((and (eas-object-p input) (plist-get input :name))
    (eas-data-org--from-lisp
     (eas-data-org--named (plist-get input :name) (plist-get input :buffer))))
   ((and (listp input) (listp (car input))) (eas-data-org--from-lisp input))
   (t (eas-shape-invalid
       "org-table input is (:at-point t), (:name N [:buffer B]) or a babel result list" nil))))

(eas-register-adapter
 "org-table" :doc "Org table at point, named table, or babel result (header row first)."
 :convert #'eas-data-org--convert
 :example '(("x" "y") hline (1 2) (2 3)))

(provide 'eas-data-org)
;;; eas-data-org.el ends here

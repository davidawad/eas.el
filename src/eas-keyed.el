;;; eas-keyed.el --- keyed pushes: replace or delete rows by key -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A plain push appends rows.  A keyed push names a key
;; column and updates a live table in place:
;;
;;   {"type": "push", "key": "price", "rows": [ROW, ...]}
;;
;; A row whose key matches existing rows replaces them where they
;; stand; a row with a new key is appended; a row carrying
;; "_eas_delete": true removes the rows with its key (it needs only the
;; key column).  Within one push the latest row per key wins and keys
;; keep the order they first appear in, so a push (or a stream frame,
;; eas-stream.el) of many deltas to the same key changes one row.
;;
;; Everything here is pure: data in, data out.  `eas-keyed-check'
;; schema-checks rows as `eas-data-append' does and `eas-keyed-apply'
;; merges them into data/v1.

;;; Code:

(require 'eas-core)
(require 'eas-data)
(require 'eas-adapters)

(defconst eas-keyed-delete-column "_eas_delete"
  "Row column that turns a keyed row into a delete of its key.")

(defun eas-keyed-field (key)
  "The plist keyword of column KEY (a string)."
  (intern (concat ":" key)))

(defun eas-keyed-delete-p (row)
  "Non-nil when ROW deletes its key."
  (eq (plist-get row :_eas_delete) t))

(defun eas-keyed--strip (row)
  "ROW without its delete marker."
  (if (plist-member row :_eas_delete) (eas--plist-without row :_eas_delete) row))

(defun eas-keyed-check (data rows key)
  "Schema-check ROWS for a push into DATA (data/v1) keyed by KEY.
KEY nil checks a plain push.  Each row passes `eas-data-append' once
its delete marker is gone, and must carry a non-null KEY.  Failures
are SHAPE_INVALID whose :index counts within ROWS; a KEY the schema
lacks is EVENT_INVALID on field key.  Returns ROWS as a vector, with
false delete markers dropped."
  (let ((rows (vconcat rows))
        (schema (plist-get data :schema)))
    (if (null key)
        (progn (eas-data-append (list :schema schema :rows []) rows) rows)
      (unless (and (stringp key) (eas-data-field-type data key))
        (eas-signal "EVENT_INVALID"
                    (format "push key %S is not a column; columns: %s" key
                            (mapconcat (lambda (c) (plist-get c :name)) schema ", "))
                    :field "key"))
      (let ((field (eas-keyed-field key)) (index 0))
        (seq-doseq (row rows)
          (when (and (eas-object-p row) (memq (plist-get row field) '(nil :null)))
            (eas-shape-invalid (format "Keyed row %d needs a value for its key %s" index key)
                               index key))
          (let ((marker (plist-get row :_eas_delete)))
            (unless (memq marker '(nil t :false))
              (eas-shape-invalid (format "Keyed row %d %s is true or false" index eas-keyed-delete-column)
                                 index eas-keyed-delete-column)))
          (setq index (1+ index))))
      (let ((stripped (vconcat (seq-map (lambda (row) (if (eas-object-p row) (eas-keyed--strip row) row))
                                        rows))))
        (eas-data-append (list :schema schema :rows []) stripped)
        (vconcat (seq-map (lambda (row) (if (eas-keyed-delete-p row) row (eas-keyed--strip row)))
                          rows))))))

(defun eas-keyed-merge (table order rows key)
  "Merge ROWS into TABLE (key value -> latest row) keyed by KEY.
ORDER lists the keys of TABLE newest first; return it with ROWS' new
keys pushed on."
  (let ((field (eas-keyed-field key)))
    (seq-doseq (row rows)
      (let ((k (plist-get row field)))
        (unless (gethash k table) (push k order))
        (puthash k row table)))
    order))

(defun eas-keyed-coalesce (rows key)
  "ROWS keeping only the latest row per KEY value, in first-seen order."
  (let* ((table (make-hash-table :test 'equal))
         (order (eas-keyed-merge table nil rows key)))
    (vconcat (mapcar (lambda (k) (gethash k table)) (nreverse order)))))

(defun eas-keyed-apply (data rows key)
  "Return DATA (data/v1) with ROWS merged in by column KEY.
Rows are assumed checked (`eas-keyed-check').  An existing row whose
KEY matches a pushed row is replaced in place, or dropped when that
row deletes; pushed rows with new keys are appended in order."
  (let* ((field (eas-keyed-field key))
         (table (make-hash-table :test 'equal))
         (order (nreverse (eas-keyed-merge table nil rows key)))
         (seen (make-hash-table :test 'equal))
         kept)
    (seq-doseq (row (plist-get data :rows))
      (let* ((k (plist-get row field)) (new (gethash k table)))
        (cond ((null new) (push row kept))
              ((eas-keyed-delete-p new) (puthash k t seen))
              (t (puthash k t seen) (push new kept)))))
    (dolist (k order)
      (let ((new (gethash k table)))
        (unless (or (gethash k seen) (eas-keyed-delete-p new))
          (push new kept))))
    (eas-plist-put data :rows (vconcat (nreverse kept)))))

(provide 'eas-keyed)
;;; eas-keyed.el ends here

;;; eas-params-index.el --- indexed {"param": ...} filters -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1/L6 (fc-qx1.9).  The Vega-Lite crosshair idiom is a rule
;; layer filtered by a point selection, {"filter": {"param": "hover"}},
;; so every pointermove re-runs that filter over all rows.  Testing each
;; row was 77% of a 100k-row hover.  `eas-params-index-filter' answers
;; the same filter from a hash of each row's tuple over the store's
;; fields, built once per rows vector and kept in a weak table, so a
;; hover costs the selected rows, not all of them.
;;
;; Keys normalise values the way `eas-params--same' compares them:
;; numbers as floats, date strings as epoch ms, anything else as itself.
;; Interval stores, and rows holding integers past 2^53 (where floats
;; would merge distinct values), fall back to the row-by-row test.

;;; Code:

(require 'eas-core)
(require 'eas-time)

(defvar eas-params-index--tables (make-hash-table :test 'eq :weakness 'key)
  "Rows vector -> alist of (FIELDS . HASH of normalised tuple -> indices).")

(defconst eas-params-index--exact (expt 2 53)
  "Largest integer magnitude a float holds exactly.")

(defun eas-params-index--norm (v)
  "V normalised so that `equal' on results is `eas-params--same' on values.
Throw `eas-params-index-skip' for integers floats cannot hold."
  (cond ((integerp v)
         (when (> (abs v) eas-params-index--exact) (throw 'eas-params-index-skip nil))
         (float v))
        ((floatp v) (+ 0.0 v))
        ((stringp v) (let ((ms (eas-time-parse v))) (if ms (eas-params-index--norm ms) v)))
        (t v)))

(defun eas-params-index--table (rows fields)
  "The tuple index of ROWS over FIELDS (strings), built on first use."
  (let ((cached (assoc fields (gethash rows eas-params-index--tables))))
    (or (cdr cached)
        (let ((table (make-hash-table :test 'equal :size (max 1 (length rows))))
              (keys (mapcar #'eas-key fields)))
          (dotimes (i (length rows))
            (let* ((row (aref rows i))
                   (tuple (mapcar (lambda (k) (eas-params-index--norm (plist-get row k))) keys)))
              (puthash tuple (cons i (gethash tuple table)) table)))
          (push (cons fields table) (gethash rows eas-params-index--tables))
          table))))

(defun eas-params-index--matches (store rows)
  "Indices of ROWS in point STORE, unsorted; throws when no index applies."
  (let ((table (eas-params-index--table rows (append (plist-get store :fields) nil))))
    (cl-loop for tuple across (plist-get store :values)
             append (copy-sequence (gethash (mapcar #'eas-params-index--norm tuple) table)))))

(defun eas-params-index-changed (rows pairs old new)
  "Sorted indices of ROWS whose membership may differ between OLD and NEW.
OLD and NEW are view states; PAIRS are (PARAM . EMPTY).  Returns `all'
when an index cannot bound the change (interval stores, or an empty
selection that means everything)."
  (or (catch 'eas-params-index-skip
        (let (hits)
          (dolist (p pairs)
            (dolist (state (list old new))
              (let ((store (plist-get (plist-get state :params) (eas-key (car p)))))
                (cond ((and (null store) (not (cdr p))))
                      ((equal (plist-get store :type) "point")
                       (setq hits (nconc (eas-params-index--matches store rows) hits)))
                      (t (throw 'eas-params-index-skip nil))))))
          (sort (delete-dups hits) #'<)))
      'all))

(defun eas-params-index-filter (store rows empty)
  "ROWS (a vector) in selection STORE, in order, or nil when no index applies.
A nil STORE is the empty selection: all ROWS when EMPTY, else none."
  (cond
   ((null store) (if empty rows []))
   ((equal (plist-get store :type) "point")
    (catch 'eas-params-index-skip
      (vconcat (mapcar (lambda (i) (aref rows i))
                       (sort (delete-dups (eas-params-index--matches store rows)) #'<)))))))

(provide 'eas-params-index)
;;; eas-params-index.el ends here

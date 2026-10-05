;;; eas-transform-lookup.el --- lookup and pivot transforms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega-Lite's lookup joins each row with the first row of
;; a secondary source whose KEY equals the row's LOOKUP field:
;;
;;   from {"data": {"values": [...]}, "key": K, "fields"?: [...]}
;;        copies those fields (or, with "as", the whole row under AS)
;;   from {"param": NAME, "key": K}
;;        joins against the rows NAME currently selects, found among the
;;        transformed rows themselves; the match is stored whole under
;;        "as" (default NAME), null when nothing matches
;;
;; pivot turns the values of one field into columns: one row per
;; groupby tuple, each pivot value's column holding op (default sum)
;; of the value field.

;;; Code:

(require 'eas-core)
(require 'eas-transform-agg)

(defvar eas-transform-param-predicate)

(defun eas-transform-lookup--index (rows key)
  "Hash of KEY value -> first row of ROWS with it."
  (let ((index (make-hash-table :test 'equal)))
    (seq-doseq (r rows)
      (let ((k (plist-get r key)))
        (unless (gethash k index) (puthash k r index))))
    index))

(defun eas-transform-lookup (tr rows env)
  "Apply lookup transform TR to ROWS (ENV holds param values)."
  (let* ((from (plist-get tr :from))
         (field (eas-key (plist-get tr :lookup)))
         (key (eas-key (plist-get from :key)))
         (param (plist-get from :param))
         (as (plist-get tr :as))
         (secondary (if param
                        (seq-filter (lambda (r) (funcall eas-transform-param-predicate param r env nil)) rows)
                      (plist-get (plist-get from :data) :values)))
         (index (eas-transform-lookup--index secondary key))
         (fields (and (not param) (plist-get from :fields))))
    (unless (or param (vectorp secondary))
      (eas-signal "UNSUPPORTED_FEATURE" "lookup needs from.data.values (inline data) or from.param"
                  :feature "transform/lookup"))
    (seq-map (lambda (row)
               (let ((hit (gethash (plist-get row field) index)))
                 (cond
                  ((or param (and as (null fields)))
                   (eas-plist-put row (eas-key (or (if (vectorp as) (aref as 0) as) param)) (or hit :null)))
                  (t (let ((names (or fields (vconcat (seq-remove (lambda (k) (eq k :_eas_row))
                                                                  (mapcar #'eas-key-name (eas-plist-keys hit)))))))
                       (cl-loop for f across names for i from 0
                                do (setq row (eas-plist-put row (eas-key (if (vectorp as) (aref as i) f))
                                                            (if hit (plist-get hit (eas-key f)) :null))))
                       row)))))
             rows)))

(provide 'eas-transform-lookup)
;;; eas-transform-lookup.el ends here

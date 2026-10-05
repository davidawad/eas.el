;;; eas-compile-sort.el --- discrete domains sorted by a field aggregate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega-Lite's sort field: {"field": F, "op": OP, "order": ORDER} orders
;; a discrete domain by OP (default "min") of F over the rows of each
;; domain value.  When the unit is aggregated F may only survive as the
;; aggregate column "OP_F", which is then read instead.

;;; Code:

(require 'eas-core)
(require 'eas-encode)
(require 'eas-transform-agg)

(defun eas-compile-sort-by-field (sort def rows values)
  "VALUES (distinct domain values of DEF) ordered by sort field SORT over ROWS."
  (let* ((op (or (plist-get sort :op) "min"))
         (field (plist-get sort :field))
         (key (and field (eas-key field)))
         (alt (and field (eas-key (format "%s_%s" op field))))
         (key (if (and key (seq-some (lambda (r) (plist-member r key)) rows)) key alt))
         (groups (make-hash-table :test 'equal)))
    (seq-doseq (row rows)
      (puthash (eas-encode-raw def row) (cons (and key (plist-get row key))
                                              (gethash (eas-encode-raw def row) groups))
               groups))
    (let* ((score (mapcar (lambda (v) (cons v (eas-agg-apply op (reverse (gethash v groups)) "/sort")))
                          values))
           (sorted (sort score (lambda (a b) (let ((x (cdr a)) (y (cdr b)))
                                               (cond ((not (numberp x)) (numberp y))
                                                     ((not (numberp y)) nil)
                                                     (t (< x y))))))))
      (mapcar #'car (if (equal (plist-get sort :order) "descending") (nreverse sorted) sorted)))))

(provide 'eas-compile-sort)
;;; eas-compile-sort.el ends here

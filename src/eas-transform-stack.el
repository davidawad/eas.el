;;; eas-transform-stack.el --- the Vega-Lite stack transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  {"stack": FIELD, "groupby": [...], "sort": [{field,
;; order}], "offset": "zero"|"center"|"normalize", "as": [START, END]}
;; is Vega's stack: rows are partitioned by groupby (in order of first
;; appearance), each group is stably sorted, and START/END accumulate
;; FIELD.  zero stacks negative values downward from 0 apart from the
;; positive ones; center centres each group on the largest group's sum
;; of absolute values; normalize divides by the group's sum of absolute
;; values.

;;; Code:

(require 'eas-core)

(defun eas-transform-stack--less (sort)
  "Stable comparator for SORT, a vector of {field, order}."
  (lambda (a b)
    (cl-loop for s across sort
             for k = (eas-key (plist-get s :field))
             for va = (plist-get a k) for vb = (plist-get b k)
             for desc = (equal (plist-get s :order) "descending")
             unless (equal va vb)
             return (let ((lt (if (and (numberp va) (numberp vb)) (< va vb)
                                (string< (format "%s" va) (format "%s" vb)))))
                      (if desc (not lt) lt)))))

(defun eas-transform-stack (tr rows)
  "Apply stack transform TR to ROWS, keeping their order."
  (let* ((rows (vconcat rows))
         (field (eas-key (plist-get tr :stack)))
         (as (or (plist-get tr :as) (vector (concat (plist-get tr :stack) "_start")
                                            (concat (plist-get tr :stack) "_end"))))
         (y0 (eas-key (aref as 0))) (y1 (eas-key (aref as 1)))
         (groupby (mapcar #'eas-key (append (plist-get tr :groupby) nil)))
         (offset (or (plist-get tr :offset) "zero"))
         (sort (plist-get tr :sort))
         (value (lambda (i) (let ((x (plist-get (aref rows i) field))) (if (numberp x) x 0))))
         (groups nil) (index (make-hash-table :test 'equal)) (out (copy-sequence rows)))
    (dotimes (i (length rows))
      (let* ((key (mapcar (lambda (g) (plist-get (aref rows i) g)) groupby))
             (cell (gethash key index)))
        (unless cell (setq cell (list nil)) (puthash key cell index) (push cell groups))
        (push i (car cell))))
    (let* ((groups (mapcar (lambda (c) (nreverse (car c))) (nreverse groups)))
           (sums (mapcar (lambda (g) (apply #'+ (mapcar (lambda (i) (abs (funcall value i))) g))) groups))
           (max (apply #'max 0 sums))
           (less (and (vectorp sort) (eas-transform-stack--less sort))))
      (cl-loop for g in groups for sum in sums
               do (let ((g (if less (seq-sort (lambda (a b) (funcall less (aref rows a) (aref rows b))) g) g))
                        (pos 0) (neg 0) (last (if (equal offset "center") (/ (- max sum) 2.0) 0)))
                    (dolist (i g)
                      (let* ((v (funcall value i))
                             (ends (pcase offset
                                     ("center" (cons last (setq last (+ last (abs v)))))
                                     ("normalize" (cons (if (zerop sum) 0 (/ pos (float sum)))
                                                        (progn (setq pos (+ pos (abs v)))
                                                               (if (zerop sum) 0 (/ pos (float sum))))))
                                     (_ (if (< v 0) (cons neg (setq neg (+ neg v)))
                                          (cons pos (setq pos (+ pos v))))))))
                        (aset out i (eas-plist-put (eas-plist-put (aref rows i) y0 (car ends)) y1 (cdr ends))))))))
    out))

(provide 'eas-transform-stack)
;;; eas-transform-stack.el ends here

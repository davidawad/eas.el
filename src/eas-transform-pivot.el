;;; eas-transform-pivot.el --- the pivot transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega-Lite's pivot widens long rows: one output row per
;; groupby group (first-seen order) holding the group's fields plus one
;; field per distinct value of the pivot field (ascending, at most
;; `limit'), each the `op' (default sum) of `value' over the group's
;; rows with that pivot value.  A group with no such row gets null, so
;; series starting late stay absent rather than zero.

;;; Code:

(require 'eas-core)
(require 'eas-transform-agg)

(defun eas-transform-pivot--name (v)
  "The field name pivot value V becomes."
  (cond ((stringp v) v)
        ((and (floatp v) (= v (ftruncate v)) (< (abs v) 1e15)) (format "%d" (truncate v)))
        (t (format "%s" v))))

(defun eas-transform-pivot (tr rows path)
  "Apply pivot transform TR to ROWS; PATH is its JSON pointer."
  (let* ((pivot (eas-key (plist-get tr :pivot))) (value (eas-key (plist-get tr :value)))
         (op (or (plist-get tr :op) "sum"))
         (groupby (mapcar #'eas-key (plist-get tr :groupby)))
         (keys (sort (delete-dups (seq-remove (lambda (v) (memq v '(nil :null)))
                                              (seq-map (lambda (r) (plist-get r pivot)) rows)))
                     (lambda (a b) (if (and (numberp a) (numberp b)) (< a b)
                                     (string< (format "%s" a) (format "%s" b))))))
         (keys (if (and (numberp (plist-get tr :limit)) (> (plist-get tr :limit) 0))
                   (seq-take keys (plist-get tr :limit))
                 keys)))
    (eas-agg-op op (concat path "/op"))
    (vconcat
     (mapcar (lambda (group)
               (append (cl-loop for k in groupby for v in (car group) append (list k v))
                       (cl-loop for key in keys
                                for vals = (delq nil (mapcar (lambda (r) (and (equal (plist-get r pivot) key)
                                                                             (list (plist-get r value))))
                                                             (cdr group)))
                                append (list (eas-key (eas-transform-pivot--name key))
                                             (if vals (eas-agg-apply op (mapcar #'car vals) path) :null)))))
             (eas-agg--groups rows groupby)))))

(provide 'eas-transform-pivot)
;;; eas-transform-pivot.el ends here

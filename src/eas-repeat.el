;;; eas-repeat.el --- Vega-Lite repeat as concatenation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2.  A repeat spec ({"repeat": {"row": [...], "column":
;; [...]}, "spec": {...}}) is the same view drawn once per field.
;; `eas-repeat-expand' rewrites it into the concatenation Vega-Lite
;; lays out: a column repeat is an hconcat, a row repeat a vconcat,
;; both a vconcat of hconcats, and an array repeat an hconcat (or
;; rows of "columns" cells).  Every {"repeat": "row"|"column"|
;; "repeat"} reference in the inner spec becomes the field name.
;; Params inside the spec are repeated with it under the same name, as
;; Vega-Lite's do: one selection, set from whichever cell is used.

;;; Code:

(require 'eas-core)

(defun eas-repeat--substitute (node bindings)
  "NODE with every {\"repeat\": KEY} replaced by KEY's value in BINDINGS."
  (cond
   ((and (consp node) (keywordp (car node)))
    (if (and (stringp (plist-get node :repeat)) (= (length node) 2))
        (or (plist-get bindings (eas-key (plist-get node :repeat))) node)
      (cl-loop for (k v) on node by #'cddr append (list k (eas-repeat--substitute v bindings)))))
   ((vectorp node) (vconcat (mapcar (lambda (x) (eas-repeat--substitute x bindings)) node)))
   (t node)))

(defun eas-repeat-expand (spec)
  "SPEC with a top-level repeat rewritten as concatenation (else SPEC)."
  (let ((repeat (plist-get spec :repeat)) (inner (plist-get spec :spec)))
    (if (not (and repeat inner (eas-object-p inner))) spec
      (let* ((outer (eas--plist-without (eas--plist-without (eas--plist-without spec :repeat) :spec) :columns))
             (cell (lambda (bindings) (eas-repeat-expand (eas-repeat--substitute inner bindings))))
             (rows (and (eas-object-p repeat) (plist-get repeat :row)))
             (cols (and (eas-object-p repeat) (plist-get repeat :column))))
        (cond
         ((vectorp repeat)
          (let* ((n (or (plist-get spec :columns) (length repeat)))
                 (cells (mapcar (lambda (f) (funcall cell (list :repeat f))) repeat)))
            (if (>= n (length cells)) (append outer (list :hconcat (vconcat cells)))
              (append outer (list :vconcat (vconcat (cl-loop for i from 0 below (length cells) by n
                                                             collect (list :hconcat (vconcat (seq-subseq cells i (min (length cells) (+ i n))))))))))))
         ((and rows cols)
          (append outer (list :x-eas (list :grid t)
                              :vconcat (vconcat (mapcar (lambda (r) (list :hconcat (vconcat (mapcar (lambda (c) (funcall cell (list :row r :column c))) cols))))
                                                        rows)))))
         (rows (append outer (list :vconcat (vconcat (mapcar (lambda (r) (funcall cell (list :row r))) rows)))))
         (cols (append outer (list :hconcat (vconcat (mapcar (lambda (c) (funcall cell (list :column c))) cols)))))
         (t spec))))))

(provide 'eas-repeat)
;;; eas-repeat.el ends here

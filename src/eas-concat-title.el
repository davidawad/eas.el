;;; eas-concat-title.el --- titles of nested concatenations -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2.  A concat nested in another may carry a title, which
;; Vega-Lite draws above the nested block, anchored at its left edge.
;; That edge is its first view's, and the views of a row align their
;; plots, so the title is drawn the same as the first view's own title:
;; `eas-concat-title-lower' moves it there (unless that view has one).
;; Export keeps the title where it was (`eas-facet-keep').

;;; Code:

(require 'eas-core)

(defvar eas-facet-keep)

(defun eas-concat-title--first-view (node)
  "Path of keys to NODE's first non-concat descendant, or nil."
  (let ((key (seq-find (lambda (k) (vectorp (plist-get node k))) '(:hconcat :vconcat))))
    (when key
      (let ((child (aref (plist-get node key) 0)))
        (if (seq-some (lambda (k) (vectorp (plist-get child k))) '(:hconcat :vconcat))
            (cons key (eas-concat-title--first-view child))
          (list key))))))

(defun eas-concat-title--push (node title)
  "NODE with TITLE given to its first view, unless that view has a title."
  (let* ((key (seq-find (lambda (k) (vectorp (plist-get node k))) '(:hconcat :vconcat)))
         (children (copy-sequence (plist-get node key)))
         (first (aref children 0)))
    (cond
     ((seq-some (lambda (k) (vectorp (plist-get first k))) '(:hconcat :vconcat))
      (aset children 0 (eas-concat-title--push first title)))
     ((plist-member first :title) (setq children nil))
     (t (aset children 0 (eas-plist-put first :title title))))
    (if children (eas-plist-put node key children) :keep)))

(defun eas-concat-title-lower (spec &optional nested)
  "SPEC with the titles of nested concatenations moved to their first views.
NESTED is non-nil below the root (the root's title is the chart title)."
  (if (or (bound-and-true-p eas-facet-keep) (not (and spec (eas-object-p spec)))) spec
    (let ((out spec))
      (dolist (key '(:vconcat :hconcat))
        (when (vectorp (plist-get out key))
          (setq out (eas-plist-put out key (vconcat (mapcar (lambda (c) (eas-concat-title-lower c t))
                                                            (plist-get out key)))))))
      (if (and nested (plist-get out :title) (eas-concat-title--first-view out)
               ;; A facet's own title is its chart's.
               (not (plist-get (plist-get out :x-eas) :facet)))
          (let ((moved (eas-concat-title--push out (plist-get out :title))))
            (if (eq moved :keep) out (eas--plist-without moved :title)))
        out))))

(defvar eas-spec-rewrite-functions)
(add-hook 'eas-spec-rewrite-functions #'eas-concat-title-lower t)

(provide 'eas-concat-title)
;;; eas-concat-title.el ends here

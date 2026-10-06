;;; eas-concat-wrap.el --- Vega-Lite's wrapped concat as rows of hconcat -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2.  Vega-Lite's general concatenation ({"concat": [...],
;; "columns": N}) wraps its views into rows of N.  `eas-concat-wrap'
;; rewrites it into what the compiler reads: an hconcat when every view
;; fits on one row (no "columns" means one row, as in Vega-Lite), else a
;; vconcat of hconcat rows.  `eas-spec-parse' runs it before any other
;; rewrite, so facets, repeats and data URLs inside the views are
;; lowered as usual.
;;
;; A template's concat may hold {"x-eas:each"} or {"x-eas:when"}
;; elements, whose count is known only once slots resolve.  Such a
;; concat is left as it is; resolve expands the elements and the next
;; parse wraps the result.

;;; Code:

(require 'eas-core)

(defun eas-concat-wrap--deferred-p (views)
  "Non-nil when VIEWS hold a template element that resolve expands."
  (seq-some (lambda (v) (and (eas-object-p v) v
                             (or (plist-member v :x-eas:each) (plist-member v :x-eas:when))))
            views))

(defun eas-concat-wrap--rows (views columns spacing)
  "VIEWS (a list) as hconcat rows of COLUMNS views, each with SPACING."
  (cl-loop for i from 0 below (length views) by columns
           collect (append (list :hconcat (vconcat (seq-subseq views i (min (length views)
                                                                             (+ i columns)))))
                           (and spacing (list :spacing spacing)))))

(defun eas-concat-wrap (spec)
  "SPEC with every Vega-Lite concat in its view tree as hconcat rows."
  (if (not (and (eas-object-p spec) spec)) spec
    (let ((out spec))
      (dolist (key '(:layer :vconcat :hconcat :concat))
        (when (vectorp (plist-get out key))
          (setq out (eas-plist-put out key (vconcat (mapcar #'eas-concat-wrap (plist-get out key)))))))
      (when (and (plist-get out :spec) (eas-object-p (plist-get out :spec)))
        (setq out (eas-plist-put out :spec (eas-concat-wrap (plist-get out :spec)))))
      (let ((views (plist-get out :concat)))
        (if (not (and (vectorp views) (not (eas-concat-wrap--deferred-p views))))
            out
          (let* ((views (append views nil))
                 (columns (plist-get out :columns))
                 (columns (if (and (natnump columns) (> columns 0)) columns (max 1 (length views))))
                 (spacing (let ((s (plist-get out :spacing))) (and (numberp s) s)))
                 (outer (eas--plist-without (eas--plist-without out :concat) :columns)))
            (if (<= (length views) columns)
                (append outer (list :hconcat (vconcat views)))
              (append outer (list :vconcat (vconcat (eas-concat-wrap--rows views columns spacing)))))))))))

(provide 'eas-concat-wrap)
;;; eas-concat-wrap.el ends here

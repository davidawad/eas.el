;;; eas-compile-aux.el --- day and month names ordering a timeUnit sort -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite details of the non-positional scales:
;;
;; - A sort array of day or month names orders a day/month timeUnit
;;   (full names or three-letter initials, any case).

;;; Code:

(require 'eas-core)
(require 'eas-encode)
(require 'eas-time)

(defconst eas-compile-aux--names
  '(("day" :weekday "sun" "mon" "tue" "wed" "thu" "fri" "sat")
    ("month" :month nil "jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))
  "Per timeUnit: the calendar field and its names, indexed by field value.")

(defun eas-compile-aux-timeunit-sort (def sort values)
  "VALUES of timeUnit DEF ordered by SORT, an array of day or month names.
Nil when DEF is not a day or month timeUnit or SORT names none of them."
  (when-let* ((field (and (equal (plist-get def :derived) "timeUnit") (plist-get def :field)))
              (entry (assoc (car (split-string field "_")) eas-compile-aux--names))
              (names (nthcdr 2 entry))
              (order (delq nil (mapcar (lambda (s)
                                         (and (stringp s) (>= (length s) 3)
                                              (seq-position names (downcase (substring s 0 3)))))
                                       sort))))
    (let* ((index (lambda (v)
                    (and (numberp v)
                         (seq-position order (plist-get (eas-time-fields v) (nth 1 entry))))))
           (known (seq-filter index values)))
      (append (sort (copy-sequence known) (lambda (a b) (< (funcall index a) (funcall index b))))
              (seq-remove index values)))))

(provide 'eas-compile-aux)
;;; eas-compile-aux.el ends here

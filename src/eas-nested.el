;;; eas-nested.el --- nested field references: "a.b", "a[0]", "a['b']" -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite reads a field "record.low" as the property
;; low of the property record, "ranges[2]" as an array element, and
;; "a\\.b" as the flat key "a.b".  Compile reads rows by flat keys, so
;; `eas-nested-flatten' adds, to every row, a column named by each
;; nested reference an encoding makes (field "record.low" becomes key
;; :record.low), and every later step reads plain fields.

;;; Code:

(require 'eas-core)

(defun eas-nested-path (field)
  "FIELD split into its access path (strings and integers), or nil when flat."
  (when (and (stringp field) (string-match-p "[.[]" field))
    (let ((i 0) (n (length field)) (cur "") path)
      (while (< i n)
        (let ((c (aref field i)))
          (cond
           ((and (eq c ?\\) (< (1+ i) n)) (setq cur (concat cur (string (aref field (1+ i)))) i (+ i 2)))
           ((eq c ?.) (push cur path) (setq cur "" i (1+ i)))
           ((eq c ?\[)
            (unless (string-empty-p cur) (push cur path))
            (setq cur "")
            (let* ((close (or (string-search "]" field i) n))
                   (inner (substring field (1+ i) close)))
              (push (if (string-match "\\`['\"]\\(.*\\)['\"]\\'" inner) (match-string 1 inner)
                      (string-to-number inner))
                    path)
              (setq i (1+ close))
              (when (and (< i n) (eq (aref field i) ?.)) (setq i (1+ i)))))
           (t (setq cur (concat cur (string c)) i (1+ i))))))
      (unless (string-empty-p cur) (push cur path))
      (let ((path (nreverse path)))
        (and (cdr path) path)))))

(defun eas-nested-get (row path)
  "The value at PATH (from `eas-nested-path') in ROW, or nil."
  (let ((v row))
    (dolist (p path v)
      (setq v (cond ((and (integerp p) (vectorp v) (< -1 p (length v))) (aref v p))
                    ((and (stringp p) (eas-object-p v)) (plist-get v (eas-key p)))
                    (t nil))))))

(defun eas-nested--fields (encoding)
  "Nested field references ENCODING makes, as (KEY . PATH) pairs."
  (let (out)
    (cl-labels ((def (d)
                  (cond ((vectorp d) (seq-do #'def d))
                        ((eas-object-p d)
                         (let ((path (eas-nested-path (plist-get d :field))))
                           (when path (push (cons (eas-key (plist-get d :field)) path) out)))
                         (let ((c (plist-get d :condition))) (when c (def c)))))))
      (cl-loop for (_ d) on encoding by #'cddr do (def d)))
    (delete-dups out)))

(defun eas-nested-flatten (rows encoding)
  "ROWS with a flat column for every nested field reference in ENCODING.
Rows that already carry the flat key keep it."
  (let ((fields (eas-nested--fields encoding)))
    (if (null fields) rows
      (vconcat (seq-map (lambda (row)
                          (let ((extra (cl-loop for (key . path) in fields
                                                unless (plist-member row key)
                                                append (list key (let ((v (eas-nested-get row path)))
                                                                   (if (null v) :null v))))))
                            (if extra (append row extra) row)))
                        rows)))))

(provide 'eas-nested)
;;; eas-nested.el ends here

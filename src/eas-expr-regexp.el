;;; eas-expr-regexp.el --- regexp() and test() for expressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's expression language has regexp(PATTERN, FLAGS)
;; and test(REGEXP, STRING) (gallery: a search box filtering points).
;; Patterns are JavaScript regular expressions; `eas-expr-regexp-js'
;; translates the common subset (groups, alternation, braces, classes
;; and the \d \w \s escapes) to Emacs syntax.  The "i" flag folds case.

;;; Code:

(require 'eas-core)

(defun eas-expr-regexp-js (pattern)
  "Emacs regexp for JavaScript regexp PATTERN."
  (let ((out nil) (i 0) (n (length pattern)))
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) n))
          (let ((d (aref pattern (1+ i))))
            (push (pcase d
                    (?d "[0-9]") (?D "[^0-9]") (?w "[[:alnum:]_]") (?W "[^[:alnum:]_]")
                    (?s "[[:space:]]") (?S "[^[:space:]]") (?b "\\b")
                    (?n "\n") (?t "\t")
                    (_ (regexp-quote (string d))))
                  out)
            (setq i (1+ i))))
         ((memq c '(?\( ?\) ?| ?{ ?}))
          (push (if (and (eq c ?\() (< (+ i 2) n) (eq (aref pattern (1+ i)) ??) (eq (aref pattern (+ i 2)) ?:))
                    (progn (setq i (+ i 2)) "\\(?:")
                  (concat "\\" (string c)))
                out))
         (t (push (string c) out))))
      (setq i (1+ i)))
    (apply #'concat (nreverse out))))

(defun eas-expr-regexp-make (pattern &optional flags)
  "A regexp value for PATTERN with FLAGS, as Vega's regexp() returns."
  (list :regexp (eas-expr-regexp-js (if (stringp pattern) pattern (format "%s" pattern)))
        :fold (and (stringp flags) (string-match-p "i" flags) t)))

(defun eas-expr-regexp-test (re string)
  "t when regexp value (or pattern string) RE matches STRING, else :false."
  (let* ((re (if (stringp re) (eas-expr-regexp-make re) re))
         (case-fold-search (plist-get re :fold)))
    (if (and (stringp string) (string-match-p (plist-get re :regexp) string)) t :false)))

(provide 'eas-expr-regexp)
;;; eas-expr-regexp.el ends here

;;; eas-checkdoc.el --- checkdoc over files, failing on any warning -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; `make checkdoc' runs
;;   emacs -Q --batch -L src -l scripts/eas-checkdoc.el -f eas-checkdoc-batch FILE...
;; and exits 1 when checkdoc warns about any FILE, printing each
;; warning as FILE:LINE: MESSAGE.

;;; Code:

(require 'checkdoc)

(defun eas-checkdoc-file (file)
  "Run checkdoc on FILE; return its warnings as \"FILE:LINE: MESSAGE\" strings."
  (let ((warnings nil))
    (with-current-buffer (find-file-noselect file)
      (let ((checkdoc-create-error-function
             (lambda (text start _end &optional _unfixable)
               (push (format "%s:%d: %s" file (line-number-at-pos start) text) warnings)
               nil))
            (checkdoc-autofix-flag 'never)
            (checkdoc-spellcheck-documentation-flag nil))
        (checkdoc-current-buffer t))
      (kill-buffer))
    (nreverse warnings)))

(defun eas-checkdoc-batch ()
  "Check every file left on the command line; exit 1 on any warning."
  (let ((warnings (mapcan #'eas-checkdoc-file command-line-args-left)))
    (setq command-line-args-left nil)
    (dolist (w warnings) (princ (concat w "\n")))
    (princ (format "checkdoc: %d warning(s)\n" (length warnings)))
    (kill-emacs (if warnings 1 0))))

(provide 'eas-checkdoc)
;;; eas-checkdoc.el ends here

;;; name-check.el --- spike: is the `easel' name free? -*- lexical-binding: t; -*-

;; Usage: emacs -Q --batch -l name-check.el ARCHIVE-CONTENTS-FILE...
;; Each file is a downloaded package archive-contents.  Prints the
;; package count per archive and every package whose name starts with
;; or contains "easel", then every easel* symbol interned in emacs -Q
;; after loading the libraries an easel buffer would touch.

(dolist (file command-line-args-left)
  (let* ((contents (with-temp-buffer
                     (insert-file-contents file)
                     (read (current-buffer))))
         (names (mapcar (lambda (entry) (symbol-name (car entry)))
                        (cdr contents)))
         (hits (seq-filter (lambda (name) (string-match-p "easel" name))
                           names)))
    (princ (format "%s packages=%d easel-hits=%S\n"
                   (file-name-nondirectory file) (length names) hits))))
(setq command-line-args-left nil)

(dolist (feature '(chart svg image xt-mouse org ox eww shr dom json))
  (require feature nil t))
(let (hits)
  (mapatoms (lambda (symbol)
              (when (string-prefix-p "easel" (symbol-name symbol))
                (push symbol hits))))
  (princ (format "emacs-%s easel-symbols=%S\n" emacs-version hits)))

;;; eas-package-layout-test.el --- eas installed flat, as MELPA builds it -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; MELPA installs recipes/eas into one directory: src/*.el and
;; src/*.json at the top, templates/ and examples/ beside them, no
;; test/.  These are the offline halves of scripts/melpa-layout-check
;; (`make melpa-check'), which builds that directory with package-build
;; and runs the doctor and package-lint in it.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defun eas-package-layout-test--data-refs (value)
  "Every (KEY . STRING) under VALUE whose KEY is :file or :url."
  (cond ((eas-object-p value)
         (cl-loop for (k v) on value by #'cddr
                  append (if (and (memq k '(:file :url)) (stringp v)) (list (cons k v))
                           (eas-package-layout-test--data-refs v))))
        ((vectorp value) (cl-mapcan #'eas-package-layout-test--data-refs (append value nil)))
        ((consp value) (cl-mapcan #'eas-package-layout-test--data-refs value))))

(ert-deftest eas-examples-read-only-files-the-package-ships ()
  "Every example's data file exists under examples/, never under test/."
  (let ((examples (eas-test-file "examples")) (seen 0))
    (dolist (file (directory-files-recursively examples "\\.data\\.json\\'"))
      (pcase-dolist (`(,key . ,ref) (eas-package-layout-test--data-refs (eas-json-read-file file)))
        ;; A binding's file reads beside the example; a geojson url, from the root.
        (let ((path (expand-file-name ref (if (eq key :file) (file-name-directory file) eas-template--root))))
          (setq seen (1+ seen))
          (should (equal (list file ref (file-in-directory-p path examples)) (list file ref t)))
          (should (file-readable-p path)))))
    (should (> seen 20))))

(ert-deftest eas-template-root-finds-templates-beside-the-library ()
  "Flat (templates/ next to eas-template.el) and repository layouts both work."
  (let* ((dir (make-temp-file "eas-flat" t))
         (copy (expand-file-name "eas-template.el" dir)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "templates" dir))
          (copy-file (eas-test-file "src/eas-template.el") copy)
          (should (equal (file-name-as-directory eas-template--root)
                         (file-name-as-directory eas-test-root)))
          (should (equal (with-temp-buffer
                           (call-process (expand-file-name invocation-name invocation-directory) nil t nil
                                         "-Q" "--batch" "-L" (eas-test-file "src") "-l" copy
                                         "--eval" "(princ eas-template--root)")
                           (file-name-as-directory (car (last (split-string (buffer-string) "\n" t)))))
                         (file-name-as-directory (file-truename dir)))))
      (delete-directory dir t))))

(ert-deftest eas-melpa-recipe-ships-what-runtime-reads ()
  "recipes/eas lists the libraries, their JSON, templates/ and examples/."
  (let ((files (plist-get (cdr (with-temp-buffer
                                 (insert-file-contents (eas-test-file "recipes/eas"))
                                 (read (current-buffer))))
                          :files)))
    (dolist (f '("src/*.el" "src/*.json" "templates" "examples"))
      (should (member f files)))
    (should (member "src/*-test.el" (assq :exclude files)))))

(provide 'eas-package-layout-test)
;;; eas-package-layout-test.el ends here

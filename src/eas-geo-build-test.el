;;; eas-geo-build-test.el --- tests for building the optional geo module -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The build runs a fake cargo (a shell script that writes the library
;; where cargo would), so these tests need neither Rust nor the crate.

;;; Code:

(require 'cl-lib)
(require 'eas-test-support)
(require 'eas-geo-build)
(require 'eas-template)

(defmacro eas-geo-build-test--in-temp (dir &rest body)
  "Bind DIR to a fresh temporary directory around BODY, then delete it."
  (declare (indent 1))
  `(let ((,dir (file-name-as-directory (make-temp-file "eas-geo-build" t))))
     (unwind-protect (progn ,@body) (delete-directory ,dir t))))

(defun eas-geo-build-test--write (file text &optional mode)
  "Write TEXT to FILE, creating its directory; chmod it to MODE if given."
  (make-directory (file-name-directory file) t)
  (with-temp-file file (insert text))
  (when mode (set-file-modes file mode))
  file)

(defun eas-geo-build-test--fake-cargo (dir status)
  "A fake cargo in DIR that exits STATUS.
On 0 it writes target/release/libeas_geo.so beside --manifest-path,
holding $EMACS_MODULE_HEADER, so a test sees what cargo was given."
  (eas-geo-build-test--write
   (expand-file-name "cargo" dir)
   (concat "#!/bin/sh\n"
           "while [ $# -gt 0 ]; do [ \"$1\" = --target-dir ] && t=\"$2\"; shift; done\n"
           (if (zerop status)
               (concat "mkdir -p \"$t/release\"\n"
                       "printf %s \"$EMACS_MODULE_HEADER\" > \"$t/release/libeas_geo.so\"\n"
                       "echo built\n")
             (format "echo 'error: broken' >&2\nexit %d\n" status)))
   #o755))

(defun eas-geo-build-test--run (plan)
  "Run the build for PLAN; return (TARGET . FAILURE) once cargo exits."
  (let* ((done nil)
         (proc (eas-geo-build--start plan " *eas-geo-build-test*"
                                     (lambda (target &optional failure)
                                       (setq done (cons target failure))))))
    (with-timeout (20 (error "Fake cargo did not finish"))
      (while (not done) (accept-process-output proc 0.05)))
    (kill-buffer " *eas-geo-build-test*")
    done))

(ert-deftest eas-geo-build-installs-under-lib-with-the-emacs-suffix ()
  "The target is lib/eas-geo-module<module-file-suffix> under the eas root."
  (let ((module-file-suffix ".dylib"))
    (should (equal (eas-geo-build-target)
                   (expand-file-name "lib/eas-geo-module.dylib" eas-template--root))))
  (should (equal (eas-geo-build-root) (file-name-as-directory eas-template--root)))
  (should (equal (eas-geo-build-root) (file-name-as-directory eas-test-root))))

(ert-deftest eas-geo-build-finds-what-cargo-built ()
  "The artifact is the platform's lib name under target/release, else nil."
  (eas-geo-build-test--in-temp dir
    (should-not (eas-geo-build-artifact dir))
    (let ((dylib (eas-geo-build-test--write (expand-file-name "target/release/libeas_geo.dylib" dir) "")))
      (should (equal (eas-geo-build-artifact dir) dylib))
      (let ((so (eas-geo-build-test--write (expand-file-name "target/release/libeas_geo.so" dir) "")))
        (should (equal (let ((system-type 'gnu/linux)) (eas-geo-build-artifact dir)) so))
        (should (equal (let ((system-type 'darwin)) (eas-geo-build-artifact dir)) dylib))))))

(ert-deftest eas-geo-build-finds-the-module-header ()
  "$EMACS_MODULE_HEADER (file or directory) wins; nil when nothing has it."
  (eas-geo-build-test--in-temp dir
    (let ((h (eas-geo-build-test--write (expand-file-name "inc/emacs-module.h" dir) "")))
      (cl-letf (((symbol-function 'eas-geo-build--header-dirs) (lambda () (list dir))))
        (let ((process-environment (cons "EMACS_MODULE_HEADER" process-environment)))
          (should-not (eas-geo-build-module-header)))
        (dolist (env (list h (file-name-directory h)))
          (let ((process-environment (cons (concat "EMACS_MODULE_HEADER=" env) process-environment)))
            (should (equal (eas-geo-build-module-header) h))))))))

(ert-deftest eas-geo-build-checks-say-what-is-missing ()
  "No crate, no cargo, no header, no module support: each a clear user-error."
  (eas-geo-build-test--in-temp dir
    (let* ((crate (file-name-directory
                   (eas-geo-build-test--write (expand-file-name "crate/Cargo.toml" dir) "")))
           (cargo (eas-geo-build-test--fake-cargo dir 0))
           (header (eas-geo-build-test--write (expand-file-name "emacs-module.h" dir) ""))
           (eas-geo-build-source-directory (expand-file-name "none" dir))
           (eas-geo-build-cargo cargo)
           (process-environment (cons (concat "EMACS_MODULE_HEADER=" header) process-environment)))
      (should (string-match-p "No module/Cargo.toml"
                              (cadr (should-error (eas-geo-build-check) :type 'user-error))))
      (setq eas-geo-build-source-directory crate)
      (let ((plan (eas-geo-build-check)))
        (should (equal (plist-get plan :source) crate))
        (should (equal (plist-get plan :cargo) cargo))
        (should (equal (plist-get plan :header) header))
        (should (equal (plist-get plan :target) (eas-geo-build-target)))
        (should (equal (last (eas-geo-build-command plan) 2)
                       (list "--target-dir" (expand-file-name "target" crate)))))
      (let ((eas-geo-build-cargo nil) (exec-path nil)
            (process-environment (append '("CARGO" "HOME=/nonexistent") process-environment)))
        (should (string-match-p "Cargo not found"
                                (cadr (should-error (eas-geo-build-check) :type 'user-error)))))
      (cl-letf (((symbol-function 'eas-geo-build-module-header) #'ignore))
        (should (string-match-p "emacs-module.h not found"
                                (cadr (should-error (eas-geo-build-check) :type 'user-error)))))
      (let ((module-file-suffix nil))
        (should (string-match-p "without dynamic module support"
                                (cadr (should-error (eas-geo-build-check) :type 'user-error))))))))

(ert-deftest eas-geo-build-copies-the-library-to-its-target ()
  "A build that succeeds installs the library; one that fails reports it."
  (unless (executable-find "sh")
    (eas-test-skip "no sh to run the fake cargo"))
  (eas-geo-build-test--in-temp dir
    (let* ((target (expand-file-name "root/lib/eas-geo-module.so" dir))
           (plan (list :cargo (eas-geo-build-test--fake-cargo dir 0) :header "/inc/emacs-module.h"
                       :source dir :target target)))
      (should (equal (eas-geo-build-test--run plan) (list target)))
      (should (equal (with-temp-buffer (insert-file-contents target) (buffer-string))
                     "/inc/emacs-module.h"))
      (should (equal (directory-files (file-name-directory target) nil "\\`[^.]")
                     '("eas-geo-module.so")))
      (delete-directory (expand-file-name "target" dir) t)
      (let ((cargo (eas-geo-build-test--fake-cargo (expand-file-name "bad" dir) 3)))
        (should (equal (eas-geo-build-test--run (plist-put (copy-sequence plan) :cargo cargo))
                       '(nil . "cargo exited with status 3"))))
      (let ((cargo (eas-geo-build-test--write (expand-file-name "empty/cargo" dir) "#!/bin/sh\n" #o755)))
        (should (string-match-p "left no eas_geo library"
                                (cdr (eas-geo-build-test--run
                                       (plist-put (copy-sequence plan) :cargo cargo)))))))))

(ert-deftest eas-geo-build-load-keeps-a-loaded-module ()
  "A second build cannot replace a loaded module; it says to restart."
  (let ((had (featurep 'eas-geo-module)))
    (unwind-protect
        (progn (provide 'eas-geo-module)
               (should (string-match-p "restart Emacs" (eas-geo-build-load "/nonexistent.so"))))
      (unless had (setq features (delq 'eas-geo-module features))))))

(ert-deftest eas-geo-build-only-module-targets-need-cargo ()
  "In the Makefile only module and module-clean touch cargo or the module."
  (let ((rule nil) (offenders nil))
    (with-temp-buffer
      (insert-file-contents (eas-test-file "Makefile"))
      (dolist (line (split-string (buffer-string) "\n"))
        (cond ((string-match "\\`\\([-a-zA-Z$(),%_ ]+\\):\\([^=]\\|\\'\\)" line)
               (setq rule (match-string 1 line)))
              ((and (string-prefix-p "\t" line)
                    (string-match-p "cargo\\|eas-geo-build\\|module/target" line)
                    (not (member rule '("module" "module-clean"))))
               (push (cons rule line) offenders)))))
    (should-not offenders)))

(provide 'eas-geo-build-test)
;;; eas-geo-build-test.el ends here

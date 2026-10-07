;;; eas-melpa-check.el --- eas as MELPA installs it: one flat directory -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; scripts/melpa-layout-check drives these stages, each in its own
;; `emacs -Q --batch' so no stage sees another's load-path:
;;
;;   fetch ELPA            package-build and package-lint from MELPA
;;                         (compat from GNU ELPA) into package-user-dir ELPA
;;   copy ELPA ROOT FLAT   recipes/eas expanded by package-build itself,
;;                         ROOT's files copied into the flat directory FLAT
;;   doctor FLAT           with only FLAT on load-path: eas loads from FLAT,
;;                         finds its root there, and every doctor row
;;                         (each template's example resolving and
;;                         rendering) passes or skips
;;   lint ELPA FLAT        package-lint over every library in FLAT
;;
;; Byte-compiling FLAT is plain `batch-byte-compile', run by the script.

;;; Code:

(require 'package)

(declare-function package-recipe-lookup "package-recipe" (name))
(declare-function package-build-expand-files-spec "package-build" (rcp &optional assert repo spec))
(declare-function package-build--copy-package-files "package-build" (rcp files target-dir))
(declare-function package-lint-batch-and-exit "package-lint" ())
(declare-function eas-agent-doctor-rows "eas-agent-health" ())

(defun eas-melpa-check--package-init (elpa)
  "Use ELPA as `package-user-dir', with GNU ELPA and MELPA, and initialize."
  (setq package-user-dir (expand-file-name elpa)
        package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (package-initialize))

(defun eas-melpa-check-fetch (elpa)
  "Install package-build and package-lint into ELPA."
  (eas-melpa-check--package-init elpa)
  (package-refresh-contents)
  (dolist (p '(package-build package-lint))
    (unless (package-installed-p p) (package-install p))))

(defun eas-melpa-check-copy (elpa root flat)
  "Copy the files ROOT's recipes/eas lists into FLAT, as MELPA would.
ELPA holds package-build."
  (eas-melpa-check--package-init elpa)
  (require 'package-build)
  (defvar package-build-recipes-dir)
  (let* ((default-directory (file-name-as-directory (expand-file-name root)))
         (package-build-recipes-dir (expand-file-name "recipes/" default-directory))
         (rcp (package-recipe-lookup "eas"))
         (files (package-build-expand-files-spec rcp t default-directory)))
    (package-build--copy-package-files rcp files flat)
    (message "melpa-layout-check: %d entries from recipes/eas into %s" (length files) flat)))

(defun eas-melpa-check--fail (fmt &rest args)
  "Print FMT with ARGS as a failure and exit 1."
  (message "melpa-layout-check: FAIL %s" (apply #'format fmt args))
  (kill-emacs 1))

(defun eas-melpa-check-doctor (flat)
  "Load eas from FLAT alone and fail unless every doctor row passes or skips."
  (let ((flat (file-name-as-directory (expand-file-name flat))))
    ;; Root-relative data must come from FLAT, not from where Emacs runs.
    (setq default-directory (file-name-as-directory temporary-file-directory))
    (require 'eas)
    (require 'eas-agent)
    (dolist (entry load-history)
      (let ((lib (car entry)))
        (when (and (stringp lib) (string-match-p "\\`\\(ob-\\)?eas[-.]" (file-name-nondirectory lib))
                   (not (string-prefix-p "eas-melpa-check" (file-name-nondirectory lib)))
                   (not (file-in-directory-p lib flat)))
          (eas-melpa-check--fail "%s loaded from outside %s" lib flat))))
    (defvar eas-template--root)
    (unless (equal (file-name-as-directory eas-template--root) flat)
      (eas-melpa-check--fail "eas-template--root is %s, not %s" eas-template--root flat))
    (let* ((rows (eas-agent-doctor-rows))
           (templates (seq-filter (lambda (r) (string-prefix-p "template:" (plist-get r :name))) rows))
           (failed (seq-filter (lambda (r) (equal (plist-get r :status) "fail")) rows)))
      (dolist (r rows)
        (message "  %-4s %s: %s" (plist-get r :status) (plist-get r :name) (plist-get r :detail)))
      (when failed
        (eas-melpa-check--fail "%d doctor row(s) failed: %s" (length failed)
                               (mapconcat (lambda (r) (plist-get r :name)) failed ", ")))
      (when (< (length templates) 2)
        (eas-melpa-check--fail "only %d template rows; templates/ did not load" (length templates)))
      (unless (equal (plist-get (seq-find (lambda (r) (equal (plist-get r :name) "supported.json")) rows)
                                :status)
                     "pass")
        (eas-melpa-check--fail "supported.json is not next to the libraries"))
      (message "melpa-layout-check: doctor ok, %d templates render from %s" (length templates) flat))))

(defun eas-melpa-check-lint (elpa flat)
  "Run package-lint (from ELPA) over every library in FLAT; exit with its status."
  (eas-melpa-check--package-init elpa)
  (require 'package-lint)
  (defvar package-lint-main-file)
  (let ((default-directory (file-name-as-directory (expand-file-name flat))))
    (setq package-lint-main-file "eas.el")
    (setq command-line-args-left (directory-files default-directory nil "\\.el\\'"))
    (message "melpa-layout-check: package-lint over %d files" (length command-line-args-left))
    (package-lint-batch-and-exit)))

(defun eas-melpa-check-batch ()
  "Run the stage named by the first command-line argument; see the commentary."
  (let ((args command-line-args-left))
    (setq command-line-args-left nil)
    (pcase args
      (`("fetch" ,elpa) (eas-melpa-check-fetch elpa))
      (`("copy" ,elpa ,root ,flat) (eas-melpa-check-copy elpa root flat))
      (`("doctor" ,flat) (eas-melpa-check-doctor flat))
      (`("lint" ,elpa ,flat) (eas-melpa-check-lint elpa flat))
      (_ (eas-melpa-check--fail "unknown stage %S" args)))))

(provide 'eas-melpa-check)
;;; eas-melpa-check.el ends here

;;; eas-geo-build.el --- build the optional Rust geo module from source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas has no dependencies.  The Rust crate in module/ is an optional
;; accelerator for the map projection math (docs/design/geo-first-render.md);
;; without it eas draws every map in pure Elisp, which stays the source
;; of truth.  `eas-geo-backend' picks the backend.
;;
;; This file builds the module: `cargo build --release' in module/,
;; then a copy of the library to lib/eas-geo-module<SUFFIX> under the
;; eas root, SUFFIX being the running Emacs's `module-file-suffix'.
;;
;;   M-x eas-geo-build-module    in a compilation buffer, then offers to load
;;   make module                 the same, synchronously (`eas-geo-build-batch')
;;   bin/eas module              the same, from a shell
;;
;; Before it starts, the build checks that this Emacs supports dynamic
;; modules, that cargo is found (`eas-geo-build-cargo', $CARGO,
;; ~/.cargo/bin) and that the Emacs module header emacs-module.h is
;; found ($EMACS_MODULE_HEADER, then the include directories beside
;; this Emacs and the usual prefixes).  Each check fails with a
;; `user-error' that says what to install.  The header's path goes to
;; cargo as $EMACS_MODULE_HEADER.

;;; Code:

(require 'compile)
(require 'seq)

(declare-function eas-geo-backend-active "eas-geo" ())

(defgroup eas-geo-build nil
  "Building the optional Rust geo module."
  :group 'eas :prefix "eas-geo-build-")

(defcustom eas-geo-build-cargo nil
  "The cargo executable, or nil to find it.
nil tries $CARGO, then cargo on the variable `exec-path', then
~/.cargo/bin/cargo."
  :type '(choice (const :tag "Find it" nil) file))

(defcustom eas-geo-build-source-directory nil
  "The crate directory (holding Cargo.toml), or nil for module/ under the root.
Set it to the module/ directory of an eas checkout when this install
does not ship the crate (a MELPA install, for one)."
  :type '(choice (const :tag "module/ under the eas root" nil) directory))

(defconst eas-geo-build-library-name "eas_geo"
  "The crate's library name; cargo writes lib<NAME>.so, .dylib or <NAME>.dll.")

(defconst eas-geo-build-module-name "eas-geo-module"
  "The installed module's base name, before `module-file-suffix'.")

(defconst eas-geo-build--root
  (let ((here (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (if (file-directory-p (expand-file-name "templates" here)) here
      (file-name-directory (directory-file-name here))))
  "The eas root, found as `eas-template--root' is.
Computed here, not required from eas-template, so that building the
module loads nothing else of eas and works in any Emacs with modules.")

(defvar eas-geo-build--buffer "*eas-geo-build*"
  "The buffer `eas-geo-build-module' builds in.")

;;;; Where things are

(defun eas-geo-build-root ()
  "The eas root that module/ and lib/ hang off."
  (file-name-as-directory eas-geo-build--root))

(defun eas-geo-build-source-dir ()
  "The crate directory, or nil when it has no Cargo.toml."
  (let ((dir (file-name-as-directory
              (expand-file-name (or eas-geo-build-source-directory "module")
                                (eas-geo-build-root)))))
    (and (file-readable-p (expand-file-name "Cargo.toml" dir)) dir)))

(defun eas-geo-build-target ()
  "The file the built module is installed as: lib/eas-geo-module<SUFFIX>."
  (expand-file-name (concat "lib/" eas-geo-build-module-name (or module-file-suffix ""))
                    (eas-geo-build-root)))

(defun eas-geo-build-artifact (source)
  "The library cargo built under SOURCE's target/release/, or nil.
The name depends on the platform (lib*.so, lib*.dylib, *.dll), so
take the first that exists, this system's own first."
  (let* ((release (expand-file-name "target/release" source))
         (lib eas-geo-build-library-name)
         (names (list (concat "lib" lib ".so") (concat "lib" lib ".dylib") (concat lib ".dll"))))
    (when (eq system-type 'darwin) (setq names (cons (nth 1 names) names)))
    (when (memq system-type '(windows-nt cygwin)) (setq names (cons (nth 2 names) names)))
    (seq-some (lambda (n) (let ((f (expand-file-name n release))) (and (file-exists-p f) f)))
              names)))

(defun eas-geo-build-cargo ()
  "The cargo executable to build with, or nil when none is found."
  (seq-some (lambda (c) (and c (if (file-name-absolute-p c)
                                   (and (file-executable-p c) c)
                                 (executable-find c))))
            (list eas-geo-build-cargo (getenv "CARGO") "cargo"
                  (expand-file-name "~/.cargo/bin/cargo"))))

(defun eas-geo-build--header-dirs ()
  "Directories that may hold emacs-module.h for the running Emacs."
  (let ((bins (delete-dups (list invocation-directory
                                 (file-name-directory
                                  (file-truename (expand-file-name invocation-name
                                                                   invocation-directory)))))))
    (append
     (mapcan (lambda (bin)
               ;; PREFIX/bin, PREFIX/libexec/emacs/VER/ARCH, Emacs.app/Contents/MacOS.
               (mapcar (lambda (rel) (expand-file-name rel bin))
                       '("../include" "../../include" "../../../../include"
                         "../Resources/include")))
             bins)
     (and installation-directory (list (expand-file-name "include" installation-directory)))
     (split-string (or (getenv "C_INCLUDE_PATH") "") path-separator t)
     '("/usr/include" "/usr/local/include" "/opt/homebrew/include"))))

(defun eas-geo-build-module-header ()
  "The emacs-module.h this Emacs's modules build against, or nil.
$EMACS_MODULE_HEADER, a file or its directory, wins."
  (let ((env (getenv "EMACS_MODULE_HEADER")))
    (seq-some (lambda (f) (and (file-readable-p f) (not (file-directory-p f))
                               (expand-file-name f)))
              (append (and env (list env (expand-file-name "emacs-module.h" env)))
                      (mapcar (lambda (d) (expand-file-name "emacs-module.h" d))
                              (eas-geo-build--header-dirs))))))

;;;; Checks and the build

(defun eas-geo-build-check ()
  "Check that the module can be built here; return a plist.
The plist holds :cargo, :header, :source and :target.  Signal a
`user-error' that says what is missing otherwise."
  (unless (and module-file-suffix (fboundp 'module-load))
    (user-error "This Emacs was built without dynamic module support; eas uses pure Elisp"))
  (let ((cargo (eas-geo-build-cargo))
        (header (eas-geo-build-module-header))
        (source (eas-geo-build-source-dir)))
    (unless source
      (user-error "No module/Cargo.toml under %s; set `eas-geo-build-source-directory' \
to the module/ directory of an eas checkout" (eas-geo-build-root)))
    (unless cargo
      (user-error "Cargo not found; install Rust (https://rustup.rs) or set \
`eas-geo-build-cargo'"))
    (unless header
      (user-error "Emacs module header emacs-module.h not found; install your \
Emacs's development files or set EMACS_MODULE_HEADER"))
    (list :cargo cargo :header header :source source :target (eas-geo-build-target))))

(defun eas-geo-build-command (plan)
  "The cargo command line for PLAN, a plist from `eas-geo-build-check'."
  (let ((source (plist-get plan :source)))
    (list (plist-get plan :cargo) "build" "--release"
          "--manifest-path" (expand-file-name "Cargo.toml" source)
          ;; Explicit, so $CARGO_TARGET_DIR cannot send the output elsewhere.
          "--target-dir" (expand-file-name "target" source))))

(defun eas-geo-build-install (plan)
  "Copy the library cargo built for PLAN to its target; return the target.
Write a temporary file and rename it, so an Emacs that has an older
module loaded keeps a valid mapping of the file it loaded."
  (let ((artifact (eas-geo-build-artifact (plist-get plan :source)))
        (target (plist-get plan :target)))
    (unless artifact
      (user-error "Cargo succeeded but left no %s library under %starget/release"
                  eas-geo-build-library-name (plist-get plan :source)))
    (make-directory (file-name-directory target) t)
    (let ((tmp (make-temp-name (concat target ".tmp"))))
      (copy-file artifact tmp t)
      (rename-file tmp target t))
    target))

(defun eas-geo-build--start (plan buffer on-done)
  "Start cargo for PLAN with output in BUFFER.
Call ON-DONE with the installed target, or with nil and the
failure message, when cargo exits.  Return the process."
  (let ((process-environment
         (cons (concat "EMACS_MODULE_HEADER=" (plist-get plan :header)) process-environment))
        (default-directory (plist-get plan :source))
        (command (eas-geo-build-command plan)))
    (with-current-buffer (get-buffer-create buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "eas-geo-build: %s\n\n" (mapconcat #'shell-quote-argument command " ")))))
    (make-process
     :name "eas-geo-build" :buffer buffer :command command :noquery t
     :connection-type 'pipe
     :sentinel
     (lambda (proc _event)
       (unless (process-live-p proc)
         (let ((status (process-exit-status proc)))
           (if (and (eq (process-status proc) 'exit) (zerop status))
               (condition-case err
                   (funcall on-done (eas-geo-build-install plan))
                 (user-error (funcall on-done nil (error-message-string err))))
             (funcall on-done nil (format "cargo exited with status %s" status)))))))))

(defun eas-geo-build-load (file)
  "Load the module FILE unless a geo module is already loaded.
Return a one-line report of what happened."
  (cond ((featurep 'eas-geo-module)
         "A geo module is already loaded; restart Emacs to use the new build")
        (t (module-load file)
           (format "Loaded %s%s" file
                   (if (fboundp 'eas-geo-backend-active)
                       (format "; active geo backend: %s" (eas-geo-backend-active))
                     "")))))

;;;###autoload
(defun eas-geo-build-module ()
  "Build the optional Rust geo module and install it under lib/.
Run cargo in a compilation buffer, copy the library to
lib/eas-geo-module<SUFFIX> under the eas root, then offer to load it.
eas works without the module; see `eas-geo-backend'."
  (interactive)
  (let ((plan (eas-geo-build-check)))
    (with-current-buffer (get-buffer-create eas-geo-build--buffer)
      (compilation-mode)
      (setq default-directory (plist-get plan :source))
      ;; rustc's "  --> src/lib.rs:12:5", relative to the crate.
      (setq-local compilation-error-regexp-alist
                  '(("^ *--> \\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))))
    (display-buffer eas-geo-build--buffer)
    (eas-geo-build--start
     plan eas-geo-build--buffer
     (lambda (target &optional failure)
       (if (not target)
           (message "eas-geo-build: %s; see %s" failure eas-geo-build--buffer)
         (message "eas-geo-build: installed %s" target)
         (when (y-or-n-p (format "Built %s.  Load it now? " (file-name-nondirectory target)))
           (message "eas-geo-build: %s" (eas-geo-build-load target))))))))

(defun eas-geo-build-batch ()
  "Build the module in batch Emacs (`make module', `bin/eas module').
Stream cargo's output to stderr, print the installed path and exit
0, or print why not and exit 1."
  (let ((result nil) (failure nil))
    (condition-case err
        (let* ((plan (eas-geo-build-check))
               (proc (eas-geo-build--start plan " *eas-geo-build*"
                                           (lambda (target &optional why)
                                             (setq result (or target 'failed) failure why)))))
          (message "eas-geo-build: cargo %s\neas-geo-build: header %s"
                   (plist-get plan :cargo) (plist-get plan :header))
          (set-process-filter proc (lambda (_p s) (princ s #'external-debugging-output)))
          (while (not result) (accept-process-output proc 0.1)))
      (user-error (setq result 'failed failure (error-message-string err))))
    (if (stringp result)
        (progn (princ (format "eas-geo-build: installed %s\n" result))
               (kill-emacs 0))
      (message "eas-geo-build: FAILED: %s" failure)
      (kill-emacs 1))))

(provide 'eas-geo-build)
;;; eas-geo-build.el ends here

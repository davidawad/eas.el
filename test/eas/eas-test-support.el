;;; eas-test-support.el --- shared helpers for eas ERT tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Goldens live in test/eas/golden/.  Set EAS_UPDATE_GOLDEN=1 to
;; rewrite them, then review the diff.

;;; Code:

(require 'ert)
(require 'eas-core)
(require 'eas-geo-native)

;; EAS_GEO_BACKEND=lisp|native|auto runs the suite under one geo backend:
;; `eas-geo-backend' takes its initial value from that variable.

(defconst eas-test-root
  (file-name-directory
   (directory-file-name
    (file-name-directory (directory-file-name
                          (file-name-directory (or load-file-name buffer-file-name))))))
  "The repository root.")

(defun eas-test-file (&rest parts)
  "Return the absolute repository file joined from PARTS."
  (expand-file-name (string-join parts "/") eas-test-root))

(defun eas-test-golden (name actual)
  "Compare string ACTUAL with golden NAME under test/eas/golden/."
  (let ((file (eas-test-file "test/eas/golden" name)))
    (if (or (getenv "EAS_UPDATE_GOLDEN") (not (file-exists-p file)))
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (set-buffer-file-coding-system 'utf-8-unix)
            (insert actual))
          (unless (getenv "EAS_UPDATE_GOLDEN")
            (ert-fail (format "Golden %s was missing and has been written; review and rerun"
                              name))))
      (should (equal (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))
                     actual)))))

(defmacro eas-test-should-code (code &rest body)
  "Assert BODY signals an `eas-error' with reason CODE; return its plist."
  (declare (indent 1))
  `(let ((err (should-error (progn ,@body) :type 'eas-error)))
     (should (equal (plist-get (eas-error-plist err) :code) ,code))
     (eas-error-plist err)))

(defvar eas-test--gallery-templates nil
  "Alist of (EXAMPLE . TEMPLATE) from test/vega-examples/manifest.json.")

(defun eas-test-gallery-template (name)
  "The template that reproduces Vega gallery example NAME.
Most share the example's name; manifest.json records the others."
  (unless eas-test--gallery-templates
    (setq eas-test--gallery-templates
          (mapcar (lambda (e) (cons (plist-get e :name) (plist-get e :template)))
                  (plist-get (eas-json-read-file (eas-test-file "test/vega-examples/manifest.json"))
                             :examples))))
  (or (cdr (assoc name eas-test--gallery-templates)) name))

(defvar eas-test-chart-program "chart"
  "The bin/chart executable used as the static oracle.")

(defun eas-test-skip (reason)
  "Skip the current test with REASON, printed so `make test' shows it.
With TEST_SKIP_LOG set, also append \"TEST: REASON\" to that file."
  (let ((name (ert-test-name (ert-running-test))))
    (message "SKIP %s: %s" name reason)
    (when-let* ((log (getenv "TEST_SKIP_LOG")))
      (write-region (format "%s: %s\n" name reason) nil log t 'silent))
    (ert-skip reason)))

(defun eas-test-require-chart ()
  "Skip the current test, with a visible reason, unless bin/chart is on PATH."
  (unless (executable-find eas-test-chart-program)
    (eas-test-skip (format "bin/chart (%s) not on PATH; install a Vega-Lite CLI renderer (eas-chart-program) to run this oracle check"
                             eas-test-chart-program))))

(provide 'eas-test-support)
;;; eas-test-support.el ends here

;;; eas-compile-memo.el --- transforms shared by layers run once per compile -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (fc-qx1.43).  Each unit of a layered chart runs its
;; ancestors' transforms on their data before its own, so a parent's
;; fold, window or calculate ran once per layer: seven times for the
;; gallery's parallel coordinates.  Vega-Lite runs a shared dataflow
;; once.  While `eas-compile-memo' is in effect, a transform array run
;; on the same rows vector (by identity) with the same params returns
;; the first run's rows, each row a fresh copy so the unit that gets
;; them may change them freely.

;;; Code:

(require 'eas-core)
(require 'eas-transform)

(defvar eas-compile-memo--runs nil
  "Hash table of rows vector -> ((TRANSFORMS ENV) . ROWS) runs, while a
compile is in progress; nil otherwise.")

(defmacro eas-compile-memo (&rest body)
  "Run BODY with transform runs memoized."
  `(let ((eas-compile-memo--runs (make-hash-table :test 'eq))) ,@body))

(defun eas-compile-memo-transform-run (transforms rows env path)
  "`eas-transform-run' of TRANSFORMS on ROWS under ENV (PATH for errors),
reusing an equal run on the same ROWS within `eas-compile-memo'."
  (if (or (null eas-compile-memo--runs) (zerop (length transforms)) (not (vectorp rows)))
      (eas-transform-run transforms rows env path)
    (let* ((key (list transforms env))
           (hit (assoc key (gethash rows eas-compile-memo--runs)))
           (out (if hit (cdr hit)
                  (let ((r (eas-transform-run transforms rows env path)))
                    (push (cons key r) (gethash rows eas-compile-memo--runs))
                    r))))
      (vconcat (mapcar #'copy-sequence out)))))

(provide 'eas-compile-memo)
;;; eas-compile-memo.el ends here

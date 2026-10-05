;;; eas-gc.el --- defer garbage collection while a chart is in use -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.9).  On Linux/Xvfb at Emacs's default
;; `gc-cons-threshold', a 1k-row hover collected 2.5-3 times per move,
;; 80-97 ms of a 123-141 ms move, and every collection traces the whole
;; heap (engine-spikes.md section 8.5).  Binding the threshold around
;; the handler alone only moves that collection to just after it.
;;
;; So, in the style of gcmh: the first event of an interaction raises
;; `gc-cons-threshold' to `eas-gc-cons-threshold', and once Emacs has
;; been idle for `eas-gc-idle-delay' seconds eas collects once and
;; puts the user's value back.  It never lowers a larger value, and it
;; leaves alone a value someone else changed in between.  Set
;; `eas-gc-cons-threshold' to nil to leave GC to the user entirely.
;; Batch Emacs (tests, bin/eas) is left alone.

;;; Code:

(defgroup eas-gc nil
  "Garbage collection around eas chart interaction."
  :group 'eas :prefix "eas-gc-")

(defcustom eas-gc-cons-threshold (* 64 1024 1024)
  "`gc-cons-threshold' while a chart is in use, or nil to leave GC alone."
  :type '(choice (const :tag "Leave GC alone" nil) integer))

(defcustom eas-gc-idle-delay 1.0
  "Idle seconds after which eas collects and restores `gc-cons-threshold'."
  :type 'number)

(defvar eas-gc--saved nil
  "The user's `gc-cons-threshold' while eas has raised it, else nil.")

(defvar eas-gc--timer nil "Pending idle collection.")

(defun eas-gc-defer ()
  "Defer collection until idle; call before handling a chart event."
  (when (and eas-gc-cons-threshold (not noninteractive) (not eas-gc--saved)
             (< gc-cons-threshold eas-gc-cons-threshold))
    (setq eas-gc--saved gc-cons-threshold
          gc-cons-threshold eas-gc-cons-threshold))
  (when (and eas-gc--saved (not (timerp eas-gc--timer)))
    (setq eas-gc--timer (run-with-idle-timer eas-gc-idle-delay nil #'eas-gc-collect))))

(defun eas-gc-collect ()
  "Restore the user's `gc-cons-threshold' and collect once."
  (when (timerp eas-gc--timer) (cancel-timer eas-gc--timer))
  (setq eas-gc--timer nil)
  (when eas-gc--saved
    (when (eql gc-cons-threshold eas-gc-cons-threshold)
      (setq gc-cons-threshold eas-gc--saved))
    (setq eas-gc--saved nil)
    (garbage-collect)))

(provide 'eas-gc)
;;; eas-gc.el ends here

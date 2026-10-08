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
;; `eas-gc-defer' leaves batch Emacs (tests, bin/eas) alone.
;;
;; A render (opening a view, printing its SVG or text) calls
;; `eas-gc-defer-render' (eas-gzi): in a session that is
;; `eas-gc-defer'.  Batch Emacs is never idle, so there it raises the
;; threshold for good: a batch render then collects as a session's
;; does, not while it draws.

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

(defun eas-gc-defer-render ()
  "Defer collection before a render: open a view, print its SVG or text.
In a session this is `eas-gc-defer'.  Batch Emacs (tests, bin/eas) is
never idle, so there `gc-cons-threshold' is raised to
`eas-gc-cons-threshold' and stays: a map's first render conses tens of
megabytes, which a session collects once idle, not while it draws."
  (if noninteractive
      (when (and eas-gc-cons-threshold (< gc-cons-threshold eas-gc-cons-threshold))
        (setq gc-cons-threshold eas-gc-cons-threshold))
    (eas-gc-defer)))

(provide 'eas-gc)
;;; eas-gc.el ends here

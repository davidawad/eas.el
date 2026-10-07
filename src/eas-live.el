;;; eas-live.el --- no work for views nobody sees, frames within budget -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (eas-7r1.19).  Live and animated views cost a frame per
;; push or tick.  Two rules keep that cost to what someone sees and
;; what Emacs can afford:
;;
;;   visibility   a view whose buffer no live window of a visible frame
;;                displays (another tab or workspace, buried, frame
;;                iconified) takes no frames: streams keep their
;;                (coalesced) queue, play timers pause and redraws are
;;                deferred.  When a window shows the buffer again,
;;                `eas-live-wake' draws the latest data once, restarts
;;                timers and redraws (window-buffer-change and
;;                window-configuration-change hooks).
;;   budget       a frame that took longer than its interval pushes the
;;                next one out: the frame rate drops to what the view
;;                can render and frames are skipped, never queued.
;;
;; A view with no buffer (headless: agents, ERT, babel) is always
;; visible, and so is every view in batch Emacs.  `eas-live-visible-function' can be rebound, so ERT drives
;; every case in --batch.

;;; Code:

(require 'eas-core)
(require 'eas-view)

(defvar eas-live-visible-function #'eas-live--visible-p
  "Function of a view returning non-nil when someone may see it.")

(defvar eas-live-budget-factor 1.0
  "How many frame costs must pass between two frames of a view.
A frame that took longer than its interval delays the next one by
this factor times its cost, so the frame rate drops to what the view
can render.")

(defvar eas-live-wake-functions nil
  "Hook run with a VIEW that became visible again.
Streams flush their queue, plays restart their timer and the glue
redraws a stale buffer.")

(defvar eas-live--costs (make-hash-table :test 'equal)
  "View id -> seconds its last frame took.")

(defvar eas-live--starts (make-hash-table :test 'equal)
  "View id -> when its last timed frame started.")

(defvar eas-live--hidden (make-hash-table :test 'equal)
  "View ids that skipped work while hidden and want a wake.")

(defun eas-live--visible-p (view)
  "Non-nil when VIEW has no buffer, or a window of a visible frame has it.
In batch Emacs (no display) every view counts as visible."
  (let ((buffer (eas-view-buffer view)))
    (or noninteractive (null buffer)
        (and (buffer-live-p buffer) (get-buffer-window buffer 'visible) t))))

(defun eas-live-visible-p (view)
  "Non-nil when VIEW (an id or view) may be seen; see `eas-live-visible-function'."
  (funcall eas-live-visible-function (eas-view-get view)))

(defun eas-live-defer (view)
  "Note that hidden VIEW skipped work and wants `eas-live-wake'."
  (puthash (eas-view-id (eas-view-get view)) t eas-live--hidden))

(defun eas-live-wake (&optional _)
  "Run `eas-live-wake-functions' for deferred views that are visible now.
Called from window hooks with an ignored argument."
  (let (woken)
    (maphash (lambda (id _)
               (let ((view (gethash id eas-views)))
                 (cond ((null view) (push id woken))
                       ((eas-live-visible-p view)
                        (push id woken)
                        (run-hook-with-args 'eas-live-wake-functions view)))))
             eas-live--hidden)
    (dolist (id woken) (remhash id eas-live--hidden))
    woken))

(defun eas-live--wake-soon (&optional _)
  "Wake deferred views from a timer: window hooks run inside redisplay."
  (when (> (hash-table-count eas-live--hidden) 0)
    (run-at-time 0 nil #'eas-live-wake)))

(add-hook 'window-buffer-change-functions #'eas-live--wake-soon)
(add-hook 'window-configuration-change-hook #'eas-live--wake-soon)
(add-hook 'window-state-change-functions #'eas-live--wake-soon)

;;; Budget

(defmacro eas-live-timed (view &rest body)
  "Run BODY, recording how long it took as VIEW's frame cost."
  (declare (indent 1) (debug t))
  (let ((t0 (make-symbol "t0")) (id (make-symbol "id")))
    `(let ((,id (eas-live--id ,view)) (,t0 (float-time)))
       (puthash ,id ,t0 eas-live--starts)
       (prog1 (progn ,@body)
         (puthash ,id (- (float-time) ,t0) eas-live--costs)))))

(defun eas-live--id (view)
  "VIEW's id; VIEW is an id or a view."
  (if (eas-view-p view) (eas-view-id view) view))

(defun eas-live-cost (view)
  "Seconds VIEW's (an id or view) last timed frame took, or 0."
  (gethash (eas-live--id view) eas-live--costs 0))

(defun eas-live-interval (view interval)
  "The interval in seconds VIEW can keep: INTERVAL, or more after slow frames."
  (max interval (* eas-live-budget-factor (eas-live-cost view))))

(defun eas-live-due-p (view interval now)
  "Non-nil when VIEW, ticking every INTERVAL seconds, may take a frame at NOW.
A view whose last frame took longer than INTERVAL skips ticks until
that cost (times `eas-live-budget-factor') has passed since the frame
started, so a slow view never falls behind its timer."
  (let ((cost (eas-live-cost view))
        (start (gethash (eas-live--id view) eas-live--starts)))
    (or (null start) (<= cost interval)
        (>= now (+ start (* eas-live-budget-factor cost))))))

(provide 'eas-live)
;;; eas-live.el ends here

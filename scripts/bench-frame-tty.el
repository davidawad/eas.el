;;; bench-frame-tty.el --- ms per live text frame in a real terminal -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.1.  The batch bench (bench-frame-text.el) cannot redisplay.
;; This one runs inside `emacs -nw' and times a frame as the terminal
;; sees it: the update, `eas-mode-redraw' and a forced `redisplay'.  It
;; then checks the buffer against a render from scratch, so the patch
;; model held under real redisplay (jit-lock adds `fontified').  Results
;; go to the file named by `bench-frame-tty-out'; Emacs then exits.
;;
;;   tmux -L eas new-session -d -x 110 -y 46 \
;;     "EAS_BENCH_MODE=byte emacs -nw -Q -l scripts/bench-frame-tty.el \
;;        --eval '(setq bench-frame-tty-out \"/tmp/tty.txt\")' -f bench-frame-tty-run"

;;; Code:

(require 'cl-lib)
;; An interactive Emacs would native-compile the loaded .elc files in
;; the background while the frames are timed.
(setq native-comp-jit-compilation nil)
(setq bench-frame-text-no-report t)
(defvar bench-frame-text-mode)
(defvar bench-frame-text-root)
;; EAS_BENCH_MODE (interpreted, byte, native) and EAS_BENCH_ROOT pick
;; the build and the tree, as bench-frame-text.el's arguments do.
(setq bench-frame-text-mode (or (getenv "EAS_BENCH_MODE") "byte"))
(when (getenv "EAS_BENCH_ROOT") (setq bench-frame-text-root (file-name-as-directory (getenv "EAS_BENCH_ROOT"))))
(load (expand-file-name "bench-frame-text.el" (file-name-directory (or load-file-name buffer-file-name))))

(defvar bench-frame-tty-out nil "File the results are written to.")
(defvar bench-frame-tty-frames 40 "Frames timed per workload.")

(defun bench-frame-tty--frames (view step)
  "Mean ms of STEP, a redraw of VIEW and a redisplay; and whether it matched."
  (let ((buffer (get-buffer-create "*bench-frame-tty*")) (total 0.0))
    (switch-to-buffer buffer)
    (delete-other-windows)
    (setq-local eas-mode--view view)
    (funcall step 0) (eas-mode-redraw buffer) (redisplay t)
    (garbage-collect)
    (dotimes (i bench-frame-tty-frames)
      (let ((t0 (float-time)))
        (funcall step (1+ i))
        (eas-mode-redraw buffer)
        (redisplay t)
        (cl-incf total (- (float-time) t0))))
    (let* ((full (let ((eas-render-cache-enabled nil))
                   (concat (eas-text-render (eas-view-scene view)) "\n" (eas-mode-strip-string view))))
           (same (equal-including-properties
                  full (let ((s (buffer-string))) (remove-list-of-text-properties 0 (length s) '(fontified) s) s))))
      (kill-buffer buffer)
      (list (/ (* 1000 total) bench-frame-tty-frames) same))))

(defun bench-frame-tty-run ()
  "Time the text workloads with redisplay; write the table and exit."
  (let ((bench-frame-text--size (list :cols (- (window-max-chars-per-line) 1)
                                      :rows (- (window-body-height) 2)))
        (rows nil))
    (cl-letf (((symbol-function 'bench-frame-text--frames) #'bench-frame-tty--frames))
      (pcase-dolist (`(,name ,fn . ,args) bench-frame-text-workloads)
        (unless (string-match-p "pi-monte" name)
          (push (cons name (condition-case err (apply fn args) (error (list nil (format "%S" err))))) rows))))
    (with-temp-file bench-frame-tty-out
      (insert (format "terminal %sx%s, %s, eas-text-render %s\n\n" (frame-width) (frame-height) (getenv "TERM")
                      (if (byte-code-function-p (symbol-function 'eas-text-render)) "byte-compiled" "not byte-compiled")))
      (insert "| workload | ms/frame with redisplay | buffer equals a full render |\n|---|---|---|\n")
      (dolist (r (nreverse rows))
        (insert (format "| %s | %s | %s |\n" (car r) (if (nth 1 r) (format "%.2f" (nth 1 r)) "n/a") (nth 2 r)))))
    (kill-emacs 0)))

(provide 'bench-frame-tty)
;;; bench-frame-tty.el ends here

;;; tty-mouse.el --- spike: xterm-mouse decoding and grid redisplay in a tty -*- lexical-binding: t; -*-

;; Usage (driven by tty-mouse.sh inside tmux):
;;   emacs -nw -Q -l tty-mouse.el
;; Logs every mouse event Emacs decodes to $SPIKE_OUT, then, on F5,
;; times rewriting a 100x30 propertized grid with a forced redisplay
;; and exits.

(require 'xt-mouse)
(defvar spike-out (getenv "SPIKE_OUT"))
(defun spike-log (fmt &rest args)
  (with-temp-buffer
    (insert (apply #'format fmt args) "\n")
    (append-to-file (point-min) (point-max) spike-out)))

(xterm-mouse-mode 1)
(setq track-mouse t)
(spike-log "xterm-mouse-mode=%s track-mouse=%s tty-type=%s"
           xterm-mouse-mode track-mouse (tty-type))

(defun spike-record-mouse (event)
  (interactive "e")
  (let ((posn (event-start event)))
    (spike-log "event=%s col-row=%S" (car event) (posn-col-row posn))))

(dolist (key '([mouse-1] [down-mouse-1] [drag-mouse-1] [mouse-movement]
               [mouse-4] [mouse-5] [wheel-up] [wheel-down]))
  (global-set-key key #'spike-record-mouse))

(defun spike-grid ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*grid*"))
  (let ((rows 30) (cols 100) (reps 50) (start (float-time)))
    (dotimes (k reps)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dotimes (r rows)
          (dotimes (c cols)
            (insert (propertize (string (+ #x2800 (% (+ k (* r c)) 255)))
                                'easel-datum (+ (* r cols) c) 'help-echo "d")))
          (insert "\n")))
      (redisplay t))
    (spike-log "grid-rewrite+redisplay 100x30 mean=%.2fms"
               (/ (* 1000 (- (float-time) start)) reps)))
  ;; Crosshair-style change: one column of 30 cells flips, rest untouched.
  (let ((reps 50) (start (float-time)) (inhibit-read-only t))
    (dotimes (k reps)
      (dotimes (r 30)
        (let ((pos (+ 1 (* r 101) (% (* k 7) 100))))
          (goto-char pos)
          (delete-char 1)
          (insert (propertize "|" 'easel-datum k))))
      (redisplay t))
    (spike-log "one-column-change+redisplay 100x30 mean=%.2fms"
               (/ (* 1000 (- (float-time) start)) reps)))
  (kill-emacs 0))
(global-set-key [f5] #'spike-grid)

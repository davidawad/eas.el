;;; ns-common.el --- shared helpers for the macOS NS GUI spikes (fc-qx1.24) -*- lexical-binding: t; -*-

;; Loaded by run-ns.sh before each ns-*.el spike, in a GUI (NS) Emacs.
;; Counterpart of common.el, which needs X (xdotool, /proc).

(require 'svg)
(require 'cl-lib)

(defvar spike-out (getenv "SPIKE_OUT"))

(defun spike-log (fmt &rest args)
  "Append FMT formatted with ARGS, and a newline, to `spike-out'."
  (let ((line (apply #'format fmt args)))
    (with-temp-buffer
      (insert line "\n")
      (append-to-file (point-min) (point-max) spike-out))))

(defun spike-now-ms () (* 1000.0 (float-time)))

(defun spike-stats (xs)
  "Plist :mean :p50 :p95 :max of the numbers XS."
  (let* ((v (vconcat (sort (copy-sequence xs) #'<))) (n (length v)))
    (list :n n :mean (/ (apply #'+ xs) (float n))
          :p50 (aref v (/ n 2))
          :p95 (aref v (min (1- n) (floor (* 0.95 n))))
          :max (aref v (1- n)))))

(defun spike-fmt (stats)
  (format "mean=%7.2f p50=%7.2f p95=%7.2f max=%7.2f"
          (plist-get stats :mean) (plist-get stats :p50) (plist-get stats :p95) (plist-get stats :max)))

(defun spike-rss-kb ()
  "Resident set size of this Emacs in KB (ps)."
  (with-temp-buffer
    (call-process "ps" nil t nil "-o" "rss=" "-p" (number-to-string (emacs-pid)))
    (string-to-number (buffer-string))))

(defun spike-setup-frame (&optional w h)
  "One window, W x H pixels (default 1000x700), no chrome."
  (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1) (blink-cursor-mode -1)
  (setq inhibit-startup-screen t)
  (set-frame-position nil 40 60)
  (set-frame-size nil (or w 1000) (or h 700) t)
  (delete-other-windows)
  (sit-for 0.3)
  (redisplay t))

(defun spike-run (fn)
  "Run FN after startup, log any error, and exit."
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer 0.8 nil
                              (lambda ()
                                (condition-case err (funcall fn)
                                  (error (spike-log "ERROR %S" err)))
                                (kill-emacs 0))))))

;;; ns-common.el ends here

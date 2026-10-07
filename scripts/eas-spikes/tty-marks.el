;;; tty-marks.el --- spike: xterm-mouse clicks and hover land on the right marks (fc-qx1.24) -*- lexical-binding: t; -*-

;; Driven by tty-marks.sh inside a private tmux server:
;;   emacs -nw -Q -L SRC -l tty-marks.el
;; Opens the "bars" template as a text chart, writes $SPIKE_PLAN (one
;; line per bar: datum, terminal col, terminal row, both 1-based) and
;; then logs, for every mouse event Emacs decodes, the event, the cell
;; and the datum the view reports hovered or selected.  The driver sends
;; the SGR reports; tty-marks.sh compares plan and log.

(require 'eas)
(require 'eas-mode)
(require 'eas-template)

(defvar spike-out (getenv "SPIKE_OUT"))
(defvar spike-plan (getenv "SPIKE_PLAN"))

(defun spike-log (fmt &rest args)
  (with-temp-buffer
    (insert (apply #'format fmt args) "\n")
    (append-to-file (point-min) (point-max) spike-out)))

(defvar spike-view nil)

(defun spike-datum-state ()
  "The hovered datum of the view, or nil."
  (let ((h (plist-get (eas-view-state spike-view) :hover)))
    (and h (format "%S" h))))

(defun spike-post-command ()
  (when (and (consp last-input-event) (memq (car last-input-event) '(mouse-1 down-mouse-1 mouse-movement)))
    (let ((posn (event-start last-input-event)))
      (spike-log "event=%s col-row=%S hover=%s px=%S"
                 (car last-input-event) (posn-col-row posn) (spike-datum-state)
                 (ignore-errors (eas-mode-event-px last-input-event t))))))

(defun spike-main ()
  (setq spike-view (eas-view-open "bars" :bindings (eas-template-example "bars") :id "bars"))
  (eas-show spike-view 'text)
  (delete-other-windows)
  (eas-mode-refresh)
  (redisplay t)
  (spike-log "TERM(frame)=%S TERM(process)=%S capable=%S" (getenv "TERM" (selected-frame)) (getenv "TERM")
             (eas-tty-mouse-capable-p))
  (spike-log "xterm-mouse-mode=%s tty-type=%s window=%S inside-edges=%S cell=%S"
             xterm-mouse-mode (tty-type) (window-edges) (window-inside-edges)
             (plist-get (plist-get (eas-view-scene spike-view) :size) :cell))
  (let* ((cell (plist-get (plist-get (eas-view-scene spike-view) :size) :cell))
         (edges (window-inside-edges))
         (mark (aref (plist-get (aref (plist-get (eas-view-scene spike-view) :views) 0) :marks) 0))
         lines)
    (cl-loop for item across (plist-get mark :items)
             for px = (+ (plist-get item :x) (/ (plist-get item :w) 2.0))
             for py = (+ (plist-get item :y) (/ (plist-get item :h) 2.0))
             do (push (format "%S %d %d" (plist-get item :datum)
                              (+ (nth 0 edges) (floor px (aref cell 0)) 1)
                              (+ (nth 1 edges) (floor py (aref cell 1)) 1))
                      lines))
    (with-temp-file spike-plan (insert (mapconcat #'identity (nreverse lines) "\n") "\n")))
  (add-hook 'post-command-hook #'spike-post-command))

(add-hook 'emacs-startup-hook (lambda () (run-with-timer 0.5 nil #'spike-main)))
(global-set-key [f5] (lambda () (interactive) (kill-emacs 0)))

;;; tty-marks.el ends here

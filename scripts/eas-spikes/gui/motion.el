;;; motion.el --- spike: xdotool motion -> eas hover, per-move vs idle redraw -*- lexical-binding: t; -*-

;; Usage: scripts/eas-spikes/gui/run.sh motion.el OUT   (needs xdotool)
;; A real eas view (eas-view-mode, its own keymap and glue) is shown
;; at 800x400.  xdotool, a separate X client, moves the pointer across
;; the plot MOVES times, either as fast as it can ("flood") or every
;; 8 ms (125 Hz, a typical mouse report rate).  The command loop is left
;; free, as in real use.  For each redraw strategy:
;;   idle       the shipped default: `eas-mode--schedule' redraws on
;;              an idle timer (latest state wins)
;;   immediate  every view change redraws synchronously
;; it reports pointer events handled, redraws done, mean handler and
;; redraw ms, and the lag from xdotool's last move to the end of the
;; redisplay that shows the final position.  Each case runs at the
;; default `gc-cons-threshold' (800 KB) and at 64 MB.  Every redraw is followed by
;; (redisplay t) so its end time includes rasterization.

(require 'eas)
(require 'eas-mode)

(defvar spike--handled 0)
(defvar spike--handler-ms nil)
(defvar spike--redraws 0)
(defvar spike--redraw-ms nil)
(defvar spike--last-redraw-end 0)
(defvar spike--redraw-ends nil "End times of redraws, ms.")
(defvar spike--activity 0 "Time of the last handler or redraw, ms.")
(defvar spike--last-px nil)
(defvar spike--immediate nil)
(defvar spike--queue nil "Phases still to run: (LABEL SPEC MODE RATE MOVES).")

(defun spike--pointer-around (fn event)
  (let ((t0 (spike-now-ms)))
    (funcall fn event)
    (cl-incf spike--handled)
    (setq spike--last-px eas-mode--last-px spike--activity (spike-now-ms))
    (push (- spike--activity t0) spike--handler-ms)))

(defun spike--redraw-around (fn &rest args)
  (let ((t0 (spike-now-ms)))
    (apply fn args)
    (redisplay t)
    (cl-incf spike--redraws)
    (setq spike--last-redraw-end (spike-now-ms) spike--activity spike--last-redraw-end)
    (push spike--last-redraw-end spike--redraw-ends)
    (push (- spike--last-redraw-end t0) spike--redraw-ms)))

(defun spike--schedule-around (fn view)
  (if (not spike--immediate) (funcall fn view)
    (when-let* ((buffer (eas-view-buffer view)))
      (eas-mode-redraw buffer))))

(advice-add 'eas-mode-pointer :around #'spike--pointer-around)
(advice-add 'eas-mode-redraw :around #'spike--redraw-around)
(advice-add 'eas-mode--schedule :around #'spike--schedule-around)

(defun spike--when-quiet (fn)
  "Call FN once no handler or redraw has run for 2 s."
  (let ((timer nil))
    (setq timer (run-with-timer 1 0.5 (lambda ()
                                        (when (> (- (spike-now-ms) spike--activity) 2000)
                                          (cancel-timer timer)
                                          (funcall fn)))))))

(defun spike--line-spec (n)
  (list :data (list :values (vconcat (mapcar (lambda (i) (list :t i :p (+ 100 (* 10 (sin (/ i 37.0))))))
                                             (number-sequence 0 (1- n)))))
        :layer (vector (list :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))]
                             :mark "line"
                             :encoding '(:x (:field "t" :type "quantitative")
                                         :y (:field "p" :type "quantitative" :scale (:zero :false))))
                       (list :transform [(:filter (:param "hover" :empty :false))]
                             :mark "rule" :encoding '(:x (:field "t" :type "quantitative"))))))

(defun spike--open (spec)
  (clrhash eas-views)
  (let* ((view (eas-view-open spec :id "motion" :size '(800 . 400) :target 'svg))
         (buffer (get-buffer-create "*eas motion*")))
    (switch-to-buffer buffer)
    (eas-view-mode)
    (setq eas-mode--view view)
    (setf (eas-view-buffer view) buffer)
    (eas-mode-redraw buffer)
    view))

(defun spike--next ()
  (if (null spike--queue) (kill-emacs 0)
    (pcase-let* ((`(,label ,spec ,mode ,rate ,moves ,gc) (pop spike--queue))
                 (view (spike--open spec))
                 (o (spike-screen-origin))
                 (args (cl-loop for i below moves
                                append (append (list "mousemove" (number-to-string (+ (car o) 60 (round (* i (/ 680.0 moves)))))
                                                     (number-to-string (+ (cdr o) 200 (% i 3))))
                                               (when rate (list "sleep" (format "%.3f" (/ 1.0 rate)))))))
                 (final (vector (float (+ 60 (round (* (1- moves) (/ 680.0 moves))))) (float (+ 200 (% (1- moves) 3))))))
      (setq gc-cons-threshold gc)
      (setq spike--immediate (eq mode 'immediate)
            spike--handled 0 spike--handler-ms nil spike--redraws 0 spike--redraw-ms nil)
      (redisplay t)
      (call-process "xdotool" nil nil nil "mousemove" (number-to-string (+ (car o) 20)) (number-to-string (+ (cdr o) 20)))
      (sit-for 0.5)
      (setq spike--handled 0 spike--handler-ms nil spike--redraws 0 spike--redraw-ms nil spike--redraw-ends nil)
      (let ((start (spike-now-ms)))
        (make-process
         :name "xdotool" :buffer (generate-new-buffer " *xdotool*")
         :command (append (list "sh" "-c" "xdotool \"$@\" && date +%s.%N" "sh") args)
         :connection-type 'pipe
         :sentinel
         (lambda (proc _)
           (unless (process-live-p proc)
             (let ((end (* 1000 (string-to-number (with-current-buffer (process-buffer proc)
                                                     (car (last (split-string (buffer-string) "\n" t))))))))
               (spike--when-quiet
                (lambda ()
                  (spike-log "%-16s %-9s %-5s moves=%d sent-in=%6.0fms handled=%-4d redraws=%-4d (during motion %d) handler %s"
                             label mode (if rate (format "%dHz" rate) "flood") moves (- end start)
                             spike--handled spike--redraws (cl-count-if (lambda (e) (< e end)) spike--redraw-ends)
                             (if spike--handler-ms (spike-fmt (spike-stats spike--handler-ms)) "-"))
                  (spike-log "%-16s %-9s %-5s redraw+raster %s; last px %S (sent %S) lag(last move -> last frame)=%.0fms"
                             label mode (if rate (format "%dHz" rate) "flood")
                             (if spike--redraw-ms (spike-fmt (spike-stats spike--redraw-ms)) "-")
                             spike--last-px final (- spike--last-redraw-end end))
                  (kill-buffer "*eas motion*")
                  (spike--next)))))))))))

(spike-run-async
 (lambda ()
   (spike-setup-frame)
   (spike-log "emacs %s window-system=%s xdotool=%s" emacs-version window-system (executable-find "xdotool"))
   (dolist (gc (list gc-cons-threshold (* 64 1024 1024)))
     (dolist (n '(1000 10000))
       (dolist (mode '(idle immediate))
         (dolist (rate '(125 nil))
           (setq spike--queue
                 (append spike--queue
                         (list (list (format "%s line-f %dk" (if (> gc 1000000) "gc64" "gc0.8") (/ n 1000))
                                     (spike--line-spec n) mode rate 150 gc))))))))
   (spike--next)))

;;; motion.el ends here

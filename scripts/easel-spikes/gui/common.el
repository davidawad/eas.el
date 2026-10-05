;;; common.el --- shared helpers for the GUI spikes (fc-qx1.23) -*- lexical-binding: t; -*-

;; Loaded by run.sh before each spike, in a GUI (X) Emacs.

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
  "Resident set size of this Emacs in KB."
  (with-temp-buffer
    (insert-file-contents "/proc/self/status")
    (if (re-search-forward "^VmRSS:[ \t]+\\([0-9]+\\)" nil t) (string-to-number (match-string 1)) -1)))

(defun spike-bars-svg (n x &optional title-p)
  "SVG string 800x400: N bars, a crosshair at X and a label (direct DOM).
When TITLE-P, each bar carries a <title> child."
  (let ((w (/ 780.0 n)) children)
    (dotimes (i n)
      (push (apply #'dom-node 'rect
                   `((x . ,(+ 10 (* i w))) (y . ,(- 390 (% (* i 37) 300)))
                     (width . ,(max 0.5 (* 0.8 w))) (height . ,(% (* i 37) 300))
                     (fill . "#4c78a8"))
                   (and title-p (list (dom-node 'title nil (format "bar %d" i)))))
            children))
    (push (dom-node 'line `((x1 . ,x) (y1 . 0) (x2 . ,x) (y2 . 400) (stroke . "#888"))) children)
    (push (dom-node 'text '((x . 20) (y . 20) (font-size . 12)) (format "x=%s" x)) children)
    (with-temp-buffer
      (svg-print (apply #'dom-node 'svg '((width . 800) (height . 400) (xmlns . "http://www.w3.org/2000/svg"))
                        (nreverse children)))
      (buffer-string))))

(defun spike-grid-map (n)
  "A :map of N rect areas tiling the 800x400 image, row-major."
  (let* ((cols (max 1 (ceiling (sqrt (* 2 n))))) (rows (max 1 (ceiling n (float cols))))
         (w (/ 800.0 cols)) (h (/ 400.0 rows)) areas)
    (dotimes (i n)
      (let ((x0 (round (* (% i cols) w))) (y0 (round (* (/ i cols) h))))
        (push `((rect . ((,x0 . ,y0) . (,(round (+ x0 w)) . ,(round (+ y0 h)))))
                ,(intern (format "area-%d" i))
                (help-echo ,(format "area %d" i) pointer hand))
              areas)))
    (nreverse areas)))

(defun spike-setup-frame ()
  "One window, 1200x700 pixels, no chrome."
  (menu-bar-mode -1) (tool-bar-mode -1) (scroll-bar-mode -1) (blink-cursor-mode -1)
  (setq inhibit-startup-screen t)
  (set-frame-size nil 1200 700 t)
  (set-frame-position nil 0 0)
  (delete-other-windows)
  ;; Xvfb has no window manager.  Until the frame is focused and has
  ;; seen one button press, Emacs reports no mouse-movement at all, even
  ;; with `track-mouse' (pointer warps and XTEST motion are both
  ;; dropped; clicks and keys arrive).  Focus it and click once in
  ;; *scratch*, before any chart is shown.
  (call-process "xdotool" nil nil nil "windowfocus" "--sync" (frame-parameter nil 'outer-window-id))
  (let ((f (frame-edges nil 'inner-edges)))
    (call-process "xdotool" nil nil nil "mousemove" (number-to-string (+ (nth 0 f) 50))
                  (number-to-string (+ (nth 1 f) 50)) "click" "1"))
  (sit-for 0.5)
  (redisplay t))

(defun spike-image-origin ()
  "Frame-relative pixel (X . Y) of the image at `point-min' in the selected window."
  (let ((edges (window-inside-pixel-edges))
        (xy (posn-x-y (posn-at-point (point-min)))))
    (cons (+ (nth 0 edges) (car xy)) (+ (nth 1 edges) (cdr xy)))))

(defun spike-screen-origin ()
  "Root-window pixel (X . Y) of the image at `point-min' (for xdotool)."
  (let ((o (spike-image-origin)) (f (frame-edges nil 'inner-edges)))
    (cons (+ (nth 0 f) (car o)) (+ (nth 1 f) (cdr o)))))

(defun spike-show (image)
  "Show IMAGE alone in buffer *spike* in the selected window."
  (switch-to-buffer (get-buffer-create "*spike*"))
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert-image image "[img]")
    (goto-char (point-min)))
  (redisplay t))

(defun spike-swap (image)
  "Replace the displayed image by IMAGE (no buffer rebuild)."
  (with-current-buffer "*spike*"
    (with-silent-modifications (put-text-property (point-min) (1+ (point-min)) 'display image))))

(defun spike-read-motion (timeout)
  "Read events until a mouse-movement; nil after TIMEOUT seconds."
  (let ((track-mouse t) ev (deadline (+ (float-time) timeout)))
    (while (and (null ev) (< (float-time) deadline))
      (let ((e (read-event nil nil (max 0.001 (- deadline (float-time))))))
        (when (and (consp e) (eq (car e) 'mouse-movement)) (setq ev e))))
    ev))

(defun spike-run (fn)
  "Run FN after startup, log any error, and exit."
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer 0.5 nil
                              (lambda ()
                                (condition-case err (funcall fn)
                                  (error (spike-log "ERROR %S" err)))
                                (kill-emacs 0))))))

;;; common.el ends here

(defun spike-run-async (fn)
  "Run FN after startup; FN arranges its own `kill-emacs'."
  (add-hook 'emacs-startup-hook
            (lambda ()
              (run-with-timer 0.5 nil
                              (lambda ()
                                (condition-case err (funcall fn)
                                  (error (spike-log "ERROR %S" err) (kill-emacs 1))))))))

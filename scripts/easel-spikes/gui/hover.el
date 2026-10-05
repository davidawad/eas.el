;;; hover.el --- spike: posn-object-x-y and :scale, :map hover cost, <title> -*- lexical-binding: t; -*-

;; Usage: scripts/easel-spikes/gui/run.sh hover.el OUT
;; The pointer is warped with `set-mouse-pixel-position' (an X
;; XWarpPointer, which the server reports as real motion) and the
;; resulting mouse-movement is read back with `track-mouse' on.
;;   posn   for image :scale 1, 2 and create-image's default (no :scale):
;;          posn-object-x-y at known display offsets, the hot-spot id
;;          (posn-area) for one map area, and `easel-mode-event-px'.
;;   map    ms from warp to mouse-movement read, with a :map of 0, 100,
;;          1k, 10k areas; help-echo deliveries counted.
;;   title  an SVG whose bars carry <title>, no :map: does any tooltip or
;;          help-echo appear?  Positive control: the same image with a :map.

(require 'easel)
(require 'easel-mode)

(defvar spike--helps nil "help-echo strings seen by `show-help-function'.")
(defvar spike--tips nil "Strings `x-show-tip' was asked to show.")

(defun spike--count-help (help) (when help (push help spike--helps)))

(defun spike--warp (x y)
  "Warp to image-relative display pixel X Y; return the mouse-movement event."
  (let ((o (spike-image-origin)))
    (set-mouse-pixel-position (selected-frame) (+ (car o) x) (+ (cdr o) y))
    (spike-read-motion 2)))

(defun spike--posn (scale)
  (let* ((map '(((rect . ((100 . 50) . (200 . 100))) hot (help-echo "hot"))))
         (img (if scale (create-image (spike-bars-svg 100 400) 'svg t :scale scale :map map)
                (create-image (spike-bars-svg 100 400) 'svg t :map map))))
    (spike-show img)
    (spike--warp 5 5)
    (dolist (d '((150 . 75) (300 . 150)))
      (let* ((ev (spike--warp (car d) (cdr d))) (posn (and ev (event-start ev))))
        (spike-log "posn image-:scale=%S displayed-size=%S warp-display-px=%S -> posn-object-x-y=%S posn-area=%S easel-mode-event-px=%S"
                   (plist-get (cdr img) :scale) (image-size img t) d
                   (and posn (posn-object-x-y posn)) (and posn (posn-area posn))
                   (and ev (condition-case err (easel-mode-event-px ev) (error (list 'ERROR err)))))
        ;; The glue before fc-qx1.23 divided by (or :scale 1).
        (spike-log "posn   pre-fix glue (/ x (float (or :scale 1))) -> %S"
                   (and posn (condition-case err
                                 (/ (car (posn-object-x-y posn))
                                    (float (or (plist-get (cdr (posn-image posn)) :scale) 1)))
                               (error (list 'ERROR err)))))))))

(defun spike--map-cost (n reps)
  (let ((img (create-image (spike-bars-svg 100 400) 'svg t :scale 1 :map (spike-grid-map n)))
        xs (misses 0))
    (spike-show img)
    (setq spike--helps nil)
    (spike--warp 1 1)
    (dotimes (k reps)
      (let ((x (+ 2 (% (* k 97) 796))) (y (+ 2 (% (* k 53) 396))) (t0 (spike-now-ms)))
        (if (spike--warp x y) (push (- (spike-now-ms) t0) xs) (cl-incf misses))))
    (spike-log "map areas=%-5d warp->mouse-movement %s missed=%d help-echo-deliveries=%d last=%S"
               n (spike-fmt (spike-stats xs)) misses (length spike--helps) (car spike--helps))))

(defun spike--title (with-map)
  (let ((img (if with-map
                 (create-image (spike-bars-svg 20 400 t) 'svg t :scale 1 :map (spike-grid-map 20))
               (create-image (spike-bars-svg 20 400 t) 'svg t :scale 1))))
    (spike-show img)
    (setq spike--tips nil spike--helps nil)
    (spike--warp 30 380)
    (dotimes (k 5)
      (spike--warp (+ 30 (* 39 k)) 380)
      (sit-for 0.6))
    (spike-log "title svg-has-<title>=t :map=%s -> x-show-tip calls=%d %S; help-echo=%d"
               with-map (length spike--tips) (seq-uniq spike--tips) (length spike--helps))
    (when-let* ((dir (getenv "SPIKE_ARTIFACTS")))
      (when (fboundp 'x-export-frames)
        (with-temp-file (expand-file-name (format "title-map-%s.png" with-map) dir)
          (set-buffer-multibyte nil)
          (insert (x-export-frames nil 'png)))))))

(spike-run
 (lambda ()
   (spike-setup-frame)
   (spike-log "emacs %s window-system=%s frame-char-width=%d image-scaling-factor=%S auto-factor=%S"
              emacs-version window-system (frame-char-width) image-scaling-factor (image-compute-scaling-factor))
   (dolist (s '(1 2 nil)) (spike--posn s))
   (setq show-help-function #'spike--count-help)
   (dolist (n '(0 100 1000 10000)) (spike--map-cost n 200))
   (setq show-help-function #'tooltip-show-help tooltip-delay 0.1 tooltip-short-delay 0.1)
   (tooltip-mode 1)
   (advice-add 'x-show-tip :before (lambda (s &rest _) (push s spike--tips)))
   (advice-add 'tooltip-show-help :before (lambda (h) (when h (push h spike--helps))))
   (spike--title nil)
   (spike--title t)))

;;; hover.el ends here

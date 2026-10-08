;;; ns-frames.el --- spike: per-frame cost of live views in a GUI (NS) frame (eas-poo) -*- lexical-binding: t; -*-

;; Usage: scripts/eas-spikes/gui/run-ns.sh ns-frames.el OUT
;; The 0.2.4 workloads (ladder and depth push, clock and pacman tick,
;; airport-connections hover and parked pointer) as real frames: the
;; change, the readout line (`eas-mode-strip-string', an image line in
;; a GUI), `eas-mode-redraw', then `(redisplay t)' (librsvg and the NS
;; draw).  ms per frame, WARMUP frames dropped, then FRAMES timed.
;; Configs: shipped; render cache off; no image-flush (cache growth);
;; readout without its image.

(require 'eas)
(require 'eas-mode)
(require 'eas-template)
(require 'eas-render-cache)
(require 'eas-stream)
(require 'eas-play)
(require 'eas-mode-strip)

(defvar nsf-warmup 8)
(defvar nsf-frames 50)

(defun nsf--book (seed levels)
  (let ((state seed))
    (vconcat
     (cl-loop for side in '("ask" "bid")
              append (cl-loop for i below levels
                              do (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                              collect (list :side side :level i
                                            :price (if (equal side "ask") (+ 100.01 (* 0.01 i))
                                                     (- 99.99 (* 0.01 i)))
                                            :size (+ 1 (% (/ state 7) 20))))))))

(defun nsf--depth-rows (book)
  (let ((acc (list (cons "ask" 0) (cons "bid" 0))))
    (vconcat (mapcar (lambda (r)
                       (let ((cell (assoc (plist-get r :side) acc)))
                         (setcdr cell (+ (cdr cell) (plist-get r :size)))
                         (append r (list :depth (cdr cell)))))
                     book))))

(defconst nsf--ladder
  (list :width 400 :height 300
        :encoding (list :y (list :field "price" :type "ordinal" :sort "descending"
                                 :axis (list :format ".2f")))
        :layer (vector (list :mark (list :type "bar")
                             :encoding (list :x (list :field "size" :type "quantitative"
                                                      :scale (list :domain [0 40]))
                                             :color (list :field "side" :type "nominal")))
                       (list :mark (list :type "text" :align "left")
                             :encoding (list :x (list :field "size" :type "quantitative")
                                             :text (list :field "size" :type "quantitative"))))))

(defconst nsf--depth
  (list :width 400 :height 300
        :mark (list :type "area" :interpolate "step-after" :fillOpacity 0.4 :line t)
        :encoding (list :x (list :field "price" :type "quantitative" :scale (list :domain [99.7 100.3]))
                        :y (list :field "depth" :type "quantitative" :scale (list :domain [0 600]))
                        :color (list :field "side" :type "nominal"))))

(defvar nsf--clock 1.7e9)

(defun nsf--show (view)
  (eas-show view)
  (delete-other-windows (get-buffer-window (eas-view-buffer view)))
  (eas-view-resize view (eas-mode--window-size (get-buffer-window (eas-view-buffer view)) 'svg) 'svg)
  (eas-mode-redraw (eas-view-buffer view))
  (redisplay t)
  view)

(defun nsf--push-case (spec rows-fn)
  "Return (VIEW . STEP) for a 25-level book pushed 10 levels at a time."
  (let* ((book (nsf--book 1 25)) (seed 1)
         (view (nsf--show (eas-view-open (eas-json-encode spec) :id "book" :size '(400 . 300)
                                         :rows (funcall rows-fn book)))))
    (cons view
          (lambda (i)
            (let ((fresh (nsf--book (+ 2 i) 25)))
              (setq book (copy-sequence book))
              (dotimes (_ 10)
                (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
                (aset book (% seed 50) (aref fresh (% seed 50)))))
            (eas-dispatch view (list :type "push" :rows (funcall rows-fn book) :window 50))))))

(defvar nsf--clock-step 1.0)

(defun nsf--play-case (template)
  (setq nsf--clock 1.7e9)
  (let* ((view (nsf--show (eas-play-open template :bindings (eas-template-example template) :target 'svg))))
    (setq eas-play-clock (lambda () nsf--clock))
    (cons view (lambda (_) (cl-incf nsf--clock nsf--clock-step) (eas-play-tick view)))))

(defun nsf--hover-items (view)
  (seq-some (lambda (m) (and (member (plist-get m :mark) '("circle" "point" "symbol"))
                             (> (length (plist-get m :items)) 0) (plist-get m :items)))
            (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))

(defun nsf--hover-case (template &optional parked)
  (let* ((view (nsf--show (eas-view-open template :bindings (eas-template-example template)
                                         :target 'svg :id template)))
         (items (nsf--hover-items view)))
    (cons view
          (lambda (i)
            (if parked (eas-dispatch view (list :type "pointermove" :px (vector 3 3)))
              (let ((item (aref items (% (* 7 i) (length items)))))
                (eas-dispatch view (list :type "pointermove"
                                         :px (vector (plist-get item :x) (plist-get item :y))))))))))

(defvar nsf--flush t "Non-nil: eas-mode flushes replaced images (shipped).")
(advice-add 'eas-mode--flush-replaced :around
            (lambda (orig old new) (when nsf--flush (funcall orig old new))))
(defvar nsf--strip-image t "Nil: the readout line is plain text, not an SVG image.")
(advice-add 'eas-mode-strip--string :filter-args
            (lambda (args) (if nsf--strip-image args (list (nth 0 args) (nth 1 args) nil (nth 3 args)))))

(defun nsf--run-frames (case)
  "Time frames of CASE (VIEW . STEP); return stats plist per stage."
  (let* ((view (car case)) (step (cdr case)) (buf (eas-view-buffer view))
         stp strp red ras tot (distinct (make-hash-table :test 'equal)))
    (dotimes (k (+ nsf-warmup nsf-frames))
      (let ((t0 (spike-now-ms)))
        (funcall step k)
        (let ((t1 (spike-now-ms)))
          (eas-mode-strip-string view)
          (let ((t2 (spike-now-ms)))
            (eas-mode-redraw buf)
            (let ((t3 (spike-now-ms)))
              (redisplay t)
              (let ((t4 (spike-now-ms)))
                (when (>= k nsf-warmup)
                  (puthash (secure-hash 'md5 (format "%S" (plist-get (cdr (with-current-buffer buf (get-text-property (point-min) 'display))) :data)))
                           t distinct)
                  (push (- t1 t0) stp) (push (- t2 t1) strp) (push (- t3 t2) red)
                  (push (- t4 t3) ras) (push (- t4 t0) tot))))))))
    (list :distinct (hash-table-count distinct) :step (spike-stats stp) :strip (spike-stats strp) :redraw (spike-stats red)
          :raster (spike-stats ras) :total (spike-stats tot))))

(defconst nsf--cases
  `(("ladder push (25 levels)" . ,(lambda () (nsf--push-case nsf--ladder #'identity)))
    ("depth push (25 levels)" . ,(lambda () (nsf--push-case nsf--depth #'nsf--depth-rows)))
    ("clock tick (+60 s, hands move)" . ,(lambda () (setq nsf--clock-step 60.0) (nsf--play-case "clock")))
    ("clock tick (+1 s, as live)" . ,(lambda () (setq nsf--clock-step 1.0) (nsf--play-case "clock")))
    ("pacman tick" . ,(lambda () (nsf--play-case "pacman")))
    ("airport-connections hover" . ,(lambda () (nsf--hover-case "airport-connections")))
    ("airport-connections parked" . ,(lambda () (nsf--hover-case "airport-connections" t)))))

(defconst nsf--configs
  '(("shipped" t t t) ("render-cache-off" nil t t) ("no-image-flush" t nil t) ("readout-text-only" t t nil))
  "(NAME RENDER-CACHE FLUSH STRIP-IMAGE).")

;;; Extras: the readout image alone, and raster against image size.

(defun nsf--extras ()
  "Raster cost of the readout image alone, and of the clock at three frame sizes."
  (let* ((case (nsf--hover-case "airport-connections")) (view (car case)) (step (cdr case))
         (buf (get-buffer-create " *nsf-strip*")) xs ys)
    (switch-to-buffer buf)
    (delete-other-windows)
    (dotimes (k (+ nsf-warmup nsf-frames))
      (funcall step k)
      (let* ((svg (eas-readout-svg view (plist-get (plist-get (eas-view-scene view) :size) :w)
                                   (default-line-height) "#666666"))
             (image (create-image svg 'svg t)))
        (erase-buffer) (insert-image image "[strip]")
        (let ((t0 (spike-now-ms)))
          (redisplay t)
          (when (>= k nsf-warmup) (push (- (spike-now-ms) t0) xs)))
        (image-flush image t)))
    (spike-log "%-44s %s" "readout image alone, 1 distinct svg per frame" (spike-fmt (spike-stats xs)))
    (kill-buffer buf)
    (ignore-errors (eas-view-close view))
    (dolist (sz '((500 . 350) (800 . 560) (1000 . 700)))
      (set-frame-size nil (car sz) (cdr sz) t)
      (let* ((case (progn (setq nsf--clock-step 60.0) (nsf--play-case "clock"))) (b (eas-view-buffer (car case))))
        (setq ys nil)
        (dotimes (k (+ nsf-warmup nsf-frames))
          (funcall (cdr case) k) (eas-mode-redraw b)
          (let ((t0 (spike-now-ms)))
            (redisplay t)
            (when (>= k nsf-warmup) (push (- (spike-now-ms) t0) ys))))
        (spike-log "%-44s %s (image %S px)" (format "clock raster, frame %dx%d" (car sz) (cdr sz))
                   (spike-fmt (spike-stats ys))
                   (with-current-buffer b (image-size (get-text-property (point-min) 'display) t)))
        (ignore-errors (kill-buffer b)) (ignore-errors (eas-view-close (car case)))
        (clear-image-cache)))))

(defun nsf--main ()
  (setq debug-on-error t
        debugger (lambda (&rest _) (spike-log "%s" (with-output-to-string (backtrace))) (kill-emacs 1)))
  (spike-setup-frame 1000 700)
  (spike-log "emacs %s frame-scale-factor=%s frame=%dx%d compiled=%s load-avg=%S warmup=%d frames=%d"
             emacs-version (frame-scale-factor) (frame-pixel-width) (frame-pixel-height)
             (compiled-function-p (symbol-function 'eas-dispatch)) (load-average t) nsf-warmup nsf-frames)
  (spike-log "%-28s %-18s %-7s %s" "workload" "config" "stage" "ms")
  (if (getenv "NSF_EXTRAS") (nsf--extras)
  (dolist (cfg nsf--configs)
    (dolist (c nsf--cases)
      (let ((eas-render-cache-enabled (nth 1 cfg)) (nsf--flush (nth 2 cfg)) (nsf--strip-image (nth 3 cfg)))
        (eas-render-cache-clear) (clear-image-cache) (garbage-collect)
        (let* ((case (handler-bind ((error (lambda (e) (spike-log "ERR %S\n%s" e (with-output-to-string (backtrace))))))
                       (funcall (cdr c)))) (buf (eas-view-buffer (car case)))
               (cache0 (image-cache-size)) (rss0 (spike-rss-kb))
               (m (nsf--run-frames case)))
          (spike-log "%-28s %-18s distinct chart SVGs: %d of %d" (car c) (car cfg) (plist-get m :distinct) nsf-frames)
          (dolist (p '(:step :strip :redraw :raster :total))
            (spike-log "%-28s %-18s %-7s %s" (car c) (car cfg) p (spike-fmt (plist-get m p))))
          (spike-log "%-28s %-18s cache   image-cache-size %d -> %d bytes (%+d), RSS %+d KB"
                     (car c) (car cfg) cache0 (image-cache-size) (- (image-cache-size) cache0)
                     (- (spike-rss-kb) rss0))
          (ignore-errors (eas-play-detach (car case)))
          (ignore-errors (kill-buffer buf))
          (ignore-errors (eas-view-close (car case)))
          (clear-image-cache)))))))

(spike-run #'nsf--main)

;;; ns-frames.el ends here

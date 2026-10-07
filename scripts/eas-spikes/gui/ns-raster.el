;;; ns-raster.el --- spike: SVG re-raster and image cache on the NS build (fc-qx1.24) -*- lexical-binding: t; -*-

;; Usage: scripts/eas-spikes/gui/run-ns.sh ns-raster.el OUT
;; Real eas views in a real NS frame.  Per frame, ms for
;;   compile    eas-view-resize / eas-push / pointermove (scene)
;;   redraw     eas-mode-redraw: SVG string + create-image + insert
;;   raster     (redisplay t): librsvg + NS draw
;; Variants: image-flush of the replaced image on (shipped) and off, and
;; eas-render-cache on and off.  "hit" is a forced redisplay of an image
;; already in the Emacs image cache.

(require 'eas)
(require 'eas-mode)
(require 'eas-template)
(require 'eas-render-cache)
(require 'eas-stream)

(defun nsr--book (seed levels)
  "A LEVELS-per-side order book as keyed rows (id = side+level)."
  (let ((state seed))
    (vconcat
     (cl-loop for side in '("ask" "bid")
              append (cl-loop for i below levels
                              do (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                              collect (list :id (format "%s%d" side i) :side side :level i
                                            :price (if (equal side "ask") (+ 100.01 (* 0.01 i))
                                                     (- 99.99 (* 0.01 i)))
                                            :size (+ 1 (% (/ state 7) 20))))))))

(defconst nsr--ladder
  (list :width 400 :height 300
        :encoding (list :y (list :field "price" :type "ordinal" :sort "descending"
                                 :axis (list :format ".2f")))
        :layer (vector (list :mark (list :type "bar")
                             :encoding (list :x (list :field "size" :type "quantitative"
                                                      :scale (list :domain [0 40]))
                                             :color (list :field "side" :type "nominal")))
                       (list :mark (list :type "text" :align "left")
                             :encoding (list :x (list :field "size" :type "quantitative")
                                             :text (list :field "size" :type "quantitative")))))
  "A 2x25-level ladder, as in eas-live-test.")

(defvar nsr--flush 'shipped
  "`shipped': eas-mode as is.  `always': flush the replaced image even when the
new one repeats its data (the code before fc-qx1.24).  nil: never flush.")
(defvar nsr--distinct nil "Hash of the SVG strings drawn in this run.")

(defun nsr--always-flush (old _new)
  (when (and (eq (car-safe old) 'image)) (image-flush old t)))
(defun nsr--flush-advice (orig &rest args) (when nsr--flush (apply orig args)))
(advice-add 'eas-mode--flush-replaced :around
            (lambda (orig old new)
              (pcase nsr--flush
                ('always (nsr--always-flush old new))
                ('shipped (funcall orig old new)))))

(defun nsr--open (name)
  "Open and show view NAME; return it.  The window is the whole frame."
  (let* ((view (pcase name
                 ("order-book" (eas-view-open (eas-json-encode nsr--ladder) :id "book" :size '(400 . 300)
                                              :rows (nsr--book 1 25)))
                 (_ (eas-view-open name :bindings (eas-template-example name) :id name)))))
    (eas-show view)
    ;; pop-to-buffer may have split the frame: give the chart all of it.
    (delete-other-windows (get-buffer-window (eas-view-buffer view)))
    (eas-view-resize view (nsr--size view) 'svg)
    (eas-mode-redraw (eas-view-buffer view))
    (redisplay t)
    view))

(defun nsr--size (view)
  (eas-mode--window-size (get-buffer-window (eas-view-buffer view)) 'svg))

(defun nsr--measure (view action n)
  "Run ACTION (a function of K and VIEW) N times; return (compile redraw raster total) stats."
  (let ((buffer (eas-view-buffer view)) cs rs ds ts)
    (dotimes (k n)
      (let ((t0 (spike-now-ms)))
        (funcall action k view)
        (let ((t1 (spike-now-ms)))
          (eas-mode-redraw buffer)
          (puthash (plist-get (cdr (with-current-buffer buffer (get-text-property (point-min) 'display))) :data) t nsr--distinct)
          (let ((t2 (spike-now-ms)))
            (redisplay t)
            (let ((t3 (spike-now-ms)))
              (push (- t1 t0) cs) (push (- t2 t1) rs) (push (- t3 t2) ds) (push (- t3 t0) ts))))))
    (list :compile (spike-stats cs) :redraw (spike-stats rs) :raster (spike-stats ds) :total (spike-stats ts))))

(defun nsr--report (label m)
  (dolist (p '(:compile :redraw :raster :total))
    (spike-log "%-34s %-8s %s" label p (spike-fmt (plist-get m p)))))

(defvar nsr--sizes '((1000 . 700) (900 . 640) (1100 . 720) (800 . 600) (1000 . 700) (950 . 680)))

(defun nsr--resize-action (k view)
  "Resize the frame, then recompile VIEW at its window's new size."
  (let ((s (let ((b (nth (% k (length nsr--sizes)) nsr--sizes))) (cons (+ (car b) (* 3 k)) (cdr b)))))
    (set-frame-size nil (car s) (cdr s) t)
    (eas-view-resize view (nsr--size view) 'svg)))

(defun nsr--hover-action (k view)
  "Pointer over the k-th of the first mark's items (as eas-render-cache-test does)."
  (let* ((items (seq-some (lambda (m) (and (member (plist-get m :mark) '("circle" "point" "symbol"))
                                           (> (length (plist-get m :items)) 0) (plist-get m :items)))
                          (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))
         (item (aref items (% (* 7 k) (length items)))))
    (eas-dispatch view (list :type "pointermove" :px (vector (plist-get item :x) (plist-get item :y))))))

(defun nsr--still-action (_k view)
  "Pointer parked on a blank corner: the same frame again."
  (eas-dispatch view (list :type "pointermove" :px (vector 3 3))))

(defun nsr--push-action (k view)
  (eas-push view (nsr--book (+ 2 k) 25) :key "id"))

(defun nsr--cache-state (label)
  (spike-log "%-34s image-cache-size=%d bytes RSS=%d KB" label (image-cache-size) (spike-rss-kb)))

(defun nsr--hit (view n)
  "Forced redisplay of the image already shown: no raster."
  (let (xs)
    (dotimes (_ n)
      (let ((t0 (spike-now-ms)))
        (force-window-update (get-buffer-window (eas-view-buffer view)))
        (redisplay t)
        (push (- (spike-now-ms) t0) xs)))
    (spike-stats xs)))

(defun nsr--variant (name action n)
  "Measure view NAME under ACTION with the flush variants and the render cache on/off."
  (dolist (cfg '((shipped t "shipped") (always t "always-flush(old)") (nil t "no-flush")
                 (shipped nil "shipped,no-rcache")))
    (let ((nsr--flush (nth 0 cfg)) (eas-render-cache-enabled (nth 1 cfg))
          (nsr--distinct (make-hash-table :test 'equal))
          (view (nsr--open name))
          (label (cdr (assq action '((nsr--resize-action . resize) (nsr--hover-action . hover)
                                     (nsr--still-action . still) (nsr--push-action . push))))))
      (eas-render-cache-clear)
      (clear-image-cache) (garbage-collect)
      (let ((rss0 (spike-rss-kb)))
        (let ((nsr--distinct (make-hash-table :test 'equal))) (nsr--measure view action 2))
        (clrhash nsr--distinct)
        (let ((m (nsr--measure view action n)))
          (nsr--report (format "%s %s %s" name (nth 2 cfg) label) m)
          (spike-log "%-34s %d distinct SVGs of %d frames; image-cache-size=%d bytes, RSS %+d KB"
                     (format "  %s %s" name (nth 2 cfg)) (hash-table-count nsr--distinct) n (image-cache-size) (- (spike-rss-kb) rss0))))
      (let ((buf (eas-view-buffer view)))
        (eas-view-close view)
        (kill-buffer buf))
      (clear-image-cache))))

(defun nsr--main ()
  (spike-setup-frame 1000 700)
  (advice-add 'image-flush :around #'nsr--flush-advice)
  (spike-log "emacs %s window-system=%s frame-scale-factor=%s image-scaling-factor=%S rsvg=%s frame=%dx%d compiled=%s load-avg=%S"
             emacs-version window-system (frame-scale-factor) image-scaling-factor (image-type-available-p 'svg)
             (frame-pixel-width) (frame-pixel-height) (compiled-function-p (symbol-function 'eas-dispatch))
             (load-average t))
  ;; Cache-hit redisplay and raster-size per view, once.
  (dolist (name '("line" "treemap" "airport-connections" "order-book"))
    (let* ((view (nsr--open name))
           (img (get-text-property (point-min) 'display (eas-view-buffer view))))
      (spike-log "%-20s image-size=%S (pixels) scene-size=%S hit-redisplay %s"
                 name (with-current-buffer (eas-view-buffer view) (image-size (get-text-property (point-min) 'display) t))
                 (plist-get (eas-view-scene view) :size)
                 (spike-fmt (nsr--hit view 20)))
      (ignore img)
      (kill-buffer (eas-view-buffer view))))
  (dolist (name '("line" "treemap" "airport-connections"))
    (nsr--variant name #'nsr--resize-action 12))
  (dolist (name '("airport-connections" "scatter-plot"))
    (nsr--variant name #'nsr--hover-action 15))
  (nsr--variant "airport-connections" #'nsr--still-action 15)
  (nsr--variant "order-book" #'nsr--push-action 20)
  (nsr--variant "order-book" #'nsr--resize-action 12))

(spike-run #'nsr--main)

;;; ns-raster.el ends here

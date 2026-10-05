;;; raster.el --- spike: librsvg re-raster per pointer move, image cache -*- lexical-binding: t; -*-

;; Usage: scripts/eas-spikes/gui/run.sh raster.el OUT
;; In a GUI frame (X, librsvg), ms per step:
;;   synthetic  N bars + crosshair (800x400); each step moves the
;;              crosshair: serialize, create-image, swap the display
;;              property, (redisplay t).  "hit" is a forced redisplay of
;;              an image already in the cache (no rasterization).
;;   engine     real eas views (default GC threshold, then 64 MB): eas-dispatch pointermove, then
;;              eas-mode-redraw (SVG + :map + insert), then (redisplay t).
;;   cache      image-cache-size and RSS over 300 distinct images, then
;;              after `clear-image-cache'; and the same run calling
;;              `image-flush' on each replaced image.

(require 'eas)
(require 'eas-mode)

(defun spike--raster-synthetic (n reps)
  (let (ser disp hit (img nil))
    (spike-show (create-image (spike-bars-svg n 400) 'svg t :scale 1))
    (dotimes (k reps)
      (let* ((x (+ 50 (* 7 k)))
             (t0 (spike-now-ms))
             (svg (spike-bars-svg n x))
             (t1 (spike-now-ms)))
        (setq img (create-image svg 'svg t :scale 1))
        (spike-swap img)
        (redisplay t)
        (let ((t2 (spike-now-ms)))
          (push (- t1 t0) ser) (push (- t2 t1) disp)
          (force-window-update)
          (redisplay t)
          (push (- (spike-now-ms) t2) hit))))
    (spike-log "synthetic n=%-5d bytes=%-7d serialize %s" n (length (spike-bars-svg n 1)) (spike-fmt (spike-stats ser)))
    (spike-log "synthetic n=%-5d create+swap+redisplay(raster) %s" n (spike-fmt (spike-stats disp)))
    (spike-log "synthetic n=%-5d redisplay cache-hit %s" n (spike-fmt (spike-stats hit)))
    (clear-image-cache)))

(defun spike--line-spec (n filter)
  (let ((values (vconcat (mapcar (lambda (i) (list :t i :p (+ 100 (* 10 (sin (/ i 37.0))))))
                                 (number-sequence 0 (1- n)))))
        (hover '(:name "hover" :select (:type "point" :on "pointermove" :nearest t :encodings ["x"]))))
    (if filter
        (list :data (list :values values)
              :layer (vector (list :params (vector hover) :mark "line"
                                   :encoding '(:x (:field "t" :type "quantitative")
                                               :y (:field "p" :type "quantitative" :scale (:zero :false))))
                             (list :transform [(:filter (:param "hover" :empty :false))]
                                   :mark "rule" :encoding '(:x (:field "t" :type "quantitative")))))
      (list :data (list :values values)
            :layer (vector (list :mark "line" :encoding '(:x (:field "t" :type "quantitative")
                                                          :y (:field "p" :type "quantitative" :scale (:zero :false))))
                           (list :params (vector hover) :mark "rule"
                                 :encoding '(:x (:field "t" :type "quantitative")
                                             :opacity (:condition (:param "hover" :empty :false :value 1) :value 0))))))))

(defun spike--point-spec (n)
  "N points with tooltips and a hover highlight: discrete marks, one :map area each."
  (list :data (list :values (vconcat (mapcar (lambda (i) (list :a i :b (% (* i 37) 101))) (number-sequence 0 (1- n)))))
        :params [(:name "hover" :select (:type "point" :on "pointermove" :nearest t))]
        :mark "point"
        :encoding '(:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")
                    :tooltip [(:field "a" :type "quantitative") (:field "b" :type "quantitative")]
                    :color (:condition (:param "hover" :empty :false :value "darkorange") :value "steelblue"))))

(defun spike--raster-engine (label spec reps)
  (let* ((eas-views (make-hash-table :test 'equal))
         (view (eas-view-open spec :id "spike" :size '(800 . 400) :target 'svg))
         (buffer (get-buffer-create "*eas spike*"))
         dispatch redraw display total areas (gc0 gc-elapsed) (gcs0 gcs-done))
    (switch-to-buffer buffer)
    (eas-view-mode)
    (setq eas-mode--view view)
    (setf (eas-view-buffer view) buffer)
    (eas-mode-redraw buffer)
    (redisplay t)
    (dotimes (k reps)
      (let ((t0 (spike-now-ms)))
        (eas-dispatch view (list :type "pointermove" :px (vector (+ 60 (* 37 k)) 200)))
        (let ((t1 (spike-now-ms)))
          (eas-mode-redraw buffer)
          (let ((t2 (spike-now-ms)))
            (redisplay t)
            (let ((t3 (spike-now-ms)))
              (push (- t1 t0) dispatch) (push (- t2 t1) redraw) (push (- t3 t2) display) (push (- t3 t0) total))))))
    (setq areas (length (plist-get (cdr (get-text-property (point-min) 'display)) :map)))
    (when (timerp eas-mode--timer) (cancel-timer eas-mode--timer))
    (spike-log "engine %-14s areas=%-5d dispatch %s" label areas (spike-fmt (spike-stats dispatch)))
    (spike-log "engine %-14s areas=%-5d redraw(svg+map+insert) %s" label areas (spike-fmt (spike-stats redraw)))
    (spike-log "engine %-14s areas=%-5d redisplay(raster) %s" label areas (spike-fmt (spike-stats display)))
    (spike-log "engine %-14s areas=%-5d TOTAL per move %s" label areas (spike-fmt (spike-stats total)))
    (spike-log "engine %-14s GC: %d collections, %.1f ms per move" label (- gcs-done gcs0)
               (/ (* 1000 (- gc-elapsed gc0)) reps))
    (kill-buffer buffer)
    (clear-image-cache)))

(defun spike--raster-parts ()
  "Rasterization alone for SVG parts at 800x400: what librsvg costs per element kind."
  (let* ((head "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"800\" height=\"400\" font-family=\"sans-serif\">")
         (labels (mapconcat (lambda (i) (format "<text x=\"%d\" y=\"390\" font-size=\"10\">%d</text>" (* i 26) (* i 100)))
                            (number-sequence 0 29) ""))
         (path (concat "<path fill=\"none\" stroke=\"#4c78a8\" d=\"M0,200"
                       (mapconcat (lambda (i) (format "L%d,%.1f" i (+ 200 (* 100 (sin (/ i 20.0)))))) (number-sequence 1 799) "")
                       "\"/>"))
         (rects (lambda (n) (mapconcat (lambda (i) (format "<rect x=\"%.1f\" y=\"100\" width=\"%.1f\" height=\"200\" fill=\"#4c78a8\"/>"
                                                           (* i (/ 780.0 n)) (max 0.5 (* 0.8 (/ 780.0 n)))))
                                       (number-sequence 0 (1- n)) ""))))
    (spike-show (create-image (concat head "</svg>") 'svg t :scale 1))
    (dolist (part `(("empty" . "") ("30 text labels" . ,labels) ("1 path, 800 vertices" . ,path)
                    ("100 rects" . ,(funcall rects 100)) ("1000 rects" . ,(funcall rects 1000))
                    ("10000 rects" . ,(funcall rects 10000))))
      (let (xs)
        (dotimes (k 10)
          (let ((img (create-image (concat head (cdr part) (format "<!-- %d --></svg>" k)) 'svg t :scale 1))
                (t0 (spike-now-ms)))
            (spike-swap img)
            (redisplay t)
            (push (- (spike-now-ms) t0) xs)
            (image-flush img)))
        (spike-log "raster-only %-22s %s" (car part) (spike-fmt (spike-stats xs)))))))

(defun spike--cache (n steps flush)
  (clear-image-cache) (garbage-collect)
  (let ((rss0 (spike-rss-kb)) prev)
    (spike-show (create-image (spike-bars-svg n 0) 'svg t :scale 1))
    (dotimes (k steps)
      (let ((img (create-image (spike-bars-svg n (+ 10 (* 2 k))) 'svg t :scale 1)))
        (spike-swap img)
        (redisplay t)
        (when (and flush prev) (image-flush prev))
        (setq prev img))
      (when (zerop (% (1+ k) 100))
        (spike-log "cache n=%d flush=%s after %3d images: image-cache-size=%d bytes, RSS +%d KB"
                   n flush (1+ k) (image-cache-size) (- (spike-rss-kb) rss0))))
    (clear-image-cache) (garbage-collect)
    (spike-log "cache n=%d flush=%s after clear-image-cache: image-cache-size=%d bytes, RSS +%d KB"
               n flush (image-cache-size) (- (spike-rss-kb) rss0))))

(spike-run
 (lambda ()
   (spike-setup-frame)
   (spike-log "emacs %s window-system=%s toolkit=lucid cairo=%s rsvg=%s image-scaling-factor=%S frame-char-width=%d eas-compiled=%s"
              emacs-version window-system (and (memq 'cairo features) t) (image-type-available-p 'svg)
              image-scaling-factor (frame-char-width) (compiled-function-p (symbol-function 'eas-dispatch)))
   (spike--raster-parts)
   (dolist (n '(100 1000 10000))
     (spike--raster-synthetic n (if (= n 10000) 10 30)))
   (dolist (n '(1000 10000))
     (spike--raster-engine (format "line-cond %d" n) (spike--line-spec n nil) 15)
     (spike--raster-engine (format "line-filter %d" n) (spike--line-spec n t) 15))
   (dolist (n '(100 1000))
     (spike--raster-engine (format "points %d" n) (spike--point-spec n) 15))
   ;; The same moves with a 64 MB GC threshold (as compile binds, and
   ;; as gcmh-style configs set): how much of a move is collection.
   (let ((gc-cons-threshold (* 64 1024 1024)))
     (spike--raster-engine "gc64 line-f 1k" (spike--line-spec 1000 t) 15)
     (spike--raster-engine "gc64 line-f 10k" (spike--line-spec 10000 t) 15)
     (spike--raster-engine "gc64 points 100" (spike--point-spec 100) 15))
   (spike--cache 1000 300 nil)
   (spike--cache 1000 300 t)))

;;; raster.el ends here

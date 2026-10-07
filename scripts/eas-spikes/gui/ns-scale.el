;;; ns-scale.el --- spike: pointer coordinates under the NS backing scale (fc-qx1.24) -*- lexical-binding: t; -*-

;; Usage: scripts/eas-spikes/gui/run-ns.sh ns-scale.el OUT
;; Retina (backing scale factor 2).  No real pointer is moved: events
;; come from `posn-at-x-y' on the live window, which is the same
;; geometry the NS port reports for a real click (frame pixels are
;; points).  For each image :scale S we put the scene's item (x y) at
;; the display pixel S*(x y), ask Emacs for the posn there, hand the
;; event to `eas-mode-event-px' and compare it with (x y).

(require 'eas)
(require 'eas-mode)
(require 'eas-template)

(defun nss--items (view n)
  "N point-like items (plists with :x :y) of VIEW's first point-like mark."
  (let* ((items (seq-some (lambda (m) (and (member (plist-get m :mark) '("circle" "point" "symbol" "rect"))
                                           (> (length (plist-get m :items)) 4)
                                           (plist-get m :items)))
                          (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks))))
    (cl-loop for i from 0 below n collect (aref items (% (* 5 i) (length items))))))

(defun nss--show (view scale)
  "Draw VIEW's scene as an image with :scale SCALE (nil: create-image's own default)."
  (with-current-buffer (eas-view-buffer view)
    (let ((inhibit-read-only t)
          (image (if scale
                     (eas-svg-image (eas-view-scene view) :scale scale)
                   ;; What the code did before fc-qx1.23: no :scale given.
                   (create-image (eas-svg-render (eas-view-scene view)) 'svg t))))
      (erase-buffer)
      (insert-image image "[chart]")
      (goto-char (point-min))
      (redisplay t)
      image)))

(defun nss--origin (buffer window)
  "Window-relative pixel of the image's top-left, as `posn-at-x-y' counts it.
`posn-at-point' leaves out the header line, `posn-at-x-y' counts it."
  (let ((xy (with-current-buffer buffer (posn-x-y (posn-at-point (point-min) window)))))
    (cons (car xy) (+ (cdr xy) (window-header-line-height window)))))

(defun nss--probe (view scale label)
  (let* ((image (nss--show view scale))
         (buffer (eas-view-buffer view))
         (window (get-buffer-window buffer))
         (origin (nss--origin buffer window))
         (s (let ((p (plist-get (cdr image) :scale))) (if (numberp p) p nil)))
         (px (image-size image t))
         (maxerr 0.0) (rows nil))
    (spike-log "-- %s: image :scale=%S image-size(px)=%S origin=%S frame-scale-factor=%s"
               label (plist-get (cdr image) :scale) px origin (frame-scale-factor))
    (dolist (item (nss--items view 8))
      (let* ((ix (plist-get item :x)) (iy (plist-get item :y)) (k (or s 1))
             (x (round (+ (car origin) (* k ix)))) (y (round (+ (cdr origin) (* k iy))))
             (posn (posn-at-x-y x y window))
             (ev (list 'mouse-movement posn))
             (oxy (posn-object-x-y posn))
             (res (condition-case err (eas-mode-event-px ev) (error (list 'error err)))))
        (if (vectorp res)
            (let ((e (max (abs (- (aref res 0) ix)) (abs (- (aref res 1) iy)))))
              (setq maxerr (max maxerr e))
              (push (format "item (%.1f %.1f) -> display (%d %d) -> object-x-y %S -> scene (%.1f %.1f) err %.2f" ix iy x y oxy (aref res 0) (aref res 1) e) rows))
          (push (format "item (%.1f %.1f) -> display (%d %d) -> object-x-y %S -> %S" ix iy x y oxy res) rows))))
    (dolist (r (nreverse rows)) (spike-log "   %s" r))
    (spike-log "   max |error| in scene px: %.2f   (a datum is hit when error is below the mark radius)" maxerr)))

(defun nss--hover-end-to-end (view)
  "Real dispatch: pointermove at the posn of a point item must hover that datum."
  (let* ((image (nss--show view 1))
         (buffer (eas-view-buffer view)) (window (get-buffer-window buffer))
         (origin (nss--origin buffer window))
         (hits 0) (n 0))
    (ignore image)
    (dolist (item (nss--items view 8))
      (let* ((x (round (+ (car origin) (plist-get item :x)))) (y (round (+ (cdr origin) (plist-get item :y))))
             (px (eas-mode-event-px (list 'mouse-movement (posn-at-x-y x y window)))))
        (eas-dispatch view (list :type "pointermove" :px px))
        (cl-incf n)
        (when (plist-get (eas-view-state view) :hover) (cl-incf hits))))
    (spike-log "end-to-end pointermove at 8 item positions (:scale 1): %d of %d produced a hover" hits n)))

(defun nss--area-probe (view scale)
  "Whether a posn over a :map area's centre reports that area (C hit test in points).
Areas that another area covers completely are not expected to match."
  (let* ((image (progn (nss--show view scale)
                              (sit-for 0.1) (let ((i (with-current-buffer (eas-view-buffer view) (get-text-property (point-min) 'display)))) i)))
         (buffer (eas-view-buffer view)) (window (get-buffer-window buffer))
         (origin (nss--origin buffer window))
         (map (plist-get (cdr image) :map)) (same 0) (n 0) (other nil))
    (dolist (area (seq-take map 40))
      (let* ((shape (car area))
             (c (pcase (car shape)
                  ('rect (cons (/ (+ (car (cadr shape)) (car (cddr shape))) 2) (/ (+ (cdr (cadr shape)) (cdr (cddr shape))) 2)))
                  ('circle (cadr shape)) (_ nil))))
        (when (and c (<= 0 (cdr c) (- (window-body-height window t) 20)) (<= 0 (car c) (- (window-body-width window t) 2)))
          (let* ((posn (posn-at-x-y (round (+ (car origin) (car c))) (round (+ (cdr origin) (cdr c))) window))
                 (id (nth 1 posn)))
            (cl-incf n)
            (if (eq id (nth 1 area)) (cl-incf same) (when (< (length other) 3) (push (list (nth 1 area) '-> id) other)))))))
    (spike-log "map (:scale %s, %d areas): %d of the first %d area centres report their own id; others e.g. %S"
               scale (length map) same n (nreverse other))))

(defun nss--main ()
  (spike-setup-frame 1000 700)
  (spike-log "emacs %s window-system=%s frame-scale-factor=%s image-scaling-factor=%S display-pixel-width=%d frame-pixel=%dx%d char=%dx%d"
             emacs-version window-system (frame-scale-factor) image-scaling-factor (display-pixel-width)
             (frame-pixel-width) (frame-pixel-height) (frame-char-width) (frame-char-height))
  (dolist (name '("airport-connections" "scatter-plot"))
    (let* ((view (eas-view-open name :bindings (eas-template-example name) :id name)))
      (eas-show view)
      (delete-other-windows (get-buffer-window (eas-view-buffer view)))
      (eas-view-resize view (eas-mode--window-size (get-buffer-window (eas-view-buffer view)) 'svg) 'svg)
      (spike-log "== %s scene %S window-body %S" name (plist-get (eas-view-scene view) :size)
                 (cons (window-body-width nil t) (window-body-height nil t)))
      (nss--probe view nil "unpinned create-image (pre-fc-qx1.23 behaviour)")
      (dolist (s '(1 0.5))
        (nss--probe view s (format "eas-svg-image :scale %s" s)))
      (nss--hover-end-to-end view)
      (nss--area-probe view 1)
      ;; :scale 2 doubles the image: compile at half the window so it fits.
      (eas-view-resize view '(450 . 290) 'svg)
      (nss--probe view 2 "eas-svg-image :scale 2, scene compiled at 450x290")
      (nss--area-probe view 2)
      (eas-view-close view)
      (kill-buffer (eas-view-buffer view)))))

(spike-run #'nss--main)

;;; ns-scale.el ends here

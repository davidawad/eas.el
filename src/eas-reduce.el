;;; eas-reduce.el --- pure reducers: (state event scene) -> state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  `eas-reduce' is the whole of interaction: it reads the
;; current scene (scales, hit index, params) and returns the next view
;; state.  It never touches buffers, so every interaction runs in
;; --batch and replays from a log.  State:
;;
;;   :domains  (VIEW-KEY (:x [LO HI] :y [LO HI]))  zoom/pan (bind "scales")
;;   :params   (NAME STORE)                        selection stores
;;   :hover    hit plist or nil                     readout and crosshair
;;   :pointer  last pointermove px or nil           the values strip's column
;;   :drag     in-progress pointer drag
;;   :history / :future                             zoom history stack
;;   :wheel    (VIEW PX DOMAINS) of the last wheel  one history entry per gesture
;;   :stream-cursor                                 rows received by push

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-hit)
(require 'eas-params)
(require 'eas-params-init)
(require 'eas-zoom)
(require 'eas-intersect)
(require 'eas-link)

(defconst eas-reduce-click-slop 3 "Pixels a press may move and still be a click.")
(defconst eas-reduce-hover-radius 30 "Pixels beyond which hover finds nothing.")
(defconst eas-reduce-zoom-step 1.25 "Domain scale factor per +/- key.")
(defconst eas-reduce-wheel-step eas-zoom-wheel-step "Domain scale factor per wheel step.")
(defconst eas-reduce-history-limit 50 "Zoom history entries kept.")

(defun eas-reduce--view (scene id)
  "SCENE view with ID."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get scene :views)))

(defun eas-reduce--view-at (scene px)
  "SCENE view under PX, or nil."
  (let ((x (aref px 0)) (y (aref px 1)))
    (seq-find (lambda (v) (eas-hit--contains (plist-get v :bounds) x y)) (plist-get scene :views))))

(defun eas-reduce--params (scene view-id pred)
  "Selection params of SCENE's VIEW-ID satisfying PRED."
  (seq-filter (lambda (p) (and (equal (plist-get p :view) view-id) (funcall pred p)))
              (eas-params-of scene)))

(defun eas-reduce--scales-param-p (p) "Non-nil for bind-scales P." (equal (plist-get p :bind) "scales"))
(defun eas-reduce--brush-p (p)
  "Non-nil for an interval P not bound to scales."
  (and (equal (plist-get (plist-get p :def) :type) "interval") (not (eas-reduce--scales-param-p p))))
(defun eas-reduce--on-p (p on)
  "Non-nil when point P triggers on event name ON."
  (and (equal (plist-get (plist-get p :def) :type) "point")
       (not (equal (plist-get p :bind) "legend"))
       (member (plist-get (plist-get p :def) :on)
               (if (equal on "pointermove") '("pointermove" "pointerover" "mouseover" "mousemove") (list on)))))

(defun eas-reduce--put (state key value)
  "STATE with KEY set to VALUE."
  (eas-plist-put state key value))

(defun eas-reduce--store (state name store)
  "STATE with selection NAME holding STORE (nil empties it)."
  (eas-reduce--put state :params (eas-plist-put (plist-get state :params) (eas-key name) store)))

(defun eas-reduce--scale (scene view-id channel)
  "Continuous scale of CHANNEL in SCENE's VIEW-ID, or nil."
  (let ((s (plist-get (plist-get (eas-reduce--view scene view-id) :scales) channel)))
    (and (member (plist-get s :type) '("linear" "log" "time" "utc")) s)))

(defun eas-reduce--channels (param)
  "Positional channels (keywords) PARAM's encodings cover."
  (mapcar #'eas-key (or (plist-get (plist-get param :def) :encodings) ["x" "y"])))

(defun eas-reduce--set-domain (state view-id channel domain)
  "STATE with VIEW-ID's CHANNEL domain set to DOMAIN."
  (let* ((key (eas-key view-id)) (domains (plist-get state :domains)))
    (eas-reduce--put state :domains
                       (eas-plist-put domains key (eas-plist-put (plist-get domains key) channel domain)))))

(defun eas-reduce--remember (state &optional domains)
  "STATE with DOMAINS (default: its current domains) pushed on the history.
An unzoomed state is recorded as `:none'."
  (let ((history (cons (or (if domains (car domains) (plist-get state :domains)) :none)
                       (plist-get state :history))))
    (eas-reduce--put (eas-reduce--put state :history (seq-take history eas-reduce-history-limit))
                       :future nil)))

(defun eas-reduce--target-views (scene state)
  "Return the SCENE views that keyboard zoom and pan act on.
The view hovered in STATE when it is bound to scales, else every bound
view."
  (let* ((bound (delete-dups (mapcar (lambda (p) (plist-get p :view))
                                     (seq-filter #'eas-reduce--scales-param-p (eas-params-of scene)))))
         (hovered (plist-get (plist-get state :hover) :view)))
    (if (member hovered bound) (list hovered) bound)))

(defun eas-reduce--zoom (state scene view-id factor &optional px)
  "Scale VIEW-ID's bound domains by FACTOR around PX (default: the centre).
Return the updated STATE; SCENE supplies the scales."
  (let ((params (eas-reduce--params scene view-id #'eas-reduce--scales-param-p)))
    (dolist (channel (delete-dups (apply #'append (mapcar #'eas-reduce--channels params))))
      (when-let* ((scale (eas-reduce--scale scene view-id channel))
                  (domain (eas-zoom-domain scale factor (and px (aref px (if (eq channel :x) 0 1))))))
        (setq state (eas-reduce--set-domain state view-id channel domain))))
    state))

(defun eas-reduce--pan (state scene view-id channel fraction)
  "Shift VIEW-ID's CHANNEL domain by FRACTION of its range.
Return the updated STATE; SCENE supplies the scale."
  (if-let* ((scale (eas-reduce--scale scene view-id channel))
            (domain (eas-zoom-step-domain scale fraction)))
      (eas-reduce--set-domain state view-id channel domain)
    state))

(defun eas-reduce--if-moved (state next)
  "Return NEXT, or STATE when the domains in NEXT are unchanged.
Keeps no-op zooms (unbound views, saturated domains) out of the history."
  (if (equal (plist-get state :domains) (plist-get next :domains)) state next))

(defun eas-reduce--wheel (state scene px delta)
  "Zoom the SCENE view under PX by DELTA wheel ticks around PX.
Return the updated STATE.  Consecutive wheel events at one pointer
position are one gesture and one history entry."
  (if-let* ((view (eas-reduce--view-at scene px)))
      (let* ((id (plist-get view :id))
             (last (plist-get state :wheel))
             (same (and (equal (nth 0 last) id) (equal (nth 1 last) px)
                        (equal (nth 2 last) (plist-get state :domains))))
             (next (eas-reduce--if-moved
                    state (eas-reduce--zoom (if same state (eas-reduce--remember state)) scene id
                                              (expt eas-reduce-wheel-step delta) px))))
        (if (eq next state) state
          (eas-reduce--put next :wheel (list id px (plist-get next :domains)))))
    state))

;;; Pointer

(defun eas-reduce--hover (state scene px)
  "Hover at PX in SCENE: nearest datum plus pointermove-driven selections.
Return the updated STATE."
  (let* ((view (eas-reduce--view-at scene px))
         (id (plist-get view :id))
         (movers (and view (eas-reduce--params scene id (lambda (p) (eas-reduce--on-p p "pointermove")))))
         (x-only (seq-some (lambda (p) (and (plist-get (plist-get p :def) :nearest)
                                            (equal (append (plist-get (plist-get p :def) :encodings) nil) '("x"))))
                           movers))
         (nearest (and view (or x-only (seq-some (lambda (p) (plist-get (plist-get p :def) :nearest)) movers))
                       (eas-hit scene id px x-only)))
         (nearest (and nearest (or x-only (<= (plist-get nearest :distance) eas-reduce-hover-radius)) nearest))
         ;; Marks interact only where the pointer touches them (fc-qx1.34);
         ;; an x-only crosshair snaps anywhere in the plot.
         (hit (if x-only nearest (and view (eas-intersect scene view px)))))
    (setq state (eas-reduce--put (eas-reduce--put state :hover hit) :pointer px))
    (dolist (p movers)
      (let ((h (if (plist-get (plist-get p :def) :nearest) nearest hit)))
        (setq state (eas-reduce--store
                     state (plist-get p :name)
                     (and h (eas-params-point-store (eas-params-point-fields scene p) (list (plist-get h :row))))))))
    state))

(defun eas-reduce--legend-click (state scene px)
  "Toggle a legend-bound selection when PX is on a legend entry; nil otherwise.
Return the updated STATE; SCENE supplies the legends."
  (cl-loop for view across (plist-get scene :views)
           thereis
           (cl-loop for legend across (plist-get view :legends)
                    thereis
                    (cl-loop for entry across (plist-get legend :entries)
                             when (eas-hit--contains (plist-get entry :bounds) (aref px 0) (aref px 1))
                             return
                             (when-let* ((p (car (eas-reduce--params scene (plist-get view :id)
                                                                       (lambda (p) (equal (plist-get p :bind) "legend")))))
                                         (field (eas-params-channel-field scene (plist-get view :id)
                                                                            (plist-get legend :channel))))
                               (eas-reduce--store
                                state (plist-get p :name)
                                (eas-params-toggle (plist-get (plist-get state :params) (eas-key (plist-get p :name)))
                                                     (list field)
                                                     (list (eas-key field) (plist-get entry :value)))))))))

(defun eas-reduce--click (state scene event)
  "Apply a click EVENT in SCENE to STATE.
A click toggles a legend entry, sets point selections or clears a brush."
  (let* ((px (plist-get event :px)))
    (or (eas-reduce--legend-click state scene px)
        (let* ((view (eas-reduce--view-at scene px)) (id (plist-get view :id)))
          (dolist (p (and view (eas-reduce--params scene id (lambda (p) (eas-reduce--on-p p "click")))))
            (let* ((def (plist-get p :def))
                   (x-only (equal (append (plist-get def :encodings) nil) '("x")))
                   (hit (eas-hit scene id px x-only))
                   (hit (and hit (or (plist-get def :nearest) (<= (plist-get hit :distance) eas-reduce-click-slop)) hit))
                   (fields (eas-params-point-fields scene p))
                   (old (plist-get (plist-get state :params) (eas-key (plist-get p :name)))))
              (setq state (eas-reduce--store
                           state (plist-get p :name)
                           (cond ((null hit) nil)
                                 ((and (eq (plist-get event :shift) t) (plist-get def :toggle))
                                  (eas-params-toggle old fields (plist-get hit :row)))
                                 (t (eas-params-point-store fields (list (plist-get hit :row)))))))))
          (dolist (p (and view (eas-reduce--params scene id #'eas-reduce--brush-p)))
            (setq state (eas-reduce--store state (plist-get p :name) nil)))
          state))))

(defun eas-reduce--brush-store (scene param start end)
  "Interval store for PARAM spanning pixels START..END in SCENE."
  (let ((fields nil) (store (list :type "interval")))
    (dolist (channel (eas-reduce--channels param))
      (when-let* ((scale (eas-reduce--scale scene (plist-get param :view) channel)))
        (let ((i (if (eq channel :x) 0 1)))
          (setq fields (append fields (list channel (or (plist-get scale :bin-source) (plist-get scale :field)))))
          (setq store (append store (list channel (vector (min (eas-scale-invert scale (aref start i))
                                                               (eas-scale-invert scale (aref end i)))
                                                          (max (eas-scale-invert scale (aref start i))
                                                               (eas-scale-invert scale (aref end i))))))))))
    (when fields (append store (list :fields fields)))))

(defun eas-reduce--drag-to (state scene px)
  "Continue the drag in STATE to PX over SCENE."
  (let* ((drag (plist-get state :drag)) (start (plist-get drag :start))
         (moved (or (plist-get drag :moved)
                    (> (+ (abs (- (aref px 0) (aref start 0))) (abs (- (aref px 1) (aref start 1))))
                       eas-reduce-click-slop)))
         (state (eas-reduce--put state :drag (eas-plist-put (eas-plist-put drag :current px) :moved moved))))
    (if (not moved) state
      (pcase (plist-get drag :mode)
        ("brush" (let ((p (seq-find (lambda (p) (equal (plist-get p :name) (plist-get drag :param))) (eas-params-of scene))))
                   (eas-reduce--store state (plist-get drag :param) (eas-reduce--brush-store scene p start px))))
        ("pan" (let ((view-id (plist-get drag :view)))
                 (dolist (channel '(:x :y))
                   (when-let* ((snap (plist-get (plist-get drag :snapshot) channel))
                               (scale (eas-reduce--scale scene view-id channel))
                               (i (if (eq channel :x) 0 1))
                               (domain (eas-zoom-pan-domain (plist-put (copy-sequence scale) :domain snap)
                                                              (- (aref px i) (aref start i)))))
                     (setq state (eas-reduce--set-domain state view-id channel domain))))
                 state))
        (_ state)))))

(defun eas-reduce--press (state scene px)
  "Start a drag at PX: brush when the view has a brush, pan when bound to scales.
Return the updated STATE; SCENE supplies the view."
  (let* ((view (eas-reduce--view-at scene px)) (id (plist-get view :id))
         (brush (and view (car (eas-reduce--params scene id #'eas-reduce--brush-p))))
         (pan (and view (car (eas-reduce--params scene id #'eas-reduce--scales-param-p)))))
    (eas-reduce--put state :drag
                       (list :view id :start px :current px :moved nil
                             :mode (cond (brush "brush") (pan "pan") (t "none"))
                             :param (and brush (plist-get brush :name))
                             :snapshot (and pan (cl-loop for ch in (eas-reduce--channels pan)
                                                         for s = (eas-reduce--scale scene id ch)
                                                         when s append (list ch (plist-get s :domain))))
                             :domains0 (plist-get state :domains)))))

(defun eas-reduce--release (state scene px)
  "End the drag at PX; an unmoved press is a click.
Return the updated STATE; SCENE supplies the view."
  (let ((drag (plist-get state :drag)) (state (eas-reduce--put state :drag nil)))
    (cond ((null drag) state)
          ((not (plist-get drag :moved)) (eas-reduce--click state scene (list :px px)))
          ((equal (plist-get drag :mode) "pan")
           (eas-reduce--remember state (list (plist-get drag :domains0))))
          (t state))))

;;; Entry point

(defun eas-reduce--zoom-to-brush (state scene)
  "Zoom every brushed SCENE view to its brush's ranges; clear the brushes.
Return the updated STATE.  The previous domains go on the history, so
[ undoes it."
  (let ((brushes (seq-filter (lambda (p) (and (eas-reduce--brush-p p)
                                              (plist-get (plist-get state :params) (eas-key (plist-get p :name)))))
                             (eas-params-of scene))))
    (if (null brushes) state
      (let ((s (eas-reduce--remember state)))
        (dolist (p brushes s)
          (let ((store (plist-get (plist-get s :params) (eas-key (plist-get p :name)))))
            (dolist (ch '(:x :y))
              (when-let* ((r (plist-get store ch))
                          ((/= (aref r 0) (aref r 1))))
                (setq s (eas-reduce--set-domain s (plist-get p :view) ch
                                                  (vector (min (aref r 0) (aref r 1)) (max (aref r 0) (aref r 1)))))))
            (setq s (eas-reduce--store s (plist-get p :name) nil))))))))

(defun eas-reduce--brush-event (state scene event)
  "Apply an agent's brush EVENT (data-space ranges) to STATE in SCENE."
  (let* ((params (eas-params-of scene "interval"))
         ;; A repeated spec has one param per view: "view" picks the cell.
         (params (or (seq-filter (lambda (p) (equal (plist-get p :view) (plist-get event :view))) params) params))
         (p (if (plist-get event :param)
                (seq-find (lambda (p) (equal (plist-get p :name) (plist-get event :param))) params)
              (or (seq-find #'eas-reduce--brush-p params) (car params))))
         (num (lambda (v) (if (numberp v) v (eas-time-parse v)))))
    (unless p
      (eas-signal "EVENT_INVALID" "This view has no interval selection param to brush"
                    :field "param" :params (vconcat (mapcar (lambda (p) (plist-get p :name)) (eas-params-of scene)))))
    (if (eas-reduce--scales-param-p p)
        (progn
          (setq state (eas-reduce--remember state))
          (dolist (ch '(:x :y) state)
            (when-let* ((r (plist-get event ch)))
              (setq state (eas-reduce--set-domain state (plist-get p :view) ch
                                                    (vector (funcall num (aref r 0)) (funcall num (aref r 1)))))))
          state)
      (let ((store (list :type "interval")) (fields nil))
        (dolist (ch '(:x :y))
          (when-let* ((r (plist-get event ch)))
            (setq fields (append fields (list ch (eas-params-channel-field scene (plist-get p :view) (eas-key-name ch))))
                  store (append store (list ch (vector (funcall num (aref r 0)) (funcall num (aref r 1))))))))
        (eas-reduce--store state (plist-get p :name) (append store (list :fields fields)))))))

(defun eas-reduce (state event scene)
  "Return the view STATE after EVENT, given the current SCENE.  Pure.
Views sharing a param bound to scales move together (`eas-link-scales')."
  (eas-link-scales state (eas-reduce--step state event scene) scene))

(defun eas-reduce--step (state event scene)
  "Return the view STATE after EVENT under SCENE, before linked views follow."
  (let ((px (plist-get event :px)))
    (pcase (plist-get event :type)
      ("pointermove" (if (plist-get state :drag) (eas-reduce--drag-to state scene px)
                       (eas-reduce--hover state scene px)))
      ("pointerdown" (eas-reduce--press state scene px))
      ("pointerup" (if (plist-get state :drag)
                       (eas-reduce--release (eas-reduce--drag-to state scene px) scene px)
                     state))
      ("click" (eas-reduce--click state scene event))
      ("dblclick"
       (let ((view (eas-reduce--view-at scene px)))
         (dolist (p (eas-params-of scene))
           (when (equal (plist-get p :view) (plist-get view :id))
             (setq state (eas-reduce--store state (plist-get p :name) nil))))
         (if (plist-get (plist-get state :domains) (eas-key (plist-get view :id)))
             (eas-reduce--put (eas-reduce--remember state) :domains
                                (eas--plist-without (plist-get state :domains) (eas-key (plist-get view :id))))
           state)))
      ("pointerleave"
       (dolist (p (eas-params-of scene "point"))
         (when (eas-reduce--on-p p "pointermove") (setq state (eas-reduce--store state (plist-get p :name) nil))))
       (eas-reduce--put (eas-reduce--put (eas-reduce--put state :hover nil) :drag nil) :pointer nil))
      ("wheel" (eas-reduce--wheel state scene px (plist-get event :delta)))
      ("drag"
       (let* ((s (eas-reduce--press state scene (plist-get event :from)))
              (s (eas-reduce--drag-to s scene (plist-get event :to))))
         (eas-reduce--release s scene (plist-get event :to))))
      ("brush" (eas-reduce--brush-event state scene event))
      ("param" (eas-params-set state scene (plist-get event :param) (plist-get event :value)))
      ("key" (eas-reduce--key state scene (plist-get event :key)))
      ("link" (eas-link-reduce state event scene))
      ("push" (eas-reduce--put state :stream-cursor
                                 (+ (or (plist-get state :stream-cursor) 0) (length (plist-get event :rows)))))
      (_ state))))

(defun eas-reduce--key (state scene key)
  "Apply KEY to STATE; SCENE gives the views it acts on."

  (let ((views (eas-reduce--target-views scene state)))
    (pcase key
      ((or "+" "=" "-")
       (let ((s (eas-reduce--remember state)))
         (dolist (v views (eas-reduce--if-moved state s))
           (setq s (eas-reduce--zoom s scene v (if (equal key "-") eas-reduce-zoom-step
                                                   (/ 1.0 eas-reduce-zoom-step)))))))
      ("0" (let ((s (eas-reduce--remember state)))
             (eas-reduce--if-moved
              state (eas-reduce--put s :domains (cl-loop for (k v) on (plist-get s :domains) by #'cddr
                                                           unless (member (eas-key-name k) views)
                                                           append (list k v))))))
      ((or "left" "right" "up" "down")
       (let ((s (eas-reduce--remember state))
             (channel (if (member key '("left" "right")) :x :y))
             (fraction (if (member key '("left" "down")) -0.1 0.1)))
         (dolist (v views (eas-reduce--if-moved state s)) (setq s (eas-reduce--pan s scene v channel fraction)))))
      ("z" (eas-reduce--zoom-to-brush state scene))
      ("escape" (eas-reduce--put (eas-reduce--put state :params nil) :hover nil))
      ("[" (if-let* ((prev (car (plist-get state :history))))
               (thread-first state
                             (eas-reduce--put :future (cons (or (plist-get state :domains) :none)
                                                              (plist-get state :future)))
                             (eas-reduce--put :history (cdr (plist-get state :history)))
                             (eas-reduce--put :domains (if (eq prev :none) nil prev)))
             state))
      ("]" (if-let* ((next (car (plist-get state :future))))
               (thread-first state
                             (eas-reduce--put :history (cons (or (plist-get state :domains) :none)
                                                               (plist-get state :history)))
                             (eas-reduce--put :future (cdr (plist-get state :future)))
                             (eas-reduce--put :domains (if (eq next :none) nil next)))
             state))
      (_ state))))

(provide 'eas-reduce)
;;; eas-reduce.el ends here

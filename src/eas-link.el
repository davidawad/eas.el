;;; eas-link.el --- linked views: shared scale binds and the link event -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.6).  Pure functions over view state and scene.
;;
;; Shared scale binds.  A param bound to scales that several views of
;; one spec hold (a concat-level param, see eas-link-scale.el) is one
;; selection in Vega-Lite, so zooming or panning any of those views
;; moves all of them on the param's channels.  `eas-link-scales' runs
;; after every reducer step and copies the moved view's domains.
;;
;; The link event.  Views in different buffers (or specs) share
;; selections through a named bus (eas-link-bus.el), which delivers
;; them as event/v1, so they are logged and replay like any other:
;;
;;   {"type": "link", "param": NAME, "from"?: VIEW-ID,
;;    "store": {"type": "interval", "x": [LO, HI], "y"?: [LO, HI]}
;;           | {"type": "point", "encodings": ["x"], "values": [[V], ...]}
;;           | {"type": "point", "fields": [F, ...], "values": [[V, ...], ...]}
;;           | null}
;;
;; Stores travel by channel, so "the same x" means the same x even when
;; the two views name their date fields differently.  A receiving point
;; selection with nearest: true snaps each x to its own nearest datum,
;; so a crosshair lands on the other ticker's bar for that day.  A
;; receiving param bound to scales takes the interval as its domains.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-hit)
(require 'eas-params)

(defun eas-link--key (name) "State key of param NAME." (eas-key name))

(defun eas-link--range (r)
  "Range R as numbers (dates become epoch ms), or nil."
  (when (and (vectorp r) (= (length r) 2))
    (let ((lo (eas-params--number (aref r 0))) (hi (eas-params--number (aref r 1))))
      (and lo hi (vector lo hi)))))

(defun eas-link--channels (param)
  "Channel keywords PARAM's selection covers."
  (mapcar #'eas-key (or (plist-get (plist-get param :def) :encodings) ["x" "y"])))

(defun eas-link--scales-p (param) "Non-nil when PARAM is bound to scales." (equal (plist-get param :bind) "scales"))

(defun eas-link--view-domains (state view-id)
  "VIEW-ID's zoomed domains in STATE (a plist :x :y), or nil."
  (plist-get (plist-get state :domains) (eas-key view-id)))

(defun eas-link--set-domain (state view-id channel domain)
  "STATE with VIEW-ID's CHANNEL domain DOMAIN; nil DOMAIN unzooms CHANNEL."
  (let* ((key (eas-key view-id)) (domains (plist-get state :domains))
         (mine (if domain (eas-plist-put (plist-get domains key) channel domain)
                 (eas--plist-without (plist-get domains key) channel))))
    (eas-plist-put state :domains (if mine (eas-plist-put domains key mine) (eas--plist-without domains key)))))

;;; Shared scale binds within one spec

(defun eas-link-scale-groups (scene)
  "Views of SCENE sharing a param bound to scales.
A list of (CHANNELS . VIEW-IDS), each with two views or more."
  (let (groups)
    (dolist (p (eas-params-of scene))
      (when (eas-link--scales-p p)
        (let ((entry (assoc (plist-get p :name) groups)))
          (if entry (cl-pushnew (plist-get p :view) (cddr entry) :test #'equal)
            (push (cons (plist-get p :name) (cons (eas-link--channels p) (list (plist-get p :view)))) groups)))))
    (cl-loop for (_ channels . views) in groups
             when (cdr views) collect (cons channels (reverse views)))))

(defun eas-link-scales (old new scene)
  "NEW state with zooms shared across views bound to one scales param.
OLD is the state before the reducer step; SCENE the scene it ran on.
The first view (in scene order) whose domain moved on a shared channel
sets that channel for the others."
  (let ((state new))
    (dolist (group (eas-link-scale-groups scene))
      (dolist (channel (car group))
        (when-let* ((moved (seq-find (lambda (id) (not (equal (plist-get (eas-link--view-domains old id) channel)
                                                              (plist-get (eas-link--view-domains new id) channel))))
                                     (cdr group))))
          (let ((domain (plist-get (eas-link--view-domains new moved) channel)))
            (dolist (id (cdr group))
              (unless (equal id moved) (setq state (eas-link--set-domain state id channel domain))))))))
    ;; One wheel gesture stays one history entry (eas-reduce compares :wheel's domains).
    (when-let* ((wheel (plist-get state :wheel))
                ((not (eq state new)))
                ((equal (nth 2 wheel) (plist-get new :domains))))
      (setq state (eas-plist-put state :wheel (list (nth 0 wheel) (nth 1 wheel) (plist-get state :domains)))))
    state))

;;; Outgoing: what a view's change looks like on the bus

(defun eas-link--param (scene name)
  "SCENE's first selection param called NAME."
  (seq-find (lambda (p) (equal (plist-get p :name) name)) (eas-params-of scene)))

(defun eas-link-store (scene state name)
  "Selection NAME of SCENE under STATE as a channel store for the bus, or nil."
  (when-let* ((p (eas-link--param scene name)))
    (if (eas-link--scales-p p)
        (let ((domains (eas-link--view-domains state (plist-get p :view))))
          (when-let* ((chs (seq-filter (lambda (ch) (plist-get domains ch)) (eas-link--channels p))))
            (append (list :type "interval") (cl-loop for ch in chs append (list ch (plist-get domains ch))))))
      (let ((store (plist-get (plist-get state :params) (eas-link--key name))))
        (pcase (plist-get store :type)
          ("interval" (append (list :type "interval")
                              (cl-loop for ch in '(:x :y) when (plist-get store ch) append (list ch (plist-get store ch)))))
          ("point" (let ((def (plist-get p :def)))
                     (if (and (plist-get def :encodings) (not (plist-get def :fields)))
                         (list :type "point" :encodings (vconcat (plist-get def :encodings))
                               :values (plist-get store :values))
                       (list :type "point" :fields (plist-get store :fields) :values (plist-get store :values))))))))))

(defun eas-link-changes (old-scene old new-scene new names)
  "Link events for params among NAMES whose bus store differs OLD -> NEW.
OLD-SCENE and NEW-SCENE are the scenes each state was drawn with.
NAMES nil means every selection param of NEW-SCENE."
  (let ((names (or names (delete-dups (mapcar (lambda (p) (plist-get p :name)) (eas-params-of new-scene))))))
    (cl-loop for name in names
             for before = (and old-scene (eas-link-store old-scene old name))
             for after = (eas-link-store new-scene new name)
             when (and (eas-link--param new-scene name) (not (equal before after)))
             collect (list :type "link" :param name :store (or after :null)))))

;;; Incoming: the link event

(defun eas-link--snap (scene param values)
  "VALUES (x data values) snapped to the nearest x of PARAM's view in SCENE.
A value outside the view's x range stays as it is (matching nothing)."
  (let* ((view (seq-find (lambda (v) (equal (plist-get v :id) (plist-get param :view))) (plist-get scene :views)))
         (xs (plist-get (plist-get view :scales) :x))
         (field (plist-get xs :field))
         (b (plist-get view :bounds)))
    (mapcar (lambda (v)
              (let ((px (and xs b (eas-scale-apply xs v))))
                (or (and px field (<= (aref b 0) px (+ (aref b 0) (aref b 2)))
                         (when-let* ((hit (eas-hit scene (plist-get view :id)
                                                     (vector px (+ (aref b 1) (/ (aref b 3) 2.0))) t)))
                           (plist-get (plist-get hit :row) (eas-key field))))
                    v)))
            values)))

(defun eas-link--point-store (scene param store)
  "STORE (a bus point store) as PARAM's own point store in SCENE, or `skip'."
  (let ((def (plist-get param :def)) (values (append (plist-get store :values) nil)))
    (if (plist-get store :encodings)
        (let* ((src (mapcar #'eas-key (plist-get store :encodings)))
               (mine (mapcar #'eas-key (or (plist-get def :encodings) [])))
               (idx (mapcar (lambda (ch) (cl-position ch src)) mine)))
          (if (or (null mine) (memq nil idx) (plist-get def :fields)) 'skip
            (let* ((tuples (mapcar (lambda (tuple) (mapcar (lambda (i) (aref tuple i)) idx)) values))
                   (tuples (if (and (plist-get def :nearest) (equal mine '(:x)))
                               (mapcar #'list (eas-link--snap scene param (mapcar #'car tuples)))
                             tuples)))
              (list :type "point"
                    :fields (vconcat (eas-params-point-fields scene param))
                    :values (vconcat (mapcar #'vconcat (delete-dups tuples)))))))
      (let ((fields (append (plist-get store :fields) nil)))
        (if (equal fields (eas-params-point-fields scene param))
            (list :type "point" :fields (vconcat fields) :values (vconcat values))
          'skip)))))

(defun eas-link--interval-store (scene param store)
  "STORE (a bus interval store) as PARAM's own interval store in SCENE, or `skip'."
  (let ((out (list :type "interval")) fields)
    (dolist (ch (eas-link--channels param))
      (when-let* ((r (eas-link--range (plist-get store ch)))
                  (field (eas-params-channel-field scene (plist-get param :view) (eas-key-name ch))))
        (setq fields (append fields (list ch field)) out (append out (list ch r)))))
    (if fields (append out (list :fields fields)) 'skip)))

(defun eas-link-reduce (state event scene)
  "Apply link EVENT to STATE under SCENE: every param of its name takes the store."
  (let* ((name (plist-get event :param))
         (store (let ((s (plist-get event :store))) (and (not (eq s :null)) s)))
         (params (seq-filter (lambda (p) (equal (plist-get p :name) name)) (eas-params-of scene))))
    (dolist (p params state)
      (cond
       ((eas-link--scales-p p)
        (dolist (ch (eas-link--channels p))
          (let ((r (eas-link--range (plist-get store ch))))
            (when (or (null store) r)
              (setq state (eas-link--set-domain state (plist-get p :view) ch r))))))
       (t
        (let ((mine (cond ((null store) nil)
                          ((not (equal (plist-get store :type) (plist-get (plist-get p :def) :type))) 'skip)
                          ((equal (plist-get store :type) "point") (eas-link--point-store scene p store))
                          (t (eas-link--interval-store scene p store)))))
          (unless (eq mine 'skip)
            (setq state (eas-plist-put state :params
                                       (eas-plist-put (plist-get state :params) (eas-link--key name) mine))))))))))

(provide 'eas-link)
;;; eas-link.el ends here

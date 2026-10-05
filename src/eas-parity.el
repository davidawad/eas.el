;;; eas-parity.el --- one event log, two backends, one view state -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.8).  GUI and terminal glue emit the same event/v1
;; stream, but pointer events carry scene pixels, and the svg and text
;; targets lay a chart out differently (cells snap axes and margins).
;; Parity therefore means: an event aimed at a datum on one backend,
;; aimed at the same datum on the other, leaves both views in the same
;; state.
;;
;;   `eas-parity-translate'  maps an event's pixels from one scene to
;;                             another through data space (scale invert,
;;                             then apply); legend entries map to the
;;                             same entry, other pixels proportionally.
;;                             Keys, brushes and pushes pass unchanged.
;;   `eas-parity-state'      a view's state without the pixel-only
;;                             parts: domains, selections, hovered and
;;                             clicked datum, history depth, drag mode.
;;   `eas-parity-diff'       where two such states differ.
;;   `eas-parity-replay'     drives two views in lockstep from one log
;;                             (oldest first, or a view's own log),
;;                             translating each event from the first
;;                             view's scene to the second's, and compares
;;                             after every step.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-hit)
(require 'eas-event)
(require 'eas-view)

(defvar eas-parity-tolerance 1e-9
  "Relative difference below which two numbers are the same state.")

;;; Translation

(defun eas-parity--legend-entry (scene x y)
  "(VIEW-ID CHANNEL VALUE) of the legend entry of SCENE under X Y, or nil."
  (cl-loop for view across (plist-get scene :views)
           thereis (cl-loop for legend across (or (plist-get view :legends) [])
                            thereis (cl-loop for entry across (or (plist-get legend :entries) [])
                                             when (and (plist-get entry :bounds)
                                                       (eas-hit--contains (plist-get entry :bounds) x y))
                                             return (list (plist-get view :id) (plist-get legend :channel)
                                                          (plist-get entry :value))))))

(defun eas-parity--legend-px (scene key)
  "Centre of SCENE's legend entry matching KEY from `eas-parity--legend-entry'."
  (cl-loop for view across (plist-get scene :views)
           when (equal (plist-get view :id) (nth 0 key))
           thereis (cl-loop for legend across (or (plist-get view :legends) [])
                            when (equal (plist-get legend :channel) (nth 1 key))
                            thereis (cl-loop for entry across (or (plist-get legend :entries) [])
                                             when (equal (plist-get entry :value) (nth 2 key))
                                             return (let ((b (plist-get entry :bounds)))
                                                      (vector (+ (aref b 0) (/ (aref b 2) 2.0))
                                                              (+ (aref b 1) (/ (aref b 3) 2.0))))))))

(defun eas-parity--view (scene id)
  "SCENE's view with ID."
  (seq-find (lambda (v) (equal (plist-get v :id) id)) (plist-get scene :views)))

(defun eas-parity--proportional (from-box to-box v i)
  "Coordinate V on axis I placed at the same fraction of TO-BOX as of FROM-BOX."
  (let ((f0 (aref from-box i)) (fw (aref from-box (+ i 2)))
        (t0 (aref to-box i)) (tw (aref to-box (+ i 2))))
    (if (zerop fw) (+ t0 (/ tw 2.0)) (+ t0 (* tw (/ (- v f0) (float fw)))))))

(defun eas-parity--through-scale (from to v i)
  "Coordinate V on axis I of scene view FROM, mapped to scene view TO via data."
  (let* ((ch (if (= i 0) :x :y))
         (fs (plist-get (plist-get from :scales) ch))
         (ts (plist-get (plist-get to :scales) ch))
         (data (and fs ts (equal (plist-get fs :type) (plist-get ts :type))
                    (eas-scale-invert fs v)))
         (out (and data (eas-scale-apply ts data))))
    (cond ((null out) (eas-parity--proportional (plist-get from :bounds) (plist-get to :bounds) v i))
          ((member (plist-get ts :type) '("band" "point"))
           ;; The same fraction of the way across the band.
           (let ((fw (or (plist-get fs :bandwidth) 0)) (tw (or (plist-get ts :bandwidth) 0)))
             (+ out (if (zerop fw) 0 (* tw (/ (- v (eas-scale-apply fs data)) (float fw)))))))
          (t out))))

(defun eas-parity-translate-px (px from-scene to-scene)
  "Scene pixel PX of FROM-SCENE as the matching pixel of TO-SCENE.
A legend entry maps to the same entry; a point in a plot maps through
its view's scales, so it lands on the same data; anything else keeps
its fraction of the scene."
  (let* ((x (aref px 0)) (y (aref px 1))
         (legend (eas-parity--legend-entry from-scene x y))
         (from (seq-find (lambda (v) (eas-hit--contains (plist-get v :bounds) x y))
                         (plist-get from-scene :views)))
         (to (and from (eas-parity--view to-scene (plist-get from :id)))))
    (cond
     ((and legend (eas-parity--legend-px to-scene legend)))
     (to (vector (eas-parity--through-scale from to x 0) (eas-parity--through-scale from to y 1)))
     (t (let* ((fs (plist-get from-scene :size)) (ts (plist-get to-scene :size))
               (fbox (vector 0 0 (plist-get fs :w) (plist-get fs :h)))
               (tbox (vector 0 0 (plist-get ts :w) (plist-get ts :h))))
          (vector (eas-parity--proportional fbox tbox x 0)
                  (eas-parity--proportional fbox tbox y 1)))))))

(defun eas-parity-translate (event from-scene to-scene)
  "EVENT (event/v1) with its pixels moved from FROM-SCENE to TO-SCENE.
Events without pixels (key, brush, push, pointerleave) are returned as is."
  (let ((event (eas-event-parse event)))
    (dolist (field '(:px :from :to) event)
      (when-let* ((px (plist-get event field)))
        (setq event (eas-plist-put event field (eas-parity-translate-px px from-scene to-scene)))))))

;;; Comparable state

(defun eas-parity-state (view)
  "VIEW's state without what depends on pixels, as comparable data."
  (let* ((state (eas-view-state (eas-view-get view)))
         (hover (plist-get state :hover)) (click (plist-get state :click))
         (drag (plist-get state :drag)))
    (list :domains (plist-get state :domains)
          :params (plist-get state :params)
          :hover (and hover (list :view (plist-get hover :view) :mark (plist-get hover :mark)
                                  :datum (plist-get hover :datum)))
          :click (and click (list :view (plist-get click :view) :mark (plist-get click :mark)
                                  :datum (plist-get click :datum) :action (plist-get click :action)))
          :drag (and drag (list :view (plist-get drag :view) :mode (plist-get drag :mode)
                                :param (plist-get drag :param) :moved (plist-get drag :moved)))
          :history (length (plist-get state :history))
          :future (length (plist-get state :future))
          :stream-cursor (plist-get state :stream-cursor))))

(defun eas-parity--same-number (a b)
  "Non-nil when numbers A and B agree within `eas-parity-tolerance'."
  (<= (abs (- a b)) (* eas-parity-tolerance (max 1.0 (abs a) (abs b)))))

(defun eas-parity-diff (a b &optional path)
  "Differences between comparable data A and B, as (:path :a :b) plists.
PATH prefixes the reported paths."
  (let ((path (or path "")))
    (cond
     ((and (numberp a) (numberp b)) (unless (eas-parity--same-number a b) (list (list :path path :a a :b b))))
     ((and (vectorp a) (vectorp b) (= (length a) (length b)))
      (cl-loop for i below (length a)
               append (eas-parity-diff (aref a i) (aref b i) (format "%s/%d" path i))))
     ((and (consp a) (consp b) (keywordp (car a)) (keywordp (car b)))
      (cl-loop for k in (delete-dups (append (eas-plist-keys a) (eas-plist-keys b)))
               append (eas-parity-diff (plist-get a k) (plist-get b k)
                                         (format "%s/%s" path (eas-key-name k)))))
     ((and (consp a) (consp b) (proper-list-p a) (proper-list-p b) (= (length a) (length b)))
      (cl-loop for x in a for y in b for i from 0
               append (eas-parity-diff x y (format "%s/%d" path i))))
     ((equal a b) nil)
     (t (list (list :path path :a a :b b))))))

;;; Lockstep replay

(defun eas-parity--events (log)
  "LOG's events oldest first (a view's own log is newest first)."
  (let ((entries (append log nil)))
    (when (and (cdr entries) (plist-get (car entries) :seq)
               (> (plist-get (car entries) :seq) (plist-get (cadr entries) :seq)))
      (setq entries (reverse entries)))
    (mapcar (lambda (e) (or (plist-get e :event) e)) entries)))

(defun eas-parity-replay (log a b)
  "Apply LOG to views A and B in lockstep and compare their states.
Each event goes to A as written and to B translated from A's current
scene to B's.  Returns (:ok t|:false :steps N :mismatches [(:step
:event :path :a :b) ...]); step 0 compares the states before any event."
  (let* ((a (eas-view-get a)) (b (eas-view-get b))
         (mismatches nil) (step 0)
         (compare (lambda (event)
                    (dolist (d (eas-parity-diff (eas-parity-state a) (eas-parity-state b)))
                      (push (append (list :step step :event (or event :null)) d) mismatches)))))
    (funcall compare nil)
    (dolist (event (eas-parity--events log))
      (cl-incf step)
      (let ((other (eas-parity-translate event (eas-view-scene a) (eas-view-scene b)))
            (eas-view-replaying t))
        (eas-dispatch a event)
        (eas-dispatch b other))
      (funcall compare (eas-event-describe (eas-event-parse event))))
    (list :ok (if mismatches :false t) :steps step :mismatches (vconcat (nreverse mismatches)))))

(provide 'eas-parity)
;;; eas-parity.el ends here

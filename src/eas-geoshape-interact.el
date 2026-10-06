;;; eas-geoshape-interact.el --- zoom and pan a map through its params -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A map zooms and pans by changing the params its
;; projection reads, as the Vega gallery's zoomable world map does
;; with signals:
;;
;;   "scale": {"expr": "S"}                  a wheel tick multiplies S
;;   "rotate": {"expr": "[L, ...]"}          a drag adds the longitude
;;   "center": {"expr": "[..., P]"}          moved to L and the latitude
;;                                           to P (else rotate's second)
;;
;; where S, L and P name variable params.  A bound range input clamps
;; each (its min and max).  The drag inverts the projection at both
;; ends (`eas-geo-proj-invert'), as Vega's invert(projection, xy()).
;; A map whose projection reads no param leaves the event to the usual
;; reducer.  Compile puts each geoshape mark's projection on the scene
;; (`eas-geoshape-mark-meta'): the spec with its expressions and the
;; plain one it resolved to.

;;; Code:

(require 'cl-lib)
(require 'eas-core)
(require 'eas-geo-proj)

(defvar eas-zoom-wheel-step)

(defun eas-geoshape-mark-meta (unit)
  "Scene mark properties of geoshape UNIT: its projection, or nil."
  (when-let* ((proj (plist-get unit :geo-proj)))
    (list :geo (list :projection (plist-get (plist-get (plist-get (plist-get unit :node) :x-eas) :geo) :projection)
                     :resolved (plist-get proj :spec)))))

(defun eas-geoshape--map-at (scene px)
  "The :geo of the first projected mark of SCENE's view under PX, or nil."
  (let ((view (seq-find (lambda (v) (let ((b (plist-get v :bounds)))
                                      (and (<= (aref b 0) (aref px 0) (+ (aref b 0) (aref b 2)))
                                           (<= (aref b 1) (aref px 1) (+ (aref b 1) (aref b 3))))))
                        (plist-get scene :views))))
    (when view
      (seq-some (lambda (m) (and (plist-get m :geo) (cons view (plist-get m :geo)))) (plist-get view :marks)))))

(defun eas-geoshape--names (expr)
  "The elements of EXPR, an {\"expr\": ...} array literal or name, as strings."
  (let ((s (and (eas-object-p expr) (plist-get expr :expr))))
    (when (stringp s)
      (mapcar #'string-trim (split-string (string-trim s "[ \t[]+" "[] \t]+") ",")))))

(defun eas-geoshape--param (scene state name)
  "Param NAME of SCENE with its value in STATE: (VALUE MIN MAX), or nil."
  (let ((raw (and (stringp name) (string-match-p "\\`[A-Za-z_$][A-Za-z0-9_$]*\\'" name)
                  (seq-find (lambda (p) (equal (plist-get p :name) name)) (plist-get scene :params)))))
    (when (and raw (not (plist-get raw :select)))
      (let ((v (let ((s (plist-get (plist-get state :params) (eas-key name))))
                 (if (numberp s) s (plist-get raw :value))))
            (bind (plist-get raw :bind)))
        (when (numberp v)
          (list v (and (eas-object-p bind) (plist-get bind :min)) (and (eas-object-p bind) (plist-get bind :max))))))))

(defun eas-geoshape--set (state name value lo hi)
  "STATE with variable param NAME at VALUE clamped to LO HI."
  (let ((v (max (or lo -1.0e+INF) (min (or hi 1.0e+INF) value))))
    (eas-plist-put state :params (eas-plist-put (plist-get state :params) (eas-key name) v))))

(defun eas-geoshape-wheel (state scene px delta)
  "STATE after DELTA wheel ticks at PX over a map of SCENE, or nil.
nil when no map there reads its scale from a param."
  (when-let* ((hit (eas-geoshape--map-at scene px))
              (name (car (eas-geoshape--names (plist-get (plist-get (cdr hit) :projection) :scale))))
              (p (eas-geoshape--param scene state name)))
    (eas-geoshape--set state name (/ (nth 0 p) (expt (or (bound-and-true-p eas-zoom-wheel-step) 1.25) delta))
                       (nth 1 p) (nth 2 p))))

(defun eas-geoshape-drag (state scene from to)
  "STATE after dragging a map of SCENE from pixel FROM to TO, or nil.
nil when no map there reads its rotation or centre from params."
  (when-let* ((hit (eas-geoshape--map-at scene from)))
    (let* ((view (car hit)) (geo (cdr hit)) (b (plist-get view :bounds))
           (spec (plist-get geo :projection))
           (rot (eas-geoshape--names (plist-get spec :rotate)))
           (cen (eas-geoshape--names (plist-get spec :center)))
           (lon-name (nth 0 rot)) (lat-name (or (nth 1 cen) (nth 1 rot)))
           (lon (eas-geoshape--param scene state lon-name))
           (lat (eas-geoshape--param scene state lat-name)))
      (when (or lon lat)
        (let* ((proj (eas-geo-proj (plist-get geo :resolved)))
               (a (eas-geo-proj-invert proj (- (aref from 0) (aref b 0)) (- (aref from 1) (aref b 1))))
               (z (and a (eas-geo-proj-invert proj (- (aref to 0) (aref b 0)) (- (aref to 1) (aref b 1)) (append a nil)))))
          (when (and a z)
            (when lon
              (setq state (eas-geoshape--set state lon-name (+ (nth 0 lon) (- (aref z 0) (aref a 0))) (nth 1 lon) (nth 2 lon))))
            (when lat
              (setq state (eas-geoshape--set state lat-name (+ (nth 0 lat) (- (aref a 1) (aref z 1))) (nth 1 lat) (nth 2 lat))))
            state))))))

(provide 'eas-geoshape-interact)
;;; eas-geoshape-interact.el ends here

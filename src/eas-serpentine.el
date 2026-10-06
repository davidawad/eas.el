;;; eas-serpentine.el --- the serpentine domain transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A serpentine timeline lays a numeric domain along a path that runs
;; straight across, turns down a half circle, runs back, and so on.
;; Vega-Lite has no path layout, so the "serpentine" transform computes
;; it at resolve time, as the Vega gallery's serpentine timeline does
;; with signals and formulas:
;;
;;   {"x-eas:transform": "serpentine", "field": "year",
;;    "domain": [1926, 2026], "width": 300, "diameter": 125, "arcs": 2.2}
;;
;; Each input row is placed at its FIELD value and tagged category
;; "milestone"; the transform appends the path itself (category
;; "serpentine", one row per STEP of the domain, "first" true on the
;; first row of each straight-plus-arc segment), TICKS evenly spaced
;; "tick" rows and one "start" and one "end" row.  Every row gets
;; pixel x and y (from the path's top-left), the segment index i, the
;; arc angle alpha (radians), type "straight" or "arc", side
;; ("right"/"left" on arcs), hemisphere ("top"/"bottom"), labelAngle
;; (degrees, tangent to the path) and direction ("→" or "←").  A
;; template draws them with pixel x and y scales.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)

(defun eas-serpentine--place (k sw sh)
  "Geometry plist of path position K (pixels along the path).
SW is the straight run's length and SH the arc diameter."
  (let* ((sa (* sh float-pi 0.5)) (swsa (+ sw sa))
         (i (floor k swsa))
         (r (- k (* i swsa)))
         (alpha (/ (- r sw) (/ sh 2.0)))
         (straight (>= (- (* (1+ i) swsa) sa) k))
         (even (zerop (mod i 2)))
         (xs (if even (min r sw) (max (- sw r) 0)))
         (side (and (not straight) (if even "right" "left")))
         (top (< alpha (/ float-pi 2))))
    (list :i i :alpha alpha :type (if straight "straight" "arc")
          :x (if straight xs (+ xs (* (if even 1 -1) (sin alpha) (/ sh 2.0))))
          :y (if straight (* i sh) (+ (* i sh) (* (- 1 (cos alpha)) (/ sh 2.0))))
          :side (or side :null)
          :hemisphere (if straight :null (if top "top" "bottom"))
          :labelAngle (if straight 0
                        (+ (* (if (equal side "left") -1 1) alpha (/ 180 float-pi)) (if top 0 180)))
          :direction (cond (straight (if even "→" "←"))
                           ((equal side "left") (if top "←" "→"))
                           (t (if top "→" "←"))))))

(defun eas-serpentine--transform (rows params)
  "The serpentine domain transform of ROWS per PARAMS."
  (let* ((field (eas-key (plist-get params :field)))
         (domain (plist-get params :domain))
         (lo (float (aref domain 0))) (hi (float (aref domain 1)))
         (width (float (plist-get params :width)))
         (sh (float (plist-get params :diameter)))
         (sn (plist-get params :arcs))
         (pct (let ((p (plist-get params :straight))) (cond ((< p 0.25) 0) ((< p 0.75) 0.5) (t 1))))
         (sw (* pct width))
         (len (+ (* (1+ sn) sw) (* sn sh float-pi 0.5)))
         (r0 (* (plist-get params :start) width)) (r1 (* len (plist-get params :length)))
         (reverse (eq (plist-get params :reverse) t))
         (k-of (lambda (v) (let ((f (if (= hi lo) 0 (/ (- v lo) (- hi lo)))))
                             (+ r0 (* (if reverse (- 1 f) f) (- r1 r0))))))
         (place (lambda (v category extra)
                  (append extra (list :category category :domain v)
                          (eas-serpentine--place (funcall k-of v) sw sh))))
         (step (plist-get params :step))
         (ticks (plist-get params :ticks))
         (out nil) (last-i nil))
    (seq-doseq (row rows)
      (let ((v (plist-get row field)))
        (when (numberp v) (push (funcall place v "milestone" row) out))))
    (cl-loop for n from 0
             for v = (+ lo (* n step))
             while (< v hi)
             do (let ((p (funcall place v "serpentine" nil)))
                  (push (append p (list :first (if (equal (plist-get p :i) last-i) :false t))) out)
                  (setq last-i (plist-get p :i))))
    (cl-loop for id from 1 below (1+ ticks)
             for v = (cond ((= id 1) lo) ((= id ticks) hi)
                           (t (+ lo (* (- hi lo) (/ (- id 1.0) (max 1 (- ticks 1)))))))
             do (push (funcall place (round v) "tick" nil) out))
    (push (funcall place (if reverse hi lo) "start" nil) out)
    (push (funcall place (if reverse lo hi) "end" nil) out)
    (vconcat (nreverse out))))

(eas-register-transform
 "serpentine"
 :doc "Lay a numeric domain along a serpentine path: rows as milestones plus path, tick, start and end rows, in pixels."
 :schema '(:field (:type "string" :required t :doc "numeric field placing each input row")
           :domain (:type "array" :required t :doc "[lo hi] of the timeline")
           :width (:type "number" :default 300 :doc "width of the straight runs' span, pixels")
           :diameter (:type "number" :default 125 :doc "arc diameter, pixels")
           :arcs (:type "number" :default 2.2 :doc "number of arcs, fractional allowed")
           :straight (:type "number" :default 1 :doc "share of width the straight runs take (0, 0.5 or 1)")
           :start (:type "number" :default 0 :doc "share of width the timeline starts at")
           :length (:type "number" :default 1 :doc "share of the path's length the timeline ends at")
           :reverse (:type "boolean" :default :false :doc "run the domain backwards")
           :step (:type "number" :default 0.1 :doc "domain step between path rows")
           :ticks (:type "integer" :default 21 :doc "tick rows, ends included"))
 :fn #'eas-serpentine--transform)

(provide 'eas-serpentine)
;;; eas-serpentine.el ends here

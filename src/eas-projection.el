;;; eas-projection.el --- map projections of longitude/latitude points -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A view with a Vega-Lite "projection" places its marks by
;; longitude and latitude.  `eas-projection-expand' lowers such a unit
;; before compile: the "eas-project" domain transform adds the planar
;; coordinates of each row under the projection's raw formula (d3-geo's,
;; north up), and the unit draws them on linear x and y scales without
;; axes, zero or nice.  After layout, `eas-projection-ranges' fits the
;; two scales as d3's fitSize does for Vega-Lite's default projection
;; fit: one scale factor k = min(w/dx, h/dy), so the map keeps its
;; aspect, centred in the plot.
;;
;; Native: points, circles, squares, text, rules without x2/y2 and the
;; other point-placed marks, under equalEarth, mercator or
;; equirectangular with no other projection property.  Geoshapes,
;; longitude2/latitude2, topojson and projection parameters (scale,
;; rotate, center, ...) stay unsupported (eas-spec-props.el reports them).

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)

(defconst eas-projection-types '("equalEarth" "mercator" "equirectangular")
  "Projection types drawn natively.")

(defconst eas-projection--x "eas_proj_x" "Field holding a row's projected x.")
(defconst eas-projection--y "eas_proj_y" "Field holding a row's projected y (north up).")

(defun eas-projection-raw (type lon lat)
  "Planar (X . Y) of LON LAT (degrees) under projection TYPE, north up.
These are d3-geo's raw projections, in radians."
  (let ((l (degrees-to-radians lon)) (p (degrees-to-radians lat)))
    (pcase type
      ("equalEarth"
       (let* ((a1 1.340264) (a2 -0.081106) (a3 0.000893) (a4 0.003796) (m (/ (sqrt 3) 2))
              (th (asin (* m (sin p)))) (t2 (* th th)) (t6 (* t2 t2 t2)))
         (cons (/ (* l (cos th)) (* m (+ a1 (* 3 a2 t2) (* t6 (+ (* 7 a3) (* 9 a4 t2))))))
               (* th (+ a1 (* a2 t2) (* t6 (+ a3 (* a4 t2))))))))
      ("mercator" (cons l (log (tan (/ (+ (/ float-pi 2) p) 2)))))
      (_ (cons l p)))))

(defun eas-projection--rows (rows params)
  "ROWS with PARAMS's projected coordinates added (the eas-project transform)."
  (let ((type (plist-get params :projection))
        (lon (eas-key (plist-get params :longitude))) (lat (eas-key (plist-get params :latitude)))
        (as (plist-get params :as)))
    (seq-map (lambda (row)
               (let ((x (plist-get row lon)) (y (plist-get row lat)))
                 (if (and (numberp x) (numberp y))
                     (let ((xy (eas-projection-raw type x y)))
                       (append (list (eas-key (aref as 0)) (car xy) (eas-key (aref as 1)) (cdr xy)) row))
                   row)))
             rows)))

(eas-register-transform
 "eas-project"
 :doc "Planar coordinates of longitude/latitude rows under a map projection (Vega-Lite projection, lowered)."
 :schema '(:projection (:type "string" :required t) :longitude (:type "string" :required t)
           :latitude (:type "string" :required t) :as (:type "array" :required t))
 :fn #'eas-projection--rows)

(defun eas-projection--type (proj)
  "The type of Vega-Lite projection PROJ (equalEarth by default)."
  (or (and (eas-object-p proj) (plist-get proj :type)) "equalEarth"))

(defun eas-projection--unit (view proj)
  "Unit VIEW placed by longitude/latitude under projection PROJ, lowered."
  (let* ((enc (plist-get view :encoding))
         (lon (plist-get enc :longitude)) (lat (plist-get enc :latitude))
         (axis (lambda (field title)
                 (list :field field :type "quantitative" :axis :null
                       :scale (list :zero :false :nice :false)
                       :title (or title :null)))))
    (if (not (and (eas-object-p lon) (eas-object-p lat) (plist-get lon :field) (plist-get lat :field)))
        view
      (let ((enc (eas--plist-without (eas--plist-without enc :longitude) :latitude)))
        (setq enc (append (list :x (funcall axis eas-projection--x (plist-get lon :field))
                                :y (funcall axis eas-projection--y (plist-get lat :field)))
                          enc))
        (eas-plist-put
          (eas-plist-put view :transform
                         (vconcat (plist-get view :transform)
                                  (list (list :x-eas:transform "eas-project"
                                              :projection (eas-projection--type proj)
                                              :longitude (plist-get lon :field) :latitude (plist-get lat :field)
                                              :as (vector eas-projection--x eas-projection--y)))))
          :encoding enc)))))

(defun eas-projection-expand (spec &optional proj)
  "SPEC with every unit under a projection (its own or PROJ, inherited)
placed on projected x and y."
  (let ((proj (or (plist-get spec :projection) proj)))
    (cond
     ((null proj) (if (seq-some (lambda (k) (vectorp (plist-get spec k))) '(:layer :vconcat :hconcat))
                      (eas-projection--children spec nil)
                    spec))
     ((plist-get spec :mark) (eas-projection--unit spec proj))
     (t (eas-projection--children spec proj)))))

(defun eas-projection--children (spec proj)
  "SPEC with its layer/concat children expanded under PROJ."
  (let ((out spec))
    (dolist (k '(:layer :vconcat :hconcat))
      (when (vectorp (plist-get out k))
        (setq out (eas-plist-put out k (vconcat (mapcar (lambda (c) (eas-projection-expand c proj))
                                                        (plist-get out k)))))))
    out))

(defun eas-projection-view-p (group)
  "Non-nil when GROUP draws projected positions (this file's or eas-geo's).
Vega-Lite styles such a view \"view\", not \"cell\": config.view's fill
and frame stroke do not paint it."
  (member (plist-get (plist-get (plist-get group :scales) :x) :field) (list eas-projection--x "x_projected")))

(defun eas-projection--fit (scale lo hi)
  "SCALE with domain [LO HI], keeping its range."
  (plist-put (copy-sequence scale) :domain (vector lo hi)))

(declare-function eas-compile-set-range "eas-compile-scales")

(defun eas-projection-ranges (group)
  "Fit GROUP's projected x and y scales to its plot, as d3's fitSize does."
  (let* ((scales (plist-get group :scales)) (sx (plist-get scales :x)) (sy (plist-get scales :y)))
    (when (and (equal (plist-get sx :field) eas-projection--x) (equal (plist-get sy :field) eas-projection--y)
               (equal (plist-get sx :type) "linear") (equal (plist-get sy :type) "linear"))
        (let* ((dx (or (plist-get sx :proj-domain) (plist-get sx :domain)))
               (dy (or (plist-get sy :proj-domain) (plist-get sy :domain)))
               (w (float (plist-get group :w))) (h (float (plist-get group :h)))
               (spanx (- (aref dx 1) (aref dx 0))) (spany (- (aref dy 1) (aref dy 0)))
               (k (cond ((and (> spanx 0) (> spany 0)) (min (/ w spanx) (/ h spany)))
                        ((> spanx 0) (/ w spanx)) ((> spany 0) (/ h spany)) (t 1.0)))
               (cx (/ (+ (aref dx 0) (aref dx 1)) 2.0)) (cy (/ (+ (aref dy 0) (aref dy 1)) 2.0))
               (hx (/ w 2 k)) (hy (/ h 2 k)))
          (plist-put scales :x (eas-compile-set-range
                                (plist-put (eas-projection--fit sx (- cx hx) (+ cx hx)) :proj-domain dx)
                                (plist-get sx :range)))
          (plist-put scales :y (eas-compile-set-range
                                (plist-put (eas-projection--fit sy (- cy hy) (+ cy hy)) :proj-domain dy)
                                (plist-get sy :range)))
          (plist-put group :scales scales)))))

(provide 'eas-projection)
;;; eas-projection.el ends here

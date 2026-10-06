;;; eas-mark-style.el --- area and arc mark properties beyond fill and stroke -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Parts of L4 and L5 for the area and arc marks (fc-qx1.42).  The
;; compiler gives every item its fill, stroke and opacity
;; (`eas-marks--style'); `eas-mark-style-extras' adds the rest of the
;; Vega-Lite mark properties these marks honor:
;;
;;   strokeDashOffset strokeMiterLimit blend           area, arc, line
;;   href                                              area, line (arcs: eas-marks--extras)
;;   strokeCap strokeJoin                              area, arc
;;   strokeDash cornerRadius                           arc
;;
;; and gives a stroked area its outline width (Vega's default 1).
;; `eas-mark-style-svg' writes them, with fillOpacity and strokeOpacity,
;; as SVG presentation attributes on the item's path; blend becomes
;; mix-blend-mode.  The text renderer draws none of them: a cell has no
;; dash, cap or blend.

;;; Code:

(require 'eas-core)

(defconst eas-mark-style-blend-modes
  '("multiply" "screen" "overlay" "darken" "lighten" "color-dodge" "color-burn" "hard-light"
    "soft-light" "difference" "exclusion" "hue" "saturation" "color" "luminosity")
  "Vega's mark.blend values (CSS mix-blend-mode).")

(defun eas-mark-style--visible-p (paint)
  "Non-nil when PAINT is a color that draws."
  (and paint (not (member paint '("none" "transparent"))) (not (eq paint :null))))

(defun eas-mark-style-extras (unit style)
  "Item properties of UNIT's area, arc or line mark beyond STYLE.
STYLE is the item's `eas-marks--style' plist."
  (let* ((mark (plist-get unit :mark)) (type (plist-get mark :type))
         (area (equal type "area")) (arc (equal type "arc"))
         (blend (plist-get mark :blend)))
    (append
     (cl-loop for k in '(:strokeDashOffset :strokeMiterLimit)
              for v = (plist-get mark k) when (numberp v) append (list k v))
     (when (member blend eas-mark-style-blend-modes) (list :blend blend))
     ;; Arcs take href through `eas-marks--extras'; a series is one hot spot.
     (when (and (not arc) (stringp (plist-get mark :href))) (list :href (plist-get mark :href)))
     (when (or area arc)
       (cl-loop for k in '(:strokeCap :strokeJoin)
                for v = (plist-get mark k) when (stringp v) append (list k v)))
     (when arc
       (append (when (vectorp (plist-get mark :strokeDash)) (list :strokeDash (plist-get mark :strokeDash)))
               (when (numberp (plist-get mark :cornerRadius)) (list :cornerRadius (plist-get mark :cornerRadius)))))
     (when (and area (eas-mark-style--visible-p (plist-get style :stroke)))
       (list :outline (if (numberp (plist-get mark :strokeWidth)) (plist-get mark :strokeWidth) 1))))))

(defun eas-mark-style-svg (node item)
  "NODE, the SVG path of area, line or arc ITEM, with ITEM's extra attributes.
Those are the presentation attributes NODE does not already carry."
  (let* ((outline (plist-get item :outline))
         (have (mapcar #'car (cadr node)))
         (extra
          (append
           (when (and outline (> outline 0))
             (list (cons 'stroke (eas-svg--escape (plist-get item :stroke)))
                   (cons 'stroke-width (eas-svg--n outline))))
           (cl-loop for (key attr) in '((:fillOpacity fill-opacity) (:strokeOpacity stroke-opacity)
                                        (:strokeDashOffset stroke-dashoffset) (:strokeMiterLimit stroke-miterlimit))
                    for v = (plist-get item key)
                    when (numberp v) collect (cons attr (eas-svg--n v)))
           (cl-loop for (key attr) in '((:strokeCap stroke-linecap) (:strokeJoin stroke-linejoin))
                    for v = (plist-get item key)
                    when (stringp v) collect (cons attr (eas-svg--escape v)))
           (when (vectorp (plist-get item :strokeDash))
             (list (cons 'stroke-dasharray (mapconcat #'eas-svg--n (plist-get item :strokeDash) ","))))
           (when (plist-get item :blend)
             (list (cons 'style (concat "mix-blend-mode:" (plist-get item :blend)))))))
         (extra (seq-remove (lambda (a) (memq (car a) have)) extra)))
    (if extra (cl-list* (car node) (append (cadr node) extra) (cddr node)) node)))

(declare-function eas-svg--n "eas-svg")
(declare-function eas-svg--escape "eas-svg")

(provide 'eas-mark-style)
;;; eas-mark-style.el ends here

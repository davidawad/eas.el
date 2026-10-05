;;; eas-paint.el --- gradient paints for mark fills -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega-Lite accepts a gradient wherever a mark takes a color:
;;   {"gradient": "linear", "x1": 0, "y1": 0, "x2": 1, "y2": 0,
;;    "stops": [{"offset": 0, "color": "white"}, ...]}
;; ("radial" adds r1 and r2), in the item's bounding box units.
;; Compile keeps the gradient on the item as :gradient and puts a solid
;; :fill beside it (the last stop), which is what the text backend and
;; legends use; the SVG renderer emits one <linearGradient> or
;; <radialGradient> per distinct gradient and fills with url(#id).

;;; Code:

(require 'eas-core)
(require 'dom)

(defun eas-paint-gradient-p (value)
  "Non-nil when VALUE is a Vega gradient object."
  (and (eas-object-p value) (stringp (plist-get value :gradient)) (vectorp (plist-get value :stops))))

(defun eas-paint-solid (gradient)
  "A single color standing in for GRADIENT: its last stop's."
  (let ((stops (plist-get gradient :stops)))
    (and (> (length stops) 0) (plist-get (aref stops (1- (length stops))) :color))))

(defun eas-paint-style (style)
  "STYLE (:fill :stroke ...) with gradient paints split into :gradient
\(fill) and a solid fallback color."
  (let ((fill (plist-get style :fill)) (stroke (plist-get style :stroke)))
    (when (eas-paint-gradient-p stroke)
      (setq style (plist-put (copy-sequence style) :stroke (eas-paint-solid stroke))))
    (if (eas-paint-gradient-p fill)
        (append (plist-put (copy-sequence style) :fill (eas-paint-solid fill)) (list :gradient fill))
      style)))

;;; SVG

(defvar eas-paint--svg-defs nil
  "Gradient definitions emitted while rendering one SVG document.")

(defun eas-paint--id (gradient)
  "Stable element id of GRADIENT."
  (format "paint-%x" (abs (sxhash-equal gradient))))

(defun eas-paint--svg-def (gradient id)
  "The SVG gradient element ID for GRADIENT."
  (let* ((radial (equal (plist-get gradient :gradient) "radial"))
         (num (lambda (k default) (let ((v (plist-get gradient k))) (if (numberp v) v default))))
         (attrs (if radial
                    `((id . ,id) (fx . ,(funcall num :x1 0.5)) (fy . ,(funcall num :y1 0.5))
                      (fr . ,(funcall num :r1 0)) (cx . ,(funcall num :x2 0.5)) (cy . ,(funcall num :y2 0.5))
                      (r . ,(funcall num :r2 0.5)))
                  `((id . ,id) (x1 . ,(funcall num :x1 0)) (y1 . ,(funcall num :y1 0))
                    (x2 . ,(funcall num :x2 1)) (y2 . ,(funcall num :y2 0))))))
    (apply #'dom-node (if radial 'radialGradient 'linearGradient)
           (mapcar (lambda (a) (cons (car a) (format "%s" (cdr a)))) attrs)
           (mapcar (lambda (s) (dom-node 'stop `((offset . ,(format "%s" (plist-get s :offset)))
                                                (stop-color . ,(plist-get s :color)))))
                   (plist-get gradient :stops)))))

(defun eas-paint-svg-fill (item)
  "SVG fill of ITEM: url(#id) for a gradient (recording its definition
in `eas-paint--svg-defs'), else its :fill."
  (if-let* ((gradient (plist-get item :gradient)))
      (let ((id (eas-paint--id gradient)))
        (unless (seq-some (lambda (d) (equal (dom-attr d 'id) id)) eas-paint--svg-defs)
          (setq eas-paint--svg-defs (append eas-paint--svg-defs (list (eas-paint--svg-def gradient id)))))
        (format "url(#%s)" id))
    (plist-get item :fill)))

(provide 'eas-paint)
;;; eas-paint.el ends here

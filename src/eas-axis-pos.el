;;; eas-axis-pos.el --- axis title placement and band position -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, after an axis is placed (`eas-layout-axis-place').
;; Vega-Lite axis properties that move parts of a placed axis:
;;
;;   titleX, titleY      the title's position from the axis origin (the
;;                       plot's left edge for x, its top for y; the
;;                       axis line's side across it), which turns off
;;                       Vega's automatic title layout on that axis
;;   titleAngle          the title's rotation; placed automatically, its
;;                       anchor still sits titlePadding past the labels
;;   titleAlign,         its alignment and baseline
;;   titleBaseline
;;   bandPosition        where ticks and grid lines sit in a band (0
;;                       its start, 0.5 the centre); labels stay at the
;;                       centre, as Vega draws them
;;   zindex              above 0, the svg renderer draws the axis over
;;                       the marks
;;
;; It also holds Vega-Lite's default alignment and baseline of angled
;; x labels (`eas-axis-pos-x-align', `eas-axis-pos-x-baseline').
;;
;; The axis bounds are recomputed around the moved title, so the
;; chrome follows it.  The character grid keeps its own titles.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(declare-function eas-layout-text-p "eas-layout")
(declare-function eas-layout-text-bounds "eas-layout")
(declare-function eas-layout-union "eas-layout")
(declare-function eas-layout--round "eas-layout")

(defconst eas-axis-pos-keys
  '((:titleX . :title-x) (:titleY . :title-y) (:titleAngle . :title-angle)
    (:titleAlign . :title-align) (:titleBaseline . :title-baseline) (:bandPosition . :band-position)
    (:zindex . :zindex) (:titlePadding . :title-padding))
  "Axis properties handled here and the model keys they become.")

(defun eas-axis-pos-props (get)
  "Model properties from GET, a function KEY -> the axis or config value."
  (cl-loop for (key . model) in eas-axis-pos-keys
           for v = (funcall get key)
           when (or (numberp v) (stringp v)) append (list model v)))

(defun eas-axis-pos-x-baseline (angle top)
  "Vega-Lite's default baseline of x labels at ANGLE (degrees).
TOP is non-nil for a top axis, nil for a bottom one."
  (let ((a (mod angle 360)))
    (cond ((or (< 45 a 135) (< 225 a 315)) "middle")
          ((eq (or (<= a 45) (<= 315 a)) (and top t)) "bottom")
          (t "top"))))

(defun eas-axis-pos-x-align (angle top)
  "Vega-Lite's default alignment of x labels at ANGLE (degrees).
TOP is non-nil for a top axis.  Nil for a multiple of 180 degrees,
where labels keep their own."
  (let ((a (mod angle 360)))
    (unless (zerop (mod a 180))
      (if (eq (< 0 a 180) (not top)) "left" "right"))))

(defun eas-axis-pos--title-p (axis)
  "Non-nil when AXIS moves its title."
  (seq-some (lambda (k) (plist-get axis k)) '(:title-x :title-y :title-angle :title-align :title-baseline)))

(defun eas-axis-pos--title (axis placed bounds metrics)
  "Title mark of AXIS placed against the untitled PLACED axis in plot BOUNDS."
  (let* ((orient (plist-get axis :orient))
         (horiz (member orient '("bottom" "top")))
         (x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3))
         (style (plist-get axis :style))
         (size (or (plist-get style :titleFontSize) (plist-get metrics :title-size)))
         (weight (or (plist-get style :titleFontWeight) (plist-get metrics :title-weight)))
         (tpad (or (plist-get axis :title-pad) (plist-get axis :title-padding) (plist-get metrics :title-pad)))
         (ab (plist-get placed :bounds))
         (angle (or (plist-get axis :title-angle)
                    (pcase orient ("left" -90) ("right" 90) (_ 0))))
         (align (or (plist-get axis :title-align) "center"))
         (baseline (or (plist-get axis :title-baseline)
                       (pcase orient ("bottom" "top") ("top" "bottom") (_ "bottom"))))
         ;; The axis group's origin: Vega translates each axis to its side.
         (ox (if (equal orient "right") (+ x0 w) x0))
         (oy (if (equal orient "bottom") (+ y0 h) y0))
         (tx (plist-get axis :title-x)) (ty (plist-get axis :title-y))
         (title (plist-get axis :title))
         (bounds-at (lambda (x y) (eas-layout-text-bounds metrics title size x y align baseline angle weight)))
         ;; Automatic: centred along the axis.
         (x (or (and tx (+ ox tx)) (if horiz (+ x0 (/ w 2.0)) ox)))
         (y (or (and ty (+ oy ty)) (if horiz oy (+ y0 (/ h 2.0))))))
    ;; Vega's axisTitleLayout: the anchor sits titlePadding past the labels.
    (unless (if horiz ty tx)
      (pcase orient
        ("bottom" (setq y (+ (aref ab 3) tpad)))
        ("top" (setq y (- (aref ab 1) tpad)))
        ("left" (setq x (- (aref ab 0) tpad)))
        (_ (setq x (+ (aref ab 2) tpad)))))
    (list :text title :x (+ x 0.5) :y (+ y 0.5) :align align :baseline baseline :angle angle
          :box (funcall bounds-at x y))))

(defun eas-axis-pos--band (axis scale)
  "Placed AXIS with ticks and grid at its bandPosition in band SCALE."
  (let ((b (plist-get axis :band-position)) (bw (plist-get scale :bandwidth))
        (horiz (member (plist-get axis :orient) '("bottom" "top"))))
    (if (not (and (numberp b) bw (equal (plist-get scale :type) "band") (not (plist-get axis :tick-band)))) axis
      (plist-put (copy-sequence axis) :ticks
                 (vconcat
                  (seq-map (lambda (tk)
                             (let ((s (eas-scale-apply scale (plist-get tk :value))))
                               (if (not (numberp s)) tk
                                 (let ((p (+ 0.5 (eas-layout--round (- (+ s (* b bw)) 0.5)))) (out (copy-sequence tk)))
                                   (dolist (k '(:tick :grid))
                                     (when-let* ((v (plist-get tk k)))
                                       (setq out (plist-put out k (if horiz (vector p (aref v 1) p (aref v 3))
                                                                    (vector (aref v 0) p (aref v 2) p))))))
                                   out))))
                           (plist-get axis :ticks)))))))

(defun eas-axis-pos-place (axis scale bounds metrics place)
  "AXIS placed by PLACE (a function of the axis), then moved for its
title and band position properties.  SCALE and BOUNDS are the axis's
scale and plot; METRICS the layout's."
  (if (or (eas-layout-text-p metrics)
          (not (or (eas-axis-pos--title-p axis) (plist-get axis :band-position))))
      (funcall place axis)
    (let* ((title (and (plist-get axis :title) (eas-axis-pos--title-p axis)))
           (placed (eas-axis-pos--band (funcall place (if title (plist-put (copy-sequence axis) :title nil) axis))
                                       scale)))
      (if (not title) placed
        (let ((tm (eas-axis-pos--title axis placed bounds metrics)))
          (append (eas--plist-without (eas--plist-without placed :title) :bounds)
                  (list :title (plist-get axis :title)
                        :bounds (eas-layout-union (plist-get placed :bounds) (plist-get tm :box))
                        :title-mark (eas--plist-without tm :box))))))))

(provide 'eas-axis-pos)
;;; eas-axis-pos.el ends here

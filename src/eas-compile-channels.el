;;; eas-compile-channels.el --- shape, angle and offset channels -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Scales for the channels a point mark reads besides
;; position, color, size and opacity:
;;
;;   angle            linear onto [0, 360] degrees, or scale.range
;;   xOffset/yOffset  linear onto one band of the x/y position scale,
;;                    or a band scale inside it when discrete
;;
;; A shape encoding the same field as color merges into the color
;; legend, whose entries then carry their shapes.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)
(require 'eas-compile-scales)
(require 'eas-symbols)
(require 'eas-offset)

(defun eas-compile-channels--field-pairs (units channel)
  "(UNIT . DEF) pairs of UNITS whose CHANNEL maps a field."
  (seq-filter (lambda (p) (plist-get (cdr p) :field)) (eas-compile--defs units channel)))

(defun eas-compile-channels--offset-pairs (units channel)
  "(UNIT . DEF) pairs of UNITS whose offset CHANNEL maps a field or datum."
  (seq-filter (lambda (p) (or (plist-get (cdr p) :field) (plist-member (cdr p) :datum)))
              (eas-compile--defs units channel)))

(defun eas-compile-channels-scales (units)
  "Plist of the angle and offset scales UNITS need."
  (append
   ;; A discrete offset is a band scale nested in its parent band.
   (cl-loop for ch in '(:xOffset :yOffset)
            for pairs = (eas-compile-channels--offset-pairs units ch)
            when (and pairs (eas-encode-discrete-p (cdar pairs)))
            append (list ch (plist-get (eas-offset-scales units) ch)))
   (cl-loop for ch in '(:xOffset :yOffset)
            for pairs = (eas-compile-channels--field-pairs units ch)
            when (and pairs (not (eas-encode-discrete-p (cdar pairs))))
            append (let ((nums (seq-filter #'numberp (eas-compile--values pairs ch))))
                     ;; Fractions of the band; marks scale them by its bandwidth.
                     (list ch (eas-scale-continuous "linear" (if nums (apply #'min nums) 0)
                                                    (if nums (apply #'max nums) 1) [0 1]
                                                    :field (plist-get (cdar pairs) :field)))))
   (when-let* ((pairs (eas-compile-channels--field-pairs units :angle)))
     (let* ((def (cdar pairs)) (sp (plist-get def :scale))
            (explicit (and (vectorp (plist-get sp :domain)) (plist-get sp :domain)))
            (nums (seq-filter #'numberp (eas-compile--values pairs :angle)))
            (range (or (and (vectorp (plist-get sp :range)) (plist-get sp :range)) [0 360])))
       (list :angle (eas-scale-continuous "linear"
                                          (if explicit (aref explicit 0) (if nums (apply #'min nums) 0))
                                          (if explicit (aref explicit 1) (if nums (apply #'max nums) 1))
                                          range :field (plist-get def :field)))))))

(defun eas-compile-channels-legend-shape (units)
  "The shape scale's field when it matches the color field of UNITS, else nil."
  (let ((shape (cdar (eas-compile-channels--field-pairs units :shape)))
        (color (cdar (eas-compile-channels--field-pairs units :color))))
    (and shape color (equal (plist-get shape :field) (plist-get color :field)))))

(provide 'eas-compile-channels)
;;; eas-compile-channels.el ends here

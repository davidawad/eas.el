;;; eas-bar-extra.el --- bar size and pixel offsets -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  `eas-marks' gives a bar or rect its span along x and
;; along y; this file applies the mark properties that edit a span:
;;
;;   width, height     on a band scale, a number is the bar's thickness
;;                     in pixels and {"band": F} that fraction of the
;;                     bandwidth, both centred in the band
;;   xOffset, yOffset  pixels added to the x (y) end; a bar filling a
;;                     band moves whole
;;   x2Offset,         pixels added to the x2 (y2) end, the zero
;;   y2Offset          baseline or the secondary channel's position
;;
;; as Vega-Lite compiles them.  Offsets are sub-cell in a terminal and
;; are left out there.  strokeDash, strokeCap and strokeJoin reach the
;; item for the svg renderer to outline the bar with.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)

(defconst eas-bar-extra--keys '(:width :height :xOffset :yOffset :x2Offset :y2Offset)
  "Mark properties that edit a bar's span.")

(defvar eas-bar-extra--last nil
  "(MARK SPANS-P . STROKE) for the mark seen last: every row of a unit
shares its mark, so the per-row hooks decide once per mark.")

(defun eas-bar-extra--of (mark)
  "(SPANS-P . STROKE) of MARK, from `eas-bar-extra--last' when it is MARK."
  (if (eq (car eas-bar-extra--last) mark) (cdr eas-bar-extra--last)
    (cdr (setq eas-bar-extra--last
               (cons mark (cons (and (seq-some (lambda (k) (plist-get mark k)) eas-bar-extra--keys) t)
                                (cl-loop for k in '(:strokeDash :strokeCap :strokeJoin)
                                         for v = (plist-get mark k)
                                         when (or (stringp v) (and (vectorp v) (> (length v) 0)))
                                         append (list k v))))))))

(defun eas-bar-extra-stroke (mark)
  "Item properties for MARK's strokeDash, strokeCap and strokeJoin, when set."
  (cdr (eas-bar-extra--of mark)))

(defun eas-bar-extra--number (mark key)
  "MARK's KEY when it is a number, else 0."
  (let ((v (plist-get mark key))) (if (numberp v) v 0)))

(defun eas-bar-extra--size (mark channel scale span)
  "SPAN resized by MARK's width (CHANNEL :x) or height on band SCALE."
  (let* ((v (plist-get mark (if (eq channel :x) :width :height)))
         (bw (plist-get scale :bandwidth))
         (size (cond ((numberp v) v)
                     ((and (eas-object-p v) (numberp (plist-get v :band)) bw) (* bw (plist-get v :band))))))
    (if (not (and size (equal (plist-get scale :type) "band"))) span
      (let ((c (/ (+ (car span) (cdr span)) 2.0)))
        (cons (- c (/ size 2.0)) (+ c (/ size 2.0)))))))

(defun eas-bar-extra-span (unit scales channel span row text)
  "SPAN (LO . HI) of ROW's bar in UNIT along CHANNEL with the mark's size
and offsets applied.  SCALES are the view's; TEXT is non-nil for the
character grid, where offsets do not apply."
  (if (or (null span) (not (car (eas-bar-extra--of (plist-get unit :mark))))) span
    (let* ((mark (plist-get unit :mark)) (scale (plist-get scales channel))
           (enc (plist-get unit :encoding))
           (ranged (plist-get enc (if (eq channel :x) :x2 :y2)))
           (span (if ranged span (eas-bar-extra--size mark channel scale span)))
           (o1 (if text 0 (eas-bar-extra--number mark (if (eq channel :x) :xOffset :yOffset))))
           (o2 (if text 0 (eas-bar-extra--number mark (if (eq channel :x) :x2Offset :y2Offset)))))
      (cond
       ((and (zerop o1) (zerop o2)) span)
       ;; A bar filling its band moves whole.
       ((and (not ranged) (equal (plist-get scale :type) "band"))
        (cons (+ (car span) o1) (+ (cdr span) o1)))
       (t
        (let* ((def (plist-get enc channel))
               (raw (and (eas-object-p def) (eas-encode-raw def row)))
               (p (and scale raw (eas-scale-apply scale raw)))
               (p (and (numberp p) (if (member (plist-get scale :type) '("band" "point"))
                                       (+ p (/ (or (plist-get scale :bandwidth) 0) 2.0))
                                     p)))
               ;; The end nearer the primary channel's position takes its offset.
               (first (or (null p) (<= (abs (- p (car span))) (abs (- p (cdr span))))))
               (a (+ (car span) (if first o1 o2))) (b (+ (cdr span) (if first o2 o1))))
          (cons (min a b) (max a b))))))))

(provide 'eas-bar-extra)
;;; eas-bar-extra.el ends here

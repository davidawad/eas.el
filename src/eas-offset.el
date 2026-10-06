;;; eas-offset.el --- xOffset and yOffset: bands nested in bands -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite's xOffset/yOffset channels place marks inside
;; their x/y band (grouped bars).  The offset channel gets its own band
;; scale (paddings scale.offsetBandPaddingInner/Outer, 0 by default)
;; whose range is the parent's bandwidth; the parent band pads by
;; bandWithNestedOffsetPaddingInner/Outer (0.2 each).  With the default
;; step sizing, the step (20px) belongs to the offset band, so a parent
;; step is 20 * n / (1 - 0.2) for n offset values.  A mark's position is
;; the parent band's start plus its offset, and its band is the offset's.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)

(defconst eas-offset-nested-padding 0.2
  "Vega-Lite's bandWithNestedOffsetPaddingInner and ...Outer.")

(defconst eas-offset-channels '((:x . :xOffset) (:y . :yOffset))
  "Position channels and their offset channels.")

(defun eas-offset-channel (channel)
  "The offset channel of position CHANNEL."
  (alist-get channel eas-offset-channels))

(defun eas-offset--defs (units channel)
  "(UNIT . DEF) pairs of UNITS whose offset CHANNEL maps a field or datum."
  (cl-loop for u in units
           for d = (plist-get (plist-get u :encoding) channel)
           when (and d (eas-object-p d) (or (plist-get d :field) (plist-member d :datum)))
           collect (cons u d)))

(defun eas-offset-nested-p (units channel)
  "Non-nil when UNITS nest an offset band in position CHANNEL's band."
  (and (eas-offset-channel channel) (eas-offset--defs units (eas-offset-channel channel)) t))

(defun eas-offset--domain (pairs)
  "Discrete domain of offset PAIRS, in data order sorted ascending."
  (let* ((def (cdar pairs)) (sort (plist-get def :sort)) (sp (plist-get def :scale)) values)
    (dolist (p pairs)
      (if (plist-member (cdr p) :datum) (push (plist-get (cdr p) :datum) values)
        (seq-doseq (row (plist-get (car p) :rows))
          (let ((v (eas-encode-raw (cdr p) row))) (unless (memq v '(nil :null)) (push v values))))))
    (setq values (delete-dups (nreverse values)))
    (vconcat (cond ((vectorp (plist-get sp :domain)) (plist-get sp :domain))
                   ((vectorp sort) (append sort (seq-remove (lambda (v) (seq-contains-p sort v)) values)))
                   ((or (eq sort :null) (seq-every-p (lambda (p) (plist-member (cdr p) :datum)) pairs)) values)
                   ((member (plist-get def :type) '("quantitative" "temporal")) (sort values #'<))
                   (t (sort values (lambda (a b) (if (and (numberp a) (numberp b)) (< a b)
                                                   (string< (format "%s" a) (format "%s" b))))))))))

(defun eas-offset-scales (units)
  "Offset band scales of UNITS as a plist (:xOffset S :yOffset S), ranges unset."
  (cl-loop for (_ . ch) in eas-offset-channels
           for pairs = (eas-offset--defs units ch)
           when pairs
           append (let* ((sp (plist-get (cdar pairs) :scale))
                         (inner (or (plist-get sp :paddingInner) (plist-get sp :padding) 0))
                         (outer (or (plist-get sp :paddingOuter) (plist-get sp :padding) 0)))
                    (list ch (append (eas-scale-band "band" (eas-offset--domain pairs) [0 1] inner outer)
                                     (list :padding-inner inner :padding-outer outer))))))

(defun eas-offset-set-ranges (scales)
  "SCALES with each offset band spanning its parent's bandwidth."
  (cl-loop for (pos . ch) in eas-offset-channels
           for s = (plist-get scales ch)
           when (and s (equal (plist-get s :type) "band"))
           do (let ((bw (or (plist-get (plist-get scales pos) :bandwidth) 0)))
                (setq scales (plist-put scales ch
                                        (append (eas-scale-band "band" (plist-get s :domain) (vector 0 bw)
                                                                (plist-get s :padding-inner) (plist-get s :padding-outer))
                                                (list :padding-inner (plist-get s :padding-inner)
                                                      :padding-outer (plist-get s :padding-outer)))))))
  scales)

(defun eas-offset-step (scales channel step)
  "Size per category of position CHANNEL's band when an offset band takes STEP.
SCALES are the view's scales.  Nil without an offset.  The parent's step
holds the offset band (bandspace(n, inner, outer) * STEP) as its
bandwidth, and the plot is bandspace(N, inner, outer) parent steps for N
categories."
  (let ((off (plist-get scales (eas-offset-channel channel)))
        (pos (plist-get scales channel)))
    (when (and off (equal (plist-get off :type) "band") (equal (plist-get pos :type) "band"))
      (let* ((n (length (plist-get off :domain)))
             (inner (or (plist-get pos :padding-inner) eas-offset-nested-padding))
             (outer (or (plist-get pos :padding-outer) (/ inner 2.0)))
             (parents (max 1 (length (plist-get pos :domain))))
             (pstep (/ (* step (max 1 (+ (- n (plist-get off :padding-inner)) (* 2 (plist-get off :padding-outer)))))
                       (- 1.0 inner))))
        (/ (* pstep (+ (- parents inner) (* 2 outer))) parents)))))

(defun eas-offset-shift (unit scales channel row)
  "\(SHIFT . BANDWIDTH) of ROW's offset along CHANNEL in UNIT, or nil.
SCALES are the view's scales."
  (let* ((och (eas-offset-channel channel))
         (s (and och (plist-get scales och)))
         (def (and s (plist-get (plist-get unit :encoding) och))))
    (when (and def (eas-object-p def))
      (let ((v (if (plist-member def :datum) (plist-get def :datum) (eas-encode-raw def row))))
        (cons (or (eas-scale-apply s v) 0) (plist-get s :bandwidth))))))

(defun eas-offset-keys (unit row)
  "Values of ROW's offset fields in UNIT, which split a stack's groups."
  (let ((enc (plist-get unit :encoding)))
    (cl-loop for (_ . ch) in eas-offset-channels
             for d = (plist-get enc ch)
             when (and d (eas-object-p d) (plist-get d :field))
             collect (eas-encode-raw d row))))

(provide 'eas-offset)
;;; eas-offset.el ends here

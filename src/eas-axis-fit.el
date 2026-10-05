;;; eas-axis-fit.el --- discrete axis labels fitted to a resized plot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (fc-qx1.43).  Vega-Lite keeps every label of a nominal
;; axis, however crowded; at a spec's own size that is what eas draws
;; too.  A chart fitted to a size it was not designed for (an Emacs
;; window, the gallery's 320x200) can leave a horizontal discrete x axis
;; with labels wider than their step, colliding.  There each label is
;; cut to its step with an ellipsis, as labelLimit cuts it, so every
;; category keeps a readable label.  Thinned axes (labelOverlap) and
;; rotated labels are left alone, and so is the text target, which lays
;; its labels out on the character grid.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(declare-function eas-place--local-scale "eas-compile-place")

(defconst eas-axis-fit-gap 4
  "Pixels kept clear between neighbouring fitted labels.")

(defun eas-axis-fit--size (axis metrics)
  "Font size of AXIS's labels under METRICS."
  (or (plist-get (plist-get axis :style) :labelFontSize) (plist-get metrics :label-size)))

(defun eas-axis-fit--collide-p (axis step metrics)
  "Non-nil when neighbouring labels of AXIS, STEP apart, collide."
  (let ((size (eas-axis-fit--size axis metrics))
        (labels (seq-remove #'string-empty-p (mapcar (lambda (tk) (plist-get tk :label)) (plist-get axis :ticks)))))
    (cl-loop for (a b) on labels while b
             thereis (> (+ (/ (+ (eas-layout-text-width metrics a size) (eas-layout-text-width metrics b size)) 2.0)
                           eas-axis-fit-gap)
                        step))))

(defun eas-axis-fit-overlap (before after fitted)
  "AFTER, the axes `eas-axis-fit-labels' made of BEFORE, with default label
overlap thinned when FITTED.  An axis it left alone (a continuous or
vertical axis cannot cut labels to a step) whose spec leaves labelOverlap
unset (:overlap-set is nil) thins its labels with Vega's \"parity\"
strategy, as continuous axes already do (fc-qx1.38)."
  (if (not fitted) after
    (cl-mapcar (lambda (old axis)
                 (if (or (not (eq old axis)) (plist-get axis :overlap) (plist-get axis :overlap-set))
                     axis
                   (plist-put (copy-sequence axis) :overlap "parity")))
               before after)))

(defun eas-axis-fit-labels (axes group metrics)
  "AXES of fitted GROUP with crowded discrete x labels cut to their step."
  (if (or (eas-layout-text-p metrics) (null (plist-get group :fit-height))) axes
    (mapcar
     (lambda (axis)
       (let ((step (and (equal (plist-get axis :channel) "x") (eq (plist-get axis :discrete) t)
                        (null (plist-get axis :overlap)) (eql (plist-get axis :labelAngle) 0)
                        (plist-get (eas-place--local-scale group :x) :step))))
         (if (not (and step (eas-axis-fit--collide-p axis step metrics))) axis
           (let ((size (eas-axis-fit--size axis metrics)) (limit (- step eas-axis-fit-gap)))
             (plist-put (copy-sequence axis) :ticks
                        (vconcat (mapcar (lambda (tk)
                                           (let ((label (string-remove-suffix "…" (plist-get tk :label))))
                                             (plist-put (copy-sequence tk) :label
                                                        (eas-layout-truncate metrics label size limit))))
                                         (plist-get axis :ticks))))))))
     axes)))

(provide 'eas-axis-fit)
;;; eas-axis-fit.el ends here

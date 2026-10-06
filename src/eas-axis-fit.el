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
;; its labels out on the character grid: there `eas-axis-fit-text'
;; blanks a label that would print over, or run into, one before it.

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
  "Return non-nil when neighbouring labels of AXIS, STEP apart, collide.
METRICS measures the labels."
  (let ((size (eas-axis-fit--size axis metrics))
        (labels (seq-remove #'string-empty-p (mapcar (lambda (tk) (plist-get tk :label)) (plist-get axis :ticks)))))
    (cl-loop for (a b) on labels while b
             thereis (> (+ (/ (+ (eas-layout-text-width metrics a size) (eas-layout-text-width metrics b size)) 2.0)
                           eas-axis-fit-gap)
                        step))))

(defun eas-axis-fit-overlap (before after fitted)
  "Return AFTER with default label overlap thinned when FITTED.
AFTER is the axes `eas-axis-fit-labels' made of BEFORE.  An axis it
left alone (a continuous or vertical axis cannot cut labels to a step)
whose spec leaves labelOverlap unset (:overlap-set is nil) thins its
labels with Vega's \"parity\" strategy, as continuous axes already do
\(fc-qx1.38)."

  (if (not fitted) after
    (cl-mapcar (lambda (old axis)
                 (if (or (not (eq old axis)) (plist-get axis :overlap) (plist-get axis :overlap-set))
                     axis
                   (plist-put (copy-sequence axis) :overlap "parity")))
               before after)))

(defun eas-axis-fit-labels (axes group metrics)
  "AXES of fitted GROUP with crowded discrete x labels cut to their step.
METRICS measures the labels; under text METRICS AXES stay as they are."
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

;;; Text: labels a cell apart

(defun eas-axis-fit--text-spans (tk cols cw ch)
  "Cell spans (ROW START . END) of tick TK's label lines on a grid COLS wide.
As the text renderer places them: a label running off a side is moved in.
CW and CH are the cell width and height in pixels."
  (seq-map-indexed
   (lambda (line i)
     (let* ((len (string-width line)) (x (/ (plist-get tk :lx) (float cw)))
            (start (pcase (plist-get tk :align) ("left" (round x)) ("right" (- (round x) len))
                          (_ (round (- x (/ len 2.0))))))
            (start (if (<= len cols) (max 0 (min start (- cols len))) start)))
       (cons (+ i (floor (/ (plist-get tk :ly) (float ch)))) (cons start (+ start len)))))
   (split-string (plist-get tk :label) "\n")))

(defun eas-axis-fit--text-clash-p (spans kept)
  "Non-nil when a span of SPANS overlaps or touches a span of KEPT on its row."
  (seq-some (lambda (a) (seq-some (lambda (b) (and (= (car a) (car b))
                                                    (< (cadr a) (1+ (cddr b))) (< (cadr b) (1+ (cddr a)))))
                                  kept))
            spans))

(defun eas-axis-fit-text (view width metrics)
  "Return VIEW with its axes' tick labels thinned on a text canvas.
The canvas is WIDTH pixels wide under METRICS; return VIEW itself when
METRICS is not text.

Labels the renderer would print over, or run into, one kept before
them are blanked (:full keeps the text), so a squeezed plot reads
\"0  1,000\" as one label at most, never \"01,000\", and the scene says
what the terminal shows.  An axis whose spec sets labelOverlap false
keeps every label (fc-qx1.52)."
  (if (not (and (eas-layout-text-p metrics) (plist-get view :axes)))
      view
    (let* ((cw (aref (plist-get metrics :cell) 0)) (ch (aref (plist-get metrics :cell) 1))
           (cols (round (/ (float width) cw))))
      (eas-plist-put
       view :axes
       (vconcat
        (mapcar (lambda (axis)
                  (if (plist-get axis :label-overlap-off) axis
                    (let (kept)
                      (eas-plist-put
                       axis :ticks
                       (vconcat
                        (mapcar (lambda (tk)
                                  (let ((label (plist-get tk :label)))
                                    (if (not (and (stringp label) (not (string-empty-p label))
                                                  (numberp (plist-get tk :lx)) (numberp (plist-get tk :ly))))
                                        tk
                                      (let ((spans (eas-axis-fit--text-spans tk cols cw ch)))
                                        (if (eas-axis-fit--text-clash-p spans kept)
                                            (eas-plist-put (eas-plist-put tk :label "") :full label)
                                          (setq kept (append spans kept))
                                          tk)))))
                                (plist-get axis :ticks)))))))
                (plist-get view :axes)))))))

(provide 'eas-axis-fit)
;;; eas-axis-fit.el ends here

;;; eas-zoom.el --- zoom and pan: scale domains moved in range space -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.3).  An interval selection with bind "scales"
;; edits scale domains, never bitmaps.  The arithmetic is done in range
;; (pixel) space and mapped back through `eas-scale-invert', so one
;; rule serves linear, time and log scales and reversed ranges: the
;; datum under the anchor pixel stays under it, and a pan by N pixels
;; moves the picture by N pixels.  Domains that would collapse, overflow
;; or leave a log scale's positive half are refused (nil), so repeated
;; zooming saturates instead of breaking the scale.
;;
;; Native input that is not a plain wheel click becomes a wheel delta
;; here too: precision (trackpad) scrolling scales by its pixel delta,
;; and a pinch gesture by the change in its finger distance.

;;; Code:

(require 'eas-scale)

(defconst eas-zoom-min-span 1e-9
  "Smallest domain span kept, relative to the domain's magnitude.")

(defconst eas-zoom-max-magnitude 1e300
  "Largest domain bound kept.")

(defconst eas-zoom-wheel-pixels 40.0
  "Precision-scroll pixels that count as one wheel step.")

(defconst eas-zoom-wheel-step 1.2
  "Domain scale factor per wheel step.")

(defun eas-zoom--domain (scale p0 p1)
  "SCALE's domain between range pixels P0 and P1, or nil when degenerate."
  (let ((lo (eas-scale-invert scale p0)) (hi (eas-scale-invert scale p1)))
    (when (and (numberp lo) (numberp hi)
               (not (isnan lo)) (not (isnan hi))
               (< (abs lo) eas-zoom-max-magnitude) (< (abs hi) eas-zoom-max-magnitude)
               (or (not (equal (plist-get scale :type) "log")) (and (> lo 0) (> hi 0)))
               (> (abs (- hi lo)) (* eas-zoom-min-span (max 1.0 (abs lo) (abs hi)))))
      (vector lo hi))))

(defun eas-zoom-domain (scale factor &optional anchor)
  "Domain of continuous SCALE scaled by FACTOR about range pixel ANCHOR.
FACTOR < 1 zooms in.  ANCHOR defaults to the range's centre.  Returns
nil when the result would be degenerate."
  (let* ((r (plist-get scale :range)) (r0 (aref r 0)) (r1 (aref r 1))
         (a (or anchor (/ (+ r0 r1) 2.0))))
    (eas-zoom--domain scale (+ a (* factor (- r0 a))) (+ a (* factor (- r1 a))))))

(defun eas-zoom-pan-domain (scale pixels)
  "Domain of SCALE after the picture moves PIXELS along its range.
Positive PIXELS drag the content toward larger pixel values, which
brings data from the low end of the range into view."
  (let ((r (plist-get scale :range)))
    (eas-zoom--domain scale (- (aref r 0) pixels) (- (aref r 1) pixels))))

(defun eas-zoom-step-domain (scale fraction)
  "Domain of SCALE moved FRACTION of its range toward the range's end.
On x that is right (later data); on a bottom-up y range it is up."
  (let ((r (plist-get scale :range)))
    (eas-zoom-pan-domain scale (- (* fraction (- (aref r 1) (aref r 0)))))))

;;; Native input -> wheel delta

(defun eas-zoom-wheel-delta (event)
  "Wheel steps of mouse wheel EVENT; negative zooms in.
A precision-scroll EVENT carrying a pixel delta (Emacs 29+, trackpads)
gives a fractional step proportional to it; otherwise one step."
  (let* ((up (memq (event-basic-type event) '(wheel-up mouse-4)))
         (pixels (nth 4 event))
         (dy (and (consp pixels) (numberp (cdr pixels)) (abs (cdr pixels)))))
    (* (if up -1 1)
       (if (and dy (> dy 0)) (/ dy eas-zoom-wheel-pixels) 1))))

(defun eas-zoom-pinch-delta (scale previous)
  "Wheel steps for a pinch whose finger-distance ratio went from PREVIOUS
to SCALE (both relative to the gesture's start).  Spreading the fingers
\(SCALE > PREVIOUS) zooms in."
  (if (and (numberp scale) (numberp previous) (> scale 0) (> previous 0))
      (/ (log (/ previous (float scale))) (log eas-zoom-wheel-step))
    0))

(provide 'eas-zoom)
;;; eas-zoom.el ends here

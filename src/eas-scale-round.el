;;; eas-scale-round.el --- Vega's round for band and point scales -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A band or point scale with "round": true snaps to whole pixels as
;; Vega's band scale does: the step is floored, the leftover range is
;; split by the alignment (0.5) and the start and bandwidth rounded, so
;; every band or point sits on an integer pixel.

;;; Code:

(require 'eas-core)

(defun eas-scale-round-band (scale)
  "SCALE (band or point, its range set) snapped as by Vega's round.
Unchanged unless SCALE's :round is t."
  (if (not (and (eq (plist-get scale :round) t) (numberp (plist-get scale :step))
                (> (plist-get scale :step) 0)))
      scale
    (let* ((point (equal (plist-get scale :type) "point"))
           (n (length (plist-get scale :domain)))
           (range (plist-get scale :range))
           (lo (float (min (aref range 0) (aref range 1))))
           (hi (float (max (aref range 0) (aref range 1))))
           (inner (if point 1.0 (- 1 (/ (plist-get scale :bandwidth) (plist-get scale :step)))))
           (step (float (floor (plist-get scale :step))))
           (start (float (round (+ lo (* 0.5 (- hi lo (* step (- n inner))))))))
           (out (copy-sequence scale)))
      (setq out (plist-put out :step step))
      (setq out (plist-put out :start start))
      (plist-put out :bandwidth (if point 0.0 (float (round (* step (- 1 inner)))))))))

(provide 'eas-scale-round)
;;; eas-scale-round.el ends here

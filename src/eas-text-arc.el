;;; eas-text-arc.el --- arcs as braille sector fills in text charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, terminal half (fc-qx1.49).  A wedge is filled at braille
;; resolution (2x4 dots per cell), so a donut keeps its hole and a pie
;; its round edge even in a small terminal.  Each wedge leaves its
;; starting edge one dot wide unfilled: neighbouring wedges stay apart
;; without color.  A full ring has no edge to leave.

;;; Code:

(require 'eas-core)
(require 'eas-arc)

(defun eas-text-arc-dots (item cw ch fn)
  "Call FN with (DX DY), the braille dot coordinates of each dot whose
centre lies inside arc ITEM, for cells of CW x CH pixels."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (r (or (plist-get item :outerRadius) 0))
         (sx (/ cw 2.0)) (sy (/ ch 4.0))
         (angles (eas-arc-angles item))
         (start (min (car angles) (cdr angles)))
         (full (>= (abs (- (cdr angles) (car angles))) (- (* 2 float-pi) 1e-6)))
         (gap (/ (min sx sy) 1.0)))
    (when (and cx cy (> r 0))
      (cl-loop for dy from (floor (- cy r) sy) to (ceiling (+ cy r) sy)
               for py = (* (+ dy 0.5) sy)
               do (cl-loop for dx from (floor (- cx r) sx) to (ceiling (+ cx r) sx)
                           for px = (* (+ dx 0.5) sx)
                           when (and (eas-arc-contains-p item px py)
                                     (or full
                                         ;; Distance from the starting edge's ray, ahead of it.
                                         (let* ((ux (sin start)) (uy (- (cos start)))
                                                (vx (- px cx)) (vy (- py cy))
                                                (along (+ (* vx ux) (* vy uy)))
                                                (across (abs (- (* vx uy) (* vy ux)))))
                                           (or (<= along 0) (>= across gap)))))
                           do (funcall fn dx dy))))))

(provide 'eas-text-arc)
;;; eas-text-arc.el ends here

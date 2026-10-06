;;; eas-text-arc.el --- arcs as braille sector fills in text charts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, terminal half (fc-qx1.49).  A wedge is filled at braille
;; resolution (2x4 dots per cell), so a donut keeps its hole and a pie
;; its round edge even in a small terminal.  Wedges meet without a gap
;; (fc-qx1.51): the text renderer gives a cell inside the arc a full
;; block in the color of the wedge holding most of its dots, and keeps
;; braille only where the arc's own edge cuts a cell.

;;; Code:

(require 'eas-core)
(require 'eas-arc)

(defun eas-text-arc-dots (item cw ch fn)
  "Call FN with (DX DY), the braille dot coordinates of each dot whose
centre lies inside arc ITEM, for cells of CW x CH pixels."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (r (or (plist-get item :outerRadius) 0))
         (sx (/ cw 2.0)) (sy (/ ch 4.0)))
    (when (and cx cy (> r 0))
      (cl-loop for dy from (floor (- cy r) sy) to (ceiling (+ cy r) sy)
               for py = (* (+ dy 0.5) sy)
               do (cl-loop for dx from (floor (- cx r) sx) to (ceiling (+ cx r) sx)
                           for px = (* (+ dx 0.5) sx)
                           when (eas-arc-contains-p item px py)
                           do (funcall fn dx dy))))))

(provide 'eas-text-arc)
;;; eas-text-arc.el ends here

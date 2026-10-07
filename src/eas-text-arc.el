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

(defvar eas-text-arc--memo (make-hash-table :test 'equal)
  "(ITEM CW CH) -> the dots of `eas-text-arc-dots', a vector [DX DY ...].
A clock's face repaints every tick behind its hands: an arc seen before
replays its dots instead of testing every dot of its box with float
arithmetic (eas-b2s.5).")

(defvar eas-text-arc--memo-max 1024 "Arcs `eas-text-arc--memo' holds before it starts afresh.")

(defun eas-text-arc-dots (item cw ch fn)
  "Call FN with (DX DY) for each braille dot centred inside arc ITEM.
DX DY are the braille dot coordinates, for cells of CW x CH pixels."
  (let* ((key (list item cw ch))
         (dots (or (gethash key eas-text-arc--memo)
                   (progn
                     (when (>= (hash-table-count eas-text-arc--memo) eas-text-arc--memo-max)
                       (clrhash eas-text-arc--memo))
                     (puthash key (eas-text-arc--dots item cw ch) eas-text-arc--memo))))
         (i 0) (n (length dots)))
    (while (< i n)
      (funcall fn (aref dots i) (aref dots (1+ i)))
      (setq i (+ i 2)))))

(defun eas-text-arc--dots (item cw ch)
  "The braille dots inside arc ITEM on CW x CH cells, a vector [DX DY ...]."
  (let* ((cx (plist-get item :cx)) (cy (plist-get item :cy))
         (r (or (plist-get item :outerRadius) 0))
         (sx (/ cw 2.0)) (sy (/ ch 4.0))
         (dots nil))
    (when (and cx cy (> r 0))
      (cl-loop for dy from (floor (- cy r) sy) to (ceiling (+ cy r) sy)
               for py = (* (+ dy 0.5) sy)
               do (cl-loop for dx from (floor (- cx r) sx) to (ceiling (+ cx r) sx)
                           for px = (* (+ dx 0.5) sx)
                           when (eas-arc-contains-p item px py)
                           do (push dx dots) (push dy dots))))
    (vconcat (nreverse dots))))

(provide 'eas-text-arc)
;;; eas-text-arc.el ends here

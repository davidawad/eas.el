;;; eas-text-labels.el --- text marks that would print over each other -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, text target.  A cell holds one character, so two text
;; marks whose cells meet print over each other ("Hi3gh" for "High"
;; and "3"), which is worse than either alone.  On a small canvas
;; (a grid of tiny charts) annotations collide often.
;; `eas-text-labels-thin' keeps each text item whose cells clear, by a
;; cell, every item kept before it, in drawing order (earlier marks,
;; then earlier items, win: a later layer's label yields to the one it
;; would hide).  A dropped item stays in the scene, so datum refs keep
;; their indices, with opacity 0 (nothing to draw) and :dropped
;; "overlap", which says why.  The svg target keeps every label.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(defun eas-text-labels--spans (item cw ch)
  "The (ROW START . END) cells of text ITEM's lines on CW x CH cells.
The anchoring is the text renderer's: ALIGN left, right or centred
on the item's x, one row per line from its y."
  (let ((text (plist-get item :text)) (x (plist-get item :x)) (y (plist-get item :y)))
    (when (and (stringp text) (numberp x) (numberp y))
      (cl-loop for line in (split-string text "\n")
               for i from 0
               for len = (string-width line)
               for c = (round (/ x (float cw)))
               for start = (pcase (plist-get item :align)
                             ("left" c) ("right" (- c len))
                             (_ (round (- (/ x (float cw)) (/ len 2.0)))))
               unless (zerop len)
               collect (cons (+ (floor (/ y (float ch))) i) (cons start (+ start len)))))))

(defun eas-text-labels--clash-p (spans kept)
  "Non-nil when one of SPANS touches one of KEPT (same row, a cell apart).
Plain loops: this runs per text item per frame, and a closure per span
was most of a ladder push's allocation (eas-b2s.5)."
  (let ((clash nil))
    (while (and spans (not clash))
      (let ((s (car spans)) (ks kept))
        (while (and ks (not clash))
          (let ((k (car ks)))
            (when (and (= (car s) (car k)) (< (cadr s) (1+ (cddr k))) (< (cadr k) (1+ (cddr s))))
              (setq clash t)))
          (setq ks (cdr ks))))
      (setq spans (cdr spans)))
    clash))

(defun eas-text-labels-thin (view metrics)
  "VIEW with text items that would print over a kept one hidden.
METRICS give the cells; VIEW comes back as it is unless they are text."
  (if (not (eas-layout-text-p metrics))
      view
    (let* ((cw (aref (plist-get metrics :cell) 0)) (ch (aref (plist-get metrics :cell) 1))
           (kept nil)
           (marks (mapcar
                   (lambda (mark)
                     (if (not (equal (plist-get mark :mark) "text"))
                         mark
                       (let* ((dropped nil)
                              (items (vconcat
                                      (mapcar (lambda (item)
                                                (let ((spans (and (not (equal (plist-get item :opacity) 0))
                                                                  (eas-text-labels--spans item cw ch))))
                                                  (cond ((null spans) item)
                                                        ((eas-text-labels--clash-p spans kept)
                                                         (setq dropped t)
                                                         (append (list :opacity 0 :dropped "overlap")
                                                                 (eas--plist-without item :opacity)))
                                                        (t (setq kept (append spans kept)) item))))
                                              (plist-get mark :items)))))
                         (if dropped (eas-plist-put mark :items items) mark))))
                   (append (plist-get view :marks) nil))))
      (eas-plist-put view :marks (vconcat marks)))))

(provide 'eas-text-labels)
;;; eas-text-labels.el ends here

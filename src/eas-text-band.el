;;; eas-text-band.el --- cells shared by stacked area slices -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5, terminal half (fc-qx1.51).  A cell holds one glyph, but
;; the slices of a stacked area (or any areas that tile a column) can
;; meet inside it.  Drawn one slice at a time, the cell keeps whichever
;; glyph won, and an eighth block's empty top shows the terminal's
;; background: a hole in the stack.  The text renderer records every
;; slice's extent per cell and, once the mark is drawn,
;; `eas-text-band-resolve' composes the cells the slices tile: the
;; slice reaching the cell's floor as an eighth block, the slice above
;; it as the block's background.

;;; Code:

(require 'eas-core)
(require 'eas-glyph)

(defun eas-text-band--tiled-p (segs y0 y1 slack)
  "Non-nil when SEGS ((TOP BOTTOM ...) ...) cover Y0..Y1 without a hole.
No gap may be wider than SLACK pixels: slices of series sampled at
different x meet unevenly, and a gap under a quarter cell reads as a
seam, not a hole."
  (let ((reach y0))
    (dolist (s (sort (copy-sequence segs) (lambda (a b) (< (car a) (car b)))))
      (when (<= (car s) (+ reach slack)) (setq reach (max reach (cadr s)))))
    (>= reach (- y1 slack))))

(defun eas-text-band--at (segs y slack)
  "Return the last drawn of SEGS covering pixel row Y.
Else return the nearest within SLACK pixels (SEGS are newest first)."
  (or (seq-find (lambda (s) (and (<= (car s) y) (< y (cadr s)))) segs)
      (car (sort (seq-filter (lambda (s) (< (max (- (car s) y) (- y (cadr s))) slack)) segs)
                 (lambda (a b) (< (max (- (car a) y) (- y (cadr a))) (max (- (car b) y) (- y (cadr b)))))))))

(defun eas-text-band-resolve (segs y0 ch)
  "Glyph for a cell from Y0, CH pixels high, that area slices SEGS share.
Nil when they do not tile it.  SEGS are (TOP BOTTOM PROPS),
newest first, and may reach past the cell.
Return (CHAR PROPS UNDER), UNDER the props whose color backs CHAR, or nil."
  (let ((y1 (+ y0 ch)) (slack (/ ch 4.0)))
    (when (and (cdr segs) (eas-text-band--tiled-p segs y0 y1 slack))
      (let* ((lower (eas-text-band--at segs (- y1 0.01) slack))
             (top (max y0 (car lower)))
             (n (round (* 8 (/ (- y1 top) (float ch)))))
             (upper (and (< 0 n 8) (eas-text-band--at segs (- top 0.01) slack))))
        (cond ((>= n 8) (list ?█ (nth 2 lower) nil))
              ((or (<= n 0) (null upper))
               (list ?█ (nth 2 (or upper (eas-text-band--at segs y0 slack))) nil))
              (t (list (eas-glyph-lower n) (nth 2 lower) (nth 2 upper))))))))

(provide 'eas-text-band)
;;; eas-text-band.el ends here

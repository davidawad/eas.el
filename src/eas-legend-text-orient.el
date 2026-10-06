;;; eas-legend-text-orient.el --- text legends above, below and left of the plot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (text).  The character grid honours a view legend's
;; orient as the svg target does (eas-legend-orient.el), on cells:
;;
;;   top     above the plot and any top axis, from the plot's left edge
;;   bottom  one row below the bottom axis, from the plot's left edge
;;   left    left of the y axis, two cells from it, from the plot's top
;;   right   (and every other orient) stacked right of the plot
;;
;; A symbol legend on top or bottom runs in a row: its title on a line
;; of its own, then its entries side by side, two cells apart, wrapped
;; at the plot's width.  Legends sharing the top or bottom sit one
;; below the other; left legends stack.  Compile reserves the chrome,
;; so the renderer only draws what the scene places.  The shared
;; legends of a composition (eas-compile-shared.el) stay on the right.

;;; Code:

(require 'eas-core)
(require 'eas-layout)

(declare-function eas-legend-size "eas-legend")

(defun eas-legend-text-orient (legend)
  "The side of the plot LEGEND sits on in text: top, bottom, left or right."
  (let ((orient (plist-get legend :orient)))
    (if (member orient '("top" "bottom" "left")) orient "right")))

(defun eas-legend-text-orient-row-p (legend)
  "Non-nil when text LEGEND lays its entries out in a row."
  (and (member (eas-legend-text-orient legend) '("top" "bottom"))
       (equal (plist-get legend :type) "symbol")
       (not (equal (plist-get legend :direction) "vertical"))))

(defun eas-legend-text-orient--entry-w (_legend e metrics)
  "Width of a row legend's entry E (symbol, gap, label) under METRICS."
  (+ (plist-get metrics :symbol) (plist-get metrics :char-w)
     (eas-layout-text-width metrics (plist-get e :label) (plist-get metrics :label-size))))

(defun eas-legend-text-orient--rows (legend limit metrics)
  "LEGEND's entries in rows no wider than LIMIT: a list of (ENTRY . X) lists.
METRICS give the cell sizes."
  (let ((pad (* 2 (plist-get metrics :char-w))) (x 0) row rows)
    (dolist (e (append (plist-get legend :entries) nil))
      (let ((w (eas-legend-text-orient--entry-w legend e metrics)))
        (when (and row (> (+ x w) limit))
          (push (nreverse row) rows)
          (setq row nil x 0))
        (push (cons e x) row)
        (setq x (+ x w pad))))
    (when row (push (nreverse row) rows))
    (nreverse rows)))

(defun eas-legend-text-orient-row-size (legend limit metrics)
  "Return (WIDTH . HEIGHT) of row LEGEND wrapped at LIMIT under METRICS."
  (let* ((rows (eas-legend-text-orient--rows legend limit metrics))
         (title-h (if (plist-get legend :title) (plist-get metrics :title-size) 0))
         (title-w (if (plist-get legend :title)
                      (eas-layout-text-width metrics (plist-get legend :title) (plist-get metrics :title-size))
                    0))
         (width (apply #'max title-w
                       (mapcar (lambda (row) (let ((last (car (last row))))
                                               (+ (cdr last) (eas-legend-text-orient--entry-w legend (car last) metrics))))
                               rows))))
    (cons width (+ title-h (* (max 1 (length rows)) (plist-get metrics :row))))))

(defun eas-legend-text-orient-place-row (legend x y metrics)
  "Row LEGEND on the character grid with its top-left corner at X Y.
It wraps at its :wrap-width (the plot's).  METRICS are the layout's."
  (let* ((sym (plist-get metrics :symbol)) (row-h (plist-get metrics :row))
         (gap (plist-get metrics :char-w))
         (title-h (if (plist-get legend :title) (plist-get metrics :title-size) 0))
         (limit (or (plist-get legend :wrap-width) 1.0e9))
         (size (eas-legend-text-orient-row-size legend limit metrics)))
    (append (eas--plist-without legend :entries)
            (list :x x :y y :width (car size))
            (when (plist-get legend :title)
              (list :title-mark (list :text (plist-get legend :title) :x x :y y :align "left" :baseline "top")))
            (list :entries
                  (vconcat
                   (cl-loop for row in (eas-legend-text-orient--rows legend limit metrics)
                            for i from 0
                            append (cl-loop for (e . ex) in row
                                            for top = (+ y title-h (* i row-h))
                                            for cy = (+ top (/ row-h 2.0))
                                            collect (append e (list :sx (+ x ex (/ sym 2.0)) :sy cy
                                                                    :lx (+ x ex sym gap) :ly cy
                                                                    :bounds (vector (+ x ex) top
                                                                                    (eas-legend-text-orient--entry-w legend e metrics)
                                                                                    row-h))))))))))

(defun eas-legend-text-orient-flow (legends w h chrome limit metrics)
  "Place text LEGENDS around a W by H plot whose axes take CHROME.
Right legends stack from the plot's top-right, a column that would
end below LIMIT (a height, or nil) starting another to its right.
Return (LEGENDS OFFSETS CHROME LEGEND-H): the legends (row ones
knowing their wrap width), their offsets from the plot origin in
order, the chrome grown to hold them and the height of the right and
left legends.  METRICS give the cells."
  (let* ((cw (plist-get metrics :char-w)) (row (plist-get metrics :row))
         (legends (mapcar (lambda (l) (if (eas-legend-text-orient-row-p l)
                                          (plist-put (copy-sequence l) :wrap-width (max w (* 8 cw)))
                                        l))
                          legends))
         (size (lambda (l) (if (eas-legend-text-orient-row-p l)
                               (eas-legend-text-orient-row-size l (plist-get l :wrap-width) metrics)
                             (eas-legend-size l metrics))))
         (top 0) (bottom 0) (left-y 0) (left-w 0)
         (rx 0) (ry 0) (col-w 0) (tallest 0)
         (chrome (copy-sequence chrome))
         offsets)
    ;; Top legends stack upwards from the top axis: measure them first.
    (let ((top-h (apply #'+ (mapcar (lambda (l) (cdr (funcall size l)))
                                    (seq-filter (lambda (l) (equal (eas-legend-text-orient l) "top")) legends)))))
      (dolist (l legends)
        (let ((s (funcall size l)))
          (pcase (eas-legend-text-orient l)
            ("top"
             (push (cons 0 (- top (+ (plist-get chrome :top) top-h))) offsets)
             (setq top (+ top (cdr s))))
            ("bottom"
             (push (cons 0 (+ h (plist-get chrome :bottom) row bottom)) offsets)
             (setq bottom (+ bottom (cdr s))))
            ("left"
             ;; Its width counts the offset, which falls between it and the axis.
             (push (cons 'left left-y) offsets)
             (setq left-y (+ left-y (cdr s)) left-w (max left-w (car s))))
            (_
             (when (and limit (> ry 0) (> (+ ry (cdr s)) limit))
               (setq rx (+ rx col-w) ry 0 col-w 0))
             (push (cons (+ w (plist-get metrics :legend-offset) rx) ry) offsets)
             (setq ry (+ ry (cdr s)) col-w (max col-w (car s)) tallest (max tallest ry))))))
      (let ((left-x (- (+ (plist-get chrome :left) left-w))))
        (setq offsets (mapcar (lambda (o) (if (eq (car o) 'left) (cons left-x (cdr o)) o))
                              (nreverse offsets))))
      (plist-put chrome :top (+ (plist-get chrome :top) top-h))
      (when (> bottom 0) (plist-put chrome :bottom (+ (plist-get chrome :bottom) row bottom)))
      (plist-put chrome :left (+ (plist-get chrome :left) left-w))
      (when (seq-some (lambda (l) (equal (eas-legend-text-orient l) "right")) legends)
        (plist-put chrome :right (+ rx col-w)))
      (list legends offsets chrome (max tallest left-y)))))

(provide 'eas-legend-text-orient)
;;; eas-legend-text-orient.el ends here

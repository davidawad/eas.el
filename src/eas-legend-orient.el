;;; eas-legend-orient.el --- legends above, below and left of the plot -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (svg).  Vega-Lite's legend orient "top", "bottom" and
;; "left" place a legend outside the plot on that side, as Vega's view
;; layout does (legendParams and gridLayout):
;;
;;   top     its bottom edge legend.offset (18) above the plot and any
;;           top axis, its left edge on the plot's
;;   bottom  its top edge offset below the plot and any bottom axis
;;   left    its right edge offset left of the plot and any left axis,
;;           its top on the plot's
;;
;; Legends sharing a side sit side by side on top and bottom and
;; stacked on the left, 8px (legend margin) apart.  A legend on top or
;; bottom runs horizontally unless its direction says otherwise: a
;; symbol legend puts its entries in one row, each its symbol's width
;; plus labelOffset plus its label, 10px (columnPadding) apart; a
;; gradient legend lays its bar out horizontally (eas-legend-extra.el).
;; legend.columns sets the column count of a symbol legend's grid
;; (column-major when it runs vertically), rows rowPadding apart.
;; The character grid keeps every legend on the right.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(declare-function eas-legend-symbol-type "eas-legend")

(declare-function eas-legend--symbol "eas-legend")
(declare-function eas-legend--title "eas-legend")
(declare-function eas-legend-place "eas-legend")

(defconst eas-legend-orient-sides '("top" "bottom" "left")
  "Legend orients placed outside the plot by this file.")

(defconst eas-legend-orient-column-padding 10 "Vega's legend columnPadding.")

(defun eas-legend-orient-direction (legend)
  "LEGEND's direction: its own, else horizontal on top and bottom."
  (or (plist-get legend :direction)
      (and (member (plist-get legend :orient) '("top" "bottom")) "horizontal")))

(defun eas-legend-orient-columns (legend)
  "LEGEND's entry columns: legend.columns, else 1 vertical and 0 (all) horizontal."
  (let ((c (plist-get legend :columns)))
    (if (and (numberp c) (>= c 0)) c (if (equal (eas-legend-orient-direction legend) "horizontal") 0 1))))

(defun eas-legend-orient-row-p (legend metrics)
  "Non-nil when symbol LEGEND lays its entries out in a grid under svg METRICS:
horizontally, or in more than one column."
  (and (equal (plist-get legend :type) "symbol") (not (eas-layout-text-p metrics))
       (/= (eas-legend-orient-columns legend) 1)))

(defun eas-legend-orient--cells (legend n)
  "(ROW . COLUMN) of each of N entries of LEGEND, as Vega's legend grid."
  (let* ((cols (eas-legend-orient-columns legend)) (ncols (if (zerop cols) n (max 1 cols)))
         (nrows (ceiling n (float ncols)))
         (vertical (not (equal (eas-legend-orient-direction legend) "horizontal"))))
    (cl-loop for i below n
             collect (if (and vertical (> cols 0)) (cons (% i nrows) (/ i nrows))
                       (cons (/ i ncols) (% i ncols))))))

(defun eas-legend-orient-place-row (legend x y metrics)
  "Symbol LEGEND with its top-left at X Y, its entries in Vega's grid:
columns (legend.columns) columnPadding apart, rows rowPadding apart,
each entry centred in its row.  METRICS sizes the text."
  (let* ((fs (plist-get metrics :legend-label-size))
         (title (eas-legend--title legend x y metrics))
         (top (cdr title)) (es (append (plist-get legend :entries) nil))
         (cols (eas-legend-orient-columns legend))
         (looks (mapcar (lambda (e) (eas-legend--symbol legend e metrics)) es))
         (sizes (mapcar (lambda (s) (max (ceiling (+ (sqrt (plist-get s :size)) (plist-get s :stroke-width))) fs)) looks))
         ;; With a column count every symbol takes the widest one's width.
         (widest (apply #'max 0 sizes))
         (cells (eas-legend-orient--cells legend (length es)))
         ;; Each entry relative to its own origin: (E LOOK SIZE OFFSET [X1 Y1 X2 Y2]).
         (local (cl-loop for e in es for s in looks for size in sizes
                         collect (let* ((off (if (zerop cols) size widest))
                                        (r (+ (/ (sqrt (plist-get s :size)) 2.0)
                                              (if (plist-get s :stroke) (/ (plist-get s :stroke-width) 2.0) 0)))
                                        (cy (/ size 2.0))
                                        (lbox (eas-layout-text-bounds metrics (plist-get e :label) fs
                                                                      (+ off (plist-get metrics :legend-label-offset)) cy "left" "middle")))
                                   (list e s size off (vector (min 0 (- (/ off 2.0) r)) (min (aref lbox 1) (- cy r))
                                                              (max (aref lbox 2) (+ (/ off 2.0) r)) (max (aref lbox 3) (+ cy r)))))))
         (ncol (1+ (apply #'max 0 (mapcar #'cdr cells)))) (nrow (1+ (apply #'max 0 (mapcar #'car cells))))
         (extent (lambda (key idx) (apply #'max 0 (cl-loop for c in cells for l in local
                                                             when (= (funcall key c) idx) collect (ceiling (aref (nth 4 l) (if (eq key #'cdr) 2 3)))))))
         (lead (lambda (key idx pad) (apply #'max 0 (cl-loop for c in cells for l in local
                                                               when (= (funcall key c) idx)
                                                               collect (+ pad (let ((v (aref (nth 4 l) (if (eq key #'cdr) 0 1)))) (if (< v 0) (ceiling (- v)) 0)))))))
         (xs (let ((acc 0)) (cl-loop for c below ncol
                                     collect (setq acc (if (zerop c) 0 (+ acc (funcall extent #'cdr (1- c))
                                                                          (funcall lead #'cdr c (or (plist-get legend :column-padding) eas-legend-orient-column-padding))))))))
         (ys (let ((acc 0)) (cl-loop for r below nrow
                                     collect (setq acc (if (zerop r) 0 (+ acc (funcall extent #'car (1- r))
                                                                          (funcall lead #'car r (plist-get metrics :legend-row-pad))))))))
         (box nil)
         (entries
          (cl-loop
           for c in cells for l in local
           collect (pcase-let* ((`(,e ,s ,size ,off ,b) l)
                                (cy (/ size 2.0))
                                ;; Rows centre their entries vertically.
                                (dy (if (/= cols 1) (max 0 (/ (- (funcall extent #'car (car c)) (aref b 3)) 2.0)) 0))
                                (ex (+ x (nth (cdr c) xs))) (ey (+ top (nth (car c) ys) dy)))
                     (setq box (eas-layout-union box (vector (+ ex (aref b 0)) (+ ey (aref b 1)) (+ ex (aref b 2)) (+ ey (aref b 3)))))
                     (append s e (list :sx (+ ex (/ off 2.0)) :sy (+ ey cy)
                                       :lx (+ ex off (plist-get metrics :legend-label-offset)) :ly (+ ey cy)
                                       :bounds (vector ex ey (- (aref b 2) (aref b 0)) size)))))))
    (when (car title)
      (setq box (eas-layout-union box (eas-layout-text-bounds metrics (plist-get legend :title)
                                                              (plist-get metrics :legend-title-size) x y "left" "top"
                                                              0 (plist-get metrics :legend-title-weight)))))
    (append (eas--plist-without legend :entries)
            (list :x x :y y :width (if box (ceiling (- (aref box 2) x)) 0) :font-size fs
                  :symbol-type (eas-legend-symbol-type legend metrics)
                  :box (if box (vector x y (+ x (ceiling (- (aref box 2) x))) (+ y (ceiling (- (aref box 3) y))))
                         (vector x y x y))
                  :entries (vconcat entries))
            (when (car title) (list :title-mark (car title))))))

(defun eas-legend-orient-offsets (legends axes w h metrics)
  "Offsets from the plot origin of LEGENDS placed on top, bottom or left.
AXES are the view's placed axes and W H its plot size.  Return an alist
\(LEGEND . (X . Y)) for those legends; others are absent.  METRICS gives
the default offset."

  (let* ((default (plist-get metrics :legend-offset))
         (axis-box (lambda (horiz)
                     (apply #'eas-layout-union (vector 0 0 w h)
                            (delq nil (mapcar (lambda (a) (and (eq horiz (and (member (plist-get a :orient) '("top" "bottom")) t))
                                                               (plist-get a :bounds)))
                                              axes)))))
         (yb (funcall axis-box t)) (xb (funcall axis-box nil))
         out)
    (dolist (side eas-legend-orient-sides)
      (let* ((group (seq-filter (lambda (l) (equal (plist-get l :orient) side)) legends))
             (offset (if (seq-some (lambda (l) (plist-get l :offset)) group)
                         (apply #'max (delq nil (mapcar (lambda (l) (plist-get l :offset)) group)))
                       default))
             (sizes (mapcar (lambda (l) (let ((b (plist-get (eas-legend-place l 0 0 metrics) :box)))
                                          (cons (aref b 2) (aref b 3))))
                            group))
             (cursor 0))
        (when group
          (if (equal side "left")
              (let ((x (- (floor (aref xb 0)) offset (apply #'max (mapcar #'car sizes)))))
                (cl-loop for l in group for s in sizes
                         do (push (cons l (cons x cursor)) out)
                         (setq cursor (+ cursor (cdr s) (plist-get metrics :legend-margin)))))
            (let ((y (if (equal side "top") (- (floor (aref yb 1)) offset (apply #'max (mapcar #'cdr sizes)))
                       (+ (ceiling (aref yb 3)) offset))))
              (cl-loop for l in group for s in sizes
                       do (push (cons l (cons cursor y)) out)
                       (setq cursor (+ cursor (car s) (plist-get metrics :legend-margin)))))))))
    out))

(provide 'eas-legend-orient)
;;; eas-legend-orient.el ends here

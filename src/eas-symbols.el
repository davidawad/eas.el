;;; eas-symbols.el --- Vega's symbol shapes, for SVG and text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L5.  Point marks, the shape channel and legends draw Vega's
;; symbols: circle, square, cross, diamond, triangle(-up, -down, -left,
;; -right), wedge, arrow and stroke, each of area SIZE centred on its
;; point and turned ANGLE degrees clockwise (the angle channel).  The
;; geometry is vega-scenegraph's (path/symbols.js).  On the character
;; grid each shape is one glyph; wedges and arrows point along their
;; angle.

;;; Code:

(require 'eas-core)

(defconst eas-symbols-range
  ["circle" "square" "triangle-up" "cross" "diamond" "triangle-right" "triangle-down" "triangle-left"]
  "Vega-Lite's default shape range (config.range.symbol).")

(defconst eas-symbols--half-sqrt3 0.8660254037844386)
(defconst eas-symbols--tan30 0.5773502691896257)

(defun eas-symbols-points (shape size)
  "Polygon of SHAPE with area SIZE about the origin, or nil for circles."
  (let* ((r (/ (sqrt (max 0 size)) 2.0)) (h (* eas-symbols--half-sqrt3 r)))
    (pcase shape
      ("square" (list (cons (- r) (- r)) (cons r (- r)) (cons r r) (cons (- r) r)))
      ("cross" (let ((q (/ (sqrt (max 0 size)) 5.0)))
                 (mapcar (lambda (p) (cons (* q (car p)) (* q (cdr p))))
                         '((-3 . -1) (-1 . -1) (-1 . -3) (1 . -3) (1 . -1) (3 . -1)
                           (3 . 1) (1 . 1) (1 . 3) (-1 . 3) (-1 . 1) (-3 . 1)))))
      ("diamond" (list (cons (- r) 0) (cons 0 (- r)) (cons r 0) (cons 0 r)))
      ((or "triangle" "triangle-up") (list (cons 0 (- h)) (cons (- r) h) (cons r h)))
      ("triangle-down" (list (cons 0 h) (cons (- r) (- h)) (cons r (- h))))
      ("triangle-right" (list (cons h 0) (cons (- h) (- r)) (cons (- h) r)))
      ("triangle-left" (list (cons (- h) 0) (cons h (- r)) (cons h r)))
      ("wedge" (let ((o (- h (* r eas-symbols--tan30))) (b (/ r 4)))
                 (list (cons 0 (- (- h) o)) (cons (- b) (- h o)) (cons b (- h o)))))
      ("arrow" (let ((s (/ r 7)) (tt (/ r 2.5)) (v (/ r 8)))
                 (list (cons (- s) r) (cons s r) (cons s (- v)) (cons tt (- v)) (cons 0 (- r))
                       (cons (- tt) (- v)) (cons (- s) (- v)))))
      ("stroke" (list (cons (- r) 0) (cons r 0))))))

(defun eas-symbols--fmt (v)
  "V for SVG path data: two decimals without trailing zeros (\"-0\" is 0)."
  (let* ((s (format "%.2f" v)) (e (length s)))
    (if (equal s "-0.00") "0"
      ;; Trim zeros back to the point, then the point (no regexp: one per vertex).
      (while (eq (aref s (1- e)) ?0) (setq e (1- e)))
      (when (eq (aref s (1- e)) ?.) (setq e (1- e)))
      (substring s 0 e))))

(defun eas-symbols-path (shape x y size &optional angle)
  "SVG path data of SHAPE (area SIZE) at X Y turned ANGLE degrees.\nReturn nil for circles."
  (when-let* ((pts (eas-symbols-points shape size)))
    (let* ((a (degrees-to-radians (or angle 0))) (c (cos a)) (s (sin a)))
      (concat (mapconcat (lambda (p)
                           (concat (if (eq p (car pts)) "M" "L")
                                   (eas-symbols--fmt (+ x (- (* c (car p)) (* s (cdr p)))))
                                   ","
                                   (eas-symbols--fmt (+ y (* s (car p)) (* c (cdr p))))))
                         pts "")
              (if (equal shape "stroke") "" "Z")))))

(defun eas-symbols-glyph (shape filled &optional angle)
  "One character drawing SHAPE (FILLED or not) on the text grid, turned ANGLE."
  (pcase shape
    ("square" (if filled ?■ ?□))
    ("cross" ?✚)
    ("diamond" (if filled ?◆ ?◇))
    ((or "triangle" "triangle-up") (if filled ?▲ ?△))
    ("triangle-down" (if filled ?▼ ?▽))
    ("triangle-right" (if filled ?▶ ?▷))
    ("triangle-left" (if filled ?◀ ?◁))
    ((or "wedge" "arrow") (aref "↑↗→↘↓↙←↖" (mod (round (/ (or angle 0) 45.0)) 8)))
    ("stroke" ?━)
    (_ (if filled ?● ?○))))

(provide 'eas-symbols)
;;; eas-symbols.el ends here

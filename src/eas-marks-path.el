;;; eas-marks-path.el --- series paths: order, breaks and curves -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, for line, area and trail marks (eas-marks.el).  A series
;; is drawn through its rows in Vega-Lite's order: by the order
;; channel's field when there is one, else along x (along y when
;; mark.orient is "horizontal", Vega-Lite's dimension for that orient).  A row whose position is invalid (null) breaks the
;; path, as Vega-Lite 6's default mark.invalid does; "filter" connects
;; across it instead.  Curved interpolation is flattened into points
;; here so both renderers draw the same polyline: `eas-marks-path-monotone'
;; is d3's curveMonotoneX (monotone cubic Hermite through every vertex),
;; sampled by eas-curve.el.

;;; Code:

(require 'eas-core)
(require 'eas-encode)
(require 'eas-curve)

(defconst eas-marks-path-curves eas-curve-modes
  "Curved interpolations flattened natively, besides linear and steps.")

(defun eas-marks-path-sort-key (unit)
  "Function ROW -> sort key of UNIT's series vertices, nil for x, y or data."
  (let* ((enc (plist-get unit :encoding)) (order (plist-get enc :order)))
    (cond
     ;; mark.order false or null, or an order channel valued null: data order (fc-qx1.41).
     ((or (memq (plist-get (plist-get unit :mark) :order) '(:false :null))
          (and (eas-object-p order) (plist-member order :value) (eq (plist-get order :value) :null)))
      'data)
     ((and (eas-object-p order) (plist-get order :field))
      (let ((key (eas-encode-field order))) (lambda (row) (plist-get row key))))
     ((equal (plist-get (plist-get unit :mark) :orient) "horizontal") 'y))))

(defun eas-marks-path--less (a b)
  "Ascending order of mixed sort keys A and B (numbers before strings)."
  (cond ((and (numberp a) (numberp b)) (< a b))
        ((numberp a) t) ((numberp b) nil)
        (t (string< (format "%s" a) (format "%s" b)))))

(defun eas-marks-path-sort (pts)
  "PTS ((X Y I BASE KEY VALID) lists) in path order, stable.
KEY nil means x order."
  (sort pts (lambda (a b) (let ((ka (nth 4 a)) (kb (nth 4 b)))
                            (if (or ka kb) (eas-marks-path--less ka kb) (< (car a) (car b)))))))

(defun eas-marks-path-runs (pts filter)
  "Split PTS into lists of consecutive valid vertices.
These are its runs; FILTER non-nil keeps one run."

  (let (runs run)
    (dolist (p pts)
      (cond ((nth 5 p) (push p run))
            ((not filter) (when run (push (nreverse run) runs)) (setq run nil))))
    (when run (push (nreverse run) runs))
    (nreverse runs)))

(defun eas-marks-path-curve (points mode)
  "POINTS ((X Y) lists) flattened for curved interpolation MODE (eas-curve.el)."
  (eas-curve-apply (let (out) (dolist (p points) (unless (equal p (car out)) (push p out))) (nreverse out))
                   mode))

(provide 'eas-marks-path)
;;; eas-marks-path.el ends here

;;; eas-dotbin.el --- Vega's dotbin as an x-eas transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega-Lite has no dot plot binning, so the "dotbin"
;; domain transform ports Vega's DotBin (vega-transforms) and dotbin
;; (vega-statistics): Wilkinson's dot plot algorithm.
;;
;;   {"x-eas:transform": "dotbin", "field": F, "step": S,
;;    "smooth": false, "groupby": [G ...], "as": "bin"}
;;
;; Within each group, sorted by F, a bin opens at the first value and
;; takes every value less than STEP above it; the next value opens the
;; next bin.  Each row gets its bin's center (the midpoint of its least
;; and greatest value) in AS.  STEP defaults to a thirtieth of F's span.
;; SMOOTH swaps values between stacks closer than 1.25 STEP so their
;; heights even out, as Wilkinson suggests.  Rows keep their order;
;; rows without a number in F get null.  Stack the result (a window
;; row_number or a stack transform grouped by AS) to draw the dots.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)

(defun eas-dotbin (values step &optional smooth)
  "Dot plot bin centers of the ascending vector of numbers VALUES.
STEP is the bin width; SMOOTH evens out adjacent stacks.  Return a
vector of centers, one per value."
  (let* ((n (length values)) (v (make-vector n 0.0)))
    (when (> n 0)
      (let* ((i 0) (a (aref values 0)) (b a) (w (+ a step)))
        (cl-loop for j from 1 below n
                 for x = (aref values j)
                 do (when (>= x w)
                      (let ((c (/ (+ a b) 2.0)))
                        (while (< i j) (aset v i c) (setq i (1+ i))))
                      (setq w (+ x step) a x))
                    (setq b x))
        (let ((c (/ (+ a b) 2.0)))
          (while (< i n) (aset v i c) (setq i (1+ i)))))
      (when smooth (eas-dotbin--smooth v (+ step (/ step 4.0)))))
    v))

(defun eas-dotbin--smooth (v thresh)
  "Even out adjacent stacks of bin centers V (closer than THRESH), in place.
Vega's smoothing: a stack and its right neighbour split their dots as
evenly as their sizes allow."
  (let ((n (length v)) (a 0) (b 1))
    (cl-flet ((at (k) (and (< k n) (aref v k))))
      (while (and (< b n) (eql (at a) (at b))) (setq b (1+ b)))
      (while (< b n)
        (let ((c (1+ b)))
          (while (and (< c n) (eql (at b) (at c))) (setq c (1+ c)))
          (when (< (- (aref v b) (aref v (1- b))) thresh)
            (let ((d (+ b (ash (- (+ a c) b b) -1))))
              (while (< d b) (aset v d (aref v b)) (setq d (1+ d)))
              (while (> d b) (aset v d (aref v a)) (setq d (1- d)))))
          (setq a b b c))))
    v))

(defun eas-dotbin--transform (rows params)
  "The dotbin domain transform: ROWS with their dot plot bin per PARAMS."
  (let* ((field (eas-key (plist-get params :field)))
         (as (eas-key (plist-get params :as)))
         (groupby (mapcar #'eas-key (plist-get params :groupby)))
         (smooth (eq (plist-get params :smooth) t))
         (numbers (seq-filter #'numberp (seq-map (lambda (r) (plist-get r field)) rows)))
         (span (if numbers (- (apply #'max numbers) (apply #'min numbers)) 0))
         (step (let ((s (plist-get params :step)))
                 (cond ((and (numberp s) (> s 0)) s)
                       ((> span 0) (/ span 30.0))
                       (t 1))))
         (out (make-vector (length rows) nil))
         (groups nil))
    (seq-do-indexed
     (lambda (row i)
       (if (numberp (plist-get row field))
           (let* ((key (mapcar (lambda (g) (plist-get row g)) groupby)) (cell (assoc key groups)))
             (if cell (push i (cdr cell)) (push (list key i) groups)))
         (aset out i (eas-plist-put (copy-sequence row) as :null))))
     rows)
    (dolist (g groups)
      ;; Stable ascending order, as Vega sorts each group.
      (let* ((idx (vconcat (sort (nreverse (cdr g))
                                 (lambda (p q) (< (plist-get (aref rows p) field)
                                                  (plist-get (aref rows q) field))))))
             (bins (eas-dotbin (vconcat (seq-map (lambda (k) (plist-get (aref rows k) field)) idx))
                               step smooth)))
        (seq-do-indexed (lambda (k j)
                          (aset out k (eas-plist-put (copy-sequence (aref rows k)) as (aref bins j))))
                        idx)))
    out))

(eas-register-transform
 "dotbin"
 :doc "Vega's dotbin: Wilkinson dot plot bins of a numeric field; each row gets its bin's center."
 :schema '(:field (:type "string" :required t :doc "numeric field to bin")
           :step (:type "number" :doc "bin width; default a thirtieth of the field's span")
           :smooth (:type "boolean" :default :false :doc "even out the heights of adjacent stacks")
           :groupby (:type "array" :default [] :doc "fields whose groups are binned apart")
           :as (:type "string" :default "bin" :doc "output field: the bin center"))
 :fn #'eas-dotbin--transform)

(provide 'eas-dotbin)
;;; eas-dotbin.el ends here

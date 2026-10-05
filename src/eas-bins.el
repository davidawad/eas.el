;;; eas-bins.el --- binned fields, merged axis titles, point size range -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4: Vega-Lite rules the histogram family depends on.
;;
;; - Pre-binned data: `bin: "binned"' or `bin: {binned: true, step}'
;;   marks a field whose rows already hold bin starts; its x2/y2 holds
;;   the ends.  It is not binned again (`eas-bins-binned-def').
;; - A binned scale carries Vega's `bins': the bin boundaries, which
;;   are its axis ticks (`eas-bins-boundaries').
;; - Axis titles merge every field title on the channel, its x2/y2
;;   partner and the other layers: "start, end" (`eas-bins-axis-def').
;; - Point marks size by `pow(0.95 * min(x step, y step), 2)', a step
;;   being a band step or a bin's width in pixels, else 20
;;   (`eas-bins-size-range').
;; - Rows whose continuous x or y is invalid do not feed the other
;;   channels' domains, as Vega-Lite filters them (`eas-bins-valid-p').

;;; Code:

(require 'eas-core)
(require 'eas-scale)

(declare-function eas-encode-title "eas-encode")

(defun eas-bins-binned-p (def)
  "Non-nil when DEF's field is already binned (bin \"binned\" or {binned: true})."
  (let ((bin (plist-get def :bin)))
    (or (equal bin "binned") (and (eas-object-p bin) bin (eq (plist-get bin :binned) t)))))

(defun eas-bins-binned-def (def partner)
  "DEF of a pre-binned field normalized: bin ends from PARTNER's field.
The result carries :derived \"binned\", :bin-end and :bin-step (or nil)."
  (let ((bin (plist-get def :bin)))
    (append (eas--plist-without def :bin)
            (list :derived "binned"
                  :bin-step (and (eas-object-p bin) (plist-get bin :step)))
            (when (and (eas-object-p partner) (stringp (plist-get partner :field)))
              (list :bin-end (plist-get partner :field))))))

(defun eas-bins-boundaries (def rows lo hi)
  "Vega's scale bins for binned DEF over domain LO..HI, or nil.
Binned-by-encoding fields step by their rows' bin width; pre-binned
fields only with an explicit bin step."
  (let ((step (pcase (plist-get def :derived)
                ("bin" (let ((s (eas-key (plist-get def :field))) (e (eas-key (plist-get def :bin-end))))
                         (seq-some (lambda (r) (let ((a (plist-get r s)) (b (plist-get r e)))
                                                 (and (numberp a) (numberp b) (> b a) (- b a))))
                                   rows)))
                ("binned" (plist-get def :bin-step)))))
    (when (and (numberp step) (> step 0) (< lo hi))
      (vconcat (cl-loop for i from 0
                        for v = (+ lo (* i step))
                        while (<= v (+ hi (/ step 2.0)))
                        collect (if (> v hi) hi v))))))

(defun eas-bins-axis-def (units channel)
  "CHANNEL's axis definition across UNITS, with Vega-Lite's merged title.
The first field definition, titled with its explicit title, or every
distinct default title of the channel and its x2/y2 partner joined by
\", \"."
  (let* ((partner (if (eq channel :x) :x2 :y2))
         (defs (cl-loop for u in units
                        for enc = (plist-get u :encoding)
                        append (cl-loop for ch in (list channel partner)
                                        for d = (plist-get enc ch)
                                        when (and d (eas-object-p d)
                                                  (or (plist-get d :field) (plist-member d :datum)
                                                      (plist-get d :aggregate)))
                                        collect (cons ch d))))
         (first (cdr (assq channel defs))))
    (when first
      (let ((explicit (seq-find (lambda (d) (plist-member (cdr d) :title)) defs)))
        (cond
         ((plist-member first :title) first)
         (explicit (append (list :title (plist-get (cdr explicit) :title)) first))
         (t (let ((titles (delete-dups (delq nil (mapcar (lambda (d) (eas-encode-title (cdr d))) defs)))))
              (if (cdr titles) (append (list :title (string-join titles ", ")) first) first))))))))

(defun eas-bins--step-px (scale def extent)
  "Pixel step of positional SCALE (DEF its field) spanning EXTENT pixels, or nil."
  (cond
   ((null scale) nil)
   ((member (plist-get scale :type) '("band" "point")) (plist-get scale :step))
   ((and def (equal (plist-get def :derived) "bin") (> (length (plist-get scale :bins)) 1))
    (/ extent (float (1- (length (plist-get scale :bins))))))
   ((and def (equal (plist-get def :derived) "binned") (numberp (plist-get def :bin-step)))
    (let ((d (plist-get scale :domain)))
      (/ extent (/ (abs (- (aref d 1) (aref d 0))) (float (plist-get def :bin-step))))))))

(defun eas-bins-size-range (group local-scale)
  "Point marks' size range in GROUP from its plot steps; nil when not applicable.
LOCAL-SCALE maps a channel to GROUP's scale over its unplaced plot."
  (let* ((units (plist-get group :units))
         (size (plist-get (plist-get group :scales) :size))
         (pairs (cl-loop for u in units
                         for d = (plist-get (plist-get u :encoding) :size)
                         when (and (eas-object-p d) d (plist-get d :field)) collect (cons u d))))
    (when (and size pairs
               (seq-every-p (lambda (p) (member (plist-get (plist-get (car p) :mark) :type) '("point" "circle" "square")))
                            pairs)
               (not (vectorp (plist-get (plist-get (cdar pairs) :scale) :range))))
      (let* ((steps (delq nil (list (eas-bins--step-px (funcall local-scale :x)
                                                       (plist-get (plist-get (caar pairs) :encoding) :x)
                                                       (plist-get group :w))
                                    (eas-bins--step-px (funcall local-scale :y)
                                                       (plist-get (plist-get (caar pairs) :encoding) :y)
                                                       (plist-get group :h)))))
             (step (if steps (apply #'min steps) 20)))
        (let ((sp (plist-get (cdar pairs) :scale)))
          (vector (or (plist-get sp :rangeMin) (aref (plist-get size :range) 0))
                  (or (plist-get sp :rangeMax) (expt (* 0.95 step) 2))))))))

(defun eas-bins-pad-log (domain frac nice)
  "Log DOMAIN widened by FRAC about its centre in log space, as Vega's padDomain.
With NICE, rounded out to powers of ten."
  (let* ((a (log (aref domain 0) 10)) (b (log (aref domain 1) 10)) (c (/ (+ a b) 2.0))
         (lo (+ c (* frac (- a c)))) (hi (+ c (* frac (- b c)))))
    (if nice (vector (expt 10.0 (floor lo)) (expt 10.0 (ceiling hi)))
      (vector (expt 10.0 lo) (expt 10.0 hi)))))

(defun eas-bins-band-space (scale n)
  "Vega's bandspace of discrete SCALE with N values, in steps.
N for default paddings; a point scale with padding P spans N - 1 + 2P."
  (let* ((point (equal (plist-get scale :type) "point"))
         (inner (if point 1.0 (or (plist-get scale :padding-inner) 0.1)))
         (outer (or (plist-get scale :padding-outer) (if point 0.5 (/ inner 2.0)))))
    (+ (- n inner) (* 2 outer))))

(defconst eas-bins-shapes
  ["circle" "square" "cross" "diamond" "triangle-up" "triangle-down" "triangle-right" "triangle-left"]
  "Vega-Lite's default shape range.")

(defun eas-bins-shape-scale (units)
  "Ordinal scale for UNITS' shape field, or nil: the spec's domain and
range, else the data's values onto `eas-bins-shapes'."
  (when-let* ((pairs (cl-loop for u in units
                              for d = (plist-get (plist-get u :encoding) :shape)
                              when (and (eas-object-p d) d (plist-get d :field)) collect (cons u d))))
    (let* ((def (cdar pairs)) (sp (plist-get def :scale)) (key (eas-key (plist-get def :field))))
      (append (eas-scale-ordinal (or (and (vectorp (plist-get sp :domain)) (plist-get sp :domain))
                                     (vconcat (delete-dups (cl-loop for p in pairs
                                                                    append (seq-map (lambda (r) (plist-get r key))
                                                                                    (plist-get (car p) :rows))))))
                                 (or (and (vectorp (plist-get sp :range)) (plist-get sp :range)) eas-bins-shapes))
              (list :field (plist-get def :field))))))

(defun eas-bins-valid-p (unit row)
  "Non-nil unless ROW's continuous x or y in UNIT is null (Vega-Lite drops it)."
  (let ((enc (plist-get unit :encoding)))
    (cl-loop for ch in '(:x :y)
             for d = (plist-get enc ch)
             never (and (eas-object-p d) d (plist-get d :field)
                        (member (plist-get d :type) '("quantitative" "temporal"))
                        (memq (plist-get row (eas-key (plist-get d :field))) '(nil :null))))))

(provide 'eas-bins)
;;; eas-bins.el ends here

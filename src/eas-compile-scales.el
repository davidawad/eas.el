;;; eas-compile-scales.el --- scale domains for a compiled view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  One view (a unit or a layer) shares one scale per
;; channel, as in Vega-Lite's default "shared" resolution.  Domains
;; follow Vega-Lite's defaults: zero and nice for linear x/y unless
;; binned or explicit, discrete domains sorted ascending, bars on band
;; scales, other discrete marks on point scales.  Zoomed domains from
;; view state replace the computed ones.  Ranges are set after layout.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)
(require 'eas-scheme)
(require 'eas-bins)

(defun eas-compile--defs (units channel)
  "Return (UNIT . DEF) pairs for CHANNEL across UNITS with a data def."
  (cl-loop for u in units
           for d = (plist-get (plist-get u :encoding) channel)
           when (and d (eas-object-p d) (or (plist-get d :field) (plist-member d :datum)))
           collect (cons u d)))

(defun eas-compile--values (pairs channel)
  "Raw values feeding CHANNEL's domain from PAIRS of (UNIT . DEF)."
  (let (out)
    (dolist (pair pairs)
      (let* ((u (car pair)) (d (cdr pair)) (enc (plist-get u :encoding))
             (partner (plist-get enc (if (eq channel :x) :x2 :y2)))
             (keys (delq nil (list (eas-encode-field d)
                                   (and partner (eas-encode-field partner))
                                   (and (plist-get d :stack-start) (eas-key (plist-get d :stack-start)))
                                   (and (plist-get d :bin-end) (eas-key (plist-get d :bin-end)))))))
        (if (plist-member d :datum)
            (push (plist-get d :datum) out)
          (seq-doseq (row (plist-get u :rows))
            (when (or (memq channel '(:x :y)) (eas-bins-valid-p u row))
              (dolist (k keys)
                (let ((v (plist-get row k)))
                  (unless (memq v '(nil :null)) (push v out)))))))))
    (nreverse out)))

(defun eas-compile--scale-type (pairs channel)
  "Default scale type for positional CHANNEL from PAIRS."
  (let* ((def (cdar pairs)) (explicit (plist-get (plist-get def :scale) :type)))
    (cond
     (explicit (if (equal explicit "utc") "time" explicit))
     ((plist-get def :bin-end) "linear")
     ((equal (plist-get def :type) "quantitative") "linear")
     ((equal (plist-get def :type) "temporal") "time")
     ((and (memq channel '(:x :y))
           (seq-some (lambda (p) (member (plist-get (plist-get (car p) :mark) :type) '("bar" "rect" "tick")))
                     pairs))
      "band")
     ((memq channel '(:x :y)) "point")
     (t "ordinal"))))

(defun eas-compile--less (a b)
  "Ascending order for mixed domain values A and B."
  (if (and (numberp a) (numberp b)) (< a b) (string< (format "%s" a) (format "%s" b))))

(defun eas-compile--discrete-domain (pairs values)
  "Discrete domain from VALUES, ordered per the first def in PAIRS."
  (let* ((def (cdar pairs)) (sort (plist-get def :sort))
         (unique (delete-dups (copy-sequence values))))
    (vconcat
     (cond
      ;; An explicit scale domain is the domain.
      ((vectorp (plist-get (plist-get def :scale) :domain)) (plist-get (plist-get def :scale) :domain))
      ((eq sort :null) unique)
      ((vectorp sort) (append (seq-filter (lambda (v) (member v unique)) sort)
                              (seq-remove (lambda (v) (seq-contains-p sort v)) unique)))
      ((equal sort "descending") (reverse (sort unique #'eas-compile--less)))
      ((and (stringp sort) (string-match "\\`\\(-?\\)\\([xy]\\)\\'" sort))
       (let* ((desc (equal (match-string 1 sort) "-"))
              (other (eas-key (match-string 2 sort)))
              (u (caar pairs)) (odef (plist-get (plist-get u :encoding) other))
              (sums (make-hash-table :test 'equal)))
         (seq-doseq (row (plist-get u :rows))
           (let ((k (eas-encode-raw def row)) (v (eas-encode-raw odef row)))
             (when (numberp v) (puthash k (+ v (gethash k sums 0)) sums))))
         (let ((sorted (sort unique (lambda (a b) (< (gethash a sums 0) (gethash b sums 0))))))
           (if desc (nreverse sorted) sorted))))
      (t (sort unique #'eas-compile--less))))))

(defun eas-compile--continuous (type pairs channel values zoom)
  "Continuous scale of TYPE for CHANNEL over VALUES (ZOOM overrides domain)."
  (let* ((def (cdar pairs)) (sp (plist-get def :scale))
         (explicit (and (vectorp (plist-get sp :domain)) (plist-get sp :domain)))
         (nums (if (equal type "time") (delq nil (mapcar #'eas-time-parse values))
                 (seq-filter #'numberp values)))
         (positional (memq channel '(:x :y)))
         (binned (plist-get def :bin-end))
         (lo (cond (zoom (aref zoom 0)) (explicit (eas-compile--num type (aref explicit 0)))
                   ((plist-get sp :domainMin) (eas-compile--num type (plist-get sp :domainMin)))
                   (nums (apply #'min nums)) (t 0)))
         (hi (cond (zoom (aref zoom 1)) (explicit (eas-compile--num type (aref explicit 1)))
                   ((plist-get sp :domainMax) (eas-compile--num type (plist-get sp :domainMax)))
                   (nums (apply #'max nums)) (t 1)))
         (custom (or zoom explicit))
         (zero (if (plist-member sp :zero) (eq (plist-get sp :zero) t)
                 (and (equal type "linear") (not binned) (not custom)
                      (or (eq channel :size)
                          (and positional (not (eas-compile--dimension-p pairs channel)))))))
         (nice (if (plist-member sp :nice) (eq (plist-get sp :nice) t)
                 (and (member type '("linear" "log")) positional (not binned) (not custom)))))
    (append (eas-scale-continuous type lo hi [0 1] :zero zero :nice nice
                                    :field (plist-get def :field)
                                    ;; utc scales tick and label in UTC whatever `eas-time-zone' is.
                                    :utc (if (equal (plist-get sp :type) "utc") t :false)
                                    :reverse (if (eq (plist-get sp :reverse) t) t :false))
            (when-let* ((bins (and binned (not custom) (equal type "linear")
                                   (eas-bins-boundaries def (plist-get (caar pairs) :rows) lo hi))))
              (list :bins bins))
            ;; Vega-Lite pads a bar's continuous dimension by continuousBandSize.
            (let ((pad (or (plist-get sp :padding)
                           (and (not custom) (not binned) (not (plist-get def :derived))
                                (eas-compile--dimension-p
                                 (seq-filter (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "bar")) pairs)
                                 channel)
                                5))))
              (when pad (append (list :padding pad)
                                ;; Vega nices a padded log domain again.
                                (when (and nice (equal type "log")) (list :renice t))))))))

(defun eas-compile--dimension-p (pairs channel)
  "Non-nil when CHANNEL is the dimension (not the measure) of a bar, area
or line in PAIRS; Vega-Lite does not extend dimension scales to zero."
  (seq-some (lambda (p)
              (let* ((u (car p)) (enc (plist-get u :encoding))
                     (type (plist-get (plist-get u :mark) :type))
                     (x (plist-get enc :x)) (y (plist-get enc :y))
                     (horizontal (and x y (eas-encode-discrete-p y) (not (eas-encode-discrete-p x)))))
                (and (member type '("bar" "area" "line"))
                     (eq channel (if horizontal :y :x)))))
            pairs))

(defun eas-compile--num (type v)
  "Domain bound V as a number for scale TYPE."
  (if (equal type "time") (eas-time-parse v) v))

(defun eas-compile-position-scale (units channel zoom)
  "Scale for positional CHANNEL shared by UNITS, or nil.
ZOOM is a [LO HI] domain from view state, or nil."
  (when-let* ((pairs (eas-compile--defs units channel)))
    (let* ((type (eas-compile--scale-type pairs channel))
           (values (eas-compile--values pairs channel))
           (sp (plist-get (cdar pairs) :scale)))
      (if (member type '("band" "point"))
          (let* ((only (lambda (type) (seq-every-p (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) type))
                                                   pairs)))
                 (rect (funcall only "rect"))
                 ;; Vega-Lite: rect bands touch; tick bands pad 0.25 inside, 0.125 outside.
                 (tick (and (equal type "band") (funcall only "tick")))
                 (inner (or (plist-get sp :paddingInner) (plist-get sp :padding) (cond (rect 0) (tick 0.25))))
                 (outer (or (plist-get sp :paddingOuter) (plist-get sp :padding) (cond (rect 0) (tick 0.125)))))
            (append (eas-scale-band type (eas-compile--discrete-domain pairs values) [0 1] inner outer)
                    (list :field (plist-get (cdar pairs) :field) :padding-inner inner :padding-outer outer)))
        (eas-compile--continuous type pairs channel values zoom)))))

(defun eas-compile-color-scale (units &optional config)
  "Return (CHANNEL DEF SCALE) for the first field-mapped color channel.
Ranges come from CONFIG's range.category, .heatmap and .ramp."
  (cl-loop for channel in '(:color :fill :stroke)
           for pairs = (seq-filter (lambda (p) (plist-get (cdr p) :field))
                                   (eas-compile--defs units channel))
           when pairs
           return (let* ((def (cdar pairs)) (sp (plist-get def :scale))
                         (values (eas-compile--values pairs channel)))
                    (list channel def
                          (if (eas-encode-discrete-p def)
                              (append (eas-scale-ordinal (or (and (vectorp (plist-get sp :domain)) (plist-get sp :domain))
                                                                 (eas-compile--discrete-domain pairs values))
                                                           (or (and (vectorp (plist-get sp :range)) (plist-get sp :range))
                                                               (eas-scheme-colors (plist-get sp :scheme))
                                                               (eas-compile--config-range config :category)
                                                               eas-scale-tableau10))
                                      (list :field (plist-get def :field)))
                            (let ((nums (if (equal (plist-get def :type) "temporal")
                                            (delq nil (mapcar #'eas-time-parse values))
                                          (seq-filter #'numberp values))))
                              (list :type "sequential"
                                    :domain (vector (if nums (apply #'min nums) 0) (if nums (apply #'max nums) 1))
                                    :range (or (and (vectorp (plist-get sp :range)) (plist-get sp :range))
                                               ;; Vega-Lite: config.range.heatmap for rect, ramp otherwise.
                                               (if (seq-some (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "rect"))
                                                             pairs)
                                                   (or (eas-compile--config-range config :heatmap)
                                                       eas-scale-yellowgreenblue)
                                                 (or (eas-compile--config-range config :ramp) eas-scale-blues)))
                                    :field (plist-get def :field))))))))

(defun eas-compile--config-range (config key)
  "CONFIG's range KEY when it is an explicit array of colors."
  (let ((r (plist-get (plist-get config :range) key))) (and (vectorp r) (> (length r) 0) r)))

(defun eas-compile-aux-scale (units channel range)
  "Linear scale for CHANNEL (size or opacity) onto RANGE, or nil."
  (when-let* ((pairs (seq-filter (lambda (p) (plist-get (cdr p) :field)) (eas-compile--defs units channel))))
    (let ((nums (seq-filter #'numberp (eas-compile--values pairs channel))))
      (eas-scale-continuous "linear" (if (eq channel :size) 0 (if nums (apply #'min nums) 0))
                              (if nums (apply #'max nums) 1) range :field (plist-get (cdar pairs) :field)))))

(defun eas-compile-set-range (scale range)
  "Return SCALE mapped onto RANGE (recomputing band geometry).
A continuous :padding P (pixels) widens the domain about its centre so
the data spans the range less P on each side, like Vega's padDomain."
  (let ((range (if (eq (plist-get scale :reverse) t) (vector (aref range 1) (aref range 0)) range)))
    (if (member (plist-get scale :type) '("band" "point"))
        (append (eas-scale-band (plist-get scale :type) (plist-get scale :domain) range
                                  (plist-get scale :padding-inner) (plist-get scale :padding-outer))
                (list :field (plist-get scale :field)))
      (let ((out (plist-put (copy-sequence scale) :range range))
            (pad (plist-get scale :padding))
            (span (abs (- (aref range 1) (aref range 0)))))
        (when (and pad (> span (* 2 pad)) (member (plist-get scale :type) '("linear" "time" "utc")))
          (let* ((d (plist-get scale :domain)) (c (/ (+ (aref d 0) (aref d 1)) 2.0))
                 (frac (/ span (- span (* 2.0 pad)))))
            (setq out (plist-put out :domain (vector (+ c (* frac (- (aref d 0) c))) (+ c (* frac (- (aref d 1) c))))))
            (setq out (plist-put out :padding nil))))
        (when (and pad (> span (* 2 pad)) (equal (plist-get scale :type) "log"))
          (setq out (plist-put out :domain (eas-bins-pad-log (plist-get scale :domain) (/ span (- span (* 2.0 pad)))
                                                             (plist-get scale :renice))))
          (setq out (plist-put out :padding nil)))
        out))))

(provide 'eas-compile-scales)
;;; eas-compile-scales.el ends here

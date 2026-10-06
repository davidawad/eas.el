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
(require 'eas-compile-aux)
(require 'eas-time-band)
(require 'eas-compile-sort)
(require 'eas-color-names)
(require 'eas-scale-discretize)
(require 'eas-scale-sequential)
(require 'eas-scale-round)

(defun eas-compile--defs (units channel)
  "Return (UNIT . DEF) pairs for CHANNEL across UNITS with a data def."
  (cl-loop for u in units
           for d = (eas-encode-data-def (plist-get (plist-get u :encoding) channel))
           when (and d (eas-object-p d) (or (plist-get d :field) (plist-member d :datum)))
           collect (cons u d)))

(defun eas-compile--values (pairs channel &optional keep-null)
  "Raw values feeding CHANNEL's domain from PAIRS of (UNIT . DEF).
With KEEP-NULL, null values count too (discrete domains show them)."
  (let (out)
    (dolist (pair pairs)
      (let* ((u (car pair)) (d (cdr pair)) (enc (plist-get u :encoding))
             (partner (and (memq channel '(:x :y)) (plist-get enc (if (eq channel :x) :x2 :y2))))
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
                  (unless (or (null v) (and (eq v :null) (not keep-null))) (push v out))))))
          ;; Bars spanning their time unit reach the next unit's start.
          (dolist (v (eas-time-band-values u d (plist-get u :rows))) (push v out)))))
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
           (seq-some (lambda (p) (or (member (plist-get (plist-get (car p) :mark) :type) '("bar" "rect" "tick"))
                                     (eas-compile--offset-p p channel)))
                     pairs))
      "band")
     ((memq channel '(:x :y)) "point")
     (t "ordinal"))))

(defun eas-compile--offset-p (pair channel)
  "Non-nil when PAIR's unit nests an offset (xOffset/yOffset) in CHANNEL."
  (plist-get (plist-get (car pair) :encoding) (if (eq channel :x) :xOffset :yOffset)))

(defun eas-compile--less (a b)
  "Ascending order for mixed domain values A and B; null first, as in Vega."
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((eq a :null) (not (eq b :null)))
        ((eq b :null) nil)
        ((and (numberp a) (numberp b)) (< a b))
        (t (string< (format "%s" a) (format "%s" b)))))

(defun eas-compile--discrete-domain (pairs values)
  "Discrete domain from VALUES, ordered per the first def in PAIRS."
  (let* ((def (cdar pairs)) (sort (plist-get def :sort))
         (unique (delete-dups (copy-sequence values))))
    (vconcat
     (cond
      ;; An explicit scale domain is the domain.
      ((vectorp (plist-get (plist-get def :scale) :domain)) (plist-get (plist-get def :scale) :domain))
      ;; Vega-Lite leaves a domain of datums (a repeat's fields) in layer order.
      ((or (eq sort :null) (seq-every-p (lambda (p) (plist-member (cdr p) :datum)) pairs)) unique)
      ((and (vectorp sort) (eas-compile-aux-timeunit-sort def sort (sort unique #'eas-compile--less))))
      ((vectorp sort) (append (seq-filter (lambda (v) (member v unique)) sort)
                              (seq-remove (lambda (v) (seq-contains-p sort v)) unique)))
      ((equal sort "descending") (reverse (sort unique #'eas-compile--less)))
      ((and (eas-object-p sort) (or (plist-get sort :field) (plist-get sort :op)))
       (eas-compile-sort-by-field sort def (seq-mapcat (lambda (p) (plist-get (car p) :rows)) pairs 'vector)
                                  (sort unique #'eas-compile--less)))
      ((and (stringp sort) (string-match "\\`\\(-?\\)\\([xy]\\)\\'" sort))
       (let* ((desc (equal (match-string 1 sort) "-"))
              (other (eas-key (match-string 2 sort)))
              (u (caar pairs)) (odef (plist-get (plist-get u :encoding) other))
              (sums (make-hash-table :test 'equal)))
         (seq-doseq (row (plist-get u :rows))
           (let ((k (eas-encode-raw def row)) (v (eas-encode-raw odef row)))
             (when (numberp v) (puthash k (+ v (gethash k sums 0)) sums))))
         ;; A stable sort either way: ties keep their first-seen order, as Vega's do.
         (sort unique (lambda (a b) (funcall (if desc #'> #'<) (gethash a sums 0) (gethash b sums 0))))))
      (t (sort unique #'eas-compile--less))))))

(defun eas-compile--continuous (type pairs channel values zoom)
  "Continuous scale of TYPE for CHANNEL over VALUES (ZOOM overrides domain).
PAIRS are the channel's (UNIT . DEF) pairs."
  (let* ((def (cdar pairs)) (sp (plist-get def :scale))
         (explicit (eas-compile--domain (plist-get sp :domain) (plist-get (caar pairs) :env) channel))
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
         ;; domainMin/domainMax win over zero, as Vega applies them last,
         ;; and Vega-Lite does not nice a domain they fix.
         (fix-lo (and (not custom) (numberp (plist-get sp :domainMin))))
         (fix-hi (and (not custom) (numberp (plist-get sp :domainMax))))
         (nice (if (plist-member sp :nice) (eq (plist-get sp :nice) t)
                 (and (member type '("linear" "log" "sqrt" "pow")) positional (not custom) (not fix-lo) (not fix-hi)
                      ;; Vega-Lite merges a layer's nice from the first layer that sets one:
                      ;; a binned layer leaves it to the next (fc-qx1.44).
                      (seq-some (lambda (p) (not (plist-get (cdr p) :bin-end))) pairs))))
         (lo (if (and zero fix-hi (not fix-lo)) (min lo 0) lo))
         (hi (if (and zero fix-lo (not fix-hi)) (max hi 0) hi))
         (zero (and zero (not fix-lo) (not fix-hi))))
    (append (eas-scale-continuous type lo hi [0 1] :zero zero :nice nice
                                    :field (plist-get def :field)
                                    ;; utc scales tick and label in UTC whatever `eas-time-zone' is.
                                    :utc (if (equal (plist-get sp :type) "utc") t :false)
                                    :reverse (if (if (plist-member sp :reverse) (eq (plist-get sp :reverse) t)
                                                   ;; Vega-Lite: sort descending reverses a continuous scale.
                                                   (equal (plist-get def :sort) "descending"))
                                               t :false))
            ;; Vega's round: positions snap to whole pixels.
            (and (eq (plist-get sp :round) t) (list :round t))
            (when-let* ((bins (and binned (not custom) (equal type "linear")
                                   (eas-bins-boundaries def (plist-get (caar pairs) :rows) lo hi))))
              (list :bins bins))
            ;; Selections over a binned field test the raw field, as Vega-Lite's
            ;; do, and Vega-Lite ticks a binned axis at the bin boundaries.
            (when (and binned positional (equal (plist-get def :derived) "bin"))
              (list :bin-source (plist-get def :source) :bin-step (eas-compile--bin-step pairs)))
            ;; Vega-Lite pads a bar's continuous dimension by continuousBandSize.
            (let ((pad (or (plist-get sp :padding)
                           (and (not custom) (not binned) (not (plist-get def :derived)) (not (plist-get def :binned))
                                (eas-compile--dimension-p
                                 (seq-filter (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "bar")) pairs)
                                 channel)
                                5))))
              (when pad (append (list :padding pad)
                                ;; Vega nices a padded log domain again.
                                (when (and nice (equal type "log")) (list :renice t))
                                ;; Vega pads a linear domain after zero, before nice.
                                (when (and (equal type "linear") (not custom) nice)
                                  (list :raw-domain (vector (if zero (min lo 0) lo) (if zero (max hi 0) hi))
                                        :renice t))))))))

(defun eas-compile--bin-step (pairs)
  "The bin width of the binned def in PAIRS, read off its rows."
  (let* ((def (cdar pairs)) (k0 (eas-encode-field def)) (k1 (eas-key (plist-get def :bin-end))))
    (seq-some (lambda (row) (let ((a (plist-get row k0)) (b (plist-get row k1)))
                              (and (numberp a) (numberp b) (> b a) (- b a))))
              (plist-get (caar pairs) :rows))))

(defun eas-compile--range (range env)
  "An explicit RANGE array with {\"expr\": E} entries evaluated in ENV."
  (when (vectorp range)
    (vconcat (mapcar (lambda (v) (if (and (consp v) (plist-get v :expr)) (eas-expr-evaluate (plist-get v :expr) nil env) v))
                     range))))

(defun eas-compile--domain (domain env channel)
  "Resolve an explicit scale DOMAIN on CHANNEL in ENV.
DOMAIN is an array, or {\"param\": NAME} for the interval selection
NAME's range (nil while empty)."
  (cond ((vectorp domain) domain)
        ((and (consp domain) (plist-get domain :param))
         (let* ((store (plist-get env (eas-key (plist-get domain :param))))
                (r (and (consp store) (or (plist-get store (eas-key (or (plist-get domain :encoding) (eas-key-name channel))))
                                          (plist-get store :x) (plist-get store :y)))))
           (and (vectorp r) (vector (min (aref r 0) (aref r 1)) (max (aref r 0) (aref r 1))))))))

(defun eas-compile--dimension-p (pairs channel)
  "Non-nil when CHANNEL is the dimension of a bar, area or line in PAIRS.
The dimension is not the measure; Vega-Lite does not extend dimension
scales to zero."
  (seq-some (lambda (p)
              (let* ((u (car p)) (enc (plist-get u :encoding))
                     (type (plist-get (plist-get u :mark) :type))
                     (x (plist-get enc :x)) (y (plist-get enc :y))
                     (horizontal (or (and x y (eas-encode-discrete-p y) (not (eas-encode-discrete-p x)))
                                     ;; x alone, continuous: a horizontal bar's measure.
                                     (and x (null y) (not (eas-encode-discrete-p x))))))
                (and (member type '("bar" "area" "line" "trail"))
                     (eq channel (if horizontal :y :x)))))
            pairs))

(defun eas-compile--num (type v)
  "Domain bound V as a number for scale TYPE."
  (if (equal type "time") (eas-time-parse v) v))

(defvar eas-compile-scales-text nil
  "Non-nil while a plan is compiled for the text target.
Character cells have no room for a rect band's fractional outer padding.")

(defun eas-compile-position-scale (units channel zoom)
  "Scale for positional CHANNEL shared by UNITS, or nil.
ZOOM is a [LO HI] domain from view state, or nil."
  (when-let* ((pairs (eas-compile--defs units channel)))
    (let* ((type (eas-compile--scale-type pairs channel))
           ;; Vega-Lite keeps null as a category of a discrete domain.
           (values (eas-compile--values pairs channel (member type '("band" "point"))))
           (sp (plist-get (cdar pairs) :scale)))
      (if (member type '("band" "point"))
          (let* ((only (lambda (type) (seq-every-p (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) type))
                                                   pairs)))
                 ;; A layer takes its band paddings from its first band mark.
                 (first (seq-some (lambda (p) (car (member (plist-get (plist-get (car p) :mark) :type)
                                                           '("bar" "rect" "tick"))))
                                  pairs))
                 (rect (equal first "rect"))
                 ;; Vega-Lite: rect bands touch; tick bands pad 0.25 inside, 0.125 outside.
                 (tick (and (equal type "band") (funcall only "tick")))
                 ;; and bands holding a nested offset pad 0.2 both ways.
                 (nested (and (equal type "band") (seq-some (lambda (p) (eas-compile--offset-p p channel)) pairs)))
                 (cfg (and (equal type "band") (plist-get (plist-get (plist-get (car (car pairs)) :ctx) :config) :scale)))
                 (inner (or (plist-get sp :paddingInner) (plist-get sp :padding)
                            (and nested 0.2)
                            ;; Vega-Lite: bandPaddingInner, else the first band mark's own.
                            (plist-get cfg :bandPaddingInner)
                            (plist-get cfg (pcase first ("rect" :rectBandPaddingInner) ("bar" :barBandPaddingInner)
                                             ("tick" :tickBandPaddingInner)))
                            (cond (rect 0) (tick 0.25) (nested 0.2))))
                 (outer (or (plist-get sp :paddingOuter) (plist-get sp :padding)
                            (and nested 0.2) (plist-get cfg :bandPaddingOuter)
                            ;; Vega-Lite: paddingInner / 2 (0.125 for a tick's default 0.25,
                            ;; 0 for touching rects), so a step size spans whole pixels.
                            (cond ((and rect eas-compile-scales-text) 0)
                                  ((or rect tick) (/ inner 2.0)) (nested 0.2)))))
            (append (eas-scale-band type (eas-compile--discrete-domain pairs values) [0 1] inner outer)
                    (list :field (plist-get (cdar pairs) :field) :padding-inner inner :padding-outer outer)
                    (and (eq (plist-get sp :round) t) (list :round t))))
        (eas-compile--continuous type pairs channel values zoom)))))

(defun eas-compile--field-missing-p (pairs)
  "Non-nil when a row of PAIRS lacks its def's field altogether."
  (seq-some (lambda (p)
              (when-let* ((k (and (plist-get (cdr p) :field) (eas-encode-field (cdr p)))))
                (seq-some (lambda (row) (not (plist-member row k))) (plist-get (car p) :rows))))
            pairs))

(defun eas-compile-color-scale (units &optional config channels)
  "Return (CHANNEL DEF SCALE) for the first field-mapped color channel of UNITS.
CHANNELS, default color, fill and stroke, are the ones tried, in order.
Ranges come from CONFIG's range.category, .heatmap and .ramp."
  (cl-loop for channel in (or channels '(:color :fill :stroke))
           ;; A nominal datum (a repeat's field name) joins the scale too.
           ;; scale: null uses the field's values as the colors themselves.
           for pairs = (seq-filter (lambda (p) (and (not (eq (plist-get (cdr p) :scale) :null))
                                                    (or (plist-get (cdr p) :field)
                                                        (and (plist-member (cdr p) :datum) (eas-encode-discrete-p (cdr p))))))
                                   (eas-compile--defs units channel))
           when pairs
           return (let* ((def (cdar pairs)) (sp (plist-get def :scale))
                         (values (eas-compile--values pairs channel)))
                    ;; Vega keeps a row's missing (undefined) field as a category of
                    ;; its own, first in order: the domain holds it as nil.
                    (when (and (eas-encode-discrete-p def) (eas-compile--field-missing-p pairs))
                      (push nil values))
                    (list channel def
                          (if (eas-scale-discretize-p def)
                              (eas-scale-discretize-make def values channel config)
                          (if (eas-encode-discrete-p def)
                              (let ((domain (if (vectorp (plist-get sp :domain)) (plist-get sp :domain)
                                              (eas-compile--discrete-domain pairs values))))
                                (append (eas-scale-ordinal domain
                                                           (or (eas-compile--range (plist-get sp :range) (plist-get (caar pairs) :env))
                                                               (and (plist-get sp :scheme)
                                                                    (eas-scheme-discrete-range (plist-get sp :scheme) (length domain)))
                                                               (and (equal (plist-get def :type) "ordinal")
                                                                    (eas-compile--config-range config :ordinal (length domain)))
                                                               (eas-compile--config-range config :category (length domain))
                                                               eas-scale-tableau10))
                                        (list :field (plist-get def :field))))
                            (let ((nums (if (equal (plist-get def :type) "temporal")
                                            (delq nil (mapcar #'eas-time-parse values))
                                          (seq-filter #'numberp values))))
                              (eas-scale-sequential-clamp
                               sp
                               (list :type "sequential"
                                    :domain (eas-scale-sequential-domain sp nums)
                                    :mid (plist-get sp :domainMid)
                                    ;; Vega-Lite interpolates an explicit range in HCL.
                                    :interpolate (and (vectorp (plist-get sp :range)) "hcl")
                                    :range (eas-scale-sequential-range
                                            sp
                                            (or (and (vectorp (plist-get sp :range))
                                                    (vconcat (mapcar (lambda (c) (or (eas-color-hex c) c)) (plist-get sp :range))))
                                               (and (plist-get sp :scheme) (eas-scheme-ramp (plist-get sp :scheme)))
                                               (and (plist-get sp :domainMid)
                                                    (or (eas-compile--config-range config :diverging) eas-scale-blueorange-reversed))
                                               ;; Vega-Lite: config.range.heatmap for rect, ramp otherwise.
                                               (if (seq-some (lambda (p) (equal (plist-get (plist-get (car p) :mark) :type) "rect"))
                                                             pairs)
                                                   (or (eas-compile--config-range config :heatmap)
                                                       eas-scale-yellowgreenblue)
                                                 (or (eas-compile--config-range config :ramp) eas-scale-blues))))
                                    :field (plist-get def :field))))))))))

(defconst eas-compile-dash-range [[1 0] [4 2] [2 1] [1 1] [1 2 4 2]]
  "Vega-Lite's default strokeDash range (config.range.strokeDash).")

(defun eas-compile-dash-scale (units &optional config)
  "Ordinal scale for UNITS' field-mapped strokeDash channel, or nil.
Its range is the scale's own, else CONFIG's range.strokeDash, else
`eas-compile-dash-range'."
  (when-let* ((pairs (seq-filter (lambda (p) (plist-get (cdr p) :field)) (eas-compile--defs units :strokeDash))))
    (let* ((def (cdar pairs)) (sp (plist-get def :scale))
           (configured (plist-get (plist-get config :range) :strokeDash)))
      (append (eas-scale-ordinal (eas-compile--discrete-domain pairs (eas-compile--values pairs :strokeDash))
                                 (cond ((vectorp (plist-get sp :range)) (plist-get sp :range))
                                       ((and (vectorp configured) (> (length configured) 0)) configured)
                                       (t eas-compile-dash-range)))
              (list :field (plist-get def :field))))))

(defun eas-compile--config-range (config key &optional count)
  "Return CONFIG's range KEY as an array of colors, or nil.
A {scheme} range is sampled COUNT times for a discrete scale, else it
gives its ramp stops."
  (let ((r (plist-get (plist-get config :range) key)))
    (cond ((and (vectorp r) (> (length r) 0)) r)
          ((and (eas-object-p r) (plist-get r :scheme))
           (if count (eas-scheme-discrete-range (plist-get r :scheme) count)
             (eas-scheme-ramp (plist-get r :scheme)))))))

(defun eas-compile-aux-scale (units channel range)
  "Linear scale for CHANNEL (size or opacity) of UNITS onto RANGE, or nil.
The scale's own range wins; a trail's size is its width, onto
Vega-Lite's [minStrokeWidth, maxStrokeWidth] = [1, 4]."
  (when-let* ((pairs (seq-filter (lambda (p) (plist-get (cdr p) :field)) (eas-compile--defs units channel))))
    (or
     (eas-scale-discretize-make (cdar pairs) (eas-compile--values pairs channel) channel
                                (plist-get (plist-get (caar pairs) :ctx) :config)
                                (plist-get (plist-get (caar pairs) :mark) :type))
    (let* ((nums (seq-filter #'numberp (eas-compile--values pairs channel)))
           (sp (plist-get (cdar pairs) :scale))
           ;; {"expr": E} entries of an explicit range read the params.
           (own (eas-compile--range (plist-get sp :range) (plist-get (caar pairs) :env)))
           (range (cond ((and own (= (length own) 2)) own)
                        ((and (eq channel :size) (equal (plist-get (plist-get (caar pairs) :mark) :type) "trail")) [1 4])
                        (t range)))
           (range (vector (or (plist-get sp :rangeMin) (aref range 0))
                          (or (plist-get sp :rangeMax) (aref range 1))))
           (explicit (plist-get sp :domain)))
      (cond
       ;; A domain and range of more than two stops: piecewise linear.
       ((and (vectorp explicit) (> (length explicit) 2) own (= (length own) (length explicit)))
        (list :type "linear" :domain (vconcat (mapcar #'float explicit)) :range own :field (plist-get (cdar pairs) :field)))
       ((and (vectorp explicit) (numberp (aref explicit 0)))
        (eas-scale-continuous "linear" (aref explicit 0) (aref explicit 1) range :field (plist-get (cdar pairs) :field)))
       (t
        (eas-scale-continuous "linear" (if (eq channel :size) 0 (if nums (apply #'min nums) 0))
                              (if nums (apply #'max nums) 1) range :field (plist-get (cdar pairs) :field))))))))

(defun eas-compile-set-range (scale range)
  "Return SCALE mapped onto RANGE (recomputing band geometry).
A continuous :padding P (pixels) widens the domain about its centre so
the data spans the range less P on each side, like Vega's padDomain."
  (let ((range (if (eq (plist-get scale :reverse) t) (vector (aref range 1) (aref range 0)) range)))
    (if (member (plist-get scale :type) '("band" "point"))
        (eas-scale-round-band
         (append (eas-scale-band (plist-get scale :type) (plist-get scale :domain) range
                                 (plist-get scale :padding-inner) (plist-get scale :padding-outer))
                 (list :field (plist-get scale :field))
                 ;; Keep the paddings, so setting the range again keeps them too.
                 (cl-loop for key in '(:padding-inner :padding-outer)
                          when (plist-get scale key) append (list key (plist-get scale key)))
                 (and (eq (plist-get scale :round) t) (list :round t))))
      (let ((out (plist-put (copy-sequence scale) :range range))
            (pad (plist-get scale :padding))
            (span (abs (- (aref range 1) (aref range 0)))))
        (when (and pad (> span (* 2 pad)) (member (plist-get scale :type) '("linear" "time" "utc")))
          (let* ((d (or (plist-get scale :raw-domain) (plist-get scale :domain))) (c (/ (+ (aref d 0) (aref d 1)) 2.0))
                 (frac (/ span (- span (* 2.0 pad)))))
            (setq out (plist-put out :domain (vector (+ c (* frac (- (aref d 0) c))) (+ c (* frac (- (aref d 1) c))))))
            (when (plist-get scale :raw-domain)
              (let ((d (plist-get out :domain)))
                (setq out (plist-put out :domain (plist-get (eas-scale-continuous "linear" (aref d 0) (aref d 1) range
                                                                                  :nice t)
                                                            :domain)))))
            (setq out (plist-put out :padding nil))))
        (when (and pad (> span (* 2 pad)) (equal (plist-get scale :type) "log"))
          (setq out (plist-put out :domain (eas-bins-pad-log (plist-get scale :domain) (/ span (- span (* 2.0 pad)))
                                                             (plist-get scale :renice))))
          (setq out (plist-put out :padding nil)))
        out))))

(provide 'eas-compile-scales)
;;; eas-compile-scales.el ends here

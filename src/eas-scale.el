;;; eas-scale.el --- invertible scales, nice domains and ticks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Scales are plain JSON-ready plists so they can live in the scene:
;;
;;   (:type "linear"|"log"|"time"|"utc" :domain [LO HI] :range [R0 R1])
;;   (:type "band"|"point" :domain [V ...] :range [R0 R1]
;;    :step S :bandwidth B :start X0)
;;   (:type "ordinal" :domain [V ...] :range [COLOR ...])
;;
;; plus :zero, :nice and :field for provenance.  `eas-scale-apply'
;; maps data to range, `eas-scale-invert' maps back (zoom, brush and
;; crosshair work in data space through it).  Nice domains and ticks
;; port d3-array's tickIncrement, so they match Vega's.

;;; Code:

(require 'eas-core)
(require 'eas-format)
(require 'eas-time)
(require 'eas-color)
(require 'eas-scale-time)

(defconst eas-scale--e10 (sqrt 50.0))
(defconst eas-scale--e5 (sqrt 10.0))
(defconst eas-scale--e2 (sqrt 2.0))

(defun eas-scale--tick-spec (start stop count)
  "d3's tickSpec: (I1 I2 INC) for START..STOP with about COUNT ticks."
  (let* ((step (/ (- stop start) (float (max 0 count))))
         (power (floor (log step 10)))
         (err (/ step (expt 10.0 power)))
         (factor (cond ((>= err eas-scale--e10) 10) ((>= err eas-scale--e5) 5)
                       ((>= err eas-scale--e2) 2) (t 1)))
         i1 i2 inc)
    (if (< power 0)
        (progn (setq inc (/ (expt 10.0 (- power)) factor)
                     i1 (round (* start inc)) i2 (round (* stop inc)))
               (when (< (/ i1 inc) start) (setq i1 (1+ i1)))
               (when (> (/ i2 inc) stop) (setq i2 (1- i2)))
               (setq inc (- inc)))
      (setq inc (* (expt 10.0 power) factor)
            i1 (round (/ start inc)) i2 (round (/ stop inc)))
      (when (< (* i1 inc) start) (setq i1 (1+ i1)))
      (when (> (* i2 inc) stop) (setq i2 (1- i2))))
    (if (and (< i2 i1) (<= 0.5 count) (< count 2))
        (eas-scale--tick-spec start stop (* count 2))
      (list i1 i2 inc))))

(defun eas-scale-tick-increment (start stop count)
  "d3.tickIncrement: negative for 1/step, positive for step."
  (nth 2 (eas-scale--tick-spec start stop count)))

(defun eas-scale-linear-ticks (start stop count)
  "d3.ticks: about COUNT round values in START..STOP, ascending."
  (cond
   ((or (<= count 0) (not (and (numberp start) (numberp stop)))) nil)
   ((= start stop) (list start))
   (t (let* ((reverse (< stop start))
             (spec (if reverse (eas-scale--tick-spec stop start count)
                     (eas-scale--tick-spec start stop count)))
             (i1 (nth 0 spec)) (i2 (nth 1 spec)) (inc (nth 2 spec))
             (ticks (cl-loop for i from i1 to i2
                             collect (if (< inc 0) (/ i (- inc)) (* i inc)))))
        (if reverse (nreverse ticks) ticks)))))

(defun eas-scale-nice-linear (lo hi &optional count)
  "d3 linear.nice: extend LO..HI outward to round values; return (LO . HI)."
  (let ((count (or count 10)) (prestep nil) (step nil) (iter 10) (done nil))
    (if (or (= lo hi) (not (and (numberp lo) (numberp hi))))
        (cons lo hi)
      (while (and (not done) (> iter 0))
        (setq iter (1- iter)
              step (eas-scale-tick-increment lo hi count))
        (cond ((and prestep (= step prestep)) (setq done t))
              ((> step 0) (setq lo (* (floor (/ lo step)) step) hi (* (ceiling (/ hi step)) step)))
              ((< step 0) (setq lo (/ (ceiling (* lo step)) step) hi (/ (floor (* hi step)) step)))
              (t (setq done t)))
        (setq prestep step))
      (cons (if (zerop lo) 0.0 (float lo)) (if (zerop hi) 0.0 (float hi))))))

;;; Construction

(defun eas-scale-continuous (type lo hi range &rest props)
  "Return a continuous scale of TYPE over LO..HI onto RANGE with PROPS.
PROPS may set :zero and :nice, applied here, plus provenance keys."
  (let ((lo (float lo)) (hi (float hi)))
    (when (and (plist-get props :zero) (member type '("linear")))
      (setq lo (if (>= lo 0) 0.0 lo) hi (if (<= hi 0) 0.0 hi)))
    (when (= lo hi)
      (if (member type '("time" "utc"))
          (setq lo (- lo 43200000.0) hi (+ hi 43200000.0))
        (setq lo (- lo (if (zerop lo) 1.0 (* 0.5 (abs lo))))
              hi (+ hi (if (zerop hi) 1.0 (* 0.5 (abs hi)))))))
    (when (plist-get props :nice)
      (pcase type
        ("linear" (let ((n (eas-scale-nice-linear lo hi 10))) (setq lo (car n) hi (cdr n))))
        ("log" (setq lo (expt 10.0 (floor (log lo 10))) hi (expt 10.0 (ceiling (log hi 10)))))))
    (append (list :type type :domain (vector lo hi) :range range)
            (eas--plist-without (eas--plist-without props :zero) :nice))))

(defun eas-scale-band (type domain range &optional padding-inner padding-outer)
  "Return a band or point scale (TYPE) of DOMAIN values over RANGE.
Defaults follow Vega-Lite: band inner 0.1, outer 0.05; point padding 0.5."
  (let* ((n (length domain))
         (r0 (aref range 0)) (r1 (aref range 1))
         (start (float (min r0 r1))) (stop (float (max r0 r1)))
         (point (equal type "point"))
         (inner (if point 1.0 (or padding-inner 0.1)))
         (outer (or padding-outer (if point 0.5 (/ inner 2))))
         (step (/ (- stop start) (max 1.0 (+ (- n inner) (* 2 outer)))))
         (start (+ start (* 0.5 (- stop start (* step (- n inner))))))
         (bandwidth (if point 0.0 (* step (- 1 inner)))))
    (list :type type :domain domain :range range :step step :bandwidth bandwidth
          :start start :reverse (if (< r1 r0) t :false))))

(defun eas-scale-ordinal (domain range)
  "Return an ordinal scale mapping DOMAIN values to RANGE values cyclically."
  (list :type "ordinal" :domain domain :range range))

;;; Apply / invert

(defun eas-scale--lerp (lo hi range v)
  "Map V from LO..HI linearly onto RANGE (a 2-vector)."
  (let ((r0 (aref range 0)) (r1 (aref range 1)))
    (if (= lo hi) (/ (+ r0 r1) 2.0)
      (+ r0 (* (- r1 r0) (/ (float (- v lo)) (- hi lo)))))))

(defun eas-scale--index (scale v)
  "Index of V in discrete SCALE's domain, or nil."
  (seq-position (plist-get scale :domain) v #'equal))

(defun eas-scale-fn (scale)
  "Return a function mapping data values through SCALE, precomputed.
Equivalent to `eas-scale-apply' but without per-call dispatch; used
in compile's per-row loops."
  (let ((domain (plist-get scale :domain)) (range (plist-get scale :range)))
    (pcase (plist-get scale :type)
      ((or "linear" "time" "utc")
       (let* ((d0 (float (aref domain 0))) (d1 (float (aref domain 1)))
              (r0 (aref range 0)) (r1 (aref range 1))
              (k (if (= d0 d1) 0.0 (/ (- r1 r0) (- d1 d0))))
              (mid (/ (+ r0 r1) 2.0))
              (time (member (plist-get scale :type) '("time" "utc"))))
         (lambda (v)
           (let ((v (if (and time (not (numberp v))) (eas-time-parse v) v)))
             (and (numberp v) (if (= d0 d1) mid (+ r0 (* k (- v d0)))))))))
      ((or "band" "point" "ordinal")
       (let ((index (make-hash-table :test 'equal)) (i 0))
         (seq-doseq (v domain) (unless (gethash v index) (puthash v i index)) (setq i (1+ i)))
         (if (equal (plist-get scale :type) "ordinal")
             (let ((n (length range)))
               (lambda (v) (let ((j (gethash v index))) (and j (> n 0) (aref range (mod j n))))))
           (let ((start (plist-get scale :start)) (step (plist-get scale :step))
                 (rev (eq (plist-get scale :reverse) t)) (n (length domain)))
             (lambda (v) (let ((j (gethash v index)))
                           (and j (+ start (* step (if rev (- n 1 j) j))))))))))
      (_ (lambda (v) (eas-scale-apply scale v))))))

(defun eas-scale-apply (scale value)
  "Map data VALUE through SCALE; nil when VALUE has no position."
  (let ((domain (plist-get scale :domain)))
    (pcase (plist-get scale :type)
      ("linear" (and (numberp value)
                     (eas-scale--lerp (aref domain 0) (aref domain 1) (plist-get scale :range) value)))
      ("log" (and (numberp value) (> value 0)
                  (eas-scale--lerp (log (aref domain 0)) (log (aref domain 1))
                                     (plist-get scale :range) (log value))))
      ((or "sqrt" "pow")
       (and (numberp value)
            (let ((e (eas-scale--exponent scale)))
              (eas-scale--lerp (eas-scale--pow (aref domain 0) e) (eas-scale--pow (aref domain 1) e)
                               (plist-get scale :range) (eas-scale--pow value e)))))
      ((or "time" "utc")
       (let ((ms (eas-time-parse value)))
         (and ms (eas-scale--lerp (aref domain 0) (aref domain 1) (plist-get scale :range) ms))))
      ((or "band" "point")
       (when-let* ((i (eas-scale--index scale value)))
         (let ((n (length domain)))
           (+ (plist-get scale :start)
              (* (plist-get scale :step)
                 (if (eq (plist-get scale :reverse) t) (- n 1 i) i))))))
      ("ordinal"
       (let ((i (eas-scale--index scale value)) (range (plist-get scale :range)))
         (and i (> (length range) 0) (aref range (mod i (length range))))))
      ("sequential" (eas-scale-color-ramp scale value)))))

(defun eas-scale-invert (scale px)
  "Map range position PX back to data space through SCALE.
Continuous scales return a number (epoch ms for time); band and point
scales return the domain value whose band is nearest PX."
  (let ((domain (plist-get scale :domain)) (range (plist-get scale :range)))
    (pcase (plist-get scale :type)
      ((or "linear" "time" "utc")
       (eas-scale--lerp (aref range 0) (aref range 1) domain px))
      ("log" (exp (eas-scale--lerp (aref range 0) (aref range 1)
                                     (vector (log (aref domain 0)) (log (aref domain 1))) px)))
      ((or "sqrt" "pow")
       (let ((e (eas-scale--exponent scale)))
         (eas-scale--pow (eas-scale--lerp (aref range 0) (aref range 1)
                                          (vector (eas-scale--pow (aref domain 0) e) (eas-scale--pow (aref domain 1) e))
                                          px)
                         (/ 1.0 e))))
      ((or "band" "point")
       (when (> (length domain) 0)
         (let ((half (/ (plist-get scale :bandwidth) 2.0)))
           (eas--min-by (lambda (v) (abs (- px (+ half (eas-scale-apply scale v)))))
                           domain)))))))

(defun eas-scale--exponent (scale)
  "The exponent of a sqrt or pow SCALE."
  (if (equal (plist-get scale :type) "sqrt") 0.5 (or (plist-get scale :exponent) 1)))

(defun eas-scale--pow (v e)
  "Sign-preserving V to the power E, as d3's pow scale computes it."
  (if (< v 0) (- (expt (- (float v)) e)) (expt (float v) e)))

(defun eas--min-by (fn seq)
  "Return the element of SEQ minimizing FN (first on ties)."
  (let (best best-key)
    (seq-doseq (x seq)
      (let ((k (funcall fn x)))
        (when (or (null best-key) (< k best-key)) (setq best x best-key k))))
    best))

(defun eas-scale-span (scale)
  "Return (LO . HI) of SCALE's range."
  (let ((r (plist-get scale :range)))
    (cons (min (aref r 0) (aref r 1)) (max (aref r 0) (aref r 1)))))

;;; Color ramp

(defconst eas-scale-blues
  ["#cfe1f2" "#bed8ec" "#a8cee5" "#8fc1de" "#74b2d7" "#5ba3cf"
   "#4592c6" "#3181bd" "#206fb2" "#125ca4" "#0a4a90"]
  "Vega's \"blues\" scheme, the default quantitative color ramp.")

(defconst eas-scale-yellowgreenblue
  ["#eff9bd" "#dbf1b4" "#bde5b5" "#94d5b9" "#69c5be" "#45b4c2"
   "#2c9ec0" "#2182b8" "#2163aa" "#23479c" "#1c3185"]
  "Vega's \"yellowgreenblue\" scheme, Vega-Lite's heatmap ramp.")

(defconst eas-scale-blueorange-reversed
  ["#994a07" "#c5690d" "#e8932f" "#fbbf74" "#fce0ba" "#f2f0eb"
   "#d2e5ef" "#9dcae1" "#5da2cb" "#2f78b3" "#134b85"]
  "Vega's \"blueorange\" scheme over extent [1, 0], Vega-Lite's diverging ramp.")

(defconst eas-scale-tableau10
  ["#4c78a8" "#f58518" "#e45756" "#72b7b2" "#54a24b"
   "#eeca3b" "#b279a2" "#ff9da6" "#9d755d" "#bab0ac"]
  "Vega's \"tableau10\" scheme, the default categorical palette.")

(defun eas-scale--hex (color)
  "Return COLOR \"#rrggbb\" as a list of three integers."
  (list (string-to-number (substring color 1 3) 16)
        (string-to-number (substring color 3 5) 16)
        (string-to-number (substring color 5 7) 16)))

(defun eas-scale-color-ramp (scale value)
  "Interpolate SCALE's :range color stops at numeric VALUE.
With a :mid (Vega-Lite's domainMid) the scale is diverging: each half of
the domain spans half the ramp, interpolated in HCL as Vega does."
  (if-let* ((mid (and (numberp value) (plist-get scale :mid))))
      (let* ((domain (plist-get scale :domain)) (lo (aref domain 0)) (hi (aref domain 1))
             (tt (+ 0.5 (* 0.5 (if (< value mid) (if (= mid lo) 0 (/ (- value mid) (float (- mid lo))))
                                 (if (= hi mid) 0 (/ (- value mid) (float (- hi mid)))))))))
        (eas-color-piecewise-hcl (plist-get scale :range) tt))
  (when (numberp value)
    (let* ((domain (plist-get scale :domain)) (stops (plist-get scale :range))
           (lo (aref domain 0)) (hi (aref domain 1))
           (tt (if (= lo hi) 0.5 (max 0.0 (min 1.0 (/ (- value lo) (float (- hi lo)))))))
           (pos (* tt (1- (length stops))))
           (i (min (floor pos) (- (length stops) 2)))
           (f (- pos i))
           (a (eas-scale--hex (aref stops i))) (b (eas-scale--hex (aref stops (1+ i)))))
      (apply #'format "#%02x%02x%02x"
             (cl-mapcar (lambda (x y) (round (+ x (* f (- y x))))) a b))))))

;;; Ticks and formats

(defun eas-scale-ticks (scale count)
  "Return about COUNT tick values for SCALE (the domain for discrete)."
  (let ((domain (plist-get scale :domain)))
    (pcase (plist-get scale :type)
      ((or "linear" "sqrt" "pow") (eas-scale-linear-ticks (aref domain 0) (aref domain 1) count))
      ("log" (eas-scale-log-ticks (aref domain 0) (aref domain 1) count))
      ((or "time" "utc") (let ((eas-time-zone (eas-scale--zone scale)))
                           (eas-scale-time-ticks (aref domain 0) (aref domain 1) count)))
      (_ (append domain nil)))))

(defun eas-scale-log-ticks (lo hi &optional count)
  "d3 log.ticks for LO..HI (base 10): every k*10^i when the span has fewer
decades than COUNT (default 10), else powers of ten."
  (let* ((count (or count 10)) (i (log lo 10)) (j (log hi 10)))
    (if (< (- j i) count)
        (let ((z (cl-loop for p from (floor i) to (ceiling j)
                          append (cl-loop for k from 1 below 10
                                          for v = (if (< p 0) (/ k (expt 10.0 (- p))) (* k (expt 10.0 p)))
                                          when (<= (* lo (- 1 1e-12)) v (* hi (+ 1 1e-12))) collect v))))
          (if (< (* 2 (length z)) count) (eas-scale-linear-ticks lo hi count) z))
      (mapcar (lambda (e) (expt 10.0 e))
              (eas-scale-linear-ticks (floor i) (ceiling j) (min (- (ceiling j) (floor i)) count))))))

(defun eas-scale-log-label-p (v ticks count)
  "d3's log tickFormat filter: label tick V only when its mantissa is small."
  (let* ((k (max 1 (/ (* 10.0 count) (max 1 (length ticks)))))
         (m (/ v (expt 10.0 (round (log v 10)))))
         (m (if (< (* m 10) 9.5) (* m 10) m)))
    (<= m k)))

(defun eas-scale--group-thousands (digits)
  "Insert commas into the integer DIGITS string."
  (let ((out "") (n (length digits)))
    (dotimes (i n)
      (when (and (> i 0) (zerop (% (- n i) 3))) (setq out (concat out ",")))
      (setq out (concat out (string (aref digits i)))))
    out))

(defun eas-scale-format-number (value decimals)
  "Format VALUE like d3.format(\",.Nf\") with N = DECIMALS."
  (let* ((text (format (format "%%.%df" decimals) (abs value)))
         (parts (split-string text "\\."))
         (negative (and (< value 0) (string-match-p "[1-9]" text))))
    (concat (if negative "−" "")
            (eas-scale--group-thousands (car parts))
            (if (cadr parts) (concat "." (cadr parts)) ""))))

(defun eas-scale-tick-decimals (scale count)
  "Decimals d3's tickFormat uses for SCALE's ticks at COUNT."
  (let ((domain (plist-get scale :domain)))
    (if (not (equal (plist-get scale :type) "linear")) 0
      (let ((step (abs (eas-scale-tick-increment (aref domain 0) (aref domain 1) count))))
        (if (zerop step) 0
          (let ((step (if (< (eas-scale-tick-increment (aref domain 0) (aref domain 1) count) 0)
                          (/ 1.0 step) step)))
            (max 0 (- (floor (log step 10))))))))))

(defun eas-scale--zone (scale)
  "The time zone SCALE ticks and labels in: none (UTC) for utc scales."
  (unless (or (equal (plist-get scale :type) "utc") (eq (plist-get scale :utc) t)) eas-time-zone))

(defun eas-scale-tick-format (scale count &optional format)
  "Return a function formatting SCALE's tick values at COUNT.
FORMAT is a Vega-Lite format string; only d3 \",.Nf\"/\".N%\" style
and strftime-style time formats are honored."
  (pcase (plist-get scale :type)
    ((or "time" "utc")
     (let ((zone (eas-scale--zone scale)))
       (if format
           (lambda (v) (let ((eas-time-zone zone)) (eas-time-format v (eas-scale--d3-time-format format))))
         (lambda (v) (let ((eas-time-zone zone)) (eas-scale-time-multi-format v))))))
    ((or "linear" "log")
     (let ((decimals (eas-scale-tick-decimals scale count)))
       (cond
        ((and format (string-match "\\.\\([0-9]+\\)%" format))
         (let ((d (string-to-number (match-string 1 format))))
           (lambda (v) (concat (eas-scale-format-number (* 100 v) d) "%"))))
        ((and format (string-match "\\.\\([0-9]+\\)f" format))
         (let ((d (string-to-number (match-string 1 format))))
           (lambda (v) (eas-scale-format-number v d))))
        (format (lambda (v) (eas-format-number format v)))
        ((equal (plist-get scale :type) "log")
         (let ((ticks (eas-scale-ticks scale count)))
           (lambda (v) (if (eas-scale-log-label-p v ticks count)
                           (eas-scale-format-number v (max 0 (- (floor (+ 1e-9 (log v 10))))))
                         ""))))
        (t (lambda (v) (eas-scale-format-number v decimals))))))
    (_ (lambda (v) (cond ((stringp v) v) ((eq v :null) "null") (t (format "%s" v)))))))

(defun eas-scale--d3-time-format (format)
  "Translate d3 time FORMAT to `format-time-string' (they mostly agree)."
  (replace-regexp-in-string "%L" "%3N" (replace-regexp-in-string "%-" "%" format)))

(provide 'eas-scale)
;;; eas-scale.el ends here

;;; eas-marks.el --- scene items for each mark type -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Turns one compiled unit (rows, normalized encoding,
;; mark definition) plus its view's scales into scene items.  Every
;; item carries :datum, the index of its row in the mark's :rows, and
;; fully resolved style, tooltip and href, so renderers decide nothing.
;; Mark definitions arrive with their config defaults resolved
;; (`eas-marks-resolve-mark'); the remaining fallbacks are Vega-Lite's:
;; opacity 0.7 for point-like marks, line width 2, rule 1.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)
(require 'eas-lttb)
(require 'eas-layout)
(require 'eas-paint)
(require 'eas-curve)
(require 'eas-offset)
(require 'eas-time-band)
(require 'eas-marks-path)
(require 'eas-marks-image)
(require 'eas-marks-series)
(require 'eas-mark-style)
(require 'eas-marks-props)
(require 'eas-stack-band)
(require 'eas-bar-extra)

(defconst eas-marks-default-color "#4c78a8" "Vega-Lite's default mark color.")

(defun eas-marks--stroked-p (type mark)
  "Non-nil when mark TYPE (with MARK def) is drawn by stroke, not fill."
  (or (member type '("line" "rule" "tick")) (and (eq (plist-get mark :filled) :false) (not (equal type "text")))
      (and (equal type "point") (not (eq (plist-get mark :filled) t)))))

;;; Channel values

(defvar eas-marks--cache nil
  "Per-unit memo of channel accessors and scale functions, bound by
`eas-marks-items' so per-row work never re-resolves definitions.")

(defmacro eas-marks--memo (key &rest body)
  "Value of BODY memoized under KEY in the current unit's cache.
A hit allocates nothing, which matters in per-row loops."
  (declare (indent 1))
  (let ((k (make-symbol "key")) (hit (make-symbol "hit")))
    `(let* ((,k ,key)
            (,hit (if eas-marks--cache (gethash ,k eas-marks--cache :none) :none)))
       (if (not (eq ,hit :none)) ,hit
         (let ((value (progn ,@body)))
           (when eas-marks--cache (puthash ,k value eas-marks--cache))
           value)))))

(defun eas-marks--sfn (scales channel)
  "Precompiled function for CHANNEL's scale in SCALES, or nil."
  (eas-marks--memo (if (eq channel :x) 'scale-x (if (eq channel :y) 'scale-y (cons 'scale channel)))
    (when-let* ((s (plist-get scales channel))) (eas-scale-fn s))))

(defun eas-marks--accessor (unit scales channel)
  "Function ROW -> scaled CHANNEL value for UNIT, built once per unit."
  (eas-marks--memo channel
   (progn
     (let* ((def (plist-get (plist-get unit :encoding) channel))
            (sfn (eas-marks--sfn scales channel))
            (env (plist-get unit :env)))
       (cond
        ((null def) #'ignore)
        ((plist-get def :condition)
         (lambda (row)
           (let ((active (eas-encode-active def row env)))
             (cond ((plist-member active :value) (plist-get active :value))
                   ((or (plist-get active :field) (plist-member active :datum))
                    (let ((raw (eas-encode-raw active row))) (if sfn (funcall sfn raw) raw)))))))
        ((plist-member def :value) (let ((v (plist-get def :value))) (lambda (_) v)))
        ((plist-get def :field)
         (let ((key (eas-encode-field def)))
           (if sfn (lambda (row) (funcall sfn (plist-get row key))) (lambda (row) (plist-get row key)))))
        ((plist-member def :datum)
         (let ((v (if sfn (funcall sfn (plist-get def :datum)) (plist-get def :datum)))) (lambda (_) v)))
        (t #'ignore))))))

(defun eas-marks--channel (unit scales channel row)
  "Scaled value of CHANNEL for ROW in UNIT, honoring conditions and values."
  (funcall (eas-marks--accessor unit scales channel) row))

(defun eas-marks--style (unit scales row)
  "Return (:fill F :stroke S :opacity O) for ROW in UNIT.
When no style channel depends on the row, the plist is computed once
per unit and shared by every item."
  (if (eas-marks--memo 'style-varies
        (let ((enc (plist-get unit :encoding)))
          (seq-some (lambda (ch) (let ((d (plist-get enc ch)))
                                   (and d (or (plist-get d :field) (plist-get d :condition)))))
                    '(:color :fill :stroke :opacity :fillOpacity :strokeOpacity :strokeWidth))))
      (eas-marks--style-1 unit scales row)
    (eas-marks--memo 'style (eas-marks--style-1 unit scales row))))

(defun eas-marks--style-1 (unit scales row)
  "Compute ROW's style in UNIT."
  (let* ((mark (plist-get unit :mark)) (type (plist-get mark :type))
         (color (or (eas-marks--channel unit scales :color row)
                    (plist-get mark :color) eas-marks-default-color))
         (stroked (eas-marks--stroked-p type mark))
         (fill (or (eas-marks--channel unit scales :fill row) (plist-get mark :fill)
                   (if stroked "none" color)))
         (stroke (or (eas-marks--channel unit scales :stroke row) (plist-get mark :stroke)
                     (if stroked color "none")))
         (opacity (or (eas-marks--channel unit scales :opacity row) (plist-get mark :opacity)
                      (if (and (member type '("point" "circle" "square" "tick"))
                               (not (plist-get unit :aggregated)))
                          0.7 1))))
    (eas-paint-style (append (list :fill (if (eq fill :null) "none" fill) :stroke (if (eq stroke :null) "none" stroke) :opacity opacity)
                             (cl-loop for ch in '(:fillOpacity :strokeOpacity :strokeWidth)
                                      for v = (or (eas-marks--channel unit scales ch row)
                                                  ;; Point marks take their strokeWidth elsewhere.
                                                  (and (or (not (eq ch :strokeWidth)) (member type '("bar" "rect")))
                                                       (plist-get mark ch)))
                                      when (numberp v) append (list ch v))))))

(defun eas-marks--extras (unit row)
  "Tooltip and href for ROW in UNIT, as a plist (omitting absent ones)."
  (let ((tooltip (and (eas-marks--memo 'tooltip
                        (let ((enc (plist-get unit :encoding)))
                          (or (plist-get enc :tooltip)
                              (eas-true-p (plist-get (plist-get unit :mark) :tooltip)))))
                      (eas-encode-tooltip (plist-get unit :encoding) (plist-get unit :mark) row)))
        (href (let ((def (plist-get (plist-get unit :encoding) :href)))
                (if def (eas-encode-raw def row) (plist-get (plist-get unit :mark) :href)))))
    (append (when tooltip (list :tooltip tooltip))
            (when (stringp href) (list :href href)))))

(defun eas-marks--pos (unit scales channel row bounds &optional band-start)
  "Position on CHANNEL (:x or :y) for ROW; band centre unless BAND-START.
Returns the plot centre when the channel is absent."
  (let* ((def (plist-get (plist-get unit :encoding) channel))
         (scale (plist-get scales channel))
         (fixed (cond ((and (eas-object-p def) (plist-member def :value) (not (plist-get def :condition)))
                       (list (plist-get def :value)))
                      ((null def) (let ((m (plist-member (plist-get unit :mark) channel))) (and m (list (cadr m))))))))
    (cond
     ;; A value (encoding or mark property) is pixels from the plot's
     ;; top-left; "width" and "height" are its far edges.
     (fixed (let ((v (car fixed)) (x (eq channel :x)))
              (cond ((numberp v) (+ (aref bounds (if x 0 1)) v))
                    ((member v '("width" "height")) (+ (aref bounds (if x 0 1)) (aref bounds (if x 2 3)))))))
     ((null def) (if (eq channel :x) (+ (aref bounds 0) (/ (aref bounds 2) 2.0))
                   (+ (aref bounds 1) (/ (aref bounds 3) 2.0))))
     ;; Vega-Lite puts every mark but bars and rects mid-bin.
     ((and (plist-get def :bin-end) (not band-start)
           (not (member (plist-get (plist-get unit :mark) :type) '("bar" "rect"))))
      (let ((p (eas-marks--channel unit scales channel row))
            (q (eas-marks--secondary unit scales channel row)))
        (and (numberp p) (if (numberp q) (/ (+ p q) 2.0) p))))
     (t (let ((p (eas-stack-band-pos unit scales channel row (eas-marks--channel unit scales channel row)))
              (off (and (plist-get (plist-get unit :encoding) (if (eq channel :x) :xOffset :yOffset))
                        (eas-marks--channel unit scales (if (eq channel :x) :xOffset :yOffset) row))))
          (and (numberp p)
               (cond
                ((and (numberp off) scale (not band-start) (equal (plist-get scale :type) "band")
                      (equal (plist-get (plist-get scales (if (eq channel :x) :xOffset :yOffset)) :type) "band"))
                 ;; A discrete offset: the middle of its nested band.
                 (+ p off (/ (plist-get (plist-get scales (if (eq channel :x) :xOffset :yOffset)) :bandwidth) 2.0)))
                ((and (numberp off) scale (equal (plist-get scale :type) "band")
                      (not (equal (plist-get (plist-get scales (if (eq channel :x) :xOffset :yOffset)) :type) "band")))
                 ;; A nested offset places the mark within its band.
                 (+ p (* off (plist-get scale :bandwidth))))
                ((and scale (not band-start) (member (plist-get scale :type) '("band")))
                 (+ p (/ (plist-get scale :bandwidth) 2.0)))
                ;; Vega-Lite centres non-bar marks on a bin.
                ((and (not band-start) (plist-get def :bin-end))
                 (let ((q (eas-marks--secondary unit scales channel row)))
                   (if (numberp q) (/ (+ p q) 2.0) p)))
                (t p))))))))

(defun eas-marks--zero (scale bounds channel)
  "Baseline position for SCALE: zero when in domain, else the plot edge."
  (let ((z (and scale (equal (plist-get scale :type) "linear")
                (let ((d (plist-get scale :domain))) (<= (min (aref d 0) (aref d 1)) 0 (max (aref d 0) (aref d 1))))
                (eas-scale-apply scale 0))))
    (or z (if (eq channel :y) (+ (aref bounds 1) (aref bounds 3)) (aref bounds 0)))))

(defun eas-marks--secondary (unit scales channel row)
  "Position of CHANNEL's partner (:x2/:y2, stack start or bin end) for ROW."
  (let* ((enc (plist-get unit :encoding))
         (def (plist-get enc channel))
         (scale (plist-get scales channel))
         (partner (plist-get enc (if (eq channel :x) :x2 :y2))))
    (cond
     ((plist-get def :stack-start) (funcall (eas-marks--sfn scales channel)
                                            (plist-get row (eas-key (plist-get def :stack-start)))))
     (partner (and scale (funcall (eas-marks--sfn scales channel) (eas-encode-raw partner row))))
     ((plist-get def :bin-end) (funcall (eas-marks--sfn scales channel)
                                        (plist-get row (eas-key (plist-get def :bin-end))))))))

;;; Items per mark type

(defun eas-marks--each (unit fn)
  "Collect FN applied to (ROW INDEX) over UNIT's rows, dropping nils."
  (let ((i -1) out)
    (seq-doseq (row (plist-get unit :rows))
      (setq i (1+ i))
      (when-let* ((item (funcall fn row i))) (push item out)))
    (vconcat (nreverse out))))

(defun eas-marks--mark-value (unit key row)
  "UNIT's mark property KEY for ROW; an {\"expr\": E} value is evaluated on ROW."
  (let ((v (plist-get (plist-get unit :mark) key)))
    (if (and (eas-object-p v) (plist-get v :expr))
        (eas-expr-evaluate (plist-get v :expr) row (plist-get unit :env))
      v)))

(defun eas-marks--point-row (unit scales bounds metrics)
  "Row builder (ROW I -> item) for point, circle, square and text marks."
  (let* ((mark (plist-get unit :mark)) (type (plist-get mark :type))
         ;; Circles and squares are always filled: one styled unit, not one per row.
         (styled (if (member type '("circle" "square"))
                     (plist-put (copy-sequence unit) :mark (plist-put (copy-sequence mark) :filled t))
                   unit)))
    (lambda (row i)
       (let ((x (eas-marks--pos unit scales :x row bounds))
             (y (eas-marks--pos unit scales :y row bounds)))
         (when (and (numberp x) (numberp y))
           (append (let* ((xo (or (plist-get mark :xOffset) 0)) (yo (or (plist-get mark :yOffset) 0)))
                     (if (equal type "text")
                         ;; dx/dy shift the text in pixels (whole cells in a terminal).
                         (let ((dx (eas-marks--mark-value unit :dx row)) (dy (eas-marks--mark-value unit :dy row))
                               (cell (and (eas-layout-text-p metrics) (plist-get metrics :cell))))
                           (list :datum i
                                 :x (+ x xo (if (numberp dx) (if cell (* (aref cell 0) (round dx (aref cell 0))) dx) 0))
                                 :y (+ y yo (if (numberp dy) (if cell (* (aref cell 1) (round dy (aref cell 1))) dy) 0))))
                       (list :datum i :x (+ x xo) :y (+ y yo))))
                   (if (equal type "text")
                       (let* ((text (eas-marks--channel unit scales :text row))
                              (size (if (eas-layout-text-p metrics) (aref (plist-get metrics :cell) 1)
                                      ;; Vega-Lite: size sets a text mark's font size.
                                      (or (eas-marks--channel unit scales :size row) (plist-get mark :fontSize) 11))))
                         (append
                          (eas-marks-props-text
                           unit row metrics (let ((v (or text (plist-get mark :text) "")))
                                              ;; An array is one line per element.
                                              (if (vectorp v) (mapconcat #'eas-expr--string v "\n")
                                                (eas-marks--text (plist-get (plist-get unit :encoding) :text) v)))
                           size)
                          (list :fontSize size
                               :align (let ((a (eas-marks--mark-value unit :align row))) (if (stringp a) a "center"))
                               :baseline (or (eas-marks--mark-value unit :baseline row) "middle")
                               :fill (eas-marks-props-text-fill unit scales row)
                               :opacity (or (plist-get mark :opacity) 1))
                          (when (plist-get mark :fontWeight) (list :fontWeight (plist-get mark :fontWeight)))
                          ;; font, fontStyle and the angle (channel or mark) the svg text takes.
                          (cl-loop for k in '(:font :fontStyle) for v = (plist-get mark k)
                                   when (stringp v) append (list k v))
                          (when-let* ((a (or (eas-marks--channel unit scales :angle row) (plist-get mark :angle))))
                            (when (and (numberp a) (/= a 0)) (list :angle a)))))
                     (append (list :size (or (eas-marks--channel unit scales :size row)
                                             (plist-get mark :size) 30)
                                   :shape (or (eas-marks--channel unit scales :shape row) (plist-get mark :shape)
                                              (if (equal type "point") "circle" type))
                                   :strokeWidth (or (eas-marks--channel unit scales :strokeWidth row)
                                                    (plist-get mark :strokeWidth) 2))
                             (when-let* ((a (or (eas-marks--channel unit scales :angle row) (plist-get mark :angle))))
                               (list :angle a))
                             (eas-marks--style styled scales row)))
                   (eas-marks--extras unit row)))))))

(defun eas-marks--text (def v)
  "Text mark string for value V of text channel DEF.\nDates read as Vega-Lite formats them."
  (let ((ms (and (member (plist-get def :type) '("temporal")) (eas-time-parse v))))
    (cond ((and ms (equal (plist-get def :derived) "timeUnit"))
           (let ((system-time-locale "C")) (eas-time-format ms (eas-layout-time-unit-format (plist-get def :field)))))
          (ms (eas-encode-format-value def v))
          ;; Quantitative text is d3-formatted (true minus sign), default or def.format.
          ((and (numberp v) (equal (plist-get def :type) "quantitative"))
           (eas-format-number (or (plist-get def :format) "") v))
          (t (eas-expr--string v)))))

(defun eas-marks--stack-ends (unit measure dim)
  "Hash of (DIM-VALUE . SIGN) -> the extreme MEASURE value of UNIT's stacks."
  (let* ((enc (plist-get unit :encoding)) (mdef (plist-get enc measure)) (ddef (plist-get enc dim))
         (key (eas-encode-field mdef)) (ends (make-hash-table :test 'equal)))
    (seq-doseq (row (plist-get unit :rows))
      (let ((v (plist-get row key)) (g (and ddef (eas-encode-raw ddef row))))
        (when (numberp v)
          (let* ((k (cons g (>= v 0))) (old (gethash k ends)))
            (when (or (null old) (if (>= v 0) (> v old) (< v old))) (puthash k v ends))))))
    ends))

(defun eas-marks--corners (unit horizontal)
  "Function ROW -> corner radii [TL TR BR BL] of UNIT's bar for ROW, or nil.
Vega-Lite rounds a stack's end by cornerRadiusEnd and both ends of a
ranged (x2/y2) bar."
  (let* ((mark (plist-get unit :mark)) (enc (plist-get unit :encoding))
         (r (or (plist-get mark :cornerRadiusEnd) 0)) (all (or (plist-get mark :cornerRadius) 0)))
    (cond
     ;; Per-corner radii: every bar gets them, each defaulting to cornerRadius.
     ((seq-some (lambda (k) (plist-get mark k))
                '(:cornerRadiusTopLeft :cornerRadiusTopRight :cornerRadiusBottomRight :cornerRadiusBottomLeft))
      (let ((c (vector (or (plist-get mark :cornerRadiusTopLeft) all) (or (plist-get mark :cornerRadiusTopRight) all)
                       (or (plist-get mark :cornerRadiusBottomRight) all) (or (plist-get mark :cornerRadiusBottomLeft) all))))
        (lambda (_) c)))
     ((and (zerop r) (zerop all)) nil)
     ((plist-get enc (if horizontal :x2 :y2))
      (let ((c (max r all))) (lambda (_) (vector c c c c))))
     (t (let* ((measure (if horizontal :x :y)) (dim (if horizontal :y :x))
               (key (eas-encode-field (plist-get enc measure)))
               (ddef (plist-get enc dim))
               (ends (eas-marks--stack-ends unit measure dim)))
          (lambda (row)
            (let ((v (plist-get row key)))
              (when (and (numberp v) (equal v (gethash (cons (and ddef (eas-encode-raw ddef row)) (>= v 0)) ends)))
                (pcase (list horizontal (>= v 0))
                  ('(nil t) (vector r r all all)) ('(nil nil) (vector all all r r))
                  ('(t t) (vector all r r all)) (_ (vector r all all r)))))))))))

(defun eas-marks--bar-row (unit scales bounds metrics)
  "Row builder (ROW I -> item) for bar and rect marks."
  (let* ((mark (plist-get unit :mark))
         (xs (plist-get scales :x)) (ys (plist-get scales :y))
         (xband (member (plist-get xs :type) '("band" "point")))
         (yband (member (plist-get ys :type) '("band" "point")))
         (enc (plist-get unit :encoding))
         (horizontal (pcase (plist-get mark :orient) ("horizontal" t) ("vertical" nil)
                       (_ (or (and yband (not xband)) (and (null (plist-get enc :y)) (plist-get enc :x) (not xband))))))
         (size (let ((s (plist-get mark :size))) (and (numberp s) s)))
         (text (eas-layout-text-p metrics))
         (corners (and (not text) (equal (plist-get mark :type) "bar") (eas-marks--corners unit horizontal)))
         (thin (if text (aref (plist-get metrics :cell) 0) 5)))
    (lambda (row i)
       (cl-flet ((span (channel scale band other-band)
                   (let* ((p (eas-marks--pos unit scales channel row bounds t))
                          (q (eas-marks--secondary unit scales channel row)))
                     (cond
                      ((null p) nil)
                      ;; A bar on a timeUnit spans the unit: its start to the next unit's start.
                      ((and (not band) (not q) (eas-time-band-p unit (plist-get enc channel)))
                       (let* ((span (eas-time-band-span (plist-get enc channel)
                                                        (eas-encode-raw (plist-get enc channel) row)))
                              (sfn (eas-marks--sfn scales channel))
                              (a (and span sfn (funcall sfn (car span)))) (b (and span sfn (funcall sfn (cdr span)))))
                         (when (and a b)
                           (let ((w (abs (- b a))) (lo (min a b)) (hi (max a b)))
                             (if (< w 0.25) (cons (- (+ lo 0.5) (* 0.5 (- 0.25 w))) (+ hi 0.5 (* 0.5 (- 0.25 w))))
                               (cons (+ lo 1) hi))))))
                      ;; A discrete offset band: the bar fills its nested band.
                      ((and band (eas-offset-shift unit scales channel row))
                       (let ((sh (eas-offset-shift unit scales channel row)))
                         (cons (+ p (car sh)) (+ p (car sh) (cdr sh)))))
                      ;; No channel: the bar spans the plot (Vega-Lite's
                      ;; full-range default), or mark.size about its middle.
                      ((null (plist-get enc channel))
                       (let ((lo (aref bounds (if (eq channel :x) 0 1)))
                             (ext (aref bounds (if (eq channel :x) 2 3))))
                         (if size (cons (- p (/ size 2.0)) (+ p (/ size 2.0))) (cons lo (+ lo ext)))))
                      ;; Ranged bars on a band or point scale span from band centre to centre.
                      ((and band q) (let ((h (/ (plist-get scale :bandwidth) 2.0))) (cons (+ (min p q) h) (+ (max p q) h))))
                      ;; A bar's size is its thickness, centred in the band.
                      ((and band (numberp (plist-get mark :size)) (not text))
                       (let ((c (+ p (/ (plist-get scale :bandwidth) 2.0))) (half (/ (plist-get mark :size) 2.0)))
                         (cons (- c half) (+ c half))))
                      (band (cons p (+ p (plist-get scale :bandwidth))))
                      (q (if (and (let ((d (plist-get (plist-get unit :encoding) channel))) (or (plist-get d :bin-end) (plist-get d :binned)))
                                  (not other-band))
                             ;; Vega-Lite's binSpacing (1 for bars, 0 for rects) between
                             ;; adjacent bins, both edges moved by the half-pixel translate.
                             (let ((s (or (plist-get mark :binSpacing) (if (equal (plist-get mark :type) "bar") 1 0))))
                               (if text (cons (+ (min p q) 1) (max p q))
                                 (cons (+ (min p q) 0.5 (/ s 2.0)) (- (+ (max p q) 0.5) (/ s 2.0)))))
                           (cons (min p q) (max p q))))
                      ((and (eq channel (if horizontal :x :y)) (not (equal (plist-get mark :type) "rect")))
                       (let ((z (eas-marks--zero scale bounds channel))) (cons (min p z) (max p z))))
                      (t (cons (- p (/ thin 2.0)) (+ p (/ thin 2.0))))))))
         (let ((x (eas-bar-extra-span unit scales :x (span :x xs xband yband) row text))
               (y (eas-bar-extra-span unit scales :y (span :y ys yband xband) row text)))
           (when (and x y)
             (append (list :datum i :x (car x) :y (car y) :w (- (cdr x) (car x)) :h (- (cdr y) (car y))
                           ;; Which edge grows with the value; text partial blocks use it.
                           :orient (cond ((equal (plist-get mark :type) "rect") "none")
                                         (horizontal "horizontal") (t "vertical")))
                     (when-let* ((c (and corners (funcall corners row)))) (list :corners c))
                     (eas-marks--style unit scales row)
                     (eas-marks-props-bar unit) (eas-bar-extra-stroke mark)
                     (eas-marks--extras unit row))))))))

(defun eas-marks--rule-row (unit scales bounds)
  "Row builder (ROW I -> item) for rule and tick marks."
  (let* ((enc (plist-get unit :encoding)) (mark (plist-get unit :mark))
         (tick (equal (plist-get mark :type) "tick"))
         (x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3)))
    (lambda (row i)
       (let* ((ys (plist-get scales :y)) (xs (plist-get scales :x))
              ;; Band centres, as for x/y; xOffset/x2Offset/yOffset/y2Offset shift in pixels.
              (shift (lambda (v scale key)
                       (and (numberp v)
                            (+ v (if (equal (plist-get scale :type) "band") (/ (plist-get scale :bandwidth) 2.0) 0)
                               (let ((o (plist-get mark key))) (if (numberp o) o 0))))))
              (x (and (plist-get enc :x) (let ((v (eas-marks--pos unit scales :x row bounds)))
                                           (and (numberp v) (+ v (let ((o (plist-get mark :xOffset))) (if (numberp o) o 0)))))))
              (y (and (plist-get enc :y) (let ((v (eas-marks--pos unit scales :y row bounds)))
                                           (and (numberp v) (+ v (let ((o (plist-get mark :yOffset))) (if (numberp o) o 0)))))))
              (x2 (and (plist-get enc :x2) (funcall shift (eas-marks--secondary unit scales :x row) xs :x2Offset)))
              (y2 (and (plist-get enc :y2) (funcall shift (eas-marks--secondary unit scales :y row) ys :y2Offset)))
              (xdisc (member (plist-get xs :type) '("band" "point")))
              (half (/ (or (plist-get mark :size)
                           (let ((s (if xdisc xs ys)))
                             (if (and s (> (or (plist-get s :bandwidth) 0) 0)) (plist-get s :bandwidth)
                               (or (plist-get mark :bandSize) 5))))
                       2.0))
              (seg (cond
                    ((and tick x y (not xdisc)) (vector x (- y half) x (+ y half)))
                    ((and tick x y) (vector (- x half) y (+ x half) y))
                    ;; Both ends given: a free segment (x,y)-(x2,y2).
                    ((and x y x2 y2) (vector x y x2 y2))
                    ((and x y y2) (vector x y x y2))
                    ((and x y x2) (vector x y x2 y))
                    ;; A range on one axis with no cross channel: across the plot's middle.
                    ((and x x2 (not (plist-get enc :y))) (vector x (+ y0 (/ h 2.0)) x2 (+ y0 (/ h 2.0))))
                    ((and y y2 (not (plist-get enc :x))) (vector (+ x0 (/ w 2.0)) y (+ x0 (/ w 2.0)) y2))
                    ((and tick x (not (plist-get enc :y)))
                     ;; Vega-Lite: 3/4 of the 20px default step, centred.
                     (let ((c (+ y0 (/ h 2.0))) (q (min (/ h 2.0) (/ (or (plist-get mark :size) 15) 2.0))))
                       (vector x (- c q) x (+ c q))))
                    ((and x (not (plist-get enc :y))) (vector x y0 x (+ y0 h)))
                    ((and y (not (plist-get enc :x))) (vector x0 y (+ x0 w) y))
                    ((and x y) (vector x y x (eas-marks--zero ys bounds :y))))))
         (when (and seg (seq-every-p #'numberp seg))
           (append (list :datum i :x1 (aref seg 0) :y1 (aref seg 1) :x2 (aref seg 2) :y2 (aref seg 3)
                         :strokeWidth (if tick (or (plist-get mark :strokeWidth) (plist-get mark :thickness) 1)
                                        (eas-marks-props-stroke-width unit scales row 1)))
                   (when-let* ((dash (or (eas-marks--channel unit scales :strokeDash row) (plist-get mark :strokeDash))))
                     (list :strokeDash dash))
                   (eas-marks-props-rule unit)
                   (eas-marks--style unit scales row)
                   (eas-marks--extras unit row)))))))

(defun eas-marks--step (points mode)
  "Expand POINTS ([x y] lists) for step interpolation MODE."
  (if (or (null (cdr points)) (not (member mode '("step" "step-after" "step-before")))) points
    (cl-loop for (a b) on points
             append (if (null b) (list a)
                      (pcase mode
                        ("step-after" (list a (list (car b) (cadr a))))
                        ("step-before" (list a (list (car a) (cadr b))))
                        (_ (let ((mid (/ (+ (car a) (car b)) 2.0)))
                             (list a (list mid (cadr a)) (list mid (cadr b))))))))))

(defun eas-marks--series-run (unit scales run mode area max-points sorted)
  "The item drawing RUN, one unbroken stretch of a series in UNIT.
RUN holds (X Y I BASE KEY VALID) vertices; LTTB thins them above
MAX-POINTS when SORTED along x."
  (let* ((mark (plist-get unit :mark))
         (pts (vconcat run))
         (keep (if (and sorted (> (length pts) max-points))
                   (eas-lttb-indices (vconcat (mapcar #'car pts)) (vconcat (mapcar #'cadr pts)) max-points)
                 (vconcat (number-sequence 0 (1- (length pts))))))
         (kept (mapcar (lambda (k) (aref pts k)) keep))
         (row (aref (plist-get unit :rows) (nth 2 (car kept))))
         (shape (lambda (n) (let ((ps (mapcar (lambda (p) (list (nth 0 p) (nth n p))) kept))
                                  (eas-curve-tension (plist-get mark :tension)))
                              (vconcat (mapcar #'vconcat (if (member mode eas-marks-path-curves)
                                                             (eas-marks-path-curve ps mode)
                                                           (eas-marks--step ps mode)))))))
         (dash (or (eas-marks--channel unit scales :strokeDash row) (plist-get mark :strokeDash))))
    (append (list :datum (vconcat (mapcar (lambda (p) (nth 2 p)) kept))
                  :points (funcall shape 1))
            (unless (equal mode "linear")
              (list :anchors (vconcat (mapcar (lambda (p) (vector (nth 0 p) (nth 1 p))) kept))))
            (when area (list :base (funcall shape 3)))
            (list :strokeWidth (if area (or (plist-get mark :strokeWidth) 0) (eas-marks-props-stroke-width unit scales row 2)))
            (unless area (list :strokeCap (plist-get mark :strokeCap) :strokeJoin (plist-get mark :strokeJoin)))
            (when dash (list :strokeDash dash))
            (when (equal (plist-get mark :type) "trail")
              ;; A trail's size is its width at each vertex.
              (list :widths (vconcat (mapcar (lambda (p)
                                               (let ((w (eas-marks--channel unit scales :size
                                                                            (aref (plist-get unit :rows) (nth 2 p)))))
                                                 (if (numberp w) w (or (plist-get mark :size) (plist-get mark :strokeWidth) 2))))
                                             kept))))
            (let ((style (eas-marks--style unit scales row))) (append style (eas-mark-style-extras unit style)))
            (when (> (length pts) (length kept)) (list :decimated (length pts))))))

(defun eas-marks--series-items (unit scales bounds max-points)
  "Items for line and area marks: one item per unbroken run of each series.
Vertices follow `eas-marks-path-sort-key'; invalid positions break the
path (eas-marks-path.el); LTTB thins x-ordered runs above MAX-POINTS."
  (let* ((mark (plist-get unit :mark)) (area (equal (plist-get mark :type) "area"))
         (mode (or (plist-get mark :interpolate) "linear"))
         (sort-key (eas-marks-path-sort-key unit))
         (filter (equal (plist-get mark :invalid) "filter"))
         (defs (eas-marks--series-defs unit))
         (groups (make-hash-table :test 'equal)) (order nil) (i -1))
    (unless (member mode (append '("linear" "step" "step-after" "step-before") eas-curve-modes))
      (eas-signal "UNSUPPORTED_FEATURE" (format "interpolate %s is not supported" mode)
                    :feature (concat "interpolate/" mode)))
    (seq-doseq (row (plist-get unit :rows))
      (setq i (1+ i))
      (let* ((x (eas-marks--pos unit scales :x row bounds))
             (y (eas-marks--pos unit scales :y row bounds))
             (key (eas-marks--series-key unit row defs))
             (sk (cond ((functionp sort-key) (funcall sort-key row)) ((eq sort-key 'y) y))))
        (when (if (functionp sort-key) t (numberp (if (eq sort-key 'y) y x)))
          (unless (gethash key groups) (push key order) (puthash key (list key) groups))
          (push (list x y i (and area (or (eas-marks--secondary unit scales :y row)
                                          (eas-marks--zero (plist-get scales :y) bounds :y)))
                      sk (and (numberp x) (numberp y)))
                (cdr (gethash key groups))))))
    (vconcat
     (mapcan
      (lambda (key)
        (mapcar (lambda (run) (eas-marks--series-run unit scales run mode area max-points (null sort-key)))
                (eas-marks-path-runs (eas-marks-path-sort (nreverse (cdr (gethash key groups)))) filter)))
      (nreverse order)))))


(defun eas-marks-items (unit scales bounds metrics)
  "Return the scene items for UNIT drawn with SCALES inside BOUNDS."
  (let ((eas-marks--cache (make-hash-table :test 'equal)))
    (eas-marks--items unit scales bounds metrics)))

(defun eas-marks-row-fn (unit scales bounds metrics)
  "Function (ROW I) -> item for UNIT's per-row marks, or nil for series.
Call it inside `eas-marks-with-cache'."
  (pcase (plist-get (plist-get unit :mark) :type)
    ((or "point" "circle" "square" "text") (eas-marks--point-row unit scales bounds metrics))
    ((or "bar" "rect") (eas-marks--bar-row unit scales bounds metrics))
    ((or "rule" "tick") (eas-marks--rule-row unit scales bounds))
    ("image" (eas-marks-image-row unit scales bounds))))

(defmacro eas-marks-with-cache (&rest body)
  "Run BODY with a fresh per-unit accessor cache."
  `(let ((eas-marks--cache (make-hash-table :test 'equal))) ,@body))

(declare-function eas-polar-unit-p "eas-polar")
(declare-function eas-polar-items "eas-polar")

(defun eas-marks--items (unit scales bounds metrics)
  "Dispatch UNIT's mark type to its item builder."
  (pcase (plist-get (plist-get unit :mark) :type)
    ((guard (and (fboundp 'eas-polar-unit-p) (eas-polar-unit-p unit)))
     (eas-polar-items unit scales bounds metrics))
    ((or "point" "circle" "square" "text" "bar" "rect" "rule" "tick" "image")
     (eas-marks--each unit (eas-marks-row-fn unit scales bounds metrics)))
    ((or "line" "area" "trail")
     (eas-marks--series-items unit scales bounds
                                (max 3 (round (* (aref bounds 2)
                                                 (if (eas-layout-text-p metrics)
                                                     (/ 2.0 (aref (plist-get metrics :cell) 0))
                                                   1))))))
    (type (eas-signal "UNSUPPORTED_FEATURE" (format "mark %s is not supported" type)
                        :feature (concat "mark/" type)))))

;;; Stacking

(defun eas-marks-stack (unit)
  "Stack UNIT's measure channel when Vega-Lite would; return the new unit.
Bars and areas with a discrete color/fill/detail field stack by default
\(offset zero, descending stack-by order); stack null or false opts out."
  (let* ((enc (plist-get unit :encoding)) (type (plist-get (plist-get unit :mark) :type))
         (y (plist-get enc :y)) (x (plist-get enc :x))
         (stacks (lambda (d) (and d (equal (plist-get d :type) "quantitative") (stringp (plist-get d :stack)))))
         (measure (cond ((funcall stacks y) :y)
                        ((funcall stacks x) :x)
                        ((and y (equal (plist-get y :type) "quantitative") (not (plist-get y :bin-end))
                              (not (and x (equal (plist-get x :type) "quantitative") (not (plist-get x :bin-end)))))
                         :y)
                        ((and x (equal (plist-get x :type) "quantitative") (not (plist-get x :bin-end))) :x)))
         (mdef (and measure (plist-get enc measure)))
         (by (seq-filter (lambda (d) (and d (eas-object-p d) (plist-get d :field) (eas-encode-discrete-p d)))
                         (mapcar (lambda (ch) (eas-encode-data-def (plist-get enc ch))) '(:color :fill :detail))))
         (offset (and mdef (plist-get mdef :stack))))
    (if (or (null measure) (memq offset '(:null :false))
            ;; Vega-Lite does not stack marks grouped by an offset channel.
            (and (null offset) (or (plist-get enc :xOffset) (plist-get enc :yOffset)))
            (and (null offset) (or (null by) (not (member type '("bar" "area")))))
            (plist-get enc (if (eq measure :y) :y2 :x2)))
        unit
      (let* ((field (eas-encode-field mdef))
             (dim (plist-get enc (if (eq measure :y) :x :y)))
             (start (concat (plist-get mdef :field) "_start")) (end (concat (plist-get mdef :field) "_end"))
             (groups (make-hash-table :test 'equal))
             (normalize (equal offset "normalize"))
             (rows (plist-get unit :rows))
             ;; Vega-Lite stacks by the color scale's domain order: the first
             ;; value ends at the top of a vertical stack and at the left of a
             ;; horizontal one, so the last value is the base of a vertical stack.
             (domains (mapcar (lambda (d)
                                (let ((explicit (plist-get (plist-get d :scale) :domain)))
                                  (if (vectorp explicit) (append explicit nil)
                                    (let ((vs (delete-dups (seq-map (lambda (r) (eas-encode-raw d r)) rows))))
                                      (sort vs (lambda (a b) (if (and (numberp a) (numberp b)) (< a b)
                                                               (string< (format "%s" a) (format "%s" b)))))))))
                              by))
             (ranks (eas-marks--stack-ranks rows by domains))
             (sorted (sort (number-sequence 0 (1- (length rows)))
                           (lambda (a b)
                             (let* ((ra (aref ranks a)) (rb (aref ranks b))
                                    (lt (cl-loop for x in ra for y in rb unless (= x y) return (< x y))))
                               (if (eq measure :x) lt (and (not (equal ra rb)) (not lt)))))))
             (out (copy-sequence rows)))
        (dolist (i sorted)
          (let* ((row (aref rows i)) (v (plist-get row field))
                 (g (and dim (eas-encode-raw dim row)))
                 (acc (or (gethash g groups) (cons 0 0))))
            (when (numberp v)
              (let ((base (if (>= v 0) (car acc) (cdr acc))))
                (aset out i (append row (list (eas-key start) base (eas-key end) (+ base v))))
                (puthash g (if (>= v 0) (cons (+ base v) (cdr acc)) (cons (car acc) (+ base v))) groups)))))
        (when (equal offset "center")
          ;; Vega's center offset: each stack starts at (max total - its total) / 2.
          (let ((top (apply #'max 0 (mapcar (lambda (acc) (- (car acc) (cdr acc))) (hash-table-values groups)))))
            (dotimes (i (length out))
              (let* ((row (aref out i)) (acc (gethash (and dim (eas-encode-raw dim row)) groups))
                     (shift (and acc (plist-get row (eas-key end)) (/ (- top (- (car acc) (cdr acc))) 2.0))))
                (when shift
                  (aset out i (eas-plist-put (eas-plist-put row (eas-key start) (+ shift (plist-get row (eas-key start))))
                                             (eas-key end) (+ shift (plist-get row (eas-key end))))))))))
        (when normalize
          (dotimes (i (length out))
            (let* ((row (aref out i)) (g (and dim (eas-encode-raw dim row)))
                   (tot (let ((acc (gethash g groups))) (and acc (- (car acc) (cdr acc))))))
              (when (and tot (> tot 0) (plist-get row (eas-key end)))
                (aset out i (eas-plist-put (eas-plist-put row (eas-key start) (/ (plist-get row (eas-key start)) (float tot)))
                                             (eas-key end) (/ (plist-get row (eas-key end)) (float tot))))))))
        (thread-first unit
                      (plist-put :rows out)
                      (plist-put :encoding (eas-plist-put enc measure
                                                            (append (list :field end :stack-start start
                                                                          :title (eas-encode-title mdef))
                                                                    (eas--plist-without mdef :field)))))))))

(provide 'eas-marks)
;;; eas-marks.el ends here

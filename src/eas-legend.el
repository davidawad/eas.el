;;; eas-legend.el --- legend models and placement -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A legend explains one scale: an ordinal color scale or a
;; quantitative size or opacity scale as a symbol legend, a sequential
;; color scale as a gradient.  The svg target places legends as Vega
;; does: symbol entries are max(ceil(sqrt(size) + strokeWidth),
;; labelFontSize) wide, rows are separated by their bounds plus
;; rowPadding, entries start titlePadding below the title, and gradients
;; get max(2, 2*floor(length/100)) labels.  Each placed svg legend
;; carries :box, its Vega bounds.  The text target keeps one row per
;; entry on the character grid.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)
(require 'eas-layout)
(require 'eas-legend-extra)
(require 'eas-legend-style)
(require 'eas-legend-orient)

(declare-function eas-expr--string "eas-expr")
(declare-function eas-legend-row-p "eas-legend-row")
(declare-function eas-scale-discretize-entries "eas-scale-discretize")
(declare-function eas-scale-discretize-gradient "eas-scale-discretize")
(declare-function eas-legend-row-place "eas-legend-row")

(defconst eas-legend-default-color "#4c78a8"
  "Symbol color when the mark has no constant color of its own.")

(defun eas-legend-model (spec metrics)
  "Legend model for SPEC (:channel :def :scale :shape :style), or nil.
STYLE is the mark's constant look (:fill :stroke :stroke-width
:opacity :stroked) that symbols copy.  The def's legend object
restyles it (eas-legend-style.el).  METRICS are the layout's."
  (eas-legend-style-model (plist-get (plist-get spec :def) :legend) (eas-legend--model spec metrics)))

(defun eas-legend--model (spec metrics)
  "`eas-legend-model' of SPEC before the legend object's own properties.
METRICS are the layout's."
  (let* ((channel (plist-get spec :channel)) (def (plist-get spec :def)) (scale (plist-get spec :scale))
         (legend (plist-get def :legend)) (style (plist-get spec :style))
         ;; config.legend's orient and direction apply where the legend sets none.
         (config (plist-get metrics :config)))
    (unless (or (memq legend '(:null :false)) (null scale))
      (let ((base (list :channel (eas-key-name channel)
                        :title (if (plist-member legend :title)
                                   (let ((tt (plist-get legend :title))) (and (stringp tt) tt))
                                 (eas-encode-title def (plist-get metrics :config)))
                        :shape (plist-get spec :shape) :style style
                        :orient (or (and (eas-object-p legend) (plist-get legend :orient))
                                    (let ((o (eas-theme-get config :legend :orient))) (and (stringp o) o))))))
        ;; orient "none" places the legend at legendX/legendY in the view.
        (when (equal (plist-get legend :orient) "none")
          (setq base (append base (list :orient "none" :legendX (or (plist-get legend :legendX) 0)
                                        :legendY (or (plist-get legend :legendY) 0)))))
        (when-let* ((dir (eas-legend-orient-direction
                          (list :orient (plist-get base :orient)
                                :direction (or (plist-get legend :direction)
                                               (let ((d (eas-theme-get config :legend :direction))) (and (stringp d) d)))))))
          (setq base (append base (list :direction dir))))
        (when (numberp (plist-get legend :clipHeight))
          (setq base (append base (list :clip-height (plist-get legend :clipHeight)))))
        (when (numberp (plist-get legend :gradientLength))
          (setq base (append base (list :gradient-length (plist-get legend :gradientLength)))))
        (when (numberp (plist-get legend :columns))
          (setq base (append base (list :columns (plist-get legend :columns)))))
        (when (numberp (plist-get legend :offset))
          (setq base (append base (list :offset (plist-get legend :offset)))))
        ;; The legend's own look (labelColor, symbolSize, titlePadding, ...)
        ;; overrides config.legend for this legend only.
        (when-let* ((props (eas-legend--props legend)))
          (setq base (append base (list :props props))))
        ;; Row layout (eas-legend-row.el) of a top or bottom legend.
        (when (stringp (plist-get legend :titleOrient))
          (setq base (append base (list :title-orient (plist-get legend :titleOrient)))))
        (when (numberp (plist-get legend :columnPadding))
          (setq base (append base (list :column-padding (plist-get legend :columnPadding)))))
        (when (numberp (plist-get legend :titlePadding))
          (setq base (append base (list :title-padding (plist-get legend :titlePadding)))))
        (pcase (plist-get scale :type)
          ;; Discretizing scales: one entry per bucket (eas-scale-discretize.el);
          ;; a merged size scale of the same field gives the entries and areas.
          ((and (or "quantize" "quantile" "threshold")
                (guard (and (memq channel '(:color :fill :stroke)) (null (plist-get spec :size-scale)))))
           ;; Color alone: Vega's discrete gradient, labelled at the thresholds.
           (let ((g (eas-scale-discretize-gradient scale (and (eas-object-p legend) (plist-get legend :format)))))
             (append base (list :type "gradient" :stops (plist-get g :stops) :domain (plist-get g :domain)
                                :entries [] :fixed-entries (plist-get g :entries)))))
          ((or "quantize" "quantile" "threshold")
           (let* ((size (plist-get spec :size-scale))
                  (primary (or size scale)))
             (append base (list :type "symbol"
                                :entries (eas-scale-discretize-entries
                                          primary (and (eas-object-p legend) (plist-get legend :format))
                                          (and (memq channel '(:color :fill :stroke)) (lambda (v) (eas-scale-apply scale v)))
                                          (cond ((eq channel :size) (lambda (v) (eas-scale-apply scale v)))
                                                (size (lambda (v) (eas-scale-apply size v)))))))))
          ("ordinal"
           (append base (list :type "symbol"
                              :entries (vconcat (seq-map (lambda (v)
                                                           (append
                                                            (if (eq channel :strokeDash)
                                                                (list :value v :label (eas-expr--string v)
                                                                      :dash (eas-scale-apply scale v)
                                                                      :color (or (plist-get style :stroke) (plist-get style :fill)
                                                                                 eas-legend-default-color))
                                                              (list :value v :label (eas-expr--string v)
                                                                    :color (eas-scale-apply scale v)))
                                                            (when-let* ((ss (plist-get spec :shape-scale)))
                                                              (list :shape (eas-scale-apply ss v)))))
                                                         (plist-get scale :domain))))))
          ("sequential"
           (append base (list :type "gradient"
                              :stops (if (or (plist-get scale :mid) (equal (plist-get scale :interpolate) "hcl"))
                                         ;; Diverging or HCL: sample the scale evenly along its domain.
                                         (let ((d (plist-get scale :domain)))
                                           (vconcat (cl-loop for i to 32
                                                             collect (eas-scale-apply
                                                                      scale (+ (aref d 0) (* (/ i 32.0) (- (aref d 1) (aref d 0))))))))
                                       (plist-get scale :range))
                              :domain (plist-get scale :domain) :values (and (eas-object-p legend) (plist-get legend :values))
                              :entries [])))
          (_
           (let* ((domain (plist-get scale :domain))
                  (lv (and (eas-object-p legend) (vectorp (plist-get legend :values)) (plist-get legend :values)))
                  ;; legend.values on a continuous scale are the entries themselves.
                  (values (if lv (append lv nil) (eas-scale-linear-ticks (aref domain 0) (aref domain 1) 5)))
                  (fmt (eas-scale-tick-format (list :type "linear" :domain domain) 5))
                  (values (if (and (eq channel :size) values (zerop (eas-scale-apply scale (car values))))
                              (cdr values) values)))
             (append base (list :type "symbol"
                                :entries (vconcat (mapcar (lambda (v)
                                                            (append (list :value v :label (funcall fmt v)
                                                                          :color (plist-get style :fill))
                                                                    (list (if (eq channel :size) :size :opacity)
                                                                          (eas-scale-apply scale v))))
                                                          values)))))))))))

(defconst eas-legend--placed-keys
  '(:title :orient :legendX :legendY :direction :clipHeight :gradientLength :offset :values :type)
  "Legend properties the model reads itself rather than through config.legend.")

(defun eas-legend--props (legend)
  "LEGEND object's properties that override config.legend, or nil."
  (when (eas-object-p legend)
    (cl-loop for (k v) on legend by #'cddr
             unless (memq k eas-legend--placed-keys) append (list k v))))

(defun eas-legend-metrics (legend metrics)
  "METRICS with LEGEND's own properties merged over config.legend."
  (if-let* ((props (plist-get legend :props)))
      (eas-layout-metrics (intern (plist-get metrics :target)) (plist-get metrics :cell)
                          (eas-theme-merge (plist-get metrics :config) (list :legend props)))
    metrics))

;;; Text target

(defun eas-legend--gradient-length (legend metrics)
  "Gradient length of LEGEND under METRICS.
One row per label in text, clamp(plot height, 64, 200) in svg."
  (if (eas-layout-text-p metrics)
      (* (max 5 (length (plist-get legend :entries))) (plist-get metrics :row))
    (or (plist-get legend :gradient-length)
        ;; Vega-Lite: clamp(plot height, 64, 200), or clamp(plot width, 100, 200) horizontally.
        (if (equal (plist-get legend :direction) "horizontal")
            (max 100 (min 200 (or (plist-get legend :plot-w) 200)))
          (max 64 (min 200 (or (plist-get legend :plot-h) 200)))))))

(defun eas-legend--gradient-entries (legend metrics)
  "LEGEND's gradient labels: d3 ticks, as many as Vega asks for its length.
METRICS are the layout's."
  (let* ((domain (plist-get legend :domain))
         (glen (eas-legend--gradient-length legend metrics))
         (count (if (eas-layout-text-p metrics) (max 2 (ceiling (/ glen 40.0)))
                  (max 2 (* 2 (floor glen 100)))))
         (fmt (eas-scale-tick-format (list :type "linear" :domain domain) count))
         (values (if (vectorp (plist-get legend :values)) (append (plist-get legend :values) nil)
                   (eas-scale-linear-ticks (aref domain 0) (aref domain 1) count))))
    (when (and (not (eas-layout-text-p metrics)) (< (length values) 3) (/= (aref domain 0) (aref domain 1)))
      (setq values (list (aref domain 0) (aref domain 1))))
    (vconcat (mapcar (lambda (v) (list :value v :label (funcall fmt v))) values))))

(defun eas-legend-sized (legend metrics)
  "LEGEND with its gradient entries filled in for its final length.
METRICS are the layout's."
  (if (equal (plist-get legend :type) "gradient")
      (eas-legend-style-labels (eas-plist-put legend :entries (or (plist-get legend :fixed-entries)
                                                                       (eas-legend--gradient-entries legend metrics))))
    legend))

(defun eas-legend-size (legend metrics)
  "Return (WIDTH . HEIGHT) of LEGEND under METRICS.
The size includes its offset from the plot."
  (let* ((metrics (eas-legend-style-metrics legend metrics))
         (size (plist-get metrics :label-size))
         (labels (mapcar (lambda (e) (eas-layout-text-width metrics (plist-get e :label) size))
                         (plist-get legend :entries)))
         (title-w (if (plist-get legend :title)
                      (eas-layout-text-width metrics (plist-get legend :title) (plist-get metrics :title-size))
                    0))
         (sym (plist-get metrics :symbol))
         (gradient (equal (plist-get legend :type) "gradient"))
         (body-w (+ (if gradient (* 1.5 sym) sym) (plist-get metrics :char-w) (if labels (apply #'max labels) 0)))
         (title-h (if (plist-get legend :title) (plist-get metrics :title-size) 0)))
    (cons (+ (plist-get metrics :legend-offset) (max title-w body-w))
          (+ title-h (if gradient (eas-legend--gradient-length legend metrics)
                       (* (length labels) (plist-get metrics :row)))))))

(defun eas-legend--place-text (legend x y metrics)
  "LEGEND on the character grid with its top-left corner at X Y.
METRICS are the layout's."
  (let* ((sym (plist-get metrics :symbol)) (row (plist-get metrics :row))
         (title-h (if (plist-get legend :title) (plist-get metrics :title-size) 0))
         (gap (plist-get metrics :char-w))
         (width (- (car (eas-legend-size legend metrics)) (plist-get metrics :legend-offset)))
         (gradient (equal (plist-get legend :type) "gradient"))
         (glen (eas-legend--gradient-length legend metrics)))
    (append (eas--plist-without legend :entries)
            (list :x x :y y :width width)
            (when (plist-get legend :title)
              (list :title-mark (list :text (plist-get legend :title) :x x :y y :align "left" :baseline "top")))
            (when gradient (list :bar (vector x (+ y title-h) (* 1.5 sym) glen)))
            (list :entries
                  (vconcat
                   (seq-map-indexed
                    (lambda (e i)
                      (let* ((d (plist-get legend :domain))
                             (top (+ y title-h
                                     (if gradient
                                         (* glen (/ (- (aref d 1) (plist-get e :value)) (float (max 1e-9 (- (aref d 1) (aref d 0))))))
                                       (* i row))))
                             (cy (if gradient top (+ top (/ row 2.0)))))
                        (append e (list :sx (+ x (/ sym 2.0)) :sy cy
                                        :lx (+ x (if gradient (* 1.5 sym) sym) gap) :ly cy
                                        :bounds (vector x top width row)))))
                    (plist-get legend :entries)))))))

;;; Svg target

(defun eas-legend--symbol (legend e metrics)
  "Symbol look of entry E in LEGEND: (:size :fill :stroke :stroke-width :opacity)."
  (let* ((style (plist-get legend :style)) (channel (plist-get legend :channel))
         (stroked (or (member channel '("stroke" "strokeDash"))
                      (and (plist-get style :stroked) (member channel '("color")))))
         (sw (if (and stroked (not (plist-get style :trail))) (or (plist-get style :stroke-width) 2)
               (plist-get metrics :symbol-stroke-width))))
    (if (and (plist-get style :trail) (equal channel "size"))
        (list :size (plist-get metrics :symbol-size) :fill "transparent"
              :stroke (or (eas-theme-get (plist-get metrics :config) :legend :symbolBaseStrokeColor) "#888")
              :stroke-width (or (plist-get e :size) sw) :opacity 1)
      (append
       (list :size (or (plist-get e :size) (plist-get metrics :symbol-size))
             :fill (cond (stroked "transparent")
                         ((member channel '("color" "fill")) (plist-get e :color))
                         ;; Vega-Lite draws other legends' symbols in black when color maps a field.
                         ((plist-get style :field-color) "black")
                         (t (plist-get style :fill)))
             :stroke (cond (stroked (plist-get e :color))
                           ((member channel '("size" "opacity"))
                            (if (and (plist-get style :field-color) (plist-get style :stroke)) (plist-get style :stroke)
                              "transparent"))
                           (t (plist-get style :stroke)))
             :stroke-width sw
             :opacity (or (plist-get e :opacity) (plist-get style :opacity) 1))
       (when (and (not stroked) (not (member channel '("color" "fill"))) (plist-get style :field-color)
                  (plist-get style :opacity))
         (list :fill-opacity (plist-get style :opacity)))
       (when (plist-get e :dash) (list :dash (plist-get e :dash)))))))

(defun eas-legend-symbol-type (legend metrics)
  "Return LEGEND's symbol shape.
That is its own symbolType, else its point mark's constant shape, else
config.legend's (METRICS)."
  (or (let ((o (plist-get (plist-get legend :overrides) :symbolType))) (and (stringp o) o))
      (let ((p (plist-get (plist-get legend :props) :symbolType))) (and (stringp p) p))
      (plist-get (plist-get legend :style) :mark-shape)
      (plist-get metrics :symbol-type)))

(defun eas-legend--title (legend x y metrics)
  "Return LEGEND's title item at X Y and the y of its body, as (MARK . Y).
METRICS are the layout's."
  (if-let* ((title (plist-get legend :title)))
      (cons (list :text title :x x :y y :align "left" :baseline "top")
            (+ y (plist-get metrics :legend-title-size) (plist-get metrics :legend-title-pad)))
    (cons nil y)))

(defun eas-legend--place-symbols (legend x y metrics)
  "Symbol LEGEND with its top-left at X Y, laid out as Vega does.
METRICS are the layout's."
  (let* ((fs (plist-get metrics :legend-label-size))
         (title (eas-legend--title legend x y metrics))
         (looks (mapcar (lambda (e) (eas-legend--symbol legend e metrics)) (plist-get legend :entries)))
         (widths (mapcar (lambda (s) (max (ceiling (+ (sqrt (plist-get s :size)) (plist-get s :stroke-width))) fs))
                         looks))
         ;; legend.clipHeight caps an entry's height; its symbol is clipped.
         (clip (plist-get legend :clip-height))
         (sizes (if clip (mapcar (lambda (w) (min w clip)) widths) widths))
         (offset (apply #'max 0 widths))
         (ey (cdr title)) (prev-y2 nil) (box nil)
         (entries
          (cl-loop
           for e across (plist-get legend :entries) for s in looks for size in sizes
           collect
           (let* ((r (/ (sqrt (plist-get s :size)) 2.0))
                  (grow (if (plist-get s :stroke) (plist-get s :stroke-width) 0))
                  (cy (/ size 2.0))
                  (lbox (eas-layout-text-bounds metrics (plist-get e :label) fs (+ offset (plist-get metrics :legend-label-offset))
                                                  cy "left" "middle"))
                  (half (if clip (min (+ r grow) (/ clip 2.0)) (+ r grow)))
                  (y1 (min (aref lbox 1) (- cy half))) (y2 (max (aref lbox 3) (+ cy half)))
                  (x2 (max (aref lbox 2) (+ (/ offset 2.0) r grow))))
             (when prev-y2 (setq ey (+ ey prev-y2 (plist-get metrics :legend-row-pad) (if (< y1 0) (ceiling (- y1)) 0))))
             (setq prev-y2 (ceiling y2)
                   box (eas-layout-union box (vector x (+ ey y1) (+ x x2) (+ ey y2))))
             (append s e (when clip (list :clip (vector x (+ ey cy (- half)) offset (* 2 half))))
                     (list :sx (+ x (/ offset 2.0)) :sy (+ ey cy)
                               :lx (+ x offset (plist-get metrics :legend-label-offset)) :ly (+ ey cy)
                               :bounds (vector x ey (max offset x2) size)))))))
    (when (car title)
      (setq box (eas-layout-union box (eas-layout-text-bounds metrics (plist-get legend :title)
                                                                  (plist-get metrics :legend-title-size) x y "left" "top"
                                                                  0 (plist-get metrics :legend-title-weight)))))
    (append (eas--plist-without legend :entries)
            (list :x x :y y :width (if box (ceiling (- (aref box 2) x)) 0) :font-size fs :symbol-type (eas-legend-symbol-type legend metrics)
                  :box (if box (vector x y (+ x (ceiling (- (aref box 2) x))) (+ y (ceiling (- (aref box 3) y))))
                         (vector x y x y))
                  :entries (vconcat entries))
            (when (car title) (list :title-mark (car title))))))

(defun eas-legend--place-gradient (legend x y metrics)
  "Gradient LEGEND with its top-left at X Y, laid out as Vega does.
METRICS are the layout's."
  (let* ((fs (plist-get metrics :legend-label-size))
         (title (eas-legend--title legend x y metrics))
         (by (cdr title)) (thick (plist-get metrics :gradient-thickness))
         (glen (eas-legend--gradient-length legend metrics))
         (d (plist-get legend :domain)) (span (max 1e-9 (- (aref d 1) (aref d 0))))
         (lx (+ x thick 2))
         (placed (mapcar (lambda (e)
                           (let* ((perc (/ (- (plist-get e :value) (aref d 0)) (float span)))
                                  (baseline (cond ((<= perc 0) "bottom") ((>= perc 1) "top") (t "middle")))
                                  (ly (+ by (* glen (- 1 perc)))))
                             (append e (list :lx lx :ly ly :baseline baseline :sx x :sy ly
                                             :box (eas-layout-text-bounds metrics (plist-get e :label) fs lx ly
                                                                            "left" baseline)
                                             :bounds (vector x (- ly (/ fs 2.0)) (+ thick 2) fs)))))
                         (plist-get legend :entries)))
         (shown (eas-layout--thin placed
                                    (lambda (a b) (let ((p (plist-get a :box)) (q (plist-get b :box)))
                                                    (> 0 (max (- (aref q 0) (aref p 2)) (- (aref p 0) (aref q 2))
                                                              (- (aref q 1) (aref p 3)) (- (aref p 1) (aref q 3))))))
                                    "parity" t))
         (box (apply #'eas-layout-union (vector x by (+ x thick) (+ by glen))
                     (mapcar (lambda (e) (plist-get e :box)) shown))))
    (when (car title)
      (setq box (eas-layout-union box (eas-layout-text-bounds metrics (plist-get legend :title)
                                                                  (plist-get metrics :legend-title-size) x y "left" "top"
                                                                  0 (plist-get metrics :legend-title-weight)))))
    (append (eas--plist-without legend :entries)
            (list :x x :y y :width (ceiling (- (aref box 2) x)) :font-size fs
                  :bar (vector x by thick glen)
                  :box (vector x y (+ x (ceiling (- (aref box 2) x))) (+ y (ceiling (- (aref box 3) y))))
                  :entries (vconcat (mapcar (lambda (e) (eas--plist-without e :box)) shown)))
            (when (car title) (list :title-mark (car title))))))

(defun eas-legend-place (legend x y metrics)
  "Return LEGEND with geometry, its top-left corner at X Y.
METRICS are the layout's."
  (let ((metrics (eas-legend-style-metrics legend metrics)))
    (eas-legend-style-looks
     (cond ((eas-layout-text-p metrics) (eas-legend--place-text legend x y metrics))
           ((eas-legend-extra-horizontal-p legend metrics) (eas-legend-extra-place-horizontal legend x y metrics))
           ((and (fboundp 'eas-legend-row-p)
                 (or (eas-legend-row-p legend metrics)
                     ;; A view's own top or bottom legend titled on its left is a row too.
                     (and (equal (plist-get legend :title-orient) "left") (equal (plist-get legend :type) "symbol")
                          (eas-legend-orient-row-p legend metrics))))
            (eas-legend-row-place legend x y metrics))
           ((eas-legend-orient-row-p legend metrics) (eas-legend-orient-place-row legend x y metrics))
           ((equal (plist-get legend :type) "gradient") (eas-legend--place-gradient legend x y metrics))
           (t (eas-legend--place-symbols legend x y metrics))))))

(provide 'eas-legend)
;;; eas-legend.el ends here

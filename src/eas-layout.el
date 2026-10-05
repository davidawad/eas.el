;;; eas-layout.el --- metrics, text bounds and axes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Layout is parameterized by METRICS, so one compile
;; serves both renderers.  The svg target follows Vega: sizes, fonts and
;; paddings come from the theme (a Vega config), text is measured with
;; the oracle's font metrics and bounded the way vega-scenegraph bounds
;; it, axes put labels at tickSize + labelPadding and titles at their
;; labels' extent + titlePadding, and every axis line sits on the half
;; pixel, like Vega's translate(0.5).  The text target snaps everything
;; to character cells (every font is one cell high, every glyph one cell
;; wide, y titles are horizontal).  Renderers never place anything.
;; Legends live in eas-legend.el.

;;; Code:

(require 'eas-core)
(require 'eas-scale)
(require 'eas-encode)
(require 'eas-theme)
(require 'eas-font)

(defun eas-layout-metrics (target &optional cell config)
  "Return layout metrics for TARGET (svg or text); CELL is [W H] in px.
CONFIG is the Vega config in force (default `eas-theme-default')."
  (let* ((cell (or cell [7 14])) (cw (aref cell 0)) (ch (aref cell 1))
         (config (or config eas-theme-default)))
    (cl-flet ((axis (channel key default)
                (let ((v (eas-theme-axis config channel key))) (if v v default)))
              (get (default &rest keys)
                (let ((v (apply #'eas-theme-get config keys))) (if v v default))))
      (if (eq target 'text)
          (list :target "text" :cell cell :config config :pad 0 :tick-bottom ch :tick-left cw :label-pad 0
                :label-size ch :title-size ch :title-pad 0 :chart-title-size ch :chart-title-pad 0
                :legend-offset (* 2 cw) :symbol cw :row ch :spacing ch :char-w cw
                :x-tick-spacing (* 12 cw) :y-tick-spacing (* 3 ch))
        (list :target "svg" :cell cell :config config
              :pad (let ((p (eas-theme-get config :padding))) (if (numberp p) p 5))
              :tick-bottom (axis :x :tickSize 5) :tick-left (axis :y :tickSize 5)
              :label-pad (axis :y :labelPadding 2) :label-size (axis :y :labelFontSize 10)
              :title-size (axis :y :titleFontSize 11) :title-pad (axis :y :titlePadding 4)
              :title-weight (axis :y :titleFontWeight "bold")
              :chart-title-size (get 13 :title :fontSize) :chart-title-pad (get 10 :title :offset)
              :chart-title-weight (get "bold" :title :fontWeight) :chart-title-anchor (get "middle" :title :anchor)
              :legend-offset (get 18 :legend :offset) :legend-label-size (get 10 :legend :labelFontSize)
              :legend-title-size (get 11 :legend :titleFontSize) :legend-title-pad (get 5 :legend :titlePadding)
              :legend-title-weight (get "bold" :legend :titleFontWeight)
              :legend-row-pad (get 2 :legend :rowPadding) :legend-label-offset (get 4 :legend :labelOffset)
              :legend-margin 8 :symbol-size (get 100 :legend :symbolSize)
              :symbol-type (get "circle" :legend :symbolType) :symbol-stroke-width (get 1.5 :legend :symbolStrokeWidth)
              :gradient-thickness (get 16 :legend :gradientThickness)
              :spacing 20 :char-w nil :step 20
              :width (get 300 :view :continuousWidth) :height (get 300 :view :continuousHeight)
              :x-tick-spacing 40 :y-tick-spacing 40)))))

(defun eas-layout-text-p (metrics)
  "Non-nil for text-target METRICS."
  (equal (plist-get metrics :target) "text"))

(defun eas-layout-text-width (metrics text size &optional weight)
  "Width of TEXT at font SIZE (and WEIGHT) under METRICS.
The svg target measures with the oracle's font (`eas-font-text-width')."
  (let ((text (or text "")))
    (if (plist-get metrics :char-w) (* (string-width text) (plist-get metrics :char-w))
      (eas-font-text-width text size weight))))

;;; Text bounds, as vega-scenegraph computes them

(defun eas-layout--round (v)
  "JavaScript's Math.round of V."
  (floor (+ v 0.5)))

(defun eas-layout-baseline-offset (baseline size)
  "Vega's vertical offset of the text baseline for BASELINE at font SIZE."
  (eas-layout--round (* size (pcase baseline ("top" 0.79) ("middle" 0.30) ("bottom" -0.21) (_ 0)))))

(defun eas-layout-text-bounds (metrics text size x y &optional align baseline angle weight)
  "Bounds [X1 Y1 X2 Y2] of TEXT drawn at X Y, as Vega bounds text.
ALIGN is left/center/right, BASELINE top/middle/bottom/alphabetic and
ANGLE degrees clockwise."
  (let* ((w (eas-layout-text-width metrics text size weight))
         (dy (- (eas-layout-baseline-offset baseline size) (eas-layout--round (* 0.8 size))))
         (x1 (+ x (pcase align ("center" (- (/ w 2.0))) ("right" (- w)) (_ 0))))
         (y1 (+ y dy)))
    (if (or (null angle) (zerop angle)) (vector x1 y1 (+ x1 w) (+ y1 size))
      (let* ((a (degrees-to-radians angle)) (c (cos a)) (s (sin a))
             (pts (mapcar (lambda (p) (cons (+ x (- (* c (- (car p) x)) (* s (- (cdr p) y))))
                                            (+ y (* s (- (car p) x)) (* c (- (cdr p) y)))))
                          (list (cons x1 y1) (cons x1 (+ y1 size)) (cons (+ x1 w) y1) (cons (+ x1 w) (+ y1 size))))))
        (vector (apply #'min (mapcar #'car pts)) (apply #'min (mapcar #'cdr pts))
                (apply #'max (mapcar #'car pts)) (apply #'max (mapcar #'cdr pts)))))))

(defun eas-layout-union (&rest boxes)
  "Union of bounds BOXES ([X1 Y1 X2 Y2] or nil), or nil."
  (let (out)
    (dolist (b boxes)
      (when b
        (setq out (if out (vector (min (aref out 0) (aref b 0)) (min (aref out 1) (aref b 1))
                                  (max (aref out 2) (aref b 2)) (max (aref out 3) (aref b 3)))
                    (copy-sequence b)))))
    out))

;;; Axes

(defun eas-layout-time-unit-format (field)
  "Vega-Lite's label format for a timeUnit-derived FIELD (UNIT_SOURCE)."
  (let ((unit (car (split-string field "_"))))
    (cond ((string-match-p "date\\'" unit) (if (string-match-p "year" unit) "%b %d, %Y" "%b %d"))
          ((string-match-p "month\\'" unit) (if (string-match-p "year" unit) "%b %Y" "%b"))
          ((string-match-p "quarter" unit) "Q%q")
          ((string-match-p "day\\'" unit) "%a")
          ((string-match-p "hours\\'" unit) "%H:00")
          ((string-match-p "minutes\\'" unit) "%H:%M")
          ((equal unit "year") "%Y")
          (t "%b %d, %Y"))))

(defun eas-layout-axis (channel def scale plot-size metrics)
  "Return the axis model for CHANNEL's DEF and SCALE, or nil when disabled.
PLOT-SIZE is the plot extent along the axis."
  (let ((axis (plist-get def :axis)))
    (unless (or (memq axis '(:null :false)) (null scale) (null def))
      (let* ((discrete (member (plist-get scale :type) '("band" "point")))
             (spacing (plist-get metrics (if (eq channel :x) :x-tick-spacing :y-tick-spacing)))
             (count (or (plist-get axis :tickCount)
                        (if (eas-layout-text-p metrics) (max 2 (ceiling (/ plot-size (float spacing))))
                          ;; Vega-Lite: ceil(size/40), ceil(width/10) for binned x.
                          (max 1 (ceiling (/ plot-size (if (and (eq channel :x) (plist-get def :bin-end)) 10.0
                                                         (float spacing))))))))
             (fmt (if (and discrete (equal (plist-get def :derived) "timeUnit") (null (plist-get axis :format)))
                      (let ((f (eas-layout-time-unit-format (plist-get def :field))))
                        (lambda (v) (if (numberp v)
                                        (let ((system-time-locale "C")
                                              (eas-time-zone (unless (string-prefix-p "utc" (plist-get def :field))
                                                                 eas-time-zone)))
                                          (eas-time-format v f))
                                      (format "%s" v))))
                    (eas-scale-tick-format scale count (or (plist-get axis :format) (plist-get def :format)))))
             (values (if (plist-get axis :values) (append (plist-get axis :values) nil)
                       (eas-scale-ticks scale count)))
             (title (if (plist-member axis :title)
                        (let ((tt (plist-get axis :title))) (and (stringp tt) tt))
                      (eas-encode-title def)))
             (angle (cond ((plist-get axis :labelAngle))
                          ((eas-layout-text-p metrics) 0)
                          ((and (eq channel :x) discrete (not (equal (plist-get def :derived) "timeUnit"))) 270)
                          (t 0)))
             (grid (cond ((plist-member axis :grid) (eq (plist-get axis :grid) t))
                         (discrete nil)
                         (t (not (eq (eas-theme-axis (plist-get metrics :config) channel :grid) :false))))))
        (list :channel (eas-key-name channel)
              :orient (if (eq channel :x) "bottom" "left")
              :title title :discrete (if discrete t :false) :labelAngle angle
              :overlap (cond ((and discrete (equal (plist-get def :type) "nominal")) nil)
                             ((equal (plist-get scale :type) "log") "greedy")
                             (t "parity"))
              :grid (if grid t :false)
              :ticks (vconcat (mapcar (lambda (v) (list :value v :label (funcall fmt v))) values)))))))

(defun eas-layout-axis-label-extent (axis metrics)
  "Thickness of AXIS's labels across the axis (text target)."
  (let ((widths (mapcar (lambda (tk) (eas-layout-text-width metrics (plist-get tk :label)
                                                              (plist-get metrics :label-size)))
                        (plist-get axis :ticks))))
    (if (and (equal (plist-get axis :orient) "bottom") (zerop (plist-get axis :labelAngle)))
        (plist-get metrics :label-size)
      (if widths (apply #'max widths) 0))))

(defun eas-layout--tick (axis metrics)
  "Tick length of AXIS under METRICS (one cell in text)."
  (plist-get metrics (if (equal (plist-get axis :orient) "bottom") :tick-bottom :tick-left)))

(defun eas-layout-axis-extent (axis metrics)
  "Space AXIS needs outside the plot in text: (SIDE . CELLS*PX) pairs."
  (let* ((labels (+ (eas-layout--tick axis metrics) (plist-get metrics :label-pad)
                    (eas-layout-axis-label-extent axis metrics)))
         (title (and (plist-get axis :title) (plist-get metrics :title-size)))
         (left (equal (plist-get axis :orient) "left")))
    (append (list (cons (if left :left :bottom) (+ labels (if (and title (not left)) title 0))))
            (when (and left title) (list (cons :top (plist-get metrics :title-size)))))))

(defun eas-layout--thin (ticks overlap-p strategy &optional keep-last)
  "TICKS whose labels survive Vega's overlap removal STRATEGY.
OVERLAP-P tells whether two ticks' labels overlap.  \"parity\" halves
the labels until none overlap; \"greedy\" keeps each label clear of
the last kept one.  With KEEP-LAST, as in Vega, when fewer than three
labels survive without the last one, the last replaces the last kept."
  (let* ((all (append ticks nil)) (ticks all))
    (when (and (>= (length ticks) 3)
               (cl-loop for (a b) on ticks while b thereis (funcall overlap-p a b)))
      (while (progn
               (setq ticks (if (equal strategy "greedy")
                               (let (kept)
                                 (dolist (tk ticks)
                                   (when (or (null kept) (not (funcall overlap-p (car kept) tk))) (push tk kept)))
                                 (nreverse kept))
                             (cl-loop for tk in ticks for i from 0 when (cl-evenp i) collect tk)))
               (and (>= (length ticks) 3)
                    (cl-loop for (a b) on ticks while b thereis (funcall overlap-p a b)))))
      (when (and keep-last (< (length ticks) 3) (not (memq (car (last all)) ticks)))
        (setq ticks (append (if (> (length ticks) 1) (butlast ticks) ticks) (last all)))))
    ticks))

(defun eas-layout--bottom-align (p x0 w flush angle)
  "Label alignment of a bottom tick at P (Vega-Lite labelFlush when FLUSH)."
  (cond ((not (zerop angle)) "right")
        ((and flush (< (abs (- p x0)) 0.5)) "left")
        ((and flush (< (abs (- p (+ x0 w))) 0.5)) "right")
        (t "center")))

(defun eas-layout--positioned-ticks (axis scale)
  "AXIS's ticks with :pos, the position along the axis through SCALE."
  (let ((half (/ (or (plist-get scale :bandwidth) 0) 2.0)))
    (seq-filter (lambda (tk) (plist-get tk :pos))
                (seq-map (lambda (tk)
                           (let ((p (eas-scale-apply scale (plist-get tk :value))))
                             (append tk (list :pos (and p (+ p half))))))
                         (plist-get axis :ticks)))))

(defun eas-layout-axis-place-text (axis scale bounds metrics)
  "AXIS placed for SCALE inside BOUNDS on the character grid of METRICS.
Overlapping labels drop their ticks too; lines sit at cell centres."
  (let* ((x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3))
         (tick (eas-layout--tick axis metrics)) (inset (/ tick 2.0))
         (size (plist-get metrics :label-size)) (cw (plist-get metrics :char-w))
         (bottom (equal (plist-get axis :orient) "bottom"))
         (flush (and bottom (eq (plist-get axis :discrete) :false)))
         (angle (plist-get axis :labelAngle))
         (along (and bottom (zerop angle)))
         (extent (lambda (tk)
                   (let ((p (plist-get tk :pos)))
                     (if along
                         (let ((lw (eas-layout-text-width metrics (plist-get tk :label) size)))
                           (pcase (eas-layout--bottom-align p x0 w flush 0)
                             ("left" (cons p (+ p lw))) ("right" (cons (- p lw) p))
                             (_ (cons (- p (/ lw 2.0)) (+ p (/ lw 2.0))))))
                       (cons (- p (/ size 2.0)) (+ p (/ size 2.0)))))))
         (ticks (eas-layout--thin
                 (sort (eas-layout--positioned-ticks axis scale)
                       (lambda (a b) (< (plist-get a :pos) (plist-get b :pos))))
                 (lambda (a b) (> (+ (cdr (funcall extent a)) (if along cw 0)) (car (funcall extent b))))
                 "parity"))
         (label-extent (eas-layout-axis-label-extent axis metrics))
         (title (plist-get axis :title)))
    (append (eas--plist-without axis :ticks)
            (list :ticks
                  (vconcat
                   (mapcar (lambda (tk)
                             (let ((p (plist-get tk :pos)))
                               (append tk
                                       (if bottom
                                           (list :tick (vector p (+ y0 h inset) p (+ y0 h tick))
                                                 :lx p :ly (+ y0 h tick)
                                                 :align (eas-layout--bottom-align p x0 w flush angle)
                                                 :baseline (if (zerop angle) "top" "middle"))
                                         (list :tick (vector (- x0 tick) p (- x0 inset) p)
                                               :lx (- x0 tick) :ly p :align "right" :baseline "middle"))
                                       (when (eq (plist-get axis :grid) t)
                                         (list :grid (if bottom (vector p y0 p (+ y0 h)) (vector x0 p (+ x0 w) p)))))))
                           ticks))
                  :domain-line (if bottom (vector x0 (+ y0 h inset) (+ x0 w) (+ y0 h inset))
                                 (vector (- x0 inset) y0 (- x0 inset) (+ y0 h))))
            (when title
              (list :title-mark
                    (if bottom
                        (list :text title :x (+ x0 (/ w 2.0)) :y (+ y0 h tick label-extent)
                              :align "center" :baseline "top" :angle 0)
                      (list :text title :x (- x0 tick label-extent) :y (- y0 (plist-get metrics :title-size))
                            :align "left" :baseline "top" :angle 0)))))))

(defun eas-layout-axis-place (axis scale bounds metrics)
  "Return AXIS with geometry for SCALE inside plot BOUNDS [x0 y0 w h].
The svg result carries :bounds, Vega's axis bounds (ticks, visible
labels, title) without the half-pixel translate of the drawn lines."
  (if (eas-layout-text-p metrics)
      (eas-layout-axis-place-text axis scale bounds metrics)
    (let* ((x0 (aref bounds 0)) (y0 (aref bounds 1)) (w (aref bounds 2)) (h (aref bounds 3))
           (tick (eas-layout--tick axis metrics)) (pad (plist-get metrics :label-pad))
           (size (plist-get metrics :label-size))
           (bottom (equal (plist-get axis :orient) "bottom"))
           (flush (and bottom (eq (plist-get axis :discrete) :false)))
           (angle (plist-get axis :labelAngle))
           ;; Vega's axisBand tickOffset: band axes sit half a pixel back;
           ;; ticks are rounded (tickRound), labels are not.
           (band (equal (plist-get scale :type) "band"))
           (ticks (mapcar (lambda (tk) (if band (plist-put tk :pos (- (plist-get tk :pos) 0.5)) tk))
                          (eas-layout--positioned-ticks axis scale)))
           (tick-pos (lambda (tk) (let ((p (plist-get tk :pos))) (if band (eas-layout--round p) p))))
           (label (lambda (tk)
                    (let ((p (plist-get tk :pos)))
                      (if bottom
                          (list :lx p :ly (+ y0 h tick pad)
                                :align (eas-layout--bottom-align p x0 w flush angle)
                                :baseline (if (zerop angle) "top" "middle"))
                        (list :lx (- x0 tick pad) :ly p :align "right" :baseline "middle")))))
           (box (lambda (tk) (let ((l (funcall label tk)))
                               (eas-layout-text-bounds metrics (plist-get tk :label) size (plist-get l :lx) (plist-get l :ly)
                                                         (plist-get l :align) (plist-get l :baseline)
                                                         (if bottom angle 0)))))
           (labelled (seq-remove (lambda (tk) (string-empty-p (plist-get tk :label))) ticks))
           (shown (if (null (plist-get axis :overlap)) labelled
                    (eas-layout--thin
                     labelled
                   (lambda (a b) (let ((p (funcall box a)) (q (funcall box b)))
                                   (> 0 (max (- (aref q 0) (aref p 2)) (- (aref p 0) (aref q 2))
                                             (- (aref q 1) (aref p 3)) (- (aref p 1) (aref q 3))))))
                   (plist-get axis :overlap) t)))
           ;; Ticks count with their 1px stroke on every side, as Vega bounds them.
           (ab (apply #'eas-layout-union
                      (if bottom (vector x0 (+ y0 h) (+ x0 w) (+ y0 h tick))
                        (vector (- x0 tick) y0 x0 (+ y0 h)))
                      (mapcar (lambda (tk) (let ((p (funcall tick-pos tk)))
                                             (if bottom (vector (- p 1) (+ y0 h -1) (+ p 1) (+ y0 h tick 1))
                                               (vector (- x0 tick 1) (- p 1) (+ x0 1) (+ p 1)))))
                              ticks)))
           (placed (vconcat
                    (seq-map (lambda (tk)
                               (let ((p (+ 0.5 (funcall tick-pos tk))) (l (funcall label tk)) (show (memq tk shown)))
                                 (when show (setq ab (eas-layout-union ab (funcall box tk))))
                                 (append (eas--plist-without tk :label)
                                         (list :label (if show (plist-get tk :label) ""))
                                         (if bottom
                                             (list :tick (vector p (+ y0 h 0.5) p (+ y0 h tick 0.5)))
                                           (list :tick (vector (- x0 tick -0.5) p (+ x0 0.5) p)))
                                         (list :lx (+ 0.5 (plist-get l :lx)) :ly (+ 0.5 (plist-get l :ly))
                                               :align (plist-get l :align) :baseline (plist-get l :baseline))
                                         (when (eq (plist-get axis :grid) t)
                                           (list :grid (if bottom (vector p (+ y0 0.5) p (+ y0 h 0.5))
                                                         (vector (+ x0 0.5) p (+ x0 w 0.5) p)))))))
                             ticks)))
           (title (plist-get axis :title))
           (tsize (plist-get metrics :title-size)) (tpad (plist-get metrics :title-pad))
           (weight (plist-get metrics :title-weight))
           (tm (when title
                 (if bottom
                     (list :text title :x (+ x0 (/ w 2.0) 0.5) :y (+ (aref ab 3) tpad 0.5)
                           :align "center" :baseline "top" :angle 0)
                   (list :text title :x (- (aref ab 0) tpad -0.5) :y (+ y0 (/ h 2.0) 0.5)
                         :align "center" :baseline "bottom" :angle -90)))))
      (when tm
        (setq ab (eas-layout-union ab (eas-layout-text-bounds metrics title tsize (- (plist-get tm :x) 0.5)
                                                                  (- (plist-get tm :y) 0.5) (plist-get tm :align)
                                                                  (plist-get tm :baseline) (plist-get tm :angle) weight))))
      (append (eas--plist-without axis :ticks)
              (list :ticks placed
                    :domain-line (if bottom (vector (+ x0 0.5) (+ y0 h 0.5) (+ x0 w 0.5) (+ y0 h 0.5))
                                   (vector (+ x0 0.5) (+ y0 0.5) (+ x0 0.5) (+ y0 h 0.5)))
                    :bounds ab)
              (when tm (list :title-mark tm))))))

(provide 'eas-layout)
;;; eas-layout.el ends here

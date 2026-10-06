;;; eas-label.el --- label layout: point and arc labels that do not overlap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L1 domain transforms for Vega's label layout, which Vega-Lite lacks.
;; Both work in the plot's pixels, so they take the plot size and the
;; scale domains the template gives its linear x and y.
;;
;;   {"x-eas:transform": "label", "x": X, "y": Y, "text": T,
;;    "width": W, "height": H, ...}
;;
;; is Vega's label transform for symbol marks: each row's point is
;; drawn into an occupancy bitmap (with an optional fitted trend line
;; to avoid), then each label in turn takes the first anchor (top,
;; bottom, right, left, ...) whose box stays inside the bounds and
;; touches neither a mark nor a label placed before it.  The anchor (or
;; null for a label left out) goes to the "as" column; a template draws
;; one text layer per anchor with the matching align, baseline and
;; offset.
;;
;;   {"x-eas:transform": "arc-label", "field": F, "width": W, ...}
;;
;; lays out the labels of a pie or donut drawn from F in data order:
;; each wedge's middle angle, a leader from the rim out and across to
;; its side, and label rows on each side pushed apart by the label
;; height so they do not overlap (Vega's labelled donut example).

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)
(require 'eas-transform)
(require 'eas-font)

;;; Bitmaps: a bool-vector of W*H pixels

(defun eas-label--bitmap (w h)
  "An empty W by H occupancy bitmap."
  (list :w w :h h :bits (make-bool-vector (* w h) nil)))

(defun eas-label--fill (bm x0 y0 x1 y1)
  "Mark pixels X0..X1, Y0..Y1 (inclusive, clipped) of bitmap BM."
  (let ((w (plist-get bm :w)) (h (plist-get bm :h)) (bits (plist-get bm :bits)))
    (cl-loop for y from (max 0 (floor y0)) to (min (1- h) (floor y1))
             do (cl-loop for x from (max 0 (floor x0)) to (min (1- w) (floor x1))
                         do (aset bits (+ x (* y w)) t)))))

(defun eas-label--free-p (bm x0 y0 x1 y1)
  "Non-nil when no pixel of X0..X1, Y0..Y1 in bitmap BM is marked."
  (let ((w (plist-get bm :w)) (h (plist-get bm :h)) (bits (plist-get bm :bits)))
    (cl-loop for y from (max 0 (floor y0)) to (min (1- h) (floor y1))
             always (cl-loop for x from (max 0 (floor x0)) to (min (1- w) (floor x1))
                             never (aref bits (+ x (* y w)))))))

(defun eas-label--disc (bm cx cy r)
  "Mark the disc of radius R about CX, CY in bitmap BM."
  (cl-loop for y from (floor (- cy r)) to (ceiling (+ cy r))
           for dy = (- (+ y 0.5) cy)
           for half = (sqrt (max 0 (- (* r r) (* dy dy))))
           when (> half 0) do (eas-label--fill bm (- cx half) y (+ cx half) y)))

(defun eas-label--segment (bm x0 y0 x1 y1 width)
  "Mark the segment X0,Y0 to X1,Y1, WIDTH pixels wide, in bitmap BM."
  (let* ((n (max 1 (ceiling (max (abs (- x1 x0)) (abs (- y1 y0))))))
         (r (/ width 2.0)))
    (dotimes (i (1+ n))
      (let ((x (+ x0 (* (- x1 x0) (/ (float i) n)))) (y (+ y0 (* (- y1 y0) (/ (float i) n)))))
        (eas-label--fill bm (- x r) (- y r) (+ x r) (+ y r))))))

;;; label: Vega's label transform for points

(defconst eas-label-anchors ["top-right" "top" "top-left" "left" "bottom-left" "bottom" "bottom-right" "right"]
  "Vega's default anchors, tried in order.")

(defun eas-label--domain (values given zero)
  "The [LO HI] domain: GIVEN when a 2-vector, else the extent of VALUES.
With ZERO the extent includes 0, as a Vega linear scale does."
  (if (and (vectorp given) (= (length given) 2)) (cons (aref given 0) (aref given 1))
    (let ((lo (apply #'min values)) (hi (apply #'max values)))
      (when zero (setq lo (min lo 0) hi (max hi 0)))
      (cons lo (if (= lo hi) (1+ hi) hi)))))

(defun eas-label--box (anchor px py r offset tw th)
  "The box (X0 Y0 X1 Y1) of a TW by TH label at ANCHOR of a point.
The point is at PX, PY with radius R; the label keeps OFFSET away."
  (let* ((d (+ r offset))
         (left (string-suffix-p "left" anchor)) (right (string-suffix-p "right" anchor))
         (top (string-prefix-p "top" anchor)) (bottom (string-prefix-p "bottom" anchor))
         (x0 (cond ((member anchor '("left")) (- px d tw))
                   ((member anchor '("right")) (+ px d))
                   (left (- px d tw)) (right (+ px d))
                   (t (- px (/ tw 2.0)))))
         (y0 (cond (top (- py d th)) (bottom (+ py d))
                   (t (- py (/ th 2.0))))))
    (list x0 y0 (+ x0 tw) (+ y0 th))))

(defun eas-label--transform (rows params)
  "Place a label per row of ROWS as transform PARAMS describe."
  (let* ((xf (eas-key (plist-get params :x))) (yf (eas-key (plist-get params :y)))
         (tf (eas-key (plist-get params :text)))
         (w (plist-get params :width)) (h (plist-get params :height))
         (size (or (plist-get params :size) (vector w h)))
         (bw (ceiling (aref size 0))) (bh (ceiling (aref size 1)))
         (zero (not (eq (plist-get params :zero) :false)))
         (font-size (or (plist-get params :fontSize) 11))
         (weight (plist-get params :fontWeight))
         (anchors (append (or (plist-get params :anchor) eas-label-anchors) nil))
         (offsets (let ((o (or (plist-get params :offset) [1]))) (if (vectorp o) (append o nil) (list o))))
         (r (sqrt (/ (or (plist-get params :markSize) 30) float-pi)))
         (as (eas-key (or (plist-get params :as) "label_anchor")))
         (valid (seq-filter (lambda (row) (and (numberp (plist-get row xf)) (numberp (plist-get row yf)))) rows))
         (out (copy-sequence rows)))
    (if (null valid)
        (seq-map (lambda (row) (append row (list as :null))) rows)
      (let* ((xd (eas-label--domain (mapcar (lambda (row) (plist-get row xf)) valid) (plist-get params :xDomain) zero))
             (yd (eas-label--domain (mapcar (lambda (row) (plist-get row yf)) valid) (plist-get params :yDomain) zero))
             (px (lambda (v) (* w (/ (- v (car xd)) (float (- (cdr xd) (car xd)))))))
             (py (lambda (v) (- h (* h (/ (- v (car yd)) (float (- (cdr yd) (car yd))))))))
             (marks (eas-label--bitmap bw bh))
             (labels (eas-label--bitmap bw bh)))
        (dolist (row valid)
          (eas-label--disc marks (funcall px (plist-get row xf)) (funcall py (plist-get row yf)) r))
        ;; A fitted trend line to keep clear of, as Vega's avoidMarks.
        (when-let* ((method (plist-get params :avoidRegression)))
          (let ((curve (eas-transform-run (vector (list :regression (plist-get params :y) :on (plist-get params :x)
                                                        :method method :as ["x-eas-u" "x-eas-v"]))
                                          (vconcat valid))))
            (cl-loop for i from 1 below (length curve)
                     for a = (aref curve (1- i)) for b = (aref curve i)
                     do (eas-label--segment marks (funcall px (plist-get a :x-eas-u)) (funcall py (plist-get a :x-eas-v))
                                            (funcall px (plist-get b :x-eas-u)) (funcall py (plist-get b :x-eas-v))
                                            (or (plist-get params :avoidWidth) 2)))))
        (dotimes (i (length out))
          (let* ((row (aref out i)) (text (plist-get row tf))
                 (placed
                  (when (and (numberp (plist-get row xf)) (numberp (plist-get row yf))
                             text (not (eq text :null)) (not (equal text "")))
                    (let ((x (funcall px (plist-get row xf))) (y (funcall py (plist-get row yf)))
                          (tw (eas-font-text-width (format "%s" text) font-size weight)))
                      (cl-loop for offset in offsets
                               thereis (cl-loop for anchor across (vconcat anchors)
                                                for box = (eas-label--box anchor x y r offset tw font-size)
                                                when (and (>= (nth 0 box) 0) (>= (nth 1 box) 0)
                                                          (<= (nth 2 box) bw) (<= (nth 3 box) bh)
                                                          (apply #'eas-label--free-p marks box)
                                                          (apply #'eas-label--free-p labels box))
                                                return (progn (apply #'eas-label--fill labels box) anchor)))))))
            (aset out i (append row (list as (or placed :null))))))
        out))))

(eas-register-transform
 "label"
 :doc "Vega's label layout for points: each row's anchor (top, bottom, ...) where its text overlaps no point or earlier label, or null."
 :schema '(:x (:type "string" :required t :doc "x field of the points")
           :y (:type "string" :required t :doc "y field of the points")
           :text (:type "string" :required t :doc "label text field")
           :width (:type "number" :required t :doc "plot width in pixels")
           :height (:type "number" :required t :doc "plot height in pixels")
           :size (:type "array" :doc "[w, h] pixels labels must stay inside (default the plot)")
           :xDomain (:type "array" :doc "x scale domain (default the data extent)")
           :yDomain (:type "array" :doc "y scale domain (default the data extent)")
           :zero (:type "boolean" :default t :doc "default domains include zero, as Vega's linear scales")
           :fontSize (:type "number" :default 11) :fontWeight (:type "any")
           :anchor (:type "array" :doc "anchors to try in order (default Vega's eight)")
           :offset (:type "any" :default 1 :doc "pixels between point and label, or an array to try")
           :markSize (:type "number" :default 30 :doc "point area in square pixels")
           :avoidRegression (:type "string" :doc "also avoid the fitted y-on-x regression line of this method")
           :avoidWidth (:type "number" :default 2 :doc "width of the avoided line")
           :as (:type "string" :default "label_anchor" :doc "output column: the anchor or null"))
 :fn #'eas-label--transform)

;;; arc-label: leader lines and labels of a pie or donut

(defun eas-label--arc-transform (rows params)
  "Lay out pie labels for ROWS as transform PARAMS describe."
  (let* ((field (eas-key (plist-get params :field)))
         (w (plist-get params :width)) (h (or (plist-get params :height) w))
         (cx (/ w 2.0)) (cy (/ h 2.0))
         (radius (or (plist-get params :outerRadius) (/ (min w h) 2.0)))
         (start (or (plist-get params :startAngle) 0))
         (span (- (or (plist-get params :endAngle) (* 2 float-pi)) start))
         (gap (or (plist-get params :labelHeight) 12))
         (separate (not (eq (plist-get params :separate) :false)))
         (prefix (or (plist-get params :prefix) "arc_"))
         (key (lambda (name) (eas-key (concat prefix name))))
         (values (seq-map (lambda (row) (let ((v (plist-get row field))) (if (and (numberp v) (> v 0)) v 0))) rows))
         (total (float (max (apply #'+ 0 (append values nil)) 1e-12)))
         (acc start) out)
    (seq-doseq (row rows)
      (let* ((v (let ((v (plist-get row field))) (if (and (numberp v) (> v 0)) v 0)))
             (a0 acc) (a1 (+ acc (* span (/ v total))))
             ;; Angles run clockwise from 12 o'clock; screen angles from 3 o'clock.
             (mid (- (/ (+ a0 a1) 2.0) (/ float-pi 2)))
             (from-top (mod (+ mid (/ float-pi 2)) (* 2 float-pi)))
             (right (<= from-top float-pi))
             (y2 (+ cy (* (+ radius 10) (sin mid)))))
        (setq acc a1)
        (push (append row (list (funcall key "side") (if right "right" "left")
                                (funcall key "x1") (+ cx (* radius (cos mid)))
                                (funcall key "y1") (+ cy (* radius (sin mid)))
                                (funcall key "x2") (+ cx (* (+ radius 10) (cos mid)))
                                (funcall key "y2") y2
                                (funcall key "x3") (if right (+ cx radius 20) (- cx radius 20))
                                (funcall key "x4") (if right (+ cx radius 25) (- cx radius 25))
                                (funcall key "y4") y2))
              out)))
    (setq out (vconcat (nreverse out)))
    (when separate
      ;; Down each side, a label sits at least GAP below the one above it.
      (dolist (side '("left" "right"))
        (let ((idx (sort (seq-filter (lambda (i) (equal (plist-get (aref out i) (funcall key "side")) side))
                                     (number-sequence 0 (1- (length out))))
                         (lambda (i j) (< (plist-get (aref out i) (funcall key "y2"))
                                          (plist-get (aref out j) (funcall key "y2"))))))
              (floor nil))
          (dolist (i idx)
            (let* ((row (aref out i)) (y (plist-get row (funcall key "y2")))
                   (y (if (and floor (< y (+ floor gap))) (+ floor gap) y)))
              (setq floor y)
              (aset out i (plist-put (copy-sequence row) (funcall key "y4") y)))))))
    out))

(eas-register-transform
 "arc-label"
 :doc "Pie or donut label layout: each wedge's leader line (x1 y1 x2 y2 x3 x4 y4) and side, labels kept apart."
 :schema '(:field (:type "string" :required t :doc "the wedge size field, wedges in data order")
           :width (:type "number" :required t :doc "plot width in pixels; the pie is centred")
           :height (:type "number" :doc "plot height in pixels (default width)")
           :outerRadius (:type "number" :doc "pie radius in pixels (default half the smaller side)")
           :startAngle (:type "number" :default 0 :doc "radians clockwise from 12 o'clock")
           :endAngle (:type "number" :doc "radians (default startAngle + 2 pi)")
           :labelHeight (:type "number" :default 12 :doc "least vertical distance between labels")
           :separate (:type "boolean" :default t :doc "push overlapping labels apart; false keeps each by its wedge")
           :prefix (:type "string" :default "arc_" :doc "prefix of the output columns"))
 :fn #'eas-label--arc-transform)

(provide 'eas-label)
;;; eas-label.el ends here

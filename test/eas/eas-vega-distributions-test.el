;;; eas-vega-distributions-test.el --- Vega gallery templates: distributions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The thirteen templates/vega/ templates of the Vega gallery's
;; Distributions group (eas-7r1.2) and the engine pieces they brought:
;; the dotbin domain transform, the distribution expression functions,
;; density's resolve "independent", dense_rank and minExtent on an
;; offset axis.  Interactions run headless through `eas-dispatch'.
;;
;; The :gallery test renders every example natively, rasterizes it and
;; holds it to the threshold its template records under x-eas.vega against
;; test/vega-examples/ref; it skips, with the reason, without a
;; rasterizer (`eas-chart-rsvg-program').

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-png)
(require 'eas-vl-gallery)
(require 'eas-test-support)

(defconst eas-vega-distributions-names
  '("top-k-plot" "top-k-plot-with-others" "histogram" "histogram-null-values" "dot-plot"
    "probability-density" "box-plot" "violin-plot" "binned-scatter-plot" "wheat-plot"
    "quantile-quantile-plot" "quantile-dot-plot" "time-units")
  "The Distributions examples of the Vega gallery.")

(defconst eas-vega-distributions-slow '("time-units")
  "Examples whose bindings are too large to render in the fast suite.")

(defun eas-vega-distributions-template (name)
  "The registered template of gallery example NAME."
  (eas-template-get (concat "vega/" name)))

(defun eas-vega-distributions-status (name)
  "The x-eas.vega object of example NAME's template."
  (plist-get (plist-get (eas-vega-distributions-template name) :meta) :vega))

(defun eas-vega-distributions-spec (name &optional bindings)
  "Example NAME resolved with BINDINGS, by default its example's."
  (eas-resolve (concat "vega/" name) (or bindings (eas-template-example (concat "vega/" name)))))

(defun eas-vega-distributions-ref (name)
  "The reference PNG of gallery example NAME."
  (expand-file-name (concat "test/vega-examples/ref/" name ".png") eas-template--root))

(defun eas-vega-distributions-view (name bindings)
  "Open template NAME with BINDINGS as a fresh view; return it."
  (eas-view-open (concat "vega/" name) :bindings bindings :id (concat "vega-distributions:" name)))

(defun eas-vega-distributions-mark (view name)
  "The first mark called NAME in VIEW's scene."
  (seq-some (lambda (v) (seq-find (lambda (m) (equal (plist-get m :id) name)) (plist-get v :marks)))
            (plist-get (eas-view-scene view) :views)))

(defun eas-vega-distributions-marks (view)
  "Every mark of VIEW's scene."
  (apply #'append (mapcar (lambda (v) (append (plist-get v :marks) nil))
                          (plist-get (eas-view-scene view) :views))))

(defun eas-vega-distributions-rows (n field &optional fn)
  "N rows {FIELD: (FN i)} (FN defaults to the identity), as a vector."
  (vconcat (mapcar (lambda (i) (list (eas-key field) (if fn (funcall fn i) i)))
                   (number-sequence 0 (1- n)))))

(defun eas-vega-distributions-compare (png ref)
  "Compare native PNG with REF as `eas-png-compare' does, shifts searched fully.
`eas-png-compare' aligns by ink profiles, which a sparse chart can
fool; this also tries every shift within 8 pixels across and 2 down
and keeps the one with the fewest differing pixels.  Return the plist
of the best alignment."
  (let* ((n (eas-png-read png)) (r (eas-png-read ref))
         (best (eas-png-compare n r))
         (bg (eas-png--background r))
         (limit (* 35215 eas-png-pixel-threshold eas-png-pixel-threshold))
         (off (plist-get best :offset))
         (fewest (car (eas-png--count r n (aref off 0) (aref off 1) bg limit))))
    (dolist (dx (number-sequence -8 8))
      (dolist (dy (number-sequence -2 2))
        (let ((c (eas-png--count r n dx dy bg limit fewest)))
          (when (< (car c) fewest)
            (setq fewest (car c)
                  best (plist-put (plist-put (copy-sequence best) :ratio (/ (float (car c)) (max 1 (cdr c))))
                                  :offset (vector dx dy)))))))
    best))

;;; The templates

(ert-deftest eas-vega-distributions-templates-are-registered ()
  "Every example has a vega/ template, a binding and a recorded status."
  (dolist (name eas-vega-distributions-names)
    (let* ((tpl (eas-vega-distributions-template name)) (status (eas-vega-distributions-status name)))
      (should (equal (plist-get tpl :name) (concat "vega/" name)))
      (should (file-exists-p (eas-template-example-file tpl)))
      (should (string-match-p "/examples/vega/" (eas-template-example-file tpl)))
      (should (member (plist-get status :status) '("pass" "partial" "unsupported")))
      (should (and (stringp (plist-get status :note)) (> (length (plist-get status :note)) 0)))
      (should (numberp (plist-get status :ratio)))
      (should (<= (plist-get status :ratio) (plist-get status :threshold)))))
  ;; The bare name "histogram" is still the house template.
  (should (equal (plist-get (eas-template-get "histogram") :name) "histogram")))

(ert-deftest eas-vega-distributions-examples-check-clean ()
  "Every example resolves to pure Vega-Lite the native engine draws."
  (dolist (name (seq-difference eas-vega-distributions-names eas-vega-distributions-slow))
    (let ((spec (eas-vega-distributions-spec name)))
      (should-not (plist-get spec :x-eas))
      (should (equal (cons name (eas-spec-check spec)) (list name)))
      (should (string-prefix-p "<svg" (eas-vl-gallery-svg spec)))
      (should (> (length (string-trim (eas-vl-gallery-text spec))) 0)))))

(ert-deftest eas-vega-distributions-templates-bind-other-data ()
  "Field slots let a caller bind their own data."
  (let* ((rows (eas-vega-distributions-rows 40 "score" (lambda (i) (* 0.1 (- (% (* i 7) 40) 20)))))
         (spec (eas-vega-distributions-spec "histogram" (list :data rows :field "score" :extent [-2 2] :step 0.5))))
    (should (equal (eas-spec-check spec) nil))
    (should (string-prefix-p "<svg" (eas-vl-gallery-svg spec))))
  (let* ((rows (vconcat (mapcar (lambda (i) (list :team (format "t%d" (% i 7)) :points (* i 3)))
                                (number-sequence 1 50))))
         (v (eas-vega-distributions-view "top-k-plot" (list :data rows :category "team" :value "points"
                                                            :op "sum" :k 4 :format ",d"))))
    (should (= (length (plist-get (car (eas-vega-distributions-marks v)) :items)) 4)))
  (eas-test-should-code "FIELD_MISSING"
    (eas-vega-distributions-spec "box-plot" (list :data [(:a 1)] :category "a" :value "b"))))

;;; dotbin

(defconst eas-vega-distributions-sleep
  [2.1 2.1 3.2 3.2 3.3 4.7 4.8 4.9 4.9 5.2 5.7 6.1 6.3 6.3 6.5 6.6 7.4 7.4 7.5 7.6 7.7 8.1 8.2 8.3
   8.4 8.4 8.6 9.1 9.5 9.7 10.0 10.4 10.6 10.8 10.9 11.0 11.0 11.0 11.9 11.9 12.0 12.8 13.2 13.8
   14.3 15.2 15.8 17.9]
  "The gallery dot plot's animal sleep times, ascending.")

(ert-deftest eas-vega-distributions-dotbin-matches-vega ()
  "Bin centers equal vega-statistics' dotbin, smoothed or not."
  (let ((plain (eas-dotbin eas-vega-distributions-sleep 0.65))
        (smooth (eas-dotbin eas-vega-distributions-sleep 0.65 t))
        ;; From vega-statistics' dotbin (node), on the same data.
        (vega-plain [2.1 2.1 3.25 3.25 3.25 4.95 4.95 4.95 4.95 4.95 6 6 6 6 6.55 6.55 7.55 7.55 7.55
                     7.55 7.55 8.35 8.35 8.35 8.35 8.35 8.35 9.4 9.4 9.4 10.3 10.3 10.3 10.9 10.9 10.9
                     10.9 10.9 11.95 11.95 11.95 13 13 14.05 14.05 15.5 15.5 17.9])
        (vega-smooth [2.1 2.1 3.25 3.25 3.25 4.95 4.95 4.95 4.95 4.95 6 6 6 6.55 6.55 6.55 7.55 7.55
                      7.55 7.55 7.55 8.35 8.35 8.35 8.35 8.35 8.35 9.4 9.4 9.4 10.3 10.3 10.3 10.9 10.3
                      10.9 10.9 10.9 11.95 11.95 11.95 13 13 14.05 14.05 15.5 15.5 17.9]))
    (dolist (pair (list (cons plain vega-plain) (cons smooth vega-smooth)))
      (should (= (length (car pair)) 48))
      (seq-mapn (lambda (a b) (should (< (abs (- a b)) 1e-9))) (car pair) (cdr pair))))
  (should (equal (eas-dotbin [] 1) []))
  (should (equal (eas-dotbin [5] 1 t) [5.0])))

(ert-deftest eas-vega-distributions-dotbin-transform ()
  "The x-eas dotbin transform keeps row order, groups and nulls."
  (let ((out (eas-transform-apply-domain
              '(:x-eas:transform "dotbin" :field "v" :step 1 :groupby ["g"])
              [(:g "a" :v 3) (:g "b" :v 0) (:g "a" :v 1) (:g "a" :v :null) (:g "a" :v 1.5) (:g "b" :v 0.5)])))
    (should (equal (seq-map (lambda (r) (plist-get r :bin)) out) '(3.0 0.25 1.25 :null 1.25 0.25)))
    (should (equal (seq-map (lambda (r) (plist-get r :v)) out) '(3 0 1 :null 1.5 0.5))))
  ;; Without a step: a thirtieth of the span.
  (let ((out (eas-transform-apply-domain '(:x-eas:transform "dotbin" :field "v" :as "b")
                                         [(:v 0) (:v 30) (:v 0.5) (:v 1)])))
    (should (equal (seq-map (lambda (r) (plist-get r :b)) out) '(0.25 30.0 0.25 1.0))))
  (eas-test-should-code "INVALID_INPUT"
    (eas-transform-apply-domain '(:x-eas:transform "dotbin") [(:v 1)])))

(ert-deftest eas-vega-distributions-dotbin-materializes-at-resolve ()
  "A template's dotbin becomes a plain column: the resolved spec is pure Vega-Lite."
  (let* ((spec (eas-vega-distributions-spec "dot-plot"))
         (rows (plist-get (plist-get spec :data) :values)))
    (should-not (seq-some (lambda (tr) (plist-get tr :x-eas:transform)) (plist-get spec :transform)))
    (should (= (length rows) 48))
    (should (seq-every-p (lambda (r) (numberp (plist-get r :dotbin))) rows))))

;;; Engine pieces

(ert-deftest eas-vega-distributions-expression-functions ()
  "Vega's distribution functions, as vega-statistics computes them."
  (dolist (c '(("densityNormal(0)" 0.3989422804014327) ("densityNormal(1, 1, 2)" 0.19947114020071635)
               ("cumulativeNormal(0)" 0.5) ("cumulativeNormal(1.96)" 0.9750021048517795)
               ("cumulativeNormal(-1, 0, 1)" 0.15865525393145707) ("cumulativeNormal(40)" 1.0)
               ("quantileLogNormal(0.5, 0, 1)" 1.0) ("quantileLogNormal(0.05, log(11.4), 0.2)" 8.204170561292335)
               ("cumulativeLogNormal(8.204170561292335, log(11.4), 0.2)" 0.05)
               ("densityLogNormal(1)" 0.3989422804014327) ("densityLogNormal(-1)" 0.0)
               ("densityUniform(0.5)" 1.0) ("densityUniform(3, 0, 2)" 0.0)
               ("cumulativeUniform(1, 0, 4)" 0.25) ("cumulativeUniform(9)" 1.0)))
    (should (< (abs (- (eas-expr-evaluate (car c) nil) (cadr c))) 1e-7))))

(ert-deftest eas-vega-distributions-dense-rank-starts-at-one ()
  "dense_rank numbers distinct sort keys 1, 2, 3 as Vega does."
  (should (equal (seq-map (lambda (r) (plist-get r :r))
                          (eas-transform-run [(:window [(:op "dense_rank" :as "r")] :sort [(:field "v")])]
                                             [(:v 1) (:v 2) (:v 2) (:v 3)]))
                 '(1 2 2 3))))

(ert-deftest eas-vega-distributions-density-resolve-independent ()
  "resolve \"independent\" samples each group over its own extent."
  (let* ((rows [(:g "a" :v 0) (:g "a" :v 1) (:g "a" :v 2) (:g "b" :v 10) (:g "b" :v 14)])
         (extent (lambda (resolve)
                   (let ((out (eas-transform-run (vector (append '(:density "v" :groupby ["g"] :steps 4)
                                                                 (and resolve (list :resolve resolve))))
                                                 rows)))
                     (mapcar (lambda (g)
                               (let ((xs (seq-map (lambda (r) (plist-get r :value))
                                                  (seq-filter (lambda (r) (equal (plist-get r :g) g)) out))))
                                 (list (seq-min xs) (seq-max xs))))
                             '("a" "b"))))))
    (should (equal (funcall extent nil) '((0.0 14.0) (0.0 14.0))))
    (should (equal (funcall extent "shared") '((0.0 14.0) (0.0 14.0))))
    (should (equal (funcall extent "independent") '((0.0 2.0) (10.0 14.0))))))

(ert-deftest eas-vega-distributions-offset-axis-keeps-min-extent ()
  "minExtent reserves room on an axis drawn with an offset too."
  (let ((width (lambda (axis)
                 (plist-get (plist-get (eas-compile (list :data '(:values [(:a "x" :b 1)]) :mark "bar"
                                                          :width 100 :height 50
                                                          :encoding (list :x '(:field "a" :type "nominal")
                                                                          :y (list :field "b" :type "quantitative"
                                                                                   :axis axis))))
                                       :size)
                            :w))))
    (should (= (- (funcall width '(:offset 5 :minExtent 80)) (funcall width '(:offset 5)))
               (- (funcall width '(:minExtent 80)) (funcall width nil))))
    (should (> (funcall width '(:offset 5 :minExtent 80)) (funcall width '(:offset 5))))))

;;; Interactions, headless

(ert-deftest eas-vega-distributions-top-k-input-sets-k ()
  "The k range input is a param: dispatching it redraws k bars."
  (let* ((rows (vconcat (mapcar (lambda (i) (list :who (format "d%02d" i) :gross (* i 1000)))
                                (number-sequence 1 30))))
         (bind (list :data rows :category "who" :value "gross"))
         (bars (lambda (v) (length (plist-get (car (eas-vega-distributions-marks v)) :items)))))
    (let ((v (eas-vega-distributions-view "top-k-plot" bind)))
      (should (= (funcall bars v) 20))
      (eas-dispatch v '(:type "param" :param "k" :value 5))
      (should (= (funcall bars v) 5)))
    (let ((v (eas-vega-distributions-view "top-k-plot-with-others" bind)))
      ;; k-1 categories and the others.
      (should (= (funcall bars v) 20))
      (eas-dispatch v '(:type "param" :param "k" :value 3))
      (should (= (funcall bars v) 3))
      (should (member "All Others" (seq-map (lambda (r) (plist-get r :group))
                                            (plist-get (car (eas-vega-distributions-marks v)) :rows)))))))

(ert-deftest eas-vega-distributions-quantile-dots-follow-the-threshold ()
  "Dots in bins below the threshold turn red; the readout moves with it."
  (let* ((bind (eas-template-example "vega/quantile-dot-plot"))
         (v (eas-vega-distributions-view "quantile-dot-plot" bind))
         (red (lambda () (seq-count (lambda (it) (equal (plist-get it :fill) "firebrick"))
                                    (plist-get (eas-vega-distributions-mark v "dots") :items))))
         (label (lambda () (seq-some (lambda (m) (and (equal (plist-get m :mark) "text")
                                                      (seq-map (lambda (it) (plist-get it :text)) (plist-get m :items))))
                                     (eas-vega-distributions-marks v)))))
    (should (= (funcall red) 2))
    (should (equal (funcall label) '("5.0%")))
    (eas-dispatch v '(:type "param" :param "threshold" :value 12))
    (should (= (funcall red) 11))
    (should (equal (funcall label) '("60.1%")))))

(ert-deftest eas-vega-distributions-hover-highlights-a-bar ()
  "Pointing at a bar fills it firebrick, as the gallery's hover does."
  (let* ((rows (vconcat (mapcar (lambda (i) (list :date (format "2001-01-%02dT12:00:00" (1+ i)) :delay (* i 2)))
                                (number-sequence 0 27))))
         (v (eas-vega-distributions-view "time-units" (list :data rows)))
         (bars (lambda () (plist-get (car (eas-vega-distributions-marks v)) :items)))
         (first (aref (funcall bars) 0)))
    (should (= (length (funcall bars)) 7))
    (should-not (seq-find (lambda (it) (equal (plist-get it :fill) "firebrick")) (funcall bars)))
    (eas-dispatch v (list :type "pointermove"
                          :px (vector (+ (plist-get first :x) (/ (plist-get first :w) 2))
                                      (+ (plist-get first :y) (/ (plist-get first :h) 2)))))
    (should (equal (seq-map (lambda (it) (plist-get it :fill)) (funcall bars))
                   '("firebrick" "steelblue" "steelblue" "steelblue" "steelblue" "steelblue" "steelblue")))
    (eas-dispatch v '(:type "pointerleave"))
    (should-not (seq-find (lambda (it) (equal (plist-get it :fill) "firebrick")) (funcall bars))))
  ;; A wheat plot's ring under the pointer turns firebrick alone.
  (let* ((rows (eas-vega-distributions-rows 34 "u" (lambda (i) (* 0.03 (- i 17)))))
         (v (eas-vega-distributions-view "wheat-plot" (list :data rows :height 40)))
         (ring (aref (plist-get (eas-vega-distributions-mark v "rings") :items) 5)))
    (eas-dispatch v (list :type "pointermove" :px (vector (plist-get ring :x) (plist-get ring :y))))
    (should (= (seq-count (lambda (it) (equal (plist-get it :stroke) "firebrick"))
                          (plist-get (eas-vega-distributions-mark v "rings") :items))
               1))))

;;; Against the Vega gallery (minutes)

(ert-deftest eas-vega-distributions-hold-their-status ()
  "Every example renders natively within the threshold its template records."
  :tags '(:gallery)
  (unless (executable-find eas-chart-rsvg-program)
    (eas-test-skip (format "%s is not on PATH to rasterize native SVG for test/vega-examples/ref"
                           eas-chart-rsvg-program)))
  (let (problems)
    (dolist (name eas-vega-distributions-names)
      (let* ((status (eas-vega-distributions-status name))
             (png (make-temp-file "eas-vega" nil ".png")))
        (unwind-protect
            (progn
              (eas-chart-rasterize (eas-vl-gallery-svg (eas-vega-distributions-spec name)) png)
              (let* ((cmp (eas-vega-distributions-compare png (eas-vega-distributions-ref name)))
                     (ratio (plist-get cmp :ratio)) (delta (plist-get cmp :size-delta)))
                (when (> ratio (plist-get status :threshold))
                  (push (format "%s: ratio %.4f over its threshold %s" name ratio (plist-get status :threshold)) problems))
                (when (> (max (abs (aref delta 0)) (abs (aref delta 1))) (or (plist-get status :size) 8))
                  (push (format "%s: size delta %S" name delta) problems))))
          (delete-file png))))
    (should (equal (nreverse problems) nil))))

(provide 'eas-vega-distributions-test)
;;; eas-vega-distributions-test.el ends here

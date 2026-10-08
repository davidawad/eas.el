;;; eas-vega-std-basic-test.el --- Vega gallery templates: bar, line/area, circular, scatter -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The eas-7r1.1 share of the Vega example gallery: 23 examples, each a
;; template in templates/vega/NAME.json with its example bindings in
;; examples/vega/NAME.data.json, judged against
;; test/vega-examples/ref/NAME.png.  The template's x-eas.vega block
;; records the verdict (status, note, the measured ratio and the
;; threshold the image oracle holds it to).
;;
;; The fast tests check the templates load, bind, resolve and draw
;; natively, the engine pieces they brought (eas-label.el, shared-scale
;; extra axes, kept band paddings, dense_rank) and the interactions,
;; through `eas-dispatch'.  The :gallery test rasterizes each example
;; and compares it with its reference; it skips without rsvg-convert.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-png)
(require 'eas-chart)

(defconst eas-vega-std-basic-names
  '("bar-chart" "stacked-bar-chart" "grouped-bar-chart" "nested-bar-chart" "population-pyramid"
    "line-chart" "area-chart" "stacked-area-chart" "horizon-graph" "job-voyager"
    "pie-chart" "donut-chart" "donut-chart-labelled" "radial-plot" "radar-chart"
    "scatter-plot" "scatter-plot-null-values" "connected-scatter-plot" "error-bars"
    "barley-trellis-plot" "regression" "loess-regression" "labeled-scatter-plot")
  "The Vega gallery examples this file covers.")

(defconst eas-vega-std-basic-slow '("job-voyager")
  "Examples too large to draw in the fast suite; the :gallery test draws them.")

(defun eas-vega-std-basic--name (name)
  "The registry name of Vega example NAME."
  name)

(defun eas-vega-std-basic--meta (name)
  "The x-eas.vega block of example NAME's template."
  (plist-get (plist-get (eas-template-get (eas-vega-std-basic--name name)) :meta) :vega))

(defun eas-vega-std-basic--spec (name)
  "Example NAME's template resolved with its example bindings."
  (let ((tpl (eas-vega-std-basic--name name)))
    (eas-resolve tpl (eas-template-example tpl))))

(defun eas-vega-std-basic--blocking (spec)
  "Findings of SPEC that keep it from drawing natively (not property warnings)."
  (seq-remove (lambda (f) (plist-get f :property)) (eas-spec-check spec)))

(defun eas-vega-std-basic--marks (scene)
  "Every mark of SCENE's views, nested views included."
  (let (out)
    (cl-labels ((walk (views)
                  (seq-doseq (v views)
                    (seq-doseq (m (plist-get v :marks)) (push m out))
                    (walk (plist-get v :views)))))
      (walk (plist-get scene :views)))
    (nreverse out)))

(defun eas-vega-std-basic--items (scene type)
  "Items of SCENE's marks of mark TYPE."
  (cl-loop for m in (eas-vega-std-basic--marks scene)
           when (equal (plist-get m :mark) type) append (append (plist-get m :items) nil)))

;;; Templates load and bind

(ert-deftest eas-vega-std-basic-templates-declare-their-example ()
  (dolist (name eas-vega-std-basic-names)
    (let* ((tpl (eas-template-get (eas-vega-std-basic--name name)))
           (meta (plist-get (plist-get tpl :meta) :vega)))
      (should (equal (plist-get tpl :name) (eas-vega-std-basic--name name)))
      (should (string-suffix-p (concat "templates/vega/" name ".json") (plist-get tpl :path)))
      (should (equal (plist-get meta :example) name))
      (should (member (plist-get meta :status) '("pass" "partial" "unsupported")))
      (should (stringp (plist-get meta :note)))
      (should (file-exists-p (eas-test-file "test/vega-examples/ref" (concat name ".png"))))
      (should (file-exists-p (eas-template-example-file tpl)))
      ;; A bare name finds the namespaced template.
      (should (eas-template-p name)))))

(ert-deftest eas-vega-std-basic-example-files-resolve-beside-the-example ()
  "A data slot bound to a relative file reads it beside the example."
  (let ((b (eas-template-example (eas-vega-std-basic--name "scatter-plot"))))
    (should (file-name-absolute-p (plist-get (plist-get b :data) :file)))
    (should (file-exists-p (plist-get (plist-get b :data) :file))))
  (let ((dir (make-temp-file "eas-vega-std-basic" t)))
    (unwind-protect
        (let ((file (expand-file-name "b.json" dir)))
          (with-temp-file file (insert "{\"data\": {\"file\": \"rows.json\"}, \"title\": \"t\"}"))
          (should (equal (eas-template-read-bindings file)
                         (list :data (list :file (expand-file-name "rows.json" dir)) :title "t"))))
      (delete-directory dir t))))

(ert-deftest eas-vega-std-basic-slots-take-a-callers-data ()
  "A caller's own rows and field names go through the slots."
  (let* ((rows [(:k "x" :n 3) (:k "y" :n 5)])
         (spec (eas-resolve "bar-chart" (list :data rows :category "k" :amount "n")))
         (bars (eas-vega-std-basic--items (eas-compile spec) "bar")))
    (should (= (length bars) 2)))
  (eas-test-should-code "FIELD_MISSING"
    (eas-resolve "pie-chart" (list :data [(:a 1)]))))

(defun eas-vega-std-basic--schema-faults (node path)
  "Vega-Lite schema faults under NODE at PATH, as a list of strings.
A fault is a null encoding channel or mark property, or a scale or
legend on a datum channel."
  (cond
   ((and (consp node) (keywordp (car node)))
    (cl-loop for (k v) on node by #'cddr
             for p = (concat path "." (substring (symbol-name k) 1))
             when (and (memq v '(nil :null))
                       (string-match-p "\\.\\(encoding\\|mark\\)\\.[^.]+\\'" p))
             collect (concat p " is null")
             when (and (string-match-p "\\.encoding\\.[^.]+\\'" p) (consp v)
                       (plist-member v :datum)
                       (or (plist-member v :scale) (plist-member v :legend)))
             collect (concat p " has a scale or legend on a datum")
             append (eas-vega-std-basic--schema-faults v p)))
   ((vectorp node)
    (cl-loop for e across node append (eas-vega-std-basic--schema-faults e (concat path "[]"))))))

(ert-deftest eas-vega-std-basic-templates-resolve-schema-valid ()
  "Each template takes a title slot (default empty) and resolves to
schema-valid Vega-Lite: no null channel or mark property, no scale or
legend on a datum channel."
  (dolist (name eas-vega-std-basic-names)
    (let* ((tpl (eas-template-get (eas-vega-std-basic--name name)))
           (title (plist-get (plist-get (plist-get tpl :meta) :slots) :title)))
      (should (equal (cons name (plist-get title :default)) (cons name "")))
      (should (equal (cons name (eas-vega-std-basic--schema-faults
                                 (eas-vega-std-basic--spec name) ""))
                     (list name))))))

;;; They draw natively

(ert-deftest eas-vega-std-basic-examples-draw-natively ()
  "Every example resolves to native Vega-Lite and draws as SVG and as text."
  (dolist (name (seq-difference eas-vega-std-basic-names eas-vega-std-basic-slow))
    (let ((spec (eas-vega-std-basic--spec name)))
      (should (equal (cons name (eas-vega-std-basic--blocking spec)) (list name)))
      (should (string-prefix-p "<svg" (eas-svg-render (eas-compile spec))))
      (should (> (length (string-trim (eas-text-render (eas-compile spec :target 'text
                                                                     :size '(:cols 80 :rows 24)))))
                 0)))))

(ert-deftest eas-vega-std-basic-stacks-follow-the-series-order ()
  "Stacked bars put the first series at the bottom, as Vega's stack sort does."
  (let* ((scene (eas-compile (eas-vega-std-basic--spec "stacked-bar-chart")))
         (bars (eas-vega-std-basic--items scene "bar"))
         (tip (lambda (b title) (plist-get (seq-find (lambda (f) (equal (plist-get f :title) title))
                                                     (plist-get b :tooltip))
                                           :value)))
         (first (seq-find (lambda (b) (equal (funcall tip b "c") "0")) bars))
         (second (seq-find (lambda (b) (and (equal (funcall tip b "c") "1")
                                            (equal (funcall tip b "x") (funcall tip first "x"))))
                           bars)))
    (should (and first second))
    ;; Lower on screen means a larger y.
    (should (> (plist-get first :y) (plist-get second :y)))))

(ert-deftest eas-vega-std-basic-radar-spokes-start-left-of-centre ()
  "The first key's spoke points at -PI + PI/n, as Vega's angular point scale."
  (let* ((scene (eas-compile (eas-vega-std-basic--spec "radar-chart")))
         (rules (eas-vega-std-basic--items scene "rule"))
         (first (car rules)))
    (should (= (length rules) 7))
    ;; key-0 lies left of and above the centre.
    (should (< (plist-get first :x2) (plist-get first :x1)))
    (should (< (plist-get first :y2) (plist-get first :y1)))))

;;; Engine pieces

(ert-deftest eas-vega-std-basic-dense-rank-starts-at-one ()
  (let ((rows (eas-transform-run [(:window [(:op "dense_rank" :as "r")] :sort [(:field "k")])]
                                 [(:k "b") (:k "a") (:k "b") (:k "c")])))
    (should (equal (seq-map (lambda (r) (plist-get r :r)) rows) '(2 1 2 3)))))

(ert-deftest eas-vega-std-basic-point-scale-keeps-its-padding ()
  "A point scale with padding 0 puts the first and last points on the edges."
  (let* ((scene (eas-compile '(:width 100 :height 50
                               :data (:values [(:x "a" :y 1) (:x "b" :y 2) (:x "c" :y 3)])
                               :mark "line"
                               :encoding (:x (:field "x" :type "ordinal" :scale (:type "point" :padding 0))
                                          :y (:field "y" :type "quantitative")))))
         (x (plist-get (plist-get (aref (plist-get scene :views) 0) :scales) :x)))
    (should (equal (plist-get x :padding-outer) 0))
    (should (= (eas-scale-apply x "a") (aref (plist-get x :range) 0)))
    (should (= (eas-scale-apply x "c") (aref (plist-get x :range) 1)))))

(ert-deftest eas-vega-std-basic-shared-scale-draws-each-layers-axis ()
  "resolve.axis independent over a shared scale draws a later layer's axis too."
  (let* ((spec '(:width 100 :height 80 :data (:values [(:a 1 :b 2) (:a 3 :b 4)])
                 :layer [(:mark "line" :encoding (:x (:field "a" :type "quantitative" :axis (:orient "top"))
                                                  :y (:field "b" :type "quantitative")))
                         (:mark "point" :encoding (:x (:field "a" :type "quantitative"
                                                       :axis (:orient "bottom" :title "below"))
                                                   :y (:field "b" :type "quantitative" :axis :null)))]
                 :resolve (:axis (:x "independent"))))
         (view (aref (plist-get (eas-compile spec) :views) 0))
         (axes (append (plist-get view :axes) nil))
         (xs (seq-filter (lambda (a) (string-prefix-p "x" (plist-get a :channel))) axes)))
    (should (= (length xs) 2))
    (should (equal (sort (mapcar (lambda (a) (plist-get a :orient)) xs) #'string<) '("bottom" "top")))
    (should (seq-find (lambda (a) (equal (plist-get a :title) "below")) xs))
    ;; Both read the same domain.
    (should (equal (plist-get (plist-get (plist-get view :scales) :x) :domain)
                   (plist-get (plist-get (plist-get view :scales) :x_1) :domain)))))

(ert-deftest eas-vega-std-basic-label-keeps-labels-apart ()
  "Labels take the first free anchor and never overlap a point or each other."
  (let* ((rows [(:x 10 :y 10 :t "first") (:x 10.5 :y 10 :t "second") (:x 50 :y 50 :t "far")
                (:x :null :y 3 :t "none")])
         (out (eas-transform-apply-domain
               (list :x-eas:transform "label" :x "x" :y "y" :text "t" :width 200 :height 200
                     :xDomain [0 100] :yDomain [0 100] :anchor ["top" "bottom" "right" "left"] :markSize 25)
               rows)))
    (should (equal (seq-map (lambda (r) (plist-get r :label_anchor)) out)
                   '("top" "bottom" "top" :null)))))

(ert-deftest eas-vega-std-basic-label-avoids-a-trend-line ()
  (let* ((rows (vconcat (cl-loop for i from 0 to 10 collect (list :x (* 10 i) :y (* 10 i) :t "ab"))))
         (out (eas-transform-apply-domain
               (list :x-eas:transform "label" :x "x" :y "y" :text "t" :width 100 :height 100
                     :anchor ["top"] :avoidRegression "linear")
               rows)))
    ;; Above each point runs the fitted diagonal: no top anchor fits.
    (should (seq-every-p (lambda (r) (eq (plist-get r :label_anchor) :null)) out))))

(ert-deftest eas-vega-std-basic-arc-label-separates-each-side ()
  (let* ((rows [(:id "a" :v 1) (:id "b" :v 1) (:id "c" :v 1) (:id "d" :v 20)])
         (out (eas-transform-apply-domain
               (list :x-eas:transform "arc-label" :field "v" :width 200 :outerRadius 100 :labelHeight 12)
               rows))
         (right (seq-filter (lambda (r) (equal (plist-get r :arc_side) "right")) out))
         (ys (sort (mapcar (lambda (r) (plist-get r :arc_y4)) right) #'<)))
    ;; The big wedge's middle lies past 6 o'clock: its label goes left.
    (should (= (length right) 3))
    (cl-loop for (a b) on ys while b do (should (>= (- b a) 11.999))))
  (let ((out (eas-transform-apply-domain
              (list :x-eas:transform "arc-label" :field "v" :width 200 :separate :false)
              [(:v 1) (:v 1)])))
    ;; Left in place: each label beside its own wedge.
    (seq-doseq (r out) (should (= (plist-get r :arc_y4) (plist-get r :arc_y2))))))

;;; Interactions, headless

(defmacro eas-vega-std-basic--with-view (var name &rest body)
  "Open example NAME as view VAR in a private view table and run BODY."
  (declare (indent 2))
  `(let* ((eas-views (make-hash-table :test 'equal))
          (,var (eas-view-open (eas-vega-std-basic--name ,name)
                               :bindings (eas-template-example (eas-vega-std-basic--name ,name)))))
     ,@body))

(defun eas-vega-std-basic--centre (item)
  "The pixel centre of rect ITEM, as [X Y]."
  (vector (+ (plist-get item :x) (/ (plist-get item :w) 2.0))
          (+ (plist-get item :y) (/ (plist-get item :h) 2.0))))

(ert-deftest eas-vega-std-basic-bar-hover-highlights-and-labels ()
  (eas-vega-std-basic--with-view v "bar-chart"
    (let* ((scene (eas-view-scene v))
           (bar (car (eas-vega-std-basic--items scene "bar"))))
      (should-not (eas-vega-std-basic--items scene "text"))
      (eas-dispatch v (list :type "pointermove" :px (eas-vega-std-basic--centre bar)))
      (let* ((scene (eas-view-scene v))
             (fills (mapcar (lambda (b) (plist-get b :fill)) (eas-vega-std-basic--items scene "bar")))
             (labels (eas-vega-std-basic--items scene "text")))
        (should (= (cl-count "red" fills :test #'equal) 1))
        (should (equal (mapcar (lambda (l) (plist-get l :text)) labels) '("28"))))
      (eas-dispatch v '(:type "pointerleave"))
      (should-not (eas-vega-std-basic--items (eas-view-scene v) "text")))))

(ert-deftest eas-vega-std-basic-pyramid-year-slider ()
  (eas-vega-std-basic--with-view v "population-pyramid"
    (let ((width (lambda () (apply #'+ (mapcar (lambda (b) (plist-get b :w))
                                               (eas-vega-std-basic--items (eas-view-scene v) "bar"))))))
      (let ((y2000 (funcall width)))
        (eas-dispatch v '(:type "param" :param "year" :value 1850))
        (should (< (funcall width) (* 0.5 y2000)))))))

(ert-deftest eas-vega-std-basic-job-voyager-search ()
  "The search box and the sex buttons filter the stacked areas."
  (let* ((rows (vconcat (cl-loop for job in '("Farmer" "Farm Laborer" "Teacher")
                                 append (cl-loop for sex in '("men" "women")
                                                 append (cl-loop for year in '(1850 1860 1870)
                                                                 collect (list :job job :sex sex :year year
                                                                               :perc 0.1))))))
         (eas-views (make-hash-table :test 'equal))
         (v (eas-view-open "job-voyager" :bindings (list :data rows)))
         (areas (lambda () (length (eas-vega-std-basic--items (eas-view-scene v) "area")))))
    (should (= (funcall areas) 6))
    (eas-dispatch v '(:type "param" :param "query" :value "farm"))
    (should (= (funcall areas) 4))
    (eas-dispatch v '(:type "param" :param "sex" :value "women"))
    (should (= (funcall areas) 2))))

;;; The image oracle (:gallery)

(defun eas-vega-std-basic--ink-offset (native ref bg)
  "The shift aligning NATIVE's ink box with REF's (top-left), as (DX . DY).
BG is the reference background."
  (let ((pn (eas-png--profiles native bg)) (pr (eas-png--profiles ref bg)))
    (cl-flet ((first (v) (or (cl-position-if #'cl-plusp v) 0)))
      (cons (- (first (car pr)) (first (car pn))) (- (first (cdr pr)) (first (cdr pn)))))))

(defun eas-vega-std-basic--compare (native-file ref-file)
  "Compare NATIVE-FILE with REF-FILE: `eas-png-compare', also tried from the
ink-box alignment, whose thin strokes the profile alignment can miss.
Return the `eas-png-compare' plist with the better :ratio and :offset."
  (let* ((native (eas-png-read native-file)) (ref (eas-png-read ref-file))
         (base (eas-png-compare native ref))
         (bg (eas-png--background ref))
         (limit (* 35215 eas-png-pixel-threshold eas-png-pixel-threshold))
         (score (lambda (d) (let ((c (eas-png--count ref native (car d) (cdr d) bg limit)))
                              (/ (float (car c)) (max 1 (cdr c))))))
         (at (eas-vega-std-basic--ink-offset native ref bg))
         (s (funcall score at)) (improved t))
    (while improved
      (setq improved nil)
      (dolist (d '((-1 . 0) (1 . 0) (0 . -1) (0 . 1)))
        (let* ((c (cons (+ (car at) (car d)) (+ (cdr at) (cdr d)))) (cs (funcall score c)))
          (when (< cs s) (setq at c s cs improved t)))))
    (if (< s (plist-get base :ratio))
        (plist-put (plist-put (copy-sequence base) :ratio s) :offset (vector (car at) (cdr at)))
      base)))

(ert-deftest eas-vega-std-basic-gallery-holds-its-verdicts ()
  "Each example, rasterized, stays within its threshold of the reference image."
  :tags '(:gallery)
  (unless (and (fboundp 'zlib-available-p) (zlib-available-p)) (eas-test-skip "this Emacs lacks zlib, needed to decode PNGs"))
  (unless (executable-find eas-chart-rsvg-program)
    (eas-test-skip (format "%s is not on PATH; needed to rasterize native SVG" eas-chart-rsvg-program)))
  (let (failures)
    (dolist (name eas-vega-std-basic-names)
      (let* ((meta (eas-vega-std-basic--meta name))
             (threshold (plist-get meta :threshold))
             (png (make-temp-file "eas-vega" nil ".png")))
        (unwind-protect
            (when threshold
              (eas-chart-rasterize (eas-svg-render (eas-compile (eas-vega-std-basic--spec name))) png)
              (let ((cmp (eas-vega-std-basic--compare
                          png (eas-test-file "test/vega-examples/ref" (concat name ".png")))))
                (message "%s: ratio %.4f (threshold %s, status %s)" name (plist-get cmp :ratio)
                         threshold (plist-get meta :status))
                (when (> (plist-get cmp :ratio) threshold)
                  (push (format "%s: ratio %.4f > %s" name (plist-get cmp :ratio) threshold) failures))))
          (delete-file png))))
    (should (equal (nreverse failures) nil))))

(provide 'eas-vega-std-basic-test)
;;; eas-vega-std-basic-test.el ends here

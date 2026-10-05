;;; eas-area-arc-test.el --- area and arc properties, honored or reported -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.42: the area-circular group's second pass.  d3's curves and
;; arc corners against d3-shape's own numbers, the mark, legend, title,
;; axis and scale properties the group's chart types now honor, the
;; ignored ones check reports, the caches behind the bench.json numbers,
;; and the customization specs.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-agent)
(require 'eas-vl-gallery)
(require 'eas-vl-gallery-custom)
(require 'eas-vl-gallery-bench)

(defun eas-area-arc-test--near (a b &optional eps)
  "Non-nil when numbers A and B are within EPS (default 1e-3)."
  (< (abs (- a b)) (or eps 1e-3)))

(defun eas-area-arc-test--items (scene &optional mark)
  "Items of SCENE's first view's MARK-th mark (default 0)."
  (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) (or mark 0)) :items))

;;; Curves: (MODE LENGTH FIRST MIDDLE LAST) from d3-shape, 12 samples per Bezier

(defconst eas-area-arc-test--d3-curves
  '(("basis" 39 (0.0 0.0) (17.6042 17.6042) (40.0 40.0))
    ("basis-open" 13 (10.8333 20.8333) (17.6042 17.6042) (25.0 15.0))
    ("bundle" 39 (0.0 0.0) (17.9635 17.9635) (40.0 40.0))
    ("cardinal" 37 (0.0 0.0) (17.2813 17.2813) (40.0 40.0))
    ("cardinal-open" 13 (10.0 30.0) (17.2813 17.2813) (25.0 5.0))
    ("catmull-rom" 37 (0.0 0.0) (17.2788 17.5497) (40.0 40.0))
    ("natural" 37 (0.0 0.0) (17.125 17.125) (40.0 40.0)))
  "d3.line().curve(...) through (0,0) (10,30) (25,5) (40,40); cardinal at tension 0.3.")

(ert-deftest eas-area-arc-curves-match-d3 ()
  (dolist (c eas-area-arc-test--d3-curves)
    (let* ((eas-curve-tension (and (string-prefix-p "cardinal" (car c)) 0.3))
           (pts (eas-curve-apply '((0 0) (10 30) (25 5) (40 40)) (car c))))
      (should (equal (cons (car c) (length pts)) (cons (car c) (nth 1 c))))
      (cl-loop for (got want) in (list (list (car pts) (nth 2 c))
                                       (list (nth (/ (length pts) 2) pts) (nth 3 c))
                                       (list (car (last pts)) (nth 4 c)))
               do (should (and (eas-area-arc-test--near (car got) (car want))
                               (eas-area-arc-test--near (cadr got) (cadr want))))))))

(ert-deftest eas-area-arc-interpolate-and-tension-reach-the-area ()
  (let* ((spec (lambda (mark) (list :data '(:values [(:x 0 :y 0) (:x 1 :y 3) (:x 2 :y 1) (:x 3 :y 4)])
                                    :mark mark :encoding '(:x (:field "x" :type "quantitative")
                                                           :y (:field "y" :type "quantitative")))))
         (points (lambda (mark) (plist-get (aref (eas-area-arc-test--items (eas-compile (funcall spec mark))) 0) :points))))
    (should (> (length (funcall points '(:type "area" :interpolate "basis"))) 4))
    (should-not (equal (funcall points '(:type "area" :interpolate "cardinal"))
                       (funcall points '(:type "area" :interpolate "cardinal" :tension 0.8))))
    ;; Tension only bends the curves that have one.
    (should (equal (funcall points '(:type "area")) (funcall points '(:type "area" :tension 0.8))))))

;;; Arcs: d3.arc() with padding and corners

(defconst eas-area-arc-test--d3-arcs
  '(((40 100 0 1.2 0 8) . "M0,-91.652A8,8,0,0,1,8.696,-99.621A100,100,0,0,1,89.7,-44.203A8,8,0,0,1,85.423,-33.211L44.112,-17.15A8,8,0,0,1,34.344,-20.505A40,40,0,0,0,6.667,-39.441A8,8,0,0,1,0,-47.329Z")
    ((0 100 0 4.5 0.03 10) . "M1.342,-89.433A10,10,0,0,1,12.601,-99.203A100,100,0,1,1,-94.318,33.229A10,10,0,0,1,-87.14,20.163L0,0Z")
    ((30 90 0 3.14159 0.1 20) . "M4.741,-65.482A20,20,0,0,1,31.81,-84.191A90,90,0,0,1,31.811,84.191A20,20,0,0,1,4.742,65.482L4.742,43.45A20,20,0,0,1,14.845,26.07A30,30,0,0,0,14.845,-26.07A20,20,0,0,1,4.741,-43.45Z"))
  "((INNER OUTER START END PAD CORNER) . d3's path).")

(defun eas-area-arc-test--numbers (path)
  "The numbers of PATH, in order."
  (let (out (start 0))
    (while (string-match "-?[0-9.]+" path start)
      (push (string-to-number (match-string 0 path)) out)
      (setq start (match-end 0)))
    (nreverse out)))

(ert-deftest eas-area-arc-arcs-match-d3 ()
  (pcase-dolist (`((,r0 ,r1 ,a0 ,a1 ,pad ,rc) . ,want) eas-area-arc-test--d3-arcs)
    (let ((got (eas-arc-path (list :cx 0 :cy 0 :innerRadius r0 :outerRadius r1 :startAngle a0 :endAngle a1
                                   :padAngle pad :cornerRadius rc))))
      ;; The same commands, the same numbers to the two decimals eas writes.
      (should (equal (replace-regexp-in-string "[-0-9.]+" "#" (replace-regexp-in-string "," " " got))
                     (replace-regexp-in-string "[-0-9.]+" "#" (replace-regexp-in-string "," " " want))))
      (should (cl-every (lambda (a b) (eas-area-arc-test--near a b 0.006))
                        (eas-area-arc-test--numbers got) (eas-area-arc-test--numbers want)))))
  ;; Unpadded, unrounded wedges keep their own path.
  (should (equal (eas-arc-path '(:cx 50 :cy 50 :innerRadius 0 :outerRadius 10 :startAngle 0 :endAngle 3.141592653589793))
                 "M50,40A10,10 0 0 1 50,60L50,50Z")))

;;; Mark properties

(defun eas-area-arc-test--svg (spec)
  "SVG of SPEC."
  (eas-svg-render (eas-compile spec :size '(300 . 200))))

(defconst eas-area-arc-test--area
  '(:data (:values [(:x 1 :y 2) (:x 2 :y 4) (:x 3 :y 3)])
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative"))))

(ert-deftest eas-area-arc-area-mark-properties ()
  (let* ((mark '(:type "area" :stroke "black" :strokeDash [4 2] :strokeDashOffset 1 :strokeMiterLimit 3
                 :strokeCap "square" :strokeJoin "bevel" :fillOpacity 0.4 :strokeOpacity 0.6 :blend "multiply"
                 :href "https://example.com"))
         (spec (append (list :mark mark) eas-area-arc-test--area))
         (item (aref (eas-area-arc-test--items (eas-compile spec)) 0))
         (svg (eas-area-arc-test--svg spec)))
    (should (equal (plist-get item :outline) 1))
    (should (equal (plist-get item :href) "https://example.com"))
    (dolist (attr '("stroke=\"black\"" "stroke-width=\"1\"" "stroke-dasharray=\"4,2\"" "stroke-dashoffset=\"1\""
                    "stroke-miterlimit=\"3\"" "stroke-linecap=\"square\"" "stroke-linejoin=\"bevel\""
                    "fill-opacity=\"0.4\"" "stroke-opacity=\"0.6\"" "mix-blend-mode:multiply"))
      (should (string-search attr svg)))
    ;; No stroke: no outline, as in Vega.
    (should-not (plist-get (aref (eas-area-arc-test--items (eas-compile (append '(:mark "area") eas-area-arc-test--area))) 0)
                           :outline))
    ;; filled false draws the outline in the mark's color.
    (let ((item (aref (eas-area-arc-test--items
                       (eas-compile (append '(:mark (:type "area" :filled :false :color "red")) eas-area-arc-test--area)))
                      0)))
      (should (equal (plist-get item :fill) "none"))
      (should (equal (plist-get item :stroke) "red"))
      (should (equal (plist-get item :outline) 1)))))

(defconst eas-area-arc-test--pie
  '(:data (:values [(:k "a" :v 1) (:k "b" :v 3)])
    :encoding (:theta (:field "v" :type "quantitative") :color (:field "k" :type "nominal"))))

(ert-deftest eas-area-arc-arc-mark-properties ()
  (let* ((spec (append '(:mark (:type "arc" :cornerRadius 6 :padAngle 0.05 :innerRadius 20 :stroke "white"
                                :strokeDash [3 1] :strokeJoin "round" :fillOpacity 0.5 :href "https://example.com"))
                       eas-area-arc-test--pie))
         (item (aref (eas-area-arc-test--items (eas-compile spec)) 0))
         (svg (eas-area-arc-test--svg spec)))
    (should (equal (plist-get item :cornerRadius) 6))
    (should (equal (plist-get item :href) "https://example.com"))
    (should (string-match-p "d=\"M[^\"]*A6,6" svg))
    (dolist (attr '("stroke-dasharray=\"3,1\"" "stroke-linejoin=\"round\"" "fill-opacity=\"0.5\""))
      (should (string-search attr svg))))
  ;; Mark theta and theta2 (no theta field) are the start and end angles; radius2 is the inner radius.
  (let ((item (aref (eas-area-arc-test--items
                     (eas-compile '(:data (:values [(:k 1)]) :mark (:type "arc" :theta 1 :theta2 2.5 :radius2 15)
                                    :encoding (:color (:field "k" :type "nominal")))))
                    0)))
    (should (eas-area-arc-test--near (plist-get item :startAngle) 1))
    (should (eas-area-arc-test--near (plist-get item :endAngle) 2.5))
    (should (= (plist-get item :innerRadius) 15))))

(ert-deftest eas-area-arc-polar-text-keeps-dx-dy ()
  (let* ((spec (lambda (dx) (append (list :layer (vector '(:mark "arc")
                                                         (list :mark (list :type "text" :radius 50 :dx dx :dy 3)
                                                               :encoding '(:text (:field "k" :type "nominal")))))
                                    eas-area-arc-test--pie)))
         (at (lambda (dx) (aref (eas-area-arc-test--items (eas-compile (funcall spec dx)) 1) 0))))
    (should (eas-area-arc-test--near (- (plist-get (funcall at 10) :x) (plist-get (funcall at 0) :x)) 10))))

;;; Scales

(ert-deftest eas-area-arc-theta-and-radius-scale-properties ()
  (let ((theta (lambda (scale)
                 (let* ((spec (list :mark "arc" :data '(:values [(:v 1) (:v 3)])
                                    :encoding (list :theta (list :field "v" :type "quantitative" :scale scale))))
                        (view (aref (plist-get (eas-compile spec) :views) 0)))
                   (plist-get (plist-get view :scales) :theta)))))
    (should (equal (plist-get (funcall theta '(:domainMax 8)) :domain) [0.0 8.0]))
    (should (equal (plist-get (funcall theta '(:rangeMax 3)) :range) [0 3]))
    (should (eas-area-arc-test--near (aref (plist-get (funcall theta '(:reverse t)) :range) 0) (* 2 float-pi)))))

(ert-deftest eas-area-arc-domain-min-max-win-over-zero ()
  (let ((domain (lambda (scale)
                  (let* ((spec (list :data '(:values [(:a 1 :b 2) (:a 2 :b 30)]) :mark "line"
                                     :encoding (list :x '(:field "a" :type "quantitative")
                                                     :y (list :field "b" :type "quantitative" :scale scale))))
                         (view (aref (plist-get (eas-compile spec) :views) 0)))
                    (plist-get (plist-get (plist-get view :scales) :y) :domain)))))
    (should (equal (funcall domain '(:domainMin 3)) [3.0 30.0]))
    (should (equal (funcall domain '(:domainMax 50)) [0.0 50.0]))
    (should (equal (funcall domain nil) [0.0 30.0]))))

;;; Axes

(ert-deftest eas-area-arc-axis-values-and-title-padding ()
  (let* ((spec (lambda (axis) (list :data '(:values [(:d "2020-01-01" :v 1) (:d "2020-12-01" :v 3)]) :mark "area"
                                    :encoding (list :x (list :field "d" :type "temporal" :axis axis)
                                                    :y '(:field "v" :type "quantitative")))))
         (x-axis (lambda (axis) (seq-find (lambda (a) (equal (plist-get a :orient) "bottom"))
                                          (plist-get (aref (plist-get (eas-compile (funcall spec axis)) :views) 0) :axes)))))
    (should (= (length (plist-get (funcall x-axis '(:values ["2020-03-01" "2020-09-01"])) :ticks)) 2))
    (should (= (- (plist-get (plist-get (funcall x-axis '(:titlePadding 30)) :title-mark) :y)
                  (plist-get (plist-get (funcall x-axis nil) :title-mark) :y))
               26))))

;;; Legends

(ert-deftest eas-area-arc-legend-properties ()
  (let* ((legend '(:labelColor "red" :titleColor "blue" :labelFontSize 15 :symbolType "square" :symbolSize 200
                   :symbolStrokeColor "black" :values ["b" "a"] :labelExpr "upper(datum.label)"))
         (spec (append '(:mark "arc") (list :data (plist-get eas-area-arc-test--pie :data)
                                            :encoding (list :theta '(:field "v" :type "quantitative")
                                                            :color (list :field "k" :type "nominal" :legend legend)))))
         (scene (eas-compile spec))
         (l (aref (plist-get (aref (plist-get scene :views) 0) :legends) 0))
         (svg (eas-svg-render scene)))
    (should (equal (mapcar (lambda (e) (plist-get e :label)) (plist-get l :entries)) '("B" "A")))
    (should (= (plist-get l :font-size) 15))
    (should (equal (plist-get l :symbol-type) "square"))
    (should (equal (plist-get (aref (plist-get l :entries) 0) :stroke) "black"))
    (should (string-search "fill=\"red\" text-anchor=\"start\">B<" svg))
    (should (string-search "fill=\"blue\"" svg))))

;;; Titles

(ert-deftest eas-area-arc-subtitles ()
  (let* ((spec (append '(:mark "area" :title (:text "T" :subtitle "S" :anchor "end" :subtitleColor "green" :dx 3))
                       eas-area-arc-test--area))
         (plain (append '(:mark "area" :title "T") eas-area-arc-test--area))
         (scene (eas-compile spec))
         (title (plist-get scene :title)) (sub (plist-get title :subtitle))
         (metrics (eas-layout-metrics 'svg nil eas-theme-default)))
    (should (equal (plist-get title :align) "right"))
    (should (equal (plist-get sub :text) "S"))
    (should (equal (plist-get sub :color) "green"))
    (should (= (plist-get sub :x) (plist-get title :x)))
    ;; 16px title bounds plus the default 3px subtitlePadding.
    (should (= (- (plist-get sub :y) (plist-get title :y)) 19))
    (should (> (eas-title-height spec metrics) (eas-title-height plain metrics)))
    (should (string-search ">S</text>" (eas-svg-render scene)))
    (should (string-match-p "^ *T\n *S$" (car (split-string (substring-no-properties
                                                              (eas-text-render (eas-compile spec :target 'text :size '(:cols 30 :rows 12))))
                                                             "\n[^ ]" t))))))

;;; check reports what is drawn without

(ert-deftest eas-area-arc-check-reports-ignored-properties ()
  (let* ((spec '(:data (:values [(:k "a" :v 1)]) :mark (:type "arc" :aria :false)
                 :title (:text "T" :font "serif")
                 :encoding (:theta (:field "v" :type "quantitative")
                            :color (:field "k" :type "nominal" :legend (:labelFont "serif" :orient "bottom" :columns 1)))
                 :config (:axis (:labelFont "serif") :locale (:number (:decimal ",")))))
         (findings (eas-spec-props-findings spec))
         (env (eas-agent "check" (eas-json-encode spec))))
    (should (equal (mapcar (lambda (f) (plist-get f :path)) findings)
                   '("/title/font" "/encoding/color/legend/orient")))
    (should (cl-every (lambda (f) (and (equal (plist-get f :code) "UNSUPPORTED_FEATURE") (plist-get f :property)))
                      findings))
    ;; Ignored properties warn; the chart stays native.
    (should (eq (plist-get env :ok) t))
    (should (eq (plist-get (plist-get env :data) :native) t))
    (should (= (length (plist-get (plist-get env :data) :warnings)) 2))
    (should-not (eas-spec-props-findings (eas-vl-gallery-spec "area-circular" "arc_pie_pyramid")))))

(ert-deftest eas-area-arc-agent-reads-spec-files-relative-data ()
  (let ((default-directory temporary-file-directory)
        (file (eas-test-file "test/vl-examples/area-circular/area.vl.json")))
    (should (eq (plist-get (eas-agent "check" file) :ok) t))))

;;; Caches behind bench.json

(ert-deftest eas-area-arc-caches-are-transparent ()
  (let ((eas-time-zone "America/Chicago"))
    (dolist (unit '("yearmonth" "month" "utcyearmonthdate" "quarter" "hours" "yearmonthdatehours"))
      (dolist (v '("2004-03-15T10:30:00" "Mar 1 2000" 1078000000000))
        (should (equal (eas-time-unit-floor unit v) (eas-time-unit--floor unit v))))))
  ;; Memoized ticks come back as fresh lists.
  (let ((a (eas-scale-time-ticks 0 (* 400 86400000) 5)))
    (setcar a 'x)
    (should (numberp (car (eas-scale-time-ticks 0 (* 400 86400000) 5)))))
  (should (equal (eas-scale-time-ticks 0 (* 400 86400000) 5) (eas-scale-time--ticks 0 (* 400 86400000) 5)))
  (let ((eas-expr--random-calls nil))
    (should (= (eas-expr-evaluate "random()" '(:_eas_row 3)) (eas-expr-evaluate "random()" '(:_eas_row 3))))
    (should-not (= (eas-expr-evaluate "random()" '(:_eas_row 3)) (eas-expr-evaluate "random() + random() - random()" '(:_eas_row 3))))))

(ert-deftest eas-area-arc-bench-json-covers-the-group ()
  (let* ((bench (eas-json-read-file (eas-vl-gallery-bench-file "area-circular")))
         (examples (plist-get bench :examples)))
    (should (equal (sort (mapcar #'eas-key-name (eas-plist-keys examples)) #'string<)
                   (eas-vl-gallery-names "area-circular")))
    (cl-loop for (_ v) on examples by #'cddr
             do (dolist (stage '(:compile-svg :render-svg :compile-text :render-text))
                  (should (numberp (plist-get (plist-get v :ms) stage)))
                  (should (numberp (plist-get (plist-get v :baseline_ms) stage))))))
  (let ((one (eas-vl-gallery-bench-example "area-circular" "arc_pie" 1)))
    (should (= (plist-get one :rows) 6))
    (should (numberp (plist-get (plist-get one :ms) :render-text)))))

;;; Customization specs

(ert-deftest eas-area-arc-customization-specs-hold ()
  (should (member "area-circular" (eas-vl-gallery-custom-groups)))
  (should (equal (eas-vl-gallery-custom-names "area-circular") '("custom_arc" "custom_area" "custom_radial")))
  (dolist (group (eas-vl-gallery-custom-groups))
    (dolist (name (eas-vl-gallery-custom-names group))
      (should (equal (cons name (eas-vl-gallery-custom-check group name)) (list name))))))

(provide 'eas-area-arc-test)
;;; eas-area-arc-test.el ends here

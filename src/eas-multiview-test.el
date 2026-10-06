;;; eas-multiview-test.el --- facets, repeat grids, flush concat, discretizing scales -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.45: the multiview gallery group's features, each on a small
;; spec: every facet form lowered to a grid of cells with shared scales,
;; Vega's trellis layout (labels, field titles, spacing between plots),
;; wrapped repeats, bounds "flush", quantize/quantile/threshold scales,
;; shared top/bottom legends, and what check reports for header
;; properties the layout does not draw.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-facet-grid)
(require 'eas-facet-layout)
(require 'eas-vl-gallery-custom)

(defconst eas-multiview-test--rows
  [(:a "a1" :b "b1" :g "x" :v 1) (:a "a1" :b "b2" :g "y" :v 4) (:a "a2" :b "b1" :g "x" :v 9)
   (:a "a2" :b "b2" :g "y" :v 2) (:a "a3" :b "b1" :g "y" :v 6) (:a "a3" :b "b2" :g "x" :v 3)]
  "Six rows over a 3x2 grid of levels.")

(defun eas-multiview-test--spec (&rest props)
  "A bar unit over the test rows with encoding PROPS added."
  (list :data (list :values eas-multiview-test--rows) :mark "bar"
        :encoding (append (list :x (list :field "v" :type "quantitative") :y (list :field "g" :type "nominal")) props)))

(defun eas-multiview-test--views (scene)
  "SCENE's views as a list."
  (append (plist-get scene :views) nil))

(defun eas-multiview-test--headers (scene)
  "Texts of SCENE's trellis header items."
  (cl-loop for v in (eas-multiview-test--views scene)
           append (cl-loop for m across (plist-get v :marks)
                           when (string-suffix-p "/facet-headers" (plist-get m :id))
                           append (mapcar (lambda (i) (plist-get i :text)) (plist-get m :items)))))

;;; Lowering

(ert-deftest eas-multiview-row-and-column-facet-is-a-grid ()
  (let* ((spec (eas-facet-grid-lower (eas-multiview-test--spec :row '(:field "a" :title "Factor A")
                                                               :column '(:field "b"))))
         (meta (plist-get (plist-get spec :x-eas) :facet)))
    (should (= (length (plist-get spec :vconcat)) 3))
    (should (= (length (plist-get (aref (plist-get spec :vconcat) 0) :hconcat)) 2))
    (should (equal (plist-get meta :row-labels) ["a1" "a2" "a3"]))
    (should (equal (plist-get meta :column-labels) ["b1" "b2"]))
    (should (equal (plist-get meta :row-title) "Factor A"))
    (should (equal (plist-get meta :column-title) "b"))
    ;; Each cell filters to its two levels, then runs its own transforms.
    (let ((cell (aref (plist-get (aref (plist-get spec :vconcat) 1) :hconcat) 1)))
      (should (equal (plist-get cell :transform)
                     [(:filter (:field "a" :equal "a2")) (:filter (:field "b" :equal "b2"))])))))

(ert-deftest eas-multiview-wrapped-facet-and-top-level-facet ()
  (let* ((wrapped (eas-facet-grid-lower (eas-multiview-test--spec :facet '(:field "a" :columns 2))))
         (meta (plist-get (plist-get wrapped :x-eas) :facet)))
    (should (equal (mapcar (lambda (r) (length (plist-get r :hconcat))) (plist-get wrapped :vconcat)) '(2 1)))
    (should (equal (plist-get meta :column-labels) ["a1" "a2" "a3"]))
    (should (eq (plist-get meta :wrap) t)))
  (let ((top (eas-facet-grid-lower (list :data (list :values eas-multiview-test--rows)
                                         :facet '(:field "b") :columns 1
                                         :spec '(:mark "point" :encoding (:x (:field "v" :type "quantitative")))))))
    (should (equal (mapcar (lambda (r) (length (plist-get r :hconcat))) (plist-get top :vconcat)) '(1 1)))))

(ert-deftest eas-multiview-facet-levels-follow-vega-lite ()
  (let ((rows [(:k "b" :x 1 :v 1) (:k :null :x 2 :v 2) (:k "a" :x 3 :v 9) (:k "c" :x :null :v 3)]))
    ;; Ascending, null first.
    (should (equal (eas-facet-grid--levels '(:field "k") rows) '(:null "a" "b" "c")))
    (should (equal (eas-facet-grid--levels '(:field "k" :sort "descending") rows) '("c" "b" "a" :null)))
    ;; By an aggregate of another field.
    (should (equal (eas-facet-grid--levels '(:field "k" :sort (:field "v" :op "max" :order "descending")) rows)
                   '("a" "c" :null "b")))
    ;; A non-path unit drops rows with an invalid continuous x first.
    (should (equal (eas-facet-grid--levels '(:field "k") rows
                                           '(:mark "point" :encoding (:x (:field "x" :type "quantitative"))))
                   '(:null "a" "b")))
    ;; A sort field the cells aggregate away leaves the data's order.
    (should (equal (eas-facet-grid--levels '(:field "k" :sort (:field "v"))
                                           rows '(:mark "bar" :encoding (:y (:field "w" :aggregate "mean"))))
                   '("b" :null "a" "c")))))

(ert-deftest eas-multiview-inner-cells-keep-only-grid-lines ()
  (let* ((spec (eas-facet-grid-lower (eas-multiview-test--spec :row '(:field "a") :column '(:field "b"))))
         (cell (lambda (i j) (aref (plist-get (aref (plist-get spec :vconcat) i) :hconcat) j))))
    ;; First column keeps y, last row keeps x; the rest are bare.
    (should-not (plist-get (plist-get (plist-get (funcall cell 2 0) :encoding) :x) :axis))
    (should-not (plist-get (plist-get (plist-get (funcall cell 2 0) :encoding) :y) :axis))
    (should (equal (plist-get (plist-get (plist-get (funcall cell 0 1) :encoding) :y) :axis)
                   '(:domain :false :ticks :false :labels :false :title :null)))))

(ert-deftest eas-multiview-bins-span-all-the-data ()
  (let* ((spec (eas-facet-grid-lower
                (list :data (list :values eas-multiview-test--rows) :mark "bar"
                      :encoding '(:x (:field "v" :bin t) :y (:aggregate "count") :row (:field "a")))))
         (x (plist-get (plist-get (aref (plist-get (aref (plist-get spec :vconcat) 0) :hconcat) 0) :encoding) :x)))
    (should (equal (plist-get x :bin) '(:extent [1 9])))))

;;; Compile and layout

(ert-deftest eas-multiview-cells-share-their-scales ()
  (let* ((scene (eas-compile (eas-multiview-test--spec :row '(:field "a"))))
         (domain (lambda (v) (plist-get (plist-get (plist-get v :scales) :x) :domain)))
         (domains (mapcar domain (eas-multiview-test--views scene))))
    (should (= (length domains) 3))
    (should (equal (delete-dups (copy-sequence domains)) (list (car domains))))
    ;; The domain of all the rows, as the unfaceted chart has it.
    (should (equal (car domains) (funcall domain (aref (plist-get (eas-compile (eas-multiview-test--spec)) :views) 0))))))

(ert-deftest eas-multiview-trellis-spacing-headers-and-titles ()
  (let* ((scene (eas-compile (append (eas-multiview-test--spec :row '(:field "a" :title "Factor A")
                                                               :column '(:field "b" :title "Factor B"))
                                     (list :spacing 5 :width 60))))
         (views (eas-multiview-test--views scene))
         (b (lambda (i) (plist-get (nth i views) :bounds))))
    ;; Plots, not their axes, are spacing apart; all cells the same size.
    (should (= (- (aref (funcall b 1) 0) (+ (aref (funcall b 0) 0) (aref (funcall b 0) 2))) 5))
    (should (= (- (aref (funcall b 2) 1) (+ (aref (funcall b 0) 1) (aref (funcall b 0) 3))) 5))
    (should (equal (eas-multiview-test--headers scene) '("a1" "a2" "a3" "Factor A" "b1" "b2" "Factor B")))
    ;; Labels sit labelPadding (10) beyond the plots.
    (let ((items (plist-get (seq-find (lambda (m) (string-suffix-p "/facet-headers" (plist-get m :id)))
                                      (plist-get (car views) :marks))
                            :items)))
      (should (= (plist-get (seq-find (lambda (i) (equal (plist-get i :text) "b1")) items) :y)
                 (- (aref (funcall b 0) 1) 10)))
      (should (eql (plist-get (seq-find (lambda (i) (equal (plist-get i :text) "Factor A")) items) :angle) -90)))
    (should-not (eas-vl-gallery-overlaps scene))))

(ert-deftest eas-multiview-facet-renders-in-a-terminal ()
  (let ((text (substring-no-properties
               (eas-text-render (eas-compile (eas-multiview-test--spec :row '(:field "a") :column '(:field "b"))
                                             :target 'text :size '(:cols 70 :rows 24))))))
    (should (string-match-p "a1 · b1" text))
    (should (string-match-p "a3 · b2" text))))

(ert-deftest eas-multiview-facet-cells-are-interactive ()
  (let* ((spec (append (eas-multiview-test--spec :column '(:field "b"))
                       (list :params [(:name "pick" :select (:type "point" :on "pointermove" :nearest t))])))
         (eas-views (make-hash-table :test 'equal))
         (v (eas-view-open spec :id "f"))
         (cell (aref (plist-get (eas-view-scene v) :views) 1))
         (b (plist-get cell :bounds))
         (inspect (eas-dispatch v (list :type "pointermove"
                                        :px (vector (+ (aref b 0) 2) (+ (aref b 1) (/ (aref b 3) 4.0)))))))
    (should (equal (plist-get (plist-get (plist-get inspect :hover) :row) :b) "b2"))
    (should (equal (plist-get (plist-get inspect :hover) :view) (plist-get cell :id)))
    ;; A replayed log lands in the same state.
    (let ((w (eas-view-open spec :id "g")))
      (eas-replay w (eas-view-log-entries v))
      (should (equal (plist-get (eas-inspect w) :hover) (plist-get (eas-inspect v) :hover))))))

(ert-deftest eas-multiview-export-keeps-the-facet ()
  (let ((out (eas-resolve-spec (eas-multiview-test--spec :row '(:field "a")))))
    (should (plist-get (plist-get out :encoding) :row))
    (should-not (plist-get out :vconcat))))

;;; Repeat, flush concat, titles

(ert-deftest eas-multiview-array-repeat-wraps-after-columns ()
  (let ((spec (eas-vl-lower '(:repeat ["v" "w" "x"] :columns 2
                              :spec (:mark "bar" :encoding (:x (:field (:repeat "repeat") :bin t)))))))
    (should (equal (mapcar (lambda (r) (length (plist-get r :hconcat))) (plist-get spec :vconcat)) '(2 1)))
    (should (eq (plist-get (plist-get spec :x-eas) :grid) t)))
  (should (= (length (plist-get (eas-vl-lower '(:repeat ["v" "w"] :spec (:mark "bar"))) :hconcat)) 2)))

(ert-deftest eas-multiview-flush-bounds-space-the-plots ()
  (let* ((scene (eas-compile (list :data (list :values eas-multiview-test--rows) :bounds "flush" :spacing 15
                                   :vconcat [(:mark "bar" :height 60 :encoding (:x (:field "v" :bin t) :y (:aggregate "count")))
                                             (:mark "point" :encoding (:x (:field "v" :type "quantitative")
                                                                       :y (:field "v" :type "quantitative")))])))
         (views (eas-multiview-test--views scene))
         (a (plist-get (car views) :bounds)) (b (plist-get (cadr views) :bounds)))
    (should (= (aref a 0) (aref b 0)))
    (should (= (aref b 1) (+ (aref a 1) (aref a 3) 15)))))

(ert-deftest eas-multiview-nested-concat-title-is-drawn ()
  (let* ((scene (eas-compile (list :data (list :values eas-multiview-test--rows)
                                   :vconcat [(:title "Top" :hconcat [(:mark "point" :encoding (:x (:field "v" :type "quantitative")))
                                                                     (:mark "point" :encoding (:x (:field "v" :type "quantitative")))])])))
         (marks (plist-get (aref (plist-get scene :views) 0) :marks)))
    (should (seq-some (lambda (m) (and (string-suffix-p "/title" (plist-get m :id))
                                       (equal (plist-get (aref (plist-get m :items) 0) :text) "Top")))
                      marks))))

;;; Customization specs (test/vl-examples/multiview/custom)

(ert-deftest eas-multiview-repeat-count-axes-take-the-count-title ()
  (let* ((scene (eas-compile '(:data (:values [(:p 1 :q 2) (:p 3 :q 4)]) :repeat ["p" "q"]
                               :spec (:mark "bar" :encoding (:x (:field (:repeat "repeat") :type "quantitative" :bin t)
                                                             :y (:aggregate "count" :type "quantitative")))
                               :config (:countTitle "Cars"))))
         (titles (cl-loop for v in (eas-multiview-test--views scene)
                          append (cl-loop for a across (plist-get v :axes)
                                          when (equal (plist-get a :channel) "y") collect (plist-get a :title)))))
    (should titles)
    (should (seq-every-p (lambda (tt) (equal tt "Cars")) titles))))

(ert-deftest eas-multiview-wrapped-cells-with-own-y-axes-clear-their-labels ()
  ;; Vega places a wrapped cell's header above the cell's content and the
  ;; x axes (column footers) under it: independent y axes' end labels hang
  ;; over the plot, so the header rises and the x axes drop below them.
  (let* ((spec (lambda (resolve)
                 (append (list :data (list :values eas-multiview-test--rows) :mark "point"
                               :width 60 :height 40
                               :encoding '(:facet (:field "a" :type "nominal" :columns 2)
                                           :x (:field "v" :type "quantitative")
                                           :y (:field "v" :type "quantitative")))
                         resolve)))
         (layout (lambda (resolve)
                   (let* ((views (eas-multiview-test--views (eas-compile (funcall spec resolve))))
                          (v (car views))
                          (header (cl-loop for m across (plist-get v :marks)
                                           when (string-suffix-p "/facet-headers" (plist-get m :id))
                                           return (aref (plist-get m :items) 0)))
                          (x-axis (cl-loop for v in views
                                           thereis (cl-loop for a across (plist-get v :axes)
                                                            when (equal (plist-get a :channel) "x") return a))))
                     (list (- (aref (plist-get v :bounds) 1) (plist-get header :y)) (or (plist-get x-axis :offset) 0)))))
         (shared (funcall layout nil))
         (own (funcall layout '(:resolve (:scale (:y "independent"))))))
    (should (= (cadr shared) 0))
    (should (> (car own) (+ (car shared) 3)))
    (should (> (cadr own) 3))))

;;; Map projections

(ert-deftest eas-multiview-albers-usa-matches-d3 ()
  ;; d3.geoAlbersUsa().scale(1).translate([0, 0]) of the same points.
  (let ((f (eas-geo-projection '(:type "albersUsa"))))
    (should (equal (mapcar (lambda (c) (let ((p (apply f c))) (and p (list (/ (round (* 1e6 (car p))) 1e6)
                                                                          (/ (round (* 1e6 (cdr p))) 1e6)))))
                           '((-72.637078 40.922326) (-149.9 61.2) (-157.8 21.3) (-66.1 18.4)))
                   '((0.310564 -0.076482) (-0.288625 0.184171) (-0.168692 0.187961) nil))))
  ;; A projected unit draws on fixed x and y, fitted to its size, and check is clean.
  (let* ((spec '(:width 200 :height 100 :data (:values [(:lo -100 :la 40) (:lo -80 :la 30) (:lo -120 :la 45)])
                 :projection (:type "albersUsa") :mark "circle"
                 :encoding (:longitude (:field "lo" :type "quantitative") :latitude (:field "la" :type "quantitative"))))
         (items (plist-get (aref (plist-get (aref (plist-get (eas-compile spec) :views) 0) :marks) 0) :items)))
    (should-not (eas-spec-check spec))
    (should (= (length items) 3))
    ;; Fitted: it fills one side of the 200x100 view and stays inside the other.
    (let ((span (lambda (k) (- (apply #'max (mapcar (lambda (i) (plist-get i k)) items))
                               (apply #'min (mapcar (lambda (i) (plist-get i k)) items))))))
      (should (or (< (abs (- (funcall span :x) 200)) 1e-6) (< (abs (- (funcall span :y) 100)) 1e-6)))
      (should (<= (funcall span :x) (+ 200 1e-6)))
      (should (<= (funcall span :y) (+ 100 1e-6))))))

;;; Discretizing scales and legends

(ert-deftest eas-multiview-discretizing-scales ()
  (let ((q (eas-scale-discretize-make '(:field "b" :scale (:type "quantize" :zero t)) '(19 28 91) :color nil)))
    (should (equal (plist-get q :thresholds) [18.2 36.4 54.6 72.8]))
    (should (= (length (plist-get q :range)) 5))
    (should (equal (eas-scale-apply q 40) (aref (plist-get q :range) 2))))
  (let ((q (eas-scale-discretize-make '(:field "b" :scale (:type "quantile" :range [1 2 3 4 5]))
                                      '(19 28 43 52 53 55 81 87 91) :size nil)))
    (should (equal (mapcar #'round (append (plist-get q :thresholds) nil)) '(37 52 55 83))))
  (let* ((th (eas-scale-discretize-make '(:field "b" :scale (:type "threshold" :domain [30 70])) '(1 99) :size nil "circle"))
         (entries (eas-scale-discretize-entries th nil nil (lambda (v) (eas-scale-apply th v)))))
    (should (equal (plist-get th :range) [4.0 182.5 361.0]))
    (should (equal (mapcar (lambda (e) (plist-get e :label)) entries) '("< 30" "30 – 70" "≥ 70")))
    (should (equal (mapcar (lambda (e) (plist-get e :size)) entries) '(4.0 182.5 361.0)))))

(ert-deftest eas-multiview-same-field-legends-merge ()
  (let* ((eas-spec-supported-function nil)
         (scene (eas-compile '(:data (:values [(:a "A" :b 28) (:a "B" :b 55) (:a "C" :b 91)]) :mark "circle"
                               :encoding (:y (:field "a" :type "nominal")
                                          :size (:field "b" :type "quantitative" :scale (:type "quantize"))
                                          :color (:field "b" :type "quantitative" :scale (:type "quantize"))))))
         (legends (plist-get (aref (plist-get scene :views) 0) :legends)))
    (should (= (length legends) 1))
    (should (equal (mapcar (lambda (e) (plist-get e :label)) (plist-get (aref legends 0) :entries))
                   '("< 44" "44 – 60" "60 – 75" "≥ 75")))))

(ert-deftest eas-multiview-shared-legend-at-the-bottom ()
  (let* ((scene (eas-compile (eas-multiview-test--spec
                              :row '(:field "a")
                              :color '(:field "g" :type "nominal" :legend (:orient "bottom" :titleOrient "left")))))
         (views (eas-multiview-test--views scene))
         (legend (aref (plist-get (car views) :legends) 0))
         (entries (append (plist-get legend :entries) nil))
         (last-plot (plist-get (car (last views)) :bounds)))
    ;; One row, below the grid, starting at the first plot's x.
    (should (= (length (delete-dups (mapcar (lambda (e) (plist-get e :sy)) entries))) 1))
    (should (> (plist-get legend :y) (+ (aref last-plot 1) (aref last-plot 3))))
    (should (= (plist-get legend :x) (aref (plist-get (car views) :bounds) 0)))
    (should (< (plist-get (car entries) :sx) (plist-get (cadr entries) :sx)))))

;;; Shared-code fixes the group needed

(ert-deftest eas-multiview-axis-and-scale-fixes ()
  (let* ((rows [(:c "x" :p 0.5) (:c "y" :p 0.2)])
         (axes (lambda (spec) (plist-get (aref (plist-get (eas-compile spec) :views) 0) :axes)))
         (x-title (lambda (spec) (plist-get (seq-find (lambda (a) (equal (plist-get a :channel) "x"))
                                                      (funcall axes spec))
                                            :title))))
    ;; A stacked bar keeps title: null.
    (should-not (funcall x-title (list :data (list :values rows) :mark "bar"
                                       :encoding '(:y (:field "c" :type "nominal")
                                                   :x (:field "p" :type "quantitative" :title :null)
                                                   :color (:field "c" :type "nominal")))))
    ;; config.countTitle and axis titleLimit.
    (should (equal (funcall x-title (list :data (list :values rows) :mark "bar" :config '(:countTitle "Count")
                                          :encoding '(:x (:aggregate "count") :y (:field "c" :type "nominal"))))
                   "Count"))
    (should (string-suffix-p "…" (funcall x-title (list :data (list :values rows) :mark "point"
                                                             :encoding '(:x (:field "p" :type "quantitative" :title "A long axis title"
                                                                             :axis (:titleLimit 40)))))))
    ;; sort descending reverses a continuous scale.
    (let ((x (plist-get (plist-get (aref (plist-get (eas-compile (list :data (list :values rows) :mark "bar"
                                                                       :encoding '(:y (:field "c" :type "nominal")
                                                                                   :x (:field "p" :type "quantitative"
                                                                                       :sort "descending"))))
                                                    :views) 0)
                                   :scales)
                        :x)))
      (should (eq (plist-get x :reverse) t)))))

(provide 'eas-multiview-test)
;;; eas-multiview-test.el ends here

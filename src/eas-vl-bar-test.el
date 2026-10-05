;;; eas-vl-bar-test.el --- bar gallery polish: layout, bars, axes, legends, check -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.38: what turned the bar group's partial examples into passes
;; and what makes its chart types fully customizable, each on a small
;; spec.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery)
(require 'eas-agent)

(defun eas-vl-bar-test--view (spec &rest args)
  (aref (plist-get (apply #'eas-compile spec args) :views) 0))

(defun eas-vl-bar-test--items (spec &optional k &rest args)
  (plist-get (aref (plist-get (apply #'eas-vl-bar-test--view spec args) :marks) (or k 0)) :items))

(defun eas-vl-bar-test--axis (view channel)
  (seq-find (lambda (a) (equal (plist-get a :channel) channel)) (plist-get view :axes)))

(defconst eas-vl-bar-test--ages
  (list :data (list :values (vconcat (mapcar (lambda (i) (list :age (* 5 i) :n (+ 10 i))) (number-sequence 0 18))))
        :mark "bar" :height '(:step 17)
        :encoding '(:y (:field "age" :type "nominal") :x (:field "n" :type "quantitative")))
  "Nineteen nominal labels: they collide when fitted to a small size.")

;;; Layout

(ert-deftest eas-vl-bar-fitted-nominal-labels-thin ()
  (let ((labels (lambda (scene) (mapcar (lambda (tk) (plist-get tk :label))
                                        (plist-get (eas-vl-bar-test--axis (aref (plist-get scene :views) 0) "y") :ticks)))))
    ;; At its own size every label shows, as Vega-Lite's labelOverlap false does.
    (should-not (member "" (funcall labels (eas-compile eas-vl-bar-test--ages))))
    ;; Fitted to a small window they thin by parity and no longer collide.
    (let ((fitted (eas-compile eas-vl-bar-test--ages :size '(320 . 200))))
      (should (member "" (funcall labels fitted)))
      (should-not (eas-vl-gallery-overlaps fitted)))
    ;; An explicit labelOverlap false is kept.
    (let* ((spec (copy-tree eas-vl-bar-test--ages))
           (spec (plist-put spec :encoding '(:y (:field "age" :type "nominal" :axis (:labelOverlap :false))
                                             :x (:field "n" :type "quantitative")))))
      (should-not (member "" (funcall labels (eas-compile spec :size '(320 . 200))))))))

(ert-deftest eas-vl-bar-container-width ()
  (let ((spec '(:data (:values [(:k "a" :v 1) (:k "b" :v 3)]) :width "container" :height 100 :mark "bar"
                :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))))
    (should (= (plist-get (plist-get (eas-compile spec) :size) :w) eas-container-width))
    (let ((eas-container-width 300))
      (should (= (plist-get (plist-get (eas-compile spec) :size) :w) 300)))
    ;; Fitted to a window, the window is the container.
    (should (= (plist-get (plist-get (eas-compile spec :size '(640 . 300)) :size) :w) 640))))

(ert-deftest eas-vl-bar-linear-padding-pads-before-nice ()
  "Vega pads a continuous domain after zero and before nice."
  (let* ((spec '(:data (:values [(:k "a" :v -33) (:k "b" :v 91)]) :mark "bar" :width 480
                 :encoding (:y (:field "k" :type "nominal")
                            :x (:field "v" :type "quantitative" :scale (:padding 20)))))
         (x (plist-get (plist-get (eas-vl-bar-test--view spec) :scales) :x)))
    (should (equal (plist-get x :domain) [-40.0 100.0]))))

;;; Bars

(ert-deftest eas-vl-bar-stacked-text-band-position ()
  (let* ((spec '(:data (:values [(:g "a" :c "p" :v 2) (:g "a" :c "q" :v 6)])
                 :encoding (:y (:field "g" :type "nominal"))
                 :layer [(:mark "bar" :encoding (:x (:field "v" :type "quantitative" :aggregate "sum")
                                                 :color (:field "c" :type "nominal")))
                         (:mark "text" :encoding (:x (:field "v" :type "quantitative" :aggregate "sum" :stack "zero"
                                                      :bandPosition 0.5)
                                                  :detail (:field "c") :text (:field "v" :type "quantitative")))]))
         (view (eas-vl-bar-test--view spec))
         (bars (plist-get (aref (plist-get view :marks) 0) :items))
         (texts (plist-get (aref (plist-get view :marks) 1) :items)))
    ;; Each label sits mid-segment of its bar.
    (seq-doseq (tx texts)
      (should (seq-some (lambda (b) (< (abs (- (plist-get tx :x) (+ (plist-get b :x) (/ (plist-get b :w) 2.0)))) 0.01))
                        bars)))))

(ert-deftest eas-vl-bar-datums-type-and-group ()
  "A repeat layer's datums: nominal color and xOffset, in layer order."
  (let* ((spec '(:data (:values [(:g "x" :a 1 :b 2) (:g "y" :a 3 :b 1)])
                 :repeat (:layer ["b" "a"])
                 :spec (:mark "bar" :encoding (:x (:field "g" :type "nominal")
                                              :y (:field (:repeat "layer") :type "quantitative")
                                              :color (:datum (:repeat "layer"))
                                              :xOffset (:datum (:repeat "layer"))))))
         (view (eas-vl-bar-test--view spec))
         (scales (plist-get view :scales))
         (b (aref (plist-get (aref (plist-get view :marks) 0) :items) 0))
         (a (aref (plist-get (aref (plist-get view :marks) 1) :items) 0)))
    (should (equal (plist-get (plist-get scales :color) :domain) ["b" "a"]))
    (should (equal (plist-get (plist-get scales :xOffset) :type) "band"))
    ;; Side by side in the band, layer "b" first, in different colors.
    (should (< (plist-get b :x) (plist-get a :x)))
    (should-not (equal (plist-get b :fill) (plist-get a :fill)))))

(ert-deftest eas-vl-bar-ranged-band-bars-and-offsets ()
  (let* ((spec '(:data (:values [(:x 1 :lo "-1" :hi "1")]) :height 100
                 :mark (:type "bar" :yOffset -3 :y2Offset 3 :xOffset 2 :x2Offset -2)
                 :encoding (:x (:field "x" :type "quantitative") :x2 (:datum 5)
                            :y (:field "hi") :y2 (:field "lo"))))
         (view (eas-vl-bar-test--view spec))
         (ys (plist-get (plist-get view :scales) :y))
         (item (aref (plist-get (aref (plist-get view :marks) 0) :items) 0))
         (centre (lambda (v) (+ (eas-scale-apply ys v) (/ (plist-get ys :bandwidth) 2.0)))))
    ;; y and y2 on a band scale: band centre to band centre, each end moved by its offset.
    (should (< (abs (- (plist-get item :y) (+ (min (funcall centre "1") (funcall centre "-1")) 3))) 0.01))
    (should (< (abs (- (+ (plist-get item :y) (plist-get item :h))
                       (- (max (funcall centre "1") (funcall centre "-1")) 3)))
               0.01))))

(ert-deftest eas-vl-bar-band-size-and-stroke ()
  (let* ((spec '(:data (:values [(:k "a" :v 1)]) :height 100
                 :mark (:type "bar" :height (:band 0.5) :yOffset 5 :strokeDash [4 2] :stroke "black")
                 :encoding (:y (:field "k" :type "nominal") :x (:field "v" :type "quantitative"))))
         (view (eas-vl-bar-test--view spec))
         (ys (plist-get (plist-get view :scales) :y))
         (item (aref (plist-get (aref (plist-get view :marks) 0) :items) 0)))
    (should (< (abs (- (plist-get item :h) (* 0.5 (plist-get ys :bandwidth)))) 0.01))
    (should (< (abs (- (plist-get item :y) (+ (eas-scale-apply ys "a") (* 0.25 (plist-get ys :bandwidth)) 5))) 0.01))
    (should (equal (plist-get item :strokeDash) [4 2]))
    (should (string-match-p "stroke-dasharray=\"4,2\"" (eas-svg-render (eas-compile spec))))))

(ert-deftest eas-vl-bar-band-padding-precedence ()
  "Nested offsets pad 0.2; else bandPaddingInner, else the mark's own config."
  (let* ((base '(:data (:values [(:k "a" :g "x" :v 1) (:k "b" :g "y" :v 2)]) :mark "bar"))
         (inner (lambda (enc config)
                  (let ((x (plist-get (plist-get (eas-vl-bar-test--view (append base (list :encoding enc :config config)))
                                                 :scales)
                                      :x)))
                    (/ (round (* 100 (- 1 (/ (plist-get x :bandwidth) (plist-get x :step))))) 100.0)))))
    (should (= (funcall inner '(:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative"))
                        '(:scale (:barBandPaddingInner 0.3)))
               0.3))
    (should (= (funcall inner '(:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative"))
                        '(:scale (:barBandPaddingInner 0.3 :bandPaddingInner 0.4)))
               0.4))
    (should (= (funcall inner '(:x (:field "k" :type "nominal") :xOffset (:field "g" :type "nominal")
                                :y (:field "v" :type "quantitative"))
                        '(:scale (:barBandPaddingInner 0.3)))
               0.2))))

;;; Axes

(ert-deftest eas-vl-bar-axis-title-position-and-band-position ()
  (let* ((spec '(:data (:values [(:k "a" :v 1) (:k "b" :v 2)]) :height (:step 40) :mark "bar"
                 :encoding (:y (:field "k" :type "nominal" :scale (:padding 0)
                                :axis (:bandPosition 0 :grid t :titleX 5 :titleY -5 :titleAngle 0 :titleAlign "left"))
                            :x (:field "v" :type "quantitative"))))
         (view (eas-vl-bar-test--view spec))
         (b (plist-get view :bounds))
         (y (eas-vl-bar-test--axis view "y"))
         (tm (plist-get y :title-mark))
         (ys (plist-get (plist-get view :scales) :y)))
    (should (equal (list (plist-get tm :x) (plist-get tm :y) (plist-get tm :angle) (plist-get tm :align))
                   (list (+ (aref b 0) 5 0.5) (+ (aref b 1) -5 0.5) 0 "left")))
    ;; Grid lines at the band starts; labels stay at the centres.
    (let ((tk (aref (plist-get y :ticks) 1)))
      (should (= (aref (plist-get tk :grid) 1) (+ 0.5 (eas-layout--round (- (eas-scale-apply ys "b") 0.5)))))
      (should (> (plist-get tk :ly) (aref (plist-get tk :grid) 1))))))

(ert-deftest eas-vl-bar-axis-zindex-draws-over-marks ()
  (let* ((spec '(:data (:values [(:k "a" :v 1)]) :mark "bar"
                 :encoding (:x (:field "k" :type "nominal" :axis (:zindex 1 :title "XAXIS"))
                            :y (:field "v" :type "quantitative" :axis (:title "YAXIS")))))
         (svg (eas-svg-render (eas-compile spec))))
    ;; The y axis draws before the marks' group, the x axis after it.
    (should (< (string-search "YAXIS" svg) (string-search "<g>" svg)))
    (should (> (string-search "XAXIS" svg) (string-search "</g>" svg)))))

(ert-deftest eas-vl-bar-angled-label-defaults ()
  "Vega-Lite's defaults for angled x labels."
  (should (equal (list (eas-axis-pos-x-align -30 nil) (eas-axis-pos-x-baseline -30 nil)) '("right" "top")))
  (should (equal (list (eas-axis-pos-x-align 30 nil) (eas-axis-pos-x-baseline 30 nil)) '("left" "top")))
  (should (equal (list (eas-axis-pos-x-align 270 nil) (eas-axis-pos-x-baseline 270 nil)) '("right" "middle")))
  (should (equal (list (eas-axis-pos-x-align 0 nil) (eas-axis-pos-x-baseline 0 t)) '(nil "bottom")))
  (should (equal (eas-axis-pos-x-align 30 t) "right")))

(ert-deftest eas-vl-bar-count-title-and-title-limit ()
  (let* ((spec '(:data (:values [(:k "a") (:k "b")]) :mark "bar"
                 :config (:countTitle "N" :axisX (:titleLimit 30))
                 :encoding (:x (:field "a rather long field name" :type "nominal") :y (:aggregate "count"))))
         (view (eas-vl-bar-test--view spec)))
    (should (equal (plist-get (eas-vl-bar-test--axis view "y") :title) "N"))
    (should (string-suffix-p "…" (plist-get (eas-vl-bar-test--axis view "x") :title)))))

;;; Legends

(defconst eas-vl-bar-test--legend
  '(:data (:values [(:k "a" :c "first" :v 1) (:k "b" :c "second" :v 2) (:k "c" :c "third" :v 3)]) :mark "bar"
    :width 200 :height 100)
  "A three-entry color legend's chart, encoding added per test.")

(defun eas-vl-bar-test--legend (legend &optional config)
  (let* ((spec (append eas-vl-bar-test--legend
                       (list :encoding (list :x '(:field "k" :type "nominal") :y '(:field "v" :type "quantitative")
                                             :color (append '(:field "c" :type "nominal") (when legend (list :legend legend))))
                             :config config)))
         (view (eas-vl-bar-test--view spec)))
    (list (plist-get view :bounds) (aref (plist-get view :legends) 0))))

(ert-deftest eas-vl-bar-legend-orients-and-rows ()
  (pcase-let ((`(,b ,l) (eas-vl-bar-test--legend '(:orient "top" :title :null))))
    ;; Above the plot, legend.offset clear of it, its entries in one row.
    (should (= (aref (plist-get l :box) 3) (- (aref b 1) 18)))
    (should (= (aref (plist-get l :box) 0) (aref b 0)))
    (let ((ys (delete-dups (mapcar (lambda (e) (plist-get e :sy)) (plist-get l :entries))))
          (xs (mapcar (lambda (e) (plist-get e :sx)) (plist-get l :entries))))
      (should (= (length ys) 1))
      (should (equal xs (sort (copy-sequence xs) #'<)))))
  (pcase-let ((`(,b ,l) (eas-vl-bar-test--legend '(:orient "bottom"))))
    (should (> (aref (plist-get l :box) 1) (+ (aref b 1) (aref b 3)))))
  (pcase-let ((`(,b ,l) (eas-vl-bar-test--legend '(:orient "left" :offset 10))))
    (should (< (aref (plist-get l :box) 2) (aref b 0))))
  ;; config.legend.orient is the default orient.
  (pcase-let ((`(,b ,l) (eas-vl-bar-test--legend nil '(:legend (:orient "top")))))
    (should (< (aref (plist-get l :box) 3) (aref b 1))))
  ;; Two columns: entries 1 and 2 share a row, entry 3 starts the next.
  (pcase-let ((`(,_ ,l) (eas-vl-bar-test--legend '(:orient "bottom" :columns 2))))
    (let ((e (plist-get l :entries)))
      (should (= (plist-get (aref e 0) :sy) (plist-get (aref e 1) :sy)))
      (should (< (plist-get (aref e 1) :sy) (plist-get (aref e 2) :sy)))
      (should (= (plist-get (aref e 0) :sx) (plist-get (aref e 2) :sx))))))

(ert-deftest eas-vl-bar-legend-values-and-format ()
  (pcase-let ((`(,_ ,l) (eas-vl-bar-test--legend '(:values ["third" "first"]))))
    (should (equal (mapcar (lambda (e) (plist-get e :label)) (plist-get l :entries)) '("third" "first"))))
  (let* ((spec '(:data (:values [(:x 1 :y 1 :s 10) (:x 2 :y 2 :s 40)]) :mark "point"
                 :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                            :size (:field "s" :type "quantitative" :legend (:values [10 25 40] :format ".1f")))))
         (l (aref (plist-get (eas-vl-bar-test--view spec) :legends) 0)))
    (should (equal (mapcar (lambda (e) (plist-get e :label)) (plist-get l :entries)) '("10.0" "25.0" "40.0")))))

;;; View background

(ert-deftest eas-vl-bar-view-fill-and-stroke-width ()
  (let ((svg (eas-svg-render (eas-compile '(:data (:values [(:k "a" :v 1)]) :mark "bar"
                                            :config (:view (:fill "#eeeeee" :strokeWidth 3))
                                            :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))))))
    (should (string-match-p "<rect x=\"[0-9.]+\" y=\"[0-9.]+\" width=\"[0-9.]+\" height=\"[0-9.]+\" fill=\"#eeeeee\" stroke-width=\"3\"" svg))))

;;; check

(ert-deftest eas-vl-bar-check-reports-unhonored-properties ()
  (let ((eas-spec-supported-function nil)
        (spec '(:data (:values [(:k "a" :v 1)]) :mark "bar"
                :title (:text "T" :subtitle "S")
                :encoding (:x (:field "k" :type "nominal" :axis (:labelFontStyle "italic" :labelAngle 0 :x-eas:note 1))
                           :y (:field "v" :type "quantitative")
                           :color (:field "k" :type "nominal" :legend (:symbolDash [2 2] :orient "top")))
                :config (:axisBand (:grid t) :view (:discreteWidth 30) :bar (:strokeMiterLimit 2)
                         :legend (:orient "bottom")))))
    (should (equal (mapcar (lambda (f) (list (plist-get f :code) (plist-get f :path))) (eas-spec-check spec))
                   '(("UNSUPPORTED_FEATURE" "/encoding/color/legend/symbolDash")
                     ("UNSUPPORTED_FEATURE" "/config/view/discreteWidth")
                     ("UNSUPPORTED_FEATURE" "/config/bar/strokeMiterLimit"))))
    ;; An undrawn property is a warning: the chart still draws natively.
    (should (eas-compile spec))
    ;; Honored properties alone check clean.
    (should-not (eas-spec-check '(:data (:values [(:k "a" :v 1)]) :mark (:type "bar" :height (:band 0.5) :yOffset 2)
                                  :encoding (:y (:field "k" :type "nominal"
                                                 :axis (:titleX 1 :titleY -2 :bandPosition 0 :zindex 1 :titleLimit 50))
                                             :x (:field "v" :type "quantitative")
                                             :color (:field "k" :type "nominal" :legend (:orient "top" :columns 2)))
                                  :config (:countTitle "N" :legend (:orient "left") :scale (:barBandPaddingInner 0.2)))))))

;;; Time and agent

(ert-deftest eas-vl-bar-zoned-time-caches-agree ()
  "Cached zone conversions give what decode-time and encode-time give, across a DST change."
  (let ((eas-time-zone "America/Chicago"))
    (dolist (s '(1331449200 1331452800 1331456400 1352008800 1352012400))
      (dotimes (_ 2)
        (let ((f (eas-time-fields (* 1000 s))) (d (decode-time s "America/Chicago")))
          (should (equal (list (plist-get f :year) (plist-get f :month) (plist-get f :day) (plist-get f :hours))
                         (list (decoded-time-year d) (decoded-time-month d) (decoded-time-day d) (decoded-time-hour d)))))))
    (dotimes (_ 2)
      (should (= (eas-time-ms 2012 3 11 3) (* 1000 (time-convert (encode-time (list 0 0 3 11 3 2012 nil -1 "America/Chicago")) 'integer)))))))

(ert-deftest eas-vl-bar-agent-reads-data-urls-beside-the-spec ()
  (let* ((default-directory temporary-file-directory)
         (env (eas-agent "check" (eas-test-file "test/vl-examples/bar/bar_aggregate.vl.json"))))
    (should (eq (plist-get env :ok) t))))

(provide 'eas-vl-bar-test)
;;; eas-vl-bar-test.el ends here

;;; eas-vl-layered-test.el --- fc-qx1.44: the layered group's shared fixes -*- lexical-binding: t; -*-

;;; Commentary:

;; Each engine change the layered gallery polish made, on a small spec:
;; Vega's multi-line text, undefined categories, layer title and nice
;; merges, labels without ticks, concat cell bounds, front axes, text
;; limit and style, rect corners, corner legend offsets, the title
;; anchor, tick formats, padded linear domains, bar band padding, the
;; local-zone date fast path, reference defects and unhonored-property
;; findings.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery)
(require 'eas-agent)

(defun eas-vl-layered-test--view (scene &optional i)
  "View I (default 0) of SCENE."
  (aref (plist-get scene :views) (or i 0)))

(defun eas-vl-layered-test--items (scene type)
  "Items of SCENE's first mark of TYPE, over every view."
  (cl-loop for v across (plist-get scene :views)
           thereis (cl-loop for m across (plist-get v :marks)
                            when (equal (plist-get m :mark) type) return (plist-get m :items))))

;;; Text

(ert-deftest eas-vl-layered-multi-line-text-runs-down-from-its-anchor ()
  "Vega draws a text mark's first line on the anchor whatever the baseline,
and bounds every line."
  (let* ((spec '(:data (:values [(:x 1)]) :mark (:type "text" :text ["one" "two"] :baseline "middle")
                 :encoding (:x (:field "x" :type "quantitative"))))
         (svg (eas-svg-render (eas-compile spec)))
         (item (aref (eas-vl-layered-test--items (eas-compile spec) "text") 0)))
    (should (string-match-p "<tspan x=\"[0-9.]+\" dy=\"0\">one</tspan><tspan x=\"[0-9.]+\" dy=\"13\">two</tspan>" svg))
    (let ((b (eas-marks-item-bounds "text" item (eas-layout-metrics 'svg nil nil))))
      (should (> (- (aref b 3) (aref b 1)) 20)))))

(ert-deftest eas-vl-layered-text-style-and-limit ()
  (let* ((spec '(:data (:values [(:x 1 :t "a long label here")])
                 :mark (:type "text" :fontStyle "italic" :font "Georgia" :limit 30)
                 :encoding (:x (:field "x" :type "quantitative") :text (:field "t"))))
         (item (aref (eas-vl-layered-test--items (eas-compile spec) "text") 0))
         (svg (eas-svg-render (eas-compile spec))))
    (should (string-suffix-p "…" (plist-get item :text)))
    (should (< (length (plist-get item :text)) 10))
    ;; Both attributes on the text element, in whichever order eas-svg writes them.
    (should (string-match-p "<text[^>]*font-style=\"italic\"[^>]*>a lo" svg))
    (should (string-match-p "<text[^>]*font-family=\"Georgia\"[^>]*>a lo" svg))))

;;; Scales

(ert-deftest eas-vl-layered-undefined-is-a-category ()
  "A row without the color field takes the scale's first color, as in Vega."
  (let* ((spec '(:data (:values [(:x 1 :y 1) (:x 2 :y 2 :c t)]) :mark "point"
                 :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                            :fill (:field "c" :scale (:range ["black" "white"]) :legend :null))))
         (items (eas-vl-layered-test--items (eas-compile spec) "point")))
    (should (equal (mapcar (lambda (i) (plist-get i :fill)) items) '("black" "white")))))

(ert-deftest eas-vl-layered-nice-merges-across-layers ()
  "A binned first layer leaves nice to the next layer, as Vega-Lite merges it."
  (let* ((spec '(:data (:values [(:a 1565 :b 1570 :v 3) (:a 1820 :b 1825 :v 5)])
                 :layer [(:mark "bar" :encoding (:x (:field "a" :type "quantitative" :bin "binned") :x2 (:field "b")
                                                 :y (:field "v" :type "quantitative")))
                         (:mark "line" :encoding (:x (:field "a" :type "quantitative") :y (:field "v" :type "quantitative")))]))
         (x (plist-get (plist-get (eas-vl-layered-test--view (eas-compile spec)) :scales) :x)))
    (should (equal (plist-get x :domain) [1560.0 1840.0]))))

(ert-deftest eas-vl-layered-padding-then-nice ()
  "Vega pads a linear domain in pixels before nicing it."
  (let* ((spec '(:height 200 :data (:values [(:t 1 :v 23.2) (:t 2 :v 33.7)]) :mark "point"
                 :encoding (:x (:field "t" :type "quantitative")
                            :y (:field "v" :type "quantitative" :scale (:zero :false :padding 20)))))
         (y (plist-get (plist-get (eas-vl-layered-test--view (eas-compile spec)) :scales) :y)))
    ;; [23.2 33.7] padded 20px of 200 is [21.9 35.0]; d3's nice (step 2) makes it [20 36].
    (should (equal (plist-get y :domain) [20.0 36.0]))))

(ert-deftest eas-vl-layered-bar-band-padding-config ()
  (let* ((spec '(:data (:values [(:k "a" :v 1) (:k "b" :v 2)]) :mark "bar"
                 :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative"))
                 :config (:scale (:barBandPaddingInner 0))))
         (x (plist-get (plist-get (eas-vl-layered-test--view (eas-compile spec)) :scales) :x)))
    ;; No inner padding: each band is a whole step.
    (should (= (plist-get x :bandwidth) (plist-get x :step)))))

(ert-deftest eas-vl-layered-tick-formats-are-d3s ()
  "Axis formats other than \",.Nf\" go through d3-format: symbols, no grouping."
  (let ((scale (eas-scale-continuous "linear" 0 2000 [0 100])))
    (should (equal (funcall (eas-scale-tick-format scale 5 "$.0f") 34) "$34"))
    (should (equal (funcall (eas-scale-tick-format scale 5 ".0f") 1500) "1500"))
    (should (equal (funcall (eas-scale-tick-format scale 5 ",.0f") 1500) "1,500"))))

;;; Axes, legends, titles, concat cells

(ert-deftest eas-vl-layered-config-axis-title-null-beats-layer-titles ()
  "config.axis.title null hides the title Vega-Lite joins from a layer's fields."
  (let* ((spec '(:data (:values [(:a 1 :b 2)])
                 :layer [(:mark "point" :encoding (:x (:field "a" :type "quantitative") :y (:field "a" :type "quantitative")))
                         (:mark "point" :encoding (:x (:field "b" :type "quantitative") :y (:field "b" :type "quantitative")))]
                 :config (:axis (:title :null))))
         (axes (plist-get (eas-vl-layered-test--view (eas-compile spec)) :axes)))
    (should (seq-every-p (lambda (a) (null (plist-get a :title))) axes))))

(ert-deftest eas-vl-layered-labels-without-ticks-sit-at-the-padding ()
  (let* ((spec (lambda (ticks)
                 `(:data (:values [(:k "a" :v 1)]) :mark "bar"
                   :encoding (:y (:field "k" :type "nominal" :axis (:ticks ,ticks :labelPadding 5))
                              :x (:field "v" :type "quantitative")))))
         (lx (lambda (ticks) (let* ((v (eas-vl-layered-test--view (eas-compile (funcall spec ticks))))
                                    (a (seq-find (lambda (a) (equal (plist-get a :orient) "left")) (plist-get v :axes))))
                               (- (aref (plist-get v :bounds) 0) (plist-get (aref (plist-get a :ticks) 0) :lx))))))
    (should (= (funcall lx :false) 4.5))
    (should (= (funcall lx t) 8.5))))

(ert-deftest eas-vl-layered-concat-cells-grow-by-a-stroked-frame ()
  "Vega bounds a stroked concat cell half its stroke all round, transparent too."
  (let* ((spec (lambda (stroke)
                 `(:data (:values [(:a 1)]) :config (:view (:stroke ,stroke))
                   :vconcat [(:mark "point" :height 20 :encoding (:x (:field "a" :type "quantitative" :axis :null)))
                             (:mark "point" :height 20 :encoding (:x (:field "a" :type "quantitative" :axis :null)))])))
         (gap (lambda (stroke) (let ((s (eas-compile (funcall spec stroke))))
                                 (- (aref (plist-get (eas-vl-layered-test--view s 1) :bounds) 1)
                                    (aref (plist-get (eas-vl-layered-test--view s 0) :bounds) 1))))))
    (should (= (- (funcall gap "transparent") (funcall gap :null)) 2))))

(ert-deftest eas-vl-layered-front-axis-draws-after-marks ()
  (let* ((spec (lambda (z) `(:data (:values [(:x 1 :y 1)]) :mark "point"
                              :encoding (:x (:field "x" :type "quantitative")
                                         :y (:field "y" :type "quantitative" :axis (:zindex ,z :title "WHY"))))))
         (order (lambda (z) (let ((svg (eas-svg-render (eas-compile (funcall spec z)))))
                              (< (string-search "WHY" svg) (string-search "<circle" svg))))))
    (should (funcall order 0))
    (should-not (funcall order 1))))

(ert-deftest eas-vl-layered-rect-corners ()
  (let* ((spec '(:data (:values [(:a "x" :b "y")]) :mark (:type "rect" :cornerRadius 3)
                 :encoding (:x (:field "a" :type "nominal") :y (:field "b" :type "nominal"))))
         (item (aref (eas-vl-layered-test--items (eas-compile spec) "rect") 0)))
    (should (equal (plist-get item :corners) [3 3 3 3]))))

(ert-deftest eas-vl-layered-corner-legend-takes-its-offset ()
  (let* ((spec (lambda (offset)
                 `(:data (:values [(:a 1 :b 1 :c "p") (:a 2 :b 2 :c "q")]) :mark "point"
                   :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")
                              :color (:field "c" :type "nominal" :legend (:orient "top-left" :offset ,offset))))))
         (x (lambda (offset) (let ((v (eas-vl-layered-test--view (eas-compile (funcall spec offset)))))
                               (- (aref (plist-get (aref (plist-get v :legends) 0) :box) 0) (aref (plist-get v :bounds) 0))))))
    (should (= (- (funcall x 20) (funcall x 4)) 16))))

(ert-deftest eas-vl-layered-title-object-anchor ()
  (let* ((spec '(:width 200 :title (:text "T" :anchor "end") :data (:values [(:a 1)]) :mark "point"
                 :encoding (:x (:field "a" :type "quantitative"))))
         (title (plist-get (eas-compile spec) :title)))
    (should (equal (plist-get title :align) "right"))))

;;; Time

(ert-deftest eas-vl-layered-local-zone-converts-the-same ()
  "The local-zone fast path gives the named zone's fields and instants and
restores TZ."
  (let ((eas-time-zone "America/Chicago") (old (getenv "TZ"))
        (instants '(0 1710054000000 1730613600000 -2208988800000)))
    (let ((slow (mapcar #'eas-time-fields instants))
          (slow-ms (eas-time-ms 2024 3 10 2 30)))
      (eas-time-with-local-zone
       (should (equal (mapcar #'eas-time-fields instants) slow))
       (should (equal (eas-time-ms 2024 3 10 2 30) slow-ms))
       (let ((eas-time-zone nil)) (should (equal (plist-get (eas-time-fields 0) :hours) 0)))))
    (should (equal (getenv "TZ") old))))

;;; Reference defects

(ert-deftest eas-vl-layered-ref-defect ()
  (let ((spec '(:layer [(:mark "bar") (:mark (:type "line"))])))
    (should (equal (eas-vl-gallery-defect-apply spec '(:why "w" :set [(:path "/layer/0/mark/opacity" :value 0)]))
                   '(:layer [(:mark (:type "bar" :opacity 0)) (:mark (:type "line"))])))
    (eas-test-should-code "INVALID_INPUT"
      (eas-vl-gallery-defect-apply spec '(:set [(:path "/layer/0/mark/opacity" :value 0)])))
    (should (eas-vl-gallery-defect (eas-vl-gallery-status "layered") "wheat_wages"))))

;;; Unhonored properties

(ert-deftest eas-vl-layered-cli-spec-file-reads-data-beside-it ()
  "A spec file's relative data.url resolves against the file's directory."
  (let ((default-directory temporary-file-directory)
        (file (eas-test-file "test/vl-examples/layered/wheat_wages.vl.json")))
    (should (eq (plist-get (eas-agent "check" file) :ok) t))))

(provide 'eas-vl-layered-test)
;;; eas-vl-layered-test.el ends here

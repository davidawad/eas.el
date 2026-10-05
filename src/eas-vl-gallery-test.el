;;; eas-vl-gallery-test.el --- official Vega-Lite examples and area features -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.29: the area-circular group of test/vl-examples renders
;; natively for both backends, holds its status.json verdicts (image
;; comparison wherever a rasterizer exists), lays out at three sizes
;; without overlap, and its text renderings are goldens.  Below, the
;; features the area examples needed, each on a small spec.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery)

(defun eas-vl-gallery-test--groups ()
  "Gallery groups to check: EAS_GALLERY_GROUPS (space-separated), or all.
`make test-gallery' runs one Emacs per group this way."
  (let ((only (split-string (or (getenv "EAS_GALLERY_GROUPS") ""))))
    (seq-filter (lambda (g) (or (null only) (member g only))) (eas-vl-gallery-groups))))

(ert-deftest eas-vl-gallery-groups-hold-their-status ()
  "Every example of every group has a verdict and holds it: renders,
threshold, no overlap."
  :tags '(:gallery)
  (should (eas-vl-gallery-test--groups))
  (dolist (group (eas-vl-gallery-test--groups))
    (let ((names (eas-vl-gallery-names group))
          (status (eas-vl-gallery-status group)))
      (should (equal (cons group (sort (mapcar #'eas-key-name (eas-plist-keys status)) #'string<))
                     (cons group names)))
      (dolist (name names)
        (should (equal (cons (concat group "/" name) (eas-vl-gallery-check group name))
                       (list (concat group "/" name))))))))

(ert-deftest eas-vl-gallery-inlines-url-data ()
  (let* ((spec (eas-vl-gallery-spec "area-circular" "area_overlay"))
         (rows (plist-get (plist-get spec :data) :values)))
    (should (vectorp rows))
    (should (equal (aref rows 0) '(:symbol "MSFT" :date "Jan 1 2000" :price 39.81)))))

(ert-deftest eas-vl-gallery-overlaps-are-found ()
  (let ((scene '(:size (:w 100 :h 100)
                 :views [(:id "a" :bounds [0 0 60 60] :legends [(:box [50 10 80 50])])
                         (:id "b" :bounds [40 40 80 20]
                          :legends [(:x 0 :y 70 :width 20 :entries [(:bounds [0 70 20 14])])])])))
    (should (equal (eas-vl-gallery-overlaps scene)
                   '("views a and b overlap" "a legend overlaps view a" "view b leaves the 100x100 canvas"
                     "a legend overlaps view b")))
    (should (equal (eas-vl-gallery--legend-box '(:x 0 :y 70 :width 20 :entries [(:bounds [0 70 20 14])]))
                   [0 70 20 14]))))

;;; Data and time

(ert-deftest eas-vl-gallery-primitive-values-become-data-rows ()
  (let ((scene (eas-compile '(:data (:values [3 5]) :mark "bar"
                              :encoding (:x (:field "data" :type "ordinal") :y (:field "data" :type "quantitative"))))))
    (should (= (length (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :items)) 2))))

(ert-deftest eas-vl-gallery-parses-month-name-dates ()
  (let ((eas-time-zone "America/Chicago"))
    (should (equal (eas-time-fields (eas-time-parse "Mar 1 2000"))
                   '(:year 2000 :month 3 :day 1 :hours 0 :minutes 0 :seconds 0 :milliseconds 0 :weekday 3))))
  (should (eas-time-string-p "Dec 15 2004"))
  (should-not (eas-time-string-p "Decimal 1")))

(ert-deftest eas-vl-gallery-time-unit-titles-read-coarse-to-fine ()
  (should (equal (eas-encode-title '(:field "yearmonth_date" :derived "timeUnit" :source "date"))
                 "date (year-month)")))

;;; Color schemes, gradients, axes

(ert-deftest eas-vl-gallery-named-schemes ()
  (should (equal (seq-take (eas-scheme-colors "category20b") 2) ["#393b79" "#5254a3"]))
  (should (= (length (eas-scheme-colors '(:name "tableau20"))) 20))
  (should-not (eas-scheme-colors "no-such-scheme"))
  (let* ((scene (eas-compile '(:data (:values [(:k "a" :v 1) (:k "b" :v 2)]) :mark "bar"
                               :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                          :color (:field "k" :type "nominal" :scale (:scheme "set1"))))))
         (items (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :items)))
    (should (equal (mapcar (lambda (i) (plist-get i :fill)) items) '("#e41a1c" "#377eb8")))))

(defconst eas-vl-gallery-test--gradient
  '(:data (:values [(:x 1 :y 2) (:x 2 :y 4)]) :mark (:type "area" :color (:gradient "linear" :x1 1 :y1 1 :x2 1 :y2 0
                                                                       :stops [(:offset 0 :color "white")
                                                                               (:offset 1 :color "darkgreen")]))
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative"))))

(ert-deftest eas-vl-gallery-gradient-fills ()
  (let* ((scene (eas-compile eas-vl-gallery-test--gradient))
         (item (aref (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :items) 0))
         (svg (eas-svg-render scene)))
    (should (equal (plist-get item :fill) "darkgreen"))
    (should (equal (plist-get (plist-get item :gradient) :gradient) "linear"))
    (should (string-match-p "<linearGradient id=\"paint-[0-9a-f]+\" x1=\"1\" y1=\"1\" x2=\"1\" y2=\"0\">" svg))
    (should (string-match-p "fill=\"url(#paint-" svg))
    (should (eas-text-render (eas-compile eas-vl-gallery-test--gradient :target 'text :size '(:cols 30 :rows 8))))))

(ert-deftest eas-vl-gallery-axis-domain-and-tick-size ()
  (let* ((spec '(:data (:values [(:x 1 :y 2) (:x 2 :y 4)]) :mark "line"
                 :encoding (:x (:field "x" :type "quantitative" :axis (:domain :false :tickSize 0))
                            :y (:field "y" :type "quantitative"))))
         (axes (plist-get (aref (plist-get (eas-compile spec) :views) 0) :axes))
         (x (seq-find (lambda (a) (equal (plist-get a :orient) "bottom")) axes))
         (y (seq-find (lambda (a) (equal (plist-get a :orient) "left")) axes)))
    (should-not (plist-get x :domain-line))
    (should (plist-get y :domain-line))
    (should (equal (plist-get x :tickSize) 0))
    ;; Labels sit right under the plot when ticks have no length.
    (let ((tk (aref (plist-get x :ticks) 0)) (b (plist-get (aref (plist-get (eas-compile spec) :views) 0) :bounds)))
      (should (= (plist-get tk :ly) (+ (aref b 1) (aref b 3) 4 0.5))))))

;;; Legends fitted to a size

(ert-deftest eas-vl-gallery-legends-fit-the-height-they-get ()
  (let* ((spec (list :data (list :values (vconcat (mapcar (lambda (i) (list :k (format "k%02d" i) :v i))
                                                          (number-sequence 1 14))))
                     :mark "bar"
                     :encoding '(:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")
                                 :color (:field "k" :type "nominal"))))
         (legend (lambda (scene) (aref (plist-get (aref (plist-get scene :views) 0) :legends) 0))))
    ;; At its own size the canvas grows to hold all 14 entries.
    (should (= (length (plist-get (funcall legend (eas-compile spec)) :entries)) 14))
    (dolist (scene (list (eas-compile spec :size '(400 . 160))
                         (eas-compile spec :target 'text :size '(:cols 60 :rows 10))))
      (let* ((l (funcall legend scene)) (n (length (plist-get l :entries))))
        (should (< 0 n 14))
        (should (= (plist-get l :truncated) 14))
        (should (equal (plist-get l :title) (format "k, %d of 14" n)))
        (should-not (eas-vl-gallery-overlaps scene))))))

;;; Overlays

(ert-deftest eas-vl-gallery-overlays-expand-to-layers ()
  (let* ((enc '(:x (:field "d" :type "temporal") :y (:field "p" :type "quantitative")))
         (out (eas-overlay-expand (list :data '(:values []) :mark '(:type "area" :line t :point t) :encoding enc)))
         (layer (plist-get out :layer)))
    (should-not (plist-get out :mark))
    (should (equal (plist-get out :data) '(:values [])))
    (should (equal (mapcar (lambda (l) (plist-get (plist-get l :mark) :type)) layer) '("area" "line" "point")))
    (should (equal (plist-get (aref layer 0) :mark) '(:type "area" :opacity 0.7)))
    (should (equal (plist-get (aref layer 2) :mark) '(:type "point" :opacity 1 :filled t))))
  (let ((layer (plist-get (eas-overlay-expand '(:mark (:type "area" :opacity 0.4 :line (:color "red"))
                                                :encoding (:x (:field "x")))) :layer)))
    (should (equal (plist-get (aref layer 0) :mark) '(:type "area" :opacity 0.4)))
    (should (equal (plist-get (aref layer 1) :mark) '(:type "line" :color "red"))))
  (should (equal (plist-get (aref (plist-get (eas-overlay-expand '(:mark (:type "line" :point "transparent")))
                                             :layer) 1) :mark)
                 '(:type "point" :opacity 1 :filled t :opacity 0)))
  ;; Nothing to expand: the spec comes back equal.
  (should (equal (eas-overlay-expand '(:mark "area" :encoding (:x (:field "x"))))
                 '(:mark "area" :encoding (:x (:field "x"))))))

(ert-deftest eas-vl-gallery-stacked-overlays-follow-the-stack ()
  (let* ((enc '(:x (:field "x" :type "ordinal") :y (:field "y" :type "quantitative")
                :color (:field "c" :type "nominal")))
         (layer (plist-get (eas-overlay-expand (list :mark '(:type "area" :line t) :encoding enc)) :layer)))
    (should (equal (plist-get (plist-get (plist-get (aref layer 1) :encoding) :y) :stack) "zero"))
    (should-not (plist-get (plist-get (plist-get (aref layer 0) :encoding) :y) :stack))))

;;; Stacking and curves

(ert-deftest eas-vl-gallery-stack-center ()
  "Each stack starts at (max total - its total) / 2, as Vega's center offset."
  (let* ((spec '(:data (:values [(:x 1 :c "a" :y 1) (:x 1 :c "b" :y 1) (:x 2 :c "a" :y 3) (:x 2 :c "b" :y 5)])
                 :mark "area"
                 :encoding (:x (:field "x" :type "ordinal") :y (:field "y" :type "quantitative" :stack "center")
                            :color (:field "c" :type "nominal"))))
         (unit (car (plist-get (car (plist-get (eas-compile-plan spec) :groups)) :units)))
         (lows (make-hash-table)))
    (seq-doseq (r (plist-get unit :rows))
      (puthash (plist-get r :x) (min (gethash (plist-get r :x) lows 99) (plist-get r :y_start)) lows))
    (should (= (gethash 1 lows) 3.0))
    (should (= (gethash 2 lows) 0.0))))

(ert-deftest eas-vl-gallery-monotone-curve ()
  (let* ((pts '((0 0) (10 10) (20 10) (30 0)))
         (curve (eas-curve-monotone pts)))
    (should (equal (car curve) '(0 0)))
    (should (= (length curve) (1+ (* 3 eas-curve-samples))))
    ;; It passes through every data point and never overshoots them.
    (dolist (p pts) (should (seq-some (lambda (q) (and (< (abs (- (car q) (car p))) 1e-9)
                                                      (< (abs (- (cadr q) (cadr p))) 1e-9)))
                                      curve)))
    (should (seq-every-p (lambda (q) (<= -1e-9 (cadr q) (+ 10 1e-9))) curve))
    (should (equal (eas-curve-apply pts "linear") pts))))

;;; Layers: nested fields, independent scales, top axes, titles, datasets, facets

(defun eas-vl-gallery-test--view (spec &rest args)
  (aref (plist-get (apply #'eas-compile spec args) :views) 0))

(ert-deftest eas-vl-gallery-nested-fields-read-into-rows ()
  (let* ((spec '(:data (:values [(:k "a" :rec (:lo 1 :hi 5) :ranges [2 4 6])
                                 (:k "b" :rec (:lo 2 :hi 8) :ranges [3 5 9])])
                 :mark "bar"
                 :encoding (:x (:field "k" :type "nominal")
                            :y (:field "rec.lo" :type "quantitative") :y2 (:field "rec.hi"))))
         (items (plist-get (aref (plist-get (eas-vl-gallery-test--view spec) :marks) 0) :items)))
    (should (= (length items) 2))
    (should (> (plist-get (aref items 0) :h) 0)))
  (should (equal (eas-nested-path "a.b[1]['c']") '("a" "b" 1 "c")))
  (should (equal (eas-nested-path "a\\.b") nil)))

(ert-deftest eas-vl-gallery-top-and-right-axes ()
  (let* ((spec '(:data (:values [(:x 1 :y 2) (:x 2 :y 4)]) :mark "point"
                 :encoding (:x (:field "x" :type "quantitative" :axis (:orient "top"))
                            :y (:field "y" :type "quantitative" :axis (:orient "right")))))
         (axes (plist-get (eas-vl-gallery-test--view spec) :axes))
         (orients (mapcar (lambda (a) (plist-get a :orient)) axes)))
    (should (equal (sort (copy-sequence orients) #'string<) '("right" "top")))
    (let ((top (seq-find (lambda (a) (equal (plist-get a :orient) "top")) axes))
          (b (plist-get (eas-vl-gallery-test--view spec) :bounds)))
      (should (< (aref (plist-get top :domain-line) 1) (+ 1 (aref b 1)))))))

(ert-deftest eas-vl-gallery-independent-layers-draw-two-axes ()
  (let* ((spec '(:data (:values [(:t 1 :a 10 :b 0.1) (:t 2 :a 20 :b 0.5)])
                 :resolve (:scale (:y "independent"))
                 :encoding (:x (:field "t" :type "quantitative"))
                 :layer [(:mark "line" :encoding (:y (:field "a" :type "quantitative")))
                         (:mark "line" :encoding (:y (:field "b" :type "quantitative")))]))
         (view (eas-vl-gallery-test--view spec))
         (ys (seq-filter (lambda (a) (member (plist-get a :orient) '("left" "right"))) (plist-get view :axes))))
    (should (= (length ys) 2))
    (should (equal (sort (mapcar (lambda (a) (plist-get a :orient)) ys) #'string<) '("left" "right")))))

(ert-deftest eas-vl-gallery-title-lines-and-concat-cell-titles ()
  (let* ((scene (eas-compile '(:title (:text ["One" "Two"]) :data (:values [(:x 1)]) :mark "point"
                               :encoding (:x (:field "x" :type "quantitative")))))
         (title (plist-get scene :title)))
    (should (equal (plist-get title :lines) ["One" "Two"]))
    (should (= (plist-get title :lineHeight) 18)))
  (let* ((scene (eas-compile '(:vconcat [(:title "Cell" :data (:values [(:x 1)]) :mark "point"
                                          :encoding (:x (:field "x" :type "quantitative")))])))
         (marks (plist-get (aref (plist-get scene :views) 0) :marks)))
    (should (seq-some (lambda (m) (string-suffix-p "/title" (plist-get m :id))) marks))))

(ert-deftest eas-vl-gallery-datasets-and-facet-spec-are-lowered ()
  (let ((spec (eas-vl-lower '(:datasets (:d [(:g "a" :v 1) (:g "b" :v 2)]) :data (:name "d") :mark "bar"
                              :encoding (:x (:field "g" :type "nominal") :y (:field "v" :type "quantitative"))))))
    (should (equal (plist-get (plist-get spec :data) :values) [(:g "a" :v 1) (:g "b" :v 2)]))
    (should-not (plist-member spec :datasets)))
  (let ((spec (eas-facet-lower '(:data (:values [(:g "a" :v 1) (:g "b" :v 2)])
                                 :facet (:row (:field "g" :type "ordinal" :header (:labelAngle 0)))
                                 :resolve (:scale (:x "independent"))
                                 :spec (:mark "bar" :encoding (:x (:field "v" :type "quantitative")))))))
    ;; A grid of one-cell rows (eas-facet-grid.el); the header goes to the trellis.
    (should (= (length (plist-get spec :vconcat)) 2))
    (should (eql (plist-get (plist-get (plist-get (plist-get spec :x-eas) :facet) :row-header) :labelAngle) 0))))

;;; Bars: stack transform, discrete offsets, time-unit bars, SI axis labels, corners

(ert-deftest eas-vl-gallery-stack-transform ()
  (let* ((rows [(:g "a" :v 1) (:g "a" :v 3) (:g "b" :v 2)])
         (out (eas-transform-run [(:stack "v" :groupby ["g"] :as ["lo" "hi"] :offset "normalize")] rows))
         (ends (seq-map (lambda (r) (list (plist-get r :lo) (plist-get r :hi))) out)))
    (should (equal ends '((0.0 0.25) (0.25 1.0) (0.0 1.0))))))

(ert-deftest eas-vl-gallery-discrete-offsets-nest-bands ()
  (let* ((spec '(:data (:values [(:c "A" :g "x" :v 1) (:c "A" :g "y" :v 2) (:c "B" :g "x" :v 3) (:c "B" :g "y" :v 4)])
                 :mark "bar"
                 :encoding (:x (:field "c" :type "nominal") :xOffset (:field "g" :type "nominal")
                            :y (:field "v" :type "quantitative"))))
         (items (plist-get (aref (plist-get (eas-vl-gallery-test--view spec) :marks) 0) :items)))
    ;; Side by side inside the band, not stacked.
    (should (< (plist-get (aref items 0) :x) (plist-get (aref items 1) :x)))
    (should (< (abs (- (plist-get (aref items 0) :w) (plist-get (aref items 1) :w))) 1e-6))
    (should (> (plist-get (aref items 1) :h) (plist-get (aref items 0) :h)))))

(ert-deftest eas-vl-gallery-time-unit-bars-span-their-unit ()
  (let* ((eas-time-zone "UTC")
         (spec '(:data (:values [(:d "2020-01-15" :v 1) (:d "2020-02-15" :v 2) (:d "2020-03-15" :v 3)])
                 :mark "bar"
                 :encoding (:x (:field "d" :type "temporal" :timeUnit "month") :y (:field "v" :type "quantitative"))))
         (items (plist-get (aref (plist-get (eas-vl-gallery-test--view spec) :marks) 0) :items)))
    (should (> (plist-get (aref items 0) :w) 30))
    ;; Adjacent months touch, less the binSpacing pixel.
    (should (< (abs (- (+ (plist-get (aref items 0) :x) (plist-get (aref items 0) :w))
                       (plist-get (aref items 1) :x))) 3))))

(ert-deftest eas-vl-gallery-si-axis-format-uses-one-prefix ()
  (let* ((scale (eas-scale-continuous "linear" -12e6 12e6 [0 1]))
         (fmt (eas-scale-tick-format scale 6 "s")))
    (should (equal (mapcar fmt '(-12e6 0 4e6)) '("−12M" "0M" "4M")))))

(ert-deftest eas-vl-gallery-per-corner-radii ()
  (let* ((spec '(:data (:values [(:c "A" :v 1)]) :mark (:type "bar" :cornerRadiusTopLeft 3 :cornerRadius 1)
                 :encoding (:x (:field "c" :type "nominal") :y (:field "v" :type "quantitative"))))
         (item (aref (plist-get (aref (plist-get (eas-vl-gallery-test--view spec) :marks) 0) :items) 0)))
    (should (equal (plist-get item :corners) [3 1 1 1]))))

(provide 'eas-vl-gallery-test)
;;; eas-vl-gallery-test.el ends here

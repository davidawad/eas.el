;;; eas-projection-test.el --- projections and shared layout fixes (fc-qx1.40) -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.40, the scatter-table polish: map projections of
;; longitude/latitude points, positional scales shared across concat
;; views, secondary channels kept out of non-positional domains, layer
;; axis titles, ticks-off axis extents and config.concat.spacing.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-projection)

(defun eas-projection-test--near (a b &optional eps)
  "Non-nil when numbers A and B differ by less than EPS (1e-5)."
  (< (abs (- a b)) (or eps 1e-5)))

(ert-deftest eas-projection-equal-earth-matches-d3 ()
  "d3-geo's geoEqualEarthRaw: the equator's end and the pole."
  (let ((p (eas-projection-raw "equalEarth" 0 0)))
    (should (and (eas-projection-test--near (car p) 0) (eas-projection-test--near (cdr p) 0))))
  (should (eas-projection-test--near (car (eas-projection-raw "equalEarth" 180 0)) 2.706629 1e-5))
  (should (eas-projection-test--near (cdr (eas-projection-raw "equalEarth" 0 90)) 1.317363 1e-5))
  (should (eas-projection-test--near (cdr (eas-projection-raw "mercator" 0 45)) 0.881374 1e-5))
  (should (eas-projection-test--near (car (eas-projection-raw "equirectangular" 90 0)) (/ float-pi 2))))

(defconst eas-projection-test--spec
  '(:width 300 :height 300 :projection (:type "equalEarth")
    :data (:values [(:lon -10 :lat 0) (:lon 10 :lat 0) (:lon 0 :lat 5) (:lon 0 :lat -5)])
    :mark "circle"
    :encoding (:longitude (:field "lon" :type "quantitative") :latitude (:field "lat" :type "quantitative")))
  "Four points, twice as wide as tall under the projection.")

(ert-deftest eas-projection-fits-the-plot-keeping-the-aspect ()
  "One scale factor for both axes, the points centred: d3's fitSize."
  (let* ((view (aref (plist-get (let ((eas-spec-supported-function nil)) (eas-compile eas-projection-test--spec)) :views) 0))
         (b (plist-get view :bounds))
         (items (plist-get (aref (plist-get view :marks) 0) :items))
         (xs (mapcar (lambda (i) (plist-get i :x)) items)) (ys (mapcar (lambda (i) (plist-get i :y)) items))
         (w (- (apply #'max xs) (apply #'min xs))) (h (- (apply #'max ys) (apply #'min ys))))
    ;; The wide extent fills the plot's width, the tall one is in proportion.
    (should (eas-projection-test--near w (aref b 2) 0.01))
    (let* ((dx (- (car (eas-projection-raw "equalEarth" 10 0)) (car (eas-projection-raw "equalEarth" -10 0))))
           (dy (- (cdr (eas-projection-raw "equalEarth" 0 5)) (cdr (eas-projection-raw "equalEarth" 0 -5)))))
      (should (eas-projection-test--near (/ h w) (/ dy dx) 1e-3)))
    ;; North is up and the map is centred.
    (should (< (plist-get (aref items 2) :y) (plist-get (aref items 3) :y)))
    (should (eas-projection-test--near (/ (+ (apply #'max ys) (apply #'min ys)) 2.0) (+ (aref b 1) (/ (aref b 3) 2.0)) 0.01))
    ;; No axes on a map.
    (should (equal (plist-get view :axes) []))))

(ert-deftest eas-projection-is-native-and-exports-unchanged ()
  "A projected point chart checks clean and renders in both backends;
the resolved spec keeps its projection (export stays pure Vega-Lite)."
  (let ((eas-spec-supported-function nil))
    (should-not (eas-spec-check eas-projection-test--spec))
    (should (string-match-p "<circle" (eas-svg-render (eas-compile eas-projection-test--spec))))
    (should (string-match-p "●" (eas-text-render (eas-compile eas-projection-test--spec :target 'text
                                                                :size '(:cols 30 :rows 10)))))
    (should (plist-get (eas-resolve-spec eas-projection-test--spec) :projection))))

(ert-deftest eas-projection-other-types-stay-unsupported ()
  ;; albersUsa, albers and conicEqualArea are eas-geo.el's (fc-qx1.45),
  ;; the other d3 types eas-geoshape.el's (eas-7r1.6); identity is drawn by none.
  (let ((eas-spec-supported-function nil))
    (should (equal (plist-get (car (eas-spec-check (plist-put (copy-tree eas-projection-test--spec)
                                                               :projection '(:type "identity"))))
                              :code)
                   "UNSUPPORTED_FEATURE"))))

;;; Shared layout fixes

(ert-deftest eas-projection-test-secondary-channels-stay-positional ()
  "y2 values join the y domain, never color's or opacity's."
  (let* ((spec '(:data (:values [(:a 0 :b 10 :c "p" :o 3) (:a 5 :b 20 :c "q" :o 8)]) :mark "rect"
                 :encoding (:x (:field "a" :type "quantitative") :y (:field "a" :type "quantitative")
                            :y2 (:field "b") :color (:field "c" :type "nominal")
                            :opacity (:field "o" :type "quantitative"))))
         (scales (plist-get (aref (plist-get (eas-compile spec) :views) 0) :scales)))
    (should (equal (plist-get (plist-get scales :color) :domain) ["p" "q"]))
    (should (equal (plist-get (plist-get scales :opacity) :domain) [3.0 8.0]))))

(ert-deftest eas-projection-test-concat-shares-resolved-positions ()
  "resolve.scale.x shared: concatenated views take the union domain."
  (let* ((spec '(:data (:values [(:a 1 :b 2) (:a 10 :b 30)])
                 :vconcat [(:mark "point" :encoding (:x (:field "a" :type "quantitative" :aggregate "min")))
                           (:mark "point" :encoding (:x (:field "b" :type "quantitative")))]
                 :resolve (:scale (:x "shared"))))
         (views (plist-get (eas-compile spec) :views))
         (d (lambda (i) (plist-get (plist-get (plist-get (aref views i) :scales) :x) :domain))))
    (should (equal (funcall d 0) (funcall d 1)))
    (should (>= (aref (funcall d 0) 1) 30)))
  ;; Independent (the concat default) keeps each view's own.
  (let* ((spec '(:data (:values [(:a 1 :b 2) (:a 10 :b 30)])
                 :vconcat [(:mark "point" :encoding (:x (:field "a" :type "quantitative")))
                           (:mark "point" :encoding (:x (:field "b" :type "quantitative")))]))
         (views (plist-get (eas-compile spec) :views)))
    (should-not (equal (plist-get (plist-get (plist-get (aref views 0) :scales) :x) :domain)
                       (plist-get (plist-get (plist-get (aref views 1) :scales) :x) :domain)))))

(ert-deftest eas-projection-test-layer-axis-title-wins ()
  "A layer's axis.title titles the shared axis, as Vega-Lite merges it."
  (let* ((spec '(:data (:values [(:a 1 :b 2 :c 3)])
                 :layer [(:mark "rect" :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")
                                                  :y2 (:field "c")))
                         (:mark "text" :encoding (:x (:field "a" :type "quantitative")
                                                  :y (:field "b" :type "quantitative" :axis (:title "Named"))
                                                  :text (:field "a")))]))
         (axes (plist-get (aref (plist-get (eas-compile spec) :views) 0) :axes)))
    (should (equal (plist-get (seq-find (lambda (a) (equal (plist-get a :channel) "y")) axes) :title) "Named"))))

(ert-deftest eas-projection-test-ticks-off-and-concat-spacing ()
  "Ticks off leave no tick room before the title; config.concat.spacing
separates concatenated views."
  (let* ((base '(:data (:values [(:a 1 :b 2)]) :mark "point"
                 :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative"))))
         (x0 (lambda (spec) (aref (plist-get (aref (plist-get (eas-compile spec) :views) 0) :bounds) 0))))
    (should (= (- (funcall x0 base)
                  (funcall x0 (append base '(:config (:axis (:ticks :false :labels :false))))))
               ;; the labels and the 5px ticks are gone
               (- (funcall x0 base) (funcall x0 (append base '(:config (:axis (:labels :false)))))
                  -5))))
  (let* ((spec (lambda (sp) `(:data (:values [(:a 1)]) :config (:concat (:spacing ,sp))
                              :vconcat [(:mark "point" :encoding (:x (:field "a" :type "quantitative")))
                                        (:mark "point" :encoding (:x (:field "a" :type "quantitative")))])))
         (gap (lambda (sp) (let ((vs (plist-get (eas-compile (funcall spec sp)) :views)))
                             (- (aref (plist-get (aref vs 1) :bounds) 1) (aref (plist-get (aref vs 0) :bounds) 1))))))
    (should (= (- (funcall gap 40) (funcall gap 10)) 30))))

(provide 'eas-projection-test)
;;; eas-projection-test.el ends here

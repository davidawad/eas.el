;;; eas-vl-gallery-interactive-test.el --- shared fixes behind the interactive group -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.46: each fix that turned an interactive example from partial to
;; pass, on a small spec.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery)

(defun eas-vl-gallery-interactive-test--views (scene)
  "SCENE's views as a list."
  (append (plist-get scene :views) nil))

(ert-deftest eas-vl-gallery-interactive-argmax-compares-dates ()
  (let ((rows (plist-get (car (plist-get (car (plist-get (eas-compile-plan
                                                          '(:data (:values [(:s "a" :d "Jan 1 2000" :p 1) (:s "a" :d "Mar 1 2000" :p 3)
                                                                            (:s "a" :d "Feb 1 2000" :p 2)])
                                                            :mark "circle"
                                                            :encoding (:x (:aggregate "max" :field "d" :type "temporal")
                                                                       :y (:aggregate (:argmax "d") :field "p" :type "quantitative"))))
                                                         :groups))
                                         :units))
                         :rows)))
    (should (= (length rows) 1))
    (should (equal (plist-get (aref rows 0) :argmax_d_p) 3))))

(ert-deftest eas-vl-gallery-interactive-column-repeats-align-as-a-grid ()
  (let* ((spec (lambda (fields) (list :data '(:values [(:a 1 :b 1000000) (:a 2 :b 2)])
                                      :repeat (list :column fields)
                                      :spec '(:mark "point" :encoding (:x (:field (:repeat "column") :type "quantitative")
                                                                       :y (:field (:repeat "column") :type "quantitative"))))))
         (x0 (lambda (fields) (aref (plist-get (aref (plist-get (eas-compile (eas-vl-lower (funcall spec fields))) :views) 0)
                                               :bounds)
                                    0))))
    (should (plist-get (plist-get (eas-vl-lower (funcall spec ["a" "b"])) :x-eas) :grid))
    ;; Every cell leaves room for the widest y labels, those of column "b".
    (should (> (funcall x0 ["a" "b"]) (funcall x0 ["a"])))))

(ert-deftest eas-vl-gallery-interactive-sort-by-channel-keeps-ties-in-data-order ()
  (let* ((spec '(:data (:values [(:k "c" :v 1) (:k "a" :v 5) (:k "b" :v 1) (:k "d" :v 1)]) :mark "bar"
                 :encoding (:y (:field "k" :type "nominal" :sort "-x") :x (:field "v" :type "quantitative"))))
         (y (plist-get (plist-get (car (plist-get (eas-compile-plan spec) :groups)) :scales) :y)))
    (should (equal (plist-get y :domain) ["a" "c" "b" "d"]))))

(ert-deftest eas-vl-gallery-interactive-shared-legends-fit-the-height ()
  (let* ((unit (lambda (enc) (list :mark "point" :encoding enc)))
         (spec (list :data (list :values (vconcat (mapcar (lambda (i) (list :c (format "c%d" i) :v i)) (number-sequence 1 8))))
                     :vconcat (vector (funcall unit '(:x (:field "v" :type "quantitative") :color (:field "c" :type "nominal")
                                                      :size (:field "v" :type "quantitative")))
                                      (funcall unit '(:x (:field "v" :type "quantitative") :color (:field "c" :type "nominal"))))))
         (scene (eas-compile spec :size '(400 . 200))))
    (should-not (eas-vl-gallery-overlaps scene))))

(ert-deftest eas-vl-gallery-interactive-size-legends-show-the-marks-sizes ()
  (let* ((scene (eas-compile '(:data (:values [(:x 1 :y 1 :n 1) (:x 2 :y 2 :n 10)]) :mark "point"
                               :encoding (:x (:field "x" :type "quantitative" :bin t) :y (:field "y" :type "quantitative" :bin t)
                                          :size (:field "n" :type "quantitative")))))
         (view (aref (plist-get scene :views) 0))
         (marks (seq-find (lambda (m) (equal (plist-get m :mark) "point")) (plist-get view :marks)))
         (largest (seq-max (seq-map (lambda (i) (plist-get i :size)) (plist-get marks :items))))
         (legend (aref (plist-get view :legends) 0))
         (scale (plist-get (plist-get view :scales) :size)))
    (should (= largest (eas-scale-apply scale 10)))
    (should (seq-every-p (lambda (e) (= (plist-get e :size) (eas-scale-apply scale (plist-get e :value))))
                         (plist-get legend :entries)))))

(ert-deftest eas-vl-gallery-interactive-datasets-urls-read-the-mirror ()
  (should (equal (eas-vl-gallery--mirror "https://cdn.jsdelivr.net/npm/vega-datasets@v1.29.0/data/cars.json")
                 (expand-file-name "data/cars.json" eas-vl-gallery-directory)))
  (should (equal (eas-vl-gallery--mirror "../data/cars.json") "../data/cars.json")))

(ert-deftest eas-vl-gallery-interactive-rotated-labels-collide-only-when-they-touch ()
  (let* ((ticks (vconcat (mapcar (lambda (i) (list :label "Concert/Performance" :lx (* 20 i) :ly 100
                                                   :align "right" :baseline "middle"))
                                 (number-sequence 1 4))))
         (scene (list :target "svg" :size '(:w 200 :h 200 :cell [7 14])
                      :views (vector (list :id "v" :bounds [0 0 10 10]
                                           :axes (vector (list :channel "x" :orient "bottom" :labelAngle -45 :ticks ticks)))))))
    (should-not (eas-vl-gallery--label-collisions scene))
    (aset (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :axes) 0) :ticks) 1
          (list :label "Concert/Performance" :lx 22 :ly 100 :align "right" :baseline "middle"))
    (should (eas-vl-gallery--label-collisions scene))))

(ert-deftest eas-vl-gallery-interactive-d3-axis-formats-and-legend-shapes ()
  (let* ((scene (eas-compile '(:data (:values [(:x 1 :y 1200 :c "a") (:x 2 :y 3400 :c "b")])
                               :mark (:type "point" :shape "diamond")
                               :encoding (:x (:field "x" :type "quantitative")
                                          :y (:field "y" :type "quantitative" :axis (:format "$,.0f"))
                                          :color (:field "c" :type "nominal")))))
         (view (aref (plist-get scene :views) 0))
         (y (seq-find (lambda (a) (equal (plist-get a :orient) "left")) (plist-get view :axes))))
    (should (member "$1,000" (mapcar (lambda (tk) (plist-get tk :label)) (plist-get y :ticks))))
    (should (equal (plist-get (aref (plist-get view :legends) 0) :symbol-type) "diamond"))))

(ert-deftest eas-vl-gallery-interactive-repeat-leaves-data-rows-alone ()
  (let ((spec (eas-vl-lower '(:repeat (:column ["a"])
                              :spec (:data (:values [(:repeat "column")]) :mark "point"
                                     :encoding (:x (:field (:repeat "column") :type "quantitative")))))))
    (should (equal (plist-get (plist-get (aref (plist-get spec :hconcat) 0) :data) :values) [(:repeat "column")]))))

(provide 'eas-vl-gallery-interactive-test)
;;; eas-vl-gallery-interactive-test.el ends here

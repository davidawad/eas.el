;;; eas-polar-test.el --- tests for the arc mark and polar encodings -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.29: arc geometry, theta stacking and scales, polar text,
;; legend orient "none", and hovering a wedge headlessly through
;; `eas-dispatch'.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defconst eas-polar-test--pie
  '(:data (:values [(:category 1 :value 4) (:category 2 :value 6) (:category 3 :value 10)])
    :mark (:type "arc" :tooltip t)
    :encoding (:theta (:field "value" :type "quantitative")
               :color (:field "category" :type "nominal")))
  "Three wedges: 4, 6 and 10 of 20.")

(defun eas-polar-test--items (scene &optional mark)
  "Items of MARK (default 0) in SCENE's first view, as a list."
  (append (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) (or mark 0)) :items) nil))

(defun eas-polar-test--near (a b) "A within 1e-6 of B." (< (abs (- a b)) 1e-6))

;;; Geometry

(ert-deftest eas-polar-arc-paths ()
  (let ((half '(:cx 50 :cy 50 :innerRadius 0 :outerRadius 10 :startAngle 0 :endAngle 3.141592653589793))
        (donut '(:cx 0 :cy 0 :innerRadius 5 :outerRadius 10 :startAngle 0 :endAngle 1.5707963267948966))
        (full '(:cx 0 :cy 0 :innerRadius 0 :outerRadius 10 :startAngle 0 :endAngle 6.283185307179586)))
    (should (equal (eas-arc-path half) "M50,40A10,10 0 0 1 50,60L50,50Z"))
    (should (equal (eas-arc-path donut) "M0,-10A10,10 0 0 1 10,0L5,0A5,5 0 0 0 0,-5Z"))
    (should (equal (eas-arc-path full) "M0,-10A10,10 0 1 1 0,10A10,10 0 1 1 0,-10Z"))
    (should (eas-arc-contains-p half 55 50))
    (should-not (eas-arc-contains-p half 45 50))
    (should-not (eas-arc-contains-p donut 1 -1))
    (should (eas-arc-contains-p donut 5 -5))
    (should (equal (eas-arc-bounds half) [50.0 40.0 60.0 60.0]))))

;;; Stacking and scales

(ert-deftest eas-polar-pie-stacks-theta-in-color-order ()
  (let* ((items (eas-polar-test--items (eas-compile eas-polar-test--pie)))
         (angles (mapcar (lambda (i) (cons (plist-get i :startAngle) (plist-get i :endAngle))) items)))
    (should (eas-polar-test--near (car (nth 0 angles)) 0))
    (should (eas-polar-test--near (cdr (nth 0 angles)) (* 2 float-pi 0.2)))
    (should (eas-polar-test--near (car (nth 2 angles)) (* 2 float-pi 0.5)))
    (should (eas-polar-test--near (cdr (nth 2 angles)) (* 2 float-pi)))
    ;; The default view is 480x300: wedges fill a radius of 150 about its centre.
    (should (= (plist-get (car items) :outerRadius) 150))
    (should (equal (plist-get (car items) :fill) "#2a78d6"))
    (should (equal (plist-get (car items) :stroke) "#fcfcfb"))))

(ert-deftest eas-polar-normalize-and-order ()
  (let* ((spec '(:data (:values [(:k "x" :v 1 :o 2) (:k "y" :v 3 :o 1)]) :mark "arc"
                 :encoding (:theta (:field "v" :type "quantitative" :stack "normalize"
                                    :scale (:range [1 2]))
                            :color (:field "k" :type "nominal") :order (:field "o"))))
         (scene (eas-compile spec))
         (theta (plist-get (plist-get (aref (plist-get scene :views) 0) :scales) :theta))
         (items (eas-polar-test--items scene)))
    (should (equal (plist-get theta :domain) [0.0 1.0]))
    ;; The order channel puts y (o=1) first, from the range's start.
    (should (eas-polar-test--near (plist-get (nth 1 items) :startAngle) 1))
    (should (eas-polar-test--near (plist-get (nth 1 items) :endAngle) 1.75))
    (should (eas-polar-test--near (plist-get (nth 0 items) :endAngle) 2))))

(ert-deftest eas-polar-sqrt-radius-scale ()
  (let ((s '(:type "sqrt" :domain [0.0 100.0] :range [20 120])))
    (should (= (eas-scale-apply s 25) 70.0))
    (should (eas-polar-test--near (eas-scale-invert s 70) 25))
    (should (= (eas-scale-apply '(:type "pow" :exponent 2 :domain [0.0 10.0] :range [0 100]) 5) 25.0))))

(ert-deftest eas-polar-radial-chart ()
  (let* ((spec '(:data (:values [4 16]) :width 200 :height 200
                 :layer [(:mark (:type "arc" :innerRadius 10))
                         (:mark (:type "text" :radiusOffset 5) :encoding (:text (:field "data" :type "quantitative")))]
                 :encoding (:theta (:field "data" :type "quantitative" :stack t)
                            :radius (:field "data" :scale (:type "sqrt" :zero t :rangeMin 10)))))
         (scene (eas-compile spec))
         (view (aref (plist-get scene :views) 0))
         (arcs (eas-polar-test--items scene 0)) (labels (eas-polar-test--items scene 1))
         (b (plist-get view :bounds)) (cx (+ (aref b 0) 100)) (cy (+ (aref b 1) 100)))
    (should (equal (plist-get (plist-get (plist-get view :scales) :radius) :range) [10 100.0]))
    ;; sqrt(16) of sqrt(16) reaches the full radius; sqrt(4) half of the span.
    (should (= (plist-get (nth 1 arcs) :outerRadius) 100.0))
    (should (= (plist-get (nth 0 arcs) :outerRadius) 55.0))
    (should (= (plist-get (nth 0 arcs) :innerRadius) 10))
    ;; The label of 4 sits mid-wedge (0.2 of a turn / 2) at radius 55 + 5.
    (let ((label (nth 0 labels)) (a (* float-pi 0.2)))
      (should (equal (plist-get label :text) "4"))
      (should (eas-polar-test--near (plist-get label :x) (+ cx (* 60 (sin a)))))
      (should (eas-polar-test--near (plist-get label :y) (- cy (* 60 (cos a))))))))

;;; Layout

(ert-deftest eas-polar-legend-orient-none ()
  (let* ((spec (append eas-polar-test--pie
                       '(:width 200 :height 200)))
         (spec (plist-put (copy-tree spec) :encoding
                          '(:theta (:field "value" :type "quantitative")
                            :color (:field "category" :type "nominal"
                                    :legend (:orient "none" :legendX 30 :legendY 40 :title :null)))))
         (scene (eas-compile spec))
         (view (aref (plist-get scene :views) 0))
         (b (plist-get view :bounds))
         (legend (aref (plist-get view :legends) 0)))
    (should (equal (plist-get legend :orient) "none"))
    (should (= (plist-get legend :x) (+ (aref b 0) 30)))
    (should (= (plist-get legend :y) (+ (aref b 1) 40)))
    (should-not (plist-get legend :title-mark))
    ;; Inside the plot, the canvas does not grow for it.
    (should (= (plist-get (plist-get scene :size) :w) (+ 200 (* 2 (aref b 0)))))))

(ert-deftest eas-polar-text-backend-fills-wedges-in-braille ()
  "Wedges are braille sector fills (fc-qx1.49), each its own datum's cells."
  (let* ((scene (eas-compile eas-polar-test--pie :target 'text :size '(:cols 40 :rows 12)))
         (text (eas-text-render scene))
         (data (delete-dups (cl-loop for i below (length text) for d = (get-text-property i 'eas-datum text)
                                     when (and d (get-text-property i 'eas-mark text)) collect d))))
    (should (string-match-p "⣿" text))
    (should (= (length data) (length (eas-polar-test--items scene))))
    (should (string-match-p "● 3" (substring-no-properties text)))))

;;; Interaction

(ert-deftest eas-polar-hover-a-wedge ()
  (let ((eas-views (make-hash-table :test 'equal)))
    (let* ((v (eas-view-open eas-polar-test--pie :id "pie"))
           (item (nth 2 (eas-polar-test--items (eas-view-scene v))))
           (hover (plist-get (eas-dispatch v (list :type "pointermove"
                                                   :px (vector (plist-get item :x) (plist-get item :y))))
                             :hover)))
      (should (eas-view-interactive v))
      (should (= (plist-get hover :datum) 2))
      (should (equal (mapcar (lambda (p) (cons (plist-get p :title) (plist-get p :value))) (plist-get hover :tooltip))
                     '(("value" . "10") ("category" . "3"))))
      (let ((area (car (eas-svg-hot-spots (eas-view-scene v)))))
        (should (eq (car (car area)) 'poly))
        (should (eq (cadr area) 'eas:main|main/0|0))))))

(provide 'eas-polar-test)
;;; eas-polar-test.el ends here

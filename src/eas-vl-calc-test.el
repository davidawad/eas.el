;;; eas-vl-calc-test.el --- the calculations gallery's engine features -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.43, polishing the calculations group: closed lines, free
;; rules, text and stroke properties, labels fitted to a resize, shared
;; transforms run once, zone offsets, references that omit marks, and
;; the customization specs.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery)
(require 'eas-vl-gallery-custom)

(defun eas-vl-calc-test--items (spec &optional k)
  "Items of mark K (default 0) of SPEC's first view."
  (plist-get (aref (plist-get (aref (plist-get (eas-compile spec) :views) 0) :marks) (or k 0)) :items))

(defconst eas-vl-calc-test--xy
  '(:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative"))
  "Quantitative x and y.")

(ert-deftest eas-vl-calc-linear-closed-lines-close-and-fill ()
  (let* ((spec `(:data (:values [(:x 0 :y 0) (:x 1 :y 1) (:x 2 :y 0)])
                 :mark (:type "line" :interpolate "linear-closed" :fill "#ff0000") :encoding ,eas-vl-calc-test--xy))
         (pts (plist-get (aref (eas-vl-calc-test--items spec) 0) :points)))
    (should (= (length pts) 4))
    (should (equal (aref pts 0) (aref pts 3)))
    (should (string-match-p "<path d=\"M[^\"]+\" fill=\"#ff0000\"" (eas-svg-render (eas-compile spec)))))
  (should (equal (eas-curve-apply '((0 0) (1 1)) "linear-closed") '((0 0) (1 1)))))

(ert-deftest eas-vl-calc-rules-with-both-ends-are-free-segments ()
  (let* ((spec '(:data (:values [(:x 0 :y 0 :x2 2 :y2 2)]) :mark "rule"
                 :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                            :x2 (:field "x2") :y2 (:field "y2"))))
         (item (aref (eas-vl-calc-test--items spec) 0)))
    (should-not (= (plist-get item :x1) (plist-get item :x2)))
    (should-not (= (plist-get item :y1) (plist-get item :y2)))))

(ert-deftest eas-vl-calc-top-level-padding ()
  (let ((spec `(:data (:values [(:x 0 :y 0)]) :mark "point" :encoding ,eas-vl-calc-test--xy)))
    (should (= (- (plist-get (plist-get (eas-compile (append spec '(:padding 50))) :size) :w)
                  (plist-get (plist-get (eas-compile spec) :size) :w))
               90))))

(ert-deftest eas-vl-calc-size-is-the-stroke-width-of-lines-and-rules ()
  (let ((line `(:data (:values [(:x 0 :y 0) (:x 1 :y 1)]) :mark "line"
                :encoding ,(append eas-vl-calc-test--xy '(:size (:value 1))))))
    (should (= (plist-get (aref (eas-vl-calc-test--items line) 0) :strokeWidth) 1)))
  (let ((rule '(:data (:values [(:x 1)]) :mark (:type "rule" :size 6 :strokeCap "round")
                :encoding (:x (:field "x" :type "quantitative")))))
    (should (= (plist-get (aref (eas-vl-calc-test--items rule) 0) :strokeWidth) 6))
    (should (string-match-p "stroke-linecap=\"round\"" (eas-svg-render (eas-compile rule))))))

(ert-deftest eas-vl-calc-text-mark-properties ()
  (let* ((spec `(:data (:values [(:x 0 :y 0 :t "a long label here")])
                 :mark (:type "text" :font "Courier New" :fontStyle "italic" :angle 30 :limit 40 :ellipsis "~"
                        :fill "#123456")
                 :encoding ,(append eas-vl-calc-test--xy '(:text (:field "t" :type "nominal")))))
         (item (aref (eas-vl-calc-test--items spec) 0))
         (svg (eas-svg-render (eas-compile spec))))
    (should (string-suffix-p "~" (plist-get item :text)))
    (should (< (length (plist-get item :text)) (length "a long label here")))
    (should (equal (plist-get item :fill) "#123456"))
    (dolist (re '("font-family=\"Courier New\"" "font-style=\"italic\"" "rotate(30 "))
      (should (string-match-p (regexp-quote re) svg)))))

(ert-deftest eas-vl-calc-bar-stroke-width-and-dash ()
  (let ((svg (eas-svg-render (eas-compile '(:data (:values [(:k "a" :v 1)])
                                           :mark (:type "bar" :stroke "black" :strokeWidth 3 :strokeDash [4 2])
                                           :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))))))
    (should (string-match-p "stroke-width=\"3\"" svg))
    (should (string-match-p "stroke-dasharray=\"4,2\"" svg))))

(ert-deftest eas-vl-calc-axis-fonts-and-opacities ()
  (let ((svg (eas-svg-render
              (eas-compile '(:data (:values [(:k "a" :v 1)]) :mark "bar"
                             :encoding (:x (:field "k" :type "nominal"
                                            :axis (:labelFont "Courier New" :labelFontStyle "italic" :labelOpacity 0.5))
                                        :y (:field "v" :type "quantitative" :axis (:titleFont "Georgia" :domainDash [2 2]))))))))
    (dolist (re '("font-family=\"Courier New\"" "font-style=\"italic\"" "opacity=\"0.5\"" "font-family=\"Georgia\""
                  "stroke-dasharray=\"2,2\""))
      (should (string-match-p (regexp-quote re) svg)))))

(ert-deftest eas-vl-calc-crowded-labels-fit-a-resized-plot ()
  (let* ((spec '(:data (:values [(:k "Beak Length (mm)" :v 1) (:k "Flipper Length (mm)" :v 2) (:k "Body Mass (g)" :v 3)])
                 :mark "point" :width 600
                 :encoding (:x (:field "k" :type "nominal" :axis (:labelAngle 0)) :y (:field "v" :type "quantitative"))))
         (labels (lambda (scene) (mapcar (lambda (tk) (plist-get tk :label))
                                         (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :axes) 0) :ticks)))))
    ;; At its own size every label is whole, as Vega-Lite draws it.
    (should (equal (funcall labels (eas-compile spec)) '("Beak Length (mm)" "Body Mass (g)" "Flipper Length (mm)")))
    ;; Fitted to 200px they are cut to their step and stop colliding.
    (let ((small (eas-compile spec :size '(200 . 150))))
      (should (seq-every-p (lambda (l) (string-suffix-p "…" l)) (funcall labels small)))
      (should-not (eas-vl-gallery-overlaps small)))))

(ert-deftest eas-vl-calc-layers-share-their-parent-transforms ()
  (let ((runs 0)
        (spec '(:data (:values [(:a 1) (:a 2)]) :transform [(:calculate "datum.a * 2" :as "b")]
                :layer [(:mark "point" :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")))
                        (:mark "line" :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")))
                        (:mark "tick" :encoding (:x (:field "a" :type "quantitative") :y (:field "b" :type "quantitative")))])))
    (advice-add 'eas-transform-run :before (lambda (transforms &rest _) (when (> (length transforms) 0) (setq runs (1+ runs))))
                '((name . eas-vl-calc-test-count)))
    (unwind-protect (eas-compile spec)
      (advice-remove 'eas-transform-run 'eas-vl-calc-test-count))
    (should (= runs 1))
    ;; Each layer still gets rows of its own.
    (let ((views (plist-get (eas-compile spec) :views)))
      (should-not (eq (aref (plist-get (aref (plist-get (aref views 0) :marks) 0) :rows) 0)
                      (aref (plist-get (aref (plist-get (aref views 0) :marks) 1) :rows) 0))))))

(ert-deftest eas-vl-calc-zone-offsets-match-emacs ()
  "Week-cached offsets agree with `decode-time' and `encode-time', DST weeks included."
  (let ((eas-time-zone "America/Chicago"))
    ;; Around the 2021 spring-forward and fall-back transitions, hour by hour.
    (dolist (start '(1615708800000 1636268400000))
      (dotimes (h 72)
        (let* ((ms (+ start (* h 3600000) 1234))
               (d (decode-time (floor ms 1000) eas-time-zone))
               (f (eas-time-fields ms)))
          (should (equal (list (plist-get f :year) (plist-get f :month) (plist-get f :day) (plist-get f :hours)
                               (plist-get f :weekday))
                         (list (decoded-time-year d) (decoded-time-month d) (decoded-time-day d) (decoded-time-hour d)
                               (decoded-time-weekday d))))
          (should (= (eas-time-ms (plist-get f :year) (plist-get f :month) (plist-get f :day) (plist-get f :hours)
                                  (plist-get f :minutes) (plist-get f :seconds) (plist-get f :milliseconds))
                     (+ (* 1000 (time-convert (encode-time (list (plist-get f :seconds) (plist-get f :minutes)
                                                                 (plist-get f :hours) (plist-get f :day) (plist-get f :month)
                                                                 (plist-get f :year) nil -1 eas-time-zone))
                                              'integer))
                        (plist-get f :milliseconds)))))))
    (should (eas-time-offset-week 1600000000000 "America/Chicago"))
    (should-not (eas-time-offset-week 1615708800000 "America/Chicago"))))

(ert-deftest eas-vl-calc-reference-omissions ()
  (let ((scene '(:views [(:marks [(:mark "bar" :items [1]) (:mark "tick" :items [2])])])))
    (should (equal (eas-vl-gallery-omit-marks scene '("bar")) '(:views [(:marks [(:mark "tick" :items [2])])])))
    (should (eq (eas-vl-gallery-omit-marks scene nil) scene)))
  (should (equal (eas-vl-gallery-ref-omits "calculations" "layer_cumulative_histogram") '("bar")))
  (should-not (eas-vl-gallery-ref-omits "calculations" "lookup"))
  ;; The conformance entry compares without the omitted marks.
  (let ((entry (seq-find (lambda (e) (equal (plist-get e :name) "calculations/layer_cumulative_histogram"))
                         (eas-vl-gallery-conformance-entries))))
    (should (functionp (plist-get entry :oracle-scene)))))

(ert-deftest eas-vl-calc-customization-specs-hold ()
  "One customization spec per chart type of the group, each drawn natively
with every property it sets (see eas-vl-gallery-custom.el)."
  (let ((names (eas-vl-gallery-custom-names "calculations")))
    (should (equal names '("custom_area" "custom_bar" "custom_line" "custom_point" "custom_rule" "custom_text"
                           "custom_tick")))
    (dolist (name names)
      (should (equal (cons name (eas-vl-gallery-custom-check "calculations" name)) (list name))))))

(provide 'eas-vl-calc-test)
;;; eas-vl-calc-test.el ends here

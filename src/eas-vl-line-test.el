;;; eas-vl-line-test.el --- the line gallery group's polish (fc-qx1.41) -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.41: what the line group needed past its first pass, each on
;; a small spec: Vega-Lite's path grouping, facet field titles, legends
;; above and below the plot, d3's curves, line and trail mark
;; properties, a legend's, a title's and a header's own properties, the
;; soft check findings for properties drawn without, squeezed discrete
;; axes, and the customization specs (test/vl-examples/line/custom/).

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-vl-gallery)
(require 'eas-agent)

(defun eas-vl-line-test--rows ()
  "Two series a and b over x 1..4."
  (vconcat (cl-loop for s in '("a" "b") for k from 1
                    append (cl-loop for x from 1 to 4 collect (list :s s :x x :y (+ k (* x k)) :d (* 10 k))))))

(defun eas-vl-line-test--line (&rest props)
  "A line over the test rows with mark PROPS."
  (list :data (list :values (eas-vl-line-test--rows)) :mark (append (list :type "line") props)
        :encoding '(:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                    :color (:field "s" :type "nominal"))))

(defun eas-vl-line-test--items (scene &optional mark)
  "Items of SCENE's MARK (default main/0)."
  (plist-get (eas-scene-mark scene (or mark "main/0")) :items))

;;; Marks

(ert-deftest eas-vl-line-paths-group-like-vega-lite ()
  "Any unaggregated color field splits paths, quantitative too; size
splits lines but not trails."
  (let ((spec (eas-vl-line-test--line)))
    (setq spec (plist-put spec :encoding '(:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                                         :color (:field "d" :type "quantitative"))))
    (should (= (length (eas-vl-line-test--items (eas-compile spec))) 2))
    (setq spec (plist-put spec :encoding '(:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
                                         :size (:field "d" :type "quantitative"))))
    (should (= (length (eas-vl-line-test--items (eas-compile spec))) 2))
    (should (= (length (eas-vl-line-test--items (eas-compile (plist-put spec :mark '(:type "trail"))))) 1))))

(ert-deftest eas-vl-line-order-false-keeps-data-order ()
  (let* ((rows [(:x 3 :y 1) (:x 1 :y 2) (:x 2 :y 3)])
         (spec (list :data (list :values rows) :mark '(:type "line" :order :false)
                     :encoding '(:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")))))
    (should (equal (plist-get (aref (eas-vl-line-test--items (eas-compile spec)) 0) :datum) [0 1 2]))
    (should (equal (plist-get (aref (eas-vl-line-test--items (eas-compile (plist-put spec :mark "line"))) 0) :datum)
                   [1 2 0]))))

;;; Facets, legends, titles

(defconst eas-vl-line-test--facet
  `(:data (:values ,(eas-vl-line-test--rows)) :mark "line"
    :encoding (:x (:field "x" :type "quantitative") :y (:field "y" :type "quantitative")
               :column (:field "s" :title "Series" :header (:labelColor "#aa0000" :labelFontSize 12)))))

(defun eas-vl-line-test--headers (scene)
  "Items of SCENE's facet-headers mark (the facet grid draws labels and title there)."
  (seq-some (lambda (v) (seq-some (lambda (m) (and (string-suffix-p "/facet-headers" (plist-get m :id))
                                                   (append (plist-get m :items) nil)))
                                  (plist-get v :marks)))
            (plist-get scene :views)))

(ert-deftest eas-vl-line-facet-title-and-header-style ()
  (let* ((eas-spec-supported-function nil)
         (scene (eas-compile eas-vl-line-test--facet))
         (v0 (aref (plist-get scene :views) 0)) (v1 (aref (plist-get scene :views) 1))
         (items (eas-vl-line-test--headers scene))
         (item (lambda (text) (seq-find (lambda (i) (equal (plist-get i :text) text)) items)))
         (title (funcall item "Series")) (label (funcall item "a")))
    (should title)
    ;; Centred over both cells' plots, 20px above the labels' tops.
    (should (= (plist-get title :x) (/ (+ (aref (plist-get v0 :bounds) 0)
                                          (aref (plist-get v1 :bounds) 0) (aref (plist-get v1 :bounds) 2))
                                       2.0)))
    (should (= (plist-get title :y) (- (plist-get label :y) 12 20)))
    (should (equal (plist-get label :fill) "#aa0000"))
    (should (equal (aref (plist-get v0 :bounds) 1) (aref (plist-get v1 :bounds) 1)))
    ;; A null header title draws none.
    (should-not (funcall (lambda (items) (seq-find (lambda (i) (equal (plist-get i :text) "s")) items))
                         (eas-vl-line-test--headers
                          (eas-compile (plist-put (copy-tree eas-vl-line-test--facet) :encoding
                                                  '(:x (:field "x" :type "quantitative")
                                                    :y (:field "y" :type "quantitative")
                                                    :column (:field "s" :header (:title :null))))))))))

;;; Check

;;; Axes squeezed below their step

;;; The customization specs

(provide 'eas-vl-line-test)
;;; eas-vl-line-test.el ends here

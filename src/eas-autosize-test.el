;;; eas-autosize-test.el --- autosize fit and centred end labels -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Vega-Lite's autosize "fit" sizes the whole single view, axes, labels
;; overhanging the plot and title included, to the spec's width and
;; height (eas-container.el), and an axis with labelFlush false centres
;; its end labels in the SVG as it does in the layout.

;;; Code:

(require 'eas-test-support)
(require 'eas)

(defun eas-autosize-test--spec (&rest props)
  "A horizontal bar chart of eight long categories, 300 x 200, with PROPS."
  (append props
          (list :data (list :values (vconcat (mapcar (lambda (i) (list :k (format "category %d" i) :n (* 10 i)))
                                                     (number-sequence 1 8))))
                :mark "bar" :width 300 :height 200
                :encoding '(:y (:field "k" :type "nominal")
                            :x (:field "n" :type "quantitative" :axis (:labelFlush :false :offset 5))))))

(defun eas-autosize-test--size (spec)
  "The canvas (W . H) of SPEC's scene."
  (let ((size (plist-get (eas-compile spec) :size)))
    (cons (plist-get size :w) (plist-get size :h))))

(defun eas-autosize-test--x-axis (spec)
  "SPEC's x axis in its scene."
  (seq-find (lambda (a) (equal (plist-get a :channel) "x"))
            (plist-get (aref (plist-get (eas-compile spec) :views) 0) :axes)))

(ert-deftest eas-autosize-fit-sizes-the-whole-view ()
  "Fit: the canvas is the spec's size plus the padding, labels inside."
  (let ((pad '(:type "fit")))
    (should (equal (eas-autosize-test--size (eas-autosize-test--spec :autosize pad)) '(310 . 210)))
    (should (equal (eas-autosize-test--size (eas-autosize-test--spec :autosize "fit")) '(310 . 210)))
    ;; The last x label's overhang is inside the canvas, not the padding.
    (should (<= (aref (plist-get (eas-autosize-test--x-axis (eas-autosize-test--spec :autosize pad)) :bounds) 2)
                305)))
  (should (equal (eas-autosize-test--size (eas-autosize-test--spec :autosize '(:type "fit" :contains "padding")))
                 '(300 . 200)))
  ;; Without it the plot is the spec's size and the chrome adds to it.
  (let ((natural (eas-autosize-test--size (eas-autosize-test--spec))))
    (should (> (car natural) 310))
    (should (> (cdr natural) 210))
    ;; fit-x fits the width alone.
    (should (equal (eas-autosize-test--size (eas-autosize-test--spec :autosize "fit-x"))
                   (cons 310 (cdr natural))))))

(ert-deftest eas-autosize-label-flush-false-centres-the-end-labels ()
  "labelFlush false centres a continuous x axis's end labels in the SVG."
  (let ((anchors (lambda (spec)
                   (let ((svg (eas-svg-render (eas-compile spec))))
                     (mapcar (lambda (label)
                               (and (string-match (format "text-anchor=\"\\([a-z]+\\)\">%s<" label) svg)
                                    (match-string 1 svg)))
                             '("0" "80"))))))
    (should (equal (funcall anchors (eas-autosize-test--spec)) '("middle" "middle")))
    (let ((flush (copy-tree (eas-autosize-test--spec))))
      (setf (plist-get (plist-get (plist-get flush :encoding) :x) :axis) (list :offset 5))
      (should (equal (funcall anchors flush) '("start" "end"))))))

(provide 'eas-autosize-test)
;;; eas-autosize-test.el ends here

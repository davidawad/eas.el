;;; eas-demo-linked.el --- demo: linked views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Overview and detail: an x brush on the lower chart is the
;; upper chart's x domain (`scale.domain: {param: brush}'), one param
;; shared across a vconcat, so brushing the overview zooms the detail.
;; Financial: series-line over a price history.  Non-financial: area
;; over daily steps.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-linked--spec (template rows y title)
  "TEMPLATE over ROWS' date and Y as a detail above a brushed overview."
  (let* ((spec (eas-demo-spec template (list :data (eas-demo-xy rows :date y) :x_type "temporal"
                                             :x_title "date" :y_title (substring (symbol-name y) 1))))
         (unit (lambda (&rest extra)
                 (append extra (list :height 110)
                         (cl-loop for k in '(:mark :encoding :layer) when (plist-get spec k)
                                  append (list k (plist-get spec k)))))))
    (list :$schema (plist-get spec :$schema) :title title :description title
          :data (plist-get spec :data)
          :vconcat
          (vector (funcall unit :encoding
                           (let ((enc (copy-tree (plist-get spec :encoding))))
                             (when enc (plist-put (plist-get enc :x) :scale '(:domain (:param "brush"))))
                             enc))
                  (funcall unit :params [(:name "brush" :select (:type "interval" :encodings ["x"]))])))))

(defun eas-demo-linked--steps (view)
  "Steps brushing the lower (overview) plot of VIEW."
  (let* ((b (plist-get (aref (plist-get (eas-view-scene view) :views) 1) :bounds))
         (y (+ (aref b 1) (/ (aref b 3) 2.0)))
         (x0 (+ (aref b 0) (* 0.3 (aref b 2)))) (x1 (+ (aref b 0) (* 0.6 (aref b 2)))))
    (append (list (list :glide (vector x0 y) :frames 6) '(:hold 3) (list :type "pointerdown" :px (vector x0 y)))
            (cl-loop for i from 1 to 8 collect (list :type "pointermove" :px (vector (+ x0 (* i (/ (- x1 x0) 8.0))) y)))
            (list (list :type "pointerup" :px (vector x1 y)) '(:hold 14)))))

(eas-demo-run
 "linked"
 (list (list :title "ACME close, overview and detail (series-line template)"
             :source (eas-demo-linked--spec "series-line" (eas-demo-prices) :close "ACME close")
             :steps #'eas-demo-linked--steps)
       (list :title "Daily steps, overview and detail (area template)"
             :source (eas-demo-linked--spec "area" (eas-demo-heart) :steps "Daily steps")
             :steps #'eas-demo-linked--steps)))

;;; eas-demo-linked.el ends here

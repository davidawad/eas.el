;;; eas-demo-zoom.el --- demo: zoom and pan -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An interval selection bound to the scales (`bind: "scales"'): the
;; wheel zooms around the pointer, a drag pans, and inspect's domains
;; give the visible window.  Financial: the series-line template over a
;; price history.  Non-financial: the area template over daily steps.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-zoom--spec (template rows y title)
  "TEMPLATE over ROWS' date and Y with a scale-bound interval selection."
  (eas-demo-spec template (list :data (eas-demo-xy rows :date y) :x_type "temporal"
                                :x_title "date" :y_title (substring (symbol-name y) 1) :title title)
                 :params [(:name "grid" :select (:type "interval" :encodings ["x"]) :bind "scales")]))

(defun eas-demo-zoom--steps (view)
  "Wheel in twice at the middle of VIEW's plot, then drag it left."
  (let ((mid (eas-demo-at view 0.6 0.5)) (left (eas-demo-at view 0.3 0.5)))
    (append (list (list :glide mid :frames 6) '(:hold 3))
            (cl-loop repeat 3 append (list (list :type "wheel" :px mid :delta -1) '(:hold 4)))
            (list (list :type "pointerdown" :px mid))
            (cl-loop for i from 1 to 6
                     collect (list :type "pointermove"
                                   :px (vector (+ (aref mid 0) (* i (/ (- (aref left 0) (aref mid 0)) 6.0)))
                                               (aref mid 1))))
            (list (list :type "pointerup" :px left) '(:hold 10)
                  (list :type "key" :key "0") '(:hold 8)))))

(eas-demo-run
 "zoom"
 (list (list :title "ACME close (series-line template)"
             :source (eas-demo-zoom--spec "series-line" (eas-demo-prices) :close "ACME close")
             :steps #'eas-demo-zoom--steps)
       (list :title "Daily steps (area template)"
             :source (eas-demo-zoom--spec "area" (eas-demo-heart) :steps "Daily steps")
             :steps #'eas-demo-zoom--steps)))

;;; eas-demo-zoom.el ends here

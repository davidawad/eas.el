;;; eas-demo-brush.el --- demo: brush (interval selection) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; An x interval selection (`select: {type: interval, encodings: [x]}')
;; dragged across a price series and across a resting heart-rate
;; series, as a mouse would: pointerdown, pointermoves, pointerup.
;; Rows outside the brush fade.  `eas-selection' answers the range.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-brush--spec (template rows y)
  "TEMPLATE over ROWS with an x brush that fades points off Y."
  (eas-demo-spec template (list :data rows :x "date" :y y :points t :title "")
                 :params (vector '(:name "brush" :select (:type "interval" :encodings ["x"])))))

(defun eas-demo-brush--drag (f0 f1)
  "Steps dragging a brush from F0 to F1 of the plot's width."
  (lambda (view)
    (let ((p0 (eas-demo-at view f0 0.5)) (p1 (eas-demo-at view f1 0.5)))
      (append (list (list :glide p0 :frames 6) '(:hold 3) (list :type "pointerdown" :px p0))
              (cl-loop for i from 1 to 8
                       collect (list :type "pointermove"
                                     :px (vector (+ (aref p0 0) (* i (/ (- (aref p1 0) (aref p0 0)) 8.0)))
                                                 (aref p0 1))))
              (list (list :type "pointerup" :px p1) '(:hold 6)
                    (list :call (lambda (view)
                                  (message "selection: %d rows" (length (eas-selection view "brush")))
                                  nil))
                    '(:hold 10))))))

(eas-demo-run
 "brush"
 (list (list :title "ACME daily close, brushed (line template)"
             :source (eas-demo-brush--spec "line" (eas-demo-prices) "close")
             :steps (eas-demo-brush--drag 0.35 0.7))
       (list :title "Resting heart rate, brushed (line template)"
             :source (eas-demo-brush--spec "line" (eas-demo-heart) "bpm")
             :steps (eas-demo-brush--drag 0.2 0.55))))

;;; eas-demo-brush.el ends here

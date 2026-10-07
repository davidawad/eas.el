;;; eas-demo-live.el --- demo: live push and stream -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A view opened with a stream (`x-eas.stream', here passed to
;; `eas-stream-open'): `eas-stream-push' queues rows and each frame
;; draws them, the window keeping the latest N.  The demo pushes a few
;; rows per frame and flushes, as the frame timer would.  Financial:
;; ACME ticks on the series-line template.  Non-financial: a heart-rate
;; monitor's readings on the line template.
;; See examples/eas-demo.el for the three ways to run it.

;;; Code:

(load (expand-file-name "eas-demo" (file-name-directory (or load-file-name buffer-file-name))) nil t)

(defun eas-demo-live--open (source &rest args)
  "Open SOURCE (ARGS as in `eas-view-open') with a 40-row stream window."
  (apply #'eas-stream-open source :stream '(:max-fps 10 :window 40) args))

(defun eas-demo-live--steps (make-row)
  "Steps pushing three rows per frame from MAKE-ROW (called with I)."
  (let ((i 30))
    (cl-loop repeat 16
             append (list (list :call (lambda (view)
                                        (eas-stream-push view (vconcat (cl-loop repeat 3 collect (funcall make-row (cl-incf i)))))
                                        (eas-stream-flush view)
                                        nil))
                          '(:hold 1)))))

(defun eas-demo-live--tick (i)
  "ACME tick I."
  (let ((p (elt (eas-demo-prices 80) (min i 79))))
    (list :x i :y (plist-get p :close))))

(defun eas-demo-live--beat (i)
  "Heart-rate reading I, one a minute from 08:00."
  (list :date (format "2026-03-02T%02d:%02d:00" (+ 8 (/ i 60)) (% i 60))
        :bpm (+ 64 (round (* 6 (sin (* i 0.2)))) (round (* 3 (eas-demo--noise 9 i))))))

(eas-demo-run
 "live"
 (list (list :title "ACME ticks (series-line template)"
             :open #'eas-demo-live--open :source "series-line"
             :bindings (list :data (vconcat (cl-loop for i from 1 to 30 collect (eas-demo-live--tick i)))
                             :x_title "tick" :y_title "price" :title "ACME, live")
             :steps (eas-demo-live--steps #'eas-demo-live--tick))
       (list :title "Heart rate monitor (line template)"
             :open #'eas-demo-live--open :source "line"
             :bindings (list :data (vconcat (cl-loop for i from 1 to 30 collect (eas-demo-live--beat i)))
                             :y "bpm" :title "Heart rate, live")
             :steps (eas-demo-live--steps #'eas-demo-live--beat))))

;;; eas-demo-live.el ends here

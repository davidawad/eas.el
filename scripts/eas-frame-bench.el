;;; eas-frame-bench.el --- per-frame cost of live and animated views -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.19.  Milliseconds per frame of the live workloads a session
;; runs: an order-book ladder and depth-live update (financial-charts.el,
;; when its src/ is on the load path), a pacman tick, a clock tick and
;; an airport-connections hover, each in SVG and text.  A frame is the
;; update (push, tick or pointermove) plus the redraw the glue does:
;; the SVG string, or the text grid patched into a buffer.
;;
;;   emacs -Q --batch -L src -L $FC/src -L $FC/src/integrations ... \
;;     -l scripts/eas-frame-bench.el -f eas-frame-bench-report

;;; Code:

(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-stream)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-mode-patch)
(require 'eas-template)

(defvar eas-frame-bench-frames 20 "Frames timed per workload.")

(defun eas-frame-bench--draw (view buffer)
  "Draw VIEW's scene as the glue does; text goes into BUFFER."
  (let ((scene (eas-view-scene view)))
    (if (eq (eas-view-target view) 'svg) (eas-svg-render scene)
      (with-current-buffer buffer
        (let ((inhibit-read-only t)) (eas-mode-patch-text (eas-text-render scene)))))))

(defun eas-frame-bench--time (view step)
  "Mean ms per frame of STEP (a function of the frame number) then a draw of VIEW."
  (let ((buffer (generate-new-buffer " *eas-frame-bench*")) (total 0.0))
    (unwind-protect
        (progn
          (funcall step 0) (eas-frame-bench--draw view buffer)
          (garbage-collect)
          (dotimes (i eas-frame-bench-frames)
            (let ((t0 (float-time)))
              (funcall step (1+ i))
              (eas-frame-bench--draw view buffer)
              (cl-incf total (- (float-time) t0)))))
      (kill-buffer buffer))
    (/ (round (* 10000 (/ total eas-frame-bench-frames))) 10.0)))

(defun eas-frame-bench--size (target)
  "The bench size for TARGET."
  (if (eq target 'text) '(:cols 100 :rows 40) nil))

(defun eas-frame-bench--play (template target)
  "Ms per timer tick of vega TEMPLATE on TARGET."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (view (eas-play-open template :bindings (eas-template-example template)
                              :target target :size (eas-frame-bench--size target))))
    (unwind-protect
        (eas-frame-bench--time view (lambda (_) (cl-incf clock 1.0) (eas-play-tick view)))
      (eas-play-detach view) (eas-view-close view))))

(defun eas-frame-bench--hover (template target)
  "Ms per pointermove of vega TEMPLATE on TARGET, sweeping the canvas."
  (let* ((view (eas-view-open template :bindings (eas-template-example template)
                              :target target :size (eas-frame-bench--size target)))
         (size (plist-get (eas-view-scene view) :size))
         (w (or (plist-get size :width) 800)) (h (or (plist-get size :height) 500)))
    (unwind-protect
        (eas-frame-bench--time
         view (lambda (i)
                (eas-dispatch view (list :type "pointermove"
                                         :px (vector (* w (/ (+ 0.5 (% (* 7 i) 20)) 20.0))
                                                     (* h (/ (+ 0.5 (% (* 3 i) 10)) 10.0)))))))
      (eas-view-close view))))

(defun eas-frame-bench--book (template target)
  "Ms per 25-level order-book frame of TEMPLATE on TARGET, or nil."
  (when (require 'financial-chart-eas-book-bench nil t)
    (let* ((clock 0.0)
           (eas-stream-clock (lambda () clock))
           (eas-stream-use-timers nil)
           (view (financial-chart-book-open (financial-chart-book-bench-snapshot 25)
                                            :template template :levels 25 :flash 0.5 :target target
                                            :size (if (eq target 'text) '(:cols 100 :rows 40) '(640 . 360))))
           (live (financial-chart-book--live view)))
      (unwind-protect
          (eas-frame-bench--time
           view (lambda (i)
                  (setq clock (+ clock 1.0))
                  (financial-chart-book-apply (plist-get live :book)
                                              (financial-chart-book-bench--deltas 25 10 (1+ i)) clock)
                  (financial-chart-book--offer live clock)))
        (financial-chart-book-close view)))))

(defun eas-frame-bench ()
  "Rows (NAME TARGET MS) over the live workloads."
  (cl-loop for (name fn arg) in '(("order-book ladder update" eas-frame-bench--book "ladder")
                                  ("depth-live update" eas-frame-bench--book "depth-live")
                                  ("pacman tick" eas-frame-bench--play "pacman")
                                  ("clock tick" eas-frame-bench--play "clock")
                                  ("airport-connections hover" eas-frame-bench--hover "airport-connections"))
           append (cl-loop for target in '(svg text)
                           collect (list name target (funcall fn arg target)))))

(defun eas-frame-bench-report ()
  "Print `eas-frame-bench' as a Markdown table."
  (princ "| workload | target | ms/frame |\n|---|---|---|\n")
  (dolist (r (eas-frame-bench))
    (princ (format "| %s | %s | %s |\n" (nth 0 r) (nth 1 r) (or (nth 2 r) "n/a")))))

(provide 'eas-frame-bench)
;;; eas-frame-bench.el ends here

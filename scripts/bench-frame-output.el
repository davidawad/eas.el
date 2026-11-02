;;; bench-frame-output.el --- ms per terminal pixel frame (kitty, sixel) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-rhy.  Milliseconds per frame of a chart drawn as terminal pixels
;; (src/eas-output.el) at 200x60 cells, 2000x1200 pixels at the default
;; 10x20 cell: a line, a bar chart and a heatmap hovered across the plot
;; (one pointermove a frame) and a live candle stream (one push a
;; frame, the eas-perf candles workload).  A frame's stages:
;;
;;   update  the event through `eas-dispatch' (recompile included)
;;   svg     `eas-svg-render' of the scene
;;   kitty   rasterize (resvg) and encode as a Kitty transmit (zlib RGBA)
;;   sixel   rasterize and encode as sixel
;;
;; The kitty and sixel stages are measured on every frame, as the
;; cost of a frame that changed the picture.  The frame columns are what
;; `eas-output-sync' spends: update + svg, plus the encode only when the
;; SVG changed (a hover over bars or a heatmap moves no mark, so it
;; sends nothing).  Writing the bytes to the terminal is not measured.  Needs
;; the module built with its raster feature (make module
;; FEATURES=raster); byte-compile src/ first (make compile) for the
;; numbers docs/design/output-modes.md reports.
;;
;;   emacs -Q --batch -L src -l scripts/bench-frame-output.el [-- FRAMES]
;;
;; It prints a Markdown table: the mean over FRAMES (default 10) frames
;; after one warm-up frame.

;;; Code:

(require 'eas)
(require 'eas-perf)
(require 'eas-output)

(defconst bench-output-cols 200 "Cells across.")
(defconst bench-output-rows 60 "Cells down.")

(defun bench-output--ms (thunk)
  "Milliseconds THUNK takes, and its value: (MS . VALUE)."
  (let* ((t0 (float-time)) (v (funcall thunk)))
    (cons (* 1000 (- (float-time) t0)) v)))

(defun bench-output--hover (template &rest bindings)
  "A setup hovering TEMPLATE across its plot: (VIEW . UPDATE).
BINDINGS are added to the template's example bindings."
  (let* ((view (eas-view-open template :bindings (append bindings (eas-template-example template))
                              :size (bench-output--px) :target 'svg))
         (bounds (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :bounds)))
    (cons view
          (lambda (i)
            (let ((x (+ (aref bounds 0) (* (mod (* 37 i) 100) 0.01 (- (aref bounds 2) (aref bounds 0)))))
                  (y (/ (+ (aref bounds 1) (aref bounds 3)) 2.0)))
              (eas-dispatch view (list :type "pointermove" :px (vector x y))))))))

(defun bench-output--stream ()
  "The candle stream: (VIEW . UPDATE), one push a frame."
  (let ((view (eas-view-open (eas-perf--candles-spec (vconcat (mapcar #'eas-perf--candle (number-sequence 0 199))))
                             :size (bench-output--px) :target 'svg)))
    (cons view
          (lambda (i)
            (let ((c (eas-perf--candle (+ 200 i))))
              (eas-dispatch view (list :type "push" :key "t" :rows (vector c) :window 200)))))))

(defun bench-output--px ()
  "The pixel size of the bench's cells."
  (cons (* bench-output-cols (car eas-output-default-cell-size))
        (* bench-output-rows (cdr eas-output-default-cell-size))))

(defun bench-output-run (frames)
  "Print the table for FRAMES frames per workload."
  (unless (eq (eas-output-rasterizer-find) 'module)
    (error "Build the module with its raster feature: make module FEATURES=raster"))
  (let ((w (car (bench-output--px))) (h (cdr (bench-output--px))))
    (princ (format "%dx%d cells, %dx%d px, %d frames, %s\n\n" bench-output-cols bench-output-rows w h frames
                   (if (byte-code-function-p (symbol-function 'eas-svg-render)) "byte-compiled" "interpreted")))
    (princ "| workload | update ms | svg ms | kitty ms | sixel ms | changed | kitty frame ms | sixel frame ms | kitty KB | sixel KB |\n")
    (princ "|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n")
    (dolist (w-case `(("line with crosshair, hover" . ,(lambda () (bench-output--hover "line" :crosshair t)))
                      ("bars, hover" . ,(lambda () (bench-output--hover "bars")))
                      ("heatmap, hover" . ,(lambda () (bench-output--hover "heatmap")))
                      ("candle stream, push" . bench-output--stream)))
      (let* ((setup (funcall (cdr w-case))) (view (car setup)) (update (cdr setup))
             (sum (make-vector 4 0.0)) (kb (make-vector 2 0.0))
             (last nil) (changed 0))
        (dotimes (i (1+ frames))
          (let* ((u (bench-output--ms (lambda () (funcall update i))))
                 (s (bench-output--ms (lambda () (eas-svg-render (eas-view-scene view)))))
                 (k (bench-output--ms (lambda () (eas-output-encode (cdr s) w h 'kitty 1))))
                 (x (bench-output--ms (lambda () (eas-output-encode (cdr s) w h 'sixel nil "#ffffff")))))
            (when (and (> i 0) (not (equal (cdr s) last))) (cl-incf changed))
            (setq last (cdr s))
            (when (> i 0)
              (cl-loop for v in (list u s k x) for j from 0 do (cl-incf (aref sum j) (car v)))
              (cl-incf (aref kb 0) (/ (length (cdr k)) 1024.0))
              (cl-incf (aref kb 1) (/ (length (cdr x)) 1024.0)))))
        (let ((m (lambda (j) (/ (aref sum j) frames)))
              (share (/ changed (float frames))))
          (princ (format "| %s | %.1f | %.1f | %.1f | %.1f | %d/%d | %.1f | %.1f | %.0f | %.0f |\n" (car w-case)
                         (funcall m 0) (funcall m 1) (funcall m 2) (funcall m 3) changed frames
                         (+ (funcall m 0) (funcall m 1) (* share (funcall m 2)))
                         (+ (funcall m 0) (funcall m 1) (* share (funcall m 3)))
                         (/ (aref kb 0) frames) (/ (aref kb 1) frames))))
        (eas-view-close view)))))

(bench-output-run (let ((arg (car (last command-line-args-left))))
                    (if (and arg (string-match-p "\\`[0-9]+\\'" arg)) (string-to-number arg) 10)))
(setq command-line-args-left nil)

;;; bench-frame-output.el ends here

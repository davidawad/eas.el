;;; eas-text-frames-test.el --- incremental text frames equal full renders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.1.  A live text frame reuses the last one four ways: grid
;; snapshots, restored in place over the grid of two frames ago
;; (eas-render-cache.el), rows compared in place with the last grid
;; (`eas-text--same-row-p'), and the buffer patched against the lines it
;; was last given (`eas-mode-patch-lines').
;; Over random sequences of keyed pushes, timer ticks and pointer moves,
;; every frame must equal a render from scratch, text properties
;; included, both as the rendered lines and as the patched buffer.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-text)
(require 'eas-template)
(require 'eas-render-cache)
(require 'eas-mode-patch)

(defun eas-text-frames-test--book ()
  "A 40-level book with random sizes."
  (vconcat (cl-loop for side in '("bid" "ask")
                    append (cl-loop for k below 20
                                    for size = (1+ (random 99))
                                    collect (list :price (if (equal side "bid") (- 100.0 (* 0.5 k)) (+ 100.5 (* 0.5 k)))
                                                  :size size :side side :label (format "%d" size))))))

(defun eas-text-frames-test--ladder ()
  "A ladder spec: size bars by price, sizes printed beside them."
  `(:data (:values ,(eas-text-frames-test--book))
    :width 400 :height 300
    :encoding (:y (:field "price" :type "ordinal" :sort "descending")
               :x (:field "size" :type "quantitative"))
    :layer [(:mark "bar" :encoding (:color (:field "side" :type "nominal")))
            (:mark (:type "text" :align "left" :dx 3) :encoding (:text (:field "label")))]))

(defun eas-text-frames-test--check (view steps)
  "Check each frame of VIEW, driven by STEPS (functions of the frame number).
The cached lines equal an uncached render, and a buffer patched with
`eas-mode-patch-lines' frame after frame equals the frame inserted afresh."
  (eas-render-cache-clear)
  (with-temp-buffer
    (let ((patched (current-buffer)) (i 0))
      (dolist (step steps)
        (funcall step (cl-incf i))
        (let* ((scene (eas-view-scene view))
               (lines (eas-text-render-lines scene))
               (full (let ((eas-render-cache-enabled nil)) (eas-text-render scene)))
               (strip (propertize (format " frame %d" (% i 3)) 'face 'shadow)))
          (should (equal-including-properties (list i (mapconcat #'identity lines "\n")) (list i full)))
          (with-current-buffer patched
            (eas-mode-patch-lines (append lines (list strip)))
            ;; A strip update between redraws goes through the model.
            (when (= (% i 4) 1) (eas-mode-patch-last-line (propertize " moved" 'face 'shadow)))
            ;; The next frame diffs against the model, not the buffer.
            (should (eas-mode-patch--model-p)))
          (with-temp-buffer
            (insert full "\n" (if (= (% i 4) 1) (propertize " moved" 'face 'shadow) strip))
            (should (equal-including-properties (list i (buffer-string))
                                                (list i (with-current-buffer patched (buffer-string))))))))
      (should (> (plist-get eas-render-cache-stats :row-hits) 0))
      (should (> (or (plist-get eas-render-cache-stats :restored) 0) 0)))))

(ert-deftest eas-text-frames-random-pushes-equal-full-renders ()
  "Random keyed pushes to a ladder: each frame equals a full render."
  (let* ((eas-views (make-hash-table :test 'equal))
         (_ (random "eas-b2s.1 pushes"))
         (view (eas-view-open (eas-text-frames-test--ladder) :target 'text :size '(:cols 60 :rows 24))))
    (unwind-protect
        (eas-text-frames-test--check
         view (cl-loop repeat 12
                       collect (lambda (_)
                                 (let* ((book (eas-text-frames-test--book))
                                        (rows (seq-filter (lambda (_) (< (random 10) 3)) book)))
                                   (eas-dispatch view (list :type "push" :rows (vconcat rows) :key "price"))))))
      (eas-view-close view))))

(ert-deftest eas-text-frames-random-ticks-and-hovers-equal-full-renders ()
  "Clock ticks by random steps, mixed with pointer moves and leaves."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (_ (random "eas-b2s.1 ticks"))
         (view (eas-play-open "clock" :bindings (eas-template-example "clock")
                              :target 'text :size '(:cols 70 :rows 30)))
         (size (plist-get (eas-view-scene view) :size)))
    (unwind-protect
        (eas-text-frames-test--check
         view (cl-loop repeat 12
                       collect (lambda (_)
                                 (pcase (random 4)
                                   ((or 0 1) (cl-incf clock (1+ (random 20))) (eas-play-tick view))
                                   (2 (eas-dispatch view (list :type "pointermove"
                                                               :px (vector (random (max 1 (round (plist-get size :w))))
                                                                           (random (max 1 (round (plist-get size :h))))))))
                                   (_ (eas-dispatch view '(:type "pointerleave")))))))
      (eas-play-detach view)
      (eas-view-close view))))

(ert-deftest eas-text-frames-random-hovers-on-a-map-equal-full-renders ()
  "Random pointer moves over airport-connections."
  (let* ((_ (random "eas-b2s.1 hovers"))
         (view (eas-view-open "airport-connections" :bindings (eas-template-example "airport-connections")
                              :target 'text :size '(:cols 80 :rows 30)))
         (size (plist-get (eas-view-scene view) :size)))
    (unwind-protect
        (eas-text-frames-test--check
         view (cl-loop repeat 6
                       collect (lambda (_)
                                 (eas-dispatch view (list :type "pointermove"
                                                          :px (vector (random (round (plist-get size :w)))
                                                                      (random (round (plist-get size :h)))))))))
      (eas-view-close view))))

(ert-deftest eas-text-frames-patch-survives-outside-edits ()
  "The retained model notices text it did not write and diffs the buffer."
  (with-temp-buffer
    (let ((a (list (propertize "abc" 'face 'bold) "def" "ghi"))
          (b (list (propertize "abX" 'face 'bold) "def" (propertize "ghi" 'face 'italic))))
      (eas-mode-patch-lines a)
      (should (equal (buffer-string) "abc\ndef\nghi"))
      (goto-char (point-min)) (insert "zz")
      (eas-mode-patch-lines b)
      (should (equal-including-properties (buffer-string) (mapconcat #'identity b "\n")))
      ;; A line count change rewrites everything.
      (eas-mode-patch-lines '("one"))
      (should (equal (buffer-string) "one"))
      (should-not (progn (insert "!") (eas-mode-patch-last-line "two"))))))

(ert-deftest eas-text-frames-string-runs ()
  "Runs where two lines differ, by character or by properties."
  (should (equal (eas-mode-patch--string-runs "abcdef" "aXcdeY") '((5 . 6) (1 . 2))))
  (should (equal (eas-mode-patch--string-runs (concat "ab" (propertize "cd" 'face 'bold) "ef") "abcdef")
                 '((2 . 4))))
  (should (equal (eas-mode-patch--string-runs "abc" "abcdef") nil))
  (should (equal (eas-mode-patch--string-runs "abc" "xyz") '((0 . 3)))))

(ert-deftest eas-text-frames-ink-memo-matches-a-fresh-lookup ()
  "The per-background legible-color table answers as a fresh computation."
  (dolist (mode '(light dark))
    (dolist (color '("#ffff00" "#003f5c" "steelblue" "transparent" "no-such-color" "rgb(10, 200, 30)"))
      (let ((first (eas-text-ink-legible color mode)))
        (clrhash eas-text-ink--memo)
        (should (equal (list color mode first) (list color mode (eas-text-ink-legible color mode))))
        (should (equal (eas-text-ink-legible color mode) first))))))

(provide 'eas-text-frames-test)
;;; eas-text-frames-test.el ends here

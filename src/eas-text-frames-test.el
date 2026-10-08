;;; eas-text-frames-test.el --- incremental text frames equal full renders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.1, eas-b2s.7.  A live text frame reuses the last one: it
;; repaints only the rows its changed items touch, from a snapshot of
;; the grid before the first changed step, composes only those rows
;; (`eas-text--frame'), and the buffer is patched against the lines it
;; was last given (`eas-mode-patch-lines').  The update path's dirty
;; marks are a hint (`eas-render-cache-dirty'); frames are checked with
;; the true hint, with every mark listed, and with none.
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
(require 'eas-mode-strip)
(require 'eas-text-arc)
(require 'eas-arc)

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

(defun eas-text-frames-test--fresh (scene)
  "SCENE rendered from scratch: no cache, no memo, no pooled cell."
  (let ((eas-render-cache-enabled nil)
        (eas-text--props-memo (make-hash-table :test 'equal))
        (eas-text--reversed (make-hash-table :test 'eq :weakness 'key))
        (eas-text-arc--memo (make-hash-table :test 'equal))
        (eas-text-tile--derived (make-hash-table :test 'eq :weakness 'key))
        (eas-text--free-conses nil) (eas-text-tile--table nil)
        (eas-text-tile--pool (make-vector 1 nil)) (eas-text-tile--free 0))
    (eas-text-render scene)))

(defun eas-text-frames-test--hint (view i)
  "A dirty-marks hint for frame I of VIEW: true, every mark, or none."
  (pcase (% i 3)
    (0 (eas-view-dirty view))
    (1 (cl-loop for v across (plist-get (eas-view-scene view) :views)
                collect (cons (plist-get v :id) (cl-loop for m across (plist-get v :marks) collect (plist-get m :id)))))
    (_ nil)))

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
               ;; Two frames in three the buffer gets the rows the terminal
               ;; glue writes (`eas-text-render-rows', records left
               ;; uncomposed); the lines then come from the same frame.
               (rows (and (/= (% i 3) 2)
                          (let ((eas-render-cache-dirty (eas-text-frames-test--hint view i)))
                            (eas-text-render-rows scene))))
               (lines (let ((eas-render-cache-dirty (if rows nil (eas-text-frames-test--hint view i))))
                        (eas-text-render-lines scene)))
               (full (eas-text-frames-test--fresh scene))
               (strip (propertize (format " frame %d" (% i 3)) 'face 'shadow)))
          (should (equal-including-properties (list i (mapconcat #'identity lines "\n")) (list i full)))
          (with-current-buffer patched
            (eas-mode-patch-lines (append (or rows lines) (list strip)))
            ;; A strip update between redraws goes through the model.
            (when (= (% i 4) 1) (eas-mode-patch-last-line (propertize " moved" 'face 'shadow)))
            ;; The next frame diffs against the model, not the buffer.
            (should (eas-mode-patch--model-p)))
          (with-temp-buffer
            (insert full "\n" (if (= (% i 4) 1) (propertize " moved" 'face 'shadow) strip))
            (should (equal-including-properties (list i (buffer-string))
                                                (list i (with-current-buffer patched (buffer-string))))))))
      (should (> (plist-get eas-render-cache-stats :row-hits) 0))
      (should (> (or (plist-get eas-render-cache-stats :text-hits) 0) 0)))))

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

(ert-deftest eas-text-frames-random-pacman-ticks-equal-full-renders ()
  "Pacman ticks (pooled tile records) by random steps, with hovers."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (_ (random "eas-b2s.5 pacman"))
         (view (eas-play-open "pacman" :bindings (eas-template-example "pacman")
                              :target 'text :size '(:cols 80 :rows 30)))
         (size (plist-get (eas-view-scene view) :size)))
    (unwind-protect
        (eas-text-frames-test--check
         view (cl-loop repeat 10
                       collect (lambda (_)
                                 (if (< (random 4) 3)
                                     (progn (cl-incf clock (1+ (random 3))) (eas-play-tick view))
                                   (eas-dispatch view (list :type "pointermove"
                                                            :px (vector (random (max 1 (round (plist-get size :w))))
                                                                        (random (max 1 (round (plist-get size :h)))))))))))
      (eas-play-detach view)
      (eas-view-close view))))

(defun eas-text-frames-test--depth (book)
  "BOOK's rows with a cumulative :cum per side, a depth chart's data."
  (let ((acc (list (cons "bid" 0) (cons "ask" 0))))
    (vconcat (mapcar (lambda (r)
                       (let ((cell (assoc (plist-get r :side) acc)))
                         (setcdr cell (+ (cdr cell) (plist-get r :size)))
                         (append r (list :cum (cdr cell)))))
                     book))))

(ert-deftest eas-text-frames-random-depth-pushes-equal-full-renders ()
  "Random pushes to a stepped two-sided depth area (pooled slices)."
  (let* ((eas-views (make-hash-table :test 'equal))
         (_ (random "eas-b2s.5 depth"))
         (view (eas-view-open
                `(:data (:values ,(eas-text-frames-test--depth (eas-text-frames-test--book)))
                  :width 600 :height 300
                  :mark (:type "area" :interpolate "step-after" :fillOpacity 0.6 :line t)
                  :encoding (:x (:field "price" :type "quantitative" :scale (:domain [87 113]))
                             :y (:field "cum" :type "quantitative")
                             :color (:field "side" :type "nominal")))
                :target 'text :size '(:cols 60 :rows 24))))
    (unwind-protect
        (eas-text-frames-test--check
         view (cl-loop repeat 10
                       collect (lambda (_)
                                 (eas-dispatch view (list :type "push" :key "price"
                                                          :rows (eas-text-frames-test--depth
                                                                 (eas-text-frames-test--book)))))))
      (eas-view-close view))))

(ert-deftest eas-text-frames-strip-memo-equals-a-fresh-strip ()
  "The remembered strip equals one computed afresh, frame after frame."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (_ (random "eas-b2s.5 strip"))
         (view (eas-play-open "pacman" :bindings (eas-template-example "pacman")
                              :target 'text :size '(:cols 80 :rows 30)))
         (size (plist-get (eas-view-scene view) :size)))
    (unwind-protect
        (dotimes (i 12)
          (pcase (random 3)
            (0 (cl-incf clock (random 3)) (eas-play-tick view))
            (1 (eas-dispatch view (list :type "pointermove"
                                        :px (vector (random (max 1 (round (plist-get size :w))))
                                                    (random (max 1 (round (plist-get size :h))))))))
            (_ (eas-dispatch view '(:type "pointerleave"))))
          (let ((memo (eas-mode-strip-string view)))
            (should (eq memo (eas-mode-strip-string view)))
            (should (equal-including-properties
                     (list i memo)
                     (list i (let ((eas-mode-strip--memo (make-hash-table :test 'eq :weakness 'key)))
                               (eas-mode-strip-string view)))))))
      (eas-play-detach view)
      (eas-view-close view))))

(ert-deftest eas-text-frames-arc-memo-replays-the-same-dots ()
  "An arc's remembered dots are the dots it tests inside, in order."
  (let ((item '(:cx 40.0 :cy 30.0 :outerRadius 22.0 :innerRadius 6.0 :startAngle 0.3 :endAngle 2.4))
        (eas-text-arc--memo (make-hash-table :test 'equal))
        (direct nil) (replayed nil) (again nil))
    (let ((cx 40.0) (cy 30.0) (r 22.0) (sx 4.0) (sy 4.0))
      (cl-loop for dy from (floor (- cy r) sy) to (ceiling (+ cy r) sy)
               do (cl-loop for dx from (floor (- cx r) sx) to (ceiling (+ cx r) sx)
                           when (eas-arc-contains-p item (* (+ dx 0.5) sx) (* (+ dy 0.5) sy))
                           do (push (cons dx dy) direct))))
    (eas-text-arc-dots item 8 16 (lambda (dx dy) (push (cons dx dy) replayed)))
    (eas-text-arc-dots item 8 16 (lambda (dx dy) (push (cons dx dy) again)))
    (should direct)
    (should (equal direct replayed))
    (should (equal direct again))))

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

(ert-deftest eas-text-frames-second-pass-repaints-unguessed-rows ()
  "Frames stay exact when the guess of a changed item's rows fails.
With no guess, or one row only, rows the items move into are found
after the first pass and painted by the second."
  (dolist (guess (list (lambda (&rest _) nil) (lambda (&rest _) 0)))
    (cl-letf (((symbol-function 'eas-text--item-rows) guess))
      (dolist (test '(eas-text-frames-random-pushes-equal-full-renders
                      eas-text-frames-random-ticks-and-hovers-equal-full-renders
                      eas-text-frames-random-pacman-ticks-equal-full-renders))
        (funcall (ert-test-body (ert-get-test test)))))))

(ert-deftest eas-text-frames-repaint-only-changed-rows ()
  "A push that changes one level composes a few rows, not the canvas."
  (let* ((eas-views (make-hash-table :test 'equal))
         (book (eas-text-frames-test--book))
         (spec (eas-text-frames-test--ladder))
         (view (progn
                 ;; A fixed size domain: a push moves its own bar only.
                 (setf (plist-get spec :encoding)
                       (plist-put (copy-sequence (plist-get spec :encoding)) :x
                                  '(:field "size" :type "quantitative" :scale (:domain [0 100]))))
                 (setf (plist-get spec :data) (list :values book))
                 (eas-view-open spec :target 'text :size '(:cols 60 :rows 24)))))
    (unwind-protect
        (progn
          (eas-render-cache-clear)
          (eas-text-render-lines (eas-view-scene view))
          (let ((composed 0))
            (cl-letf* ((compose (symbol-function 'eas-text--compose-row))
                       ((symbol-function 'eas-text--compose-row)
                        (lambda (&rest args) (cl-incf composed) (apply compose args))))
              (dotimes (k 3)
                (let ((row (copy-sequence (aref book (* 7 k)))))
                  (eas-dispatch view (list :type "push" :key "price"
                                           :rows (vector (plist-put row :size (- 100 (plist-get row :size))))))
                  (setq composed 0)
                  (let ((lines (eas-text-render-lines (eas-view-scene view))))
                    (should (<= 1 composed 4))
                    (should (equal-including-properties (mapconcat #'identity lines "\n")
                                                        (eas-text-frames-test--fresh (eas-view-scene view)))))))))
          (should (> (plist-get eas-render-cache-stats :text-skipped) 0)))
      (eas-view-close view))))

(provide 'eas-text-frames-test)
;;; eas-text-frames-test.el ends here

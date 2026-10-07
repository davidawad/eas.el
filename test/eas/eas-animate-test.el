;;; eas-animate-test.el --- tests for scripts/eas-animate.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The animation script's planning and frame writing, in batch, and the
;; nearest hover it relies on: a map under the airports must not take
;; the hover.  Rasterizing and the GIF need rsvg-convert and ImageMagick
;; and are not tested here.

;;; Code:

(require 'ert)
(require 'eas-test-support)
(eval-and-compile (load (eas-test-file "scripts/eas-animate.el") nil t))

(ert-deftest eas-animate-ease-runs-from-0-to-1 ()
  "The cubic ease starts at 0, ends at 1 and is symmetric."
  (should (= (eas-animate-ease 0) 0))
  (should (= (eas-animate-ease 1) 1))
  (should (< (abs (- (eas-animate-ease 0.5) 0.5)) 1e-9))
  (should (< (abs (- (+ (eas-animate-ease 0.2) (eas-animate-ease 0.8)) 1)) 1e-9)))

(defun eas-animate-test-scene ()
  "A scene of three points named a, b and c, with sizes."
  (eas-compile (eas-resolve-spec
                "{\"data\": {\"values\": [{\"k\": \"a\", \"x\": 1, \"n\": 5},
                   {\"k\": \"b\", \"x\": 2, \"n\": 9}, {\"k\": \"c\", \"x\": 3, \"n\": 1}]},
                  \"mark\": \"point\",
                  \"params\": [{\"name\": \"h\", \"select\": {\"type\": \"point\", \"on\": \"pointermove\",
                                \"nearest\": true, \"fields\": [\"k\"]}}],
                  \"encoding\": {\"x\": {\"field\": \"x\", \"type\": \"quantitative\"},
                                 \"y\": {\"field\": \"n\", \"type\": \"quantitative\"}}}")
               :size '(300 . 200)))

(ert-deftest eas-animate-plan-glides-holds-and-visits ()
  "Glides ease onto their target, holds rest there, visits rank by a field."
  (let* ((scene (eas-animate-test-scene))
         (b (eas-animate-locate scene '(:mark "point" :key "k" :value "b")))
         (frames (eas-animate-plan scene (vector '(:pointer [0 0]) '(:hold 2)
                                                 (list :glide [10 20] :frames 4)
                                                 '(:visit (:mark "point" :key "k" :by "n" :ranks [1 3])
                                                          :glide 3 :hold 2)
                                                 '(:type "pointerleave")))))
    (should (equal (eas-animate-visit-values scene '(:mark "point" :key "k" :by "n" :ranks [1 2 3]))
                   '("b" "a" "c")))
    (should (= (length frames) (+ 2 4 (* 2 (+ 3 2)) 1)))
    (should (equal (plist-get (nth 0 frames) :pointer) [0 0]))
    (should-not (plist-get (nth 0 frames) :move))
    (should (equal (plist-get (nth 5 frames) :pointer) [10.0 20.0]))
    (should (plist-get (nth 5 frames) :move))
    (should (equal (plist-get (nth 8 frames) :pointer) b))
    (should (equal (plist-get (car (last frames)) :event) '(:type "pointerleave")))))

(ert-deftest eas-animate-frames-merge-and-hover ()
  "Frames dispatch pointer moves, merge identical SVGs and draw the pointer."
  (let* ((eas-views (make-hash-table :test 'equal))
         (view (eas-view-open (eas-animate-test-scene--spec)))
         (scene (eas-view-scene view))
         (b (eas-animate-locate scene '(:mark "point" :key "k" :value "b")))
         (dir (make-temp-file "eas-animate-test-" t)))
    (unwind-protect
        (let ((frames (eas-animate-frames
                       view (eas-animate-plan scene (vector (list :pointer [-5 -5]) '(:hold 3)
                                                            (list :glide b :frames 2) '(:hold 3)))
                       dir :fps 10 :tip-delay 2)))
          ;; Off-canvas holds merge; two moves (the first rest merges with
          ;; the second), then the tooltip once the pointer rests.
          (should (= (length frames) 4))
          (should (= (cdar frames) 30))
          (should (= (apply #'+ (mapcar #'cdr frames)) 80))
          (should (equal (plist-get (plist-get (eas-view-state view) :params) :h)
                         (list :type "point" :fields ["k"] :values [["b"]])))
          (with-temp-buffer
            (insert-file-contents (car (car (last frames))))
            (should (search-forward "M0 0V17" nil t))
            (should (search-forward "k: b" nil t))))
      (delete-directory dir t))))

(defun eas-animate-test-scene--spec ()
  "The spec of `eas-animate-test-scene', sized."
  (concat "{\"width\": 300, \"height\": 200, \"data\": {\"values\": [{\"k\": \"a\", \"x\": 1, \"n\": 5},"
          "{\"k\": \"b\", \"x\": 2, \"n\": 9}, {\"k\": \"c\", \"x\": 3, \"n\": 1}]},"
          "\"mark\": \"point\", \"params\": [{\"name\": \"h\", \"select\": {\"type\": \"point\","
          "\"on\": \"pointermove\", \"nearest\": true, \"fields\": [\"k\"]}}],"
          "\"encoding\": {\"x\": {\"field\": \"x\", \"type\": \"quantitative\"},"
          "\"y\": {\"field\": \"n\", \"type\": \"quantitative\"},"
          "\"tooltip\": {\"field\": \"k\", \"type\": \"nominal\"}}}"))

(ert-deftest eas-animate-text-frames-place-each-glyph ()
  "Text frames draw each run's glyphs at their cells, in their colors."
  (let ((svg (eas-animate-text-svg (concat "ab " (propertize "⠁⠂" 'face '(:foreground "#ff0000")))
                                   [7 14] "tip")))
    (should (string-match-p "x=\"0 7 14\"" svg))
    (should (string-match-p "x=\"21 28\" y=\"11.2\" fill=\"#ff0000\"" svg))
    (should (string-match-p ">tip</text>" svg))))

(defun eas-animate-test-airports (n)
  "The airport-connections example binding cut to its first N airports."
  (let* ((b (copy-sequence (eas-template-example "airport-connections")))
         (airports (seq-take (plist-get b :airports) n))
         (codes (mapcar (lambda (a) (plist-get a :iata)) airports)))
    (plist-put (plist-put b :airports (vconcat airports))
               :flights (vconcat (seq-filter (lambda (f) (and (member (plist-get f :origin) codes)
                                                              (member (plist-get f :destination) codes)))
                                             (plist-get b :flights))))))

(ert-deftest eas-animate-airport-hover-off-centre ()
  "Hovering inside a state, off an airport's centre, picks the nearest airport.
The states drawn under the airports carry no origin and must not win."
  (let* ((eas-views (make-hash-table :test 'equal))
         (view (eas-view-open "airport-connections" :bindings (eas-animate-test-airports 20)))
         (atl (eas-animate-locate (eas-view-scene view) '(:mark "circle" :key "origin" :value "ATL")))
         (px (vector (+ (aref atl 0) 3) (+ (aref atl 1) 4))))
    (eas-dispatch view (list :type "pointermove" :px px))
    (should (equal (plist-get (plist-get (eas-view-state view) :params) :hover)
                   (list :type "point" :fields ["origin"] :values [["ATL"]])))
    (should (> (length (plist-get (car (eas-animate--marks (eas-view-scene view) "rule")) :items)) 0))))

(provide 'eas-animate-test)
;;; eas-animate-test.el ends here

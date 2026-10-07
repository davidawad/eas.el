;;; eas-text-tile-test.el --- tests for rect tiles in text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-text-tile.el: rect marks drawn in painter's order, transparent
;; rects as hover only, stroked rects outlined on a shared lattice, and
;; fills kept behind what is drawn over them.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-text)
(require 'eas-text-check)

(defun eas-text-tile-test--scene (marks &optional w h)
  "A one-view scene W x H pixels (cells 7 x 14) holding MARKS."
  (let ((w (or w 140)) (h (or h 112)))
    (list :size (list :w w :h h :cell [7 14])
          :views (vector (list :id "v" :bounds (vector 0 0 w h) :marks (vconcat marks) :axes [])))))

(defun eas-text-tile-test--rect (x y w h fill &rest more)
  "A rect item at X Y, W x H pixels, filled FILL, with MORE properties."
  (append (list :x x :y y :w w :h h :orient "none" :fill fill :opacity 1) more))

(defun eas-text-tile-test--face-at (text col row key)
  "Face attribute KEY of the cell at COL ROW of rendered TEXT."
  (let* ((line-start (let ((p 0)) (dotimes (_ row) (setq p (1+ (string-search "\n" text p)))) p))
         (face (get-text-property (+ line-start col) 'face text)))
    (and (consp face) (keywordp (car face)) (plist-get face key))))

(defun eas-text-tile-test--char-at (text col row)
  "The character at COL ROW of rendered TEXT."
  (aref (nth row (split-string text "\n")) col))

(ert-deftest eas-text-tile-nested-rects-show-in-their-own-color ()
  ;; A child drawn after its parent takes its cells, as SVG paints it.
  (let* ((eas-text-background-mode 'light)
         (text (eas-text-render
                (eas-text-tile-test--scene
                 (list (list :id "m" :mark "rect"
                             :items (vector (eas-text-tile-test--rect 0 0 140 112 "#3182bd")
                                            (eas-text-tile-test--rect 70 56 70 56 "#e6550d"))))))))
    (should (equal (eas-text-tile-test--face-at text 1 1 :foreground) "#3182bd"))
    (should (equal (eas-text-tile-test--face-at text 15 6 :foreground) "#e6550d"))))

(ert-deftest eas-text-tile-adjacent-rects-keep-their-boundary ()
  ;; Two stroked rects of one color side by side, as a heatmap's cells:
  ;; a line runs between them, joined where it meets the top edge.
  (let* ((eas-text-background-mode 'light)
         (item (lambda (x) (eas-text-tile-test--rect x 0 70 112 "#4c78a8" :stroke "#fcfcfb" :strokeWidth 1)))
         (text (eas-text-render
                (eas-text-tile-test--scene
                 (list (list :id "m" :mark "rect" :items (vector (funcall item 0) (funcall item 70))))))))
    (should (eq (eas-text-tile-test--char-at text 10 3) ?│))
    (should (eq (eas-text-tile-test--char-at text 10 0) ?┬))
    (should (eq (eas-text-tile-test--char-at text 5 3) ?█))
    ;; The line sits on the fill.
    (should (equal (eas-text-tile-test--face-at text 10 3 :background) "#4c78a8"))))

(ert-deftest eas-text-tile-transparent-rects-draw-only-edges-and-hover ()
  ;; Treemap leaves: see-through, stroked; the group's fill shows through.
  (let* ((eas-text-background-mode 'light)
         (text (eas-text-render
                (eas-text-tile-test--scene
                 (list (list :id "g" :mark "rect" :items (vector (eas-text-tile-test--rect 0 0 140 112 "#e6550d")))
                       (list :id "leaf" :mark "rect"
                             :items (vector (eas-text-tile-test--rect 0 0 70 112 "transparent" :stroke "#fff" :strokeWidth 1
                                                                    :datum '(:name "a"))
                                            (eas-text-tile-test--rect 70 0 70 112 "transparent" :stroke "#fff" :strokeWidth 1
                                                                    :datum '(:name "b")))))))))
    (should (eq (eas-text-tile-test--char-at text 5 3) ?█))
    (should (equal (eas-text-tile-test--face-at text 5 3 :foreground) "#e6550d"))
    (should (eq (eas-text-tile-test--char-at text 10 3) ?│))
    (should (equal (get-text-property (+ 15 (* 3 21)) 'eas-datum text) '(:name "b")))
    (should (equal (get-text-property (+ 5 (* 3 21)) 'eas-mark text) "leaf"))))

(ert-deftest eas-text-tile-labels-sit-on-their-tile ()
  ;; A label over a fill keeps the fill behind it, legible on it.
  (let* ((eas-text-background-mode 'light)
         (text (eas-text-render
                (eas-text-tile-test--scene
                 (list (list :id "m" :mark "rect" :items (vector (eas-text-tile-test--rect 0 0 140 112 "#3182bd")))
                       (list :id "t" :mark "text"
                             :items (vector (list :x 70 :y 56 :text "ab cd" :align "center" :fill "#3182bd" :opacity 1))))))))
    (should (string-match-p "ab cd" text))
    (let ((col (string-search "ab cd" (nth 4 (split-string text "\n")))))
      (should (equal (eas-text-tile-test--face-at text (+ col 2) 4 :background) "#3182bd"))
      (should-not (equal (eas-text-tile-test--face-at text col 4 :foreground) "#3182bd")))))

(ert-deftest eas-text-tile-paper-strokes-need-a-fill-behind ()
  ;; A stroke the color of the paper over nothing shows nothing, as in SVG.
  (let ((text (eas-text-render
               (eas-text-tile-test--scene
                (list (list :id "m" :mark "rect"
                            :items (vector (eas-text-tile-test--rect 0 0 70 112 "transparent" :stroke "#fcfcfb"))))))))
    (should (string-empty-p (string-trim text)))))

;; Pale fills, a paper-white stroke and a label on the tile: each glyph
;; reads at 3:1 against what is under it, on light and dark paper.
(ert-deftest eas-text-tile-glyphs-pass-the-contrast-check ()
  (let ((scene (eas-text-tile-test--scene
                (list (list :id "m" :mark "rect"
                            :items (vector (eas-text-tile-test--rect 0 0 70 112 "#72b7b2" :stroke "#fcfcfb")
                                           (eas-text-tile-test--rect 70 0 70 112 "#f58518" :stroke "#fcfcfb")))
                      (list :id "t" :mark "text"
                            :items (vector (list :x 35 :y 56 :text "ab" :align "center" :fill "#fff" :opacity 1)))))))
    (should-not (eas-text-check-contrast scene 'light))
    (should-not (eas-text-check-contrast scene 'dark))))

(provide 'eas-text-tile-test)
;;; eas-text-tile-test.el ends here

;;; eas-render-cache-test.el --- cached renders draw what uncached ones do -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.19: animated vega templates (a clock and pacman ticking, the
;; airport-connections map hovered) render each frame with and without
;; `eas-render-cache-enabled', SVG and text.  The outputs are the same,
;; text properties included, and the cache is actually used.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-template)
(require 'eas-render-cache)

(defun eas-render-cache-test--size (target)
  "The canvas size the tests open TARGET views at."
  (and (eq target 'text) '(:cols 100 :rows 40)))

(defun eas-render-cache-test--ticks (template target n)
  "The scenes of N clock ticks of vega TEMPLATE on TARGET."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (view (eas-play-open template :bindings (eas-template-example template)
                              :target target :size (eas-render-cache-test--size target)))
         scenes)
    (unwind-protect
        (dotimes (_ n)
          (cl-incf clock 1.0)
          (eas-play-tick view)
          (push (eas-view-scene view) scenes))
      (eas-play-detach view)
      (eas-view-close view))
    (nreverse scenes)))

(defun eas-render-cache-test--hovers (template target n)
  "The scenes of N pointer moves over vega TEMPLATE's points on TARGET.
The pointer visits the first points of the first point-like mark, so
the hover state changes from frame to frame."
  (let* ((view (eas-view-open template :bindings (eas-template-example template)
                              :target target :size (eas-render-cache-test--size target)))
         (items (seq-some (lambda (m) (and (member (plist-get m :mark) '("circle" "point" "symbol"))
                                           (> (length (plist-get m :items)) 0)
                                           (plist-get m :items)))
                          (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))
         scenes)
    (unwind-protect
        (dotimes (i n)
          (let ((item (aref items (% (* 7 i) (length items)))))
            (eas-dispatch view (list :type "pointermove"
                                     :px (vector (plist-get item :x) (plist-get item :y)))))
          (push (eas-view-scene view) scenes))
      (eas-view-close view))
    (nreverse scenes)))

(defun eas-render-cache-test--check (scenes target)
  "Render SCENES on TARGET cached and not; they agree.  Return the stats."
  (let* ((render (if (eq target 'svg) #'eas-svg-render #'eas-text-render))
         (plain (let ((eas-render-cache-enabled nil)) (mapcar render scenes))))
    (eas-render-cache-clear)
    (let ((cached (let ((eas-render-cache-enabled t)) (mapcar render scenes))))
      (cl-loop for a in plain for b in cached for i from 0
               do (should (equal (list i a) (list i b)))
               do (should (equal-including-properties a b))))
    (prog1 eas-render-cache-stats (eas-render-cache-clear))))

(ert-deftest eas-render-cache-ticks-draw-as-uncached ()
  "Clock and pacman ticks render alike cached, reusing the static parts."
  (dolist (template '("clock" "pacman"))
    (let ((svg (eas-render-cache-test--check (eas-render-cache-test--ticks template 'svg 6) 'svg))
          (text (eas-render-cache-test--check (eas-render-cache-test--ticks template 'text 6) 'text)))
      (should (> (plist-get svg :svg-hits) 0))
      (should (> (plist-get text :text-hits) 0))
      (should (> (plist-get text :text-skipped) 0)))))

(ert-deftest eas-render-cache-hovers-draw-as-uncached ()
  "Airport-connections hovers render alike cached; the map is reused."
  (let ((svg (eas-render-cache-test--check
              (eas-render-cache-test--hovers "airport-connections" 'svg 5) 'svg))
        (text (eas-render-cache-test--check
               (eas-render-cache-test--hovers "airport-connections" 'text 5) 'text)))
    (should (> (plist-get svg :svg-hits) 0))
    (should (> (plist-get text :text-hits) 0))))

(ert-deftest eas-render-cache-disabled-caches-nothing ()
  "With `eas-render-cache-enabled' nil every frame is drawn afresh."
  (let ((scenes (eas-render-cache-test--ticks "clock" 'text 3))
        (eas-render-cache-enabled nil))
    (eas-render-cache-clear)
    (mapc #'eas-text-render scenes)
    (mapc #'eas-svg-render scenes)
    (should (equal eas-render-cache-stats
                   '(:svg-hits 0 :svg-misses 0 :text-hits 0 :text-skipped 0 :row-hits 0)))))

(defvar eas-paint--svg-defs)

(ert-deftest eas-render-cache-fragment-replays-its-defs ()
  "A cached fragment records its gradient definitions again, deduplicated."
  (eas-render-cache-clear)
  (let* ((def (lambda (id) (dom-node 'linearGradient `((id . ,id)))))
         (nodes (lambda () (setq eas-paint--svg-defs (append eas-paint--svg-defs (list (funcall def "g1"))))
                  (list (dom-node 'rect '((fill . "url(#g1)")))))))
    (dotimes (_ 2)
      (let ((eas-paint--svg-defs (list (funcall def "g0"))))
        (should (equal (eas-render-cache-svg-fragment '(:mark "rect" :id "k") nodes)
                       "<rect fill=\"url(#g1)\"></rect>"))
        (should (equal (mapcar (lambda (d) (dom-attr d 'id)) eas-paint--svg-defs) '("g0" "g1")))))
    (should (equal (plist-get eas-render-cache-stats :svg-hits) 1)))
  (eas-render-cache-clear))

(ert-deftest eas-render-cache-paint-restarts-after-the-common-prefix ()
  "Only the steps after the unchanged prefix run once a snapshot exists."
  (eas-render-cache-clear)
  (let* ((ran nil)
         (step (lambda (key) (cons key (lambda (state) (push key ran) (aset state 0 (cons key (aref state 0)))))))
         (paint (lambda (keys)
                  (setq ran nil)
                  (aref (eas-render-cache-paint '(test) (vector nil) (mapcar step keys)
                                                (lambda (fn state) (funcall fn state)))
                        0))))
    (should (equal (funcall paint '(a b c1)) '(c1 b a)))
    (should (equal (funcall paint '(a b c2)) '(c2 b a)))
    (should (equal ran '(c2 b a)))
    (should (equal (funcall paint '(a b c3)) '(c3 b a)))
    (should (equal ran '(c3)))
    (should (equal (funcall paint '(a x c3)) '(c3 x a)))
    (should (equal ran '(c3 x a))))
  (eas-render-cache-clear))

(provide 'eas-render-cache-test)
;;; eas-render-cache-test.el ends here

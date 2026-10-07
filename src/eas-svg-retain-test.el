;;; eas-svg-retain-test.el --- retained SVG frames equal fresh renders -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.2: over random sequences of pushes, timer ticks, slider steps
;; and hovers, every SVG frame drawn with retained fragments (items,
;; axes, legends) is byte for byte the SVG `svg-print' makes of the
;; plain DOM, and the retained hot spots equal fresh ones.  The printer
;; matches `svg-print' on its own.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-svg)
(require 'eas-svg-retain)
(require 'eas-template)
(require 'eas-live-test)
(require 'eas-mode)
(require 'eas-paint)
(require 'eas-action-callback)

(defun eas-svg-retain-test--reference (scene)
  "SCENE drawn as SVG by `svg-print' from the plain DOM, nothing retained."
  (let ((eas-render-cache-enabled nil))
    (with-temp-buffer
      (svg-print (eas-svg-dom scene))
      (buffer-string))))

(defun eas-svg-retain-test--check (scene)
  "SCENE's retained SVG and hot spots equal the fresh ones."
  (let ((svg (eas-svg-render scene)))
    (should (equal-including-properties svg (eas-svg-retain-test--reference scene)))
    (should (equal (eas-svg-hot-spots scene)
                   (let ((eas-render-cache-enabled nil)) (eas-svg-hot-spots scene))))
    (should (equal (eas-action-callback-areas scene)
                   (let ((eas-render-cache-enabled nil)) (eas-action-callback-areas scene))))))

(ert-deftest eas-svg-retain-print-is-svg-print ()
  "The printer writes what `svg-print' does: raw strings, no colon attributes."
  (let ((dom (dom-node 'svg '((width . "10") (:skip . "x") (n . 3))
                       (dom-node 'g nil "<raw/>" (dom-node 'text '((x . "1")) "a &amp; b"))
                       (dom-node 'rect '((fill . "url(#g)"))))))
    (should (equal (with-temp-buffer (eas-svg-retain-print dom) (buffer-string))
                   (with-temp-buffer (svg-print dom) (buffer-string))))
    (should (equal (eas-svg-retain-string (list dom "<x/>" dom))
                   (with-temp-buffer (mapc #'svg-print (list dom "<x/>" dom)) (buffer-string))))))

(ert-deftest eas-svg-retain-escape-and-gradients-stay-exact ()
  "Escaped text and gradient fills draw the same retained, frame after frame."
  (let* ((spec (list :width 200 :height 100
                     :mark (list :type "bar"
                                 :color (list :x1 0 :y1 0 :x2 1 :y2 0 :gradient "linear"
                                              :stops (vector (list :offset 0 :color "red")
                                                             (list :offset 1 :color "blue"))))
                     :encoding (list :x (list :field "k" :type "nominal")
                                     :y (list :field "v" :type "quantitative"))))
         (layer (list :layer (vector spec (plist-put (copy-sequence spec) :mark
                                                     (list :type "text" :dy -5)))
                      :width 200 :height 100)))
    (eas-svg-retain-clear)
    (dotimes (i 4)
      (let ((rows (vector (list :k "a & <b>" :v (+ 1 i)) (list :k "\"q\"" :v 3))))
        (eas-svg-retain-test--check (eas-compile (plist-put (copy-sequence spec) :data (list :values rows))))
        (eas-svg-retain-test--check
         (eas-compile (plist-put (copy-sequence layer) :data (list :values rows))))))
    (should (> (+ (plist-get eas-svg-retain-stats :item-hits) (plist-get eas-svg-retain-stats :item-reused)) 0))))

(ert-deftest eas-svg-retain-versions-hash-apart ()
  "Versions of an item that differ past what `sxhash-equal' sees hash apart.
A bar's width (its 8th element) and a series' 20th point, each over 50
versions, spread over (almost) 50 hashes; for the series `sxhash-equal'
gives one.  A stream's lookups do not walk a chain."
  (let ((bars (cl-loop for w below 50
                       collect (eas-svg-retain--hash (list "bar" :datum 3 :x 138 :y 20.5 :w w :h 4))))
        (series (cl-loop for v below 50
                         collect (eas-svg-retain--hash
                                  (list "area" :datum nil
                                        :points (vconcat (make-list 19 [1 2]) (list (vector 3 v))))))))
    (should (= 1 (length (delete-dups
                          (cl-loop for v below 50
                                   collect (sxhash-equal
                                            (list "area" :datum nil
                                                  :points (vconcat (make-list 19 [1 2])
                                                                   (list (vector 3 v))))))))))
    (should (>= (length (delete-dups bars)) 45))
    (should (>= (length (delete-dups series)) 45))))

(ert-deftest eas-svg-retain-a-stream-keeps-one-version-per-part ()
  "Pushes replace a part's retained version: the slots do not grow."
  (eas-live-test--with
    (eas-svg-retain-clear)
    (let ((view (eas-view-open (eas-json-encode eas-live-test--ladder) :id "r-slots" :size '(400 . 300)
                               :rows (eas-live-test--book 1 25)))
          (counts nil))
      (unwind-protect
          (dotimes (i 60)
            (eas-dispatch view (list :type "push" :rows (eas-live-test--book (+ 2 i) 25) :window 50))
            (eas-svg-image (eas-view-scene view))
            (push (hash-table-count eas-svg-retain--parts) counts))
        (eas-view-close view))
      (should (= (car counts) (nth 50 counts))))))

(ert-deftest eas-svg-retain-gradient-ids-hold-across-sessions ()
  "A gradient's id is the same in Emacs sessions that loaded other code."
  (let* ((form "(progn (require 'eas-paint) (princ (eas-paint--id '(:gradient \"linear\" :stops [(:offset 0 :color \"red\")]))))")
         (emacs (expand-file-name invocation-name invocation-directory))
         (dir (file-name-directory (locate-library "eas-paint")))
         (run (lambda (&rest pre)
                (with-temp-buffer
                  (apply #'call-process emacs nil t nil
                         (append (list "-Q" "--batch" "-L" dir) pre (list "--eval" form)))
                  (buffer-string)))))
    (should (string-prefix-p "paint-" (funcall run)))
    (should (equal (funcall run) (funcall run "--eval" "(require 'ert)" "--eval" "(intern \"eas-x\")")))
    (should (equal (funcall run) (eas-paint--id '(:gradient "linear" :stops [(:offset 0 :color "red")]))))))

(defun eas-svg-retain-test--random (seed)
  "The next state of the generator after SEED."
  (% (+ (* seed 1103515245) 12345) 2147483648))

(ert-deftest eas-svg-retain-direct-printers-print-nodes ()
  "Every direct printer writes what `eas-svg--item''s node prints to.
Random rects, rules, symbols and texts, every optional property drawn
in or left out, nil when the item has no printer."
  (let* ((seed 7) (printed 0)
         (pick (lambda (&rest choices)
                 (setq seed (eas-svg-retain-test--random seed))
                 (nth (% (/ seed 7) (length choices)) choices)))
         (num (lambda () (funcall pick 0 3 -2 12.5 0.125 100.005 -0.0 7.999 1e-3 1.0))))
    (dotimes (_ 3000)
      (let* ((kind (funcall pick "bar" "rect" "brush" "rule" "tick" "point" "circle" "square" "text"))
             (item (append
                    (list :x (funcall num) :y (funcall num) :w (funcall num) :h (funcall num)
                          :x1 (funcall num) :y1 (funcall num) :x2 (funcall num) :y2 (funcall num)
                          :size (funcall pick 30 64 0 12.25 -4)
                          :text (funcall pick "a" "" "a & <b>" "\"q\"" "two\nlines" 42)
                          :fontSize (funcall pick 10 11 12.5))
                    (cl-loop for (k . vs) in
                             '((:fill "red" "none" nil "#a&b") (:stroke "none" "black" nil)
                               (:opacity 1 0.5 nil 0) (:strokeWidth 1 2.5 nil)
                               (:fillOpacity 0.3 nil) (:strokeOpacity 0.7 nil "x")
                               (:strokeDash [2 1.5] nil "3,1") (:strokeCap "round" nil)
                               (:strokeJoin "bevel" nil) (:corners nil nil [1 2 3 4])
                               (:shape nil "circle" "square" "triangle-up" "diamond" "M0,0L1,1Z" "cross")
                               (:angle nil nil 30 0) (:align "left" "right" nil) (:baseline "top" "middle" "bottom" nil)
                               (:fontWeight nil "bold" 700) (:font nil "monospace" "Fira")
                               (:fontStyle nil "italic") (:gradient nil nil t))
                             for v = (apply pick vs)
                             when v append (list k v))))
             (direct (eas-svg--item-string kind item)))
        (when direct
          (cl-incf printed)
          (should (equal direct (eas-svg-retain-string
                                 (list (eas-svg--item (list :mark kind) item))))))))
    (should (> printed 1500))))

(ert-deftest eas-svg-retain-random-frames-equal-fresh-renders ()
  "Random push, tick, slider and hover sequences draw as fresh renders do."
  (eas-live-test--with
    (eas-svg-retain-clear)
    (let* ((seed 11)
           (ladder (eas-view-open (eas-json-encode eas-live-test--ladder) :id "r-ladder" :size '(400 . 300)
                                  :rows (eas-live-test--book 1 25)))
           (points (eas-view-open (eas-json-encode
                                   (list :width 300 :height 200 :mark "point"
                                         :encoding (list :x (list :field "price" :type "quantitative"
                                                                  :scale (list :zero :false))
                                                         :y (list :field "size" :type "quantitative")
                                                         :color (list :field "side" :type "nominal")
                                                         :tooltip (list :field "size" :type "quantitative"))))
                                  :id "r-points" :size '(300 . 200) :rows (eas-live-test--book 2 10)))
           (ticker (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg))
           (pacman (eas-play-open "pacman" :bindings (eas-template-example "pacman") :target 'svg))
           (airport (eas-view-open "airport-connections"
                                   :bindings (eas-template-example "airport-connections") :target 'svg))
           (pi (eas-view-open "pi-monte-carlo"
                              :bindings (append (list :points 60 :max_points 200)
                                                (eas-template-example "pi-monte-carlo"))
                              :target 'svg))
           (views (list ladder points ticker pacman airport pi)))
      (unwind-protect
          (dotimes (_ 60)
            (setq seed (eas-svg-retain-test--random seed))
            (let ((view (nth (% seed (length views)) views)))
              (cond
               ((memq view (list ladder points))
                (let ((rows (copy-sequence (plist-get (eas-view-data view) :rows)))
                      (fresh (eas-live-test--book seed 25)))
                  (dotimes (_ (% seed 6))
                    (setq seed (eas-svg-retain-test--random seed))
                    (let ((k (% seed (length rows)))) (aset rows k (aref fresh k))))
                  (eas-dispatch view (list :type "push" :rows rows :window 50))))
               ((memq view (list ticker pacman))
                (setq clock (+ clock 1.0))
                (eas-play-tick view))
               ((eq view pi)
                (eas-dispatch view (list :type "param" :param "num_points" :value (+ 60 (% seed 100)))))
               (t (let ((size (plist-get (eas-view-scene view) :size)))
                    (eas-dispatch view (list :type "pointermove"
                                             :px (vector (* (% seed 97) 0.01 (plist-get size :w))
                                                         (* (% (/ seed 97) 89) 0.0112 (plist-get size :h))))))))
              ;; The update path's dirty marks are a hint: true, every
              ;; mark listed, or none, the output is the same.
              (let ((eas-render-cache-dirty
                     (pcase (% seed 3)
                       (0 (eas-view-dirty view))
                       (1 (cl-loop for v across (plist-get (eas-view-scene view) :views)
                                   collect (cons (plist-get v :id)
                                                 (cl-loop for m across (plist-get v :marks) collect (plist-get m :id)))))
                       (_ nil))))
                (eas-svg-retain-test--check (eas-view-scene view)))))
        (dolist (v (list ticker pacman)) (eas-play-detach v))
        (mapc #'eas-view-close views)))
    (should (> (plist-get eas-svg-retain-stats :item-reused) 0))
    (should (> (plist-get eas-svg-retain-stats :part-hits) 0))))

(ert-deftest eas-svg-retain-redraw-keeps-an-unchanged-image ()
  "A GUI redraw of the same picture keeps the image; hot-spot ids stay bound."
  (eas-live-test--with
    (let* ((view (eas-view-open (eas-json-encode eas-live-test--ladder) :id "r-redraw" :size '(400 . 300)
                                :rows (eas-live-test--book 1 25)))
           (image (lambda () (get-text-property (point-min) 'display))))
      (unwind-protect
          (with-temp-buffer
            (eas-view-mode)
            (setq eas-mode--view view)
            (eas-mode-redraw)
            (let ((first (funcall image)) (text (buffer-string)))
              (eas-mode-redraw)
              (should (eq (funcall image) first))
              (should (equal-including-properties (buffer-string) text))
              ;; The mode run again (as `eas-show' does) resets the local
              ;; map: a redraw of the same image binds its hot spots again.
              (eas-view-mode)
              (setq eas-mode--view view)
              (eas-mode-redraw)
              (should (eq (funcall image) first))
              (dolist (area (plist-get (cdr first) :map))
                (should (eq (lookup-key (current-local-map) (vector (nth 1 area) 'mouse-1))
                            (lookup-key eas-view-mode-map [mouse-1]))))
              (let ((rows (copy-sequence (plist-get (eas-view-data view) :rows))))
                (aset rows 0 (plist-put (copy-sequence (aref rows 0)) :size 39))
                (eas-dispatch view (list :type "push" :rows rows :window 50)))
              (eas-mode-redraw)
              (should-not (eq (funcall image) first))
              (should (equal (plist-get (cdr (funcall image)) :data)
                             (eas-svg-retain-test--reference (eas-view-scene view))))
              (dolist (area (plist-get (cdr (funcall image)) :map))
                (should (eq (lookup-key (current-local-map) (vector (nth 1 area) 'mouse-1))
                            (lookup-key eas-view-mode-map [mouse-1]))))))
        (eas-view-close view)))))

(provide 'eas-svg-retain-test)
;;; eas-svg-retain-test.el ends here

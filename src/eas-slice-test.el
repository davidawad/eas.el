;;; eas-slice-test.el --- tiled GUI frames equal a full render -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-e3s.  A GUI chart drawn as tiles (eas-slice.el) re-rasterizes
;; only the tiles a frame changed.  These tests drive live views
;; (clock and pacman ticks, ladder pushes, airport hovers) through
;; `eas-mode-redraw' in batch and check that:
;;
;;   - every tile on display, kept or redrawn, rasterizes (rsvg-convert)
;;     to exactly the pixels of the same region of a full render of the
;;     current scene: a tile kept from an earlier frame was not touched
;;     by the frames since;
;;   - a frame re-rasterizes fewer tiles than it shows;
;;   - hot spots move into tile pixels and events map back to the chart.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-mode)
(require 'eas-slice)
(require 'eas-png)
(require 'eas-chart)
(require 'eas-template)
(require 'eas-test-support)

(defvar eas-slice-test--ladder
  '(:data (:values [])
	  :encoding (:y (:field "price" :type "ordinal" :sort "descending"))
	  :layer [(:mark "bar" :encoding (:x (:field "size" :type "quantitative" :scale (:domain [0 40]))
					     :color (:field "side" :type "nominal")))
		  (:mark (:type "text" :align "left")
			 :encoding (:x (:field "size" :type "quantitative") :text (:field "size" :type "quantitative")))])
  "A price ladder: bars and size labels, one band per price.")

(defun eas-slice-test--book (i)
  "Rows of a 2x12-level book at step I: a few sizes move each step."
  (vconcat
   (cl-loop for k from 0 below 24
            collect (list :price (+ 100 k) :side (if (< k 12) "bid" "ask")
                          :size (+ 5 (% (+ (* k 7) (if (= (% k 5) (% i 5)) (* 3 i) 0)) 30))))))

(defmacro eas-slice-test--with (&rest body)
  "Run BODY with fresh registries, a stepped clock `clock' and tiles on."
  (declare (indent 0) (debug t))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-plays (make-hash-table :test 'equal))
         (eas-slice-tiles t) (eas-slice-size 160)
         (clock 1.7e9))
     (let ((eas-play-clock (lambda () clock)))
       (eas-slice-stats-reset)
       ,@body)))

;;; Pixels

(defun eas-slice-test--rsvg ()
  "The rsvg-convert program, or skip the test saying why."
  (or (executable-find eas-chart-rsvg-program)
      (eas-test-skip (format "%s not on PATH; it rasterizes tiles and the full chart to compare their pixels"
                             eas-chart-rsvg-program))))

(defun eas-slice-test--raster (svg)
  "SVG rasterized by rsvg-convert, as `eas-png-read' decodes it."
  (let ((png (make-temp-file "eas-slice" nil ".png")))
    (unwind-protect (progn (eas-chart-rasterize svg png) (eas-png-read png))
      (delete-file png))))

(defun eas-slice-test--crop (img x y w h)
  "The RGBA bytes of IMG's region X Y W H."
  (let ((stride (* 4 (plist-get img :w))) (px (plist-get img :rgba)))
    (apply #'concat (cl-loop for r from y below (+ y h)
                             collect (substring px (+ (* r stride) (* 4 x)) (+ (* r stride) (* 4 (+ x w))))))))

(defvar eas-slice-test--worst 0
  "The largest share of pixels a tile set differed from a full render in.")

(defun eas-slice-test--differing (a b)
  "Pixels (4 bytes each) that differ between RGBA strings A and B."
  (let ((n 0) (i 0) (len (length a)))
    (while (< i len)
      (unless (and (= (aref a i) (aref b i)) (= (aref a (+ i 1)) (aref b (+ i 1)))
                   (= (aref a (+ i 2)) (aref b (+ i 2))) (= (aref a (+ i 3)) (aref b (+ i 3))))
        (setq n (1+ n)))
      (setq i (+ i 4)))
    n))

(defun eas-slice-test--check-pixels (frame scene)
  "Check every tile of FRAME against SCENE, drawn afresh.
A tile, kept from an earlier frame or not, must rasterize to exactly
the pixels of the same tile cut from SCENE now.  The tiles together
must also match a full render of SCENE but for the few edge pixels
librsvg anti-aliases differently once the picture is translated."
  (let* ((full (eas-slice-test--raster (eas-svg-render scene)))
         (parts (eas-slice--split (eas-svg-render scene)))
         (xs (eas-slice-frame-xs frame)) (ys (eas-slice-frame-ys frame)) (nx (1- (length xs)))
         (segs (eas-slice-frame-segs frame)) (differing 0))
    (should (= (plist-get full :w) (aref xs nx)))
    (should (= (plist-get full :h) (aref ys (1- (length ys)))))
    (seq-do-indexed
     (lambda (tile i)
       (let* ((seg (aref segs i))
              (x (aref xs (aref seg 2))) (y (aref ys (aref seg 0)))
              (w (- (aref xs (aref seg 3)) x)) (h (- (aref ys (aref seg 1)) y))
              (data (plist-get (cdr tile) :data))
              (fresh (eas-slice-tile-svg parts x y w h))
              (img (eas-slice-test--raster data)))
         (should (equal (list (plist-get img :w) (plist-get img :h)) (list w h)))
         (unless (or (string= data fresh)
                     (string= (plist-get img :rgba) (plist-get (eas-slice-test--raster fresh) :rgba)))
           (ert-fail (list "a kept tile is not what the scene draws now" :tile i :at (list x y w h))))
         (setq differing (+ differing (eas-slice-test--differing (plist-get img :rgba)
                                                                 (eas-slice-test--crop full x y w h))))))
     (eas-slice-frame-tiles frame))
    (let ((share (/ (float differing) (* (plist-get full :w) (plist-get full :h)))))
      (setq eas-slice-test--worst (max eas-slice-test--worst share))
      (should (< share 0.002)))))

;;; Driving a view

(defun eas-slice-test--drive (view size update frames check)
  "Show VIEW at SIZE in a GUI-like buffer and run FRAMES of UPDATE.
UPDATE gets VIEW and the frame number.  On frames in CHECK (a list),
the tiles are compared with a full render.  Return per frame the
images rasterized again and the share of the chart they cover, as
\(IMAGES . SHARE)."
  (let ((rastered nil))
    (with-temp-buffer
      (eas-view-mode)
      (setq eas-mode--view view)
      (eas-view-resize view size 'svg)
      (eas-mode-redraw)
      (dotimes (i frames)
        (funcall update view i)
        (let ((before (plist-get eas-slice-stats :rastered)) (px (plist-get eas-slice-stats :pixels))
              (chart (plist-get eas-slice-stats :chart-pixels)))
          (eas-mode-redraw)
          (push (cons (- (plist-get eas-slice-stats :rastered) before)
                      (/ (float (- (plist-get eas-slice-stats :pixels) px))
                         (- (plist-get eas-slice-stats :chart-pixels) chart)))
                rastered))
        (when (memq i check)
          (eas-slice-test--check-pixels eas-slice--frame (eas-view-scene view)))))
    (nreverse rastered)))

(defun eas-slice-test--tiles (size)
  "Tiles of a chart of SIZE (W . H) under the current settings."
  (* (1- (length (eas-slice--edges (car size) eas-slice-size 1)))
     (1- (length (eas-slice--edges (cdr size) eas-slice-size eas-slice-min-height)))))

;;; Tests

(ert-deftest eas-slice-edges-cut-evenly ()
  (should (equal (eas-slice--edges 640 160 32) [0 160 320 480 640]))
  (should (equal (eas-slice--edges 1000 160 1) [0 166 333 500 666 833 1000]))
  ;; Never a row thinner than the least height, always one part.
  (should (equal (eas-slice--edges 50 160 32) [0 50]))
  (should (equal (eas-slice--edges 100 20 40) [0 50 100])))

(ert-deftest eas-slice-tile-svg-moves-the-root-and-background ()
  (let* ((svg "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"300\" height=\"200\" viewBox=\"0 0 300 200\" font-family=\"Arial\"><defs></defs><rect width=\"100%\" height=\"100%\" fill=\"white\"></rect><g></g></svg>")
         (parts (eas-slice--split svg)))
    (should (equal (eas-slice-tile-svg parts 100 50 150 150)
                   "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"150\" height=\"150\" viewBox=\"100 50 150 150\" font-family=\"Arial\"><defs></defs><rect x=\"100\" y=\"50\" width=\"100%\" height=\"100%\" fill=\"white\"></rect><g></g></svg>"))
    (should-not (eas-slice--split "<svg width=\"1\">"))))

(ert-deftest eas-slice-tile-map-moves-hot-spots-into-tiles ()
  (let ((map '(((rect (10 . 10) . (40 . 30)) a (help-echo "a"))
               ((circle (200 . 100) . 5) b nil)
               ((poly . [150 0 170 0 170 20]) c nil)
               ((rect (100 . 0) . (300 . 20)) d (pointer hand))))
        (help (make-hash-table :test 'eq)))
    (should (equal (eas-slice--tile-map map 0 0 160 160 help)
                   '(((rect (10 . 10) . (40 . 30)) a (help-echo eas-slice-help-echo))
                     ((poly . [150 0 170 0 170 20]) c nil)
                     ((rect (100 . 0) . (160 . 20)) d (pointer hand)))))
    (should (equal (gethash 'a help) "a"))
    ;; Clipped to the tile: a rect that grows past it leaves it alone.
    (should (equal (eas-slice--tile-map map 160 0 320 160)
                   '(((circle (40 . 100) . 5) b nil)
                     ((poly . [-10 0 10 0 10 20]) c nil)
                     ((rect (0 . 0) . (140 . 20)) d (pointer hand)))))))

(ert-deftest eas-slice-area-at-finds-the-hot-spot ()
  (let ((image '(image :map (((rect (0 . 0) . (10 . 10)) a nil)
                             ((circle (50 . 50) . 5) b nil)
                             ((poly . [100 0 120 0 110 20]) c nil)))))
    (should (eq (eas-slice-area-at image 5 5) 'a))
    (should (eq (eas-slice-area-at image 52 49) 'b))
    (should (eq (eas-slice-area-at image 110 5) 'c))
    (should-not (eas-slice-area-at image 30 30))))

(ert-deftest eas-slice-dirty-reads-changed-items ()
  "A clock tick changes boxes around its hands; a resize changes everything."
  (eas-slice-test--with
    (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg :size '(640 . 400))))
      (unwind-protect
          (let ((old (eas-view-scene view)))
            (cl-incf clock 1.0) (eas-play-tick view clock)
            (let ((boxes (eas-slice-dirty old (eas-view-scene view))))
              (should (consp boxes))
              (dolist (b boxes) (should (and (vectorp b) (<= (aref b 0) (aref b 2)) (<= (aref b 1) (aref b 3))))))
            (should-not (eas-slice-dirty (eas-view-scene view) (eas-view-scene view)))
            (should (eq (eas-slice-dirty nil (eas-view-scene view)) t))
            (let ((before (eas-view-scene view)))
              (eas-view-resize view '(600 . 400))
              (should (eq (eas-slice-dirty before (eas-view-scene view)) t))))
        (eas-play-detach view) (eas-view-close view)))))

(ert-deftest eas-slice-clock-keeps-most-tiles ()
  "A clock tick re-rasterizes the tiles under its hands; the rest are kept."
  (eas-slice-test--with
    (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg :size '(1000 . 640))))
      (unwind-protect
          (let ((counts (eas-slice-test--drive view '(1000 . 640)
                                               (lambda (v _) (cl-incf clock 1.0) (eas-play-tick v clock))
                                               12 nil)))
            (should (= (eas-slice-test--tiles '(1000 . 640)) 24))
            ;; The first frames are one image; once ticks have shown they
            ;; change little, each redraws a few images, under half the chart.
            (should (= (car (car counts)) 1))
            (dolist (n (last counts 6))
              (should (< (car n) 12))
              (should (< (cdr n) 0.5))))
        (eas-play-detach view) (eas-view-close view)))))

(ert-deftest eas-slice-unchanged-frame-keeps-every-tile ()
  "A redraw with nothing changed keeps every image, `eq'."
  (eas-slice-test--with
    (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg :size '(640 . 400))))
      (unwind-protect
          (with-temp-buffer
            (eas-view-mode)
            (setq eas-mode--view view)
            (eas-mode-redraw)
            (let ((tiles (eas-slice-frame-tiles eas-slice--frame))
                  (before (plist-get eas-slice-stats :rastered)))
              (eas-mode-redraw)
              (should (= (plist-get eas-slice-stats :rastered) before))
              (should (cl-every #'eq tiles (eas-slice-frame-tiles eas-slice--frame)))
              ;; Each tile is a line's worth of images, then the strip.
              (should (eq (get-text-property (point-min) 'display) (aref tiles 0)))
              (should (text-property-any (point-min) (point-max) 'eas-strip t))
              (should (= (count-lines (point-min) (text-property-any (point-min) (point-max) 'eas-strip t))
                         (cl-count-if (lambda (seg) (= (aref seg 3) (1- (length (eas-slice-frame-xs eas-slice--frame)))))
                                      (eas-slice-frame-segs eas-slice--frame))))))
        (eas-play-detach view) (eas-view-close view)))))

(ert-deftest eas-slice-ring-returns-a-revisited-tile ()
  "A frame that comes back finds its tiles in the ring, not the rasterizer."
  (eas-slice-test--with
    (let ((view (eas-view-open eas-slice-test--ladder :id "slice-ring" :size '(640 . 360)
                               :rows (eas-slice-test--book 0))))
      (unwind-protect
          (let ((counts (eas-slice-test--drive
                         view '(640 . 360)
                         (lambda (v i) (eas-dispatch v (list :type "push" :key "price" :rows (eas-slice-test--book (% (1+ i) 2)))))
                         6 nil)))
            (should (> (plist-get eas-slice-stats :ring-hits) 0))
            ;; From the third frame on every frame is a revisit.
            (should (cl-every (lambda (n) (zerop (car n))) (nthcdr 2 counts))))
        (eas-view-close view)))))

(ert-deftest eas-slice-event-px-adds-the-tile-origin ()
  (let* ((image (list 'image :type 'svg :data "" :scale 1 :eas-origin '(160 . 320)))
         (event (list 'mouse-movement (list (selected-window) 1 '(5 . 5) 0 nil 1 '(0 . 0) image '(12 . 7) '(20 . 20)))))
    (should (equal (eas-mode-event-px event) [172.0 327.0]))))

(ert-deftest eas-slice-tiles-equal-a-full-render-clock ()
  "Clock ticks: every tile on display has the full render's pixels."
  (eas-slice-test--rsvg)
  (eas-slice-test--with
    (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg :size '(640 . 400))))
      (unwind-protect
          (eas-slice-test--drive view '(640 . 400) (lambda (v _) (cl-incf clock 7.0) (eas-play-tick v clock))
                                 8 '(0 3 5 7))
        (eas-play-detach view) (eas-view-close view)))))

(ert-deftest eas-slice-tiles-equal-a-full-render-pacman ()
  "Pacman ticks: every tile on display has the full render's pixels."
  (eas-slice-test--rsvg)
  (let ((eas-slice-whole-share 2.0))
    (eas-slice-test--with
      (let ((view (eas-play-open "pacman" :bindings (eas-template-example "pacman") :target 'svg :size '(640 . 400))))
	(unwind-protect
            (eas-slice-test--drive view '(640 . 400) (lambda (v _) (cl-incf clock 1.0) (eas-play-tick v clock))
                                   10 '(3 6 9))
          (eas-play-detach view) (eas-view-close view))))
    (should (> (plist-get eas-slice-stats :kept) 0))))

(ert-deftest eas-slice-tiles-equal-a-full-render-ladder ()
  "Ladder pushes: every tile on display has the full render's pixels."
  (eas-slice-test--rsvg)
  (let ((eas-slice-whole-share 2.0))
    (eas-slice-test--with
      (let ((view (eas-view-open eas-slice-test--ladder :id "slice-ladder" :size '(640 . 360)
				 :rows (eas-slice-test--book 0))))
	(unwind-protect
            (eas-slice-test--drive
             view '(640 . 360)
             (lambda (v i) (eas-dispatch v (list :type "push" :key "price" :rows (eas-slice-test--book (1+ i)))))
             5 '(0 2 4))
          (eas-view-close view))))
    (should (> (plist-get eas-slice-stats :kept) 0))))

(ert-deftest eas-slice-tiles-equal-a-full-render-airports ()
  "Airport hovers: every tile on display has the full render's pixels."
  (eas-slice-test--rsvg)
  (let ((eas-slice-whole-share 2.0))
    (eas-slice-test--with
      (let ((view (eas-view-open "airport-connections" :bindings (eas-template-example "airport-connections")
				 :target 'svg :size '(640 . 400))))
	(unwind-protect
            (eas-slice-test--drive
             view '(640 . 400)
             (lambda (v i) (eas-dispatch v (list :type "pointermove"
						 :px (vector (* 640 (/ (+ 0.5 (% (* 7 i) 20)) 20.0))
                                                             (* 400 (/ (+ 0.5 (% (* 3 i) 10)) 10.0))))))
             6 '(1 3 5))
          (eas-view-close view))))
    (should (> (plist-get eas-slice-stats :kept) 0))))

(provide 'eas-slice-test)
;;; eas-slice-test.el ends here

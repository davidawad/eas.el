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
                                               18 nil)))
            (should (= (eas-slice-test--tiles '(1000 . 640)) 24))
            ;; The first frames are one image; once ticks have shown they
            ;; change little, the chart turns to tiles and each frame
            ;; redraws a few images, under half the chart.
            (should (= (car (car counts)) 1))
            (should (> (plist-get eas-slice-stats :whole) 0))
            (dolist (n (last counts 6))
              (should (< (car n) 12)))
            (should (< (apply #'+ (mapcar #'cdr (last counts 6))) (* 6 0.5))))
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
  (let ((eas-slice-gain 2.0) (eas-slice-leave 3.0))
   (eas-slice-test--with
    (let ((view (eas-view-open eas-slice-test--ladder :id "slice-ring" :size '(640 . 360)
                               :rows (eas-slice-test--book 0))))
      (unwind-protect
          (let ((counts (eas-slice-test--drive
                         view '(640 . 360)
                         (lambda (v i) (eas-dispatch v (list :type "push" :key "price" :rows (eas-slice-test--book (% (1+ i) 2)))))
                         12 nil)))
            (should (> (plist-get eas-slice-stats :ring-hits) 0))
            ;; Every frame is a revisit: the ring serves every frame.
            (should (cl-every (lambda (n) (zerop (car n))) (last counts 6))))
        (eas-view-close view))))))

;;; The image cache, modeled

(defun eas-slice-test--shown ()
  "The chart lines of the current buffer: their images, then the strip."
  (let ((end (text-property-any (point-min) (point-max) 'eas-strip t)))
    (list (cl-loop for pos from (point-min) below (or end (point-max))
                   collect (or (get-text-property pos 'display) (char-after pos)))
          (and end (buffer-substring end (point-max))))))

(defun eas-slice-test--fresh (frame)
  "What `eas-slice-test--shown' reads after FRAME is inserted afresh."
  (let ((strip (let ((pos (text-property-any (point-min) (point-max) 'eas-strip t)))
                 (and pos (buffer-substring pos (point-max))))))
    (with-temp-buffer
      (eas-slice-insert frame)
      (when strip (insert strip))
      (eas-slice-test--shown))))

(defun eas-slice-test--cached-drive (view size update frames)
  "Show VIEW at SIZE and run FRAMES of UPDATE against a model image cache.
Emacs rasterizes an image whose spec is not `equal' to one in its
cache, and `image-flush' drops one; the model does both.  Return a
list per frame of (RASTERED SAME CACHED RING MODE SHOWN): the images
the frame rasterized, whether every image shown is `eq' to the last
frame's, the images in the cache after it, the ring's estimated
bytes, the frame's mode and the images it shows."
  (let ((cache (make-hash-table :test 'equal)) (out nil) (shown nil))
    (cl-letf (((symbol-function 'image-flush) (lambda (spec &optional _) (remhash spec cache))))
      (with-temp-buffer
        (eas-view-mode)
        (setq eas-mode--view view)
        (eas-view-resize view size 'svg)
        (dotimes (i (1+ frames))
          (unless (zerop i) (funcall update view (1- i)))
          (eas-mode-redraw)
          (let* ((tiles (append (eas-slice-frame-tiles eas-slice--frame) nil))
                 (rastered (cl-count-if-not (lambda (tile) (gethash tile cache)) tiles)))
            ;; Lines patched in place read as the frame drawn afresh.
            (should (equal (eas-slice-test--shown) (eas-slice-test--fresh eas-slice--frame)))
            (dolist (tile tiles) (puthash tile t cache))
            (unless (zerop i)
              (push (list rastered (and (= (length tiles) (length shown)) (cl-every #'eq tiles shown))
                          (hash-table-count cache) (apply #'+ (mapcar #'car eas-slice--ring))
                          (eas-slice-frame-mode eas-slice--frame) (length tiles))
                    out))
            (setq shown tiles)))))
    (nreverse out)))

(defun eas-slice-test--deep-book (seed)
  "A 2x25-level book, seeded by SEED: the ns-frames.el ladder's rows."
  (let ((state seed))
    (vconcat
     (cl-loop for side in '("ask" "bid")
              append (cl-loop for i below 25
                              do (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                              collect (list :side side :level i
                                            :price (if (equal side "ask") (+ 100.01 (* 0.01 i)) (- 99.99 (* 0.01 i)))
                                            :size (+ 1 (% (/ state 7) 20))))))))

(defun eas-slice-test--deep-push (view i)
  "Push to VIEW the book of step I: ten of its fifty levels change."
  (let ((book (eas-slice-test--deep-book 1)) (seed 1))
    (dotimes (k (1+ i))
      (let ((fresh (eas-slice-test--deep-book (+ 2 k))))
        (dotimes (_ 10)
          (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
          (aset book (% seed 50) (aref fresh (% seed 50))))))
    (eas-dispatch view (list :type "push" :rows book :window 50))))

(defconst eas-slice-test--deep-ladder
  '(:width 400 :height 300
    :encoding (:y (:field "price" :type "ordinal" :sort "descending" :axis (:format ".2f")))
    :layer [(:mark (:type "bar")
             :encoding (:x (:field "size" :type "quantitative" :scale (:domain [0 40]))
                        :color (:field "side" :type "nominal")))
            (:mark (:type "text" :align "left")
             :encoding (:x (:field "size" :type "quantitative") :text (:field "size" :type "quantitative")))])
  "The ns-frames.el price ladder.")

(ert-deftest eas-slice-unchanged-svg-rasterizes-nothing ()
  "A frame that draws the same SVG keeps the very images, whole or tiles."
  (dolist (tiles '(nil t))
    (let ((eas-slice-gain (if tiles 2.0 -1.0)) (eas-slice-leave (if tiles 3.0 0.0)))
      (eas-slice-test--with
        (let ((view (eas-view-open eas-slice-test--ladder :id "slice-same" :size '(640 . 360)
                                   :rows (eas-slice-test--book 0))))
          (unwind-protect
              (let ((frames (eas-slice-test--cached-drive
                             view '(640 . 360)
                             (lambda (v _) (eas-dispatch v (list :type "push" :key "price" :rows (eas-slice-test--book 0))))
                             6)))
                (should (eq (nth 4 (car (last frames))) (if tiles 'tiles 'whole)))
                ;; Once the mode settled, nothing rasterizes again and every
                ;; image is the last frame's own.
                (dolist (f (nthcdr 2 frames))
                  (should (zerop (nth 0 f)))
                  (should (nth 1 f)))
                (should (>= (plist-get eas-slice-stats :unchanged) 4)))
            (eas-view-close view)))))))

(ert-deftest eas-slice-parked-pointer-rasterizes-nothing ()
  "A pointer that does not move over the airports map keeps the image."
  (eas-slice-test--with
    (let ((view (eas-view-open "airport-connections" :bindings (eas-template-example "airport-connections")
                               :target 'svg :size '(640 . 400))))
      (unwind-protect
          (let ((frames (eas-slice-test--cached-drive
                         view '(640 . 400) (lambda (v _) (eas-dispatch v '(:type "pointermove" :px [3 3]))) 4)))
            (dolist (f (cdr frames))
              (should (zerop (nth 0 f)))
              (should (nth 1 f))))
        (eas-view-close view)))))

(ert-deftest eas-slice-image-cache-stays-bounded ()
  "Replaced images leave the cache: the ring is bounded and empties when idle."
  (let ((eas-slice-ring-patience 6))
    (eas-slice-test--with
      (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg :size '(1000 . 640))))
        (unwind-protect
            (let ((frames (eas-slice-test--cached-drive
                           view '(1000 . 640) (lambda (v _) (cl-incf clock 1.0) (eas-play-tick v clock)) 30)))
              (should (eq (nth 4 (car (last frames))) 'tiles))
              (dolist (f frames) (should (<= (nth 3 f) eas-slice-ring-bytes)))
              ;; A clock never comes back to a frame: once the ring has gone
              ;; unused, the cache holds what is shown and nothing else.
              (should (zerop (nth 3 (car (last frames)))))
              (should (= (nth 2 (car (last frames))) (nth 5 (car (last frames))))))
          (eas-play-detach view) (eas-view-close view)))))
  ;; One image a frame: the last one is flushed at once.
  (eas-slice-test--with
    (let ((view (eas-view-open (eas-json-encode eas-slice-test--deep-ladder) :id "slice-deep" :size '(1000 . 640)
                               :target 'svg :rows (eas-slice-test--deep-book 1))))
      (unwind-protect
          (dolist (f (eas-slice-test--cached-drive view '(1000 . 640) #'eas-slice-test--deep-push 8))
            (should (= (nth 2 f) 1))
            (should (zerop (nth 3 f))))
        (eas-view-close view)))))

(ert-deftest eas-slice-whole-chart-changes-stay-one-image ()
  "Frames that change most of the chart are one image, not tiles.
The ns-frames.el ladder push and airport hover: as tiles each image
would lay out the whole document again for a share of its pixels."
  (eas-slice-test--with
    (let ((view (eas-view-open (eas-json-encode eas-slice-test--deep-ladder) :id "slice-deep" :size '(1000 . 640)
                               :target 'svg :rows (eas-slice-test--deep-book 1))))
      (unwind-protect
          (dolist (f (eas-slice-test--cached-drive view '(1000 . 640) #'eas-slice-test--deep-push 16))
            (should (= (nth 0 f) 1)))
        (eas-view-close view)))
    (let* ((view (eas-view-open "airport-connections" :bindings (eas-template-example "airport-connections")
                                :target 'svg :size '(1000 . 640)))
           (items nil))
      (unwind-protect
          (progn
            (eas-view-resize view '(1000 . 640) 'svg)
            (setq items (seq-some (lambda (m) (and (member (plist-get m :mark) '("circle" "point" "symbol"))
                                                   (> (length (plist-get m :items)) 0) (plist-get m :items)))
                                  (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))
            (dolist (f (eas-slice-test--cached-drive
                        view '(1000 . 640)
                        (lambda (v i) (let ((item (aref items (% (* 7 i) (length items)))))
                                        (eas-dispatch v (list :type "pointermove"
                                                              :px (vector (plist-get item :x) (plist-get item :y))))))
                        16))
              (should (<= (nth 0 f) 1))
              (should (eq (nth 4 f) 'whole))))
        (eas-view-close view)))))

(ert-deftest eas-slice-revisiting-animation-finds-the-ring ()
  "Pacman revisits its frames: as tiles it finds them in the ring.
It never rasterizes more pixels than one image a frame would."
  (eas-slice-test--with
    (let ((view (eas-play-open "pacman" :bindings (eas-template-example "pacman") :target 'svg :size '(1000 . 640))))
      (unwind-protect
          (let ((frames (eas-slice-test--cached-drive
                         view '(1000 . 640) (lambda (v _) (cl-incf clock 1.0) (eas-play-tick v clock)) 60)))
            (should (memq 'tiles (mapcar (lambda (f) (nth 4 f)) frames)))
            (should (> (plist-get eas-slice-stats :ring-hits) 0))
            (should (< (plist-get eas-slice-stats :pixels) (plist-get eas-slice-stats :chart-pixels))))
        (eas-play-detach view) (eas-view-close view)))))

(ert-deftest eas-slice-event-px-adds-the-tile-origin ()
  (let* ((image (list 'image :type 'svg :data "" :scale 1 :eas-origin '(160 . 320)))
         (event (list 'mouse-movement (list (selected-window) 1 '(5 . 5) 0 nil 1 '(0 . 0) image '(12 . 7) '(20 . 20)))))
    (should (equal (eas-mode-event-px event) [172.0 327.0]))))

(ert-deftest eas-slice-tiles-equal-a-full-render-clock ()
  "Clock ticks: every tile on display has the full render's pixels."
  (eas-slice-test--rsvg)
  (let ((eas-slice-gain 2.0) (eas-slice-leave 3.0))
    (eas-slice-test--with
      (let ((view (eas-play-open "clock" :bindings (eas-template-example "clock") :target 'svg :size '(640 . 400))))
        (unwind-protect
            (eas-slice-test--drive view '(640 . 400) (lambda (v _) (cl-incf clock 7.0) (eas-play-tick v clock))
                                   8 '(0 3 5 7))
          (eas-play-detach view) (eas-view-close view))))))

(ert-deftest eas-slice-tiles-equal-a-full-render-pacman ()
  "Pacman ticks: every tile on display has the full render's pixels."
  (eas-slice-test--rsvg)
  (let ((eas-slice-whole-share 2.0) (eas-slice-gain 2.0) (eas-slice-leave 3.0))
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
  (let ((eas-slice-whole-share 2.0) (eas-slice-gain 2.0) (eas-slice-leave 3.0))
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
  (let ((eas-slice-whole-share 2.0) (eas-slice-gain 2.0) (eas-slice-leave 3.0))
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

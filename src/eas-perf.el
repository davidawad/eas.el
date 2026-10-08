;;; eas-perf.el --- the standing performance suite: workloads and metrics -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.4.  The workloads a session runs, measured per frame.  A
;; frame is the update (a push, a timer tick, a pointermove, a resize,
;; or opening a view) plus the redraw the glue does: the SVG string, or
;; the text grid patched into a buffer (`eas-mode-patch-text').
;;
;;   ladder-25 ladder-100   keyed pushes of 10 level deltas into a
;;                          price ladder (bars and size labels)
;;   depth-25 depth-100     every level's cumulative size re-pushed into
;;                          a two-sided step area
;;   candles                a candlestick stream with a moving average
;;                          and a band: the last candle updates, every
;;                          fourth frame opens a new one (window 200)
;;   clock pacman           timer ticks of the vega templates
;;   pi-monte-carlo         its sample-size slider stepping up by 100
;;   hover-airports hover-counties pointermove sweeps over the
;;                          airport-connections and county-unemployment
;;                          templates
;;   resize                 a stock chart cycling through four sizes
;;   tiles/NAME             a live workload drawn as GUI tiles at 1000x640
;;                          (eas-slice.el, SVG only): clock, pacman,
;;                          ladder-25, depth-25 and hover-airports
;;   render/NAME            opening and drawing template NAME once, for
;;                          every template (the first render)
;;   cold/NAME              the same for the map templates of
;;                          `eas-perf-cold', every cache emptied first
;;                          (files read, shapes projected, memos, SVG
;;                          fragments), as in a fresh Emacs
;;   cold-native/NAME       cold/NAME with the native geo backend
;;                          (eas-geo-native.el); left out, not failed,
;;                          when the module is not built
;;
;; Every workload but cold-native/ draws maps with the Elisp backend
;; (`eas-geo-backend' is `lisp'), so a built module changes no baseline.
;;
;; Every workload runs on both targets.  Per frame it reports three
;; kinds of metric:
;;
;;   alloc    bytes consed (from `memory-use-counts' deltas: conses,
;;            floats, vector cells, symbols, string bytes, intervals,
;;            strings) and conses; deterministic for one Emacs and one
;;            compilation mode, whatever the machine
;;   calls    full compiles (`eas-compile-plan'), patches (row and
;;            selection patches of a cached plan), scenes, renders and
;;            repaints (text grids patched into a buffer)
;;   time     wall-clock ms, median and p95 over the measured frames
;;   slices   tiles/ only: images re-rasterized per frame (:slices) out
;;            of the images shown (:slice-tiles), and the percentage of
;;            the chart's pixels they cover (:slice-area): what librsvg
;;            would draw again in a GUI frame (eas-e3s)
;;
;; Alloc and calls are hardware independent and gate regressions
;; (eas-perf-gate.el); time is reported only.  Clocks are fixed, so no
;; frame budget or timer depends on how fast the machine is.

;;; Code:

(require 'cl-lib)
(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-mode-patch)
(require 'eas-slice)
(require 'eas-template)
(require 'eas-geo-native)
(require 'eas-bench)

(defvar eas-perf-frames 20 "Frames measured per live workload.")

(defvar eas-perf-warmup 3 "Frames run before measuring a live workload.")

(defvar eas-perf-render-frames 2 "Opens measured per first-render workload.")

(defvar eas-perf-cold '("projections" "county-unemployment" "map-with-tooltip" "world-map")
  "Templates whose cold first render is a workload (cold/NAME).")

(defvar eas-perf-heavy '("hover-counties" "pi-monte-carlo")
  "Live workloads slow enough to measure over `eas-perf-heavy-frames'.
Their allocation is as deterministic as the others', so fewer frames
gate as well and keep the suite short.")

(defvar eas-perf-heavy-frames 8 "Frames measured per heavy workload.")

(defconst eas-perf-text-size '(:cols 100 :rows 40) "Cell size of the text frames.")

(defconst eas-perf-tile-size '(1000 . 640)
  "Pixel size of the tiles/ workloads: the NS window of engine-spikes 8.14.")

(defvar eas-perf--svg-size nil
  "Pixel size of SVG views when the workload names none (nil: its own).")

(defvar eas-perf--tiles nil
  "Non-nil while a tiles/ workload draws: SVG frames are drawn as tiles.")

(defconst eas-perf-targets '(svg text) "Targets every workload runs on.")

(defconst eas-perf-counted
  '((eas-compile-plan . compiles) (eas-compile-rows-patch . patches)
    (eas-compile-patch . patches) (eas-compile-scene . scenes)
    (eas-svg-render . renders) (eas-text-render . renders)
    (eas-mode-patch-text . repaints))
  "Functions whose calls are counted, and the metric each counts toward.")

(defconst eas-perf-call-metrics '(compiles patches scenes renders repaints)
  "Call-count metrics, in report order.")

(defconst eas-perf--cell-bytes [16 16 8 48 1 56 32]
  "Bytes per unit of each `memory-use-counts' entry on a 64-bit build.")

;;; Mode

(defun eas-perf-mode ()
  "Return the compilation mode of the engine: native, byte or interpreted.
The value is a string."
  (let ((f (symbol-function 'eas-dispatch)))
    (cond ((or (and (fboundp 'native-comp-function-p) (native-comp-function-p f))
               (and (fboundp 'subr-native-elisp-p) (subr-native-elisp-p f)))
           "native")
          ((eas-bench-compiled-p) "byte")
          (t "interpreted"))))

;;; Measuring

(defun eas-perf--alloc-bytes (before after)
  "Bytes consed between `memory-use-counts' BEFORE and AFTER."
  (cl-loop for b in before for a in after for i from 0
           sum (* (aref eas-perf--cell-bytes i) (- a b))))

(defun eas-perf--percentile (ms p)
  "The P quantile (0..1, nearest rank) of the numbers MS."
  (let ((sorted (sort (copy-sequence ms) #'<)))
    (nth (max 0 (1- (ceiling (* p (length sorted))))) sorted)))

(defun eas-perf--draw (view buffer)
  "Draw VIEW's scene as the glue does, text into BUFFER."
  (let ((scene (eas-view-scene view)))
    (cond
     ((and eas-perf--tiles (eq (eas-view-target view) 'svg))
      ;; What `eas-mode-redraw' does in a GUI frame, but the values strip.
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (eas-slice-redraw scene (eas-svg-render scene) (eas-svg-hot-spots scene) nil ""))))
     ((eq (eas-view-target view) 'svg) (eas-svg-render scene))
     (t
      (with-current-buffer buffer
        (let ((inhibit-read-only t)) (eas-mode-patch-text (eas-text-render scene))))))))

(defmacro eas-perf--counting (counts &rest body)
  "Run BODY, tallying each `eas-perf-counted' function into hash COUNTS."
  (declare (indent 1) (debug t))
  (let ((advices (make-symbol "advices")))
    `(let ((,advices (mapcar (lambda (entry)
                               (cons (car entry)
                                     (lambda (&rest _) (puthash (cdr entry) (1+ (gethash (cdr entry) ,counts 0))
                                                                ,counts))))
                             eas-perf-counted)))
       (unwind-protect
           (progn (dolist (a ,advices) (advice-add (car a) :before (cdr a)))
                  ,@body)
         (dolist (a ,advices) (advice-remove (car a) (cdr a)))))))

(defun eas-perf-run-frames (step frames warmup)
  "Measure FRAMES frames of STEP after WARMUP unmeasured ones.
STEP gets the frame number and the scratch buffer; it does the update
and the draw.  Return the per-frame metrics plist."
  (let ((buffer (generate-new-buffer " *eas-perf*"))
        (counts (make-hash-table :test 'eq))
        (ms nil) (bytes 0) (conses 0)
        (slices 0) (tiles 0) (pixels 0) (chart 0))
    (unwind-protect
        (progn
          (dotimes (i warmup) (funcall step i buffer))
          (setq slices (plist-get eas-slice-stats :rastered) tiles (plist-get eas-slice-stats :tiles)
                pixels (plist-get eas-slice-stats :pixels) chart (plist-get eas-slice-stats :chart-pixels))
          (garbage-collect)
          (eas-perf--counting counts
            (dotimes (i frames)
              (let* ((before (memory-use-counts)) (t0 (float-time)))
                (funcall step (+ warmup i) buffer)
                (let ((t1 (float-time)) (after (memory-use-counts)))
                  (push (* 1000.0 (- t1 t0)) ms)
                  (cl-incf bytes (eas-perf--alloc-bytes before after))
                  (cl-incf conses (- (car after) (car before))))))))
      (kill-buffer buffer))
    (append
     (list :frames frames
           :alloc-bytes (round bytes frames)
           :conses (round conses frames))
     (cl-loop for m in eas-perf-call-metrics
              append (list (intern (format ":%s" m))
                           (/ (round (* 100.0 (gethash m counts 0)) frames) 100.0)))
     (let ((shown (- (plist-get eas-slice-stats :tiles) tiles)))
       (when (> shown 0)
         (list :slices (/ (round (* 100.0 (- (plist-get eas-slice-stats :rastered) slices)) frames) 100.0)
               :slice-tiles (/ (round (* 100.0 shown) frames) 100.0)
               :slice-area (/ (round (* 1000.0 (- (plist-get eas-slice-stats :pixels) pixels))
                                     (max 1 (- (plist-get eas-slice-stats :chart-pixels) chart)))
                              10.0))))
     (list :ms-median (eas-scene-round (eas-perf--percentile ms 0.5))
           :ms-p95 (eas-scene-round (eas-perf--percentile ms 0.95))))))

;;; Workloads

(defun eas-perf--size (target &optional svg-size)
  "The view size for TARGET: the text cells, or SVG-SIZE (nil: the spec's)."
  (if (eq target 'text) eas-perf-text-size (or svg-size eas-perf--svg-size)))

(defun eas-perf--view-step (view update)
  "Return a frame step: call UPDATE with the frame number, then draw VIEW."
  (lambda (i buffer) (funcall update i) (eas-perf--draw view buffer)))

(defun eas-perf--book (levels i)
  "A deterministic ladder of LEVELS per side at frame I: rows by price."
  (vconcat
   (cl-loop for k below (* 2 levels)
            for bid = (< k levels)
            collect (list :price (+ 100.0 (* 0.01 (if bid (- k levels) (1+ (- k levels)))))
                          :side (if bid "bid" "ask")
                          :size (+ 1 (mod (+ (* k 7919) (* i 104729)) 500))))))

(defun eas-perf--ladder-spec (rows)
  "A price ladder over ROWS: size bars by price, labelled."
  (list :data (list :values rows)
        :encoding '(:y (:field "price" :type "ordinal" :sort "descending")
                    :x (:field "size" :type "quantitative"))
        :layer (vector '(:mark "bar" :encoding (:color (:field "side" :type "nominal")))
                       '(:mark (:type "text" :align "left" :dx 2) :encoding (:text (:field "size"))))))

(defun eas-perf--deltas (levels i)
  "Ten keyed level deltas for frame I of a ladder of LEVELS per side."
  (let ((book (eas-perf--book levels i)))
    (vconcat (cl-loop for j below 10
                      collect (aref book (mod (+ (* j 13) (* i 7)) (length book)))))))

(defun eas-perf--depth (levels i)
  "Cumulative depth rows of the ladder of LEVELS per side at frame I."
  (let ((book (append (eas-perf--book levels i) nil)) (out nil))
    (dolist (side '("bid" "ask"))
      (let* ((rows (seq-filter (lambda (r) (equal (plist-get r :side) side)) book))
             ;; Depth accumulates away from the spread.
             (rows (if (equal side "bid") (reverse rows) rows)) (sum 0))
        (dolist (r rows)
          (setq sum (+ sum (plist-get r :size)))
          (push (list :price (plist-get r :price) :side side :depth sum) out))))
    (vconcat (sort out (lambda (a b) (< (plist-get a :price) (plist-get b :price)))))))

(defun eas-perf--depth-spec (rows)
  "A two-sided depth chart over ROWS."
  (list :data (list :values rows)
        :mark '(:type "area" :interpolate "step-after" :opacity 0.6)
        :encoding '(:x (:field "price" :type "quantitative" :scale (:zero :false))
                    :y (:field "depth" :type "quantitative")
                    :color (:field "side" :type "nominal"))))

(defun eas-perf--candle (k)
  "Candle K: deterministic OHLC with a moving average and a band."
  (let* ((mid (+ 100 (* 8 (sin (/ k 23.0))) (* 0.5 (mod (* k 7919) 13))))
         (open (+ mid (* 0.7 (sin k)))) (close (+ mid (* 0.7 (cos (* 3 k))))))
    (list :t (* 60000 k) :open open :close close
          :high (+ (max open close) 0.4 (* 0.1 (mod k 5)))
          :low (- (min open close) 0.4 (* 0.1 (mod k 3)))
          :sma (+ 100 (* 8 (sin (/ (- k 10) 23.0))))
          :upper (+ 102.5 (* 8 (sin (/ (- k 10) 23.0))))
          :lower (+ 97.5 (* 8 (sin (/ (- k 10) 23.0)))))))

(defun eas-perf--candles-spec (rows)
  "A candlestick chart with an SMA line and a band over ROWS."
  (list :data (list :values rows)
        :encoding '(:x (:field "t" :type "temporal"))
        :layer (vector
                '(:mark (:type "area" :opacity 0.15)
                  :encoding (:y (:field "lower" :type "quantitative" :scale (:zero :false))
                             :y2 (:field "upper")))
                '(:mark "rule" :encoding (:y (:field "low" :type "quantitative" :scale (:zero :false))
                                          :y2 (:field "high")))
                '(:mark "bar"
                  :encoding (:y (:field "open" :type "quantitative" :scale (:zero :false))
                             :y2 (:field "close")
                             :color (:condition (:test "datum.open < datum.close" :value "#06982d")
                                     :value "#ae1325")))
                '(:mark "line" :encoding (:y (:field "sma" :type "quantitative"
                                                     :scale (:zero :false)))))))

(defun eas-perf--live (spec target update &optional svg-size)
  "Open SPEC on TARGET; return (STEP . CLOSE) calling UPDATE (VIEW I) per frame.
SVG-SIZE is the pixel size (default 640x360)."
  (let ((view (eas-view-open spec :target target
                             :size (eas-perf--size target (or svg-size eas-perf--svg-size '(640 . 360))))))
    (cons (eas-perf--view-step view (lambda (i) (funcall update view i)))
          (lambda () (eas-view-close view)))))

(defun eas-perf--ladder (levels)
  "Return the setup of the ladder workload of LEVELS per side."
  (lambda (target)
    (eas-perf--live (eas-perf--ladder-spec (eas-perf--book levels 0)) target
                    (lambda (view i)
                      (eas-dispatch view (list :type "push" :key "price"
                                               :rows (eas-perf--deltas levels (1+ i))))))))

(defun eas-perf--depth-workload (levels)
  "Return the setup of the depth workload of LEVELS per side."
  (lambda (target)
    (eas-perf--live (eas-perf--depth-spec (eas-perf--depth levels 0)) target
                    (lambda (view i)
                      (eas-dispatch view (list :type "push" :key "price"
                                               :rows (eas-perf--depth levels (1+ i))))))))

(defun eas-perf--candles (target)
  "Setup of the candles workload on TARGET."
  (eas-perf--live (eas-perf--candles-spec (vconcat (mapcar #'eas-perf--candle (number-sequence 0 199))))
                  target
                  (lambda (view i)
                    (let* ((k (+ 200 (/ i 4)))
                           (c (eas-perf--candle k))
                           ;; The forming candle's close moves each frame.
                           (c (plist-put c :close (+ (plist-get c :close) (* 0.05 (mod i 4))))))
                      (eas-dispatch view (list :type "push" :key "t" :rows (vector c) :window 200))))))

(defun eas-perf--play (template)
  "Setup of the timer workload of TEMPLATE, as a function of a target."
  (lambda (target)
    (let* ((clock 1.7e9)
           (eas-play-clock (lambda () clock))
           (view (eas-play-open template :bindings (eas-template-example template)
                                :target target :size (eas-perf--size target))))
      (cons (eas-perf--view-step view (lambda (_) (cl-incf clock 1.0) (eas-play-tick view clock)))
            (lambda () (eas-play-detach view) (eas-view-close view))))))

(defun eas-perf--slider (template param values)
  "Setup of TEMPLATE's PARAM stepping through VALUES (a function of i).
The result is a function of a target, as `eas-perf--play'."
  (lambda (target)
    (let ((view (eas-view-open template :bindings (eas-template-example template)
                               :target target :size (eas-perf--size target))))
      (cons (eas-perf--view-step
             view (lambda (i) (eas-dispatch view (list :type "param" :param param
                                                       :value (funcall values i)))))
            (lambda () (eas-view-close view))))))

(defun eas-perf--hover (template)
  "Setup of a pointermove sweep over TEMPLATE, as a function of a target."
  (lambda (target)
    (let* ((view (eas-view-open template :bindings (eas-template-example template)
                                :target target :size (eas-perf--size target)))
           (size (plist-get (eas-view-scene view) :size))
           (w (or (plist-get size :width) 800)) (h (or (plist-get size :height) 500)))
      (cons (eas-perf--view-step
             view (lambda (i)
                    (eas-dispatch view (list :type "pointermove"
                                             :px (vector (* w (/ (+ 0.5 (% (* 7 i) 20)) 20.0))
                                                         (* h (/ (+ 0.5 (% (* 3 i) 10)) 10.0)))))))
            (lambda () (eas-view-close view))))))

(defun eas-perf--resize (target)
  "Setup of the resize workload on TARGET."
  (let* ((sizes (if (eq target 'text)
                    '((:cols 100 :rows 40) (:cols 80 :rows 24) (:cols 120 :rows 50) (:cols 90 :rows 30))
                  '((800 . 400) (640 . 360) (1024 . 600) (720 . 480))))
         (view (eas-view-open "stock-index-chart" :bindings (eas-template-example "stock-index-chart")
                              :target target :size (car sizes))))
    (cons (eas-perf--view-step view (lambda (i) (eas-view-resize view (nth (% (1+ i) 4) sizes))))
          (lambda () (eas-view-close view)))))

(defun eas-perf--render (template)
  "Setup of the first render of TEMPLATE, as a function of a target."
  (lambda (target)
    (let ((bindings (ignore-errors (eas-template-example template))))
      (cons (lambda (_ buffer)
              (let ((view (eas-view-open template :bindings bindings :target target
                                         :size (eas-perf--size target))))
                (unwind-protect (eas-perf--draw view buffer)
                  (eas-view-close view))))
            #'ignore))))

(defun eas-perf--tiled (setup)
  "SETUP of a live workload, drawn as tiles at `eas-perf-tile-size'."
  (lambda (target)
    (let* ((pair (let ((eas-perf--svg-size eas-perf-tile-size)) (funcall setup target)))
           (step (car pair)))
      (cons (lambda (i buffer) (let ((eas-perf--tiles t)) (funcall step i buffer)))
            (cdr pair)))))

(defun eas-perf--forget ()
  "Empty the caches a first render fills, as in a fresh Emacs."
  (eas-geoshape-forget)
  (clrhash eas-data-url--cache)
  (clrhash eas-topojson--files)
  (clrhash eas-svg--n-cache)
  (eas-memo-clear)
  (eas-render-cache-clear))

(defun eas-perf--cold (template &optional backend)
  "Setup of the cold first render of TEMPLATE, as a function of a target.
BACKEND, when non-nil, is the `eas-geo-backend' the frames draw with."
  (let ((render (eas-perf--render template)))
    (lambda (target)
      (let ((pair (funcall render target)))
        (cons (lambda (i buffer)
                (let ((eas-geo-backend (or backend eas-geo-backend)))
                  (eas-perf--forget)
                  (funcall (car pair) i buffer)))
              (cdr pair))))))

(defun eas-perf-native-p ()
  "Return non-nil when the native geo module can be loaded.
The cold-native/ workloads run only then."
  (let ((eas-geo-backend 'auto)) (eq (eas-geo-backend-active) 'native)))

(defun eas-perf-live-workloads ()
  "The live workloads: alist of (NAME . SETUP), SETUP a function of a target.
SETUP returns (STEP . CLOSE)."
  (list (cons "ladder-25" (eas-perf--ladder 25))
        (cons "ladder-100" (eas-perf--ladder 100))
        (cons "depth-25" (eas-perf--depth-workload 25))
        (cons "depth-100" (eas-perf--depth-workload 100))
        (cons "candles" #'eas-perf--candles)
        (cons "clock" (eas-perf--play "clock"))
        (cons "pacman" (eas-perf--play "pacman"))
        (cons "pi-monte-carlo" (eas-perf--slider "pi-monte-carlo" "num_points"
                                                  (lambda (i) (+ 1000 (* 100 (% (1+ i) 40))))))
        (cons "hover-airports" (eas-perf--hover "airport-connections"))
        (cons "hover-counties" (eas-perf--hover "county-unemployment"))
        (cons "resize" #'eas-perf--resize)
        (cons "tiles/clock" (eas-perf--tiled (eas-perf--play "clock")))
        (cons "tiles/pacman" (eas-perf--tiled (eas-perf--play "pacman")))
        (cons "tiles/ladder-25" (eas-perf--tiled (eas-perf--ladder 25)))
        (cons "tiles/depth-25" (eas-perf--tiled (eas-perf--depth-workload 25)))
        (cons "tiles/hover-airports" (eas-perf--tiled (eas-perf--hover "airport-connections")))))

(defun eas-perf--targets (name)
  "Return the targets of workload NAME: tiles/ are GUI frames, SVG only."
  (if (string-prefix-p "tiles/" name) '(svg) eas-perf-targets))

(defun eas-perf-workloads ()
  "Every workload, live ones first, then render/NAME per template."
  (append (eas-perf-live-workloads)
          (mapcar (lambda (name) (cons (concat "render/" name) (eas-perf--render name)))
                  (eas-template-names))
          (mapcar (lambda (name) (cons (concat "cold/" name) (eas-perf--cold name)))
                  eas-perf-cold)
          (and (eas-perf-native-p)
               (mapcar (lambda (name) (cons (concat "cold-native/" name) (eas-perf--cold name 'native)))
                       eas-perf-cold))))

(defun eas-perf-measure (name setup target)
  "Measure workload NAME (SETUP as in `eas-perf-live-workloads') on TARGET.
Return its metrics plist; a workload that fails gives (:error MESSAGE)."
  (let ((render (string-match-p "\\`\\(render\\|cold\\|cold-native\\)/" name)))
    (condition-case err
        (let ((pair (funcall setup target)))
          (unwind-protect
              (eas-perf-run-frames (car pair)
                                   (cond (render eas-perf-render-frames)
                                         ((member name eas-perf-heavy)
                                          (min eas-perf-frames eas-perf-heavy-frames))
                                         (t eas-perf-frames))
                                   (if render 1 eas-perf-warmup))
            (funcall (cdr pair))))
      (error (list :error (error-message-string err))))))

(cl-defun eas-perf-run (&key only progress)
  "Measure every workload (or those whose name matches regexp ONLY).
With PROGRESS, report each workload on stderr.  Return the run plist:
\(:mode MODE :emacs V :workloads ((NAME :svg M :text M) ...))."
  (let ((eas-views (make-hash-table :test 'equal))
        (eas-geo-backend 'lisp)
        (gc-cons-threshold (max gc-cons-threshold (or eas-gc-cons-threshold 0)))
        (out nil))
    (dolist (w (eas-perf-workloads))
      (when (or (null only) (string-match-p only (car w)))
        (let ((t0 (float-time))
              (row (cl-loop for target in (eas-perf--targets (car w))
                            append (list (intern (format ":%s" target))
                                         (eas-perf-measure (car w) (cdr w) target)))))
          (when progress
            (message "eas-perf: %-32s %6.1fs" (car w) (- (float-time) t0)))
          (push (cons (car w) row) out))))
    (list :mode (eas-perf-mode) :emacs emacs-version :workloads (nreverse out))))

(provide 'eas-perf)
;;; eas-perf.el ends here

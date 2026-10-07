;;; bench-frame-text.el --- ms per live text frame, by stage -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.1.  Milliseconds per frame of the live workloads a terminal
;; session runs, on the text target: a 25-level order-book ladder push,
;; a depth-chart push, clock and pacman timer ticks, a pi-monte-carlo
;; slider step (its sample grows by 40 points a frame) and an
;; airport-connections hover.  A frame is the update (push, tick or
;; pointermove) and the redraw the terminal glue does
;; (`eas-mode-redraw': render, values strip, cell patch, readout).  The
;; table splits a frame into update, render (`eas-text-render-lines',
;; or `eas-text-render' before it existed), patch (the rest of the
;; redraw) and garbage collection (GC, with the number of collections),
;; and gives the KB allocated per frame.
;;
;; The order book is synthetic (financial-charts.el is not a
;; dependency): a layered bar+text ladder of 25 bids and 25 asks and a
;; stepped cumulative depth area, each push a keyed delta of 10 levels.
;;
;;   emacs -Q --batch -l scripts/bench-frame-text.el -- MODE [ROOT] [FRAMES] [ONLY]
;;
;; MODE is interpreted, byte or native.  byte and native copy ROOT's
;; src/ (default: this script's repository) to a temporary directory
;; and compile it there, so the tree stays clean.  Each workload runs
;; 3 rounds of FRAMES frames (default 30); the table shows their mean,
;; garbage collection included.  Collection runs as in an interactive
;; session: batch Emacs sets `gc-cons-percentage' to 1.0, an
;; interactive one to 0.1, and at 0.1 collection is most of a frame.  ONLY, a regexp, picks workloads by name.

;;; Code:

(require 'cl-lib)
(require 'bytecomp)
(require 'comp nil t)

(defvar bench-frame-text--args (cdr (member "--" command-line-args)))
(defvar bench-frame-text-mode (or (nth 0 bench-frame-text--args) "interpreted"))
(defvar bench-frame-text-root
  (file-name-as-directory
   (expand-file-name (or (nth 1 bench-frame-text--args)
                         (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))))))
(defvar bench-frame-text-frames (string-to-number (or (nth 2 bench-frame-text--args) "30")))
(defvar bench-frame-text-only (nth 3 bench-frame-text--args)
  "A regexp: bench only the workloads whose name matches, or nil.")
(when bench-frame-text--args (setq command-line-args-left nil))

(defun bench-frame-text--setup ()
  "Put ROOT's src/ on the load path, compiled per `bench-frame-text-mode'."
  (let ((src (expand-file-name "src" bench-frame-text-root)))
    (pcase bench-frame-text-mode
      ("interpreted" (push src load-path) (setq load-prefer-newer t))
      ((or "byte" "native")
       ;; A copy of ROOT whose src/ is compiled; the rest links back.
       (let* ((top (make-temp-file "eas-bench-" t)) (dir (expand-file-name "src" top)))
         (make-directory dir)
         (dolist (f (directory-files bench-frame-text-root nil directory-files-no-dot-files-regexp))
           (unless (member f '("src" ".git"))
             (make-symbolic-link (expand-file-name f bench-frame-text-root) (expand-file-name f top))))
         (dolist (f (directory-files src t "\\.\\(el\\|json\\)\\'"))
           (unless (string-suffix-p "-test.el" f) (copy-file f (expand-file-name (file-name-nondirectory f) dir))))
         (push dir load-path)
         (when (equal bench-frame-text-mode "native")
           (unless (native-comp-available-p) (error "No native compilation in this Emacs"))
           (push (expand-file-name "eln" top) native-comp-eln-load-path))
         ;; Compile in a child Emacs: compiling here would leave the
         ;; source definitions it loads along the way.
         (let ((status
                (call-process
                 (expand-file-name invocation-name invocation-directory) nil (get-buffer-create "*bench-compile*") nil
                 "-Q" "--batch" "-L" dir "--eval"
                 (prin1-to-string
                  `(progn (require (quote bytecomp)) (setq byte-compile-warnings nil native-comp-eln-load-path (quote ,native-comp-eln-load-path))
                     (dolist (f (directory-files ,dir t "\\.el\\'"))
                       (byte-compile-file f)
                       (when ,(equal bench-frame-text-mode "native") (native-compile f))))))))
           (unless (eql status 0)
             (error "Compiling %s failed: %s\n%s" dir status
                    (with-current-buffer "*bench-compile*" (buffer-string)))))))
      (_ (error "MODE is interpreted, byte or native, not %s" bench-frame-text-mode)))))

(bench-frame-text--setup)

(require 'eas)
(require 'eas-view)
(require 'eas-play)
(require 'eas-text)
(require 'eas-mode)
(require 'eas-template)

(defvar bench-frame-text--render
  (if (fboundp 'eas-text-render-lines) 'eas-text-render-lines 'eas-text-render)
  "The function the glue renders text with.")

(defvar bench-frame-text--render-time 0.0 "Seconds spent in `eas-text-render' this frame.")
(defvar bench-frame-text--render-gc 0.0 "Seconds of it spent collecting garbage.")

(defun bench-frame-text--time-render (orig &rest args)
  "Time ORIG (`eas-text-render') called with ARGS."
  (let ((t0 (float-time)) (g0 gc-elapsed))
    (prog1 (apply orig args)
      (cl-incf bench-frame-text--render-gc (- gc-elapsed g0))
      (cl-incf bench-frame-text--render-time (- (float-time) t0)))))

(defun bench-frame-text--bytes ()
  "Bytes allocated so far, roughly, from `memory-use-counts'."
  (cl-loop for n in (memory-use-counts) for w in '(16 16 8 48 1 56 32) sum (* n w)))

(defun bench-frame-text--frames (view step)
  "Mean (UPDATE RENDER PATCH TOTAL KB GCS GC) of STEP then a redraw of VIEW.
UPDATE, RENDER, PATCH, GC and TOTAL are ms; the stages exclude the time
spent collecting garbage, which is GC.  KB is allocated per frame and
GCS counts collections.  The
mean runs over 3 rounds of `bench-frame-text-frames' frames, garbage
collections included, after a warm-up frame."
  (let ((buffer (generate-new-buffer " *bench-frame-text*")) (frame 0)
        (up 0.0) (render 0.0) (draw 0.0) (up-gc 0.0) (render-gc 0.0) (draw-gc 0.0)
        (bytes 0) (gcs gcs-done) (gc-time gc-elapsed)
        (gc-cons-percentage 0.1))
    (advice-add bench-frame-text--render :around #'bench-frame-text--time-render)
    (unwind-protect
        (with-current-buffer buffer
          (setq-local eas-mode--view view)
          (funcall step 0) (eas-mode-redraw buffer)
          (garbage-collect)
          (setq gcs gcs-done gc-time gc-elapsed)
          (dotimes (_round 3)
            (let ((b0 (bench-frame-text--bytes)))
              (dotimes (_ bench-frame-text-frames)
                (let ((t0 (float-time)) (g0 gc-elapsed))
                  (funcall step (cl-incf frame))
                  (let ((t1 (float-time)) (g1 gc-elapsed))
                    (setq bench-frame-text--render-time 0.0 bench-frame-text--render-gc 0.0)
                    (eas-mode-redraw buffer)
                    (cl-incf up (- t1 t0)) (cl-incf up-gc (- g1 g0))
                    (cl-incf draw (- (float-time) t1)) (cl-incf draw-gc (- gc-elapsed g1))
                    (cl-incf render bench-frame-text--render-time)
                    (cl-incf render-gc bench-frame-text--render-gc))))
              (cl-incf bytes (- (bench-frame-text--bytes) b0)))))
      (advice-remove bench-frame-text--render #'bench-frame-text--time-render)
      (kill-buffer buffer))
    (let ((n (* 3.0 bench-frame-text-frames)))
      (append (mapcar (lambda (s) (/ (* 1000 s) n))
                      (list (- up up-gc) (- render render-gc) (- draw render (- draw-gc render-gc)) (+ up draw)))
              (list (/ bytes n 1024.0) (- gcs-done gcs) (/ (* 1000 (- gc-elapsed gc-time)) n))))))

(defconst bench-frame-text--size '(:cols 100 :rows 40) "The terminal size benched.")

(defun bench-frame-text--play (template)
  "Per-tick costs of vega TEMPLATE."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (view (eas-play-open template :bindings (eas-template-example template)
                              :target 'text :size bench-frame-text--size)))
    (unwind-protect
        (bench-frame-text--frames view (lambda (_) (cl-incf clock 1.0) (eas-play-tick view)))
      (eas-play-detach view) (eas-view-close view))))

(defun bench-frame-text--slider (template param from by)
  "Per-step costs of moving vega TEMPLATE's slider PARAM FROM by BY a frame."
  (let ((view (eas-view-open template :bindings (eas-template-example template)
                             :target 'text :size bench-frame-text--size))
        ;; Its update recompiles thousands of points: fewer frames.
        (bench-frame-text-frames (min 6 bench-frame-text-frames)))
    (unwind-protect
        (bench-frame-text--frames
         view (lambda (i) (eas-dispatch view (list :type "param" :param param :value (+ from (* by i))))))
      (eas-view-close view))))

(defun bench-frame-text--hover (template)
  "Per-pointermove costs of vega TEMPLATE, sweeping the canvas."
  (let* ((view (eas-view-open template :bindings (eas-template-example template)
                              :target 'text :size bench-frame-text--size))
         (size (plist-get (eas-view-scene view) :size))
         (w (or (plist-get size :w) 800)) (h (or (plist-get size :h) 500)))
    (unwind-protect
        (bench-frame-text--frames
         view (lambda (i)
                (eas-dispatch view (list :type "pointermove"
                                         :px (vector (* w (/ (+ 0.5 (% (* 7 i) 20)) 20.0))
                                                     (* h (/ (+ 0.5 (% (* 3 i) 10)) 10.0)))))))
      (eas-view-close view))))

;;; The synthetic order book

(defun bench-frame-text--level (side k frame)
  "Level K of SIDE (\"bid\" or \"ask\") at FRAME, a row."
  (let* ((price (if (equal side "bid") (- 100.0 (* 0.5 k)) (+ 100.5 (* 0.5 k))))
         (size (+ 1 (% (+ (* 37 k) (* 11 frame) (if (equal side "bid") 0 7)) 97))))
    (list :price price :size size :side side :label (format "%d" size))))

(defun bench-frame-text--book (frame)
  "All 50 levels at FRAME."
  (vconcat (cl-loop for side in '("bid" "ask")
                    append (cl-loop for k below 25 collect (bench-frame-text--level side k frame)))))

(defun bench-frame-text--depth (book)
  "BOOK's rows with a cumulative :cum per side."
  (let ((acc (list (cons "bid" 0) (cons "ask" 0))))
    (vconcat (mapcar (lambda (r)
                       (let ((cell (assoc (plist-get r :side) acc)))
                         (setcdr cell (+ (cdr cell) (plist-get r :size)))
                         (append r (list :cum (cdr cell)))))
                     book))))

(defun bench-frame-text--ladder-spec ()
  "A 50-level ladder: size bars with their sizes printed."
  `(:data (:values ,(bench-frame-text--book 0))
    :width 600 :height 400
    :encoding (:y (:field "price" :type "ordinal" :sort "descending")
               :x (:field "size" :type "quantitative" :scale (:domain [0 100])))
    :layer [(:mark "bar" :encoding (:color (:field "side" :type "nominal")))
            (:mark (:type "text" :align "left" :dx 3)
             :encoding (:text (:field "label")))]))

(defun bench-frame-text--depth-spec ()
  "A stepped cumulative depth chart of both sides."
  `(:data (:values ,(bench-frame-text--depth (bench-frame-text--book 0)))
    :width 600 :height 300
    :mark (:type "area" :interpolate "step-after" :fillOpacity 0.6 :line t)
    :encoding (:x (:field "price" :type "quantitative" :scale (:domain [87 113]))
               :y (:field "cum" :type "quantitative" :scale (:domain [0 2500]))
               :color (:field "side" :type "nominal"))))

(defun bench-frame-text--push (spec rows-fn)
  "Per-push costs of SPEC, each push ROWS-FN's keyed delta for the frame."
  (let ((view (eas-view-open spec :target 'text :size bench-frame-text--size)))
    (unwind-protect
        (bench-frame-text--frames
         view (lambda (i) (eas-dispatch view (list :type "push" :rows (funcall rows-fn i) :key "price"))))
      (eas-view-close view))))

(defun bench-frame-text--ladder ()
  "Per-push costs of the ladder: 10 changed levels a push."
  (bench-frame-text--push
   (bench-frame-text--ladder-spec)
   (lambda (i) (vconcat (seq-filter (lambda (r) (zerop (% (round (* 2 (plist-get r :price))) 5)))
                                    (bench-frame-text--book i))))))

(defun bench-frame-text--depth-live ()
  "Per-push costs of the depth chart, every level's cumulative size changed."
  (bench-frame-text--push (bench-frame-text--depth-spec)
                          (lambda (i) (bench-frame-text--depth (bench-frame-text--book i)))))

(defvar bench-frame-text-workloads
  '(("order-book ladder push" bench-frame-text--ladder)
    ("depth-live push" bench-frame-text--depth-live)
    ("clock tick" bench-frame-text--play "clock")
    ("pacman tick" bench-frame-text--play "pacman")
    ("pi-monte-carlo step" bench-frame-text--slider "pi-monte-carlo" "num_points" 400 40)
    ("airport-connections hover" bench-frame-text--hover "airport-connections"))
  "Rows (NAME FUNCTION ARGS...).")

(defun bench-frame-text-report ()
  "Print the per-frame table for `bench-frame-text-mode'."
  (princ (format "mode: %s (eas-text-render is %s)  root: %s  frames: 3x%d  cache: %s\n\n"
                 bench-frame-text-mode
                 (let ((f (symbol-function 'eas-text-render)))
                   (cond ((and (fboundp 'native-comp-function-p) (native-comp-function-p f)) "native")
                         ((byte-code-function-p f) "byte-compiled")
                         (t "interpreted")))
                 bench-frame-text-root bench-frame-text-frames eas-render-cache-enabled))
  (princ (concat "| workload | update | render | patch | GC | total ms/frame | KB alloc/frame |\n"
                 "|---|---|---|---|---|---|---|\n"))
  (pcase-dolist (`(,name ,fn . ,args) bench-frame-text-workloads)
   (when (or (null bench-frame-text-only) (string-match-p bench-frame-text-only name))
    (let ((row (condition-case err (apply fn args)
                 (error (message "%s: %S" name err) nil))))
      (princ (if row (format "| %s | %.2f | %.2f | %.2f | %.2f (%d) | %.2f | %.0f |\n"
                               name (nth 0 row) (nth 1 row) (nth 2 row) (nth 6 row) (nth 5 row) (nth 3 row) (nth 4 row))
               (format "| %s | n/a | | | | | |\n" name)))))))

(defvar bench-frame-text-no-report nil "Non-nil to load without printing the table.")
(when (and noninteractive (not bench-frame-text-no-report)) (bench-frame-text-report))

(provide 'bench-frame-text)
;;; bench-frame-text.el ends here

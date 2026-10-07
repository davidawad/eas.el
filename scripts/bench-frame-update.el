;;; bench-frame-update.el --- per-frame cost of the update path -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.3.  Milliseconds per frame of the compile/update path, before
;; any renderer runs: the push, tick or pointermove dispatched to a view
;; (reduce, compile plan or patch, scene assembly).  Workloads:
;;
;;   ladder push     a 2x25-level order-book ladder (bars and labels),
;;                   10 levels changed per frame (keyed push)
;;   ladder-100 push the same ladder with 2x50 levels, 20 changed per frame
;;   depth push      the same book as cumulative depth areas
;;   depth-fixed push  the same, every domain literal (colour too)
;;   candle stream   60 OHLC candles and a moving average: the last
;;                   candle updates each frame, a new one every fourth
;;   clock tick      the clock template's timer
;;   pacman tick     the pacman template's timer
;;   pi-mc step      pi-monte-carlo's slider moved by 25 points
;;   airport hover   a pointermove sweep over airport-connections
;;
;; each for the svg and text targets.  MODE picks how the engine runs:
;;
;;   interp   the .el files, interpreted
;;   byte     byte-compiled copies (built once per source tree)
;;   native   natively compiled copies (built once per source tree)
;;
;; Builds go to $EAS_BENCH_BUILD (default /tmp/eas-bench-build/HASH),
;; keyed by the hash of src/*.el, so a "before" and an "after" tree
;; each build once.
;;
;;   emacs -Q --batch -l scripts/bench-frame-update.el -- MODE [SRC] [FRAMES]
;;
;; SRC defaults to this repository's src/.  Prints a Markdown table.
;; $EAS_BENCH_ONLY, a regexp, picks workloads by name.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar bench-frame-update-frames 30 "Frames timed per workload.")

(defvar bench-frame-update-root
  (file-name-directory (directory-file-name (file-name-directory (or load-file-name buffer-file-name))))
  "The repository root this script belongs to.")

;;; Builds

(defun bench-frame-update--libs (src)
  "Library files of SRC (no tests)."
  (seq-remove (lambda (f) (string-suffix-p "-test.el" f))
              (directory-files src t "\\`eas.*\\.el\\'")))

(defun bench-frame-update--hash (src)
  "A hash of SRC's library sources."
  (secure-hash 'sha1 (mapconcat (lambda (f) (with-temp-buffer (insert-file-contents-literally f)
                                                              (buffer-string)))
                                (bench-frame-update--libs src) "")))

(defun bench-frame-update--run-jobs (commands)
  "Run shell COMMANDS, 8 at a time; signal when one fails."
  (let ((queue commands) (running nil) (failed nil))
    (while (or queue running)
      (while (and queue (< (length running) 8))
        (push (start-process-shell-command "bench-build" nil (pop queue)) running))
      (accept-process-output nil 0.2)
      (setq running (seq-filter (lambda (p) (if (process-live-p p) t
                                                (unless (zerop (process-exit-status p)) (setq failed t))
                                                nil))
                                running)))
    (when failed (error "A build job failed"))))

(defun bench-frame-update--build (src mode)
  "Build a MODE copy of SRC's libraries; return its directory."
  (let* ((base (or (getenv "EAS_BENCH_BUILD") "/tmp/eas-bench-build"))
         (dir (expand-file-name (concat (bench-frame-update--hash src) "/lib") base))
         (eln (expand-file-name "../eln" dir))
         (stamp (expand-file-name (format "../%s.done" mode) dir))
         (libs (bench-frame-update--libs src)))
    (unless (file-exists-p stamp)
      (make-directory dir t)
      (dolist (f libs) (copy-file f (expand-file-name (file-name-nondirectory f) dir) t))
      ;; Flat, as MELPA installs it: templates/ and examples/ beside the libraries.
      (dolist (d '("templates" "examples"))
        (make-symbolic-link (expand-file-name d bench-frame-update-root) (expand-file-name d dir) t))
      (let* ((emacs (expand-file-name invocation-name invocation-directory))
             (files (mapcar (lambda (f) (expand-file-name (file-name-nondirectory f) dir)) libs))
             (slices (let ((n 8) (out nil))
                       (dotimes (i n) (push (cl-loop for f in files for k from 0
                                                     when (= (% k n) i) collect f)
                                            out))
                       out))
             (job (lambda (form fs)
                    (format "%s -Q --batch -L %s --eval %s %s"
                            (shell-quote-argument emacs) (shell-quote-argument dir)
                            (shell-quote-argument (format "%S" form))
                            (mapconcat #'shell-quote-argument fs " ")))))
        (unless (file-exists-p (expand-file-name "../byte.done" dir))
          ;; Two passes: the first may load uncompiled dependencies.
          (bench-frame-update--run-jobs
           (mapcar (lambda (fs) (funcall job '(progn (setq native-comp-jit-compilation nil)
                                                       (batch-byte-compile))
                                         fs))
                   slices))
          (with-temp-file (expand-file-name "../byte.done" dir)))
        (when (eq mode 'native)
          (make-directory eln t)
          (bench-frame-update--run-jobs
           (mapcar (lambda (fs)
                     (funcall job `(progn (setq native-comp-jit-compilation nil
                                                native-comp-eln-load-path (list ,eln))
                                          (dolist (f command-line-args-left) (native-compile f))
                                          (setq command-line-args-left nil))
                              fs))
                   slices))
          (with-temp-file stamp))))
    dir))

(defun bench-frame-update--setup (mode src)
  "Put SRC's engine on the load path for MODE."
  (setq native-comp-jit-compilation nil)
  (pcase mode
    ('interp (push src load-path))
    ((or 'byte 'native)
     (let ((dir (bench-frame-update--build src mode)))
       (when (eq mode 'native)
         (setq native-comp-eln-load-path (list (expand-file-name "../eln" dir))))
       (push dir load-path)))
    (_ (error "MODE is interp, byte or native, not %s" mode)))
  (setq load-prefer-newer nil)
  (require 'eas))

;;; Workloads

(defun bench-frame-update--book (seed levels)
  "A LEVELS-per-side order book as rows, sizes drawn from SEED."
  (let ((state seed))
    (vconcat
     (cl-loop for side in '("ask" "bid")
              append (cl-loop for i below levels
                              do (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                              collect (list :id (format "%s%d" side i) :side side :level i
                                            :price (if (equal side "ask") (+ 100.01 (* 0.01 i))
                                                     (- 99.99 (* 0.01 i)))
                                            :size (+ 1 (% (/ state 7) 20))))))))

(defconst bench-frame-update--ladder
  (list :width 400 :height 300
        :encoding (list :y (list :field "price" :type "ordinal" :sort "descending"
                                 :axis (list :format ".2f")))
        :layer (vector (list :mark (list :type "bar")
                             :encoding (list :x (list :field "size" :type "quantitative"
                                                      :scale (list :domain [0 40]))
                                             :color (list :field "side" :type "nominal")))
                       (list :mark (list :type "text" :align "left")
                             :encoding (list :x (list :field "size" :type "quantitative")
                                             :text (list :field "size" :type "quantitative")))))
  "The ladder: one band per price, a fixed size domain.")

(defconst bench-frame-update--depth
  (list :width 400 :height 300
        :transform [(:window [(:op "sum" :field "size" :as "depth")]
                             :sort [(:field "level" :order "ascending")] :groupby ["side"])]
        :mark (list :type "area" :interpolate "step-after" :opacity 0.6)
        :encoding (list :x (list :field "price" :type "quantitative" :scale (list :domain [99.7 100.3]))
                        :y (list :field "depth" :type "quantitative" :scale (list :domain [0 600]))
                        :color (list :field "side" :type "nominal")))
  "Cumulative depth per side, fixed domains.")

(defconst bench-frame-update--depth-fixed
  (let ((spec (copy-tree bench-frame-update--depth)))
    (plist-put (plist-get spec :encoding) :color
               (list :field "side" :type "nominal" :scale (list :domain ["ask" "bid"])))
    spec)
  "The depth chart with every domain literal, colour too.")

(defconst bench-frame-update--candles
  (list :width 400 :height 300
        :transform [(:window [(:op "mean" :field "close" :as "sma")] :frame [-9 0]
                             :sort [(:field "t" :order "ascending")])
                    (:calculate "datum.close >= datum.open ? 'up' : 'down'" :as "dir")]
        :encoding (list :x (list :field "t" :type "quantitative" :scale (list :zero :false :nice :false)))
        :layer (vector (list :mark (list :type "rule")
                             :encoding (list :y (list :field "low" :type "quantitative"
                                                      :scale (list :domain [80 120]))
                                             :y2 (list :field "high")))
                       (list :mark (list :type "bar" :width 4)
                             :encoding (list :y (list :field "open" :type "quantitative")
                                             :y2 (list :field "close")
                                             :color (list :field "dir" :type "nominal"
                                                          :scale (list :domain ["up" "down"]
                                                                       :range ["#26a69a" "#ef5350"]))))
                       (list :mark (list :type "line" :color "orange")
                             :encoding (list :y (list :field "sma" :type "quantitative")))))
  "OHLC candles with a 10-candle moving average; a fixed price domain.")

(defun bench-frame-update--candle (seed t0)
  "Candle number T0 drawn from SEED."
  (let* ((r (lambda () (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
              (/ (% (/ seed 7) 1000) 1000.0)))
         (open (+ 95 (* 10 (funcall r)))) (close (+ 95 (* 10 (funcall r)))))
    (list :t t0 :open open :close close
          :high (+ (max open close) (* 3 (funcall r))) :low (- (min open close) (* 3 (funcall r))))))

(defun bench-frame-update--candle-stream (target)
  "A step function streaming candles: the last one updates every frame,
a new one opens every fourth frame; the last 60 are kept."
  (let ((view (eas-view-open (eas-json-encode bench-frame-update--candles) :id "candles" :target target
                             :size (bench-frame-update--size target)
                             :rows (vconcat (cl-loop for i below 60
                                                     collect (bench-frame-update--candle (1+ i) i))))))
    (cons view
          (lambda (i)
            (eas-dispatch view (list :type "push" :key "t" :window 60
                                     :rows (vector (bench-frame-update--candle (+ 100 i) (+ 60 (/ i 4))))))))))

(defun bench-frame-update--size (target)
  "The bench size for TARGET."
  (if (eq target 'text) '(:cols 100 :rows 40) '(640 . 360)))

(defun bench-frame-update--push (spec target &optional levels)
  "A step function pushing keyed level updates into a view of SPEC.
LEVELS per side (default 25); a fifth of the book changes per frame."
  (let* ((levels (or levels 25))
         (view (eas-view-open (eas-json-encode spec) :id "book" :target target
                              :size (bench-frame-update--size target)
                              :rows (bench-frame-update--book 1 levels)))
         (seed 1))
    (cons view
          (lambda (i)
            (let ((fresh (bench-frame-update--book (+ 2 i) levels)) (rows nil))
              (dotimes (_ (/ (* 2 levels) 5))
                (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
                (push (aref fresh (% seed (* 2 levels))) rows))
              (eas-dispatch view (list :type "push" :rows (vconcat rows) :key "id")))))))

(defvar bench-frame-update--clock 1.7e9 "The stepped play clock.")

(defun bench-frame-update--play (template target)
  "A step function ticking TEMPLATE's play."
  (let ((view (eas-play-open template :bindings (eas-template-example template)
                             :target target :size (and (eq target 'text) '(:cols 100 :rows 40)))))
    (cons view (lambda (_) (cl-incf bench-frame-update--clock 1.0)
                 (eas-play-tick view bench-frame-update--clock)))))

(defun bench-frame-update--slider (template target)
  "A step function moving pi-monte-carlo's slider, 500 points and up."
  (let ((view (eas-view-open template :bindings (plist-put (eas-template-example template) :points 500)
                             :target target :size (and (eq target 'text) '(:cols 100 :rows 40)))))
    (cons view (lambda (i) (eas-dispatch view (list :type "param" :param "num_points"
                                                    :value (+ 500 (* 5 (% i 40)))))))))

(defun bench-frame-update--hover (template target)
  "A step function sweeping the pointer over TEMPLATE."
  (let* ((view (eas-view-open template :bindings (eas-template-example template)
                              :target target :size (and (eq target 'text) '(:cols 100 :rows 40))))
         (size (plist-get (eas-view-scene view) :size))
         (w (or (plist-get size :w) 800)) (h (or (plist-get size :h) 500)))
    (cons view (lambda (i)
                 (eas-dispatch view (list :type "pointermove"
                                          :px (vector (* w (/ (+ 0.5 (% (* 7 i) 20)) 20.0))
                                                      (* h (/ (+ 0.5 (% (* 3 i) 10)) 10.0)))))))))

(defconst bench-frame-update-workloads
  `(("ladder push" ,(lambda (target) (bench-frame-update--push bench-frame-update--ladder target)))
    ("ladder-100 push" ,(lambda (target) (bench-frame-update--push bench-frame-update--ladder target 50)))
    ("depth push" ,(lambda (target) (bench-frame-update--push bench-frame-update--depth target)))
    ("depth-fixed push" ,(lambda (target) (bench-frame-update--push bench-frame-update--depth-fixed target)))
    ("candle stream" ,#'bench-frame-update--candle-stream)
    ("clock tick" ,(lambda (target) (bench-frame-update--play "clock" target)))
    ("pacman tick" ,(lambda (target) (bench-frame-update--play "pacman" target)))
    ("pi-mc step" ,(lambda (target) (bench-frame-update--slider "pi-monte-carlo" target)))
    ("airport hover" ,(lambda (target) (bench-frame-update--hover "airport-connections" target))))
  "(NAME MAKE): MAKE takes a target and returns (VIEW . STEP).")

(defun bench-frame-update--time (step)
  "Mean ms per call of STEP (of the frame number) and its allocation.
Returns (MS . CONSES-PER-FRAME), GC deferred as the glue defers it."
  (let ((gc-cons-threshold (* 64 1024 1024)) (n bench-frame-update-frames) (total 0.0)
        (alloc 0))
    (dotimes (i 3) (funcall step i))
    (garbage-collect)
    (dotimes (i n)
      (let ((a0 (apply #'+ (memory-use-counts))) (t0 (float-time)))
        (funcall step (+ 3 i))
        (cl-incf total (- (float-time) t0))
        (cl-incf alloc (- (apply #'+ (memory-use-counts)) a0))))
    (cons (/ (* 1000.0 total) n) (/ alloc n))))

(defun bench-frame-update-run ()
  "Rows (NAME TARGET MS ALLOC) over the workloads."
  (let ((eas-views (make-hash-table :test 'equal)))
    (cl-loop for (name make) in bench-frame-update-workloads
             when (string-match-p (or (getenv "EAS_BENCH_ONLY") "") name)
             append (cl-loop for target in '(svg text)
                             collect (let* ((made (funcall make target))
                                            (r (unwind-protect (bench-frame-update--time (cdr made))
                                                 (ignore-errors (eas-play-detach (car made)))
                                                 (eas-view-close (car made)))))
                                       (list name target (car r) (cdr r)))))))

(defun bench-frame-update-main ()
  "Entry point: read MODE [SRC] [FRAMES] from the command line, print the table."
  (let* ((args (if (equal (car command-line-args-left) "--") (cdr command-line-args-left)
                 command-line-args-left))
         (mode (intern (or (nth 0 args) "byte")))
         (src (expand-file-name (or (nth 1 args) (expand-file-name "src" bench-frame-update-root)))))
    (setq command-line-args-left nil)
    (when (nth 2 args) (setq bench-frame-update-frames (string-to-number (nth 2 args))))
    (bench-frame-update--setup mode src)
    (require 'eas-view)
    (require 'eas-play)
    (princ (format "mode %s, %s, %d frames, %s\n\n" mode src bench-frame-update-frames emacs-version))
    (princ "| workload | target | ms/frame | conses/frame |\n|---|---|---:|---:|\n")
    (dolist (r (bench-frame-update-run))
      (princ (format "| %s | %s | %.2f | %d |\n" (nth 0 r) (nth 1 r) (nth 2 r) (nth 3 r))))))

(defvar bench-frame-update-no-main nil "Non-nil loads this file without running the bench.")

(when (and noninteractive (not bench-frame-update-no-main)) (bench-frame-update-main))

(provide 'bench-frame-update)
;;; bench-frame-update.el ends here

;;; bench-frame-svg.el --- ms per live SVG frame, by stage and compile mode -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-b2s.2.  Milliseconds per frame of the live SVG workloads: a
;; 25-level order-book ladder push and a depth push (10 levels change
;; per push), clock and pacman timer ticks, a pi-monte-carlo slider step
;; (10 more samples) and an airport-connections hover.  A frame is the
;; update (push, tick, slider or pointermove) and what the GUI glue
;; does to draw it, `eas-mode-redraw' in an `eas-view-mode' buffer: the
;; SVG string, the :map hot spots, the image inserted and its hot-spot
;; keys bound.  The rasterization librsvg does in a GUI frame is not in
;; a batch Emacs.
;;
;;   emacs -Q --batch -l scripts/bench-frame-svg.el -- MODE [SRC] [FRAMES]
;;
;; MODE is `interpreted' (load SRC's .el files), `byte' (byte-compile
;; a copy of SRC first) or `native' (byte- and native-compile the copy).
;; SRC defaults to the src/ next to this script, so a checkout of an
;; older commit benches the same way.  Compiled copies are kept under
;; the temporary directory, keyed by the sources' hash.  It prints a
;; Markdown table: update, draw and total ms per frame, the mean over
;; FRAMES (default 40) frames after a warm-up frame.

;;; Code:

(require 'cl-lib)

(defvar bench-frame-svg-frames 40 "Frames timed per workload.")

(defun bench-frame-svg--sources (src)
  "Return the library files of SRC, leaving out the tests."
  (cl-remove-if (lambda (f) (string-suffix-p "-test.el" f))
                (directory-files src t "\\`eas.*\\.el\\'")))

(defun bench-frame-svg--copy (src mode)
  "A directory holding SRC's libraries compiled for MODE; return it."
  (let* ((files (bench-frame-svg--sources src))
         (hash (secure-hash 'sha1 (mapconcat (lambda (f) (with-temp-buffer
                                                           (insert-file-contents-literally f)
                                                           (buffer-string)))
                                             files "")))
         (dir (expand-file-name (format "bench-frame-svg-%s-%s" mode (substring hash 0 12))
                                temporary-file-directory))
         (stamp (expand-file-name ".done" dir)))
    (unless (file-exists-p stamp)
      (make-directory dir t)
      (dolist (f files) (copy-file f (expand-file-name (file-name-nondirectory f) dir) t))
      (dolist (f (directory-files src t "\\.json\\'"))
        (copy-file f (expand-file-name (file-name-nondirectory f) dir) t))
      ;; Laid out flat, as MELPA installs it: the data hangs off the copy.
      (dolist (d '("templates" "examples" "test"))
        (make-symbolic-link (expand-file-name d (file-name-directory (directory-file-name src)))
                            (expand-file-name d dir) t))
      (let ((copies (mapcar (lambda (f) (expand-file-name (file-name-nondirectory f) dir)) files))
            (eln (expand-file-name "eln" dir)))
        (bench-frame-svg--parallel
         (mapcar (lambda (f)
                   (append (list "-Q" "--batch" "-L" dir
                                 ;; batch-byte+native-compile writes to the last directory.
                                 "--eval" (format "(setq native-comp-eln-load-path (list %S))" eln))
                           (if (eq mode 'native)
                               (list "--eval" "(setq native-comp-speed 2)" "-f" "batch-byte+native-compile" f)
                             (list "-f" "batch-byte-compile" f))))
                 copies)))
      (write-region "" nil stamp))
    dir))

(defun bench-frame-svg--parallel (arglists)
  "Run Emacs once per ARGLISTS, eight at a time; wait for them all."
  (let ((queue arglists) (running nil) (emacs (expand-file-name invocation-name invocation-directory)))
    (while (or queue running)
      (while (and queue (< (length running) 8))
        (push (make-process :name "bench-compile" :command (cons emacs (pop queue))
                            :buffer nil :stderr (get-buffer-create " *bench-compile*"))
              running))
      (accept-process-output nil 0.05)
      (setq running (cl-remove-if-not #'process-live-p running)))))

(defun bench-frame-svg--setup (mode src)
  "Put SRC (compiled for MODE) on the load path and load the engine."
  (let ((dir (if (eq mode 'interpreted) src (bench-frame-svg--copy src mode))))
    (when (eq mode 'native)
      (setq native-comp-eln-load-path (list (expand-file-name "eln" dir)
                                            (car (last native-comp-eln-load-path)))))
    (setq load-prefer-newer nil)
    (push dir load-path)
    (dolist (f '(eas eas-view eas-play eas-svg eas-mode eas-template))
      (require f))
    (let ((lib (locate-library "eas-svg")))
      (unless (string-suffix-p (if (eq mode 'interpreted) ".el" ".elc") lib)
        (error "Expected a %s eas-svg, got %s" mode lib))
      (when (eq mode 'native)
        (unless (native-comp-function-p (symbol-function 'eas-svg-render))
          (error "Eas-svg-render is not native-compiled"))))))

;;; Workloads

(defun bench-frame-svg--book (seed levels)
  "Rows of an order book with LEVELS levels a side, sizes drawn from SEED."
  (let ((state seed))
    (vconcat
     (cl-loop for side in '("ask" "bid")
              append (cl-loop for i below levels
                              do (setq state (% (+ (* state 1103515245) 12345) 2147483648))
                              collect (list :side side :level i
                                            :price (if (equal side "ask") (+ 100.01 (* 0.01 i))
                                                     (- 99.99 (* 0.01 i)))
                                            :size (+ 1 (% (/ state 7) 20))))))))

(defun bench-frame-svg--depth-rows (book)
  "BOOK's rows with :depth, the cumulative size from the touch out."
  (let ((acc (list (cons "ask" 0) (cons "bid" 0))))
    (vconcat (mapcar (lambda (r)
                       (let ((cell (assoc (plist-get r :side) acc)))
                         (setcdr cell (+ (cdr cell) (plist-get r :size)))
                         (append r (list :depth (cdr cell)))))
                     book))))

(defconst bench-frame-svg--ladder
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
  "A 2x25-level ladder: fixed size domain, one band per price.")

(defconst bench-frame-svg--depth
  (list :width 400 :height 300
        :mark (list :type "area" :interpolate "step-after" :fillOpacity 0.4 :line t)
        :encoding (list :x (list :field "price" :type "quantitative" :scale (list :domain [99.7 100.3]))
                        :y (list :field "depth" :type "quantitative" :scale (list :domain [0 600]))
                        :color (list :field "side" :type "nominal")))
  "A depth chart over the same book: fixed domains, an area per side.")

(defvar eas-mode--view)
(defvar eas-play-clock)
(defvar bench-frame-svg--buffer nil "The buffer showing the view being timed.")

(defun bench-frame-svg--draw (view)
  "Draw VIEW's scene as the GUI glue does, into an `eas-view-mode' buffer."
  (unless (and (buffer-live-p bench-frame-svg--buffer)
               (eq (buffer-local-value 'eas-mode--view bench-frame-svg--buffer) view))
    (when (buffer-live-p bench-frame-svg--buffer) (kill-buffer bench-frame-svg--buffer))
    (setq bench-frame-svg--buffer (generate-new-buffer " *bench-frame-svg*"))
    (with-current-buffer bench-frame-svg--buffer
      (eas-view-mode)
      (setq eas-mode--view view)))
  (eas-mode-redraw bench-frame-svg--buffer))

(defun bench-frame-svg--time (view step)
  "Mean (UPDATE DRAW) ms per frame of STEP, then a draw of VIEW.
STEP is called with the frame number."
  (let ((update 0.0) (draw 0.0))
    (funcall step 0) (bench-frame-svg--draw view)
    (garbage-collect)
    (dotimes (i bench-frame-svg-frames)
      (let ((t0 (float-time)))
        (funcall step (1+ i))
        (let ((t1 (float-time)))
          (bench-frame-svg--draw view)
          (cl-incf update (- t1 t0))
          (cl-incf draw (- (float-time) t1)))))
    (list (/ (* 1000 update) bench-frame-svg-frames) (/ (* 1000 draw) bench-frame-svg-frames))))

(defun bench-frame-svg--push (spec rows-fn)
  "Ms per 10-level push of a 25-level book into SPEC; ROWS-FN maps the book."
  (let* ((book (bench-frame-svg--book 1 25))
         (view (eas-view-open (eas-json-encode spec) :id "bench-book" :size '(400 . 300)
                              :rows (funcall rows-fn book)))
         (seed 1))
    (unwind-protect
        (bench-frame-svg--time
         view (lambda (i)
                (let ((fresh (bench-frame-svg--book (+ 2 i) 25)))
                  (setq book (copy-sequence book))
                  (dotimes (_ 10)
                    (setq seed (% (+ (* seed 1103515245) 12345) 2147483648))
                    (aset book (% seed 50) (aref fresh (% seed 50)))))
                (eas-dispatch view (list :type "push" :rows (funcall rows-fn book) :window 50))))
      (eas-view-close view))))

(defun bench-frame-svg--play (template)
  "Ms per timer tick of vega TEMPLATE."
  (let* ((clock 1.7e9)
         (eas-play-clock (lambda () clock))
         (view (eas-play-open template :bindings (eas-template-example template) :target 'svg)))
    (unwind-protect
        (bench-frame-svg--time view (lambda (_) (cl-incf clock 1.0) (eas-play-tick view)))
      (eas-play-detach view) (eas-view-close view))))

(defun bench-frame-svg--slide (template param from step)
  "Ms per step of vega TEMPLATE's slider PARAM, from FROM by STEP."
  (let ((view (eas-view-open template :bindings (eas-template-example template) :target 'svg)))
    (unwind-protect
        (bench-frame-svg--time
         view (lambda (i) (eas-dispatch view (list :type "param" :param param :value (+ from (* i step))))))
      (eas-view-close view))))

(defun bench-frame-svg--hover (template)
  "Ms per pointermove of vega TEMPLATE, over its points in turn."
  (let* ((view (eas-view-open template :bindings (eas-template-example template) :target 'svg))
         (items (seq-some (lambda (m) (and (member (plist-get m :mark) '("circle" "point" "symbol"))
                                           (> (length (plist-get m :items)) 0)
                                           (plist-get m :items)))
                          (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks))))
    (unwind-protect
        (bench-frame-svg--time
         view (lambda (i)
                (let ((item (aref items (% (* 7 i) (length items)))))
                  (eas-dispatch view (list :type "pointermove"
                                           :px (vector (plist-get item :x) (plist-get item :y)))))))
      (eas-view-close view))))

(defun bench-frame-svg-rows ()
  "Rows (NAME UPDATE DRAW) over the live SVG workloads."
  (list (cons "ladder push (25 levels)" (bench-frame-svg--push bench-frame-svg--ladder #'identity))
        (cons "depth push (25 levels)" (bench-frame-svg--push bench-frame-svg--depth
                                                              #'bench-frame-svg--depth-rows))
        (cons "clock tick" (bench-frame-svg--play "clock"))
        (cons "pacman tick" (bench-frame-svg--play "pacman"))
        (cons "pi-monte-carlo slider step" (bench-frame-svg--slide "pi-monte-carlo" "num_points" 1000 10))
        (cons "airport-connections hover" (bench-frame-svg--hover "airport-connections"))))

(defun bench-frame-svg-main ()
  "Bench per the command line: MODE [SRC] [FRAMES]."
  (let* ((args (if (equal (car command-line-args-left) "--") (cdr command-line-args-left)
                 command-line-args-left))
         (mode (intern (or (nth 0 args) "interpreted")))
         (src (directory-file-name
               (if (nth 1 args) (expand-file-name (nth 1 args))
                 (expand-file-name "../src" (file-name-directory (or load-file-name buffer-file-name)))))))
    (setq command-line-args-left nil)
    (when (nth 2 args) (setq bench-frame-svg-frames (string-to-number (nth 2 args))))
    (unless (memq mode '(interpreted byte native)) (error "MODE is interpreted, byte or native"))
    (bench-frame-svg--setup mode src)
    (setq gc-cons-threshold (* 64 1024 1024))
    (princ (format "mode %s, %s, %d frames\n\n" mode src bench-frame-svg-frames))
    (princ "| workload | update ms | draw ms | frame ms |\n|---|---:|---:|---:|\n")
    (dolist (r (bench-frame-svg-rows))
      (princ (format "| %s | %.2f | %.2f | %.2f |\n" (nth 0 r) (nth 1 r) (nth 2 r) (+ (nth 1 r) (nth 2 r)))))))

(when (and noninteractive (not (bound-and-true-p bench-frame-svg-no-main))) (bench-frame-svg-main))

(provide 'bench-frame-svg)
;; The engine is loaded at run time, from the directory MODE picks.
;; Local Variables:
;; byte-compile-warnings: (not unresolved)
;; End:

;;; bench-frame-svg.el ends here

;;; eas-animate.el --- turn a list of events into frames and a GIF -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Drives a live view headless with event/v1 events (`eas-dispatch'),
;; renders a frame after each step (`eas-svg-render', or the text
;; renderer drawn as SVG cells), rasterizes the frames with
;; rsvg-convert and assembles a looping GIF with ImageMagick, then
;; shrinks it with gifsicle when one is on PATH.
;;
;;   emacs -Q --batch -L src -l scripts/eas-animate.el \
;;         -f eas-animate-batch scripts/animations/NAME.json
;;
;; A job is JSON: the template (its example binding unless "bindings"
;; is given), "fps", "width", "steps" and "outputs", each output an
;; object {"out", "target" ("svg" or "text"), "size" ({"cols", "rows"}
;; for text)} drawing the same steps.  A step is one of
;;
;;   {"type": "pointermove", "px": [x, y], ...}  an event/v1, one frame
;;   {"pointer": [x, y]}                          place the pointer
;;   {"hold": N}                                  N frames, no event
;;   {"glide": TARGET, "frames": N}               N eased pointermoves
;;   {"visit": {"mark", "key", "by", "ranks" or "values"},
;;    "glide": N, "hold": H}                      glide to each datum and hold
;;
;; where TARGET is [x, y] or {"mark", "key", "value"}: the item of the
;; first MARK (a mark type) whose row has KEY equal to VALUE, located
;; in the output's own scene, so one job drives the SVG and the text
;; renderer alike.  "visit" picks rows by KEY, ranked by BY (largest
;; first, RANKS 1-based) or listed by VALUES.  The pointer is drawn as
;; an arrow; outside the canvas it sends one pointerleave and no moves.
;; Once it has rested "tipDelay" frames, the tooltip of the datum
;; under it shows as Emacs would show it: a tooltip box in SVG, an
;; echo-area line under the text.  Identical consecutive frames merge
;; into one longer GIF frame.
;;
;; Tools come from RSVG_CONVERT, MAGICK and GIFSICLE, else PATH.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'xml)
(require 'eas)

(defvar eas-animate-rsvg (or (getenv "RSVG_CONVERT") "rsvg-convert")
  "The rsvg-convert program.")

(defvar eas-animate-magick
  (or (getenv "MAGICK") (executable-find "magick") (executable-find "magick-im7.q16") "convert")
  "The ImageMagick program that assembles the GIF.")

(defvar eas-animate-gifsicle (or (getenv "GIFSICLE") (executable-find "gifsicle"))
  "The gifsicle program that optimizes the GIF, or nil.")

(defvar eas-animate-text-font "DejaVu Sans Mono, DejaVu Sans"
  "Monospace font family list the text renderer's frames are drawn in.")

(defun eas-animate-ease (u)
  "Cubic ease-in-out of U in [0, 1]."
  (if (< u 0.5) (* 4 u u u) (- 1 (/ (expt (- 2 (* 2 u)) 3) 2.0))))

;;; Locating data in a scene

(defun eas-animate--marks (scene type)
  "Every mark of SCENE whose type is TYPE, over all views."
  (seq-mapcat (lambda (v) (seq-filter (lambda (m) (equal (plist-get m :mark) type))
                                      (plist-get v :marks)))
              (plist-get scene :views)))

(defun eas-animate--field (row key)
  "ROW's field KEY (a string)."
  (plist-get row (intern (concat ":" key))))

(defun eas-animate--rows (scene type)
  "Pairs (ROW . [X Y]) of every item of SCENE's marks of TYPE."
  (seq-mapcat (lambda (m)
                (mapcar (lambda (it) (cons (aref (plist-get m :rows) (plist-get it :datum))
                                           (vector (plist-get it :x) (plist-get it :y))))
                        (plist-get m :items)))
              (eas-animate--marks scene type)))

(defun eas-animate-locate (scene target)
  "Pixel [X Y] of TARGET in SCENE.
TARGET is [X Y] or (:mark TYPE :key K :value V)."
  (if (vectorp target) target
    (or (cdr (seq-find (lambda (p) (equal (eas-animate--field (car p) (plist-get target :key))
                                          (plist-get target :value)))
                       (eas-animate--rows scene (plist-get target :mark))))
        (error "No %s item with %s = %s" (plist-get target :mark)
               (plist-get target :key) (plist-get target :value)))))

(defun eas-animate-visit-values (scene visit)
  "The KEY values VISIT (:mark :key :by :ranks or :values) picks in SCENE."
  (or (append (plist-get visit :values) nil)
      (let* ((key (plist-get visit :key)) (by (plist-get visit :by)) seen)
        (dolist (p (eas-animate--rows scene (plist-get visit :mark)))
          (unless (assoc (eas-animate--field (car p) key) seen)
            (push (cons (eas-animate--field (car p) key) (eas-animate--field (car p) by)) seen)))
        (let ((ranked (sort seen (lambda (a b) (> (cdr a) (cdr b))))))
          (mapcar (lambda (r) (car (nth (1- r) ranked))) (plist-get visit :ranks))))))

;;; Planning frames

(defun eas-animate-plan (scene steps)
  "Frames for STEPS in SCENE: a list of (:pointer [X Y] :move BOOL :event EV).
:move says the frame moves the pointer to :pointer; :event is an
explicit event/v1 to dispatch."
  (let (frames pointer)
    (cl-labels ((frame (&rest props) (push props frames))
                (glide (to n)
                  (let ((from (or pointer to)))
                    (dotimes (i n)
                      (let ((u (eas-animate-ease (/ (1+ i) (float n)))))
                        (setq pointer (vector (+ (aref from 0) (* u (- (aref to 0) (aref from 0))))
                                              (+ (aref from 1) (* u (- (aref to 1) (aref from 1))))))
                        (frame :pointer pointer :move t)))))
                (hold (n) (dotimes (_ n) (frame :pointer pointer))))
      (seq-doseq (step steps)
        (cond ((plist-get step :type)
               (when (plist-get step :px) (setq pointer (plist-get step :px)))
               (frame :pointer pointer :event step))
              ((plist-get step :pointer) (setq pointer (plist-get step :pointer)))
              ((plist-get step :visit)
               (let ((visit (plist-get step :visit)))
                 (dolist (value (eas-animate-visit-values scene visit))
                   (glide (eas-animate-locate scene (list :mark (plist-get visit :mark)
                                                          :key (plist-get visit :key) :value value))
                          (or (plist-get step :glide) 10))
                   (hold (or (plist-get step :hold) 12)))))
              ((plist-get step :glide)
               (glide (eas-animate-locate scene (plist-get step :glide)) (or (plist-get step :frames) 10)))
              ((plist-get step :hold) (hold (plist-get step :hold)))
              (t (error "Unknown animation step %S" step)))))
    (nreverse frames)))

;;; Drawing a frame

(defun eas-animate--inside-p (scene px)
  "Non-nil when PX lies on SCENE's canvas."
  (let ((size (plist-get scene :size)))
    (and px (<= 0 (aref px 0) (plist-get size :w)) (<= 0 (aref px 1) (plist-get size :h)))))

(defun eas-animate-tip (scene px)
  "Tooltip text of the datum under PX in SCENE, or nil."
  (when-let* ((px px) (hit (eas-tip-hit scene px)))
    (eas-tip-text (eas-tip-tooltip scene nil hit))))

(defun eas-animate--pointer-svg (px)
  "An arrow pointer at PX as SVG."
  (format (concat "<path transform=\"translate(%.1f %.1f)\" d=\"M0 0V17L4.5 12.8L7.6 19.6"
                  "L10.4 18.4L7.4 11.8L13 11.6Z\" fill=\"black\" stroke=\"white\""
                  " stroke-width=\"1.2\" stroke-linejoin=\"round\"/>")
          (aref px 0) (aref px 1)))

(defun eas-animate--tip-svg (px text w h)
  "TEXT in a tooltip box beside PX, kept on a W by H canvas."
  (let* ((lines (split-string text "\n"))
         (bw (+ 12 (* 6.6 (apply #'max (mapcar #'length lines)))))
         (bh (+ 8 (* 15 (length lines))))
         (x (min (+ (aref px 0) 14) (- w bw 2)))
         (y (if (> (+ (aref px 1) 22 bh) h) (- (aref px 1) bh 6) (+ (aref px 1) 22)))
         (x (if (< x 2) 2 x)))
    (concat (format "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%d\" fill=\"#ffffe1\" stroke=\"#767676\"/>"
                    x y bw bh)
            (cl-loop for line in lines for i from 0
                     concat (format "<text x=\"%.1f\" y=\"%.1f\" font-family=\"sans-serif\" font-size=\"12\" fill=\"black\">%s</text>"
                                    (+ x 6) (+ y 16 (* 15 i)) (xml-escape-string line))))))

(defun eas-animate--overlay (svg extra)
  "SVG with EXTRA (SVG elements) drawn on top."
  (let ((end (string-search "</svg>" svg (- (length svg) 16))))
    (concat (substring svg 0 end) extra (substring svg end))))

(defun eas-animate-text-svg (text cell &optional echo)
  "SVG of TEXT, the text renderer's propertized string, in CELL [W H] cells.
ECHO, a string, is drawn as an echo-area line under it."
  (let* ((rows (split-string text "\n"))
         (cw (aref cell 0)) (ch (aref cell 1))
         (cols (apply #'max 1 (mapcar #'length rows)))
         (w (* cols cw)) (h (* (+ (length rows) (if echo 1 0)) ch))
         (bg (eas-text-ink-background)) (fg (eas-text-ink-foreground))
         (font (format "font-family=\"%s\" font-size=\"%.2f\" xml:space=\"preserve\""
                       eas-animate-text-font (/ cw 0.602))))
    (with-temp-buffer
      (insert (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\"><rect width=\"100%%\" height=\"100%%\" fill=\"%s\"/>"
                      w h bg))
      (cl-loop for row in rows for r from 0
               do (let ((i 0))
                    (while (< i (length row))
                      (let* ((next (or (next-single-property-change i 'face row) (length row)))
                             (run (substring-no-properties row i next))
                             (face (get-text-property i 'face row))
                             (color (or (and (consp face) (plist-get face :foreground)) fg)))
                        (unless (string-blank-p run)
                          ;; Each glyph at its cell: braille falls back
                          ;; to a font whose advance is not the cell's.
                          (insert (format "<text x=\"%s\" y=\"%.1f\" fill=\"%s\" %s>%s</text>"
                                          (mapconcat (lambda (k) (number-to-string (* (+ i k) cw)))
                                                     (number-sequence 0 (1- (length run))) " ")
                                          (+ (* r ch) (* 0.8 ch)) color font (xml-escape-string run))))
                        (setq i next)))))
      (when echo
        (insert (format "<text x=\"0\" y=\"%.1f\" fill=\"%s\" %s>%s</text>"
                        (+ (* (length rows) ch) (* 0.8 ch)) fg font (xml-escape-string echo))))
      (insert "</svg>")
      (buffer-string))))

(defun eas-animate-render (view pointer tip)
  "SVG of VIEW's scene with POINTER ([X Y] or nil) and its tooltip when TIP."
  (let* ((scene (eas-view-scene view))
         (size (plist-get scene :size))
         (text (and tip (eas-animate-tip scene pointer))))
    (if (equal (plist-get scene :target) "text")
        (eas-animate--overlay
         (eas-animate-text-svg (eas-text-render scene) (plist-get size :cell)
                               (if text (string-replace "\n" "   " text) ""))
         (if pointer (eas-animate--pointer-svg pointer) ""))
      (eas-animate--overlay (eas-svg-render scene)
                            (concat (if pointer (eas-animate--pointer-svg pointer) "")
                                    (if text (eas-animate--tip-svg pointer text (plist-get size :w)
                                                                   (plist-get size :h))
                                      ""))))))

(cl-defun eas-animate-frames (view frames dir &key (fps 12) (tip-delay 6))
  "Dispatch FRAMES (from `eas-animate-plan') to VIEW, writing SVGs into DIR.
Return ((FILE . CENTISECONDS) ...) at FPS, identical frames merged.
The tooltip shows once the pointer has rested TIP-DELAY frames."
  (make-directory dir t)
  (let ((still 0) inside last out (i 0))
    (dolist (f frames)
      (let* ((scene (eas-view-scene view))
             (px (plist-get f :pointer))
             (on (eas-animate--inside-p scene px)))
        (cond ((plist-get f :event) (eas-dispatch view (plist-get f :event)))
              ((and (plist-get f :move) on)
               (eas-dispatch view (list :type "pointermove" :px px)))
              ((and (plist-get f :move) inside) (eas-dispatch view '(:type "pointerleave"))))
        (setq inside on still (if (plist-get f :move) 0 (1+ still)))
        (let ((svg (eas-animate-render view (and on px) (>= still tip-delay)))
              (end (round (* 100.0 (1+ i)) fps)) (start (round (* 100.0 i) fps)))
          (if (equal svg last)
              (setcdr (car out) (+ (cdar out) (- end start)))
            (let ((file (expand-file-name (format "frame-%04d.svg" (length out)) dir)))
              (with-temp-file file (set-buffer-file-coding-system 'utf-8-unix) (insert svg))
              (push (cons file (- end start)) out)
              (setq last svg))))
        (cl-incf i)))
    (nreverse out)))

;;; Assembling the GIF

(defun eas-animate--run (program &rest args)
  "Run PROGRAM with ARGS; signal an error with its output when it fails."
  (with-temp-buffer
    (unless (eql 0 (apply #'call-process program nil t nil args))
      (error "%s failed: %s" program (buffer-string)))))

(defun eas-animate-gif (frames out width)
  "Rasterize FRAMES ((SVG . CENTISECONDS) ...) WIDTH px wide into GIF OUT."
  (let (args)
    (dolist (f frames)
      (let ((png (concat (file-name-sans-extension (car f)) ".png")))
        (eas-animate--run eas-animate-rsvg "-b" "white" "-w" (number-to-string width) "-o" png (car f))
        (setq args (append args (list "-delay" (number-to-string (cdr f)) png)))))
    (make-directory (file-name-directory (expand-file-name out)) t)
    (apply #'eas-animate--run eas-animate-magick
           (append '("-loop" "0") args '("+dither" "-colors" "255" "-layers" "Optimize") (list out)))
    (when eas-animate-gifsicle
      (eas-animate--run eas-animate-gifsicle "-b" "-O3" "--lossy=30" out))
    out))

(defun eas-animate-job (job &optional root)
  "Run JOB (a plist read from a job file); paths are relative to ROOT.
Return the GIFs written."
  (let ((root (or root eas-template--root)) written)
    (seq-doseq (output (plist-get job :outputs))
      (let* ((template (plist-get job :template))
             (target (intern (or (plist-get output :target) "svg")))
             (size (and (plist-get output :size)
                        (list :cols (plist-get (plist-get output :size) :cols)
                              :rows (plist-get (plist-get output :size) :rows))))
             (view (eas-view-open template :bindings (or (plist-get job :bindings) (eas-template-example template))
                                  :target target :size size))
             (out (expand-file-name (plist-get output :out) root))
             (dir (make-temp-file "eas-animate-" t))
             (frames (eas-animate-frames view (eas-animate-plan (eas-view-scene view) (plist-get job :steps)) dir
                                         :fps (or (plist-get job :fps) 12)
                                         :tip-delay (or (plist-get job :tipDelay) 6))))
        (message "%s: %d distinct frames in %s" (plist-get output :out) (length frames) dir)
        (eas-animate-gif frames out (or (plist-get output :width) (plist-get job :width) 900))
        (message "wrote %s (%d bytes)" out (file-attribute-size (file-attributes out)))
        (eas-view-close view)
        (unless (getenv "EAS_ANIMATE_KEEP") (delete-directory dir t))
        (push out written)))
    (nreverse written)))

(defun eas-animate-batch ()
  "Run each job file named on the command line (see the commentary)."
  (dolist (file command-line-args-left)
    (eas-animate-job (eas-json-read-file file)))
  (setq command-line-args-left nil))

(provide 'eas-animate)
;;; eas-animate.el ends here

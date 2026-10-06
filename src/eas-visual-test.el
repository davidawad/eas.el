;;; eas-visual-test.el --- text invariants from the live terminal review (fc-qx1.51) -*- lexical-binding: t; -*-

;;; Commentary:

;; Each test pins one defect seen by eye in a dark 189x56 terminal:
;; the scene or text invariant that fails when it comes back.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-mode)
(require 'eas-text-check)
(require 'eas-text-gallery)

(defmacro eas-visual-test--with (bindings &rest body)
  "Fresh registry, quiet hooks; BINDINGS as in `let*'; then BODY."
  (declare (indent 1))
  `(let ((eas-views (make-hash-table :test 'equal))
         (eas-brush-functions nil) (eas-action-inhibit t) (inhibit-message t))
     (let* ,bindings ,@body)))

(defun eas-visual-test--ohlc-volume (size)
  "The ohlc template with its volume pane, opened as text at SIZE."
  (eas-view-open "ohlc" :bindings (plist-put (copy-sequence (eas-template-example "ohlc")) :volume t)
                 :target 'text :size size))

(defun eas-visual-test--width (text)
  "Widest line of TEXT, in columns."
  (apply #'max (mapcar #'string-width (split-string text "\n"))))

;;; 1. Charts fill their window

(ert-deftest eas-visual-vconcat-panes-share-width-and-fit ()
  (eas-visual-test--with ()
    (dolist (size '((:cols 189 :rows 56) (:cols 94 :rows 28) (:cols 60 :rows 16)))
      (let* ((scene (eas-view-scene (eas-visual-test--ohlc-volume size)))
             (views (append (plist-get scene :views) nil))
             (cw (aref (plist-get (plist-get scene :size) :cell) 0)))
        (should (= (length views) 2))
        ;; Price and volume panes: one x range.
        (should (equal (aref (plist-get (nth 0 views) :bounds) 0) (aref (plist-get (nth 1 views) :bounds) 0)))
        (should (equal (aref (plist-get (nth 0 views) :bounds) 2) (aref (plist-get (nth 1 views) :bounds) 2)))
        (should (= (plist-get (plist-get scene :size) :w) (* cw (plist-get size :cols))))
        (should (<= (eas-visual-test--width (eas-text-render scene)) (plist-get size :cols)))))))

(ert-deftest eas-visual-chart-follows-its-window ()
  (eas-visual-test--with ((size '(:cols 100 :rows 30))
                          (view (eas-visual-test--ohlc-volume '(:cols 60 :rows 16)))
                          (buffer nil))
    (unwind-protect
        (cl-letf (((symbol-function 'eas-mode--window-size) (lambda (_window _target) size)))
          (setq buffer (eas-show view 'text))
          (with-current-buffer buffer
            (should (memq #'eas-mode--follow-window window-size-change-functions))
            (should (equal (eas-view-size view) '(:cols 100 :rows 30)))
            ;; The window grows (C-x 1 from a split): the chart follows.
            (setq size '(:cols 189 :rows 56))
            (eas-mode--follow-window (get-buffer-window buffer))
            (should (equal (eas-view-size view) size))
            ;; The redraw runs from a timer, after redisplay (fc-qx1.53).
            (accept-process-output nil 0.05)
            (let ((lines (split-string (buffer-string) "\n")))
              (should (= (apply #'max (mapcar #'string-width lines)) 189))))
          ;; The hook may run with another buffer current.
          (setq size '(:cols 94 :rows 27))
          (with-temp-buffer (eas-mode--follow-window (get-buffer-window buffer)))
          (should (equal (eas-view-size view) size)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

;;; 2. Ink on a dark terminal

(defun eas-visual-test--gallery-scene (group name size)
  "Gallery example NAME of GROUP compiled for text at SIZE."
  (eas-vl-gallery--native
   (eas-compile (eas-text-gallery-spec group name) :target 'text :size size)))

(defun eas-visual-test--foregrounds (text &optional mark)
  "Foreground colors of TEXT's non-blank glyphs (of MARK when non-nil)."
  (let ((pos 0) out)
    (while (< pos (length text))
      (let ((next (or (next-single-property-change pos 'face text) (length text)))
            (face (get-text-property pos 'face text)))
        (when (and (consp face) (plist-get face :foreground)
                   (or (null mark) (equal (get-text-property pos 'eas-mark text) mark))
                   (string-match-p "[^[:space:]]" (substring text pos next)))
          (cl-pushnew (plist-get face :foreground) out :test #'equal))
        (setq pos next)))
    out))

(ert-deftest eas-visual-ink-follows-the-background ()
  (dolist (example '(("distributions" "layer_point_errorbar_ci") ("layered" "layer_bar_annotations")))
    (let ((scene (apply #'eas-visual-test--gallery-scene (append example '((:cols 94 :rows 28))))))
      ;; Without legible ink a dark terminal draws the theme's black rules.
      (cl-letf (((symbol-function 'eas-text-ink-legible) (lambda (c &optional _) c)))
        (should (eas-text-check-contrast scene 'dark)))
      (dolist (mode '(light dark))
        (should-not (eas-text-check-contrast scene mode))
        (let ((colors (eas-visual-test--foregrounds (let ((eas-text-background-mode mode)) (eas-text-render scene)))))
          (should colors)
          (dolist (c colors)
            (should (>= (eas-text-ink-contrast c (eas-text-ink-background mode)) 3))))))))

(ert-deftest eas-visual-ink-keeps-legible-spec-colors ()
  (should (equal (eas-text-ink-legible "#e34948" 'dark) "#e34948"))
  (should (equal (eas-text-ink-legible "#e34948" 'light) "#e34948"))
  ;; Neutral ink becomes the default face's foreground.
  (should (equal (eas-text-ink-legible "#0b0b0b" 'dark) (eas-text-ink-foreground 'dark)))
  (should (equal (eas-text-ink-legible "black" 'light) "black"))
  ;; CSS rgb() parses; transparent is no color at all.
  (should (equal (eas-text-ink-legible "rgb(93,93,93)" 'dark) (eas-text-ink-foreground 'dark)))
  (should (equal (eas-text-ink-legible "rgb(227, 73, 72)" 'dark) "rgb(227, 73, 72)"))
  (should-not (eas-text-ink-legible "transparent" 'dark))
  ;; A hue too dark for the background lightens, keeping its hue.
  (let ((c (eas-text-ink-legible "#4a3aa7" 'dark)))
    (should (>= (eas-text-ink-contrast c (eas-text-ink-background 'dark)) 3))
    (should (< (abs (- (car (apply #'color-rgb-to-hsl (eas-text-ink--rgb c)))
                       (car (apply #'color-rgb-to-hsl (eas-text-ink--rgb "#4a3aa7")))))
               0.02))))

;;; 3. Stacked areas tile

(defun eas-visual-test--stack-holes (scene)
  "Cells inside SCENE's stacked areas (below the stack's top cell, above
its bottom cell) that are neither full blocks nor backed by the slice
above: holes, as (COL . ROW)."
  (let* ((text (eas-text-render scene)) (lines (vconcat (split-string text "\n")))
         (starts (let ((off 0)) (vconcat (mapcar (lambda (l) (prog1 off (setq off (+ off 1 (length l))))) lines))))
         (cell (plist-get (plist-get scene :size) :cell)) (cw (aref cell 0)) (ch (aref cell 1))
         (out nil))
    (seq-doseq (view (plist-get scene :views))
      (let* ((areas (seq-filter (lambda (m) (equal (plist-get m :mark) "area")) (plist-get view :marks)))
             (items (seq-mapcat (lambda (m) (append (plist-get m :items) nil)) areas))
             (b (plist-get view :bounds)))
        (cl-loop for col from (ceiling (aref b 0) cw) below (floor (+ (aref b 0) (aref b 2)) cw)
                 for cx = (* (+ col 0.5) cw)
                 for ys = (cl-loop for it in items
                                   for p = (eas-text--interp (plist-get it :points) cx)
                                   for q = (eas-text--interp (plist-get it :base) cx)
                                   when (and p q) collect p and collect q)
                 when ys
                 do (cl-loop for row from (1+ (floor (apply #'min ys) ch)) below (1- (floor (apply #'max ys) ch))
                             for line = (aref lines row)
                             for char = (if (< col (length line)) (aref line col) ?\s)
                             for face = (and (< col (length line)) (get-text-property (+ (aref starts row) col) 'face text))
                             unless (or (eq char ?█) (and (consp face) (plist-get face :background)))
                             do (push (cons col row) out)))))
    out))

(ert-deftest eas-visual-stacked-areas-tile-without-holes ()
  (let ((scene (eas-visual-test--gallery-scene "area-circular" "stacked_area_normalize" '(:cols 94 :rows 28))))
    ;; Slice by slice, eighth blocks leave holes.
    (cl-letf (((symbol-function 'eas-text-band-resolve) #'ignore))
      (should (eas-visual-test--stack-holes scene)))
    (should-not (eas-visual-test--stack-holes scene)))
  (dolist (size '((:cols 189 :rows 56) (:cols 60 :rows 16) (:cols 100 :rows 30)))
    (should (equal (list size (eas-visual-test--stack-holes
                               (eas-visual-test--gallery-scene "area-circular" "stacked_area_normalize" size)))
                   (list size nil))))
  (dolist (name '("stacked_area" "stacked_area_stream"))
    (should (equal (list name (eas-visual-test--stack-holes
                               (eas-visual-test--gallery-scene "area-circular" name '(:cols 94 :rows 28))))
                   (list name nil)))))

(ert-deftest eas-visual-band-resolve-backs-the-lower-slice ()
  (let ((a '(face (:foreground "#111111"))) (b '(face (:foreground "#222222"))))
    ;; B fills the cell's lower 3/8, A the rest: B's block on A.
    (should (equal (eas-text-band-resolve (list (list 0 8.75 a) (list 8.75 30 b)) 0 14) (list ?▃ b a)))
    ;; A gap of a quarter cell or more is no tiling.
    (should-not (eas-text-band-resolve (list (list 0 5 a) (list 9 30 b)) 0 14))
    ;; Slivers round to the whole cell.
    (should (equal (car (eas-text-band-resolve (list (list -5 0.5 a) (list 0.5 30 b)) 0 14)) ?█))))

;;; 4. Continuous legends show their ramp

(ert-deftest eas-visual-gradient-legend-draws-its-ramp ()
  (let* ((scene (eas-visual-test--gallery-scene "scatter-table" "rect_heatmap" '(:cols 94 :rows 28)))
         (legend (aref (plist-get (aref (plist-get scene :views) 0) :legends) 0))
         (bar (plist-get legend :bar))
         (ch (aref (plist-get (plist-get scene :size) :cell) 1))
         (text (let ((eas-text-background-mode 'light)) (eas-text-render scene)))
         (rows nil))
    (should (equal (plist-get legend :type) "gradient"))
    (seq-do-indexed
     (lambda (line row)
       (when-let* ((col (if (get-text-property 0 'eas-legend-ramp line) 0
                          (next-single-property-change 0 'eas-legend-ramp line))))
         (push (cons row (get-text-property col 'face line)) rows)))
     (split-string text "\n"))
    (setq rows (nreverse rows))
    ;; One cell row per row of the bar, each two samples of the scheme.
    (should (= (length rows) (- (ceiling (+ (aref bar 1) (aref bar 3)) ch) (floor (aref bar 1) ch))))
    (let ((lums (seq-mapcat (lambda (r) (list (eas-text-ink-luminance (plist-get (cdr r) :background))
                                              (eas-text-ink-luminance (plist-get (cdr r) :foreground))))
                            rows)))
      ;; High values (dark blues) on top, light at the bottom.
      (should (equal lums (sort (copy-sequence lums) #'<)))
      (should (< (car lums) (car (last lums)))))))

;;; 5. Angled labels sit under the axis

(ert-deftest eas-visual-angled-labels-sit-below-the-axis ()
  (dolist (size '((:cols 189 :rows 56) (:cols 94 :rows 28) (:cols 60 :rows 16)))
    (let* ((scene (eas-visual-test--gallery-scene "layered" "layer_candlestick" size))
           (ch (aref (plist-get (plist-get scene :size) :cell) 1))
           (axis (seq-find (lambda (a) (equal (plist-get a :orient) "bottom"))
                           (plist-get (aref (plist-get scene :views) 0) :axes)))
           (line-row (floor (aref (plist-get axis :domain-line) 1) ch))
           (label-rows (delete-dups (mapcar (lambda (tk) (floor (plist-get tk :ly) ch)) (plist-get axis :ticks)))))
      (should (equal (plist-get axis :labelAngle) 0))
      (should-not (seq-filter (lambda (p) (string-prefix-p "side:" p)) (eas-text-check scene)))
      ;; One row of labels right under the line, the title right under them.
      (should (equal label-rows (list (1+ line-row))))
      (should (= (floor (plist-get (plist-get axis :title-mark) :y) ch) (+ 2 line-row)))))
  ;; Stacked panes and facet rows end on a cell edge, so their labels too
  ;; sit under their axis lines.
  (pcase-dolist (`(,group ,name) '(("multiview" "vconcat_weather") ("layered" "facet_bullet")))
    (let* ((scene (eas-visual-test--gallery-scene group name '(:cols 100 :rows 30)))
           (ch (aref (plist-get (plist-get scene :size) :cell) 1)))
      (seq-doseq (v (plist-get scene :views))
        (let ((b (plist-get v :bounds)))
          (should (zerop (mod (+ (aref b 1) (aref b 3)) ch)))))
      (should-not (seq-filter (lambda (p) (string-prefix-p "side:" p)) (eas-text-check scene))))))

;;; 6. A brush is one solid region, an empty one nothing

(defun eas-visual-test--brush-cells (text)
  "(COL . ROW) of every cell of TEXT under a brush, with its face."
  (let (out)
    (seq-do-indexed
     (lambda (line row)
       (dotimes (col (length line))
         (when (get-text-property col 'eas-brush line)
           (push (list col row (get-text-property col 'face line)) out))))
     (split-string text "\n"))
    (nreverse out)))

(ert-deftest eas-visual-brush-is-one-solid-region ()
  (eas-visual-test--with ((view (eas-view-open (eas-text-gallery-spec "interactive" "interactive_brush")
                                               :target 'text :size '(:cols 94 :rows 28))))
    ;; The example starts with a brush (the param's value): a full rectangle
    ;; of shaded cells, braille and trailing blanks included.
    (let* ((cells (eas-visual-test--brush-cells (eas-text-render (eas-view-scene view))))
           (cols (mapcar #'car cells)) (rows (mapcar #'cadr cells)))
      (should cells)
      (should (= (length cells) (* (1+ (- (apply #'max cols) (apply #'min cols)))
                                   (1+ (- (apply #'max rows) (apply #'min rows))))))
      (dolist (c cells)
        (let ((face (nth 2 c)))
          (should (equal (if (keywordp (car-safe face)) (plist-get face :background)
                           (plist-get (car face) :background))
                         (eas-text-ink-shade))))))
    ;; Cleared, and after a click (an empty interval): nothing.
    (eas-dispatch view '(:type "key" :key "escape"))
    (should-not (eas-visual-test--brush-cells (eas-text-render (eas-view-scene view))))
    (let ((px [200 150]))
      (eas-dispatch view (list :type "drag" :from px :to px)))
    (should-not (eas-visual-test--brush-cells (eas-text-render (eas-view-scene view))))))

;;; 7. Log axes label what the reference labels

(defconst eas-visual-test--log-ref-labels
  '("0.1" "0.2" "0.3" "0.5" "1" "2" "3" "5" "10" "20" "30" "50" "100")
  "The y labels of calculations/ref/layer_line_window.png (bin/chart).")

(defun eas-visual-test--y-labels (scene)
  "SCENE's first view's y tick labels shown in its text rendering, low to high."
  (let* ((lines (vconcat (split-string (eas-text-render scene) "\n")))
         (axis (seq-find (lambda (a) (equal (plist-get a :channel) "y")) (plist-get (aref (plist-get scene :views) 0) :axes))))
    (mapcar (lambda (tk) (plist-get tk :label))
            (seq-filter (lambda (tk) (eas-text-check--shown-p scene lines (plist-get tk :lx) (plist-get tk :ly)
                                                              (plist-get tk :label) (plist-get tk :align)))
                        (reverse (append (plist-get axis :ticks) nil))))))

(ert-deftest eas-visual-log-axis-labels-match-the-reference ()
  (should (equal (eas-visual-test--y-labels (eas-visual-test--gallery-scene "calculations" "layer_line_window" '(:cols 94 :rows 28)))
                 eas-visual-test--log-ref-labels))
  (dolist (size '((:cols 60 :rows 16) (:cols 189 :rows 56)))
    (let* ((scene (eas-visual-test--gallery-scene "calculations" "layer_line_window" size))
           (labels (eas-visual-test--y-labels scene)))
      ;; Fewer rows keep a subset of the reference's labels, every power of ten.
      (should-not (cl-set-difference labels eas-visual-test--log-ref-labels :test #'equal))
      (should-not (cl-set-difference '("0.1" "1" "10" "100") labels :test #'equal))
      (should-not (eas-text-check scene)))))

;;; 8. Arc wedges meet without seams

(defun eas-visual-test--arc-seams (scene)
  "Cells of SCENE whose eight braille dot centres all lie inside its arcs
but that do not show a full block: seams, as (COL . ROW)."
  (let* ((lines (vconcat (split-string (eas-text-render scene) "\n")))
         (cell (plist-get (plist-get scene :size) :cell)) (cw (aref cell 0)) (ch (aref cell 1))
         (items (seq-mapcat (lambda (v) (seq-mapcat (lambda (m) (and (equal (plist-get m :mark) "arc")
                                                                   (append (plist-get m :items) nil)))
                                                 (plist-get v :marks)))
                            (plist-get scene :views)))
         (inside (lambda (x y) (seq-some (lambda (it) (eas-arc-contains-p it x y)) items)))
         out)
    (dotimes (row (length lines))
      (dotimes (col (round (/ (float (plist-get (plist-get scene :size) :w)) cw)))
        (when (cl-loop for j below 8
                       always (funcall inside (* (+ (* 2 col) (% j 2) 0.5) (/ cw 2.0))
                                       (* (+ (* 4 row) (/ j 2) 0.5) (/ ch 4.0))))
          (let ((line (aref lines row)))
            (unless (and (< col (length line)) (eq (aref line col) ?█))
              (push (cons col row) out))))))
    out))

(ert-deftest eas-visual-arc-wedges-meet-without-seams ()
  (pcase-dolist (`(,name ,size) '(("arc_pie" (:cols 94 :rows 28)) ("arc_pie" (:cols 60 :rows 16))
                                  ("arc_donut" (:cols 60 :rows 16))))
    (let ((scene (eas-visual-test--gallery-scene "area-circular" name size)))
      (should (equal (list name size (eas-visual-test--arc-seams scene)) (list name size nil))))))

(provide 'eas-visual-test)
;;; eas-visual-test.el ends here

;;; eas-text-parity-test.el --- text renderer parity with the SVG gallery -*- lexical-binding: t; -*-

;;; Commentary:

;; fc-qx1.49: what a terminal user saw go wrong (candles without bodies,
;; grouped bars, an empty donut, a boxplot that would not open), the
;; glyph strategies that fixed it, `eas-text-check's invariants, and the
;; text gallery: every non-map example and every template opened with
;; `eas-view-open' and shown with `eas-show' in a (fake) text window at
;; three sizes, held to test/vl-examples/text-status.json (:gallery).

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-text-gallery)

(defun eas-text-parity-test--open (source size &optional bindings)
  "A text view of SOURCE at SIZE (:cols :rows), closed by the caller."
  (eas-vl-gallery--native
   (eas-view-open source :bindings bindings :id "text-parity" :target 'text :size size)))

(defmacro eas-text-parity-test--with-view (spec &rest body)
  "Run BODY with VIEW, SCENE and TEXT bound for SPEC = (SOURCE SIZE [BINDINGS])."
  (declare (indent 1))
  `(let* ((view (apply #'eas-text-parity-test--open (list ,@spec)))
          (scene (eas-view-scene view))
          (text (eas-text-render scene)))
     (ignore text)
     (unwind-protect (progn ,@body)
       (eas-view-close view))))

(defun eas-text-parity-test--chars (text prop value)
  "Characters of TEXT whose PROP is VALUE."
  (cl-loop for i below (length text)
           when (equal (get-text-property i prop text) value) collect (aref text i)))

(defun eas-text-parity-test--columns (text mark datum)
  "Columns of TEXT's cells drawn for DATUM of MARK."
  (let ((col 0) out)
    (dotimes (i (length text))
      (if (eq (aref text i) ?\n) (setq col 0)
        (when (and (equal (get-text-property i 'eas-mark text) mark) (equal (get-text-property i 'eas-datum text) datum))
          (push col out))
        (cl-incf col)))
    (delete-dups out)))

;;; The defects seen live (live session, 515bfd3)

(ert-deftest eas-text-parity-ohlc-draws-candle-bodies ()
  "Bodies win over wicks drawn before them; rising solid, falling shaded."
  (eas-text-parity-test--with-view ("ohlc" '(:cols 80 :rows 22) (eas-template-example "ohlc"))
    (let ((items (plist-get (eas-scene-mark scene "candles") :items))
          (chars (eas-text-parity-test--chars text 'eas-mark "candles")))
      (should (seq-some (lambda (i) (eq (plist-get i :rise) t)) items))
      (should (seq-some (lambda (i) (eq (plist-get i :rise) :false)) items))
      (should (memq ?█ chars))
      (should (memq ?▒ chars))
      (should-not (eas-text-check scene)))))

(ert-deftest eas-text-parity-grouped-bars-are-zero-based-and-side-by-side ()
  (eas-text-parity-test--with-view ((eas-vl-gallery-spec "bar" "bar_grouped") '(:cols 100 :rows 30))
    (let* ((view (aref (plist-get scene :views) 0))
           (mark (aref (plist-get view :marks) 0)))
      (should (= (aref (plist-get (plist-get (plist-get view :scales) :y) :domain) 0) 0))
      (should-not (eas-text-check scene))
      (should (string-match-p "A +B +C" (substring-no-properties text)))
      ;; Category A's three bars (x, y, z) take columns of their own.
      (let ((cols (mapcar (lambda (d) (eas-text-parity-test--columns text (plist-get mark :id) d)) '(0 1 2))))
        (should (seq-every-p #'identity cols))
        (should-not (seq-intersection (nth 0 cols) (nth 1 cols)))
        (should-not (seq-intersection (nth 1 cols) (nth 2 cols)))))))

(ert-deftest eas-text-parity-donut-is-a-braille-ring ()
  (eas-text-parity-test--with-view ((eas-vl-gallery-spec "area-circular" "arc_donut") '(:cols 60 :rows 16))
    (let* ((lines (split-string (substring-no-properties text) "\n"))
           (item (aref (plist-get (eas-scene-mark scene (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :id))
                                  :items)
                       0))
           (col (floor (plist-get item :cx) 7)) (row (floor (plist-get item :cy) 14)))
      (should (string-match-p "[⠁-⣿]" (string-join lines)))
      ;; The hole stays empty.
      (should (memq (aref (concat (nth row lines) (make-string 80 ?\s)) col) '(?\s)))
      (should-not (eas-text-check scene)))))

(defconst eas-text-parity-test--boxplot
  '(:data (:url "../data/population.json")
    :mark (:type "boxplot" :extent "min-max")
    :encoding (:x (:field "age" :type "ordinal")
               :y (:field "people" :type "quantitative" :title "population")))
  "Vega-Lite's boxplot_minmax_2D_vertical example.")

(ert-deftest eas-text-parity-boxplot-opens-and-shows-in-text ()
  (let* ((spec (eas-vl-gallery-inline eas-text-parity-test--boxplot (eas-vl-gallery-group-directory "distributions")))
         (entry (list :group "distributions" :name "boxplot_minmax_2D_vertical"
                      :open (lambda (size) (eas-text-parity-test--open spec size)))))
    (should-not (eas-text-gallery-run entry '((:cols 60 :rows 16) (:cols 100 :rows 30))))
    ;; As a user opens it: supported.json in force, not the gallery's unrestricted native mode.
    (let ((view (eas-view-open spec :id "text-parity-user" :target 'text :size '(:cols 60 :rows 16))))
      (unwind-protect (should (eas-view-interactive view))
        (eas-view-close view)))
    (eas-text-parity-test--with-view (spec '(:cols 100 :rows 30))
      (dolist (type '("bar" "rule" "tick"))
        (should (seq-some (lambda (m) (and (equal (plist-get m :mark) type)
                                           (eas-text-parity-test--chars text 'eas-mark (plist-get m :id))))
                          (plist-get (aref (plist-get scene :views) 0) :marks)))))))

(ert-deftest eas-text-parity-show-in-a-fake-text-window ()
  "eas-show draws templates in a text window, in batch."
  (dolist (name '("ohlc" "bars" "panes"))
    (let ((entry (seq-find (lambda (e) (equal (plist-get e :name) name))
                           (eas-text-gallery-entries (list eas-text-gallery-group-templates)))))
      (should (equal (cons name (eas-text-gallery-run entry '((:cols 60 :rows 16)))) (list name))))))

;;; Glyph strategies

(ert-deftest eas-text-parity-painter-order-within-tiers ()
  (let ((view (list :marks (vector (list :mark "rule" :items [(:opacity 1)]) (list :mark "bar" :items [(:opacity 1)])
                                   (list :mark "line" :items [(:opacity 1)]) (list :mark "point" :items [(:opacity 0.3)])
                                   (list :mark "text" :items [(:opacity 1)])))))
    ;; The wick sits under the body; faint points under the line.
    (should (equal (eas-text--mark-prios view) '(100.0 101.0 202.0 153.0 304.0)))))

(ert-deftest eas-text-parity-bar-ends ()
  (should (eq (eas-text--vglyph 0.0 1.0) ?█))
  (should (eq (eas-text--vglyph 0.5 1.0) ?▄))
  (should (eq (eas-text--vglyph 0.97 1.0) ?▁))
  (should (eq (eas-text--vglyph 0.0 0.5) ?▀))
  (should (eq (eas-text--vglyph 0.0 0.1) ?▔))
  (should (eq (eas-text--vglyph 0.4 0.6) ?━))
  (should (eq (eas-text--hglyph 0.0 0.5) ?▌))
  (should (eq (eas-text--hglyph 0.5 1.0) ?▐))
  (should (eq (eas-text--falling ?█) ?▒))
  (should (eq (eas-text--falling ?▁) ?▁)))

(ert-deftest eas-text-parity-tiny-bars-still-show ()
  (eas-text-parity-test--with-view ('(:data (:values [(:k "a" :v 1000) (:k "b" :v 1)]) :mark "bar"
                                      :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative")))
                                    '(:cols 40 :rows 12))
    (let ((mark (plist-get (aref (plist-get (aref (plist-get scene :views) 0) :marks) 0) :id)))
      (should (eas-text-parity-test--columns text mark 1))
      (should-not (eas-text-check scene)))))

(ert-deftest eas-text-parity-bands-fill-either-way-round ()
  "An area whose base lies above its points (an errorband) still fills."
  (eas-text-parity-test--with-view ('(:data (:values [(:x 1 :lo 2 :hi 8) (:x 2 :lo 3 :hi 9) (:x 3 :lo 1 :hi 7)])
                                      :mark "area"
                                      :encoding (:x (:field "x" :type "quantitative")
                                                 :y (:field "hi" :type "quantitative") :y2 (:field "lo")))
                                    '(:cols 40 :rows 12))
    (should (memq ?█ (append (substring-no-properties text) nil)))
    (should-not (eas-text-check scene))))

(ert-deftest eas-text-parity-wide-labels-keep-columns ()
  "Double-width labels (emoji) take two cells, so later columns stay put."
  (eas-text-parity-test--with-view ((eas-vl-gallery-spec "layered" "layer_bar_fruit") '(:cols 60 :rows 16))
    (should-not (eas-text-check scene))
    (should (<= (apply #'max (mapcar #'string-width (split-string text "\n"))) 60))))

(ert-deftest eas-text-parity-labels-stay-on-the-canvas ()
  (should (equal (eas-text-string-span 10 7 14 70 0 "2010" "center") '(0 6 . 10)))
  (should (equal (eas-text-string-span 10 7 14 0 0 "−100%" "right") '(0 0 . 5)))
  (should (equal (eas-text-string-span 10 7 14 0 224 "x" "left" 16) '(15 0 . 1)))
  (should (equal (eas-text-string-span 10 7 14 0 224 "x" "left") '(16 0 . 1))))

(ert-deftest eas-text-parity-overlapping-labels-are-dropped-not-garbled ()
  (let* ((ticks (vconcat (mapcar (lambda (i) (list :label (format "label-%d" i) :lx (+ 70 (* 21 i)) :ly 150 :align "center"))
                                 (number-sequence 0 4))))
         (scene (list :size '(:w 280 :h 168 :cell [7 14])
                      :views (vector (list :id "v" :bounds [0 0 280 140] :marks []
                                           :axes (vector (list :channel "x" :orient "bottom" :ticks ticks))))))
         (text (substring-no-properties (eas-text-render scene))))
    (should (string-match-p "label-0" text))
    (should-not (string-match-p "label-1" text))
    (should-not (seq-filter (lambda (p) (string-prefix-p "collision" p)) (eas-text-check scene)))))

;;; The invariants

(ert-deftest eas-text-parity-check-finds-problems ()
  (let ((scene '(:size (:w 70 :h 42 :cell [7 14]) :views [])))
    (should (equal (eas-text-check scene) '("empty: the rendering is blank"))))
  ;; A label nothing hides yet missing (here: off the canvas to the left, too wide to move in).
  (let ((scene (list :size '(:w 70 :h 42 :cell [7 14])
                     :views (vector (list :id "v" :bounds [0 0 70 28] :marks []
                                          :axes (vector (list :channel "y" :orient "left"
                                                              :ticks [(:label "a very long label" :lx 0 :ly 14 :align "right")])))))))
    (should (member "label: y axis label \"a very long label\" of view v is not shown" (eas-text-check scene))))
  ;; Bars standing on a floor that is not zero.
  (let* ((bar (lambda (x) (list :x x :y 0 :w 7 :h 28 :orient "vertical" :fill "#000")))
         (scene (list :size '(:w 70 :h 42 :cell [7 14])
                      :views (vector (list :id "v" :bounds [0 0 70 28]
                                           :scales '(:y (:type "linear" :domain [5 10] :range [28 0]))
                                           :marks (vector (list :id "m" :mark "bar" :items (vector (funcall bar 7) (funcall bar 21))))
                                           :axes [])))))
    (should (seq-find (lambda (p) (string-match-p "^baseline: bars of m in view v stand on 5" p)) (eas-text-check scene)))))

;;; The text gallery (minutes): held to text-status.json

(defun eas-text-parity-test--hold (group)
  "Failure strings of GROUP's entries against text-status.json."
  (let ((status (plist-get (eas-text-gallery-status) (eas-key group))) out)
    (dolist (entry (eas-text-gallery-entries (list group)))
      (let* ((name (plist-get entry :name))
             (want (plist-get (plist-get status (eas-key name)) :status))
             (problems (eas-text-gallery-run entry)))
        (cond ((null want) (push (format "%s/%s has no text-status.json entry" group name) out))
              ((and (equal want "pass") problems)
               (push (format "%s/%s: %s" group name (string-join problems "; ")) out))
              ((and (equal want "partial") (null problems))
               (push (format "%s/%s now passes: promote it in text-status.json" group name) out)))))
    (nreverse out)))

(ert-deftest eas-vl-gallery-groups-hold-their-text-status ()
  "Every non-map example opens and shows as text at three sizes and holds
its text-status.json verdict (EAS_GALLERY_GROUPS picks the groups)."
  :tags '(:gallery)
  (let ((only (split-string (or (getenv "EAS_GALLERY_GROUPS") ""))))
    (dolist (group (eas-vl-gallery-groups))
      (when (or (null only) (member group only))
        (should (equal (cons group (eas-text-parity-test--hold group)) (list group)))))))

(ert-deftest eas-text-gallery-templates-hold-their-text-status ()
  "Every template, with its example bindings, holds its text verdict."
  :tags '(:gallery)
  (should (equal (eas-text-parity-test--hold eas-text-gallery-group-templates) nil)))

(provide 'eas-text-parity-test)
;;; eas-text-parity-test.el ends here

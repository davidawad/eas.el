;;; eas-vega-wordcloud-test.el --- word cloud: transforms and template -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The Vega gallery's word-cloud example (eas-7r1.8): the countpattern
;; and wordcloud transforms, the collision geometry they rest on, and
;; the templates/vega/word-cloud.json template against the vendored
;; spec and reference in test/vega-examples/.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-png)
(require 'eas-chart)
(require 'eas-text-check)
(require 'eas-test-support)

(defun eas-vega-wordcloud-gallery-spec ()
  "The vendored Vega word-cloud spec, parsed."
  (eas-json-read-file (eas-test-file "test/vega-examples/specs/word-cloud.vg.json")))

(defun eas-vega-wordcloud-gallery-data ()
  "The Vega spec's table data object: its values and transforms."
  (aref (plist-get (eas-vega-wordcloud-gallery-spec) :data) 0))

(defun eas-vega-wordcloud-counted ()
  "The gallery abstracts counted as the Vega spec counts them."
  (let* ((data (eas-vega-wordcloud-gallery-data))
         (tr (aref (plist-get data :transform) 0)))
    (eas-transform-apply-domain
     (list :x-eas:transform "countpattern" :field "abstract" :case (plist-get tr :case)
           :pattern (plist-get tr :pattern) :stopwords (plist-get tr :stopwords))
     (vconcat (mapcar (lambda (s) (list :abstract s)) (plist-get data :values))))))

(defun eas-vega-wordcloud-run (rows &rest params)
  "ROWS through the wordcloud transform with PARAMS (a plist)."
  (eas-transform-apply-domain (append (list :x-eas:transform "wordcloud") params) rows))

(defun eas-vega-wordcloud-placed (rows)
  "The rows of ROWS the wordcloud transform found room for."
  (seq-filter (lambda (r) (numberp (plist-get r :x))) rows))

(defun eas-vega-wordcloud-row-box (row &optional pad)
  "The turned box of placed ROW, grown by PAD."
  (eas-wordcloud-box (plist-get row :x) (plist-get row :y)
                     (eas-wordcloud-text-box (plist-get row :text) (plist-get row :fontSize)
                                             (plist-get row :fontWeight)
                                             (car (split-string (plist-get row :font) ",")))
                     (plist-get row :angle) pad))

(defun eas-vega-wordcloud-scene ()
  "The template's example compiled for svg."
  (eas-compile (eas-resolve "word-cloud" (eas-template-example "word-cloud"))))

;;; countpattern

(ert-deftest eas-vega-wordcloud-countpattern-counts-words ()
  (let ((out (eas-transform-apply-domain
              '(:x-eas:transform "countpattern" :field "t" :case "upper" :pattern "[\\w']{3,}"
                                 :stopwords "(the|and|it's)")
              [(:t "The cat and the hat; it's Vega’s cat") (:t 7) (:t "vega CAT")])))
    ;; Order of first appearance, folded to upper case; ’ is no word
    ;; character, so VEGA’S is VEGA; stopwords match in any case.
    (should (equal out [(:text "CAT" :count 3) (:text "HAT" :count 1) (:text "VEGA" :count 2)])))
  (should (equal (eas-transform-apply-domain
                  '(:x-eas:transform "countpattern" :field "t" :as ["w" "n"]) [(:t "a b a")])
                 [(:w "a" :n 2) (:w "b" :n 1)])))

(ert-deftest eas-vega-wordcloud-countpattern-regexp-classes ()
  (let ((case-fold-search nil))
    (should (equal (eas-countpattern-regexp "[\\w']{3,}") "[A-Za-z0-9_']\\{3,\\}"))
    (should (string-match-p (eas-countpattern-regexp "\\d+") "x42"))
    (should-not (string-match-p (concat "\\`" (eas-countpattern-regexp "[\\w]+") "\\'") "é")))
  (should (= (length (eas-vega-wordcloud-counted)) 194)))

;;; Geometry

(ert-deftest eas-vega-wordcloud-random-is-seeded ()
  (let ((a (eas-wordcloud-random 7)) (b (eas-wordcloud-random 7)) (c (eas-wordcloud-random 8)))
    (let ((xs (cl-loop repeat 50 collect (funcall a))))
      (should (equal xs (cl-loop repeat 50 collect (funcall b))))
      (should-not (equal xs (cl-loop repeat 50 collect (funcall c))))
      (should (seq-every-p (lambda (x) (and (floatp x) (<= 0 x) (< x 1))) xs)))))

(ert-deftest eas-vega-wordcloud-text-box-measures-glyphs ()
  (pcase-let ((`(,x0 ,y0 ,x1 ,y1) (eas-wordcloud-text-box "VEGA" 20)))
    (should (= (- x1 x0) (eas-font-text-width "VEGA" 20)))
    (should (= x0 (- x1)))
    (should (< -15 y0 -14))                  ; cap height
    (should (= y1 0)))                       ; no descender
  (should (> (nth 3 (eas-wordcloud-text-box "jog" 20)) 4))
  (should (> (nth 1 (eas-wordcloud-text-box "ace" 20)) (nth 1 (eas-wordcloud-text-box "ACE" 20))))
  (should (> (- (nth 2 (eas-wordcloud-text-box "VEGA" 20 700)) (nth 0 (eas-wordcloud-text-box "VEGA" 20 700)))
             (- (nth 2 (eas-wordcloud-text-box "VEGA" 20)) (nth 0 (eas-wordcloud-text-box "VEGA" 20))))))

(ert-deftest eas-vega-wordcloud-boxes-overlap-exactly ()
  (let* ((bar '(-50 -5 50 5))
         (a (eas-wordcloud-box 0 0 bar 45))
         (b (eas-wordcloud-box 40 0 bar 45))
         (c (eas-wordcloud-box 0 0 bar -45)))
    ;; Parallel diagonals whose bounding boxes overlap but whose boxes do not.
    (should (< (aref b 0) (aref a 2)))
    (should-not (eas-wordcloud-overlap-p a b))
    (should (eas-wordcloud-overlap-p a c))
    (should (eas-wordcloud-overlap-p a (eas-wordcloud-box 5 5 bar 45)))
    ;; Padding closes the gap; an offset moves the first box.
    (should (eas-wordcloud-overlap-p (eas-wordcloud-box 0 0 bar 45 25) b))
    (should (eas-wordcloud-overlap-p a b 35 0))
    ;; Touching edges do not overlap.
    (should-not (eas-wordcloud-overlap-p (eas-wordcloud-box 0 0 bar 0) (eas-wordcloud-box 100 0 bar 0)))))

(ert-deftest eas-vega-wordcloud-sprite-covers-its-box ()
  (dolist (angle '(0 30 -45 90))
    (let* ((box (eas-wordcloud-box 0 0 '(-20 -9 20 3) angle 2))
           (sprite (eas-wordcloud-sprite box))
           (row0 (aref sprite 0)))
      ;; Every pixel whose centre lies inside the box is in its row's span.
      (cl-loop for y from (floor (aref box 1)) below (ceiling (aref box 3))
               do (cl-loop for x from (floor (aref box 0)) below (ceiling (aref box 2))
                           when (eas-wordcloud-overlap-p (eas-wordcloud-box (+ x 0.5) (+ y 0.5) '(-0.01 -0.01 0.01 0.01) 0) box)
                           do (let ((i (+ 1 (* 2 (- y row0)))))
                                (should (<= (aref sprite i) x (aref sprite (1+ i))))))))))

(ert-deftest eas-vega-wordcloud-board-detects-collisions ()
  (let ((board (eas-wordcloud-board 200 100))
        (sprite (eas-wordcloud-sprite (eas-wordcloud-box 0 0 '(-30 -10 30 2) 0))))
    (should-not (eas-wordcloud-board-hits-p board sprite 100 50))
    (eas-wordcloud-board-add board sprite 100 50)
    (should (eas-wordcloud-board-hits-p board sprite 100 50))
    (should (eas-wordcloud-board-hits-p board sprite 159 50))
    (should-not (eas-wordcloud-board-hits-p board sprite 161 50))
    (should-not (eas-wordcloud-board-hits-p board sprite 100 63))
    ;; Spans crossing a 60-pixel chunk boundary.
    (should (eas-wordcloud-board-hits-p board (eas-wordcloud-sprite (eas-wordcloud-box 0 0 '(0 0 1 1) 0)) 129 45))))

;;; The wordcloud transform

(ert-deftest eas-vega-wordcloud-places-without-overlap ()
  (let* ((rows (seq-take (eas-vega-wordcloud-counted) 60))
         (out (eas-vega-wordcloud-run rows :size [500 260] :text "text" :fontSize '(:field "count")
                                      :fontSizeRange [10 40] :rotate [-45 0 45] :padding 2 :seed 3))
         (placed (eas-vega-wordcloud-placed out))
         (boxes (mapcar (lambda (r) (eas-vega-wordcloud-row-box r 2)) placed)))
    (should (> (length placed) 40))
    (dolist (b boxes)
      (should (and (>= (aref b 0) -1e-9) (>= (aref b 1) -1e-9) (<= (aref b 2) 500) (<= (aref b 3) 260))))
    ;; The padded boxes of any two words are apart.
    (cl-loop for (a . rest) on boxes
             do (dolist (b rest) (should-not (eas-wordcloud-overlap-p a b))))
    ;; Every input row comes back, with the output fields.
    (should (= (length out) 60))
    (should (equal (eas-plist-keys (aref out 0))
                   '(:text :count :x :y :font :fontSize :fontStyle :fontWeight :angle)))))

(ert-deftest eas-vega-wordcloud-is-deterministic ()
  (let ((rows (seq-take (eas-vega-wordcloud-counted) 25))
        (params '(:size [300 200] :fontSize (:field "count") :rotate [0 90])))
    (eas-memo-clear)
    (let ((a (apply #'eas-vega-wordcloud-run rows :seed 1 params)))
      (eas-memo-clear)
      (should (equal a (apply #'eas-vega-wordcloud-run rows :seed 1 params)))
      (should-not (equal a (apply #'eas-vega-wordcloud-run rows :seed 2 params)))
      ;; A cached cloud hands out fresh rows.
      (should-not (eq (aref a 0) (aref (apply #'eas-vega-wordcloud-run rows :seed 1 params) 0))))))

(ert-deftest eas-vega-wordcloud-scales-font-sizes ()
  (let ((out (eas-vega-wordcloud-run [(:text "A" :n 1) (:text "B" :n 4) (:text "C" :n 9)]
                                     :size [400 400] :fontSize '(:field "datum.n") :fontSizeRange [10 50])))
    ;; Square root of the count onto the range, truncated, as d3-cloud does.
    (should (equal (mapcar (lambda (r) (plist-get r :fontSize)) out) '(10 30 50))))
  (let ((out (eas-vega-wordcloud-run [(:text "A") (:text "B")] :size [400 400] :fontSize 21 :fontSizeRange :null)))
    (should (equal (mapcar (lambda (r) (plist-get r :fontSize)) out) '(21 21))))
  (let ((out (eas-vega-wordcloud-run [(:text "A" :n 3) (:text "B" :n 3)] :size [400 400] :fontSize '(:field "n"))))
    ;; One distinct value sits mid-range (default range [10 50]).
    (should (equal (mapcar (lambda (r) (plist-get r :fontSize)) out) '(30 30)))))

(ert-deftest eas-vega-wordcloud-rotation-sources ()
  (let ((rows (vconcat (cl-loop for i below 30 collect (list :text (format "W%d" i) :a (* 10 i))))))
    (should (equal (mapcar (lambda (r) (plist-get r :angle))
                           (eas-vega-wordcloud-run rows :size [600 600] :rotate '(:field "a")))
                   (cl-loop for i below 30 collect (* 10 i))))
    (let ((picked (mapcar (lambda (r) (plist-get r :angle))
                          (eas-vega-wordcloud-run rows :size [600 600] :rotate [-45 0 45]))))
      (should (seq-every-p (lambda (a) (memq a '(-45 0 45))) picked))
      (should (= (length (seq-uniq picked)) 3)))
    ;; random() in an expression differs from row to row.
    (let ((angles (mapcar (lambda (r) (plist-get r :angle))
                          (eas-vega-wordcloud-run rows :size [600 600] :rotate '(:expr "[-45, 0, 45][floor(random() * 3)]")))))
      (should (= (length (seq-uniq angles)) 3)))))

(ert-deftest eas-vega-wordcloud-unplaced-words-are-null ()
  (let ((out (eas-vega-wordcloud-run [(:text "ENORMOUS" :n 1) (:text "OK" :n 1)]
                                     :size [120 50] :fontSize '(:field "n") :fontSizeRange [40 40])))
    (should (eq (plist-get (aref out 0) :x) :null))
    (should (eq (plist-get (aref out 0) :y) :null))
    (should (numberp (plist-get (aref out 1) :x)))))

(ert-deftest eas-vega-wordcloud-transforms-are-described ()
  (let ((names (mapcar (lambda (tr) (plist-get tr :name)) (plist-get (eas-describe "transforms") :transforms))))
    (should (member "wordcloud" names))
    (should (member "countpattern" names)))
  (eas-test-should-code "INVALID_INPUT"
    (eas-transform-apply-domain '(:x-eas:transform "countpattern") [(:t "a")]))
  (eas-test-should-code "INVALID_INPUT"
    (eas-vega-wordcloud-run [(:text "a")] :seed 1.5)))

;;; The template

(ert-deftest eas-vega-wordcloud-template-reproduces-the-gallery ()
  (let* ((template (eas-template-get "word-cloud"))
         (meta (plist-get template :meta))
         (slots (plist-get meta :slots))
         (example (eas-template-example "word-cloud"))
         (vega (eas-vega-wordcloud-gallery-spec))
         (data (eas-vega-wordcloud-gallery-data))
         (count (aref (plist-get data :transform) 0))
         (cloud (aref (plist-get (aref (plist-get vega :marks) 0) :transform) 0)))
    (should (string-suffix-p "templates/vega/word-cloud.json" (plist-get template :path)))
    (should (member (plist-get (plist-get meta :vega) :status) '("pass" "partial" "unsupported")))
    (should (stringp (plist-get (plist-get meta :vega) :note)))
    ;; The binding holds the gallery's three abstracts, and the slot
    ;; defaults are the Vega spec's own settings.
    (should (equal (mapcar (lambda (r) (plist-get r (eas-key (plist-get example :text)))) (plist-get example :data))
                   (append (plist-get data :values) nil)))
    (should (equal (plist-get example :emphasis) "VEGA"))
    (cl-loop for (slot value) on (list :pattern (plist-get count :pattern) :stopwords (plist-get count :stopwords)
                                       :case (plist-get count :case) :fontSizeRange (plist-get cloud :fontSizeRange)
                                       :padding (plist-get cloud :padding) :font (plist-get cloud :font)
                                       :width (plist-get vega :width) :height (plist-get vega :height)
                                       :colors (plist-get (aref (plist-get vega :scales) 0) :range))
             by #'cddr
             do (should (equal (plist-get (plist-get slots slot) :default) value)))))

(ert-deftest eas-vega-wordcloud-template-binds-own-data ()
  (let* ((resolved (eas-resolve "word-cloud"
                                '(:data [(:id 1 :body "Alpha beta beta; gamma gamma gamma.")
                                         (:id 2 :body "the gamma")]
                                  :text "body" :case "lower" :angles [0] :width 300 :height 150
                                  :colors ["#111111"] :seed 9)))
         (rows (append (plist-get (plist-get resolved :data) :values) nil)))
    (should (equal (mapcar (lambda (r) (list (plist-get r :text) (plist-get r :count))) rows)
                   '(("alpha" 1) ("beta" 2) ("gamma" 4))))
    (should (seq-every-p (lambda (r) (and (eql (plist-get r :angle) 0) (<= 0 (plist-get r :x) 300)
                                          (<= 0 (plist-get r :y) 150)))
                         rows))
    (should (= (plist-get resolved :width) 300))
    (let ((items (plist-get (eas-scene-mark (eas-compile resolved) "main/0") :items)))
      (should (= (length items) 3))
      (should (seq-every-p (lambda (it) (equal (plist-get it :fill) "#111111")) items))))
  (eas-test-should-code "FIELD_MISSING"
    (eas-resolve "word-cloud" '(:data [(:body "words here")] :text "abstract"))))

(ert-deftest eas-vega-wordcloud-template-resolves-to-vega-lite ()
  (let* ((resolved (eas-resolve "word-cloud" (eas-template-example "word-cloud")))
         (rows (plist-get (plist-get resolved :data) :values))
         (json (eas-json-encode resolved)))
    (should-not (string-search "x-eas" json))
    (should (= (length rows) 194))
    (should (equal (seq-filter (lambda (r) (equal (plist-get r :fontWeight) 600)) (append rows nil))
                   (list (seq-find (lambda (r) (equal (plist-get r :text) "VEGA")) rows))))
    (let ((vega-row (seq-find (lambda (r) (equal (plist-get r :text) "VEGA")) rows)))
      (should (= (plist-get vega-row :count) 12))
      (should (= (plist-get vega-row :fontSize) 56)))))

(ert-deftest eas-vega-wordcloud-template-renders-both-backends ()
  (let* ((scene (eas-vega-wordcloud-scene))
         (rows (plist-get (plist-get (eas-resolve "word-cloud" (eas-template-example "word-cloud")) :data) :values))
         (placed (length (eas-vega-wordcloud-placed (append rows nil))))
         (items (append (plist-get (eas-scene-mark scene "main/0") :items)
                        (plist-get (eas-scene-mark scene "main/2") :items) nil))
         (svg (eas-svg-render scene)))
    (should (> placed 120))
    (should (= (length items) placed))
    (should (equal (plist-get (aref (plist-get (eas-scene-mark scene "main/2") :items) 0) :text) "VEGA"))
    (should (string-match-p "font-weight=\"600\"[^>]*>VEGA<" svg))
    (should (string-match-p "rotate(-45 " svg))
    (should (string-match-p "rotate(45 " svg))
    ;; The canvas is the Vega spec's, within Vega's overhang padding.
    (let ((size (plist-get scene :size)))
      (should (<= 800 (plist-get size :w) 808))
      (should (<= 400 (plist-get size :h) 404)))
    (let ((text (eas-text-render (eas-compile (eas-resolve "word-cloud" (eas-template-example "word-cloud"))
                                              :target 'text :size '(:cols 100 :rows 30)))))
      (should (string-search "VEGA" text))
      (should (null (eas-text-check (eas-compile (eas-resolve "word-cloud" (eas-template-example "word-cloud"))
                                                 :target 'text :size '(:cols 100 :rows 30))))))))

;; Vega's ordinal color domain is every counted word in countpattern
;; order, the unplaced ones and VEGA (drawn in its own layer) included.
(ert-deftest eas-vega-wordcloud-colors-cycle-in-counted-order ()
  (let* ((scene (eas-vega-wordcloud-scene))
         (words (mapcar (lambda (r) (plist-get r :text)) (eas-vega-wordcloud-counted)))
         (colors ["#d5a928" "#652c90" "#939597"])
         (items (append (plist-get (eas-scene-mark scene "main/0") :items)
                        (plist-get (eas-scene-mark scene "main/2") :items) nil)))
    (should (> (length items) 120))
    (dolist (item items)
      (should (equal (list (plist-get item :text) (plist-get item :fill))
                     (list (plist-get item :text)
                           (aref colors (% (seq-position words (plist-get item :text)) 3))))))
    (should (equal (plist-get (aref (plist-get (eas-scene-mark scene "main/2") :items) 0) :fill) "#652c90"))))

(ert-deftest eas-vega-wordcloud-hover-fades-the-word ()
  ;; Vega fades a hovered word to fillOpacity 0.5; the template draws it
  ;; again in white at opacity 0.5, the same blend on a white canvas.
  (let ((v (eas-view-open "word-cloud" :bindings (eas-template-example "word-cloud") :id "eas-vega-wordcloud"))
        (overlays (lambda (v) (append (plist-get (eas-scene-mark (eas-view-scene v) "main/1") :items)
                                      (plist-get (eas-scene-mark (eas-view-scene v) "main/3") :items) nil))))
    (unwind-protect
        (let* ((vega (aref (plist-get (eas-scene-mark (eas-view-scene v) "main/2") :items) 0))
               (_ (should (null (funcall overlays v))))
               (inspect (eas-dispatch v (list :type "pointermove"
                                              :px (vector (plist-get vega :x) (- (plist-get vega :y) 5)))))
               (over (funcall overlays v)))
          (should (equal (plist-get (plist-get (plist-get inspect :hover) :row) :text) "VEGA"))
          (should (= (length over) 1))
          (dolist (key '(:text :x :y :fontSize :angle :fontWeight))
            (should (equal (plist-get (car over) key) (plist-get vega key))))
          (should (equal (plist-get (car over) :fill) "white"))
          (should (equal (plist-get (car over) :opacity) 0.5))
          (eas-dispatch v '(:type "pointerleave"))
          (should (null (funcall overlays v))))
      (eas-view-close v))))

;;; Against the reference

(defun eas-vega-wordcloud-ink (png)
  "Share of PNG's pixels that are visibly inked: opaque and not near white."
  (let ((rgba (plist-get png :rgba)) (n 0))
    (cl-loop for i from 0 below (length rgba) by 4
             when (and (> (aref rgba (+ i 3)) 127)
                       (< (min (aref rgba i) (aref rgba (+ i 1)) (aref rgba (+ i 2))) 200))
             do (cl-incf n))
    (/ (float n) (* (plist-get png :w) (plist-get png :h)))))

(ert-deftest eas-vega-wordcloud-reference-has-words ()
  ;; vg2png measures text with node-canvas, so Vega placed its words.
  (unless (and (fboundp 'zlib-available-p) (zlib-available-p))
    (eas-test-skip "this Emacs lacks zlib, needed to decode PNGs"))
  (let ((ref (eas-png-read (eas-test-file "test/vega-examples/ref/word-cloud.png"))))
    (should (equal (list (plist-get ref :w) (plist-get ref :h)) '(800 400)))
    (should (> (eas-vega-wordcloud-ink ref) 0.02))))

(ert-deftest eas-vega-wordcloud-png-against-the-reference ()
  (unless (executable-find eas-chart-rsvg-program)
    (eas-test-skip (format "%s not on PATH; it rasterizes the native SVG for the PNG comparison"
                           eas-chart-rsvg-program)))
  (let ((png (make-temp-file "eas-vega-wordcloud" nil ".png")))
    (unwind-protect
        (progn
          (eas-chart-rasterize (eas-svg-render (eas-vega-wordcloud-scene)) png)
          (let ((cmp (eas-png-compare (eas-png-read png)
                                      (eas-png-read (eas-test-file "test/vega-examples/ref/word-cloud.png")))))
            ;; Both place words at random along the spiral, so pixels
            ;; cannot match; the canvases and the amount of ink can.
            ;; The canvas is the reference's, within Vega's text overhang.
            (should (<= (abs (aref (plist-get cmp :size-delta) 0)) 8))
            (should (<= (abs (aref (plist-get cmp :size-delta) 1)) 4))
            (should (> (plist-get cmp :ratio) 0))
            (let ((native (eas-vega-wordcloud-ink (eas-png-read png)))
                  (ref (eas-vega-wordcloud-ink
                        (eas-png-read (eas-test-file "test/vega-examples/ref/word-cloud.png")))))
              (should (< 0.5 (/ native ref) 2.0)))))
      (delete-file png))))

(provide 'eas-vega-wordcloud-test)
;;; eas-vega-wordcloud-test.el ends here

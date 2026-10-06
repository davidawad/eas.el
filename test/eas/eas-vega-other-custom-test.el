;;; eas-vega-other-custom-test.el --- Vega gallery: other chart types and custom designs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; eas-7r1.3: thirteen examples of the Vega gallery (heatmap,
;; parallel-coordinates, calendar-view and the custom designs) as
;; templates in templates/vega/, each with an example binding in
;; examples/vega/ that reproduces the gallery chart from
;; test/vega-examples/data.  Every template resolves, compiles for both
;; backends, records its verdict in x-eas.vega, and, where a rasterizer
;; exists, scores against test/vega-examples/ref/NAME.png within the
;; bounds its verdict claims.

;;; Code:

(require 'eas-test-support)
(require 'eas)
(require 'eas-png)
(require 'eas-chart)

(defconst eas-vega-other-custom-names
  '("heatmap" "parallel-coordinates" "calendar-view" "budget-forecasts"
    "wheat-and-wages" "falkensee-population" "annual-temperature"
    "weekly-temperature" "flight-passengers" "timelines"
    "u-district-cuisine" "warming-stripes" "serpentine-timeline")
  "The Vega gallery examples of bead eas-7r1.3.")

(defconst eas-vega-other-custom-zone "America/Chicago"
  "The zone the Vega references were drawn in.")

(defconst eas-vega-other-custom-threshold 0.03
  "Largest differing-pixel ratio of a passing template.")

(defun eas-vega-other-custom-template-file (name)
  "The template file of Vega example NAME."
  (eas-test-file "templates/vega" (concat name ".json")))

(defun eas-vega-other-custom-template (name)
  "The template plist of Vega example NAME, outside the global registry."
  (let ((eas--templates nil))
    (eas-template-get (eas-template-load (eas-vega-other-custom-template-file name)))))

(defun eas-vega-other-custom-verdict (name)
  "The x-eas.vega object of Vega example NAME."
  (plist-get (plist-get (eas-vega-other-custom-template name) :meta) :vega))

(defun eas-vega-other-custom-spec (name &optional bindings)
  "Vega example NAME resolved with BINDINGS (default: its example)."
  (let* ((template (eas-vega-other-custom-template name))
         (bindings (or bindings (eas-template-read-bindings (eas-template-example-file template)))))
    (eas-resolve template bindings)))

(defun eas-vega-other-custom-estimate-width (text size &optional _weight)
  "Vega's text width estimate: 0.8 SIZE per character of TEXT, floored.
vg2svg without node-canvas lays text out this way, so the references
were; their glyphs are still drawn in the real font.  Characters count
as JavaScript does, in UTF-16 units: one past U+FFFF counts twice."
  (float (floor (* 0.8 size (+ (length text)
                               (cl-count-if (lambda (c) (> c #xFFFF)) text))))))

(defmacro eas-vega-other-custom--native (&rest body)
  "Run BODY in the references' zone, unrestricted by supported.json.
Text is measured as the references measured it."
  `(let ((eas-time-zone eas-vega-other-custom-zone) (eas-spec-supported-function nil))
     (cl-letf (((symbol-function 'eas-font-text-width)
                #'eas-vega-other-custom-estimate-width))
       ,@body)))

(defun eas-vega-other-custom-size (name)
  "The (W . H) pixel size NAME's x-eas.vega fits its container to, or nil.
A template with a \"container\" width or height declares the size of
its reference chart there."
  (when-let* ((size (plist-get (eas-vega-other-custom-verdict name) :size)))
    (cons (aref size 0) (aref size 1))))

(defun eas-vega-other-custom-svg (name &optional spec)
  "Native SVG of Vega example NAME (or of resolved SPEC)."
  (eas-vega-other-custom--native
   (eas-svg-render (eas-compile (or spec (eas-vega-other-custom-spec name))
                                :size (eas-vega-other-custom-size name)))))

(defun eas-vega-other-custom-text (name &optional spec)
  "Text rendering of Vega example NAME (or of resolved SPEC)."
  (eas-vega-other-custom--native
   (substring-no-properties
    (eas-text-render (eas-compile (or spec (eas-vega-other-custom-spec name))
                                  :target 'text :size '(:cols 80 :rows 24))))))

(defun eas-vega-other-custom-rasterizer-p ()
  "Non-nil when native SVG can be rasterized and PNGs decoded here."
  (and (executable-find eas-chart-rsvg-program) (zlib-available-p)))

(defun eas-vega-other-custom-compare (name &optional svg out)
  "Compare native SVG of NAME with its Vega reference PNG.
Keep the native PNG in OUT when non-nil.  Return `eas-png-compare's
plist, or nil without a rasterizer."
  (when (eas-vega-other-custom-rasterizer-p)
    (let ((mine (or out (make-temp-file "eas-vega" nil ".png"))))
      (unwind-protect
          (progn (eas-chart-rasterize (or svg (eas-vega-other-custom-svg name)) mine)
                 (let* ((native (eas-png-read mine))
                        (ref (eas-png-read (eas-test-file "test/vega-examples/ref" (concat name ".png"))))
                        (aligned (eas-png-compare native ref))
                        ;; Ink profiles can lock onto a period of a repetitive
                        ;; chart (timelines' rows); the unshifted overlay competes.
                        (unshifted (eas-png-compare native ref 0)))
                   (if (<= (plist-get aligned :ratio) (plist-get unshifted :ratio)) aligned unshifted)))
        (unless out (delete-file mine))))))

(defun eas-vega-other-custom-passes-p (cmp)
  "Non-nil when comparison CMP is within the pass bounds."
  (let ((delta (plist-get cmp :size-delta)))
    (and (<= (plist-get cmp :ratio) eas-vega-other-custom-threshold)
         (<= (max (abs (aref delta 0)) (abs (aref delta 1))) 8))))

;;; Templates

(defconst eas-vega-other-custom-statuses '("pass" "partial" "unsupported")
  "The verdicts x-eas.vega.status may record.")

(defun eas-vega-other-custom-example (name &optional limit)
  "The example bindings of NAME, each data slot cut to LIMIT rows."
  (let* ((template (eas-vega-other-custom-template name))
         (bindings (eas-template-read-bindings (eas-template-example-file template))))
    (if (not limit) bindings
      (cl-loop for (k v) on bindings by #'cddr
               append (list k (if (and (plist-get (plist-get (eas-template-slots template) k) :shape)
                                       (vectorp v) (> (length v) limit))
                                  (seq-subseq v 0 limit)
                                v))))))

(defun eas-vega-other-custom--keys (node)
  "Every object key in NODE, recursively, as strings."
  (cond ((vectorp node) (apply #'append (mapcar #'eas-vega-other-custom--keys node)))
        ((and (eas-object-p node) node)
         (cl-loop for (k v) on node by #'cddr
                  append (cons (eas-key-name k) (eas-vega-other-custom--keys v))))))

(ert-deftest eas-vega-other-custom-templates-declare-their-verdicts ()
  "Each of the thirteen templates loads as vega/NAME with an example and a verdict."
  (dolist (name eas-vega-other-custom-names)
    (let* ((template (eas-vega-other-custom-template name))
           (verdict (eas-vega-other-custom-verdict name)))
      (should (equal (plist-get template :name) (concat "vega/" name)))
      (should (file-exists-p (eas-template-example-file template)))
      (should (member (plist-get verdict :status) eas-vega-other-custom-statuses))
      (should (stringp (plist-get verdict :note)))
      (should (numberp (plist-get verdict :ratio)))
      ;; A data slot, and field slots (or a fields array) naming its columns.
      (let ((slots (eas-template-slots template)))
        (should (plist-get (plist-get slots :data) :shape))
        (should (or (plist-get slots :fields)
                    (cl-loop for (_ def) on slots by #'cddr
                             thereis (equal (plist-get def :type) "field"))))))))

(ert-deftest eas-vega-other-custom-templates-render-both-backends ()
  "Each template resolves to pure Vega-Lite and draws in svg and text."
  (dolist (name eas-vega-other-custom-names)
    (let ((spec (eas-vega-other-custom-spec name (eas-vega-other-custom-example name 300))))
      (should-not (seq-some (lambda (k) (string-prefix-p "x-eas" k))
                            (eas-vega-other-custom--keys spec)))
      (should (string-prefix-p "<svg" (eas-vega-other-custom-svg name spec)))
      (should (> (length (string-trim (eas-vega-other-custom-text name spec))) 0)))))

(defconst eas-vega-other-custom-nullable
  '(:axis :title :scale :legend :sort :stack :header :stroke)
  "Properties Vega-Lite's schema lets be null.")

(defun eas-vega-other-custom--schema-faults (node path)
  "Schema faults in resolved spec NODE at PATH, as a list of strings.
A null encoding channel, mark property or other non-nullable property
is a fault, as is a scale or legend on a datum channel."
  (cond ((vectorp node)
         (let ((i -1))
           (apply #'append (mapcar (lambda (n) (eas-vega-other-custom--schema-faults
                                                n (format "%s/%d" path (cl-incf i))))
                                   node))))
        ((and (eas-object-p node) node)
         (cl-loop for (k v) on node by #'cddr
                  for p = (format "%s/%s" path (eas-key-name k))
                  unless (eq k :values)
                  append (append
                          (and (eq v :null) (not (memq k eas-vega-other-custom-nullable))
                               (list (concat p " is null")))
                          (and (string-suffix-p "/encoding" path) (eas-object-p v)
                               (plist-member v :datum)
                               (or (plist-member v :scale) (plist-member v :legend))
                               (list (concat p " has a datum and a scale or legend")))
                          (eas-vega-other-custom--schema-faults v p))))))

(ert-deftest eas-vega-other-custom-templates-resolve-schema-valid ()
  "Each template has a title slot defaulting to empty and, with or
without a title, resolves free of null channels or mark properties and
of scales or legends on datum channels."
  (dolist (name eas-vega-other-custom-names)
    (let* ((template (eas-vega-other-custom-template name))
           (title (plist-get (eas-template-slots template) :title))
           (example (eas-vega-other-custom-example name 50)))
      (should (equal (list name (plist-get title :default)) (list name "")))
      (should (equal (list name (eas-vega-other-custom--schema-faults
                                 (eas-vega-other-custom-spec name example) ""))
                     (list name nil)))
      (should (equal (list name (eas-vega-other-custom--schema-faults
                                 (eas-vega-other-custom-spec name (plist-put example :title "T")) ""))
                     (list name nil))))))

(ert-deftest eas-vega-other-custom-templates-take-their-own-fields ()
  "Field slots rename the columns a template reads."
  (let* ((rows (vconcat (cl-loop for i below 12
                                 collect (list :when (format "2020-01-%02d" (1+ i)) :price (+ 100 (* i (if (cl-evenp i) 1 -1))))))))
    (should (string-prefix-p "<svg" (eas-vega-other-custom-svg
                                     "calendar-view"
                                     (eas-vega-other-custom-spec "calendar-view" (list :data rows :date "when" :value "price"))))))
  (let ((spec (eas-vega-other-custom-spec
               "parallel-coordinates"
               (list :data [(:a 1 :b 10 :c 5) (:a 2 :b 30 :c 1) (:a 3 :b 20)] :fields ["a" "b" "c"]))))
    (should (string-prefix-p "<svg" (eas-vega-other-custom-svg "parallel-coordinates" spec))))
  (eas-test-should-code "FIELD_MISSING"
    (eas-vega-other-custom-spec "timelines" (list :data [(:who "A" :born 0 :died 10 :enter 2 :leave 4)]
                                                  :events [] :label "name"))))

(ert-deftest eas-vega-other-custom-matches-the-references ()
  "Each template's native rendering scores within its recorded verdict.
A pass stays within the pass bounds; every template stays within a
quarter (plus 0.005) of its recorded ratio."
  :tags '(:gallery)
  (unless (eas-vega-other-custom-rasterizer-p)
    (eas-test-skip (format "%s is not on PATH to rasterize native SVG (scripts/eas-spikes/vega-oracle/setup.sh provides one)"
                           eas-chart-rsvg-program)))
  (dolist (name eas-vega-other-custom-names)
    (let* ((verdict (eas-vega-other-custom-verdict name))
           (cmp (eas-vega-other-custom-compare name)))
      (should (equal (list name (<= (plist-get cmp :ratio) (+ 0.005 (* 1.25 (plist-get verdict :ratio)))))
                     (list name t)))
      (when (equal (plist-get verdict :status) "pass")
        (should (equal (list name (eas-vega-other-custom-passes-p cmp)) (list name t)))))))

;;; Transforms

(ert-deftest eas-vega-other-custom-parallel-coordinates-scales-each-field ()
  (let* ((rows [(:a 1 :b 10) (:a 3 :b 30) (:a 2) (:a 2 :b 20)])
         (out (eas-transform-apply-domain '(:x-eas:transform "parallel-coordinates" :fields ["a" "b"]) rows))
         (lines (seq-filter (lambda (r) (equal (plist-get r :part) "line")) out))
         (ticks (seq-filter (lambda (r) (equal (plist-get r :part) "tick")) out)))
    ;; The row without b is dropped; three rows times two fields remain.
    (should (= (length lines) 6))
    (should (equal (mapcar (lambda (r) (plist-get r :norm)) (seq-filter (lambda (r) (equal (plist-get r :key) "a")) lines))
                   '(0.0 1.0 0.5)))
    (should (= (length (seq-filter (lambda (r) (equal (plist-get r :part) "axis")) out)) 2))
    ;; d3's ticks and labels: 10..30 by 2 for b.
    (should (member "30" (mapcar (lambda (r) (plist-get r :label)) ticks)))
    (should (seq-every-p (lambda (r) (<= 0 (plist-get r :norm) 1)) ticks))))

(ert-deftest eas-vega-other-custom-serpentine-lays-out-the-path ()
  (let* ((out (eas-transform-apply-domain
               '(:x-eas:transform "serpentine" :field "year" :domain [1926 2026])
               [(:year 1928 :label "A") (:year 1954 :label "C")]))
         (of (lambda (c) (seq-filter (lambda (r) (equal (plist-get r :category) c)) out)))
         (path (funcall of "serpentine"))
         (start (car (funcall of "start"))) (c (cadr (funcall of "milestone"))))
    (should (= (length path) 1000))
    (should (= (length (funcall of "tick")) 21))
    (should (equal (list (plist-get start :x) (plist-get start :y) (plist-get start :type)) '(0.0 0.0 "straight")))
    ;; Three segments start; 1954 sits on the first arc, on its right.
    (should (= (length (seq-filter (lambda (r) (eq (plist-get r :first) t)) path)) 3))
    (should (equal (list (plist-get c :type) (plist-get c :side)) '("arc" "right")))
    (should (< 0 (plist-get c :y) 125))
    ;; The second run goes back, right to left.
    (should (equal (plist-get (seq-find (lambda (r) (= (plist-get r :i) 1)) path) :direction) "←"))))

;;; Engine features the templates needed

(ert-deftest eas-vega-other-custom-sequential-color-domain ()
  "nice, zero, an explicit domain and clamp shape a continuous color scale."
  (let ((domain (lambda (sp) (eas-scale-sequential-domain sp '(3.1 24.4)))))
    (should (equal (funcall domain nil) [3.1 24.4]))
    (should (equal (funcall domain '(:nice t)) [2.0 26.0]))
    (should (equal (funcall domain '(:zero t)) [0 24.4]))
    (should (equal (funcall domain '(:domain [-1 1] :nice t)) [-1 1])))
  (should (equal (eas-scale-sequential-range '(:reverse t) ["#000" "#fff"]) ["#fff" "#000"]))
  (let ((scale (eas-scale-sequential-clamp '(:clamp t) (list :type "sequential" :domain [-1 1] :mid 0
                                                             :range ["#0000ff" "#ffffff" "#ff0000"]))))
    (should (equal (eas-scale-apply scale 5) (eas-scale-apply scale 1)))))

(ert-deftest eas-vega-other-custom-round-snaps-scales ()
  (let ((point (eas-scale-round-band (append (eas-scale-band "point" ["a" "b" "c" "d" "e" "f" "g"] [0 700] nil 0)
                                             (list :round t)))))
    (should (equal (mapcar (lambda (v) (eas-scale-apply point v)) '("a" "b" "g")) '(2.0 118.0 698.0))))
  (let ((linear (append (eas-scale-continuous "linear" 0 3 [0 10]) (list :round t))))
    (should (equal (eas-scale-apply linear 1) 3.0))
    (should (equal (funcall (eas-scale-fn linear) 1) 3.0))))

(ert-deftest eas-vega-other-custom-expression-and-format-helpers ()
  (should (= (eas-expr-evaluate "indexof(['a', 'b'], 'b')" nil) 1))
  (should (= (eas-expr-evaluate "indexof([1, 2], 3)" nil) -1))
  (let ((fmt (eas-scale-tick-format (eas-scale-continuous "linear" -1 0.2 [0 1]) 5 "+%")))
    (should (equal (mapcar fmt '(0.2 0 -0.4)) '("+20%" "+0%" "−40%")))))

(ert-deftest eas-vega-other-custom-gradient-legend-details ()
  "A gradient's \"%\" labels take the ticks' precision; titleOrient left puts
the title beside a horizontal bar."
  (let* ((spec '(:data (:values [(:v -0.06) (:v 0.06)]) :mark "rect"
                 :encoding (:color (:field "v" :type "quantitative"
                                    :legend (:format "%" :orient "top" :direction "horizontal"
                                             :titleOrient "left" :title "Change")))))
         (legend (car (append (plist-get (aref (plist-get (eas-compile spec) :views) 0) :legends) nil)))
         (labels (mapcar (lambda (e) (plist-get e :label)) (plist-get legend :entries))))
    (should (equal labels (quote ("−5%" "0%" "5%"))))
    ;; Beside the title, not under it: the bar starts right of the
    ;; legend's left edge, at its top.
    (should (> (aref (plist-get legend :bar) 0) (+ (plist-get legend :x) 30)))
    (should (= (aref (plist-get legend :bar) 1) (plist-get legend :y)))))

(ert-deftest eas-vega-other-custom-rotated-text-grows-the-canvas ()
  "A rotated text mark's turned box counts toward the chart's bounds."
  (let* ((spec (lambda (angle)
                 (list :width 100 :height 50 :data '(:values [(:x 0 :t "a long label above the plot")])
                       :mark (list :type "text" :angle angle :align "left" :baseline "alphabetic")
                       :encoding '(:x (:field "x" :type "quantitative") :y (:value -10) :text (:field "t")))))
         (h (lambda (angle) (plist-get (plist-get (eas-compile (funcall spec angle)) :size) :h))))
    (should (> (funcall h -45) (+ (funcall h 0) 30)))))

(ert-deftest eas-vega-other-custom-rule-x2-value-is-pixels ()
  (let* ((scene (eas-compile '(:width 100 :height 50 :data (:values [(:y 1)]) :mark "rule"
                               :encoding (:y (:field "y" :type "quantitative") :x (:value 10) :x2 (:value 30)))))
         (item (aref (plist-get (car (append (plist-get (aref (plist-get scene :views) 0) :marks) nil)) :items) 0)))
    (should (= (- (plist-get item :x2) (plist-get item :x1)) 20))
    (should (= (plist-get item :y1) (plist-get item :y2)))))

(provide 'eas-vega-other-custom-test)
;;; eas-vega-other-custom-test.el ends here

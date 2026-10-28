;;; eas-sankey-test.el --- the sankey transform, template, hover and text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The sankey (flow) kind: its layout by hand and against d3-sankey
;; 0.12.3's own numbers (node 22, sankey().nodeId(d => d.name),
;; the parameters each test names), a cycle rejected, the template
;; drawn as SVG and text, and hover showing a node's or a link's
;; fields, whatever they are.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-text-check)
(require 'eas-test-support)

(defun eas-sankey-test--nodes (out)
  "The node rows of sankey output OUT."
  (seq-filter (lambda (r) (equal (plist-get r :eas_kind) "node")) out))

(defun eas-sankey-test--links (out)
  "One row per link of sankey output OUT: its first band vertex."
  (seq-filter (lambda (r) (and (equal (plist-get r :eas_kind) "link") (= (plist-get r :eas_step) 0))) out))

(defun eas-sankey-test--close (actual expected &optional tolerance)
  "Assert number trees ACTUAL and EXPECTED agree within TOLERANCE (0)."
  (if (consp expected)
      (progn (should (= (length actual) (length expected)))
             (cl-mapc (lambda (a e) (eas-sankey-test--close a e tolerance)) actual expected))
    (if (numberp expected)
        (should (<= (abs (- actual expected)) (or tolerance 0)))
      (should (equal actual expected)))))

(defun eas-sankey-test--layout (out)
  "OUT's nodes as (NAME LAYER X0 Y0 Y1) and links as (WIDTH Y-SOURCE Y-TARGET)."
  (list (mapcar (lambda (n) (list (plist-get n :name) (plist-get n :eas_layer) (plist-get n :eas_x0)
                                  (plist-get n :eas_y0) (plist-get n :eas_y1)))
                (eas-sankey-test--nodes out))
        (mapcar (lambda (l) (list (plist-get l :eas_width) (plist-get l :eas_y_source) (plist-get l :eas_y_target)))
                (eas-sankey-test--links out))))

(defconst eas-sankey-test--graph
  '([(:name "a") (:name "b") (:name "c") (:name "d") (:name "e") (:name "f") (:name "g")]
    . [(:source "a" :target "c" :value 5) (:source "a" :target "d" :value 3) (:source "b" :target "c" :value 2)
       (:source "b" :target "e" :value 4) (:source "c" :target "f" :value 6) (:source "c" :target "g" :value 1)
       (:source "d" :target "f" :value 2) (:source "e" :target "g" :value 4) (:source "a" :target "g" :value 1)])
  "Seven nodes in three columns; a -> g skips a column.")

(defun eas-sankey-test--run (graph &rest params)
  "The sankey transform over GRAPH (NODES . LINKS) with PARAMS."
  (eas-sankey-transform (car graph) (append (list :links (cdr graph) :id "name") params)))

;;; Layout

(ert-deftest eas-sankey-layout-by-hand ()
  "Two sources into one sink, every number worked out by hand.
A 30 and B 10 flow into C (40) on 100x100, nodes 10 wide, padding 10,
no relaxation.  ky is the tighter column's: (100 - 10) / 40 = 2.25
beats 100 / 40.  Column 0 stacks A [0, 67.5] and B [77.5, 100] and has
no slack; C is 90 tall, centred: [5, 95].  Bands are value * ky wide
and leave and enter their nodes stacked in order."
  (let ((out (eas-sankey-test--run
              '([(:name "A") (:name "B") (:name "C")]
                . [(:source "A" :target "C" :value 30) (:source "B" :target "C" :value 10)])
              :width 100 :height 100 :nodeWidth 10 :nodePadding 10 :iterations 0)))
    (eas-sankey-test--close
     (eas-sankey-test--layout out)
     '((("A" 0 0.0 0.0 67.5) ("B" 0 0.0 77.5 100.0) ("C" 1 90.0 5.0 95.0))
       ((67.5 33.75 38.75) (22.5 88.75 83.75))))
    ;; Node value is the larger of in- and outflow; each band spans
    ;; source.x1 to target.x0 with its width as its height everywhere.
    (should (equal (mapcar (lambda (n) (plist-get n :value)) (eas-sankey-test--nodes out)) '(30 10 40)))
    (let ((band (seq-filter (lambda (r) (equal (plist-get r :eas_link) 0)) out)))
      (should (= (plist-get (car band) :eas_x) 10.0))
      (should (= (plist-get (car (last band)) :eas_x) 90.0))
      (dolist (r band)
        (should (< (abs (- (- (plist-get r :eas_y1) (plist-get r :eas_y0)) 67.5)) 1e-9))))))

(ert-deftest eas-sankey-link-widths-are-proportional-to-value ()
  "Every band is value * ky wide, one ky for the whole diagram."
  (let* ((links (eas-sankey-test--links (eas-sankey-test--run eas-sankey-test--graph)))
         (ky (/ (plist-get (car links) :eas_width) (plist-get (car links) :value))))
    (should (= (length links) 9))
    (dolist (l links)
      (should (< (abs (- (plist-get l :eas_width) (* ky (plist-get l :value)))) 1e-9)))
    ;; Column 0 is the tightest: (400 - 8) / (9 + 6).
    (should (< (abs (- ky (/ 392.0 15))) 1e-12))))

(ert-deftest eas-sankey-layout-matches-d3-sankey ()
  "The port lays nodes and links out as d3-sankey 0.12.3 does, digit for digit.
d3: extent [[0, 0], [600, 400]], nodeWidth 24, nodePadding 8."
  ;; nodeAlign(sankeyJustify), iterations(6): six relaxation passes.
  (eas-sankey-test--close
   (eas-sankey-test--layout (eas-sankey-test--run eas-sankey-test--graph))
   '((("a" 0 0.0 0 235.2) ("b" 0 0.0 243.2 400.0000000000001) ("c" 1 288.0 0 182.9333333333334)
      ("d" 1 288.0 190.9333333333334 269.3333333333334) ("e" 1 288.0 295.46666666666675 400)
      ("f" 2 576.0 2.86852608337675 211.93519275004343) ("g" 2 576.0 219.93519275004343 376.73519275004344))
     ((130.66666666666666 65.33333333333333 65.33333333333333) (78.4 169.86666666666667 230.13333333333338)
      (52.266666666666666 269.3333333333333 156.79999999999998) (104.53333333333333 347.7333333333333 347.7333333333334)
      (156.8 78.4 81.26852608337676) (26.133333333333333 169.86666666666667 259.1351927500434)
      (52.266666666666666 217.06666666666672 185.8018594167101) (104.53333333333333 347.7333333333334 324.46852608337673)
      (26.133333333333333 222.13333333333333 233.0018594167101))))
  ;; nodeAlign(sankeyLeft), iterations(0): columns in data order, centred.
  (eas-sankey-test--close
   (eas-sankey-test--layout (eas-sankey-test--run eas-sankey-test--graph :align "left" :iterations 0))
   '((("a" 0 0.0 0 235.2) ("b" 0 0.0 243.2 400) ("c" 1 288.0 4.533333333333331 187.46666666666667)
      ("d" 1 288.0 200 278.40000000000003) ("e" 1 288.0 290.9333333333334 395.4666666666667)
      ("f" 2 576.0 8.711111111111109 217.77777777777777) ("g" 2 576.0 234.48888888888888 391.2888888888889))
     ((130.66666666666666 65.33333333333333 69.86666666666666) (78.4 169.86666666666667 239.2)
      (52.266666666666666 269.3333333333333 161.33333333333331) (104.53333333333333 347.7333333333333 343.20000000000005)
      (156.8 82.93333333333334 87.11111111111111) (26.133333333333333 174.4 273.68888888888887)
      (52.266666666666666 226.13333333333333 191.64444444444445) (104.53333333333333 343.20000000000005 339.0222222222222)
      (26.133333333333333 222.13333333333333 247.55555555555554)))))

(ert-deftest eas-sankey-node-order ()
  "Columns follow the longest path; relaxation reorders them to uncross links.
S1 feeds T2 and S2 feeds T1, so in data order the bands cross."
  (let ((graph '([(:name "S1") (:name "S2") (:name "T1") (:name "T2")]
                 . [(:source "S1" :target "T2" :value 10) (:source "S2" :target "T1" :value 10)])))
    (cl-flet ((columns (out)
                (mapcar (lambda (layer)
                          (mapcar (lambda (n) (plist-get n :name))
                                  (sort (seq-filter (lambda (n) (= (plist-get n :eas_layer) layer))
                                                    (eas-sankey-test--nodes out))
                                        (lambda (a b) (< (plist-get a :eas_y0) (plist-get b :eas_y0))))))
                        '(0 1))))
      ;; No relaxation: data order (d3: S1 [0, 196], T1 [0, 196]).
      (should (equal (columns (eas-sankey-test--run graph :iterations 0)) '(("S1" "S2") ("T1" "T2"))))
      ;; Relaxed, S2 rises to meet T1 (d3: S2 [0, 196], S1 [204, 400]).
      (let ((out (eas-sankey-test--run graph)))
        (should (equal (columns out) '(("S2" "S1") ("T1" "T2"))))
        (eas-sankey-test--close (eas-sankey-test--layout out)
                                '((("S1" 0 0.0 204 400) ("S2" 0 0.0 0 196) ("T1" 1 576.0 0 196) ("T2" 1 576.0 204 400))
                                  ((196 302 302) (196 98 98)))
                                1e-9))
      ;; nodeSort input keeps data order through relaxation; a field sorts.
      (should (equal (columns (eas-sankey-test--run graph :nodeSort "input")) '(("S1" "S2") ("T1" "T2"))))
      (should (equal (columns (eas-sankey-test--run
                               (cons (vconcat (seq-map-indexed (lambda (n i) (append n (list :rank (- 4 i)))) (car graph)))
                                     (cdr graph))
                               :nodeSort "rank" :iterations 0))
                     '(("S2" "S1") ("T2" "T1")))))
    ;; Depth is the longest path in: a -> g skips to the last column,
    ;; and justify puts every sink last.
    (should (equal (mapcar (lambda (n) (list (plist-get n :name) (plist-get n :eas_depth) (plist-get n :eas_layer)))
                           (eas-sankey-test--nodes (eas-sankey-test--run eas-sankey-test--graph)))
                   '(("a" 0 0) ("b" 0 0) ("c" 1 1) ("d" 1 1) ("e" 1 1) ("f" 2 2) ("g" 2 2))))
    (should (equal (mapcar (lambda (n) (plist-get n :eas_layer))
                           (eas-sankey-test--nodes
                            (eas-sankey-test--run '([(:name "x") (:name "y") (:name "z")]
                                                    . [(:source "x" :target "y" :value 1)
                                                       (:source "y" :target "z" :value 1)
                                                       (:source "x" :target "z" :value 1)]))))
                   '(0 1 2)))))

(ert-deftest eas-sankey-rejects-cycles-and-bad-links ()
  "A cycle, a self-loop, a dangling end or a negative value fail as data."
  (let ((err (should-error (eas-sankey-test--run
                            '([(:name "a") (:name "b") (:name "c")]
                              . [(:source "a" :target "b" :value 1) (:source "b" :target "c" :value 1)
                                 (:source "c" :target "a" :value 1)]))
                           :type 'eas-shape-invalid)))
    (should (equal (plist-get (cddr err) :code) "SHAPE_INVALID"))
    (should (string-match-p "cycle (a -> b -> c -> a)" (cadr err)))
    (should (equal (plist-get (cddr err) :cycle) ["a" "b" "c" "a"])))
  (let ((err (should-error (eas-sankey-test--run '([(:name "a")] . [(:source "a" :target "a" :value 1)]))
                           :type 'eas-shape-invalid)))
    (should (string-match-p "cycle (a -> a)" (cadr err))))
  (eas-test-should-code "INVALID_INPUT"
    (eas-sankey-test--run '([(:name "a")] . [(:source "a" :target "zz" :value 1)])))
  (eas-test-should-code "INVALID_INPUT"
    (eas-sankey-test--run '([(:name "a") (:name "b")] . [(:source "a" :target "b" :value -1)])))
  (eas-test-should-code "INVALID_INPUT"
    (eas-sankey-test--run '([(:name "a") (:name "b")] . [(:source "a" :target "b" :value 1)]) :align "middle"))
  ;; The template surfaces the same error from resolve.
  (eas-test-should-code "SHAPE_INVALID"
    (eas-resolve "sankey" '(:nodes [(:name "a") (:name "b")]
                            :links [(:source "a" :target "b" :value 1) (:source "b" :target "a" :value 1)]))))

(ert-deftest eas-sankey-bands-follow-d3s-link-curve ()
  "A band's middle is d3's sankeyLinkHorizontal cubic, on one shared x grid."
  ;; The cubic's control points sit at the middle x: halfway along it
  ;; is halfway down, and its ends are exact.
  (should (= (eas-sankey-band-y 0 10 100 50 0) 10))
  (should (= (eas-sankey-band-y 0 10 100 50 100) 50))
  (should (< (abs (- (eas-sankey-band-y 0 10 100 50 50) 30)) 1e-9))
  (should (< (eas-sankey-band-y 0 10 100 50 25) 20))
  (let* ((out (eas-sankey-test--run eas-sankey-test--graph :samples 4))
         (xs (lambda (link) (mapcar (lambda (r) (plist-get r :eas_x))
                                    (seq-filter (lambda (r) (equal (plist-get r :eas_link) link)) out)))))
    ;; a -> c and b -> c cross one gap (24 to 288) in 4 steps; a -> g
    ;; crosses two and shares the first gap's x.
    (should (equal (funcall xs 0) '(24.0 90.0 156.0 222.0 288.0)))
    (should (equal (funcall xs 0) (funcall xs 2)))
    (should (equal (seq-take (funcall xs 8) 5) (funcall xs 0)))
    (should (equal (seq-drop (funcall xs 8) 5) '(312.0 378.0 444.0 510.0 576.0)))))

;;; The template: SVG, text and hover

(defconst eas-sankey-test--bindings
  '(:title "Flows"
    :nodes [(:name "Source A" :p 0.8 :grade "B") (:name "Source B" :p 0.35 :grade "D")
            (:name "Hub") (:name "Sink X" :note "kept") (:name "Sink Y")]
    :links [(:source "Source A" :target "Hub" :value 6 :why "direct")
            (:source "Source B" :target "Hub" :value 3)
            (:source "Hub" :target "Sink X" :value 5)
            (:source "Hub" :target "Sink Y" :value 4)
            (:source "Source A" :target "Sink Y" :value 2)])
  "A small flow whose nodes and links carry fields the template never names.")

(defmacro eas-sankey-test--with-view (var &rest body)
  "Open the sankey test bindings as view VAR in a fresh registry; run BODY."
  (declare (indent 1))
  `(let* ((eas-views (make-hash-table :test 'equal))
          (,var (eas-view-open "sankey" :bindings eas-sankey-test--bindings :id "flow")))
     ,@body))

(defun eas-sankey-test--mark (view id)
  "Mark ID of VIEW's scene."
  (eas-scene-mark (eas-view-scene view) id))

(defun eas-sankey-test--hover (view px)
  "The hover tooltip at PX in VIEW as an alist (TITLE . VALUE)."
  (mapcar (lambda (p) (cons (plist-get p :title) (plist-get p :value)))
          (plist-get (plist-get (eas-dispatch view (list :type "pointermove" :px px)) :hover) :tooltip)))

(ert-deftest eas-sankey-template-resolves-and-draws ()
  "The example resolves to plain Vega-Lite and draws bars and bands in SVG."
  (let* ((spec (eas-resolve "sankey" (eas-template-example "sankey")))
         (svg (eas-svg-render (eas-compile spec))))
    (should-not (eas-spec-check spec))
    (should-not (plist-get spec :x-eas))
    (should (member "sankey" (eas-template-names)))
    (should (equal (eas-template-get "flow") (eas-template-get "sankey")))
    ;; Twelve bands and nine node bars.
    (should (= (with-temp-buffer (insert svg) (how-many "<path d=\"M" (point-min) (point-max))) 12))
    (should (string-match-p ">Electricity<" svg))))

(ert-deftest eas-sankey-hover-shows-every-field ()
  "Hover on a node or a band lists its fields, extra ones included, eas_* not."
  (eas-sankey-test--with-view v
    (let* ((nodes (eas-sankey-test--mark v "main/1"))
           (a (aref (plist-get nodes :items) 0))
           (centre (vector (+ (plist-get a :x) (/ (plist-get a :w) 2.0)) (+ (plist-get a :y) (/ (plist-get a :h) 2.0)))))
      (should (equal (eas-sankey-test--hover v centre)
                     '(("name" . "Source A") ("p" . "0.8") ("grade" . "B") ("value" . "8")))))
    (let* ((x (aref (plist-get (eas-sankey-test--mark v "main/1") :items) 3)))
      (should (equal (eas-sankey-test--hover v (vector (+ (plist-get x :x) 1) (+ (plist-get x :y) 2)))
                     '(("name" . "Sink X") ("note" . "kept") ("value" . "5")))))
    ;; Inside a band, nearer the top edge of the band under it than its
    ;; own top edge: still it, for hover and click.  Hub's two bands
    ;; leave it edge to edge; probe where no third band crosses them.
    (let* ((bands (eas-sankey-test--mark v "main/0"))
           (items (append (plist-get bands :items) nil))
           (row-of (lambda (item) (aref (plist-get bands :rows) (aref (plist-get item :datum) 0))))
           (hub (seq-filter (lambda (item) (equal (plist-get (funcall row-of item) :source) "Hub")) items))
           (upper (car (sort hub (lambda (a b) (< (aref (aref (plist-get a :points) 1) 1)
                                                  (aref (aref (plist-get b :points) 1) 1))))))
           (inside (lambda (item x y)
                     (cl-loop for p across (plist-get item :points) for q across (plist-get item :base)
                              thereis (and (= (aref p 0) x) (<= (aref p 1) y (aref q 1))))))
           (px (cl-loop for k from 1 below (length (plist-get upper :points))
                        for top = (aref (plist-get upper :points) k)
                        for y = (- (aref (aref (plist-get upper :base) k) 1) 1)
                        when (= 1 (cl-count-if (lambda (item) (funcall inside item (aref top 0) y)) items))
                        return (vector (aref top 0) y)))
           (target (plist-get (funcall row-of upper) :target)))
      (should (= (length hub) 2))
      (should px)
      (should (equal (eas-sankey-test--hover v px)
                     (list '("source" . "Hub") (cons "target" target)
                           (cons "value" (if (equal target "Sink X") "5" "4")))))
      (should (equal (plist-get (plist-get (plist-get (eas-dispatch v (list :type "click" :px px)) :click) :row)
                                :target)
                     target)))
    ;; A link's own extra fields show too.
    (let* ((bands (eas-sankey-test--mark v "main/0"))
           (band (seq-find (lambda (item) (equal (plist-get (aref (plist-get bands :rows) (aref (plist-get item :datum) 0)) :why)
                                                 "direct"))
                           (plist-get bands :items)))
           (top (aref (plist-get band :points) 8)) (bottom (aref (plist-get band :base) 8)))
      (should (equal (eas-sankey-test--hover v (vector (aref top 0) (/ (+ (aref top 1) (aref bottom 1)) 2)))
                     '(("source" . "Source A") ("target" . "Hub") ("value" . "6") ("why" . "direct")))))))

(ert-deftest eas-sankey-renders-as-terminal-text ()
  "The text rendering passes the checker, names nodes and hovers by cell."
  (eas-sankey-test--with-view v
    (eas-view-resize v '(:cols 70 :rows 18) 'text)
    (let* ((scene (eas-view-scene v))
           (text (eas-text-render scene))
           (lines (split-string text "\n")))
      (should-not (eas-text-check scene 70))
      (should (<= (apply #'max (mapcar #'string-width lines)) 70))
      (dolist (name '("Source A" "Source B" "Hub" "Sink X" "Sink Y"))
        (should (string-match-p (regexp-quote name) text)))
      ;; Node cells carry the node's fields as help-echo; bands have cells.
      (let (echoes marks)
        (dotimes (i (length text))
          (when-let* ((h (get-text-property i 'help-echo text))) (cl-pushnew h echoes :test #'equal))
          (when-let* ((m (get-text-property i 'eas-mark text))) (cl-pushnew m marks :test #'equal)))
        (should (member "name: Source B\np: 0.35\ngrade: D\nvalue: 3" echoes))
        (should (member "main/0" marks))
        (should (member "main/1" marks))))))

(ert-deftest eas-sankey-data-tooltip-on-any-mark ()
  "mark.tooltip {\"content\": \"data\"} shows the row's fields; encoding.tooltip wins."
  (let* ((spec '(:data (:values [(:k "a" :v 3 :eas_hidden 1 :nested [1 2])])
                 :mark (:type "bar" :tooltip (:content "data"))
                 :encoding (:x (:field "k" :type "nominal") :y (:field "v" :type "quantitative"))))
         (scene (eas-compile (eas-resolve-spec spec)))
         (item (aref (plist-get (eas-scene-mark scene "main/0") :items) 0)))
    (should (equal (plist-get item :tooltip)
                   [(:title "k" :value "a") (:title "v" :value "3") (:title "nested" :value "[1,2]")])))
  (should (equal (eas-encode-tooltip '(:tooltip [(:field "v")]) '(:tooltip (:content "data")) '(:k "a" :v 3))
                 [(:title "v" :value "3")])))

(provide 'eas-sankey-test)
;;; eas-sankey-test.el ends here

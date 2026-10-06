;;; eas-vega-hierarchy-test.el --- Vega gallery hierarchy templates and transforms -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The hierarchy transforms (eas-hierarchy*.el), linkpath
;; (eas-linkpath.el) and the templates/vega/ templates of the Vega
;; gallery's tree and network examples (tree-layout,
;; radial-tree-layout, treemap, circle-packing, sunburst,
;; zoomable-circle-packing, edge-bundling).
;;
;; Expected layout numbers are Vega 6's own (vega-hierarchy over
;; test/vega-examples/data/flare.json), so the ports are held to d3's
;; arithmetic, not to a re-derivation.
;;
;; The :gallery tests render each template's example natively, rasterize
;; it with rsvg-convert and compare it with test/vega-examples/ref/.
;; Those references come from vg2svg without node-canvas, so Vega sized
;; their canvas from estimated text widths: the images are aligned by
;; plot origin (`eas-vega-hierarchy-ref-origins') before
;; `eas-png-compare' refines the offset by at most 2 pixels.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-png)
(require 'eas-test-support)

;;; Helpers

(defconst eas-vega-hierarchy-examples
  '("tree-layout" "radial-tree-layout" "treemap" "circle-packing" "sunburst"
    "zoomable-circle-packing" "edge-bundling")
  "The Vega gallery examples these templates reproduce.")

(defconst eas-vega-hierarchy-ref-origins
  '(("tree-layout" 48 8) ("zoomable-circle-packing" 10 10) ("treemap" 2.5 2.5))
  "Plot origins of the reference PNGs that are not at (5, 5), from vg2svg.")

(defun eas-vega-hierarchy-template (name)
  "Load templates/vega/NAME.json and return its registry name."
  (eas-template-load (eas-test-file "templates/vega" (concat name ".json"))))

(defun eas-vega-hierarchy-example (name)
  "The example bindings of templates/vega/NAME.json."
  (eas-json-read-file (eas-test-file "examples/vega" (concat name ".data.json"))))

(defun eas-vega-hierarchy-spec (name &optional bindings)
  "Template NAME resolved with BINDINGS (its example by default)."
  (eas-resolve (eas-vega-hierarchy-template name) (or bindings (eas-vega-hierarchy-example name))))

(defun eas-vega-hierarchy-meta (name)
  "The x-eas.vega block of templates/vega/NAME.json."
  (plist-get (plist-get (eas-json-read-file (eas-test-file "templates/vega" (concat name ".json"))) :x-eas)
             :vega))

(defun eas-vega-hierarchy-shift (img dx dy w h bg)
  "Decoded IMG moved by DX DY onto a W x H canvas of color BG (R G B A)."
  (let* ((iw (plist-get img :w)) (ih (plist-get img :h)) (src (plist-get img :rgba))
         (out (apply #'unibyte-string (apply #'append (make-list (* w h) bg)))))
    (dotimes (y ih)
      (let* ((ty (+ y dy)) (x0 (max 0 dx)) (x1 (min w (+ iw dx))))
        (when (and (>= ty 0) (< ty h) (< x0 x1))
          (store-substring out (* 4 (+ (* ty w) x0))
                           (substring src (* 4 (+ (* y iw) (- x0 dx))) (* 4 (+ (* y iw) (- x1 dx))))))))
    (list :w w :h h :rgba out)))

(defun eas-vega-hierarchy-compare (name)
  "Compare template NAME's native rendering with Vega's reference PNG.
Return the `eas-png-compare' plist; skip when rsvg-convert is missing."
  (unless (executable-find eas-chart-rsvg-program)
    (eas-test-skip (format "%s not on PATH; it rasterizes the native SVG for the reference comparison"
                           eas-chart-rsvg-program)))
  (let* ((scene (eas-compile (eas-vega-hierarchy-spec name)))
         (bounds (plist-get (aref (plist-get scene :views) 0) :bounds))
         (origin (or (cdr (assoc name eas-vega-hierarchy-ref-origins)) '(5 5)))
         (png (make-temp-file "eas-vega-hierarchy" nil ".png")))
    (unwind-protect
        (progn
          (eas-chart-rasterize (eas-svg-render scene) png)
          (let* ((ref (eas-png-read (eas-test-file "test/vega-examples/ref" (concat name ".png"))))
                 (mine (eas-png-read png)))
            (eas-png-compare (eas-vega-hierarchy-shift mine (round (- (nth 0 origin) (aref bounds 0)))
                                                       (round (- (nth 1 origin) (aref bounds 1)))
                                                       (plist-get ref :w) (plist-get ref :h)
                                                       (eas-png--background ref))
                             ref 2)))
      (delete-file png))))


(defun eas-vega-hierarchy-flare ()
  "The flare hierarchy rows of the Vega gallery."
  (eas-json-read-file (eas-test-file "test/vega-examples/data/flare.json")))

(defun eas-vega-hierarchy-run (transforms &optional rows)
  "ROWS (flare by default) through the domain TRANSFORMS, plists in order."
  (let ((rows (or rows (eas-vega-hierarchy-flare))))
    (dolist (tr transforms rows)
      (setq rows (eas-transform-apply-domain tr rows)))))

(defun eas-vega-hierarchy-should-near (row expected)
  "Assert ROW holds EXPECTED (a plist of numbers) within 1e-9."
  (cl-loop for (k v) on expected by #'cddr
           do (should (< (abs (- (plist-get row k) v)) 1e-9))))

;;; stratify and friends

(ert-deftest eas-vega-hierarchy-stratify-checks-one-tree ()
  (let ((rows (eas-vega-hierarchy-run '((:x-eas:transform "stratify")))))
    (should (equal (seq-take (aref rows 0) 6) '(:id 1 :name "flare" :depth 0)))
    (should (= (plist-get (aref rows 0) :children) 10))
    (should (= (plist-get (aref rows 3) :depth) 3)))
  ;; Keys compare as strings, as d3.stratify's do.
  (should (= (length (eas-vega-hierarchy-run '((:x-eas:transform "stratify"))
                                             [(:id "1") (:id 2 :parent 1) (:id 3 :parent "2")]))
             3))
  (dolist (case '(([(:id 1) (:id 2)] . 1)
                  ([(:id 1) (:id 2 :parent 9)] . 1)
                  ([(:id 1) (:id 1 :parent 1)] . 1)
                  ([(:id 1) (:id 2 :parent 3) (:id 3 :parent 2)] . 1)))
    (let ((err (eas-test-should-code "SHAPE_INVALID"
                 (eas-vega-hierarchy-run '((:x-eas:transform "stratify")) (car case)))))
      (should (equal (plist-get err :index) (cdr case))))))

(ert-deftest eas-vega-hierarchy-nest-builds-a-tree-of-groups ()
  (let* ((rows [(:a "x" :b 1 :v 3) (:a "x" :b 2 :v 4) (:a "y" :b 1 :v 5)])
         (out (eas-vega-hierarchy-run '((:x-eas:transform "nest" :keys ["a" "b"])) rows)))
    ;; root, then each group as it first appears, each row after its group
    (should (= (length out) 9))
    (should (equal (mapcar (lambda (r) (plist-get r :id)) out)
                   '("root" "root/x" "root/x/1" "root/x/1#0" "root/x/2" "root/x/2#1" "root/y" "root/y/1" "root/y/1#2")))
    (should (equal (mapcar (lambda (r) (plist-get r :key)) (seq-take out 3)) '("root" "x" 1)))
    (should (equal (plist-get (aref out 8) :parent) "root/y/1"))
    (let ((laid (eas-vega-hierarchy-run '((:x-eas:transform "treemap" :field "v" :size [12 1])) out)))
      (should (equal (mapcar (lambda (i) (plist-get (aref laid i) :x1)) '(3 5 8)) '(3.0 7.0 12.0))))))

(ert-deftest eas-vega-hierarchy-treelinks-and-treepath ()
  (let* ((tree (eas-vega-hierarchy-flare))
         (links (eas-vega-hierarchy-run '((:x-eas:transform "treelinks")) tree)))
    (should (= (length links) (1- (length tree))))
    (should (equal (plist-get (plist-get (aref links 0) :source) :name) "flare")))
  ;; Vega's treePath('tree', s, t), ids along the path.
  (let ((paths (eas-vega-hierarchy-run
                `((:x-eas:transform "treepath" :links [(:source 35 :target 4) (:source 4 :target 5)
                                                         (:source 200 :target 60) (:source 1 :target 999)])))))
    (should (equal (mapcar (lambda (r) (list (plist-get r :link) (plist-get r :id))) paths)
                   '((0 35) (0 16) (0 1) (0 2) (0 3) (0 4) (1 4) (1 3) (1 5)
                     (2 200) (2 188) (2 169) (2 1) (2 58) (2 60))))
    (should (equal (plist-get (aref paths 0) :source) 35))))

(ert-deftest eas-vega-hierarchy-formula-and-subtree ()
  (let ((rows (eas-vega-hierarchy-run '((:x-eas:transform "formula" :expr "datum.id * k" :as "twice" :params (:k 2))))))
    (should (= (plist-get (aref rows 4) :twice) 10)))
  (let ((sub (eas-vega-hierarchy-run '((:x-eas:transform "subtree" :root 2)))))
    ;; analytics and its 13 descendants, analytics now the root at level 1
    (should (= (length sub) 14))
    (should (eq (plist-get (aref sub 0) :parent) :null))
    (should (equal (mapcar (lambda (r) (plist-get r :level)) (seq-take sub 3)) '(1 2 3))))
  (eas-test-should-code "NOT_FOUND" (eas-vega-hierarchy-run '((:x-eas:transform "subtree" :root 9999)))))

;;; Layouts, against Vega's own numbers for flare

(ert-deftest eas-vega-hierarchy-tree-matches-vega ()
  (let ((tidy (eas-vega-hierarchy-run '((:x-eas:transform "tree" :size [1600 500] :as ["y" "x" "depth" "children"]))))
        (cluster (eas-vega-hierarchy-run '((:x-eas:transform "tree" :method "cluster" :separation :false
                                                              :size [1600 500] :as ["y" "x" "depth" "children"])))))
    (eas-vega-hierarchy-should-near (aref tidy 200) '(:x 375 :y 1223.013698630137))
    (eas-vega-hierarchy-should-near (aref tidy 57) '(:x 125 :y 416.43835616438355))
    (eas-vega-hierarchy-should-near (aref cluster 200) '(:x 500 :y 1283.6363636363635))
    (eas-vega-hierarchy-should-near (aref cluster 1) '(:x 250 :y 43.63636363636363)))
  (eas-test-should-code "INVALID_INPUT" (eas-vega-hierarchy-run '((:x-eas:transform "tree" :method "radial")))))

(ert-deftest eas-vega-hierarchy-treemap-and-partition-match-vega ()
  (let ((sq (eas-vega-hierarchy-run '((:x-eas:transform "treemap" :field "size" :sort (:field "value") :round t
                                                        :ratio 1.6 :size [960 500]))))
        (bin (eas-vega-hierarchy-run '((:x-eas:transform "treemap" :field "size" :sort (:field "value") :round t
                                                         :method "binary" :size [960 500]))))
        (pad (eas-vega-hierarchy-run '((:x-eas:transform "treemap" :field "size" :paddingInner 3 :paddingOuter 5
                                                         :paddingTop 12 :size [960 500]))))
        (part (eas-vega-hierarchy-run '((:x-eas:transform "partition" :field "size" :sort (:field "value")
                                                          :size [6.283185307179586 300]
                                                          :as ["a0" "r0" "a1" "r1" "depth" "children"])))))
    (should (equal (seq-take (seq-drop (aref sq 200) 8) 8) '(:x0 657 :y0 221 :x1 724 :y1 295)))
    (should (equal (seq-take (seq-drop (aref sq 1) 6) 8) '(:x0 89 :y0 92 :x1 259 :y1 236)))
    (should (equal (seq-take (seq-drop (aref bin 200) 8) 8) '(:x0 819 :y0 47 :x1 878 :y1 132)))
    (eas-vega-hierarchy-should-near (aref pad 60) '(:x0 212.6127583202685 :y0 128.98302575223582
                                                    :x1 219.97831610893485 :y1 137.70094771796303))
    (eas-vega-hierarchy-should-near (aref part 200) '(:a0 4.4744832303452124 :r0 180
                                                      :a1 4.5397380548514175 :r1 240)))
  (eas-test-should-code "INVALID_INPUT" (eas-vega-hierarchy-run '((:x-eas:transform "treemap" :method "voronoi")))))

(ert-deftest eas-vega-hierarchy-pack-matches-vega ()
  (let ((pack (eas-vega-hierarchy-run '((:x-eas:transform "pack" :field "size" :sort (:field "value") :size [600 600]))))
        (padded (eas-vega-hierarchy-run '((:x-eas:transform "pack" :field "size" :padding 3 :size [500 700])))))
    (eas-vega-hierarchy-should-near (aref pack 0) '(:x 300 :y 300 :r 300))
    (eas-vega-hierarchy-should-near (aref pack 1) '(:x 404.1720474571923 :y 341.2020956499475 :r 43.14218897428232))
    (eas-vega-hierarchy-should-near (aref pack 200) '(:x 382.55227267254446 :y 113.02274462534655 :r 10.880836230339323))
    (eas-vega-hierarchy-should-near (aref padded 100) '(:x 282.0641191951901 :y 527.1432512756219 :r 1.1827928341335332))))

;;; linkpath

(ert-deftest eas-vega-hierarchy-linkpath-paths-match-vega ()
  (should (equal (eas-linkpath-path "line" "vertical" 10 20 110.5 70.25) "M10,20L110.5,70.25"))
  (should (equal (eas-linkpath-path "diagonal" "vertical" 10 20 110.5 70.25) "M10,20C10,45.125 110.5,45.125 110.5,70.25"))
  (should (equal (eas-linkpath-path "diagonal" "horizontal" 10 20 110.5 70.25) "M10,20C60.25,20 60.25,70.25 110.5,70.25"))
  (should (equal (eas-linkpath-path "orthogonal" "vertical" 10 20 110.5 70.25) "M10,20H110.5V70.25"))
  (should (equal (eas-linkpath-path "orthogonal" "horizontal" 10 20 110.5 70.25) "M10,20V70.25H110.5"))
  (should (string-prefix-p "M-16.78143058152904" (eas-linkpath-path "orthogonal" "radial" 10 20 110.5 70.25)))
  (should (string-match-p "A20,20 0 0,0 -17.1102434992049" (eas-linkpath-path "orthogonal" "radial" 10 20 110.5 70.25)))
  (eas-test-should-code "INVALID_INPUT" (eas-linkpath-path "spiral" "vertical" 0 0 1 1))
  (eas-test-should-code "INVALID_INPUT" (eas-linkpath-path "line" "diagonal" 0 0 1 1)))

(ert-deftest eas-vega-hierarchy-linkpath-points-follow-the-path ()
  (dolist (shape eas-linkpath-shapes)
    (dolist (orient '("vertical" "horizontal"))
      (let ((pts (eas-linkpath-points shape orient 10 20 110.5 70.25)))
        (should (equal (car pts) '(10 20)))
        (should (< (abs (- (car (car (last pts))) 110.5)) 1e-9))
        (should (< (abs (- (cadr (car (last pts))) 70.25)) 1e-9)))))
  ;; A diagonal's midpoint is the segment's; an arc's lies on its circle.
  (let ((pts (eas-linkpath-points "diagonal" "horizontal" 0 0 100 50)))
    (should (equal (nth 6 pts) '(50.0 25.0))))
  (dolist (p (eas-linkpath-points "arc" "vertical" 0 0 100 0))
    (should (< (abs (- (sqrt (+ (expt (- (car p) 50) 2) (expt (cadr p) 2))) 50)) 1e-9)))
  ;; Radial: angle and radius about the origin, then the origin added.
  (let ((rows (eas-vega-hierarchy-run '((:x-eas:transform "linkpath" :orient "radial" :origin [100 100]))
                                      [(:source (:x 0 :y 10) :target (:x 1.5707963267948966 :y 20) :w 3)])))
    (should (= (length rows) 2))
    (eas-vega-hierarchy-should-near (aref rows 0) '(:x 110 :y 100 :step 0 :link 0 :w 3))
    (eas-vega-hierarchy-should-near (aref rows 1) '(:x 100 :y 120 :step 1))
    (should-not (plist-member (aref rows 0) :source))))

;;; Templates

(ert-deftest eas-vega-hierarchy-templates-render-natively ()
  "Every template's example resolves, checks native and draws in both backends."
  (dolist (name eas-vega-hierarchy-examples)
    (let* ((spec (eas-vega-hierarchy-spec name)))
      (should (equal (list name nil) (list name (eas-spec-unsupported spec))))
      (should (> (length (eas-svg-render (eas-compile spec))) 1000))
      (should (string-match-p "[^ \n]" (eas-text-render (eas-compile spec :target 'text :size '(:cols 80 :rows 30))))))
    (let ((meta (eas-vega-hierarchy-meta name)))
      (should (member (plist-get meta :status) '("pass" "partial" "unsupported")))
      (should (stringp (plist-get meta :note))))))

(ert-deftest eas-vega-hierarchy-templates-take-caller-data ()
  "Slots rename the fields: a caller's own tree draws under its own names."
  (let* ((rows [(:node "a" :up :null :label "root") (:node "b" :up "a" :label "left" :bytes 2)
                (:node "c" :up "a" :label "right" :bytes 6)])
         (b (list :data rows :key "node" :parent "up" :label "label" :size "bytes")))
    (dolist (name '("treemap" "circle-packing" "sunburst" "zoomable-circle-packing"))
      (should (eas-compile (eas-vega-hierarchy-spec name b))))
    (dolist (name '("tree-layout" "radial-tree-layout"))
      (should (eas-compile (eas-vega-hierarchy-spec name (list :data rows :key "node" :parent "up" :label "label")))))
    (should (eas-compile (eas-vega-hierarchy-spec
                          "edge-bundling" (list :data rows :key "node" :parent "up" :label "label"
                                                :links [(:from "b" :to "c")] :source "from" :target "to"))))
    (eas-test-should-code "FIELD_MISSING"
      (eas-vega-hierarchy-spec "treemap" (list :data rows :key "nope")))))

(defun eas-vega-hierarchy-open (name)
  "A live view of template NAME with its example bindings."
  (eas-view-open (eas-vega-hierarchy-template name) :bindings (eas-vega-hierarchy-example name)))

(defun eas-vega-hierarchy-items (view mark)
  "Items of MARK (its id) in VIEW's scene."
  (cl-loop for v across (plist-get (eas-view-scene view) :views)
           thereis (cl-loop for m across (plist-get v :marks)
                            when (equal (plist-get m :id) mark) return (plist-get m :items))))

(ert-deftest eas-vega-hierarchy-hover-highlights ()
  "Hover marks a treemap leaf red and outlines a packed circle, headless."
  (let ((v (eas-vega-hierarchy-open "treemap")))
    (unwind-protect
        (let ((hover (plist-get (eas-dispatch v '(:type "pointermove" :px [50 480])) :hover)))
          (should (equal (plist-get (plist-get hover :row) :name) "GraphMLConverter"))
          (should (= (seq-count (lambda (it) (equal (plist-get it :fill) "red")) (eas-vega-hierarchy-items v "main/1")) 1))
          (eas-dispatch v '(:type "pointerleave"))
          (should (= (seq-count (lambda (it) (equal (plist-get it :fill) "red")) (eas-vega-hierarchy-items v "main/1")) 0)))
      (eas-view-close v)))
  (let ((v (eas-vega-hierarchy-open "circle-packing")))
    (unwind-protect
        ;; Far from any small circle's centre, inside the large "vis" circle.
        (let ((hover (plist-get (eas-dispatch v '(:type "pointermove" :px [290 30])) :hover)))
          (should (equal (plist-get (plist-get hover :row) :name) "vis")))
      (eas-view-close v))))

(ert-deftest eas-vega-hierarchy-edge-bundling-hover-shows-a-nodes-links ()
  (let ((v (eas-vega-hierarchy-open "edge-bundling")))
    (unwind-protect
        (progn
          (should (= (length (eas-vega-hierarchy-items v "edges_target")) 0))
          ;; The label of AgglomerativeCluster (id 4), imported by six classes.
          (let ((hover (plist-get (eas-dispatch v '(:type "pointermove" :px [371 82])) :hover)))
            (should (equal (plist-get (plist-get hover :row) :id) 4)))
          (should (= (length (eas-vega-hierarchy-items v "edges_target")) 6))
          (should (= (length (eas-vega-hierarchy-items v "edges_source")) 0))
          (should (equal (plist-get (aref (eas-vega-hierarchy-items v "edges_target") 0) :stroke) "firebrick")))
      (eas-view-close v))))

(ert-deftest eas-vega-hierarchy-zoomable-circle-packing-zooms ()
  "A click lays out the clicked circle's subtree; the focused root zooms out."
  (let* ((v (eas-vega-hierarchy-open "zoomable-circle-packing")) zoomed out)
    (unwind-protect
        (let* ((click (plist-get (eas-dispatch v '(:type "click" :px [290 120])) :click)))
          (should (equal (plist-get (plist-get click :row) :name) "vis"))
          (should (equal (plist-get click :action) "zoom-subtree"))
          (setq zoomed (eas-view-get (plist-get click :result)))
          (let ((items (eas-vega-hierarchy-items zoomed "circles")))
            (should (= (length items) 84))
            ;; vis fills the view, colored as at depth 1 in the whole tree.
            (should (= (plist-get (aref items 0) :size) 360000.0))
            (should (equal (plist-get (aref items 0) :fill)
                           (plist-get (aref (eas-vega-hierarchy-items v "circles") 168) :fill))))
          (setq out (eas-view-get (plist-get (plist-get (eas-dispatch zoomed '(:type "click" :px [306 20])) :click) :result)))
          (should (= (length (eas-vega-hierarchy-items out "circles")) 252)))
      (dolist (view (list v zoomed out)) (when view (eas-view-close view))))))

;;; Against the Vega gallery

(ert-deftest eas-vega-hierarchy-gallery-matches-vega ()
  "Each example renders natively within its template's recorded ratio of Vega's."
  :tags '(:gallery)
  (dolist (name eas-vega-hierarchy-examples)
    (let ((cmp (eas-vega-hierarchy-compare name))
          (bound (plist-get (eas-vega-hierarchy-meta name) :ratio)))
      (should (equal (list name t) (list name (<= (plist-get cmp :ratio) bound)))))))

(provide 'eas-vega-hierarchy-test)
;;; eas-vega-hierarchy-test.el ends here

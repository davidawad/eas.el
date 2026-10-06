;;; eas-vega-network-test.el --- force, graph and voronoi transforms; network templates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Bead eas-7r1.5: the Vega gallery's force simulation, Voronoi and
;; network examples as eas templates (templates/vega/).  The force
;; transform is held to numbers d3-force 3 itself computed (node,
;; d3-force 3.0.0, the same parameters); each template resolves with
;; its example binding; a bounded slice of that binding compiles
;; natively, passes the text checker and drives the interactions; and,
;; under :gallery (make test-gallery-conformance) where rsvg-convert is
;; installed, the full example matches its Vega reference PNG as
;; closely as its x-eas.vega block records.

;;; Code:

(require 'ert)
(require 'eas)
(require 'eas-png)
(require 'eas-text-check)
(require 'eas-test-support)

(defconst eas-vega-network-names
  '("force-directed-layout" "arc-diagram" "reorderable-matrix" "airport-connections"
    "beeswarm-plot" "packed-bubble-chart" "dorling-cartogram")
  "The Vega gallery examples bead eas-7r1.5 covers.")

(defmacro eas-vega-network-with-templates (&rest body)
  "Run BODY with templates/vega registered and no live view leaked."
  (declare (indent 0))
  `(let ((eas-template-directories (append eas-template-directories
                                           (list (eas-test-file "templates/vega"))))
         (eas--templates nil)
         (eas-views (make-hash-table :test 'equal)))
     ,@body))

(defvar eas-vega-network--specs nil
  "Alist of (NAME . RESOLVED-SPEC): resolving runs simulations, so once.")

(defun eas-vega-network-spec (name)
  "The resolved spec of template vega/NAME with its example binding."
  (or (alist-get name eas-vega-network--specs nil nil #'equal)
      (eas-vega-network-with-templates
        (setf (alist-get name eas-vega-network--specs nil nil #'equal)
              (eas-resolve (concat "vega/" name) (eas-template-example (concat "vega/" name)))))))

(defun eas-vega-network-meta (name)
  "The x-eas.vega block of template vega/NAME."
  (eas-vega-network-with-templates
    (plist-get (plist-get (eas-template-get (concat "vega/" name)) :meta) :vega)))

(defun eas-vega-network-xy (rows &optional n)
  "The (X Y) of the first N (all) of ROWS."
  (mapcar (lambda (r) (list (plist-get r :x) (plist-get r :y)))
          (if n (seq-take rows n) (append rows nil))))

(defun eas-vega-network-close (actual expected &optional tolerance)
  "Assert number lists ACTUAL and EXPECTED agree within TOLERANCE (1e-6)."
  (should (= (length actual) (length expected)))
  (cl-mapc (lambda (a e)
             (if (consp a) (eas-vega-network-close a e tolerance)
               (should (< (abs (- a e)) (or tolerance 1e-6)))))
           actual expected))

(defun eas-vega-network-miserables (&optional n)
  "The Les Misérables graph: (NODES . LINKS).
With N, only its first N nodes and the links among them."
  (let* ((data (eas-json-read-file (eas-test-file "test/vega-examples/data/miserables.json")))
         (nodes (plist-get data :nodes)) (links (plist-get data :links)))
    (if (null n)
        (cons nodes links)
      (cons (vconcat (seq-take nodes n)) (eas-vega-network-links-within links n)))))

(defun eas-vega-network-links-within (links n)
  "The LINKS whose source and target are both among the first N nodes."
  (vconcat (seq-filter (lambda (l) (and (< (plist-get l :source) n) (< (plist-get l :target) n)))
                       links)))

;; The examples are the whole gallery data: 77 nodes of a 5929-cell
;; matrix, 305 airports with 5366 routes.  Compiling them interpreted
;; takes seconds each, and every drag compiles the view again, so the
;; fast suite renders and drives bounded slices of them; the full
;; examples resolve once and meet their reference PNGs under :gallery.

(defconst eas-vega-network-bound 20
  "Most nodes, airports or rows a bounded example binding keeps.")

(defun eas-vega-network-bounded (name)
  "Template vega/NAME's example binding cut to `eas-vega-network-bound' rows.
Links and routes keep only those among the kept nodes and airports;
a simulation runs 60 ticks; the matrix shrinks to fit its nodes."
  (let* ((n eas-vega-network-bound)
         (b (copy-sequence (eas-template-example (concat "vega/" name))))
         (airports (and (plist-get b :airports) (seq-take (plist-get b :airports) n)))
         (codes (mapcar (lambda (a) (plist-get a :iata)) airports)))
    (dolist (slot '(:nodes :data))
      (when (plist-get b slot) (setq b (plist-put b slot (vconcat (seq-take (plist-get b slot) n))))))
    (when (plist-get b :links)
      (setq b (plist-put b :links (eas-vega-network-links-within (plist-get b :links) n))))
    (when airports
      (setq b (plist-put b :airports (vconcat airports)))
      (setq b (plist-put b :flights
                         (vconcat (seq-filter (lambda (f) (and (member (plist-get f :origin) codes)
                                                               (member (plist-get f :destination) codes)))
                                              (plist-get b :flights))))))
    (when (plist-get (plist-get (plist-get (eas-template-get (concat "vega/" name)) :meta) :slots) :iterations)
      (setq b (plist-put b :iterations (min 60 (or (plist-get b :iterations) 60)))))
    (when (equal name "reorderable-matrix")
      (setq b (plist-put b :size (* 10 (length (plist-get b :nodes))))))
    b))

;;; The force transform against d3-force

(ert-deftest eas-vega-network-force-matches-d3 ()
  "Vega's force-directed example lays out as d3-force does, digit for digit."
  (let* ((graph (eas-vega-network-miserables))
         (forces (vector '(:force "center" :x 350 :y 250) '(:force "collide" :radius 8)
                         '(:force "nbody" :strength -30)
                         (list :force "link" :links (cdr graph) :distance 30))))
    ;; d3: forceSimulation(nodes) with these forces, stop(), then tick() 1 and 300 times.
    (eas-vega-network-close
     (eas-vega-network-xy (eas-force-transform (car graph) (list :iterations 1 :forces forces)) 3)
     '((356.0017786575964 250.66587785297077) (335.44809803098076 261.2372260707937)
       (351.9421244416572 237.38516792848176)))
    (eas-vega-network-close
     (eas-vega-network-xy (eas-force-transform (car graph) (list :iterations 300 :forces forces)) 5)
     '((354.1054177243498 394.38822228561787) (348.83399493996194 434.4868083222836)
       (322.0120818284056 354.7542202740779) (341.2720411207187 342.2365140268173)
       (389.45825680430255 388.3028719300283)))))

(ert-deftest eas-vega-network-force-collide-and-position-match-d3 ()
  "Collision (two passes, expression radius), center, x and y forces as d3's."
  (let* ((spec (eas-json-read-file (eas-test-file "test/vega-examples/specs/packed-bubble-chart.vg.json")))
         (rows (plist-get (aref (plist-get spec :data) 0) :values))
         (top (apply #'max (mapcar (lambda (r) (plist-get r :amount)) rows)))
         (rows (vconcat (mapcar (lambda (r) (eas-plist-put r :size (/ (* 10000 (plist-get r :amount)) top))) rows)))
         (forces (vector '(:force "collide" :iterations 2 :radius (:expr "sqrt(datum.size) / 2"))
                         '(:force "center" :x 400 :y 250)
                         '(:force "x" :x 400 :strength 0.2) '(:force "y" :y 250 :strength 0.1))))
    (eas-vega-network-close
     (eas-vega-network-xy (eas-force-transform rows (list :iterations 1 :forces forces)) 3)
     '((475.223755864529 264.6538392990697) (378.2007790064543 325.1340573770892)
       (438.35216151940796 249.19962898036584)))
    (eas-vega-network-close
     (eas-vega-network-xy (eas-force-transform rows (list :iterations 300 :forces forces)) 3)
     '((496.75012053191733 359.6669168221289) (332.4179435938105 384.98793538516304)
       (505.3850976108602 265.25719378076656)))))

(ert-deftest eas-vega-network-force-jiggle-is-d3s-generator ()
  "Coincident nodes separate by d3's seeded jiggle, so exactly as d3; the seed varies it."
  (let ((rows [(:x 5 :y 5) (:x 5 :y 5) (:x 5 :y 5) (:x 5 :y 5)])
        (forces [(:force "nbody") (:force "collide" :radius 3)]))
    (eas-vega-network-close
     (eas-vega-network-xy (eas-force-transform rows (list :iterations 20 :forces forces)))
     '((120.7310278622672 85.16780603628646) (1.8270728319253018 -135.07257626398362)
       (143.96181823270754 27.202004198593528) (-132.83359850778095 -20.170989304242035)))
    (should (equal (eas-force-transform rows (list :iterations 20 :forces forces))
                   (eas-force-transform rows (list :iterations 20 :forces forces))))
    (should-not (equal (eas-force-transform rows (list :iterations 20 :forces forces :seed 7))
                       (eas-force-transform rows (list :iterations 20 :forces forces))))))

(ert-deftest eas-vega-network-force-pins-starts-and-ticks ()
  "fx/fy pin a node; x/y are where a node starts; a simulation ticks on demand."
  (let* ((graph (eas-vega-network-miserables 30))
         (nodes (vconcat (seq-map-indexed (lambda (r i) (if (= i 0) (append r '(:fx 10 :fy 20)) r)) (car graph))))
         (params (list :iterations 300 :forces (vector '(:force "nbody")
                                                       (list :force "link" :links (cdr graph)))))
         (out (eas-force-transform nodes params)))
    (should (equal (eas-vega-network-xy out 1) '((10.0 20.0))))
    ;; Started where it ended, a cooled layout hardly moves.
    (let* ((again (eas-force-transform out (list :iterations 50 :alpha 0.01 :forces (plist-get params :forces))))
           (moved (cl-loop for a across out for b across again
                           maximize (abs (- (plist-get a :x) (plist-get b :x))))))
      (should (< moved 3)))
    ;; The simulation as a value: ticks come one at a time and cool alpha.
    (let ((sim (eas-force-simulation (car graph) params)))
      (should-not (eas-force-done-p sim))
      (eas-force-tick sim 50)
      (should (equal (eas-force-rows sim (car graph) params)
                     (eas-force-transform (car graph) (plist-put (copy-sequence params) :iterations 50))))
      (eas-force-tick sim 300)
      (should (eas-force-done-p sim)))))

(ert-deftest eas-vega-network-force-links-and-band ()
  "Link rows follow the nodes with their endpoints; ids, bands and bad input."
  (let* ((nodes [(:id "a" :g 2) (:id "b" :g 1) (:id "c" :g 2)])
         (links [(:source "a" :target "b" :w 1) (:source "b" :target "c" :w 2)])
         (out (eas-force-transform nodes (list :iterations 30 :output "both"
                                               :forces (vector (list :force "link" :links links :id "id"))))))
    (should (= (length out) 5))
    (should (equal (mapcar (lambda (r) (plist-get r :eas_kind)) out) '("node" "node" "node" "link" "link")))
    (let ((a (aref out 0)) (b (aref out 1)) (ab (aref out 3)))
      (should (equal (list (plist-get ab :x) (plist-get ab :y) (plist-get ab :x2) (plist-get ab :y2) (plist-get ab :w))
                     (list (plist-get a :x) (plist-get a :y) (plist-get b :x) (plist-get b :y) 1)))))
  ;; A band target puts each row at its category's band center: g 1 -> 25, g 2 -> 75.
  (let ((out (eas-force-transform [(:g 2) (:g 1) (:g 2)]
                                  (list :iterations 300 :forces [(:force "x" :x (:band "g" :range [0 100]) :strength 1)]))))
    (eas-vega-network-close (mapcar (lambda (r) (plist-get r :x)) out) '(75 25 75) 1e-3))
  (eas-test-should-code "INVALID_INPUT"
    (eas-force-transform [(:a 1)] (list :forces [(:force "spring")])))
  (eas-test-should-code "INVALID_INPUT"
    (eas-force-transform [(:a 1)] (list :forces [(:force "link" :links [(:source 0 :target 3)])])))
  (eas-test-should-code "INVALID_INPUT"
    (eas-resolve-spec '(:data (:values [(:a 1)]) :transform [(:x-eas:transform "force")] :mark "point"))))

(ert-deftest eas-vega-network-force-survives-infinite-positions ()
  "A node at infinity stays out of the quadtree instead of growing it forever."
  (let ((out (eas-force-transform [(:x 1.0e+INF :y 0) (:x 1 :y 2) (:x 3 :y 4)]
                                  (list :iterations 5 :forces [(:force "nbody") (:force "collide" :radius 2)]))))
    (should (= (length out) 3))
    ;; The finite nodes still repel each other, and only each other.
    (dolist (i '(1 2))
      (should (< (abs (plist-get (aref out i) :x)) 100))
      (should (< (abs (plist-get (aref out i) :y)) 100)))))

;;; The graph transform

(ert-deftest eas-vega-network-graph-joins-nodes-and-links ()
  "Order (stable by sort), degree and count; prefixed link rows; node pairs."
  (let* ((nodes [(:name "p" :g 2) (:name "q" :g 1) (:name "r" :g 2) (:name "s" :g 1)])
         (links [(:source 0 :target 1 :value 3) (:source 2 :target 0 :value 1) (:source 0 :target 0 :value 1)])
         (out (eas-force-graph-transform nodes (list :links links :sort "g" :output "both" :cross t))))
    (should (= (length out) (+ 4 3 16)))
    (should (equal (mapcar (lambda (r) (list (plist-get r :name) (plist-get r :order) (plist-get r :degree) (plist-get r :count)))
                           (seq-take out 4))
                   '(("p" 3 4 4) ("q" 1 1 4) ("r" 4 1 4) ("s" 2 0 4))))
    (let ((link (aref out 4)))
      (should (equal (list (plist-get link :eas_kind) (plist-get link :value) (plist-get link :source_name)
                           (plist-get link :target_name) (plist-get link :source_order) (plist-get link :target_g))
                     '("link" 3 "p" "q" 3 1))))
    (let ((cell (aref out 7)))
      (should (equal (list (plist-get cell :eas_kind) (plist-get cell :source_name) (plist-get cell :target_name))
                     '("cell" "p" "p"))))
    ;; Without sort the order is the data order; plain output is the nodes alone.
    (should (equal (mapcar (lambda (r) (plist-get r :order)) (eas-force-graph-transform nodes (list :links links)))
                   '(1 2 3 4)))))

;;; The voronoi transform

(defun eas-vega-network-area (cell)
  "Shoelace area of CELL, a closed vector of [x y] vertices."
  (let ((sum 0.0))
    (dotimes (i (1- (length cell)))
      (let ((p (aref cell i)) (q (aref cell (1+ i))))
        (setq sum (+ sum (- (* (aref p 0) (aref q 1)) (* (aref q 0) (aref p 1)))))))
    (/ (abs sum) 2)))

(ert-deftest eas-vega-network-voronoi-cells ()
  "Cells tile the extent; repeats and pointless rows get none; keys share one."
  (let ((two (eas-voronoi-transform [(:x 0 :y 0) (:x 10 :y 0) (:x 0 :y 0) (:a 1)]
                                    (list :x "x" :y "y" :extent [[-10 -10] [20 10]] :as "path"))))
    (should (equal (mapcar (lambda (r) (plist-get r :path)) two)
                   '("M-10,-10L5,-10L5,10L-10,10Z" "M5,-10L20,-10L20,10L5,10Z" :null :null))))
  (let* ((rows (vconcat (cl-loop for i below 40
                                 collect (list :x (mod (* i 37) 101) :y (mod (* i 53) 97)))))
         (out (eas-voronoi-transform rows (list :x "x" :y "y" :extent [[0 0] [101 97]] :polygon "cell")))
         (total (cl-loop for r across out sum (eas-vega-network-area (plist-get r :cell)))))
    (should (< (abs (- total (* 101 97))) 1e-6))
    ;; Each site lies in its own cell: nearer it than any other site's.
    (cl-loop for r across out
             do (cl-loop for v across (plist-get r :cell)
                         do (let ((d (lambda (s) (+ (expt (- (aref v 0) (plist-get s :x)) 2)
                                                   (expt (- (aref v 1) (plist-get s :y)) 2)))))
                              (should (<= (funcall d r) (+ 1e-6 (cl-loop for s across out minimize (funcall d s)))))))))
  (let ((keyed (eas-voronoi-transform [(:k "a" :x 0 :y 0) (:k "b" :x 10 :y 0) (:k "a" :x 99 :y 99)]
                                      (list :x "x" :y "y" :key "k" :extent [[-10 -10] [20 10]]))))
    (should (equal (plist-get (aref keyed 0) :path) (plist-get (aref keyed 2) :path)))))

;;; The templates

(ert-deftest eas-vega-network-templates-render-natively ()
  "Each template resolves with its example; a bounded slice compiles and passes the text check."
  (eas-vega-network-with-templates
    (dolist (name eas-vega-network-names)
      (let* ((spec (eas-vega-network-spec name))
             (meta (eas-vega-network-meta name)))
        (should (member (plist-get meta :status) '("pass" "partial" "unsupported")))
        (should (stringp (plist-get meta :note)))
        (should (equal (cons name (eas-spec-check spec)) (list name)))
        (let* ((small (eas-resolve (concat "vega/" name) (eas-vega-network-bounded name)))
               (scene (eas-compile small :target 'text :size '(:cols 100 :rows 30))))
          (should (equal (cons name (eas-spec-check small)) (list name)))
          (should (equal (cons name (eas-text-check scene 100)) (list name))))))))

(defun eas-vega-network-ratio (name offset)
  "Differing-pixel ratio of NAME's native PNG against its Vega reference.
The native image is shifted by OFFSET, [DX DY]."
  (let* ((png (make-temp-file "eas-vega-network" nil ".png")))
    (unwind-protect
        (progn
          (eas-chart-rasterize (eas-svg-render (eas-compile (eas-vega-network-spec name))) png)
          (let* ((native (eas-png-read png))
                 (ref (eas-png-read (eas-test-file "test/vega-examples/ref" (concat name ".png"))))
                 (count (eas-png--count ref native (aref offset 0) (aref offset 1)
                                        (eas-png--background ref)
                                        (* 35215 eas-png-pixel-threshold eas-png-pixel-threshold))))
            (/ (float (car count)) (cdr count))))
      (delete-file png))))

(ert-deftest eas-vega-network-templates-match-references ()
  "Each template's native PNG is as close to Vega's as its x-eas.vega block says.
It renders the full examples, so it runs with the conformance gallery."
  :tags '(:gallery)
  (unless (executable-find eas-chart-rsvg-program)
    (eas-test-skip (format "%s not on PATH; needed to rasterize native SVG for the Vega reference PNGs"
                           eas-chart-rsvg-program)))
  (dolist (name eas-vega-network-names)
    (let* ((meta (eas-vega-network-meta name))
           (ratio (eas-vega-network-ratio name (plist-get meta :offset))))
      ;; Recorded with resvg and Arimo; rsvg's anti-aliasing and fonts differ a little.
      (should (equal (list name (<= ratio (+ (plist-get meta :ratio) 0.02))) (list name t)))
      (garbage-collect))))

(ert-deftest eas-vega-network-drag-pins-a-node ()
  "Dragging a node pins it where it is dropped; replay redraws the same; dblclick frees it."
  (eas-vega-network-with-templates
    (let* ((bindings (eas-vega-network-bounded "force-directed-layout"))
           (view (eas-view-open "vega/force-directed-layout" :bindings bindings))
           (node (lambda (v i) (nth i (seq-filter (lambda (r) (equal (plist-get r :eas_kind) "node"))
                                                  (plist-get (eas-view-data v) :rows)))))
           (myriel (funcall node view 0))
           (before (plist-get (funcall node view 5) :x)))
      (eas-dispatch view (list :type "drag" :from (vector (plist-get myriel :x) (plist-get myriel :y)) :to [100 100]))
      (should (equal (list (plist-get (funcall node view 0) :x) (plist-get (funcall node view 0) :fx)) '(100.0 100.0)))
      (should-not (equal (plist-get (funcall node view 5) :x) before))
      ;; A drag that starts off every node changes nothing.
      (let ((rows (plist-get (eas-view-data view) :rows)))
        (eas-dispatch view '(:type "drag" :from [5 5] :to [600 400]))
        (should (eq rows (plist-get (eas-view-data view) :rows))))
      (let ((fresh (eas-view-open "vega/force-directed-layout" :bindings bindings :id "fresh")))
        (eas-replay fresh (eas-view-log view))
        (should (equal (plist-get (eas-view-data fresh) :rows) (plist-get (eas-view-data view) :rows))))
      (eas-dispatch view '(:type "dblclick" :px [100 100]))
      (should-not (plist-get (funcall node view 0) :fx)))))

(ert-deftest eas-vega-network-matrix-drag-reorders ()
  "Dragging a row label moves that node's row; a column label its column."
  (eas-vega-network-with-templates
    (let* ((view (eas-view-open "vega/reorderable-matrix" :bindings (eas-vega-network-bounded "reorderable-matrix")))
           (order (lambda (name) (plist-get (seq-find (lambda (r) (and (equal (plist-get r :eas_kind) "node")
                                                                       (equal (plist-get r :name) name)))
                                                      (plist-get (eas-view-data view) :rows))
                                            :order)))
           (label (lambda (name column)
                    (seq-some (lambda (m) (seq-find (lambda (it) (and (equal (plist-get it :text) name)
                                                                      (eq column (not (zerop (mod (or (plist-get it :angle) 0) 360))))))
                                                    (plist-get m :items)))
                              (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))))
      ;; The bounded matrix: groups 1, 2 and 3 in data order.
      (should (equal (list (funcall order "Myriel") (funcall order "Valjean")) '(1 12)))
      (let ((row (funcall label "Myriel" nil)) (last (funcall label "Blacheville" nil)))
        (eas-dispatch view (list :type "drag" :from (vector (- (plist-get row :x) 10) (plist-get row :y))
                                 :to (vector (plist-get row :x) (plist-get last :y)))))
      (should (equal (list (funcall order "Myriel") (funcall order "Blacheville") (funcall order "Valjean")) '(19 20 11)))
      (let ((col (funcall label "Valjean" t)) (first (funcall label "Napoleon" t)))
        (eas-dispatch view (list :type "drag" :from (vector (plist-get col :x) (- (plist-get col :y) 10))
                                 ;; The first column's left half: ahead of its node, not tied.
                                 :to (vector (- (plist-get first :x) 4) 0))))
      (should (equal (list (funcall order "Valjean") (funcall order "Napoleon")) '(1 2))))))

(ert-deftest eas-vega-network-airport-hover-draws-routes ()
  "Hovering an airport draws the routes from it; the map starts with none."
  (eas-vega-network-with-templates
    (let* ((bindings (eas-vega-network-bounded "airport-connections"))
           (view (eas-view-open "vega/airport-connections" :bindings bindings))
           (marks (lambda () (plist-get (aref (plist-get (eas-view-scene view) :views) 0) :marks)))
           (rules (lambda () (length (plist-get (seq-find (lambda (m) (equal (plist-get m :mark) "rule")) (funcall marks)) :items))))
           (circles (seq-find (lambda (m) (equal (plist-get m :mark) "circle")) (funcall marks)))
           (atl (seq-find (lambda (it) (equal (plist-get (aref (plist-get circles :rows) (plist-get it :datum)) :origin) "ATL"))
                          (plist-get circles :items))))
      (should (eas-view-interactive view))
      (should (= (funcall rules) 0))
      (eas-dispatch view (list :type "pointermove" :px (vector (plist-get atl :x) (plist-get atl :y))))
      (should (= (funcall rules) (seq-count (lambda (f) (equal (plist-get f :origin) "ATL"))
                                            (plist-get bindings :flights))))
      (should (> (funcall rules) 0))
      (eas-dispatch view '(:type "pointerleave"))
      (should (= (funcall rules) 0)))))

(provide 'eas-vega-network-test)
;;; eas-vega-network-test.el ends here

;;; eas-force.el --- force-directed layout: the x-eas force transform -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  {"x-eas:transform": "force", "forces": [...]} lays rows
;; out as Vega's force transform does: a port of d3-force 3 (the
;; simulation, its center, collide, nbody, link, x and y forces) and of
;; the d3-quadtree they search, operation for operation, so a layout
;; matches Vega's to floating-point noise.  It is deterministic: rows
;; without a position start on d3's phyllotaxis spiral, and the jiggle
;; that separates coincident nodes draws from d3's linear congruential
;; generator, seeded by "seed" (d3's own seed, 1, by default).
;;
;; A static render runs "iterations" ticks (300, as Vega).  The
;; simulation is a value too: `eas-force-simulation' builds one from
;; rows, `eas-force-tick' advances it and `eas-force-rows' reads the
;; positions back, so a live view can push ticks as they come.
;;
;; Rows carry the simulation's state in the "as" fields (x, y, vx, vy):
;; a row that already has numeric x and y starts there, and numeric fx
;; or fy pin it (eas-force-drag.el pins a dragged node this way).  Each
;; force parameter that varies by node (a radius, a strength, an x or y
;; target, a link distance) is a number, a field name, a "datum.a[0]"
;; path or {"expr": E}.  "transform" holds Vega-Lite transforms applied
;; to the rows first: a template's domain transforms precede its native
;; ones, so this is where a force reads computed fields.
;;
;; With "output": "both", every link of the first link force follows
;; the nodes as a row of its own: its fields, the source position in
;; x and y and the target position in x2 and y2 (the "linkAs" fields),
;; so a rule mark draws it on the nodes' scales.  "kindAs" names the
;; field ("eas_kind") that tells "node" rows from "link" rows.

;;; Code:

(require 'eas-core)
(require 'eas-expr)
(require 'eas-transform)
(require 'eas-transform-domain)

;;; d3-quadtree

;; An internal quad is [C0 C1 C2 C3 VALUE X Y R]; a leaf is
;; [leaf INDEX NEXT nil VALUE X Y R], NEXT chaining coincident points.
;; VALUE, X, Y and R are the forces' per-quad aggregates.  A tree is
;; [ROOT X0 Y0 X1 Y1].

(defmacro eas-force--leaf-p (quad)
  "Non-nil when QUAD is a leaf."
  `(eq (aref ,quad 0) 'leaf))

(defun eas-force--cover (tree x y)
  "Extend TREE's extent to cover X Y, as d3's quadtree.cover."
  (let ((x0 (aref tree 1)) (y0 (aref tree 2)) (x1 (aref tree 3)) (y1 (aref tree 4)))
    (if (isnan x0)
        (setq x0 (ffloor x) x1 (+ x0 1.0) y0 (ffloor y) y1 (+ y0 1.0))
      (let ((z (if (= x1 x0) 1.0 (- x1 x0))) (node (aref tree 0)))
        (while (or (> x0 x) (>= x x1) (> y0 y) (>= y y1))
          (let ((i (logior (if (< y y0) 2 0) (if (< x x0) 1 0)))
                (parent (make-vector 8 nil)))
            (aset parent i node)
            (setq node parent z (* z 2))
            (pcase i
              (0 (setq x1 (+ x0 z) y1 (+ y0 z)))
              (1 (setq x0 (- x1 z) y1 (+ y0 z)))
              (2 (setq x1 (+ x0 z) y0 (- y1 z)))
              (_ (setq x0 (- x1 z) y0 (- y1 z))))))
        (when (and (aref tree 0) (not (eas-force--leaf-p (aref tree 0))))
          (aset tree 0 node))))
    (aset tree 1 x0) (aset tree 2 y0) (aset tree 3 x1) (aset tree 4 y1)
    tree))

(defun eas-force--add (tree x y i xs ys)
  "Add point I at X Y to TREE, as d3's quadtree.add.
XS and YS hold every point's coordinates."
  (let ((leaf (vector 'leaf i nil nil 0.0 0.0 0.0 0.0))
        (node (aref tree 0)) (parent nil) (j 0)
        (x0 (aref tree 1)) (y0 (aref tree 2)) (x1 (aref tree 3)) (y1 (aref tree 4)))
    (if (null node)
        (aset tree 0 leaf)
      (catch 'done
        (while (not (eas-force--leaf-p node))
          (let* ((xm (/ (+ x0 x1) 2)) (ym (/ (+ y0 y1) 2))
                 (right (>= x xm)) (bottom (>= y ym)))
            (if right (setq x0 xm) (setq x1 xm))
            (if bottom (setq y0 ym) (setq y1 ym))
            (setq j (logior (if bottom 2 0) (if right 1 0)) parent node node (aref node j))
            (unless node (aset parent j leaf) (throw 'done nil))))
        (let* ((k (aref node 1)) (xp (aref xs k)) (yp (aref ys k)))
          (if (and (= x xp) (= y yp))
              (progn (aset leaf 2 node)
                     (if parent (aset parent j leaf) (aset tree 0 leaf)))
            (let (jj)
              (while (progn
                       (setq parent (if parent (aset parent j (make-vector 8 nil))
                                      (aset tree 0 (make-vector 8 nil))))
                       (let* ((xm (/ (+ x0 x1) 2)) (ym (/ (+ y0 y1) 2))
                              (right (>= x xm)) (bottom (>= y ym)))
                         (if right (setq x0 xm) (setq x1 xm))
                         (if bottom (setq y0 ym) (setq y1 ym))
                         (setq j (logior (if bottom 2 0) (if right 1 0))
                               jj (logior (if (>= yp ym) 2 0) (if (>= xp xm) 1 0))))
                       (= j jj)))
              (aset parent jj node)
              (aset parent j leaf))))))
    tree))

(defun eas-force--tree (xs ys)
  "The quadtree of the points at XS YS, as d3's quadtree.addAll."
  (let ((tree (vector nil 0.0e+NaN 0.0e+NaN 0.0e+NaN 0.0e+NaN))
        (x0 1.0e+INF) (y0 1.0e+INF) (x1 -1.0e+INF) (y1 -1.0e+INF)
        (n (length xs)))
    (dotimes (i n)
      (let ((x (aref xs i)) (y (aref ys i)))
        (unless (or (isnan x) (isnan y))
          (when (< x x0) (setq x0 x))
          (when (> x x1) (setq x1 x))
          (when (< y y0) (setq y0 y))
          (when (> y y1) (setq y1 y)))))
    (when (and (<= x0 x1) (<= y0 y1))
      (eas-force--cover tree x0 y0)
      (eas-force--cover tree x1 y1)
      (dotimes (i n)
        (let ((x (aref xs i)) (y (aref ys i)))
          (unless (or (isnan x) (isnan y))
            (eas-force--add tree x y i xs ys)))))
    tree))

(defun eas-force--visit (tree fn)
  "Call FN on TREE's quads in pre-order, as d3's quadtree.visit.
FN gets QUAD X0 Y0 X1 Y1; non-nil skips the quad's children."
  (let ((stack (and (aref tree 0)
                    (list (list (aref tree 0) (aref tree 1) (aref tree 2) (aref tree 3) (aref tree 4))))))
    (while stack
      (let* ((q (pop stack)) (node (nth 0 q))
             (x0 (nth 1 q)) (y0 (nth 2 q)) (x1 (nth 3 q)) (y1 (nth 4 q)))
        (when (and (not (funcall fn node x0 y0 x1 y1)) (not (eas-force--leaf-p node)))
          (let ((xm (/ (+ x0 x1) 2)) (ym (/ (+ y0 y1) 2)) child)
            (when (setq child (aref node 3)) (push (list child xm ym x1 y1) stack))
            (when (setq child (aref node 2)) (push (list child x0 ym xm y1) stack))
            (when (setq child (aref node 1)) (push (list child xm y0 x1 ym) stack))
            (when (setq child (aref node 0)) (push (list child x0 y0 xm ym) stack))))))))

(defun eas-force--visit-after (quad fn)
  "Call FN on QUAD and its descendants, children first."
  (when quad
    (unless (eas-force--leaf-p quad)
      (dotimes (i 4) (eas-force--visit-after (aref quad i) fn)))
    (funcall fn quad)))

;;; The simulation

(cl-defstruct (eas-force-sim (:constructor eas-force-sim--make) (:copier nil))
  "A d3-force simulation: node state vectors, cooling and forces."
  n x y vx vy fx fy alpha alpha-min alpha-decay alpha-target velocity-decay
  forces seed)

(defun eas-force--random (sim)
  "SIM's next uniform number in [0, 1): d3-force's lcg."
  (let ((s (mod (+ (* 1664525 (eas-force-sim-seed sim)) 1013904223) 4294967296)))
    (setf (eas-force-sim-seed sim) s)
    (/ (float s) 4294967296.0)))

(defun eas-force--jiggle (sim)
  "A tiny random offset from SIM, as d3-force's jiggle."
  (* (- (eas-force--random sim) 0.5) 1e-6))

(defun eas-force--number (v)
  "V as a float, or NaN when it is not a number."
  (if (numberp v) (float v) 0.0e+NaN))

(defun eas-force--accessor (spec)
  "Function ROW -> value for the force parameter SPEC.
SPEC is a number, a field name, a \"datum...\" path or {\"expr\": E}
\(or {\"field\": F})."
  (cond
   ((numberp spec) (lambda (_row) spec))
   ((and (eas-object-p spec) (stringp (plist-get spec :expr)))
    (let ((expr (plist-get spec :expr))) (lambda (row) (eas-expr-evaluate expr row))))
   ((and (eas-object-p spec) (stringp (plist-get spec :field)))
    (eas-force--accessor (plist-get spec :field)))
   ((and (stringp spec) (or (string-prefix-p "datum." spec) (string-prefix-p "datum[" spec)))
    (lambda (row) (eas-expr-evaluate spec row)))
   ((stringp spec)
    (if (string-match-p "[.[]" spec)
        (let ((expr (concat "datum." spec))) (lambda (row) (eas-expr-evaluate expr row)))
      (let ((key (eas-key spec))) (lambda (row) (plist-get row key)))))
   (t (eas-signal "INVALID_INPUT"
                  (format "A force parameter is a number, a field or {\"expr\": ...}, not %S" spec)))))

(defun eas-force--band-less (a b)
  "Ascending order of band values A and B: numbers, then strings."
  (cond ((and (numberp a) (numberp b)) (< a b))
        ((numberp a) t) ((numberp b) nil)
        (t (string< (format "%s" a) (format "%s" b)))))

(defun eas-force--band (spec rows)
  "Function ROW -> the center of ROW's band under band SPEC over ROWS.
SPEC is {\"band\": FIELD, \"range\": [LO HI]}: the sorted distinct values
of FIELD split the range into equal bands, as a Vega band scale with
no padding (the beeswarm's xfocus)."
  (let* ((get (eas-force--accessor (plist-get spec :band)))
         (values (sort (delete-dups (seq-map get rows)) #'eas-force--band-less))
         (range (or (plist-get spec :range) [0 1]))
         (lo (float (aref range 0)))
         (step (/ (- (aref range 1) lo) (max 1 (length values)))))
    (lambda (row)
      (when-let* ((i (seq-position values (funcall get row))))
        (+ lo (* step (+ i 0.5)))))))

(defun eas-force--per-node (spec rows default)
  "Vector of floats: force parameter SPEC (or DEFAULT) for each of ROWS."
  (let* ((spec (if (or (null spec) (eq spec :null)) default spec))
         (f (if (and (eas-object-p spec) (plist-get spec :band))
                (eas-force--band spec rows)
              (eas-force--accessor spec))))
    (vconcat (seq-map (lambda (row) (eas-force--number (funcall f row))) rows))))

(defun eas-force--param (force key default)
  "FORCE's numeric parameter KEY, else DEFAULT.
The parameter may be {\"expr\": E}, evaluated once without a datum."
  (let* ((v (plist-get force key))
         (v (if (and (eas-object-p v) (stringp (plist-get v :expr)))
                (eas-expr-evaluate (plist-get v :expr) nil)
              v)))
    (if (numberp v) (float v) default)))

(defun eas-force--center (sim force _rows)
  "The force function of d3's forceCenter for SIM, configured by FORCE."
  (let ((cx (eas-force--param force :x 0.0)) (cy (eas-force--param force :y 0.0))
        (strength (eas-force--param force :strength 1.0)))
    (lambda (_alpha)
      (let* ((n (eas-force-sim-n sim)) (xs (eas-force-sim-x sim)) (ys (eas-force-sim-y sim))
             (sx 0.0) (sy 0.0))
        (dotimes (i n) (setq sx (+ sx (aref xs i)) sy (+ sy (aref ys i))))
        (setq sx (* (- (/ sx n) cx) strength) sy (* (- (/ sy n) cy) strength))
        (dotimes (i n)
          (aset xs i (- (aref xs i) sx))
          (aset ys i (- (aref ys i) sy)))))))

(defun eas-force--collide (sim force rows)
  "The force function of d3's forceCollide for SIM over ROWS, by FORCE."
  (let ((radii (eas-force--per-node (plist-get force :radius) rows 1))
        (strength (eas-force--param force :strength 1.0))
        (iterations (truncate (eas-force--param force :iterations 1.0))))
    (dotimes (i (length radii)) (when (isnan (aref radii i)) (aset radii i 0.0)))
    (lambda (_alpha)
      (let* ((n (eas-force-sim-n sim)) (xs (eas-force-sim-x sim)) (ys (eas-force-sim-y sim))
             (vxs (eas-force-sim-vx sim)) (vys (eas-force-sim-vy sim))
             (qx (make-vector n 0.0)) (qy (make-vector n 0.0)))
        (dotimes (_ iterations)
          (dotimes (i n)
            (aset qx i (+ (aref xs i) (aref vxs i)))
            (aset qy i (+ (aref ys i) (aref vys i))))
          (let ((tree (eas-force--tree qx qy)))
            (eas-force--visit-after
             (aref tree 0)
             (lambda (quad)
               (if (eas-force--leaf-p quad)
                   (aset quad 7 (aref radii (aref quad 1)))
                 (let ((r 0.0))
                   (dotimes (c 4)
                     (let ((child (aref quad c)))
                       (when (and child (> (aref child 7) r)) (setq r (aref child 7)))))
                   (aset quad 7 r)))))
            (dotimes (i n)
              (let* ((ri (aref radii i)) (ri2 (* ri ri))
                     (xi (+ (aref xs i) (aref vxs i))) (yi (+ (aref ys i) (aref vys i))))
                (eas-force--visit
                 tree
                 (lambda (quad x0 y0 x1 y1)
                   (let* ((rj (aref quad 7)) (r (+ ri rj)))
                     (if (eas-force--leaf-p quad)
                         (let ((d (aref quad 1)))
                           (when (> d i)
                             (let* ((x (- xi (aref xs d) (aref vxs d)))
                                    (y (- yi (aref ys d) (aref vys d)))
                                    (l (+ (* x x) (* y y))))
                               (when (< l (* r r))
                                 (when (= x 0) (setq x (eas-force--jiggle sim) l (+ l (* x x))))
                                 (when (= y 0) (setq y (eas-force--jiggle sim) l (+ l (* y y))))
                                 (setq l (sqrt l))
                                 (setq l (* (/ (- r l) l) strength))
                                 (setq x (* x l) y (* y l))
                                 (let* ((rj2 (* rj rj)) (w (/ rj2 (+ ri2 rj2))))
                                   (aset vxs i (+ (aref vxs i) (* x w)))
                                   (aset vys i (+ (aref vys i) (* y w)))
                                   (setq w (- 1 w))
                                   (aset vxs d (- (aref vxs d) (* x w)))
                                   (aset vys d (- (aref vys d) (* y w)))))))
                           t)
                       (or (> x0 (+ xi r)) (< x1 (- xi r)) (> y0 (+ yi r)) (< y1 (- yi r)))))))))))))))

(defun eas-force--nbody (sim force rows)
  "The force function of d3's forceManyBody for SIM over ROWS, by FORCE."
  (let* ((strengths (eas-force--per-node (plist-get force :strength) rows -30))
         (theta (eas-force--param force :theta 0.9)) (theta2 (* theta theta))
         (dmin (eas-force--param force :distanceMin 1.0)) (dmin2 (* dmin dmin))
         (dmax (eas-force--param force :distanceMax 1.0e+INF)) (dmax2 (* dmax dmax)))
    (dotimes (i (length strengths)) (when (isnan (aref strengths i)) (aset strengths i 0.0)))
    (lambda (alpha)
      (let* ((n (eas-force-sim-n sim)) (xs (eas-force-sim-x sim)) (ys (eas-force-sim-y sim))
             (vxs (eas-force-sim-vx sim)) (vys (eas-force-sim-vy sim))
             (tree (eas-force--tree xs ys)))
        (eas-force--visit-after
         (aref tree 0)
         (lambda (quad)
           (let ((strength 0.0))
             (if (eas-force--leaf-p quad)
                 (let ((q quad))
                   (aset quad 5 (aref xs (aref quad 1)))
                   (aset quad 6 (aref ys (aref quad 1)))
                   (while q
                     (setq strength (+ strength (aref strengths (aref q 1))) q (aref q 2))))
               (let ((weight 0.0) (x 0.0) (y 0.0))
                 (dotimes (c 4)
                   (let ((q (aref quad c)))
                     (when q
                       (let ((v (aref q 4)))
                         (unless (= v 0)
                           (let ((cw (abs v)))
                             (setq strength (+ strength v) weight (+ weight cw)
                                   x (+ x (* cw (aref q 5))) y (+ y (* cw (aref q 6))))))))))
                 (aset quad 5 (/ x weight))
                 (aset quad 6 (/ y weight))))
             (aset quad 4 strength))))
        (dotimes (i n)
          (let ((xi (aref xs i)) (yi (aref ys i)))
            (eas-force--visit
             tree
             (lambda (quad x0 _y0 x1 _y1)
               (let ((value (aref quad 4)))
                 (if (or (= value 0) (isnan value))
                     t
                   (let* ((x (- (aref quad 5) xi)) (y (- (aref quad 6) yi))
                          (w (- x1 x0)) (l (+ (* x x) (* y y))))
                     (cond
                      ((< (/ (* w w) theta2) l)
                       (when (< l dmax2)
                         (when (= x 0) (setq x (eas-force--jiggle sim) l (+ l (* x x))))
                         (when (= y 0) (setq y (eas-force--jiggle sim) l (+ l (* y y))))
                         (when (< l dmin2) (setq l (sqrt (* dmin2 l))))
                         (aset vxs i (+ (aref vxs i) (/ (* (* x value) alpha) l)))
                         (aset vys i (+ (aref vys i) (/ (* (* y value) alpha) l))))
                       t)
                      ((or (not (eas-force--leaf-p quad)) (>= l dmax2)) nil)
                      (t
                       (when (or (/= (aref quad 1) i) (aref quad 2))
                         (when (= x 0) (setq x (eas-force--jiggle sim) l (+ l (* x x))))
                         (when (= y 0) (setq y (eas-force--jiggle sim) l (+ l (* y y))))
                         (when (< l dmin2) (setq l (sqrt (* dmin2 l)))))
                       (let ((q quad))
                         (while q
                           (unless (= (aref q 1) i)
                             (let ((w (/ (* (aref strengths (aref q 1)) alpha) l)))
                               (aset vxs i (+ (aref vxs i) (* x w)))
                               (aset vys i (+ (aref vys i) (* y w)))))
                           (setq q (aref q 2))))
                       nil)))))))))))))

(defun eas-force--link-index (force rows)
  "Function VALUE -> node index for FORCE's link ends among ROWS.
With an \"id\" field, a link end names a node's id; else its index."
  (let ((id (plist-get force :id)))
    (if (stringp id)
        (let ((table (make-hash-table :test 'equal)) (f (eas-force--accessor id)) (i -1))
          (seq-doseq (row rows) (puthash (funcall f row) (setq i (1+ i)) table))
          (lambda (v) (gethash v table)))
      (let ((n (length rows)))
        (lambda (v) (and (integerp v) (< -1 v n) v))))))

(defun eas-force--links (force rows)
  "FORCE's links as a vector of (SOURCE TARGET ROW) over node ROWS.
Signal INVALID_INPUT for a link end that names no node."
  (let ((index (eas-force--link-index force rows)) (i -1))
    (vconcat
     (seq-map (lambda (link)
                (setq i (1+ i))
                (let ((s (funcall index (plist-get link :source)))
                      (tg (funcall index (plist-get link :target))))
                  (unless (and s tg)
                    (eas-signal "INVALID_INPUT"
                                (format "Link %d joins %S and %S, which are not both nodes" i
                                        (plist-get link :source) (plist-get link :target))
                                :index i))
                  (list s tg link)))
              (or (plist-get force :links) [])))))

(defun eas-force--link (sim force rows)
  "The force function of d3's forceLink for SIM over node ROWS, by FORCE."
  (let* ((links (eas-force--links force rows))
         (m (length links)) (n (length rows))
         (count (make-vector n 0))
         (bias (make-vector m 0.0))
         (link-rows (seq-map #'caddr links))
         (strength-spec (plist-get force :strength))
         (distances (eas-force--per-node (plist-get force :distance) link-rows 30))
         (strengths (and strength-spec (eas-force--per-node strength-spec link-rows 1)))
         (iterations (truncate (eas-force--param force :iterations 1.0))))
    (seq-doseq (l links)
      (aset count (nth 0 l) (1+ (aref count (nth 0 l))))
      (aset count (nth 1 l) (1+ (aref count (nth 1 l)))))
    (dotimes (i m)
      (let ((cs (aref count (nth 0 (aref links i)))) (ct (aref count (nth 1 (aref links i)))))
        (aset bias i (/ (float cs) (+ cs ct)))))
    (unless strengths
      (setq strengths (make-vector m 0.0))
      (dotimes (i m)
        (aset strengths i (/ 1.0 (min (aref count (nth 0 (aref links i))) (aref count (nth 1 (aref links i))))))))
    (lambda (alpha)
      (let ((xs (eas-force-sim-x sim)) (ys (eas-force-sim-y sim))
            (vxs (eas-force-sim-vx sim)) (vys (eas-force-sim-vy sim)))
        (dotimes (_ iterations)
          (dotimes (i m)
            (let* ((s (nth 0 (aref links i))) (tg (nth 1 (aref links i)))
                   (x (- (+ (aref xs tg) (aref vxs tg)) (aref xs s) (aref vxs s)))
                   (y (- (+ (aref ys tg) (aref vys tg)) (aref ys s) (aref vys s))))
              (when (or (= x 0) (isnan x)) (setq x (eas-force--jiggle sim)))
              (when (or (= y 0) (isnan y)) (setq y (eas-force--jiggle sim)))
              (let* ((l (sqrt (+ (* x x) (* y y))))
                     (l (* (* (/ (- l (aref distances i)) l) alpha) (aref strengths i)))
                     (b (aref bias i)))
                (setq x (* x l) y (* y l))
                (aset vxs tg (- (aref vxs tg) (* x b)))
                (aset vys tg (- (aref vys tg) (* y b)))
                (setq b (- 1 b))
                (aset vxs s (+ (aref vxs s) (* x b)))
                (aset vys s (+ (aref vys s) (* y b)))))))))))

(defun eas-force--position (sim force rows axis)
  "The force function of d3's forceX or forceY for SIM over ROWS, by FORCE.
AXIS is :x or :y."
  (let* ((targets (eas-force--per-node (plist-get force axis) rows 0))
         (strength (eas-force--per-node (plist-get force :strength) rows 0.1))
         (strengths (vconcat (cl-mapcar (lambda (target s) (if (isnan target) 0.0 s)) targets strength))))
    (lambda (alpha)
      (let ((ps (if (eq axis :x) (eas-force-sim-x sim) (eas-force-sim-y sim)))
            (vs (if (eq axis :x) (eas-force-sim-vx sim) (eas-force-sim-vy sim))))
        (dotimes (i (eas-force-sim-n sim))
          (unless (= (aref strengths i) 0)
            (aset vs i (+ (aref vs i) (* (* (- (aref targets i) (aref ps i)) (aref strengths i)) alpha)))))))))

(defconst eas-force-kinds '("center" "collide" "nbody" "link" "x" "y")
  "The forces a force transform can apply, as Vega names them.")

(defun eas-force--make (sim force rows)
  "The function to apply FORCE (a Vega force object) to SIM over ROWS."
  (pcase (plist-get force :force)
    ("center" (eas-force--center sim force rows))
    ("collide" (eas-force--collide sim force rows))
    ("nbody" (eas-force--nbody sim force rows))
    ("link" (eas-force--link sim force rows))
    ("x" (eas-force--position sim force rows :x))
    ("y" (eas-force--position sim force rows :y))
    (other (eas-signal "INVALID_INPUT"
                       (format "Unknown force %S; forces: %s" other (string-join eas-force-kinds ", "))
                       :force other))))

(defun eas-force--as (params)
  "The four output keys (X Y VX VY) of force PARAMS."
  (mapcar #'eas-key (append (or (plist-get params :as) ["x" "y" "vx" "vy"]) nil)))

(defun eas-force-simulation (rows params)
  "A d3-force simulation of ROWS under force transform PARAMS.
Nodes start where their x and y fields say, else on d3's phyllotaxis;
numeric fx and fy pin them.  The forces are initialized; no tick has
run."
  (let* ((n (length rows))
         (as (eas-force--as params))
         (sim (eas-force-sim--make
               :n n :x (make-vector n 0.0) :y (make-vector n 0.0)
               :vx (make-vector n 0.0) :vy (make-vector n 0.0)
               :fx (make-vector n nil) :fy (make-vector n nil)
               :alpha (eas-force--param params :alpha 1.0)
               :alpha-min (eas-force--param params :alphaMin 0.001)
               :alpha-decay (- 1 (expt 0.001 (/ 1.0 300)))
               :alpha-target (eas-force--param params :alphaTarget 0.0)
               :velocity-decay (- 1 (eas-force--param params :velocityDecay 0.4))
               :seed (let ((s (plist-get params :seed))) (if (integerp s) s 1))))
         (initial-angle (* float-pi (- 3 (sqrt 5)))))
    (seq-do-indexed
     (lambda (row i)
       (let ((x (eas-force--number (plist-get row (nth 0 as))))
             (y (eas-force--number (plist-get row (nth 1 as))))
             (vx (eas-force--number (plist-get row (nth 2 as))))
             (vy (eas-force--number (plist-get row (nth 3 as))))
             (fx (plist-get row :fx)) (fy (plist-get row :fy)))
         (when (numberp fx) (aset (eas-force-sim-fx sim) i (float fx)) (setq x (float fx)))
         (when (numberp fy) (aset (eas-force-sim-fy sim) i (float fy)) (setq y (float fy)))
         (when (or (isnan x) (isnan y))
           (let ((radius (* 10 (sqrt (+ 0.5 i)))) (angle (* i initial-angle)))
             (setq x (* radius (cos angle)) y (* radius (sin angle)))))
         (when (or (isnan vx) (isnan vy)) (setq vx 0.0 vy 0.0))
         (aset (eas-force-sim-x sim) i x) (aset (eas-force-sim-y sim) i y)
         (aset (eas-force-sim-vx sim) i vx) (aset (eas-force-sim-vy sim) i vy)))
     rows)
    (setf (eas-force-sim-forces sim)
          (mapcar (lambda (force) (eas-force--make sim force rows))
                  (append (plist-get params :forces) nil)))
    sim))

(defun eas-force-tick (sim &optional iterations)
  "Advance SIM by ITERATIONS ticks (1 by default), as d3's simulation.tick."
  (let ((n (eas-force-sim-n sim))
        (xs (eas-force-sim-x sim)) (ys (eas-force-sim-y sim))
        (vxs (eas-force-sim-vx sim)) (vys (eas-force-sim-vy sim))
        (fxs (eas-force-sim-fx sim)) (fys (eas-force-sim-fy sim))
        (decay (eas-force-sim-velocity-decay sim)))
    (dotimes (_ (or iterations 1))
      (let ((alpha (eas-force-sim-alpha sim)))
        (setq alpha (+ alpha (* (- (eas-force-sim-alpha-target sim) alpha) (eas-force-sim-alpha-decay sim))))
        (setf (eas-force-sim-alpha sim) alpha)
        (dolist (f (eas-force-sim-forces sim)) (funcall f alpha))
        (dotimes (i n)
          (if (aref fxs i)
              (progn (aset xs i (aref fxs i)) (aset vxs i 0.0))
            (aset vxs i (* (aref vxs i) decay))
            (aset xs i (+ (aref xs i) (aref vxs i))))
          (if (aref fys i)
              (progn (aset ys i (aref fys i)) (aset vys i 0.0))
            (aset vys i (* (aref vys i) decay))
            (aset ys i (+ (aref ys i) (aref vys i)))))))
    sim))

(defun eas-force-done-p (sim)
  "Non-nil when SIM has cooled below its alphaMin (d3 stops it there)."
  (< (eas-force-sim-alpha sim) (eas-force-sim-alpha-min sim)))

(defun eas-force-rows (sim rows params)
  "ROWS with SIM's positions and velocities in force PARAMS's as fields."
  (let ((as (eas-force--as params)) (i -1))
    (vconcat
     (seq-map (lambda (row)
                (setq i (1+ i))
                (let ((out row))
                  (cl-mapc (lambda (key v) (setq out (eas-plist-put out key (aref v i))))
                           as (list (eas-force-sim-x sim) (eas-force-sim-y sim)
                                    (eas-force-sim-vx sim) (eas-force-sim-vy sim)))
                  out))
              rows))))

(defun eas-force--link-rows (sim rows params)
  "Rows for the links of PARAMS's first link force, placed by SIM over ROWS."
  (when-let* ((force (seq-find (lambda (f) (equal (plist-get f :force) "link"))
                               (plist-get params :forces))))
    (let ((as (mapcar #'eas-key (append (or (plist-get params :linkAs) ["x" "y" "x2" "y2"]) nil)))
          (kind (eas-key (or (plist-get params :kindAs) "eas_kind")))
          (xs (eas-force-sim-x sim)) (ys (eas-force-sim-y sim)))
      (seq-map (lambda (l)
                 (let ((out (eas-plist-put (nth 2 l) kind "link")))
                   (cl-mapc (lambda (key v) (setq out (eas-plist-put out key v)))
                            as (list (aref xs (nth 0 l)) (aref ys (nth 0 l))
                                     (aref xs (nth 1 l)) (aref ys (nth 1 l))))
                   out))
               (eas-force--links force rows)))))

(defun eas-force-transform (rows params)
  "The force transform: ROWS laid out by the simulation PARAMS describe."
  (let* ((rows (if-let* ((pre (plist-get params :transform)))
                   (eas-transform-run pre rows nil "/transform")
                 rows))
         (sim (eas-force-simulation rows params))
         (out (plist-get params :output)))
    (eas-force-tick sim (truncate (eas-force--param params :iterations 300.0)))
    (let ((nodes (eas-force-rows sim rows params)))
      (if (not (equal out "both"))
          nodes
        (let ((kind (eas-key (or (plist-get params :kindAs) "eas_kind"))))
          (vconcat (seq-map (lambda (row) (eas-plist-put row kind "node")) nodes)
                   (eas-force--link-rows sim rows params)))))))

(eas-register-transform
 "force"
 :doc "Force-directed layout (Vega's force transform: d3-force, deterministic)."
 :schema '(:forces (:type "array" :required t
                    :doc "Vega force objects: center, collide, nbody, link, x, y")
           :iterations (:type "number" :default 300 :doc "ticks run for the static layout")
           :static (:type "boolean" :doc "accepted for Vega parity; the layout is always run to completion")
           :alpha (:type "number" :default 1)
           :alphaMin (:type "number" :default 0.001)
           :alphaTarget (:type "number" :default 0)
           :velocityDecay (:type "number" :default 0.4)
           :seed (:type "integer" :default 1 :doc "the jiggle generator's seed (d3's is 1)")
           :as (:type "array" :default ["x" "y" "vx" "vy"] :doc "position and velocity fields")
           :transform (:type "array" :doc "Vega-Lite transforms applied to the rows first")
           :output (:type "string" :default "nodes" :doc "nodes, or both: link rows follow the nodes")
           :linkAs (:type "array" :default ["x" "y" "x2" "y2"] :doc "a link row's source and target fields")
           :kindAs (:type "string" :default "eas_kind" :doc "field telling node rows from link rows"))
 :fn #'eas-force-transform)

;; The force loops touch every node pair each tick; interpreted (as
;; when `make test' loads the source) they would take seconds.
(dolist (f '(eas-force--cover eas-force--add eas-force--tree eas-force--visit eas-force--visit-after
             eas-force--random eas-force--jiggle eas-force--center eas-force--collide
             eas-force--nbody eas-force--link eas-force--position eas-force-tick))
  (unless (compiled-function-p (symbol-function f))
    (byte-compile f)))

(provide 'eas-force)
;;; eas-force.el ends here

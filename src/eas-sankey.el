;;; eas-sankey.el --- the x-eas sankey transform: a flow diagram's layout -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  A sankey (flow) diagram draws nodes as bars in columns
;; and each link as a band from its source to its target, as wide as
;; its value.  Vega-Lite has no such layout, so it is a domain
;; transform, a port of d3-sankey 0.12.3 held to its numbers:
;;
;;   {"x-eas:transform": "sankey", "links": [...], "id": "name",
;;    "width": 600, "height": 400, "nodeWidth": 24, "nodePadding": 8,
;;    "align": "justify", "iterations": 6}
;;
;; The rows are the nodes; "links" holds one row per link with
;; "source", "target" (a node's "id" field, or its position without
;; "id") and a nonnegative value (the "value" field).  The links must
;; form a DAG: a cycle (a self-loop too) signals SHAPE_INVALID naming
;; it.  Layout follows d3: a node's value is the larger of its in- and
;; outflow; its depth the longest path reaching it; "align" (left,
;; right, center, justify) picks its column; one scale ky (the
;; tightest column's) turns values into pixels; "iterations" passes of
;; relaxation move each node toward its neighbours' links, re-sorting
;; columns by position unless "nodeSort" is "input" (keep data order)
;; or a field (ascending).
;;
;; Output, in pixels with y down (draw it on identity scales):
;;
;; - each node row, its fields kept, with "value" and eas_x0, eas_x1,
;;   eas_y0, eas_y1 (its bar), eas_depth, eas_layer, and eas_label_x,
;;   eas_label_y, eas_label_side ("right" in the left half, "left" in
;;   the right half: where its label goes);
;; - then, per link, the vertex rows of its band: the link's fields
;;   (source and target replaced by the nodes' "label" field), and
;;   eas_link, eas_step, eas_x, eas_y0, eas_y1 (the band at eas_x),
;;   eas_width, and eas_y_source, eas_y_target (the band's middle where
;;   it leaves its source and reaches its target).  An area mark with detail eas_link draws the band.
;;   Bands follow d3's sankeyLinkHorizontal center line, a cubic with
;;   both control points at the middle x, sampled on one grid of
;;   "samples" steps per gap between columns, so bands over the same
;;   gap share their x and hover compares them at one x.
;;
;; "kindAs" (eas_kind) holds "node" or "link".  Fields named eas_*
;; are the engine's own, so a {"content": "data"} tooltip leaves them
;; out and shows every other field of the node or link.

;;; Code:

(require 'eas-core)
(require 'eas-transform)
(require 'eas-transform-domain)
(require 'eas-force)

(cl-defstruct (eas-sankey--node (:constructor eas-sankey--node-create)
                                (:copier nil))
  index row value depth height layer x0 x1 y0 y1 (out nil) (in nil))

(cl-defstruct (eas-sankey--link (:constructor eas-sankey--link-create)
                                (:copier nil))
  index row source target value width y0 y1)

(defconst eas-sankey-aligns '("justify" "left" "right" "center")
  "Values of the sankey transform's \"align\".")

(defun eas-sankey--number (params key default)
  "PARAMS's KEY as a float, DEFAULT when absent.
Signal INVALID_INPUT when it is present and not a number."
  (let ((v (plist-get params key)))
    (cond ((memq v '(nil :null)) (float default))
          ((numberp v) (float v))
          (t (eas-signal "INVALID_INPUT"
                         (format "sankey %s must be a number, not %S" (eas-key-name key) v)
                         :param (eas-key-name key))))))

(defun eas-sankey--name (node label)
  "NODE's name: its LABEL field, else its position."
  (let ((v (and label (plist-get (eas-sankey--node-row node) (eas-key label)))))
    (if (memq v '(nil :null)) (eas-sankey--node-index node) v)))

;;; Graph

(defun eas-sankey--graph (rows params)
  "Nodes and links of node ROWS and PARAMS's links: (NODES . LINKS) vectors."
  (let* ((nodes (vconcat (seq-map-indexed (lambda (row i) (eas-sankey--node-create :index i :row row)) rows)))
         (value-key (eas-key (or (plist-get params :value) "value")))
         (links (vconcat
                 (seq-map-indexed
                  (lambda (l i)
                    (let ((v (plist-get (nth 2 l) value-key)))
                      (unless (and (numberp v) (>= v 0))
                        (eas-signal "INVALID_INPUT"
                                    (format "Sankey link %d has value %S; give it a nonnegative number in %S"
                                            i v (eas-key-name value-key))
                                    :index i :field (eas-key-name value-key)))
                      (eas-sankey--link-create :index i :row (nth 2 l)
                                               :source (aref nodes (nth 0 l)) :target (aref nodes (nth 1 l))
                                               :value v)))
                  (eas-force--links params rows)))))
    (seq-doseq (l links)
      (push l (eas-sankey--node-out (eas-sankey--link-source l)))
      (push l (eas-sankey--node-in (eas-sankey--link-target l))))
    (seq-doseq (n nodes)
      (setf (eas-sankey--node-out n) (nreverse (eas-sankey--node-out n))
            (eas-sankey--node-in n) (nreverse (eas-sankey--node-in n))
            (eas-sankey--node-value n)
            (max (cl-loop for l in (eas-sankey--node-out n) sum (eas-sankey--link-value l))
                 (cl-loop for l in (eas-sankey--node-in n) sum (eas-sankey--link-value l)))))
    (cons nodes links)))

(defun eas-sankey--cycle (nodes label)
  "A cycle among NODES as a list of names (by LABEL), or nil."
  (let ((state (make-vector (length nodes) nil)) cycle)
    (cl-labels ((visit (n path)
                  (let ((i (eas-sankey--node-index n)))
                    (pcase (aref state i)
                      ('done nil)
                      ('open (setq cycle (cons n (reverse (cons n (seq-take-while (lambda (m) (not (eq m n))) path))))))
                      (_ (aset state i 'open)
                         (dolist (l (eas-sankey--node-out n))
                           (unless cycle (visit (eas-sankey--link-target l) (cons n path))))
                         (aset state i 'done))))))
      (seq-doseq (n nodes) (unless cycle (visit n nil))))
    (mapcar (lambda (n) (eas-sankey--name n label)) cycle)))

(defun eas-sankey--depths (nodes out-fn end-fn setter)
  "Set, by SETTER, each of NODES's longest path along OUT-FN links.
END-FN gives a link's far node.  d3's breadth-first sweep: a node
reached in sweep X is at least X."
  (let ((current (append nodes nil)) (x 0))
    (while current
      (let (next)
        (dolist (n current)
          (funcall setter n x)
          (dolist (l (funcall out-fn n))
            (cl-pushnew (funcall end-fn l) next :test #'eq)))
        (setq x (1+ x) current (nreverse next))))))

;;; Breadths (d3-sankey's computeNodeBreadths)

(defun eas-sankey--align (node columns align)
  "The column of NODE among COLUMNS columns under ALIGN."
  (pcase align
    ("left" (eas-sankey--node-depth node))
    ("right" (- columns 1 (eas-sankey--node-height node)))
    ("center" (cond ((eas-sankey--node-in node) (eas-sankey--node-depth node))
                    ((eas-sankey--node-out node)
                     (1- (cl-loop for l in (eas-sankey--node-out node)
                                  minimize (eas-sankey--node-depth (eas-sankey--link-target l)))))
                    (t 0)))
    (_ (if (eas-sankey--node-out node) (eas-sankey--node-depth node) (1- columns)))))

(defun eas-sankey--by-breadth (a b)
  "Non-nil when node A sits above node B."
  (< (eas-sankey--node-y0 a) (eas-sankey--node-y0 b)))

(defun eas-sankey--by-end (a b end)
  "Non-nil when link A's END node sits above B's, then by index.
A node not placed yet compares as d3's undefined does: by index."
  (let ((da (eas-sankey--node-y0 (funcall end a))) (db (eas-sankey--node-y0 (funcall end b))))
    (if (and da db (/= da db)) (< da db)
      (< (eas-sankey--link-index a) (eas-sankey--link-index b)))))

(defun eas-sankey--by-source (a b)
  "Non-nil when link A's source sits above B's (then by index)."
  (eas-sankey--by-end a b #'eas-sankey--link-source))

(defun eas-sankey--by-target (a b)
  "Non-nil when link A's target sits above B's (then by index)."
  (eas-sankey--by-end a b #'eas-sankey--link-target))

(defun eas-sankey--reorder (node)
  "Sort NODE's own links by the far node's position."
  (setf (eas-sankey--node-out node) (sort (eas-sankey--node-out node) #'eas-sankey--by-target)
        (eas-sankey--node-in node) (sort (eas-sankey--node-in node) #'eas-sankey--by-source)))

(defun eas-sankey--reorder-neighbours (node)
  "Sort the links of NODE's neighbours by the far node's position."
  (dolist (l (eas-sankey--node-in node))
    (let ((s (eas-sankey--link-source l)))
      (setf (eas-sankey--node-out s) (sort (eas-sankey--node-out s) #'eas-sankey--by-target))))
  (dolist (l (eas-sankey--node-out node))
    (let ((tg (eas-sankey--link-target l)))
      (setf (eas-sankey--node-in tg) (sort (eas-sankey--node-in tg) #'eas-sankey--by-source)))))

(defun eas-sankey--move (node dy)
  "Move NODE down by DY."
  (setf (eas-sankey--node-y0 node) (+ (eas-sankey--node-y0 node) dy)
        (eas-sankey--node-y1 node) (+ (eas-sankey--node-y1 node) dy)))

(defun eas-sankey--target-top (source target py)
  "The y0 of TARGET that would make its link from SOURCE straight.
PY is the node padding."
  (let ((y (- (eas-sankey--node-y0 source) (/ (* (1- (length (eas-sankey--node-out source))) py) 2))))
    (cl-loop for l in (eas-sankey--node-out source)
             until (eq (eas-sankey--link-target l) target)
             do (setq y (+ y (+ (eas-sankey--link-width l) py))))
    (cl-loop for l in (eas-sankey--node-in target)
             until (eq (eas-sankey--link-source l) source)
             do (setq y (- y (eas-sankey--link-width l))))
    y))

(defun eas-sankey--source-top (source target py)
  "The y0 of SOURCE that would make its link to TARGET straight.
PY is the node padding."
  (let ((y (- (eas-sankey--node-y0 target) (/ (* (1- (length (eas-sankey--node-in target))) py) 2))))
    (cl-loop for l in (eas-sankey--node-in target)
             until (eq (eas-sankey--link-source l) source)
             do (setq y (+ y (+ (eas-sankey--link-width l) py))))
    (cl-loop for l in (eas-sankey--node-out source)
             until (eq (eas-sankey--link-target l) target)
             do (setq y (- y (eas-sankey--link-width l))))
    y))

(defun eas-sankey--collide (column y0 y1 py alpha)
  "Push COLUMN's nodes (a vector, by position) apart within Y0..Y1.
PY is the padding; ALPHA how far a pass moves them."
  (let* ((n (length column)) (mid (ash n -1)) (subject (aref column mid)))
    (cl-flet ((up (y i)
                (cl-loop for k downfrom i to 0
                         for node = (aref column k)
                         for dy = (* (- (eas-sankey--node-y1 node) y) alpha)
                         do (when (> dy 1e-6) (eas-sankey--move node (- dy)))
                         (setq y (- (eas-sankey--node-y0 node) py))))
              (down (y i)
                (cl-loop for k from i below n
                         for node = (aref column k)
                         for dy = (* (- y (eas-sankey--node-y0 node)) alpha)
                         do (when (> dy 1e-6) (eas-sankey--move node dy))
                         (setq y (+ (eas-sankey--node-y1 node) py)))))
      (up (- (eas-sankey--node-y0 subject) py) (1- mid))
      (down (+ (eas-sankey--node-y1 subject) py) (1+ mid))
      (up y1 (1- n))
      (down y0 0))))

(defun eas-sankey--relax (columns order fn alpha beta y0 y1 py resort)
  "One relaxation pass over the COLUMNS (a vector of vectors) in ORDER.
FN gives a node's (WEIGHTED-Y . WEIGHT) target; ALPHA scales each
move, BETA the collision pass within Y0..Y1 at padding PY; RESORT
re-sorts a column by position."
  (dolist (i order)
    (let ((column (aref columns i)))
      (seq-doseq (node column)
        (let ((yw (funcall fn node)))
          (when (> (cdr yw) 0)
            (eas-sankey--move node (* (- (/ (car yw) (cdr yw)) (eas-sankey--node-y0 node)) alpha))
            (eas-sankey--reorder-neighbours node))))
      (when resort
        (setq column (vconcat (sort (append column nil) #'eas-sankey--by-breadth)))
        (aset columns i column))
      (eas-sankey--collide column y0 y1 py beta))))

(defun eas-sankey--breadths (nodes params)
  "Lay NODES out in columns by PARAMS; return the column x positions.
The result is (KX . DX): column I spans KX*I to KX*I + DX."
  (let* ((w (eas-sankey--number params :width 600)) (h (eas-sankey--number params :height 400))
         (dx (eas-sankey--number params :nodeWidth 24)) (dy (eas-sankey--number params :nodePadding 8))
         (iterations (truncate (eas-sankey--number params :iterations 6)))
         (align (or (plist-get params :align) "justify"))
         (sort-by (plist-get params :nodeSort))
         (x (1+ (cl-loop for n across nodes maximize (eas-sankey--node-depth n))))
         (kx (if (> x 1) (/ (- w dx) (1- x)) 0.0))
         (columns (make-vector x nil)))
    (unless (member align eas-sankey-aligns)
      (eas-signal "INVALID_INPUT" (format "sankey align %S is not one of %s" align
                                          (string-join eas-sankey-aligns ", "))
                  :param "align"))
    (seq-doseq (node nodes)
      (let ((i (max 0 (min (1- x) (floor (eas-sankey--align node x align))))))
        (setf (eas-sankey--node-layer node) i
              (eas-sankey--node-x0 node) (* i kx)
              (eas-sankey--node-x1 node) (+ (* i kx) dx))
        (push node (aref columns i))))
    (dotimes (i x)
      (let ((col (nreverse (aref columns i))))
        (when (and (stringp sort-by) (not (member sort-by '("auto" "input"))))
          (let ((key (eas-key sort-by)))
            (setq col (sort col (lambda (a b) (eas-force--band-less (plist-get (eas-sankey--node-row a) key)
                                                                    (plist-get (eas-sankey--node-row b) key)))))))
        (aset columns i (vconcat col))))
    (let* ((widest (cl-loop for c across columns maximize (length c)))
           (py (if (> widest 1) (min dy (/ h (1- widest))) dy))
           (ky (let ((ks (cl-loop for c across columns
                                  for total = (cl-loop for n across c sum (eas-sankey--node-value n))
                                  for k = (/ (- h (* (1- (length c)) py)) (float total))
                                  unless (isnan k) collect k)))
                 (if ks (apply #'min ks) 0.0)))
           (resort (member sort-by '(nil :null "auto"))))
      (when (or (isnan ky) (= ky 1.0e+INF)) (setq ky 0.0))
      ;; initializeNodeBreadths
      (seq-doseq (column columns)
        (let ((y 0.0))
          (seq-doseq (node column)
            (setf (eas-sankey--node-y0 node) y
                  (eas-sankey--node-y1 node) (+ y (* (eas-sankey--node-value node) ky)))
            (setq y (+ (eas-sankey--node-y1 node) py))
            (dolist (l (eas-sankey--node-out node))
              (setf (eas-sankey--link-width l) (* (eas-sankey--link-value l) ky))))
          (setq y (/ (+ (- h y) py) (1+ (length column))))
          (seq-do-indexed (lambda (node i) (eas-sankey--move node (* y (1+ i)))) column)
          (seq-doseq (node column) (eas-sankey--reorder node))))
      (let ((forward (number-sequence 1 (1- x)))
            (backward (number-sequence (- x 2) 0 -1)))
        (dotimes (i iterations)
          (let* ((alpha (expt 0.99 i))
                 (beta (max (- 1 alpha) (/ (1+ i) (float iterations)))))
            (eas-sankey--relax
             columns backward
             (lambda (source)
               (let ((y 0.0) (wt 0.0))
                 (dolist (l (eas-sankey--node-out source))
                   (let* ((target (eas-sankey--link-target l))
                          (v (* (eas-sankey--link-value l) (- (eas-sankey--node-layer target) (eas-sankey--node-layer source)))))
                     (setq y (+ y (* (eas-sankey--source-top source target py) v)) wt (+ wt v))))
                 (cons y wt)))
             alpha beta 0.0 h py resort)
            (eas-sankey--relax
             columns forward
             (lambda (target)
               (let ((y 0.0) (wt 0.0))
                 (dolist (l (eas-sankey--node-in target))
                   (let* ((source (eas-sankey--link-source l))
                          (v (* (eas-sankey--link-value l) (- (eas-sankey--node-layer target) (eas-sankey--node-layer source)))))
                     (setq y (+ y (* (eas-sankey--target-top source target py) v)) wt (+ wt v))))
                 (cons y wt)))
             alpha beta 0.0 h py resort)))))
    ;; computeLinkBreadths
    (seq-doseq (node nodes)
      (let ((y0 (eas-sankey--node-y0 node)) (y1 (eas-sankey--node-y0 node)))
        (dolist (l (eas-sankey--node-out node))
          (setf (eas-sankey--link-y0 l) (+ y0 (/ (eas-sankey--link-width l) 2)))
          (setq y0 (+ y0 (eas-sankey--link-width l))))
        (dolist (l (eas-sankey--node-in node))
          (setf (eas-sankey--link-y1 l) (+ y1 (/ (eas-sankey--link-width l) 2)))
          (setq y1 (+ y1 (eas-sankey--link-width l))))))
    (cons kx dx)))

;;; Bands

(defun eas-sankey--bezier-t (s)
  "The t at which d3's horizontal link reaches the fraction S of its run.
x(t) = 3t(1-t)/2 + t^3 rises strictly, so bisection finds it."
  (let ((lo 0.0) (hi 1.0))
    (dotimes (_ 52)
      (let ((mid (/ (+ lo hi) 2)))
        (if (< (+ (* 1.5 mid (- 1 mid)) (* mid mid mid)) s) (setq lo mid) (setq hi mid))))
    (/ (+ lo hi) 2)))

(defun eas-sankey-band-y (xa ya xb yb x)
  "Center y at X of d3's horizontal link from XA YA to XB YB."
  (cond ((or (<= xb xa) (<= x xa)) ya)
        ((>= x xb) yb)
        (t (let ((tt (eas-sankey--bezier-t (/ (- x xa) (float (- xb xa))))))
             (+ ya (* (- yb ya) tt tt (- 3 (* 2 tt))))))))

(defun eas-sankey--grid (link kx dx samples)
  "The x positions LINK's band is sampled at.
Columns sit KX apart, DX wide; each gap between columns has SAMPLES
steps, so bands over one gap share their x."
  (let* ((a (eas-sankey--node-layer (eas-sankey--link-source link)))
         (b (eas-sankey--node-layer (eas-sankey--link-target link)))
         (xa (eas-sankey--node-x1 (eas-sankey--link-source link)))
         (xb (eas-sankey--node-x0 (eas-sankey--link-target link))))
    (if (<= b a) (list xa xb)
      (cl-loop for g from a below b
               for g0 = (+ (* g kx) dx) for g1 = (* (1+ g) kx)
               append (cl-loop for j from 0 to samples
                               collect (+ g0 (/ (* j (- g1 g0)) samples)))))))

;;; The transform

(defun eas-sankey-transform (rows params)
  "The sankey transform: node ROWS laid out with the links PARAMS names."
  (let* ((rows (vconcat (if-let* ((pre (plist-get params :transform)))
                            (eas-transform-run pre rows nil "/transform")
                          rows)))
         (label (or (plist-get params :label) (plist-get params :id) "name"))
         (kind (eas-key (or (plist-get params :kindAs) "eas_kind")))
         (samples (max 1 (truncate (eas-sankey--number params :samples 16))))
         (w (eas-sankey--number params :width 600))
         (graph (eas-sankey--graph rows params))
         (nodes (car graph)) (links (cdr graph)))
    (when-let* ((cycle (eas-sankey--cycle nodes label)))
      (eas-signal "SHAPE_INVALID"
                  (format "Sankey links form a cycle (%s); a flow diagram needs links that never return to a node"
                          (mapconcat (lambda (n) (format "%s" n)) cycle " -> "))
                  :cycle (vconcat cycle)))
    (eas-sankey--depths nodes #'eas-sankey--node-out #'eas-sankey--link-target
                        (lambda (n x) (setf (eas-sankey--node-depth n) x)))
    (eas-sankey--depths nodes #'eas-sankey--node-in #'eas-sankey--link-source
                        (lambda (n x) (setf (eas-sankey--node-height n) x)))
    (let* ((columns (eas-sankey--breadths nodes params))
           (kx (car columns)) (dx (cdr columns)))
      (vconcat
       (seq-map
        (lambda (n)
          (let* ((left (< (eas-sankey--node-x0 n) (/ w 2)))
                 (row (eas-plist-put (eas-plist-put (copy-sequence (eas-sankey--node-row n)) kind "node")
                                     :value (eas-sankey--node-value n))))
            (append row
                    (list :eas_x0 (eas-sankey--node-x0 n) :eas_x1 (eas-sankey--node-x1 n)
                          :eas_y0 (eas-sankey--node-y0 n) :eas_y1 (eas-sankey--node-y1 n)
                          :eas_depth (eas-sankey--node-depth n) :eas_layer (eas-sankey--node-layer n)
                          :eas_label_x (if left (+ (eas-sankey--node-x1 n) 6) (- (eas-sankey--node-x0 n) 6))
                          :eas_label_y (/ (+ (eas-sankey--node-y0 n) (eas-sankey--node-y1 n)) 2)
                          :eas_label_side (if left "right" "left")))))
        nodes)
       (cl-loop
        for l across links
        for s = (eas-sankey--link-source l) for tg = (eas-sankey--link-target l)
        for half = (/ (eas-sankey--link-width l) 2)
        for base = (thread-first (copy-sequence (eas-sankey--link-row l))
                                 (eas-plist-put kind "link")
                                 (eas-plist-put :source (eas-sankey--name s label))
                                 (eas-plist-put :target (eas-sankey--name tg label)))
        append (cl-loop
                for x in (eas-sankey--grid l kx dx samples) for step from 0
                for y = (eas-sankey-band-y (eas-sankey--node-x1 s) (eas-sankey--link-y0 l)
                                           (eas-sankey--node-x0 tg) (eas-sankey--link-y1 l) x)
                collect (append base
                                (list :eas_link (eas-sankey--link-index l) :eas_step step :eas_x x
                                      :eas_y0 (- y half) :eas_y1 (+ y half)
                                      :eas_width (eas-sankey--link-width l)
                                      :eas_y_source (eas-sankey--link-y0 l)
                                      :eas_y_target (eas-sankey--link-y1 l)))))))))

(eas-register-transform
 "sankey"
 :doc "A sankey (flow) layout of node rows and links, after d3-sankey: node bars and link bands in pixels."
 :schema '(:links (:type "array" :required t :doc "link rows with source, target and a nonnegative value")
           :id (:type "string" :doc "node field links refer to (default: the node's position)")
           :value (:type "string" :default "value" :doc "link field holding the flow")
           :label (:type "string" :doc "node field naming it in link rows (default: id, else name)")
           :width (:type "number" :default 600 :doc "layout width in pixels")
           :height (:type "number" :default 400 :doc "layout height in pixels")
           :nodeWidth (:type "number" :default 24 :doc "node bar width in pixels")
           :nodePadding (:type "number" :default 8 :doc "vertical gap between nodes of a column")
           :align (:type "string" :default "justify" :doc "justify, left, right or center")
           :iterations (:type "number" :default 6 :doc "relaxation passes (0: columns in data order)")
           :nodeSort (:type "string" :doc "auto (by position), input (data order) or a node field")
           :samples (:type "number" :default 16 :doc "band samples per gap between columns")
           :kindAs (:type "string" :default "eas_kind" :doc "field telling node rows from link rows")
           :transform (:type "array" :doc "Vega-Lite transforms applied to the node rows first"))
 :fn #'eas-sankey-transform)

(provide 'eas-sankey)
;;; eas-sankey.el ends here

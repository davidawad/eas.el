;;; eas-facet-layout.el --- facet grids laid out as Vega's trellis -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  eas-facet-grid.el lowers a facet into a vconcat of
;; hconcat rows carrying x-eas.facet.  This file gives that node what
;; Vega's trellis layout gives a facet:
;;
;; - shared scales: every cell's x and y scale is computed over all the
;;   cells' rows (`eas-facet-layout-share'), unless resolve.scale says
;;   "independent";
;; - placement (svg): Vega's gridLayout with align "all" and bounds
;;   "full".  The spacing separates the cells' own content (plot, marks
;;   overhanging it, a wrapped facet's labels), not their axes: the y
;;   axes of the first column and the x axes under each column sit
;;   outside the grid, as Vega's row headers and column footers do;
;; - headers: a label per row (left, rotated -90 unless labelAngle says
;;   otherwise) and per column (above), labelPadding 10 from the axes
;;   or plots, and each facet field's title in bold, 10px beyond its
;;   labels plus titlePadding 10, centred on the grid.  They are drawn
;;   as non-interactive text marks of the first cell.
;;
;; In a terminal the cells keep the ordinary concat placement and a
;; one-line header (their labels) above each cell.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-compile-scales)
(require 'eas-transform)

(declare-function eas-place--local-scale "eas-compile-place")

(defun eas-facet-layout-meta (node)
  "NODE's x-eas.facet, when NODE is a lowered facet's layout node."
  (and (plist-get node :concat) (plist-get (plist-get node :x-eas) :facet)))

(defun eas-facet-layout--rows (node)
  "NODE's cells as a list of rows, each a list of groups."
  (mapcar (lambda (row) (mapcar (lambda (c) (plist-get c :group)) (plist-get row :children)))
          (plist-get node :children)))

(defun eas-facet-layout--grid-p (node)
  "Non-nil when NODE is a facet whose cells are all single views."
  (and (eas-facet-layout-meta node)
       (seq-every-p (lambda (row) (and (plist-get row :concat)
                                       (seq-every-p (lambda (c) (plist-get c :group)) (plist-get row :children))))
                    (plist-get node :children))))

(defun eas-facet-layout--nodes (tree)
  "Facet nodes in layout TREE."
  (cond ((plist-get tree :group) nil)
        ((eas-facet-layout--grid-p tree) (list tree))
        (t (apply #'append (mapcar #'eas-facet-layout--nodes (plist-get tree :children))))))

;;; Rows

(defun eas-facet-layout--cell-key (node)
  "Levels facet cell NODE keeps, normalized for hashing, or nil."
  (when-let* ((cell (plist-get (plist-get node :x-eas) :facet-cell)))
    (mapcar (lambda (f) (let ((v (or (plist-get f :equal) :null))) (if (numberp v) (float v) v)))
            (plist-get cell :keys))))

(defun eas-facet-layout-partition (node ctx rows env)
  "Facet NODE with its rows given out once: (NODE2 . ROWS2), or nil.
NODE's own transforms run once over ROWS, and each cell gets its share
of the result as its data (one pass; the rows keep their source index)
instead of filtering all of them itself.  Only for a facet at the root
of the transform chain (CTX inherits none) whose transforms read no
param (x-eas.facet.static), so a selection never needs them run again.
ENV holds param values."
  (let ((meta (plist-get (plist-get node :x-eas) :facet)))
    (when (and meta (eq (plist-get meta :static) t) (null (plist-get ctx :transforms)))
      (let* ((tx (plist-get node :transform))
             (rows (if (and (vectorp tx) (> (length tx) 0))
                       (eas-transform-run tx rows env (concat (plist-get ctx :path) "/transform"))
                     rows))
             (cells (cl-loop for row across (plist-get node :vconcat) append (append (plist-get row :hconcat) nil)))
             (fields (mapcar (lambda (f) (eas-key (plist-get f :field)))
                             (plist-get (plist-get (plist-get (car cells) :x-eas) :facet-cell) :keys)))
             (parts (make-hash-table :test 'equal)))
        (when (seq-every-p #'eas-facet-layout--cell-key cells)
          (seq-doseq (r rows)
            (let ((k (mapcar (lambda (f) (let ((v (plist-get r f)))
                                           (cond ((memq v '(nil :null)) :null) ((numberp v) (float v)) (t v))))
                             fields)))
              (puthash k (cons r (gethash k parts)) parts)))
          (let ((give (lambda (c)
                        (let* ((n (plist-get (plist-get (plist-get c :x-eas) :facet-cell) :filters))
                               (own (seq-drop (plist-get c :transform) n)))
                          (eas-plist-put (eas-plist-put c :transform (vconcat own))
                                         :data (list :values (vconcat (nreverse (copy-sequence
                                                                                 (gethash (eas-facet-layout--cell-key c) parts))))))))))
            (cons (eas-plist-put (eas--plist-without node :transform) :vconcat
                                 (vconcat (mapcar (lambda (row) (eas-plist-put row :hconcat (vconcat (mapcar give (plist-get row :hconcat)))))
                                                  (plist-get node :vconcat))))
                  rows)))))))

;;; Shared scales

(defun eas-facet-layout--merged (cells)
  "The units of CELLS (groups) merged layer by layer over all their rows."
  (let ((counts (delete-dups (mapcar (lambda (g) (length (plist-get g :units))) cells))))
    (if (cdr counts)
        (apply #'append (mapcar (lambda (g) (plist-get g :units)) cells))
      (cl-loop for k from 0 below (car counts)
               collect (let ((u (nth k (plist-get (car cells) :units))))
                         (plist-put (copy-sequence u) :rows
                                    (apply #'vconcat (mapcar (lambda (g) (plist-get (nth k (plist-get g :units)) :rows))
                                                             cells))))))))

(defun eas-facet-layout-share (tree state metrics)
  "Share the x and y scales of every facet in TREE across its cells.
Cells zoomed in STATE keep theirs.  In svg the cells' text headers are
dropped (the trellis draws its own); METRICS gives the target."
  (dolist (node (eas-facet-layout--nodes tree))
    (let* ((meta (eas-facet-layout-meta node))
           (cells (apply #'append (eas-facet-layout--rows node)))
           (zoomed (seq-some (lambda (g) (plist-get (plist-get state :domains) (eas-key (plist-get g :id)))) cells))
           (merged (eas-facet-layout--merged cells)))
      (plist-put node :facet-metrics metrics)
      (unless (eas-layout-text-p metrics)
        (dolist (g cells) (plist-put g :header nil)))
      (dolist (ch '(:x :y))
        (unless (or zoomed (memq ch (plist-get meta :independent)))
          (when-let* ((s (eas-compile-position-scale merged ch nil)))
            (dolist (g cells)
              (when (plist-get (plist-get g :scales) ch)
                (plist-put g :scales (plist-put (copy-sequence (plist-get g :scales)) ch s))))))))))

;;; Headers

(defun eas-facet-layout--num (header key default)
  "HEADER's number KEY, else DEFAULT."
  (let ((v (plist-get header key))) (if (numberp v) v default)))

(defun eas-facet-layout--labels-p (header labels)
  "Non-nil when HEADER shows LABELS."
  (and (vectorp labels) (> (length labels) 0) (not (eq (plist-get header :labels) :false))))

(defun eas-facet-layout--row-label-extent (header label metrics)
  "Pixels row LABEL under HEADER takes left of its padding, and its anchor
offset: (EXTENT . ANCHOR), ANCHOR measured leftward from the padding.
Vega offsets a horizontal row label by its own width, then aligns it."
  (let ((angle (eas-facet-layout--num header :labelAngle -90))
        (size (eas-facet-layout--num header :labelFontSize 10)))
    (if (memq (round angle) '(-90 90 270))
        (cons size 0)
      (let ((w (eas-layout-text-width metrics label size)))
        (pcase (or (plist-get header :labelAlign) "right")
          ("left" (cons w w))
          ("center" (cons (* 1.5 w) w))
          (_ (cons (* 2 w) w)))))))

(defun eas-facet-layout--text (text x y size &rest props)
  "A header text item: TEXT at X Y in SIZE px, PROPS as for scene text."
  (append (list :text text :x x :y y :fontSize size :fill (or (plist-get props :fill) "black") :opacity 1)
          (eas--plist-without props :fill)))

;;; Placement

(defun eas-facet-layout--axis-width (g)
  "How far group G's left axis reaches left of its plot (0 without one)."
  (let ((left (plist-get (plist-get g :chrome) :left))
        (over (aref (or (plist-get g :mark-over) [0 0 0 0]) 0))
        (axis (seq-find (lambda (a) (equal (plist-get a :channel) "y")) (plist-get g :axes-model))))
    (if (and axis (> left over)
             (or (plist-get axis :title)
                 (seq-some (lambda (tk) (let ((l (plist-get tk :label))) (and (stringp l) (not (string-empty-p l)))))
                           (plist-get axis :ticks))))
        left 0)))

(defun eas-facet-layout--axis-bounds (g channel metrics)
  "Bounds [X1 Y1 X2 Y2] of group G's CHANNEL axis relative to its plot, or nil."
  (when-let* ((axis (seq-find (lambda (a) (equal (plist-get a :channel) (eas-key-name channel)))
                              (plist-get g :axes-model)))
              (scale (eas-place--local-scale g channel)))
    (plist-get (eas-layout-axis-place axis scale (vector 0 0 (plist-get g :w) (plist-get g :h)) metrics) :bounds)))

(defun eas-facet-layout--box (g indep metrics)
  "Group G's content box [X1 Y1 X2 Y2] relative to its plot origin.
The axes of the channels in INDEP (independent scales) are the cell's own."
  (let* ((over (or (plist-get g :mark-over) [0 0 0 0]))
         (box (vector (- (aref over 0)) (- (aref over 1))
                      (+ (plist-get g :w) (aref over 2)) (+ (plist-get g :h) (aref over 3)))))
    (dolist (ch '(:x :y) box)
      (when-let* ((b (and (memq ch indep) (eas-facet-layout--axis-bounds g ch metrics))))
        (setq box (vector (min (aref box 0) (aref b 0)) (min (aref box 1) (aref b 1))
                          (max (aref box 2) (aref b 2)) (max (aref box 3) (aref b 3))))))))

(defun eas-facet-layout--plan (node metrics)
  "Trellis plan of facet NODE: plot origins relative to the grid's origin,
the extent of everything around them and the header items, as
\(:origins ((G X . Y) ...) :left :top :right :bottom :items ITEMS)."
  (let* ((meta (eas-facet-layout-meta node))
         (rows (eas-facet-layout--rows node))
         (wrap (eq (plist-get meta :wrap) t))
         (chdr (plist-get meta :column-header)) (rhdr (plist-get meta :row-header))
         (clabels (plist-get meta :column-labels)) (rlabels (plist-get meta :row-labels))
         (clp (eas-facet-layout--num chdr :labelPadding 10)) (cls (eas-facet-layout--num chdr :labelFontSize 10))
         (show-c (eas-facet-layout--labels-p chdr clabels))
         (pad-c (plist-get meta :spacing-column)) (pad-r (plist-get meta :spacing-row))
         (boxes (mapcar (lambda (row)
                          (mapcar (lambda (g)
                                    (let ((b (eas-facet-layout--box g (plist-get meta :independent) metrics)))
                                      (when (and wrap show-c) (aset b 1 (min (aref b 1) (- (+ clp cls)))))
                                      b))
                                  row))
                        rows))
         (all (apply #'append boxes))
         (xmax (apply #'max 0 (mapcar (lambda (b) (ceiling (aref b 2))) all)))
         (ymax (apply #'max 0 (mapcar (lambda (b) (ceiling (aref b 3))) all)))
         (offx (apply #'max 0 (cl-loop for row in boxes append
                                       (mapcar (lambda (b) (+ pad-c (max 0 (ceiling (- (aref b 0)))))) (cdr row)))))
         (offy (apply #'max 0 (mapcar (lambda (b) (+ pad-r (max 0 (ceiling (- (aref b 1))))))
                                      (apply #'append (cdr boxes)))))
         ;; Vega-Lite does not align the cells along an independent scale
         ;; the facet does not split (align "none"): each row and column
         ;; then follows its own neighbours' extents.
         (indep (plist-get meta :independent))
         (none (or (and (null (plist-get meta :row-labels)) (memq :x indep))
                   (and (or wrap (null (plist-get meta :column-labels))) (memq :y indep))))
         (origins
          (if (not none)
              (cl-loop for row in rows for i from 0
                       append (cl-loop for g in row for j from 0
                                       collect (cons g (cons (* j (+ xmax offx)) (* i (+ ymax offy))))))
            (let* ((ncols (apply #'max (mapcar #'length rows)))
                   (xext (cl-loop for j below ncols
                                  collect (apply #'max 0 (delq nil (mapcar (lambda (brow) (let ((b (nth j brow))) (and b (ceiling (aref b 2))))) boxes)))))
                   (yext (mapcar (lambda (brow) (apply #'max 0 (mapcar (lambda (b) (ceiling (aref b 3))) brow))) boxes))
                   (ys (make-vector ncols 0)) out)
              (cl-loop for row in rows for brow in boxes for i from 0
                       do (let ((x 0))
                            (cl-loop for g in row for b in brow for j from 0
                                     do (when (> j 0) (setq x (+ x (nth (1- j) xext) pad-c (max 0 (ceiling (- (aref b 0)))))))
                                     (when (> i 0) (aset ys j (+ (aref ys j) (nth (1- i) yext) pad-r (max 0 (ceiling (- (aref b 1)))))))
                                     (push (cons g (cons x (aref ys j))) out))))
              (nreverse out))))
         (origin (lambda (g) (cdr (assq g origins))))
         ;; The grid's own bounds (cells' content), and everything with axes.
         (gx1 0) (gy1 0) (gx2 0) (gy2 0) (left 0) (top 0) (right 0) (bottom 0) items)
    (cl-loop for row in rows for brow in boxes
             do (cl-loop for g in row for b in brow
                         for o = (funcall origin g) for c = (plist-get g :chrome)
                         do (setq gx1 (min gx1 (+ (car o) (aref b 0))) gy1 (min gy1 (+ (cdr o) (aref b 1)))
                                  gx2 (max gx2 (+ (car o) (aref b 2))) gy2 (max gy2 (+ (cdr o) (aref b 3)))
                                  left (min left (- (car o) (plist-get c :left)) (+ (car o) (aref b 0)))
                                  top (min top (- (cdr o) (plist-get c :top)) (+ (cdr o) (aref b 1)))
                                  right (max right (+ (car o) (plist-get g :w) (plist-get c :right)) (+ (car o) (aref b 2)))
                                  bottom (max bottom (+ (cdr o) (plist-get g :h) (plist-get c :bottom)) (+ (cdr o) (aref b 3))))))
    ;; Row headers: labels left of the first column's axes.
    (let* ((hx (min 0 (apply #'min 0 (mapcar (lambda (row) (aref (car row) 0)) boxes))))
           (indep-y (memq :y (plist-get meta :independent)))
           (axis-w (lambda (g) (if indep-y 0 (eas-facet-layout--axis-width g))))
           (hleft (apply #'min left (mapcar (lambda (row) (- hx (funcall axis-w (car row)))) rows))))
      (when (eas-facet-layout--labels-p rhdr rlabels)
        (let* ((lp (eas-facet-layout--num rhdr :labelPadding 10))
               (size (eas-facet-layout--num rhdr :labelFontSize 10))
               (angle (eas-facet-layout--num rhdr :labelAngle -90)))
          (cl-loop for row in rows for i from 0 for label across rlabels
                   do (let* ((g (car row)) (o (funcall origin g))
                             (axis (- (+ (car o) hx) (funcall axis-w g)))
                             (e (eas-facet-layout--row-label-extent rhdr label metrics))
                             (rotated (= (cdr e) 0))
                             (x (if rotated (- axis lp) (- axis lp (cdr e))))
                             (y (+ (cdr o) (/ (plist-get g :h) 2.0))))
                        (setq hleft (min hleft (- axis lp (car e))))
                        (push (eas-facet-layout--text
                               label x y size
                               :angle (if rotated angle 0)
                               :align (if rotated "center" (or (plist-get rhdr :labelAlign) "right"))
                               :baseline (if rotated "bottom" "middle")
                               :fontWeight (plist-get rhdr :labelFontWeight)
                               :fill (plist-get rhdr :labelColor))
                              items)))))
      (setq left (min left hleft))
      ;; The row field's title, 10px beyond the labels plus titlePadding.
      (when-let* ((title (plist-get meta :row-title)))
        (let* ((size (eas-facet-layout--num rhdr :titleFontSize 11))
               (x (- (floor (- hleft (if (eas-facet-layout--labels-p rhdr rlabels) 10 0)))
                     (eas-facet-layout--num rhdr :titlePadding 10))))
          (push (eas-facet-layout--text title x (/ (+ gy1 gy2) 2.0) size :angle -90 :align "center"
                                        :baseline "bottom" :fontWeight (or (plist-get rhdr :titleFontWeight) "bold")
                                        :fill (plist-get rhdr :titleColor))
                items)
          (setq left (min left (- x size))))))
    ;; Column headers: labels above the first row (or each wrapped cell).
    (let ((htop (min top (if (and show-c (not wrap)) (- (min 0 gy1) clp cls) top))))
      (when show-c
        (if wrap
            (cl-loop for (g . o) in origins for label across clabels
                     do (push (eas-facet-layout--text label (+ (car o) (/ (plist-get g :w) 2.0)) (- (cdr o) clp) cls
                                                      :align "center" :baseline "bottom"
                                                      :fontWeight (plist-get chdr :labelFontWeight)
                                                      :fill (plist-get chdr :labelColor))
                              items))
          (cl-loop for g in (car rows) for label across clabels
                   for o = (funcall origin g)
                   do (push (eas-facet-layout--text label (+ (car o) (/ (plist-get g :w) 2.0)) (- (min 0 gy1) clp) cls
                                                    :align "center" :baseline "bottom"
                                                    :fontWeight (plist-get chdr :labelFontWeight)
                                                    :fill (plist-get chdr :labelColor))
                            items))))
      (setq top htop)
      (when-let* ((title (plist-get meta :column-title)))
        (let* ((size (eas-facet-layout--num chdr :titleFontSize 11))
               (y (- (floor (- (if wrap (min 0 gy1) htop) (if (and show-c (not wrap)) 10 0)))
                     (eas-facet-layout--num chdr :titlePadding 10))))
          (push (eas-facet-layout--text title (/ (+ gx1 gx2) 2.0) y size :align "center" :baseline "bottom"
                                        :fontWeight (or (plist-get chdr :titleFontWeight) "bold")
                                        :fill (plist-get chdr :titleColor))
                items)
          (setq top (min top (- y size))))))
    ;; A wrapped facet's x axes all sit under the last row, as Vega's column
    ;; footers do: a column ending a row early moves its axis down.
    (when (and wrap (cdr rows))
      (let ((last-y (cdr (funcall origin (car (car (last rows)))))))
        (cl-loop for row in (butlast rows)
                 do (cl-loop for g in row for j from 0
                             unless (nth j (car (last rows)))
                             do (let ((dy (- last-y (cdr (funcall origin g)))))
                                  (plist-put g :facet-axis-offset dy)
                                  (setq bottom (max bottom (+ last-y (plist-get g :h) (plist-get (plist-get g :chrome) :bottom)))))))))
    (list :origins origins :left left :top top :right right :bottom bottom :items (nreverse items))))

(defun eas-facet-layout-arrange (node ox oy metrics)
  "Place facet NODE's cells with the block's top-left at OX OY.
Return the block's (W . H), or nil when NODE is no facet grid or the
target is text."
  (when (and (eas-facet-layout--grid-p node) (not (eas-layout-text-p metrics)))
    (let* ((plan (eas-facet-layout--plan node metrics))
           (dx (- ox (plist-get plan :left))) (dy (- oy (plist-get plan :top)))
           (first (car (car (eas-facet-layout--rows node)))))
      (pcase-dolist (`(,g ,x . ,y) (plist-get plan :origins))
        (plist-put g :x0 (+ dx x))
        (plist-put g :y0 (+ dy y))
        (when-let* ((off (plist-get g :facet-axis-offset))
                    (axis (seq-find (lambda (a) (equal (plist-get a :channel) "x")) (plist-get g :axes-model))))
          (plist-put axis :offset off)))
      (plist-put first :facet-items
                 (mapcar (lambda (it) (plist-put (plist-put (copy-sequence it) :x (+ dx (plist-get it :x)))
                                                 :y (+ dy (plist-get it :y))))
                         (plist-get plan :items)))
      (cons (ceiling (- (plist-get plan :right) (plist-get plan :left)))
            (ceiling (- (plist-get plan :bottom) (plist-get plan :top)))))))

(defun eas-facet-layout-lead (node key)
  "Chrome on side KEY (:left or :top) before facet NODE's first plot, or nil."
  (let ((metrics (plist-get node :facet-metrics)))
    (when (and metrics (eas-facet-layout--grid-p node) (not (eas-layout-text-p metrics))
               (plist-get (car (car (eas-facet-layout--rows node))) :chrome))
      (- (plist-get (eas-facet-layout--plan node metrics) key)))))

(defun eas-facet-layout-min-plot (group metrics)
  "Smallest (W . H) plot of GROUP whose axis labels do not collide.
A band axis keeps every label (rotated labels need their font height
each, others their width); a continuous axis keeps at least its first
and last label."
  (let ((size (plist-get metrics :label-size)) (w 0) (h 0))
    ;; In a terminal y labels take a row each and band labels are thinned,
    ;; except fewer than three, which Vega's overlap removal never thins:
    ;; only those and a continuous x axis's labels can collide.
    (dolist (axis (if (eas-layout-text-p metrics)
                      (seq-filter (lambda (a) (and (equal (plist-get a :channel) "x")
                                                   (or (not (eq (plist-get a :discrete) t))
                                                       (< (length (plist-get a :ticks)) 3))))
                                  (plist-get group :axes-model))
                    (plist-get group :axes-model)))
      (let* ((labels (delq nil (mapcar (lambda (tk) (let ((l (plist-get tk :label))) (and (stringp l) (not (string-empty-p l)) l)))
                                       (plist-get axis :ticks))))
             (widths (mapcar (lambda (l) (eas-layout-text-width metrics l size)) labels))
             (widest (apply #'max 0 widths))
             (x (equal (plist-get axis :channel) "x"))
             (rotated (and x (not (zerop (or (plist-get axis :labelAngle) 0)))))
             ;; Labels keep 2px apart, two cells in a terminal (centred labels round to cells).
             (gap (if (eas-layout-text-p metrics) (* 2 (aref (plist-get metrics :cell) 0)) 2))
             (along (if (or (not x) rotated) (+ size 2) (+ widest gap))))
        (when labels
          (let ((need (if (eq (plist-get axis :discrete) t) (* (length labels) along)
                        (* 2 along))))
            (if x (setq w (max w need)) (setq h (max h need)))))))
    (cons w h)))

(declare-function eas-place-fit-grid "eas-compile-grid")

(defun eas-facet-layout-fit (node width height metrics)
  "Resize facet NODE's plots so its block is WIDTH by HEIGHT; nil when
NODE is no facet grid.  In a terminal every cell gets one plot size, as
a repeat grid's do, so the cells' axes share their rows."
  (cond
   ((not (eas-facet-layout--grid-p node)) nil)
   ((eas-layout-text-p metrics) (eas-place-fit-grid node width height metrics) t)
   (t
    (let* ((meta (eas-facet-layout-meta node))
           (cells (apply #'append (eas-facet-layout--rows node)))
           (size (eas-facet-layout-arrange node 0 0 metrics))
           (g0 (car cells))
           (w (max (* 4 (aref (plist-get metrics :cell) 0))
                   (+ (plist-get g0 :w) (/ (- width (car size)) (float (plist-get meta :columns))))))
           (h (max (* 2 (aref (plist-get metrics :cell) 1))
                   (+ (plist-get g0 :h) (/ (- height (cdr size)) (float (plist-get meta :rows)))))))
      (dolist (g cells)
        (let ((least (eas-facet-layout-min-plot g metrics)))
          (setq w (max w (car least)) h (max h (cdr least)))))
      (dolist (g cells) (plist-put g :w w) (plist-put g :h h) (plist-put g :fit-height h))
      t))))

(defun eas-facet-layout-view-marks (group)
  "The trellis header marks GROUP draws (the first cell's), as a list."
  (when-let* ((items (plist-get group :facet-items)))
    (list (list :id (format "%s/facet-headers" (plist-get group :id)) :mark "text" :interactive-off t :rows []
                :items (vconcat items)))))

(defvar eas-place-arrange-functions)
(defvar eas-place-fit-functions)
(defvar eas-place-lead-functions)
(add-hook 'eas-place-arrange-functions #'eas-facet-layout-arrange)
(add-hook 'eas-place-fit-functions #'eas-facet-layout-fit)
(add-hook 'eas-place-lead-functions #'eas-facet-layout-lead)

(provide 'eas-facet-layout)
;;; eas-facet-layout.el ends here

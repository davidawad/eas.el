;;; eas-force-drag.el --- drag the nodes of network templates -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  Two pointer interactions of the Vega network examples,
;; declared in a template's x-eas block:
;;
;;   "interaction": {"force-drag": {"slot": "nodes"}}
;;
;; The template lays out its "nodes" data slot with the force
;; transform (eas-force.el).  A "drag" event (or a pointerdown on a
;; node then a pointerup) pins the node under the press where the
;; pointer is released, and a dblclick on a node frees it.  As in
;; Vega's force-directed example, the simulation then runs again from
;; where every node is now: the nodes slot is rebound to the current
;; layout's rows (their x, y, vx and vy are where the simulation
;; starts) with fx and fy set on the pinned node.
;;
;;   "interaction": {"matrix-reorder": {"slot": "nodes", "sort": "sort"}}
;;
;; The template orders the nodes of an adjacency matrix with the graph
;; transform (eas-force-graph.el) by the field its "sort" slot names.
;; Dragging a column label (an angled text) sideways, or a row label
;; up or down, moves that node where the pointer is released, as
;; Vega's reorderable matrix does: the node's rank becomes
;; 0.5 + count * x / width (y / height for a row) and the others keep
;; theirs; the nodes slot is rebound with that rank as "eas_rank" and
;; the sort slot names it.
;;
;; Either way the view is then resolved and compiled afresh, and the
;; result stays deterministic, so `eas-replay' of a log redraws the
;; same picture.  Event to state stays a pure reducer elsewhere; this
;; runs from `eas-view-dispatch-functions' because a drag here changes
;; the data the view was resolved from, not a scale domain or a
;; selection.

;;; Code:

(require 'eas-core)
(require 'eas-template)
(require 'eas-resolve)
(require 'eas-hit)
(require 'eas-intersect)
(require 'eas-scale)
(require 'eas-view)

(defun eas-force-drag--interaction (view kind)
  "VIEW's template interaction KIND (:force-drag, :matrix-reorder), or nil."
  (when-let* ((name (eas-view-template view))
              (template (ignore-errors (eas-template-get name))))
    (plist-get (plist-get (plist-get template :meta) :interaction) kind)))

(defun eas-force-drag--slot (view)
  "The data slot (a keyword) of the nodes the pointer drags in VIEW, or nil."
  (when-let* ((drag (or (eas-force-drag--interaction view :force-drag)
                        (eas-force-drag--interaction view :matrix-reorder))))
    (eas-key (or (plist-get drag :slot) "nodes"))))

(defun eas-force-drag--kind (row)
  "ROW's network row kind (\"node\", \"link\" or \"cell\"), or nil."
  (plist-get row :eas_kind))

(defun eas-force-drag--node-p (row)
  "Non-nil when ROW is a node row (or a row of a plain node table)."
  (member (eas-force-drag--kind row) '("node" nil)))

(defun eas-force-drag--nodes (view)
  "VIEW's node rows, in the order of its nodes slot."
  (seq-filter #'eas-force-drag--node-p (plist-get (eas-view-data view) :rows)))

(defun eas-force-drag--index (view row)
  "Position among VIEW's node rows of mark ROW (its _eas_row is the root row)."
  (let ((root (plist-get row :_eas_row)) (rows (plist-get (eas-view-data view) :rows)))
    (if (integerp root)
        (cl-loop for i below root count (eas-force-drag--node-p (aref rows i)))
      (seq-position (eas-force-drag--nodes view) row #'equal))))

(defun eas-force-drag--items (scene px pred)
  "(ITEM . ROW) of node items in SCENE's view at PX that PRED (ITEM) accepts."
  (when-let* ((sv (eas-hit-view-at scene (aref px 0) (aref px 1))))
    (let (out)
      (seq-doseq (mark (plist-get sv :marks))
        (let ((rows (plist-get mark :rows)))
          (seq-doseq (item (plist-get mark :items))
            (let ((row (and (plist-get item :datum) (aref rows (plist-get item :datum)))))
              (when (and (equal (eas-force-drag--kind row) "node")
                         (numberp (plist-get item :x)) (numberp (plist-get item :y))
                         (funcall pred item))
                (push (cons item row) out))))))
      (nreverse out))))

(defun eas-force-drag--node-at (view scene px)
  "Index among VIEW's node rows of the node drawn at PX in SCENE, or nil.
That is the nearest node symbol the pointer is on, within
`eas-intersect-slop'; links ending there do not count."
  (let ((x (aref px 0)) (y (aref px 1)) best best-d)
    (dolist (hit (eas-force-drag--items scene px (lambda (item) (not (plist-get item :text)))))
      (let* ((item (car hit))
             (d (- (sqrt (+ (expt (- (plist-get item :x) x) 2) (expt (- (plist-get item :y) y) 2)))
                   (/ (sqrt (or (plist-get item :size) 0)) 2.0))))
        (when (and (<= d eas-intersect-slop) (or (null best-d) (<= d best-d)))
          (setq best (cdr hit) best-d d))))
    (and best (eas-force-drag--index view best))))

(defun eas-force-drag--data-point (scene px)
  "The data-space (X . Y) at PX in SCENE's view there."
  (let* ((sv (or (eas-hit-view-at scene (aref px 0) (aref px 1))
                 (aref (plist-get scene :views) 0)))
         (scales (plist-get sv :scales)))
    (cons (eas-scale-invert (plist-get scales :x) (aref px 0))
          (eas-scale-invert (plist-get scales :y) (aref px 1)))))

(defun eas-force-drag--rebind (view slot-values)
  "Resolve and compile VIEW again with its bindings updated by SLOT-VALUES.
SLOT-VALUES is a plist of slot keywords and values.  Return VIEW."
  (let ((bindings (copy-sequence (eas-view-bindings view))))
    (cl-loop for (slot value) on slot-values by #'cddr
             do (setq bindings (eas-plist-put bindings slot value)))
    (let ((spec (eas-resolve (eas-view-template view) bindings)))
      (setf (eas-view-bindings view) bindings
            (eas-view-spec view) spec
            (eas-view-spec-hash view) (eas-resolve-hash spec)
            (eas-view-data view) (eas-data-make (or (eas-view--root-rows spec) []))
            (eas-view-plan view) nil)
      (setf (eas-view-scene view) (eas-view--compile view))
      (run-hook-with-args 'eas-view-changed-functions view)
      view)))

(defun eas-force-drag-rebind (view index point)
  "Run VIEW's layout again with node INDEX pinned at POINT, or freed.
POINT is a data-space (X . Y); nil frees the node.  Every node starts
from its current position.  Return VIEW."
  (eas-force-drag--rebind
   view
   (list (eas-force-drag--slot view)
         (vconcat
          (seq-map-indexed
           (lambda (row i)
             (let ((out (eas--plist-without row :eas_kind)))
               (cond ((/= i index) out)
                     (point (thread-first out
                                          (eas-plist-put :fx (car point)) (eas-plist-put :fy (cdr point))
                                          (eas-plist-put :x (car point)) (eas-plist-put :y (cdr point))))
                     (t (eas--plist-without (eas--plist-without out :fx) :fy)))))
           (eas-force-drag--nodes view))))))

(defun eas-force-drag--label-at (view scene px)
  "(INDEX . AXIS) of the node label at PX in VIEW's SCENE, or nil.
AXIS is :x for a column label (angled text), :y for a row label."
  (let ((x (aref px 0)) (y (aref px 1)) best best-d)
    (dolist (hit (eas-force-drag--items scene px (lambda (item) (plist-get item :text))))
      (let* ((item (car hit))
             (column (not (zerop (mod (or (plist-get item :angle) 0) 360))))
             (half (/ (or (plist-get item :fontSize) 10) 2.0))
             ;; Across the text, within half its size; along it, on the label's side.
             (d (if column (abs (- (plist-get item :x) x)) (abs (- (plist-get item :y) y))))
             (side (if column (<= y (+ (plist-get item :y) half)) (<= x (+ (plist-get item :x) half)))))
        (when (and side (<= d half) (or (null best-d) (< d best-d)))
          (setq best (cons (cdr hit) (if column :x :y)) best-d d))))
    (and best (cons (eas-force-drag--index view (car best)) (cdr best)))))

(defun eas-force-drag-reorder (view index axis point)
  "Move node INDEX of matrix VIEW to POINT along AXIS (:x or :y).
POINT is the data-space (X . Y) where the label was released.  Return
VIEW."
  (let* ((reorder (eas-force-drag--interaction view :matrix-reorder))
         (nodes (eas-force-drag--nodes view))
         (n (length nodes))
         (sv (aref (plist-get (eas-view-scene view) :views) 0))
         (scale (plist-get (plist-get sv :scales) axis))
         (lo (min (aref (plist-get scale :domain) 0) (aref (plist-get scale :domain) 1)))
         (hi (max (aref (plist-get scale :domain) 0) (aref (plist-get scale :domain) 1)))
         (v (if (eq axis :x) (car point) (cdr point)))
         (dest (+ 0.5 (/ (* n (- (max lo (min hi v)) lo)) (float (- hi lo))))))
    (eas-force-drag--rebind
     view
     (list (eas-force-drag--slot view)
           (vconcat (seq-map-indexed
                     (lambda (row i)
                       (eas-plist-put (eas--plist-without row :eas_kind) :eas_rank
                                      (if (= i index) dest (plist-get row :order))))
                     nodes))
           (eas-key (or (plist-get reorder :sort) "sort"))
           "eas_rank"))))

(defun eas-force-drag--state (view key value)
  "Set KEY of VIEW's state to VALUE."
  (setf (eas-view-state view) (eas-plist-put (eas-view-state view) key value)))

(defun eas-force-drag--on-drag (view from to scene)
  "Act on a drag of interactive VIEW from FROM to TO, both px, over SCENE."
  (if (eas-force-drag--interaction view :matrix-reorder)
      (when-let* ((label (eas-force-drag--label-at view scene from)))
        (eas-force-drag-reorder view (car label) (cdr label) (eas-force-drag--data-point scene to)))
    (when-let* ((i (eas-force-drag--node-at view scene from)))
      (eas-force-drag-rebind view i (eas-force-drag--data-point scene to)))))

(defun eas-force-drag--on-dispatch (view event _old-state old-scene)
  "Drag the nodes of network VIEW after EVENT on OLD-SCENE."
  (when (and (eas-view-interactive view) (eas-force-drag--slot view))
    (pcase (plist-get event :type)
      ("drag" (eas-force-drag--on-drag view (plist-get event :from) (plist-get event :to) old-scene))
      ("pointerdown" (eas-force-drag--state view :force-drag (plist-get event :px)))
      ("pointerup"
       (when-let* ((from (plist-get (eas-view-state view) :force-drag)))
         (eas-force-drag--state view :force-drag nil)
         (unless (equal from (plist-get event :px))
           (eas-force-drag--on-drag view from (plist-get event :px) old-scene))))
      ("dblclick"
       (when (eas-force-drag--interaction view :force-drag)
         (when-let* ((i (eas-force-drag--node-at view old-scene (plist-get event :px))))
           (when (plist-get (nth i (eas-force-drag--nodes view)) :fx)
             (eas-force-drag-rebind view i nil))))))))

(add-hook 'eas-view-dispatch-functions #'eas-force-drag--on-dispatch)

(provide 'eas-force-drag)
;;; eas-force-drag.el ends here

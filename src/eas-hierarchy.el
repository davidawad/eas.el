;;; eas-hierarchy.el --- hierarchies of rows: stratify, nest, links and paths -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's hierarchy transforms, as x-eas domain
;; transforms over plain rows.  Vega keeps a hidden tree between its
;; stratify and its layouts; eas rows carry no hidden state, so a
;; hierarchy is always a key column and a parent-key column ("id" and
;; "parent" by default), and every layout rebuilds the tree from them:
;;
;;   stratify   checks the rows form one tree; adds depth and children
;;   nest       groups rows by key fields into such a tree, emitting a
;;              row for the root and each group (Vega's nest, generated)
;;   treelinks  one {source, target} row per parent-child edge
;;   treepath   for each {source, target} link, the rows on the tree path
;;              between them (Vega's treePath expression), for bundling
;;   formula    Vega's formula as a domain transform, so a layout's
;;              derived columns (a radial tree's angles) exist before
;;              linkpath or treepath read them; "params" names constants
;;
;; The layouts (tree and cluster in eas-hierarchy-tree.el, treemap and
;; partition in eas-hierarchy-tile.el, pack in eas-hierarchy-pack.el)
;; share `eas-hierarchy-layout': stratify, sum FIELD (else count
;; leaves), sort siblings by SORT (Vega's compare, ties in data order),
;; run the d3-hierarchy algorithm, and write its fields back onto the
;; rows in their input order.  Keys compare as strings, as d3.stratify
;; compares them, so 1 and "1" name one node.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)
(require 'eas-expr)

(cl-defstruct (eas-hierarchy-node (:constructor eas-hierarchy--make-node) (:copier nil))
  "A node of a hierarchy built from rows."
  row index parent children (depth 0) (height 0) (value 0) x y x0 y0 x1 y1 r)

;;; Building

(defun eas-hierarchy--id (value)
  "VALUE as a node key string, or nil when VALUE is null or missing."
  (cond ((or (null value) (memq value '(:null :false))) nil)
        ((equal value "") nil)
        ((stringp value) value)
        ((and (floatp value) (= value (ffloor value)) (< (abs value) 1e15)) (format "%d" (truncate value)))
        (t (format "%s" value))))

(defun eas-hierarchy-stratify (rows key parent-key)
  "Build the hierarchy of ROWS from columns KEY and PARENT-KEY.
Return (ROOT . NODES): NODES is a vector of nodes in row order.
Signal SHAPE_INVALID, with the offending row's index, when the rows
do not form exactly one tree."
  (let* ((kk (eas-key key)) (pk (eas-key parent-key))
         (n (length rows)) (nodes (make-vector n nil))
         (by-id (make-hash-table :test 'equal :size (max 16 n))) root)
    (dotimes (i n)
      (let* ((row (aref rows i)) (id (eas-hierarchy--id (plist-get row kk))))
        (aset nodes i (eas-hierarchy--make-node :row row :index i))
        (when id
          (when (gethash id by-id)
            (eas-signal "SHAPE_INVALID" (format "Hierarchy key %s %s is not unique" key id)
                        :index i :field key))
          (puthash id (aref nodes i) by-id))))
    (dotimes (i n)
      (let* ((node (aref nodes i))
             (pid (eas-hierarchy--id (plist-get (aref rows i) pk))))
        (if (null pid)
            (if root
                (eas-signal "SHAPE_INVALID"
                            (format "Hierarchy has more than one root (rows %d and %d have no %s)"
                                    (eas-hierarchy-node-index root) i parent-key)
                            :index i :field parent-key)
              (setq root node))
          (let ((parent (gethash pid by-id)))
            (unless parent
              (eas-signal "SHAPE_INVALID" (format "Row %d names parent %s, which no row's %s holds" i pid key)
                          :index i :field parent-key))
            (setf (eas-hierarchy-node-parent node) parent)
            (push node (eas-hierarchy-node-children parent))))))
    (unless root
      (eas-signal "SHAPE_INVALID" (format "Hierarchy has no root: every row names a %s" parent-key)
                  :field parent-key))
    (let ((seen 0))
      (eas-hierarchy-each-before
       root (lambda (node)
              (setq seen (1+ seen))
              (setf (eas-hierarchy-node-children node) (nreverse (eas-hierarchy-node-children node)))
              (let ((p (eas-hierarchy-node-parent node)))
                (when p (setf (eas-hierarchy-node-depth node) (1+ (eas-hierarchy-node-depth p)))))))
      (when (< seen n)
        (eas-signal "SHAPE_INVALID" "Hierarchy has a cycle: some rows never reach the root"
                    :field parent-key :index (eas-hierarchy--cycle-index nodes root))))
    (eas-hierarchy-each-after
     root (lambda (node)
            (let ((p (eas-hierarchy-node-parent node)) (h (eas-hierarchy-node-height node)))
              (when (and p (> (1+ h) (eas-hierarchy-node-height p)))
                (setf (eas-hierarchy-node-height p) (1+ h))))))
    (cons root nodes)))

(defun eas-hierarchy--cycle-index (nodes root)
  "Index of the first of NODES that does not reach ROOT."
  (cl-loop for i below (length nodes)
           for node = (aref nodes i)
           unless (let ((p node) (steps 0))
                    (while (and p (not (eq p root)) (< steps (length nodes)))
                      (setq p (eas-hierarchy-node-parent p) steps (1+ steps)))
                    (eq p root))
           return i))

;;; Traversal (d3-hierarchy's orders)

(defun eas-hierarchy-each-before (root fn)
  "Call FN on every node under ROOT in pre-order, children left to right."
  (let ((stack (list root)))
    (while stack
      (let ((node (pop stack)))
        (funcall fn node)
        (setq stack (append (eas-hierarchy-node-children node) stack))))))

(defun eas-hierarchy-each-after (root fn)
  "Call FN on every node under ROOT in post-order, children left to right."
  (let ((stack (list root)) next)
    (while stack
      (let ((node (pop stack)))
        (push node next)
        (dolist (c (eas-hierarchy-node-children node)) (push c stack))))
    (dolist (node next) (funcall fn node))))

(defun eas-hierarchy-each (root fn)
  "Call FN on every node under ROOT breadth first."
  (let ((queue (list root)))
    (while queue
      (let ((node (pop queue)))
        (funcall fn node)
        (setq queue (append queue (eas-hierarchy-node-children node)))))))

;;; Values and order

(defun eas-hierarchy-sum (root field)
  "Set each node's value under ROOT: its FIELD plus its children's values.
A nil FIELD counts leaves instead, as d3's node.count does."
  (let ((k (and field (eas-key field))))
    (eas-hierarchy-each-after
     root (lambda (node)
            (let ((children (eas-hierarchy-node-children node)))
              (setf (eas-hierarchy-node-value node)
                    (if k
                        (let ((v (plist-get (eas-hierarchy-node-row node) k)))
                          (+ (if (numberp v) v 0)
                             (cl-loop for c in children sum (eas-hierarchy-node-value c))))
                      (if children (cl-loop for c in children sum (eas-hierarchy-node-value c)) 1))))))))

(defun eas-hierarchy--node-field (node field)
  "FIELD of NODE: value, depth or height of the node, else its row's field.
\"data.F\" always reads the row."
  (pcase field
    ("value" (eas-hierarchy-node-value node))
    ("depth" (eas-hierarchy-node-depth node))
    ("height" (eas-hierarchy-node-height node))
    (_ (plist-get (eas-hierarchy-node-row node)
                  (eas-key (if (string-prefix-p "data." field) (substring field 5) field))))))

(defun eas-hierarchy--less (a b)
  "Vega's ascending order of field values A and B (nil when not less)."
  (cond ((and (numberp a) (numberp b)) (< a b))
        ((and (stringp a) (stringp b)) (string< a b))
        ((memq a '(nil :null)) (not (memq b '(nil :null))))
        (t nil)))

(defun eas-hierarchy-compare (sort)
  "A predicate ordering nodes by Vega compare SORT, ties in data order.
SORT is {\"field\": F or [F ...], \"order\": O or [O ...]}."
  (let* ((fields (let ((f (plist-get sort :field))) (if (vectorp f) (append f nil) (list f))))
         (orders (let ((o (plist-get sort :order))) (if (vectorp o) (append o nil) (list o)))))
    (lambda (a b)
      (let ((result 'tie))
        (cl-loop for f in fields for i from 0
                 for desc = (equal (nth i orders) "descending")
                 for va = (eas-hierarchy--node-field a f) for vb = (eas-hierarchy--node-field b f)
                 do (cond ((eas-hierarchy--less va vb) (setq result (not desc)))
                          ((eas-hierarchy--less vb va) (setq result desc)))
                 until (not (eq result 'tie)))
        (if (eq result 'tie)
            (< (eas-hierarchy-node-index a) (eas-hierarchy-node-index b))
          result)))))

(defun eas-hierarchy-sort (root sort)
  "Sort every node's children under ROOT by Vega compare SORT."
  (let ((less (eas-hierarchy-compare sort)))
    (eas-hierarchy-each-before
     root (lambda (node)
            (when (eas-hierarchy-node-children node)
              (setf (eas-hierarchy-node-children node)
                    (sort (eas-hierarchy-node-children node) less)))))))

;;; The shared layout driver

(defconst eas-hierarchy-common-schema
  '(:key (:type "string" :default "id" :doc "the column holding each row's key")
    :parentKey (:type "string" :default "parent" :doc "the column holding the parent's key (null at the root)")
    :field (:type "string" :doc "the value summed up the tree; without it, leaves count 1")
    :sort (:type "object" :doc "Vega compare of siblings: {field, order}; value, depth and height are the node's"))
  "Parameters every hierarchy layout transform takes.")

(defun eas-hierarchy-round (v)
  "V rounded half up, as JavaScript's Math.round."
  (if (numberp v) (floor (+ v 0.5)) v))

(defun eas-hierarchy-layout (rows params layout fields)
  "ROWS laid out as a hierarchy by LAYOUT, under transform PARAMS.
LAYOUT is called with the root node and PARAMS after the tree is built,
summed and sorted.  FIELDS lists the node accessors written to the
rows, the last one being the children count; PARAMS's :as renames
them.  Return the rows in input order."
  (let* ((built (eas-hierarchy-stratify rows (plist-get params :key) (plist-get params :parentKey)))
         (root (car built)) (nodes (cdr built))
         (as (let ((a (plist-get params :as))) (if (vectorp a) (append a nil) (mapcar #'car fields)))))
    (unless (= (length as) (length fields))
      (eas-signal "INVALID_INPUT" (format "as needs %d field names: %s" (length fields)
                                          (mapconcat #'car fields ", "))
                  :path "as"))
    (eas-hierarchy-sum root (plist-get params :field))
    (when (plist-get params :sort) (eas-hierarchy-sort root (plist-get params :sort)))
    (funcall layout root params)
    (vconcat
     (mapcar (lambda (node)
               (let ((row (eas-hierarchy-node-row node)))
                 (cl-loop for (_ . get) in fields for name in as
                          do (setq row (eas-plist-put row (eas-key name) (funcall get node))))
                 row))
             nodes))))

(defun eas-hierarchy-children-count (node)
  "The number of NODE's children."
  (length (eas-hierarchy-node-children node)))

;;; stratify

(defun eas-hierarchy--stratify (rows params)
  "The stratify transform: ROWS checked as one tree, with depth and children.
PARAMS name the key columns and the output fields."
  (eas-hierarchy-layout rows params #'ignore
                        (list (cons "depth" #'eas-hierarchy-node-depth)
                              (cons "children" #'eas-hierarchy-children-count))))

(eas-register-transform
 "stratify"
 :doc "Check rows form one tree by key and parentKey (Vega's stratify); add depth and children counts."
 :schema (append (seq-take eas-hierarchy-common-schema 4)
                 '(:as (:type "array" :default ["depth" "children"] :doc "depth and children-count fields")))
 :fn #'eas-hierarchy--stratify)

;;; nest

(defun eas-hierarchy--nest (rows params)
  "The nest transform: ROWS grouped by PARAMS's keys into a tree.
Emit a root row, one row per group (its key field set to the group's
value, and \"key\" too), then the input rows, each with id and parent
columns (PARAMS's :as) linking them."
  (let* ((keys (mapcar #'eas-key (append (plist-get params :keys) nil)))
         (as (plist-get params :as)) (ik (eas-key (aref as 0))) (pk (eas-key (aref as 1)))
         (root-id (plist-get params :root))
         (groups (make-hash-table :test 'equal)) out)
    (push (list ik root-id pk :null :key root-id) out)
    (seq-do-indexed
     (lambda (row i)
       (let ((parent root-id) (path root-id) (prefix nil))
         (dolist (k keys)
           (let ((v (plist-get row k)))
             (setq prefix (append prefix (list k v))
                   path (concat path "/" (eas-hierarchy--id-or-null v)))
             (unless (gethash path groups)
               (puthash path t groups)
               (push (append (list ik path pk parent :key v) prefix) out))
             (setq parent path)))
         (push (eas-plist-put (eas-plist-put row ik (format "%s#%d" parent i)) pk parent) out)))
     rows)
    (vconcat (nreverse out))))

(defun eas-hierarchy--id-or-null (v)
  "V as a key segment; \"null\" for a missing value."
  (or (eas-hierarchy--id v) "null"))

(eas-register-transform
 "nest"
 :doc "Group rows by key fields into a tree of id/parent rows: a root, one row per group, then the rows."
 :schema '(:keys (:type "array" :required t :doc "the grouping fields, outermost first")
           :as (:type "array" :default ["id" "parent"] :doc "the key and parent-key fields written")
           :root (:type "string" :default "root" :doc "the root row's key"))
 :fn #'eas-hierarchy--nest)

;;; treelinks

(defun eas-hierarchy--treelinks (rows params)
  "The treelinks transform: one {source, target} row per edge of ROWS's tree.
Links come breadth first; source and target are the parent and child
rows themselves, as in Vega.  PARAMS name the key columns."
  (let ((root (car (eas-hierarchy-stratify rows (plist-get params :key) (plist-get params :parentKey))))
        out)
    (eas-hierarchy-each
     root (lambda (node)
            (when-let* ((p (eas-hierarchy-node-parent node)))
              (push (list :source (eas-hierarchy-node-row p) :target (eas-hierarchy-node-row node)) out))))
    (vconcat (nreverse out))))

(eas-register-transform
 "treelinks"
 :doc "One {source, target} row per parent-child edge of a tree of rows (Vega's treelinks)."
 :schema (seq-take eas-hierarchy-common-schema 4)
 :fn #'eas-hierarchy--treelinks)

;;; treepath

(defun eas-hierarchy-path (from to)
  "The nodes from node FROM up to the common ancestor and down to TO.
This is d3's node.path: the ancestor appears once."
  (let ((anc (let ((seen (make-hash-table :test 'eq)) (p from))
               (while p (puthash p t seen) (setq p (eas-hierarchy-node-parent p)))
               (let ((q to)) (while (and q (not (gethash q seen))) (setq q (eas-hierarchy-node-parent q))) q)))
        up down)
    (let ((p from)) (while (not (eq p anc)) (push p up) (setq p (eas-hierarchy-node-parent p))))
    (let ((q to)) (while (not (eq q anc)) (push q down) (setq q (eas-hierarchy-node-parent q))))
    (append (nreverse up) (list anc) down)))

(defun eas-hierarchy--treepath (rows params)
  "The treepath transform: for each link of PARAMS's :links, its tree path.
ROWS form the tree.  Each path node's row is emitted with the link's
index, the step along the path and the link's source and target keys
\(PARAMS's :as names them).  A link naming a key the tree lacks is
skipped."
  (let* ((built (eas-hierarchy-stratify rows (plist-get params :key) (plist-get params :parentKey)))
         (kk (eas-key (plist-get params :key)))
         (by-id (make-hash-table :test 'equal))
         (sk (eas-key (plist-get params :source))) (tk (eas-key (plist-get params :target)))
         (as (mapcar #'eas-key (append (plist-get params :as) nil)))
         out)
    (seq-do (lambda (node) (puthash (eas-hierarchy--id (plist-get (eas-hierarchy-node-row node) kk)) node by-id))
            (cdr built))
    (seq-do-indexed
     (lambda (link i)
       (let* ((s (plist-get link sk)) (tg (plist-get link tk))
              (a (gethash (eas-hierarchy--id s) by-id)) (b (gethash (eas-hierarchy--id tg) by-id)))
         (when (and a b)
           (cl-loop for node in (eas-hierarchy-path a b) for j from 0
                    do (push (append (eas-hierarchy-node-row node)
                                     (list (nth 0 as) i (nth 1 as) j (nth 2 as) s (nth 3 as) tg))
                             out)))))
     (plist-get params :links))
    (vconcat (nreverse out))))

(eas-register-transform
 "treepath"
 :doc "For each {source, target} link, the tree's rows on the path between them (Vega's treePath)."
 :schema (append (seq-take eas-hierarchy-common-schema 4)
                 '(:links (:type "array" :required t :doc "link rows, e.g. a data slot of dependencies")
                   :source (:type "string" :default "source" :doc "the link field holding the source key")
                   :target (:type "string" :default "target" :doc "the link field holding the target key")
                   :as (:type "array" :default ["link" "step" "source" "target"]
                        :doc "fields for the link index, the step along it, and its source and target keys")))
 :fn #'eas-hierarchy--treepath)

;;; formula

(defun eas-hierarchy--formula (rows params)
  "The formula transform: PARAMS's expr evaluated per row of ROWS into as.
PARAMS's params object binds names the expression may use."
  (let ((expr (plist-get params :expr)) (as (eas-key (plist-get params :as)))
        (env (plist-get params :params)))
    (vconcat (seq-map (lambda (row) (eas-plist-put row as (eas-expr-evaluate expr row env))) rows))))

(eas-register-transform
 "formula"
 :doc "A Vega expression per row into a field, run with the domain transforms (Vega's formula)."
 :schema '(:expr (:type "string" :required t :doc "the expression; datum is the row")
           :as (:type "string" :required t :doc "the field written")
           :params (:type "object" :doc "names the expression may use, e.g. {\"originX\": 360}"))
 :fn #'eas-hierarchy--formula)

(provide 'eas-hierarchy)
;;; eas-hierarchy.el ends here

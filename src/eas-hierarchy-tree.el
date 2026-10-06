;;; eas-hierarchy-tree.el --- node-link layouts: tidy tree and cluster -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's tree transform, an x-eas domain transform:
;;
;;   {"x-eas:transform": "tree", "method": "tidy"|"cluster",
;;    "size": [W, H] | "nodeSize": [DX, DY], "separation": true,
;;    "as": ["x", "y", "depth", "children"]}
;;
;; "tidy" is d3's tree (Buchheim, Junger and Leipert's linear-time
;; Reingold-Tilford), "cluster" d3's dendrogram with every leaf at the
;; same depth.  x runs along the breadth of the tree and y along its
;; depth; swap the "as" names for a horizontal tree, or read x as an
;; angle (size [1, R]) for a radial one.  With separation true (the
;; default), cousins sit twice as far apart as siblings.  The key,
;; parentKey, field and sort parameters are eas-hierarchy.el's.

;;; Code:

(require 'eas-core)
(require 'eas-hierarchy)

;;; d3's tree

(cl-defstruct (eas-hierarchy--tn (:constructor eas-hierarchy--tn-make (node i)) (:copier nil))
  "A node of d3's tidy-tree pass: prelim z, mod m, change c, shift s,
thread th, ancestor a and default ancestor anc."
  node i parent children anc a (z 0.0) (m 0.0) (c 0.0) (s 0.0) th)

(defun eas-hierarchy--tn-root (root)
  "The tidy-tree wrapper of ROOT, under a virtual parent."
  (let* ((top (eas-hierarchy--tn-make root 0)) (stack (list top)))
    (setf (eas-hierarchy--tn-a top) top)
    (while stack
      (let* ((tn (pop stack)) (kids (eas-hierarchy-node-children (eas-hierarchy--tn-node tn))))
        (when kids
          (setf (eas-hierarchy--tn-children tn)
                (vconcat (cl-loop for k in kids for i from 0
                                  collect (let ((c (eas-hierarchy--tn-make k i)))
                                            (setf (eas-hierarchy--tn-a c) c (eas-hierarchy--tn-parent c) tn)
                                            (push c stack)
                                            c)))))))
    (let ((virtual (eas-hierarchy--tn-make nil 0)))
      (setf (eas-hierarchy--tn-a virtual) virtual
            (eas-hierarchy--tn-children virtual) (vector top)
            (eas-hierarchy--tn-parent top) virtual))
    top))

(defun eas-hierarchy--tn-each-after (tn fn)
  "Call FN on TN's tree in post-order."
  (let ((stack (list tn)) next)
    (while stack
      (let ((v (pop stack)))
        (push v next)
        (seq-doseq (c (or (eas-hierarchy--tn-children v) [])) (push c stack))))
    (dolist (v next) (funcall fn v))))

(defun eas-hierarchy--tn-each-before (tn fn)
  "Call FN on TN's tree in pre-order."
  (let ((stack (list tn)))
    (while stack
      (let ((v (pop stack)))
        (funcall fn v)
        (setq stack (append (eas-hierarchy--tn-children v) stack))))))

(defun eas-hierarchy--next-left (v)
  "V's leftmost child, else its thread."
  (let ((cs (eas-hierarchy--tn-children v))) (if cs (aref cs 0) (eas-hierarchy--tn-th v))))

(defun eas-hierarchy--next-right (v)
  "V's rightmost child, else its thread."
  (let ((cs (eas-hierarchy--tn-children v))) (if cs (aref cs (1- (length cs))) (eas-hierarchy--tn-th v))))

(defun eas-hierarchy--move-subtree (wm wp shift)
  "Shift subtree WP right by SHIFT, spreading the change back to WM."
  (let ((change (/ shift (float (- (eas-hierarchy--tn-i wp) (eas-hierarchy--tn-i wm))))))
    (cl-decf (eas-hierarchy--tn-c wp) change)
    (cl-incf (eas-hierarchy--tn-s wp) shift)
    (cl-incf (eas-hierarchy--tn-c wm) change)
    (cl-incf (eas-hierarchy--tn-z wp) shift)
    (cl-incf (eas-hierarchy--tn-m wp) shift)))

(defun eas-hierarchy--execute-shifts (v)
  "Apply the shifts gathered on V's children."
  (let ((shift 0.0) (change 0.0) (cs (eas-hierarchy--tn-children v)))
    (cl-loop for i from (1- (length cs)) downto 0
             for w = (aref cs i)
             do (cl-incf (eas-hierarchy--tn-z w) shift)
             (cl-incf (eas-hierarchy--tn-m w) shift)
             (setq change (+ change (eas-hierarchy--tn-c w))
                   shift (+ shift (eas-hierarchy--tn-s w) change)))))

(defun eas-hierarchy--next-ancestor (vim v ancestor)
  "VIM's ancestor when it is V's sibling, else ANCESTOR."
  (if (eq (eas-hierarchy--tn-parent (eas-hierarchy--tn-a vim)) (eas-hierarchy--tn-parent v))
      (eas-hierarchy--tn-a vim)
    ancestor))

(defun eas-hierarchy--apportion (v w ancestor sep)
  "Push V's subtree clear of its left siblings (W the nearest); SEP spaces.
Return the new default ANCESTOR."
  (when w
    (let* ((vip v) (vop v) (vim w)
           (vom (aref (eas-hierarchy--tn-children (eas-hierarchy--tn-parent vip)) 0))
           (sip (eas-hierarchy--tn-m vip)) (sop (eas-hierarchy--tn-m vop))
           (sim (eas-hierarchy--tn-m vim)) (som (eas-hierarchy--tn-m vom)))
      (while (progn (setq vim (eas-hierarchy--next-right vim) vip (eas-hierarchy--next-left vip))
                    (and vim vip))
        (setq vom (eas-hierarchy--next-left vom) vop (eas-hierarchy--next-right vop))
        (setf (eas-hierarchy--tn-a vop) v)
        (let ((shift (+ (- (+ (eas-hierarchy--tn-z vim) sim) (eas-hierarchy--tn-z vip) sip)
                        (funcall sep (eas-hierarchy--tn-node vim) (eas-hierarchy--tn-node vip)))))
          (when (> shift 0)
            (eas-hierarchy--move-subtree (eas-hierarchy--next-ancestor vim v ancestor) v shift)
            (setq sip (+ sip shift) sop (+ sop shift))))
        (setq sim (+ sim (eas-hierarchy--tn-m vim)) sip (+ sip (eas-hierarchy--tn-m vip))
              som (+ som (eas-hierarchy--tn-m vom)) sop (+ sop (eas-hierarchy--tn-m vop))))
      (when (and vim (not (eas-hierarchy--next-right vop)))
        (setf (eas-hierarchy--tn-th vop) vim)
        (cl-incf (eas-hierarchy--tn-m vop) (- sim sop)))
      (when (and vip (not (eas-hierarchy--next-left vom)))
        (setf (eas-hierarchy--tn-th vom) vip)
        (cl-incf (eas-hierarchy--tn-m vom) (- sip som))
        (setq ancestor v))))
  ancestor)

(defun eas-hierarchy--first-walk (v sep)
  "Run d3's firstWalk on V with separation SEP."
  (let* ((cs (eas-hierarchy--tn-children v)) (parent (eas-hierarchy--tn-parent v))
         (siblings (eas-hierarchy--tn-children parent))
         (i (eas-hierarchy--tn-i v))
         (w (and (> i 0) (aref siblings (1- i)))))
    (cond
     (cs (eas-hierarchy--execute-shifts v)
         (let ((mid (/ (+ (eas-hierarchy--tn-z (aref cs 0)) (eas-hierarchy--tn-z (aref cs (1- (length cs))))) 2.0)))
           (if w
               (setf (eas-hierarchy--tn-z v) (+ (eas-hierarchy--tn-z w)
                                                (funcall sep (eas-hierarchy--tn-node v) (eas-hierarchy--tn-node w)))
                     (eas-hierarchy--tn-m v) (- (eas-hierarchy--tn-z v) mid))
             (setf (eas-hierarchy--tn-z v) mid))))
     (w (setf (eas-hierarchy--tn-z v) (+ (eas-hierarchy--tn-z w)
                                         (funcall sep (eas-hierarchy--tn-node v) (eas-hierarchy--tn-node w))))))
    (setf (eas-hierarchy--tn-anc parent)
          (eas-hierarchy--apportion v w (or (eas-hierarchy--tn-anc parent) (aref siblings 0)) sep))))

(defun eas-hierarchy-separation (separation)
  "The separation of d3: cousins twice siblings, 1 when SEPARATION is false."
  (if (memq separation '(:false nil))
      (lambda (_a _b) 1)
    (lambda (a b) (if (eq (eas-hierarchy-node-parent a) (eas-hierarchy-node-parent b)) 1 2))))

(defun eas-hierarchy-tidy (root size node-size sep)
  "Lay ROOT out as d3's tidy tree, into SIZE [W H] or with NODE-SIZE [DX DY].
SEP is the separation function.  Set each node's x and y."
  (let ((top (eas-hierarchy--tn-root root)))
    (eas-hierarchy--tn-each-after top (lambda (v) (eas-hierarchy--first-walk v sep)))
    (setf (eas-hierarchy--tn-m (eas-hierarchy--tn-parent top)) (- (eas-hierarchy--tn-z top)))
    (eas-hierarchy--tn-each-before
     top (lambda (v)
           (let ((p (eas-hierarchy--tn-parent v)))
             (setf (eas-hierarchy-node-x (eas-hierarchy--tn-node v)) (+ (eas-hierarchy--tn-z v) (eas-hierarchy--tn-m p)))
             (cl-incf (eas-hierarchy--tn-m v) (eas-hierarchy--tn-m p)))))
    (if node-size
        (eas-hierarchy-each-before
         root (lambda (n) (setf (eas-hierarchy-node-x n) (* (eas-hierarchy-node-x n) (aref node-size 0))
                                (eas-hierarchy-node-y n) (* (eas-hierarchy-node-depth n) (aref node-size 1)))))
      (let ((left root) (right root) (bottom root))
        (eas-hierarchy-each-before
         root (lambda (n)
                (when (< (eas-hierarchy-node-x n) (eas-hierarchy-node-x left)) (setq left n))
                (when (> (eas-hierarchy-node-x n) (eas-hierarchy-node-x right)) (setq right n))
                (when (> (eas-hierarchy-node-depth n) (eas-hierarchy-node-depth bottom)) (setq bottom n))))
        (let* ((s (if (eq left right) 1 (/ (funcall sep left right) 2.0)))
               (tx (- s (eas-hierarchy-node-x left)))
               (kx (/ (aref size 0) (float (+ (eas-hierarchy-node-x right) s tx))))
               (ky (/ (aref size 1) (float (max 1 (eas-hierarchy-node-depth bottom))))))
          (eas-hierarchy-each-before
           root (lambda (n) (setf (eas-hierarchy-node-x n) (* (+ (eas-hierarchy-node-x n) tx) kx)
                                  (eas-hierarchy-node-y n) (* (eas-hierarchy-node-depth n) ky)))))))))

;;; d3's cluster

(defun eas-hierarchy-cluster (root size node-size sep)
  "Lay ROOT out as d3's cluster (dendrogram), into SIZE or with NODE-SIZE.
SEP is the separation function.  Set each node's x and y."
  (let ((previous nil) (x 0.0))
    (eas-hierarchy-each-after
     root (lambda (n)
            (let ((cs (eas-hierarchy-node-children n)))
              (if cs
                  (setf (eas-hierarchy-node-x n) (/ (cl-loop for c in cs sum (eas-hierarchy-node-x c)) (float (length cs)))
                        (eas-hierarchy-node-y n) (1+ (apply #'max (mapcar #'eas-hierarchy-node-y cs))))
                (setf (eas-hierarchy-node-x n) (if previous (setq x (+ x (funcall sep n previous))) 0.0)
                      (eas-hierarchy-node-y n) 0
                      previous n)))))
    (let* ((left (let ((n root)) (while (eas-hierarchy-node-children n) (setq n (car (eas-hierarchy-node-children n)))) n))
           (right (let ((n root)) (while (eas-hierarchy-node-children n) (setq n (car (last (eas-hierarchy-node-children n))))) n))
           (x0 (- (eas-hierarchy-node-x left) (/ (funcall sep left right) 2.0)))
           (x1 (+ (eas-hierarchy-node-x right) (/ (funcall sep right left) 2.0)))
           (rx (eas-hierarchy-node-x root)) (ry (eas-hierarchy-node-y root)))
      (eas-hierarchy-each-after
       root (if node-size
                (lambda (n) (setf (eas-hierarchy-node-x n) (* (- (eas-hierarchy-node-x n) rx) (aref node-size 0))
                                  (eas-hierarchy-node-y n) (* (- ry (eas-hierarchy-node-y n)) (aref node-size 1))))
              (lambda (n) (setf (eas-hierarchy-node-x n) (* (/ (- (eas-hierarchy-node-x n) x0) (- x1 x0)) (aref size 0))
                                (eas-hierarchy-node-y n) (* (- 1 (if (/= ry 0) (/ (eas-hierarchy-node-y n) (float ry)) 1))
                                                            (aref size 1)))))))))

;;; The transform

(defun eas-hierarchy--tree (rows params)
  "The tree transform: ROWS laid out per PARAMS (see the commentary)."
  (let* ((method (plist-get params :method))
         (size (plist-get params :size)) (node-size (plist-get params :nodeSize))
         (sep (eas-hierarchy-separation (plist-get params :separation))))
    (unless (member method '("tidy" "cluster"))
      (eas-signal "INVALID_INPUT" (format "tree method %S is not tidy or cluster" method) :path "method"))
    (eas-hierarchy-layout
     rows params
     (lambda (root _params)
       (funcall (if (equal method "tidy") #'eas-hierarchy-tidy #'eas-hierarchy-cluster)
                root (or size [1 1]) node-size sep))
     (list (cons "x" #'eas-hierarchy-node-x) (cons "y" #'eas-hierarchy-node-y)
           (cons "depth" #'eas-hierarchy-node-depth) (cons "children" #'eas-hierarchy-children-count)))))

(eas-register-transform
 "tree"
 :doc "Node-link tree layout of id/parent rows: tidy (Reingold-Tilford) or cluster (Vega's tree)."
 :schema (append eas-hierarchy-common-schema
                 '(:method (:type "string" :default "tidy" :doc "tidy or cluster")
                   :size (:type "array" :doc "[width height] the layout fills (default [1 1])")
                   :nodeSize (:type "array" :doc "[dx dy] per node instead of size")
                   :separation (:type "boolean" :default t :doc "space cousins twice as far as siblings")
                   :as (:type "array" :default ["x" "y" "depth" "children"] :doc "output fields")))
 :fn #'eas-hierarchy--tree)

(provide 'eas-hierarchy-tree)
;;; eas-hierarchy-tree.el ends here

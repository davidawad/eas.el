;;; eas-force-graph.el --- the x-eas graph transform: nodes joined to links -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  A network diagram reads two tables, nodes and links,
;; and Vega joins them with aggregate and lookup transforms over
;; several datasets, which a Vega-Lite unit cannot do.
;; {"x-eas:transform": "graph", "links": [...]} does that join on the
;; node rows:
;;
;; - each node gets its "order" (1-based rank, stable, by the "sort"
;;   field when there is one, else in data order), its "degree" (the
;;   links that touch it; a self-loop counts twice) and the node
;;   "count" (the "as" fields);
;; - with "output": "both" every link follows the nodes as a row of
;;   its own: its fields, the count, and every field of its source and
;;   target nodes prefixed "source_" and "target_";
;; - with "cross": true every (column, row) pair of nodes follows too,
;;   prefixed the same way, as Vega's cross transform (an adjacency
;;   matrix draws one cell per pair).
;;
;; Links name nodes by position (Vega's default) or, with "id", by
;; that node field.  "kindAs" names the field ("eas_kind") that holds
;; "node", "link" or "cell".  Layout stays with the marks: positions
;; follow from order and count with plain calculate transforms.

;;; Code:

(require 'eas-core)
(require 'eas-transform)
(require 'eas-transform-domain)
(require 'eas-force)

(defun eas-force-graph--prefixed (row prefix)
  "ROW's fields renamed PREFIX + name."
  (cl-loop for (k v) on row by #'cddr
           append (list (eas-key (concat prefix (eas-key-name k))) v)))

(defun eas-force-graph--order (rows sort)
  "Vector of 1-based ranks of ROWS, stably sorted by field SORT if any."
  (let* ((n (length rows)) (order (make-vector n 0))
         (idx (number-sequence 0 (1- n)))
         (key (and (stringp sort) (eas-force--accessor sort)))
         (sorted (if key
                     (sort idx (lambda (a b)
                                 (let ((ka (funcall key (aref rows a))) (kb (funcall key (aref rows b))))
                                   (and (not (equal ka kb)) (eas-force--band-less ka kb)))))
                   idx)))
    (cl-loop for i in sorted for r from 1 do (aset order i r))
    order))

(defun eas-force-graph-transform (rows params)
  "The graph transform: node ROWS joined to the links PARAMS names."
  (let* ((rows (vconcat (if-let* ((pre (plist-get params :transform)))
                            (eas-transform-run pre rows nil "/transform")
                          rows)))
         (n (length rows))
         (as (mapcar #'eas-key (append (or (plist-get params :as) ["order" "degree" "count"]) nil)))
         (kind (eas-key (or (plist-get params :kindAs) "eas_kind")))
         (links (eas-force--links params rows))
         (degree (make-vector n 0))
         (order (eas-force-graph--order rows (plist-get params :sort)))
         (output (plist-get params :output)))
    (seq-doseq (l links)
      (aset degree (nth 0 l) (1+ (aref degree (nth 0 l))))
      (aset degree (nth 1 l) (1+ (aref degree (nth 1 l)))))
    (let* ((nodes (vconcat
                   (seq-map-indexed
                    (lambda (row i)
                      (thread-first row
                                    (eas-plist-put (nth 0 as) (aref order i))
                                    (eas-plist-put (nth 1 as) (aref degree i))
                                    (eas-plist-put (nth 2 as) n)))
                    rows)))
           (source (seq-map (lambda (r) (eas-force-graph--prefixed r "source_")) nodes))
           (target (seq-map (lambda (r) (eas-force-graph--prefixed r "target_")) nodes)))
      (vconcat
       (if (member output '("both" "links"))
           (seq-map (lambda (r) (eas-plist-put r kind "node")) nodes)
         nodes)
       (when (member output '("both" "links"))
         (seq-map (lambda (l)
                    (append (list kind "link" (nth 2 as) n)
                            (nth 2 l) (nth (nth 0 l) source) (nth (nth 1 l) target)))
                  links))
       (when (eas-true-p (plist-get params :cross))
         (let (cells)
           (dotimes (b n)
             (dotimes (a n)
               (push (append (list kind "cell" (nth 2 as) n) (nth a source) (nth b target)) cells)))
           (nreverse cells)))))))

(eas-register-transform
 "graph"
 :doc "Nodes joined to links: node order, degree and count; link rows with source_ and target_ node fields."
 :schema '(:links (:type "array" :required t :doc "link rows with source and target")
           :id (:type "string" :doc "node field links refer to (default: the node's position)")
           :sort (:type "string" :doc "node field the order follows (stable); data order without it")
           :output (:type "string" :default "nodes" :doc "nodes, or both: link rows follow the nodes")
           :cross (:type "boolean" :doc "true: a row per (column, row) pair of nodes follows")
           :as (:type "array" :default ["order" "degree" "count"] :doc "order, degree and count fields")
           :kindAs (:type "string" :default "eas_kind" :doc "field telling node, link and cell rows apart")
           :transform (:type "array" :doc "Vega-Lite transforms applied to the node rows first"))
 :fn #'eas-force-graph-transform)

(provide 'eas-force-graph)
;;; eas-force-graph.el ends here

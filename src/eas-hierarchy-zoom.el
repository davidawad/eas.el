;;; eas-hierarchy-zoom.el --- zoom into a hierarchy: subtree transform and action -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1 and L6.  Zooming a hierarchy layout (circle packing,
;; treemap, sunburst) into one node is laying out that node's subtree
;; alone: d3's layouts place each subtree independently of the rest,
;; so the subtree's layout is the whole layout's, scaled to fit.
;;
;;   {"x-eas:transform": "subtree", "root": KEY, "as": "level"}
;;
;; keeps the rows of the subtree under the row whose key is KEY (all
;; rows when KEY is null), makes that row the root and writes each
;; row's depth in the whole tree to "level", so colors by level do not
;; change as the view zooms.
;;
;; The "zoom-subtree" click action (eas-action.el) zooms a template's
;; view: it opens the view's template again with its own bindings and
;; slot "focus" set to the clicked node's key, slot "levels" to every
;; level of the whole tree (a stable color domain).  Clicking the node
;; already in focus zooms out to its parent.  The template names its key
;; and parent slots "key" and "parent" (defaults "id" and "parent").
;; The new view is shown like a drill's detail and its id returned.

;;; Code:

(require 'eas-core)
(require 'eas-hierarchy)
(require 'eas-view)
(require 'eas-action)
(require 'eas-action-drill)

(defun eas-hierarchy--subtree (rows params)
  "The subtree transform: ROWS under PARAMS's root key, with levels."
  (let* ((built (eas-hierarchy-stratify rows (plist-get params :key) (plist-get params :parentKey)))
         (kk (eas-key (plist-get params :key))) (pk (eas-key (plist-get params :parentKey)))
         (as (eas-key (plist-get params :as)))
         (want (eas-hierarchy--id (plist-get params :root)))
         (top (if want
                  (or (seq-find (lambda (n) (equal (eas-hierarchy--id (plist-get (eas-hierarchy-node-row n) kk)) want))
                                (cdr built))
                      (eas-signal "NOT_FOUND" (format "subtree: no row has %s %s" (plist-get params :key) want)
                                  :path "root"))
                (car built)))
         (keep (make-hash-table :test 'eq)))
    (eas-hierarchy-each-before top (lambda (n) (puthash n t keep)))
    (vconcat
     (delq nil
           (mapcar (lambda (n)
                     (when (gethash n keep)
                       (let ((row (eas-plist-put (eas-hierarchy-node-row n) as (eas-hierarchy-node-depth n))))
                         (if (eq n top) (eas-plist-put row pk :null) row))))
                   (cdr built))))))

(eas-register-transform
 "subtree"
 :doc "The rows of one node's subtree, that node made the root, each with its depth in the whole tree."
 :schema (append (seq-take eas-hierarchy-common-schema 4)
                 '(:root (:type "any" :doc "the key of the new root; null keeps the whole tree")
                   :as (:type "string" :default "level" :doc "the field holding each row's depth in the whole tree")))
 :fn #'eas-hierarchy--subtree)

;;; The zoom action

(defun eas-hierarchy--slot (bindings template slot default)
  "SLOT's value in BINDINGS, else TEMPLATE's default for it, else DEFAULT."
  (let ((v (plist-get bindings (eas-key slot))))
    (if (stringp v) v
      (let ((d (plist-get (plist-get (eas-template-slots template) (eas-key slot)) :default)))
        (if (stringp d) d default)))))

(defun eas-hierarchy-zoom (target view)
  "Zoom VIEW into the hierarchy node of click TARGET; return the new view id."
  (let* ((name (or (eas-view-template view)
                   (eas-signal "NOT_FOUND" "zoom-subtree needs a view opened from a template" :action "zoom-subtree")))
         (template (eas-template-get name))
         (bindings (copy-sequence (eas-view-bindings view)))
         (key (eas-hierarchy--slot bindings template "key" "id"))
         (parent (eas-hierarchy--slot bindings template "parent" "parent"))
         (data (let ((d (plist-get bindings :data))) (if (eas-data-p d) (plist-get d :rows) (vconcat d))))
         (built (eas-hierarchy-stratify data key parent))
         (row (plist-get target :row))
         (clicked (eas-hierarchy--id (plist-get row (eas-key key))))
         (focus (eas-hierarchy--id (plist-get bindings :focus)))
         (node (seq-find (lambda (n) (equal (eas-hierarchy--id (plist-get (eas-hierarchy-node-row n) (eas-key key))) clicked))
                         (cdr built)))
         (next (if (and focus (equal clicked focus))
                   (let ((p (and node (eas-hierarchy-node-parent node))))
                     (and p (eas-hierarchy-node-parent p)
                          (plist-get (eas-hierarchy-node-row p) (eas-key key))))
                 (plist-get row (eas-key key))))
         (deepest 0))
    (unless node
      (eas-signal "NOT_FOUND" (format "zoom-subtree: the clicked row has no %s in the tree" key) :action "zoom-subtree"))
    (seq-do (lambda (n) (setq deepest (max deepest (eas-hierarchy-node-depth n)))) (cdr built))
    (setq bindings (eas-plist-put (eas-plist-put bindings :focus (or next :null))
                                  :levels (vconcat (number-sequence 0 deepest))))
    (let ((zoomed (eas-view-open name :bindings bindings
                                 :id (format "%s>%s" (car (split-string (eas-view-id view) ">")) (or next "root"))
                                 :size (eas-view-size view) :target (eas-view-target view) :cell (eas-view-cell view))))
      (funcall eas-action-drill-show-function zoomed view)
      (eas-view-id zoomed))))

(eas-register-action
 "zoom-subtree" :fn #'eas-hierarchy-zoom
 :doc "Lay out the clicked node's subtree alone (zoom in); the node in focus zooms out to its parent.")

(provide 'eas-hierarchy-zoom)
;;; eas-hierarchy-zoom.el ends here

;;; eas-resolve.el --- resolve: template + bindings -> pure Vega-Lite -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L3, second half.  `eas-resolve' binds data to slots, fills
;; defaults, substitutes slot placeholders, materializes domain
;; transforms into columns, inlines the data and strips every x-eas
;; key.  The output is a complete, standalone Vega-Lite spec that
;; `bin/chart build' renders as-is.  It is pure and deterministic, and
;; `eas-resolve-hash' content-hashes it.
;;
;; An array element {"x-eas:each": SLOT, "spec": X} becomes one X per
;; item of array SLOT; inside X, {"x-eas:item": KEY [, "default": D]}
;; reads the item (KEY "." is the item itself) and a missing KEY with no
;; default drops the enclosing key or array element.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-template)
(require 'eas-transform-domain)

(defun eas-resolve--slot-value (values name path)
  "Return slot NAME's value from VALUES (data slots give their rows).
PATH locates the placeholder for failures."
  (let ((key (eas-key name)))
    (unless (plist-member values key)
      (eas-signal "SLOT_MISSING"
                    (format "Placeholder at %s names slot %s, which the template does not declare"
                            path name)
                    :slot name :path path))
    (let ((value (plist-get values key)))
      (if (eas-data-p value) (plist-get value :rows) value))))

(defvar eas-resolve--item nil
  "The array item an {\"x-eas:each\"} element is expanding, as (ITEM).")

(defconst eas-resolve--absent (make-symbol "absent")
  "An {\"x-eas:item\"} placeholder naming a key its item lacks.")

(defun eas-resolve--item-value (node values path)
  "Return the value of item placeholder NODE inside x-eas:each.
\"x-eas:item\" names a key of the item (\".\" is the item itself); a
missing key gives NODE's \"default\" (substituted with VALUES), else
`eas-resolve--absent', which drops the enclosing key.  PATH locates
NODE for failures."
  (unless eas-resolve--item
    (eas-signal "INVALID_INPUT" "x-eas:item is only meaningful inside an x-eas:each spec"
                  :path path))
  (let* ((item (car eas-resolve--item))
         (key (plist-get node :x-eas:item)))
    (cond
     ((equal key ".") item)
     ((and (eas-object-p item) (plist-member item (eas-key key)))
      (plist-get item (eas-key key)))
     ((plist-member node :default)
      (eas-resolve--substitute (plist-get node :default) values path))
     (t eas-resolve--absent))))

(defun eas-resolve--each (el values path)
  "Expand {\"x-eas:each\": SLOT, \"spec\": X}: one X per item of SLOT.
VALUES are the slot values; PATH locates EL."
  (let ((items (eas-resolve--slot-value values (plist-get el :x-eas:each) path)))
    (unless (vectorp items)
      (eas-signal "SLOT_TYPE" (format "x-eas:each at %s needs an array slot" path)
                    :slot (plist-get el :x-eas:each) :path path))
    (seq-map (lambda (item)
               (let ((eas-resolve--item (list item)))
                 (eas-resolve--substitute (plist-get el :spec) values path)))
             items)))

(defun eas-resolve--substitute (node values path)
  "Replace slot placeholders and named data in NODE using slot VALUES."
  (cond
   ((vectorp node)
    (let ((i -1) out)
      (seq-doseq (el node)
        (setq i (1+ i))
        (let ((epath (format "%s/%d" path i)))
          (cond
           ((and (eas-object-p el) (plist-get el :x-eas:when))
            (when (eas-true-p (eas-resolve--slot-value
                                 values (plist-get el :x-eas:when) epath))
              (push (eas-resolve--substitute (plist-get el :spec) values epath) out)))
           ((and (eas-object-p el) (plist-get el :x-eas:each))
            (dolist (spec (eas-resolve--each el values epath)) (push spec out)))
           (t (let ((value (eas-resolve--substitute el values epath)))
                (unless (eq value eas-resolve--absent) (push value out)))))))
      (vconcat (nreverse out))))
   ((and (eas-object-p node) node (plist-get node :x-eas:slot))
    (eas-resolve--slot-value values (plist-get node :x-eas:slot) path))
   ((and (eas-object-p node) node (plist-member node :x-eas:item))
    (eas-resolve--item-value node values path))
   ((and (eas-object-p node) node)
    (cl-loop for (key value) on node by #'cddr
             for kpath = (concat path "/" (eas-key-name key))
             for new = (if (and (eq key :data) (stringp (plist-get value :name))
                                (eas-data-p (plist-get values (eas-key (plist-get value :name)))))
                           (list :values (eas-resolve--slot-value
                                          values (plist-get value :name) kpath))
                         (eas-resolve--substitute value values kpath))
             unless (eq new eas-resolve--absent)
             append (list key new)))
   (t node)))

(defun eas-resolve--materialize (view cell path)
  "Run VIEW's domain transforms on its data, recursively; return the view.
CELL is a one-element list holding the nearest inherited rows (or nil).
Domain transforms must precede native transforms in an array."
  (let* ((own (plist-get (plist-get view :data) :values))
         (cell (if (vectorp own) (list own) cell))
         (native-seen nil) kept (i -1))
    (seq-doseq (tr (plist-get view :transform))
      (setq i (1+ i))
      (let ((tpath (format "%s/transform/%d" path i)))
        (cond
         ((not (plist-get tr :x-eas:transform))
          (setq native-seen t)
          (push tr kept))
         (native-seen
          (eas-signal "UNSUPPORTED_FEATURE"
                        "Domain transforms must come before native transforms; move it up"
                        :path tpath :feature "transform/x-eas-order"))
         ((null cell)
          (eas-signal "INVALID_INPUT"
                        "A domain transform needs inline data (data.values or a data slot) in scope"
                        :path tpath))
         (t (setcar cell (eas-transform-apply-domain tr (car cell) tpath))))))
    (let ((out view))
      (when (plist-member view :transform)
        (setq out (if kept
                      (eas-plist-put out :transform (vconcat (nreverse kept)))
                    (eas--plist-without out :transform))))
      (dolist (key '(:layer :vconcat :hconcat))
        (when (vectorp (plist-get view key))
          (let ((j -1))
            (setq out (eas-plist-put
                       out key
                       (vconcat (mapcar (lambda (child)
                                          (setq j (1+ j))
                                          (eas-resolve--materialize
                                           child cell (format "%s/%s/%d" path
                                                              (eas-key-name key) j)))
                                        (plist-get view key))))))))
      (when (vectorp own)
        (setq out (eas-plist-put out :data (eas-plist-put (plist-get view :data)
                                                              :values (car cell)))))
      out)))

(defun eas-resolve--strip (node)
  "Return NODE without x-eas keys, recursively."
  (cond
   ((vectorp node) (vconcat (mapcar #'eas-resolve--strip node)))
   ((and (eas-object-p node) node)
    (cl-loop for (key value) on node by #'cddr
             unless (string-prefix-p ":x-eas" (symbol-name key))
             append (list key (eas-resolve--strip value))))
   (t node)))

(defvar eas-facet-keep)

(defun eas-resolve-spec (spec &optional values)
  "Resolve chart/v1 SPEC to pure Vega-Lite using slot VALUES (a plist).
VALUES come from `eas-template-bind'; nil for a plain spec."
  (let* ((spec (let ((eas-facet-keep t)) (eas-spec-parse spec)))
         (body (eas-resolve--substitute spec values ""))
         (body (eas-resolve--materialize body nil ""))
         (body (eas-resolve--strip body)))
    (if (plist-get body :$schema)
        body
      (cons :$schema (cons eas-spec-schema-url body)))))

(defun eas-resolve (template bindings)
  "Resolve TEMPLATE (a name or template plist) with BINDINGS.
Return a complete, standalone Vega-Lite spec."
  (let ((template (if (stringp template) (eas-template-get template) template)))
    (eas-resolve-spec (plist-get template :spec)
                        (eas-template-bind template bindings))))

(defun eas-resolve-hash (resolved)
  "Return the content hash of the RESOLVED spec."
  (eas-content-hash resolved))

(provide 'eas-resolve)
;;; eas-resolve.el ends here

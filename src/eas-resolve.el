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
;;
;; {"x-eas:slot": NAME, "key": "a.b"} reads into an object (or array)
;; slot; a missing key gives the node's "default", else drops the key.
;; A slot whose value is null drops the key (or array element) it
;; fills, so a null default means "leave the property out".
;;
;; {"x-eas:expr": "datum.v > {{limit}}"} becomes a string with each
;; {{NAME}} (or {{NAME.key}}) replaced by the slot value as a JSON
;; literal, which is also a Vega expression literal; {{@KEY}} reads the
;; x-eas:each item ({{@}} is the item itself).  {"x-eas:text": ...}
;; inserts string values as they are, for titles and labels.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-template)
(require 'eas-transform-domain)
(require 'eas-font-file)

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

(defun eas-resolve--dig (value key)
  "VALUE's part at dotted KEY (\"a.b\", \"rows.0\"), or `eas-resolve--absent'."
  (let ((out value))
    (dolist (part (split-string key "\\." t))
      (setq out (cond ((eq out eas-resolve--absent) out)
                      ((and (vectorp out) (string-match-p "\\`[0-9]+\\'" part)
                            (< (string-to-number part) (length out)))
                       (aref out (string-to-number part)))
                      ((and (eas-object-p out) out (plist-member out (eas-key part)))
                       (plist-get out (eas-key part)))
                      (t eas-resolve--absent))))
    out))

(defun eas-resolve--slot-ref (node values path)
  "The value {\"x-eas:slot\": NAME [, \"key\": K, \"default\": D]} NODE stands for.
A missing key or a null value gives D (substituted with VALUES) when
NODE has one, else `eas-resolve--absent'.  PATH locates NODE."
  (let* ((value (eas-resolve--slot-value values (plist-get node :x-eas:slot) path))
         (key (plist-get node :key))
         (value (if (stringp key) (eas-resolve--dig value key) value)))
    (cond ((not (memq value (list eas-resolve--absent :null))) value)
          ((plist-member node :default)
           (eas-resolve--substitute (plist-get node :default) values path))
          (t eas-resolve--absent))))

(defvar eas-resolve--item nil
  "The items the {\"x-eas:each\"} elements are expanding, innermost first.")

(defconst eas-resolve--absent (make-symbol "absent")
  "An {\"x-eas:item\"} placeholder naming a key its item lacks.")

(defun eas-resolve--item-value (node values path)
  "Return the value of item placeholder NODE inside x-eas:each.
\"x-eas:item\" names a key of the item (\".\" is the item itself); a
missing key gives NODE's \"default\" (substituted with VALUES), else
`eas-resolve--absent', which drops the enclosing key.  Each leading
\"../\" reads the item of the next enclosing each instead.  PATH
locates NODE for failures."
  (let* ((key (plist-get node :x-eas:item))
         (up 0))
    (while (and (stringp key) (string-prefix-p "../" key))
      (setq key (substring key 3) up (1+ up))
      (when (equal key "") (setq key ".")))
    (unless (nthcdr up eas-resolve--item)
      (eas-signal "INVALID_INPUT"
                  (if (= up 0) "x-eas:item is only meaningful inside an x-eas:each spec"
                    (format "x-eas:item %s reaches past the outermost x-eas:each"
                            (plist-get node :x-eas:item)))
                  :path path))
    (eas-resolve--item-lookup (nth up eas-resolve--item) key node values path)))

(defun eas-resolve--item-lookup (item key node values path)
  "ITEM's KEY for item placeholder NODE; see `eas-resolve--item-value'.
VALUES and PATH substitute NODE's default."
  (cond
   ((equal key ".") item)
   ((and (eas-object-p item) (plist-member item (eas-key key)))
    (plist-get item (eas-key key)))
   ((plist-member node :default)
    (eas-resolve--substitute (plist-get node :default) values path))
   (t eas-resolve--absent)))

(defun eas-resolve--each (el values path)
  "Expand {\"x-eas:each\": SLOT, \"spec\": X}: one X per item of SLOT.
SLOT may instead be an {\"x-eas:item\": KEY} placeholder, which nests:
the array is KEY of the enclosing each's item.  VALUES are the slot
values; PATH locates EL."
  (let* ((source (plist-get el :x-eas:each))
         (items (if (stringp source) (eas-resolve--slot-value values source path)
                  (eas-resolve--substitute source values path)))
         (items (if (or (eq items eas-resolve--absent) (eq items :null)) [] items)))
    (unless (vectorp items)
      (eas-signal "SLOT_TYPE" (format "x-eas:each at %s needs an array slot" path)
                    :slot (plist-get el :x-eas:each) :path path))
    (seq-map (lambda (item)
               (let ((eas-resolve--item (cons item eas-resolve--item)))
                 (eas-resolve--substitute (plist-get el :spec) values path)))
             items)))

(defun eas-resolve--template-string (template values path raw)
  "TEMPLATE with each {{REF}} replaced by its value from slot VALUES.
REF is NAME, NAME.KEY or @KEY (the x-eas:each item; @ alone is the
item).  Values are JSON literals, but RAW inserts strings as they are.
PATH locates the template for failures."
  (unless (stringp template)
    (eas-signal "INVALID_INPUT" "x-eas:expr and x-eas:text take a string" :path path))
  (replace-regexp-in-string
   "{{\\s-*\\([^}]+?\\)\\s-*}}"
   (lambda (match)
     (let* ((ref (match-string 1 match))
            (value (if (string-prefix-p "@" ref)
                       (eas-resolve--item-value
                        (list :x-eas:item (if (equal ref "@") "." (substring ref 1))) values path)
                     (let ((dot (string-search "." ref)))
                       (eas-resolve--slot-ref
                        (append (list :x-eas:slot (if dot (substring ref 0 dot) ref))
                                (and dot (list :key (substring ref (1+ dot)))))
                        values path)))))
       (when (eq value eas-resolve--absent)
         (eas-signal "SLOT_MISSING" (format "{{%s}} at %s has no value" ref path)
                     :path path :ref ref))
       (cond ((and raw (stringp value)) value)
             ((and raw (eq value :null)) "")
             (t (eas-json-encode value)))))
   template t t))

(defun eas-resolve--substitute (node values path)
  "Replace slot placeholders and named data in NODE using slot VALUES.
PATH is NODE's JSON pointer, for findings."
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
    (eas-resolve--slot-ref node values path))
   ((and (eas-object-p node) node (plist-member node :x-eas:expr))
    (eas-resolve--template-string (plist-get node :x-eas:expr) values path nil))
   ((and (eas-object-p node) node (plist-member node :x-eas:text))
    (eas-resolve--template-string (plist-get node :x-eas:text) values path t))
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
Domain transforms must precede native transforms in an array.  PATH
is VIEW's JSON pointer, for findings."
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
         ;; Font files register here, so pure Vega-Lite keeps only family names.
         (_ (eas-font-file-register-spec body))
         (body (eas-resolve--materialize body nil ""))
         (body (eas-resolve--strip body)))
    (if (plist-get body :$schema)
        body
      (cons :$schema (cons eas-spec-schema-url body)))))

(defun eas-resolve (template bindings)
  "Resolve TEMPLATE (a name or template plist) with BINDINGS.
Return a complete, standalone Vega-Lite spec.  Relative x-eas.fonts
files are found beside the template's file."
  (let* ((template (if (stringp template) (eas-template-get template) template))
         (path (plist-get template :path))
         (eas-font-file-directory (if path (file-name-directory path) eas-font-file-directory)))
    (eas-resolve-spec (plist-get template :spec)
                        (eas-template-bind template bindings))))

(defun eas-resolve-hash (resolved)
  "Return the content hash of the RESOLVED spec."
  (eas-content-hash resolved))

(provide 'eas-resolve)
;;; eas-resolve.el ends here

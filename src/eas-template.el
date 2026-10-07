;;; eas-template.el --- template/v1: chart/v1 specs with typed slots -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L3, first half.  A template is a chart/v1 spec whose "x-eas" key
;; carries template, version, doc, slots and example.  A slot is one of
;;
;;   {"shape": ADAPTER}            data, converted by the named adapter
;;   {"type": T [, "items": T2]}   string number integer boolean array
;;                                 object, or "field" (a column name)
;;   {"enum": [V ...]}             one of the listed values
;;
;; plus optional "required", "default", "doc" and, for field slots,
;; "of": the data slot whose columns it names.
;;
;; In the spec body {"x-eas:slot": NAME} is replaced by the slot's
;; value and an array element {"x-eas:when": NAME, "spec": X} is kept
;; (as X) only when the slot is truthy.  {"name": NAME} data refers to a
;; data slot.  Templates are JSON files in `eas-template-directories',
;; loaded on first use.  A file that fails to load is skipped and
;; reported (`eas-template-load-errors'); the others still load.
;;
;; Names share one registry.  A package keeps its own names apart with
;; a namespace: `eas-template-add-directory' with NAMESPACE (or
;; "x-eas.namespace" in the template) registers "NAMESPACE/NAME".  A
;; bare NAME still finds a namespaced template when only one has it.

;;; Code:

(require 'eas-core)
(require 'eas-spec)
(require 'eas-data)

(defconst eas-template--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "The repository root (one level above src/).")

(defvar eas-template-directories
  (list (expand-file-name "templates" eas-template--root)
        (expand-file-name "templates/vega" eas-template--root))
  "Directories whose *.json files are eas templates.
templates/vega holds the Vega example gallery's templates.")

(defvar eas-template-namespaces
  (list (cons (file-name-as-directory (expand-file-name "templates/vega" eas-template--root))
              "vega"))
  "Alist of (DIRECTORY . NAMESPACE) for `eas-template-directories'.
A template loaded from DIRECTORY registers as \"NAMESPACE/NAME\".
The Vega gallery's templates (templates/vega) register as \"vega/NAME\".")

(defvar eas--templates nil
  "Loaded templates: alist of (NAME . PLIST) with :spec :meta :path.
nil until the first lookup loads `eas-template-directories'.")

(defvar eas-template-load-errors nil
  "Templates the last `eas-template-reload' skipped, as failure plists.
Each is (:code CODE :message M :file FILE ...).")

(defun eas-template-add-directory (directory &optional namespace)
  "Load templates from DIRECTORY too, under NAMESPACE when non-nil.
A domain package calls this once; its templates register as
\"NAMESPACE/NAME\" so they cannot collide with another package's."
  (let ((dir (file-name-as-directory (expand-file-name directory))))
    (unless (member dir (mapcar #'file-name-as-directory eas-template-directories))
      (setq eas-template-directories (append eas-template-directories (list dir))))
    (setf (alist-get dir eas-template-namespaces nil t #'equal) namespace)
    (setq eas--templates nil)
    dir))

(defun eas-template--namespace (meta path)
  "The namespace of the template with x-eas META read from PATH, or nil."
  (or (plist-get meta :namespace)
      (and path (alist-get (file-name-directory (expand-file-name path))
                           eas-template-namespaces nil nil #'equal))))

(defun eas-template--qualified (meta path)
  "The registry name of the template with x-eas META read from PATH."
  (let ((name (plist-get meta :template))
        (ns (eas-template--namespace meta path)))
    (if (and (stringp ns) (not (string-prefix-p (concat ns "/") name)))
        (concat ns "/" name)
      name)))

(defun eas-template--meta-check (meta path)
  "Signal INVALID_INPUT unless META (an x-eas object) declares a template.
PATH names the template in the error."
  (unless (eas-object-p meta)
    (eas-signal "INVALID_INPUT" (format "Template %s needs an x-eas object" path)
                :path "/x-eas" :file path))
  (dolist (key '(:template :version :slots))
    (unless (plist-get meta key)
      (eas-signal "INVALID_INPUT"
                    (format "Template %s needs x-eas.%s" path (eas-key-name key))
                    :path (concat "/x-eas/" (eas-key-name key)) :file path)))
  (cl-loop for (slot def) on (plist-get meta :slots) by #'cddr
           unless (or (plist-get def :shape) (plist-get def :type) (plist-get def :enum))
           do (eas-signal "INVALID_INPUT"
                            (format "Slot %s needs a shape, type or enum" (eas-key-name slot))
                            :path (concat "/x-eas/slots/" (eas-key-name slot)) :file path)))

(defun eas-template-register (spec &optional path)
  "Register template SPEC (a parsed chart/v1 value) read from PATH.
Return its registry name, namespaced when its directory or x-eas
declares a namespace."
  (let* ((spec (eas-spec-validate spec))
         (meta (plist-get spec :x-eas)))
    (eas-template--meta-check meta (or path "<inline>"))
    (unless (stringp (plist-get meta :template))
      (eas-signal "INVALID_INPUT" (format "Template %s: x-eas.template must be a string"
                                          (or path "<inline>"))
                  :path "/x-eas/template" :file path))
    (let ((name (eas-template--qualified meta path)))
      (setf (alist-get name eas--templates nil nil #'equal)
            (list :name name :spec spec :meta meta :path path))
      name)))

(defun eas-template-load (file)
  "Load and register the template in FILE; return its name."
  (eas-template-register (eas-json-read-file file) (expand-file-name file)))

(defun eas-template--load-file (file)
  "Load FILE for `eas-template-reload'; on failure record it and return nil.
A second file declaring a name already loaded is a failure too."
  (condition-case err
      (let* ((spec (eas-json-read-file file))
             (meta (and (eas-object-p spec) (plist-get spec :x-eas)))
             (name (and (eas-object-p meta) (stringp (plist-get meta :template))
                        (eas-template--qualified meta file)))
             (other (and name (alist-get name eas--templates nil nil #'equal))))
        (if other
            (progn
              (push (list :code "INVALID_INPUT"
                          :message (format "Template %s in %s is already defined by %s; skipped"
                                           name file (plist-get other :path))
                          :file file :template name)
                    eas-template-load-errors)
              nil)
          (eas-template-register spec file)))
    (error
     (push (append (eas-error-plist err) (list :file file)) eas-template-load-errors)
     nil)))

(defun eas-template-reload ()
  "Forget loaded templates and load every directory again.
A template that fails to load is skipped and recorded in
`eas-template-load-errors'; return the loaded names."
  (setq eas--templates nil eas-template-load-errors nil)
  (dolist (dir eas-template-directories)
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "\\.json\\'"))
        (eas-template--load-file (expand-file-name file)))))
  (setq eas-template-load-errors (nreverse eas-template-load-errors))
  (mapcar #'car eas--templates))

(defun eas-template-names ()
  "Return the sorted names of every template."
  (unless eas--templates (eas-template-reload))
  (sort (mapcar #'car eas--templates) #'string<))

(defun eas-template--unqualified (name)
  "Templates whose name is \"NAMESPACE/NAME\" for a bare NAME."
  (and (stringp name) (not (string-search "/" name))
       (seq-filter (lambda (entry) (string-suffix-p (concat "/" name) (car entry)))
                   eas--templates)))

(defun eas-template-get (name)
  "Return the template plist NAME or signal NOT_FOUND.
A bare NAME finds \"NAMESPACE/NAME\" when exactly one namespace has it."
  (unless eas--templates (eas-template-reload))
  (or (alist-get name eas--templates nil nil #'equal)
      (let ((matches (eas-template--unqualified name)))
        (cond ((= (length matches) 1) (cdar matches))
              (matches
               (eas-signal "NOT_FOUND"
                           (format "Template %S is ambiguous; use one of %s" name
                                   (string-join (sort (mapcar #'car matches) #'string<) ", "))
                           :template name))))
      (eas-signal "NOT_FOUND"
                    (format "No template %S; templates: %s" name
                            (string-join (eas-template-names) ", "))
                    :template name)))

(defun eas-template-p (name)
  "Non-nil when a template is registered as NAME, bare or namespaced."
  (and (stringp name)
       (or (member name (eas-template-names))
           (= (length (eas-template--unqualified name)) 1))
       t))

(defun eas-template-slots (template)
  "Return TEMPLATE's slots plist."
  (plist-get (plist-get template :meta) :slots))

(defun eas-template-example-file (template)
  "Return the absolute file of TEMPLATE's example bindings, or nil."
  (when-let* ((example (plist-get (plist-get template :meta) :example))
              (path (plist-get template :path)))
    (expand-file-name example (file-name-directory
                               (directory-file-name (file-name-directory path))))))

(defun eas-template-read-bindings (file)
  "Read the bindings object in FILE.
A slot bound to {\"file\": F} with a relative F reads F beside FILE,
so an example can name a dataset wherever Emacs runs."
  (let ((bindings (eas-json-read-file file)))
    (if (not (eas-object-p bindings)) bindings
      (cl-loop for (slot value) on bindings by #'cddr
               append (list slot
                            (let ((f (and (eas-object-p value) (plist-get value :file))))
                              (if (and (stringp f) (not (file-name-absolute-p f)))
                                  (plist-put (copy-sequence value) :file
                                             (expand-file-name f (file-name-directory file)))
                                value)))))))

(defun eas-template-example (name)
  "Return the example bindings of template NAME (they render as-is)."
  (let* ((template (eas-template-get name))
         (file (eas-template-example-file template)))
    (unless file
      (eas-signal "NOT_FOUND" (format "Template %s declares no example" name)
                    :template name))
    (eas-template-read-bindings file)))

(defun eas-template-describe (name)
  "Return the describe plist for template NAME."
  (let* ((template (eas-template-get name))
         (meta (plist-get template :meta)))
    (list :name (plist-get template :name) :version (plist-get meta :version)
          :doc (plist-get meta :doc)
          :slots (plist-get meta :slots)
          :example (eas-template-example-file template)
          :path (plist-get template :path))))

;;; Binding

(defun eas-template--type-ok (type value)
  "Non-nil when VALUE fits slot TYPE."
  (pcase type
    ("field" (stringp value))
    ("array" (vectorp value))
    (_ (eas-json-type-p type value))))

(defun eas-template--check-value (slot def value)
  "Signal SLOT_TYPE unless VALUE fits slot SLOT's DEF; return VALUE."
  (let ((type (plist-get def :type))
        (enum (plist-get def :enum))
        (name (eas-key-name slot)))
    (when (and enum (not (seq-contains-p enum value)))
      (eas-signal "SLOT_TYPE"
                    (format "Slot %s must be one of %s" name (eas-json-encode enum))
                    :slot name :expected enum))
    (when (and type (not (eas-template--type-ok type value)))
      (eas-signal "SLOT_TYPE" (format "Slot %s must be a %s" name type)
                    :slot name :expected type))
    (when-let* ((items (and (equal type "array") (plist-get def :items))))
      (seq-do-indexed
       (lambda (item i)
         (unless (eas-template--type-ok items item)
           (eas-signal "SLOT_TYPE"
                         (format "Slot %s item %d must be a %s" name i items)
                         :slot name :index i :expected items)))
       value))
    value))

(defun eas-template--data-value (slot def value)
  "Convert data slot SLOT's VALUE through its DEF :shape adapter."
  (condition-case err
      (eas-data-from (plist-get def :shape) value)
    (eas-error
     (signal (car err) (append (cdr err) (list :slot (eas-key-name slot)))))))

(defun eas-template-bind (template bindings)
  "Check BINDINGS against TEMPLATE's slots and fill defaults.
BINDINGS is a plist (or parsed JSON object) keyed by slot.  Data
slots come back as data/v1.  Signals SLOT_MISSING, SLOT_TYPE,
SHAPE_INVALID, FIELD_MISSING or INVALID_INPUT naming the slot."
  (let* ((slots (eas-template-slots template))
         (bindings (if (vectorp bindings) (eas-signal "INVALID_INPUT"
                                                        "Bindings must be an object keyed by slot")
                     bindings))
         bound)
    (dolist (key (eas-plist-keys bindings))
      (unless (plist-member slots key)
        (eas-signal "INVALID_INPUT"
                      (format "Template %s has no slot %s; slots: %s"
                              (plist-get template :name) (eas-key-name key)
                              (mapconcat #'eas-key-name (eas-plist-keys slots) ", "))
                      :slot (eas-key-name key))))
    (cl-loop for (slot def) on slots by #'cddr
             for given = (plist-member bindings slot)
             for value = (if given (plist-get bindings slot) (plist-get def :default))
             do (cond
                 ((and (not given) (not (plist-member def :default)))
                  (when (eas-true-p (plist-get def :required))
                    (eas-signal "SLOT_MISSING"
                                  (format "Template %s needs slot %s%s" (plist-get template :name)
                                          (eas-key-name slot)
                                          (if (plist-get def :doc)
                                              (concat ": " (plist-get def :doc)) ""))
                                  :slot (eas-key-name slot))))
                 ;; Null fits any slot, a data slot too: the property it
                 ;; fills is left out, and a layer {"x-eas:when"} it names.
                 ((eq value :null) (push (cons slot :null) bound))
                 ((plist-get def :shape)
                  (push (cons slot (eas-template--data-value slot def value)) bound))
                 (t (push (cons slot (eas-template--check-value
                                      slot def (if (and (listp value) (equal (plist-get def :type) "array"))
                                                   (vconcat value) value)))
                          bound))))
    (eas-template--check-fields slots bound)
    (cl-loop for (slot . value) in (nreverse bound) append (list slot value))))

(defun eas-template--check-fields (slots bound)
  "Signal FIELD_MISSING when a field slot in SLOTS names no column.
BOUND is an alist of slot values."
  (cl-loop for (slot def) on slots by #'cddr
           for of = (plist-get def :of)
           for data = (and of (alist-get (eas-key of) bound))
           for field = (alist-get slot bound)
           when (and (equal (plist-get def :type) "field") data (stringp field)
                     (not (eas-data-field-type data field)))
           do (eas-signal "FIELD_MISSING"
                            (format "Slot %s names field %s, which %s does not have; columns: %s"
                                    (eas-key-name slot) field of
                                    (mapconcat (lambda (c) (plist-get c :name))
                                               (plist-get data :schema) ", "))
                            :slot (eas-key-name slot) :field field)))

(provide 'eas-template)
;;; eas-template.el ends here

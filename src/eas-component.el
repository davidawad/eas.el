;;; eas-component.el --- readout components: registered renderers with props -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (eas-anj).  The hover readout and textual tooltips are
;; trees of components, like React's: a component is a named,
;; registered renderer that takes props (validated against its schema)
;; and a context (the datum, the view, the theme) and returns atoms.
;; A chart references components from JSON:
;;
;;   {"component": "row", "props": {"sep": "  "}, "children": [
;;     {"component": "field", "props": {"field": "close", "label": "C",
;;                                      "format": {"type": "number", "decimals": 2}}},
;;     {"component": "when", "props": {"test": "datum.close >= datum.open",
;;                                     "style": {"color": "green"}},
;;      "children": [{"component": "badge", "props": {"text": "UP"}}],
;;      "else": [{"component": "badge", "props": {"text": "DOWN", "background": "red"}}]}]}
;;
;; A prop may be {"expr": E}, a Vega expression on the datum.  An atom
;; is the unit the fit measures, drops and abbreviates:
;;
;;   (:variants (SPANS SHORT SHORTER) :priority P :keep K :sep SPANS :role R)
;;
;; SPANS is a list of (TEXT . STYLE), STYLE a plist of :color
;; :background :bold :italic :dim :underline.  `eas-component-fit'
;; lays atoms out on at most N lines of W columns: first it drops the
;; lowest-priority atoms, then abbreviates labels, then shortens
;; numbers, then elides with an ellipsis.  The same spans become
;; propertized text (`eas-component-propertize') or SVG tspans
;; (`eas-component-svg-tspans'), so the two backends agree.
;;
;; Packages add components with `eas-define-component'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eas-core)
(require 'eas-expr)
(require 'eas-format)
(require 'eas-time)
(require 'xml)

(defvar eas-component--registry (make-hash-table :test 'equal)
  "Component name (a string) -> definition plist (:props :render :doc :children).")

(defconst eas-component-prop-types
  '(string number integer boolean color style format expr array rules any)
  "Types a component prop may declare.")

(defconst eas-component--node-keys '(:component :props :children :else)
  "Keys of a component node.")

;;; Definition

(defun eas-component--prop-entry (entry)
  "ENTRY of a props schema, normalized to (KEY . PLIST) with a keyword KEY."
  (let* ((key (car entry))
         (key (cond ((keywordp key) key)
                    ((symbolp key) (intern (concat ":" (symbol-name key))))
                    ((stringp key) (eas-key key))
                    (t (error "A prop name is a symbol, keyword or string: %S" key))))
         (plist (cdr entry)))
    (unless (memq (or (plist-get plist :type) 'any) eas-component-prop-types)
      (error "Prop %s: :type is one of %S" key eas-component-prop-types))
    (cons key plist)))

(cl-defun eas-define-component (name &key props render doc children)
  "Register the readout component NAME (a string) and return NAME.
PROPS is the props schema: a list of (KEY :type TYPE :default D
:required R :enum VALUES :doc STRING), KEY a symbol or keyword, TYPE
one of `eas-component-prop-types'.  RENDER is a function of PROPS
\(validated, defaults applied, expressions evaluated) and CTX that
returns a list of atoms (`eas-component-atom'); CTX is a plist with
:datum :view :theme :env :children :given (the props as written)
and :path.  CHILDREN
non-nil means the node may have children, rendered with
`eas-component-render-children'.  DOC describes the component.
Defining NAME again replaces it."
  (unless (and (stringp name) (string-match-p "\\`[a-zA-Z][a-zA-Z0-9_.-]*\\'" name))
    (error "A component name is a string of letters, digits, _ . -: %S" name))
  (unless (functionp render) (error "Component %s: :render must be a function" name))
  (puthash name (list :props (mapcar #'eas-component--prop-entry props)
                      :render render :doc doc :children children)
           eas-component--registry)
  name)

(defun eas-component-get (name)
  "The definition of component NAME, or nil."
  (gethash name eas-component--registry))

(defun eas-component-names ()
  "Names of every registered component, sorted."
  (sort (hash-table-keys eas-component--registry) #'string<))

;;; Atoms and spans

(defun eas-component-span (text &optional style)
  "A span: TEXT (any value, printed) with STYLE."
  (cons (cond ((stringp text) text) ((memq text '(nil :null)) "") (t (format "%s" text))) style))

(cl-defun eas-component-atom (spans &key short shorter (priority 50) keep role)
  "Return an atom of SPANS: the unit the fit drops whole.
SHORT is SPANS with abbreviated labels, SHORTER with shortened numbers
too.  PRIORITY ranks atoms: the lowest is dropped first.  KEEP non-nil
means never drop.  ROLE `sep' marks a separator, dropped when it would
start or end a line or sit next to another."
  (list :variants (list spans (or short spans) (or shorter short spans))
        :priority priority :keep keep :role role))

(defun eas-component--spans-width (spans)
  "Columns SPANS take."
  (cl-loop for s in spans sum (string-width (car s))))

(defun eas-component--empty-p (atom)
  "Non-nil when ATOM draws nothing."
  (zerop (eas-component--spans-width (car (plist-get atom :variants)))))

(defun eas-component--merge-style (over under)
  "Style OVER on top of style UNDER."
  (let ((out (copy-sequence under)))
    (cl-loop for (k v) on over by #'cddr do (setq out (plist-put out k v)))
    out))

(defun eas-component-restyle (atoms style)
  "ATOMS with STYLE laid over every span's own style."
  (if (null style) atoms
    (mapcar (lambda (atom)
              (let ((a (copy-sequence atom)))
                (plist-put a :variants
                           (mapcar (lambda (spans)
                                     (mapcar (lambda (s) (cons (car s) (eas-component--merge-style style (cdr s))))
                                             spans))
                                   (plist-get atom :variants)))))
            atoms)))

;;; Props

(defun eas-component--invalid (path message &rest props)
  "Signal INVALID_INPUT at PATH with MESSAGE and PROPS."
  (apply #'eas-signal "INVALID_INPUT" message :path path props))

(defun eas-component--expr-p (value)
  "Non-nil when VALUE is an {\"expr\": E} object."
  (and (eas-object-p value) (stringp (plist-get value :expr)) (= (length value) 2)))

(defun eas-component--eval (expr ctx path)
  "Evaluate expression string EXPR on CTX's datum; errors name PATH."
  (condition-case err
      (eas-expr-evaluate expr (plist-get ctx :datum) (plist-get ctx :env))
    (eas-error (eas-component--invalid path (format "Expression %S: %s" expr
                                                     (plist-get (eas-error-plist err) :message))))
    (error (eas-component--invalid path (format "Expression %S: %s" expr (error-message-string err))))))

(defconst eas-component--style-keys '(:color :background :bold :italic :dim :underline)
  "Keys a style object may have.")

(defun eas-component--type-ok (type value)
  "Non-nil when VALUE has prop TYPE."
  (pcase type
    ('any t)
    ((or 'string 'color 'expr) (stringp value))
    ('number (numberp value))
    ('integer (integerp value))
    ('boolean (memq value '(t nil :false :json-false)))
    ('style (and (eas-object-p value)
                 (cl-loop for (k _) on value by #'cddr always (memq k eas-component--style-keys))))
    ('format (or (stringp value) (eas-object-p value)))
    ('array (or (vectorp value) (and (listp value) (not (eas-object-p value)))))
    ('rules (and (or (vectorp value) (listp value)) (seq-every-p #'eas-object-p value)))))

(defun eas-component--bool (value)
  "VALUE as a Lisp boolean."
  (not (memq value '(nil :false :json-false :null))))

(defun eas-component--check-prop (name key spec value path)
  "Check VALUE of prop KEY (schema SPEC) of component NAME at PATH."
  (let ((type (or (plist-get spec :type) 'any)) (enum (plist-get spec :enum)))
    (unless (eas-component--type-ok type value)
      (eas-component--invalid path (format "Component %s: prop %s must be %s, got %S"
                                           name (eas-key-name key)
                                           (if (eq type 'style)
                                               (format "a style object with keys %s"
                                                       (mapconcat #'eas-key-name eas-component--style-keys ", "))
                                             (format "of type %s" type))
                                           value)
                              :component name :prop (eas-key-name key)))
    (when (and enum (not (member value enum)))
      (eas-component--invalid path (format "Component %s: prop %s is one of %s, got %S"
                                           name (eas-key-name key) (mapconcat (lambda (e) (format "%s" e)) enum ", ")
                                           value)
                              :component name :prop (eas-key-name key)))))

(defun eas-component--props (name def props ctx path)
  "PROPS of component NAME (definition DEF) at PATH, ready to render.
Unknown props are errors; {\"expr\": E} values are evaluated on CTX's
datum (unless CTX is nil: validation only); defaults fill the rest."
  (let ((schema (plist-get def :props)) out)
    (unless (or (null props) (eas-object-p props))
      (eas-component--invalid path (format "Component %s: props is an object" name)))
    (cl-loop for (k v) on props by #'cddr
             for spec = (assq k schema)
             for ppath = (format "%s/%s" path (eas-key-name k))
             do (unless spec
                  (eas-component--invalid ppath (format "Component %s has no prop %s; props: %s" name (eas-key-name k)
                                                        (mapconcat (lambda (e) (eas-key-name (car e))) schema ", "))
                                          :component name :prop (eas-key-name k)))
             do (if (eas-component--expr-p v)
                    (if ctx
                        (let ((got (eas-component--eval (plist-get v :expr) ctx ppath)))
                          (unless (memq got '(nil :null)) (eas-component--check-prop name k (cdr spec) got ppath))
                          (setq out (plist-put out k got)))
                      (condition-case err (eas-expr-parse (plist-get v :expr))
                        (error (eas-component--invalid ppath (format "Expression %S: %s" (plist-get v :expr)
                                                                     (if (eq (car err) 'error) (cadr err)
                                                                       (plist-get (eas-error-plist err) :message)))))))
                  (eas-component--check-prop name k (cdr spec) v ppath)
                  (when ctx (setq out (plist-put out k v)))))
    (dolist (entry schema)
      (unless (plist-member props (car entry))
        (when (plist-get (cdr entry) :required)
          (eas-component--invalid (format "%s/%s" path (eas-key-name (car entry)))
                                  (format "Component %s needs prop %s" name (eas-key-name (car entry)))
                                  :component name :prop (eas-key-name (car entry))))
        (when ctx (setq out (plist-put out (car entry) (plist-get (cdr entry) :default))))))
    out))

;;; Nodes

(defun eas-component-normalize (node)
  "NODE as a (:component NAME :props P :children C :else E) plist.
A string is a text node; an object with \"field\" or \"value\" and no
\"component\" is a field node; one with \"fields\" is a row of them
\(\"separator\" its sep)."
  (cond ((stringp node) (list :component "text" :props (list :text node)))
        ((not (eas-object-p node)) node)
        ((plist-get node :component) node)
        ((plist-member node :fields)
         (list :component "row"
               :props (and (plist-get node :separator) (list :sep (plist-get node :separator)))
               :children (plist-get node :fields)))
        ((or (plist-member node :field) (plist-member node :value))
         (list :component "field" :props node))
        (t node)))

(defun eas-component--children-list (value)
  "VALUE (a node, a list or a vector of nodes) as a list of nodes."
  (cond ((null value) nil)
        ((vectorp value) (append value nil))
        ((or (stringp value) (eas-object-p value)) (list value))
        (t value)))

(defun eas-component--node (node path)
  "Check NODE's shape at PATH and return it normalized with its definition."
  (let ((n (eas-component-normalize node)))
    (unless (eas-object-p n)
      (eas-component--invalid path (format "A readout component is an object {\"component\": NAME, ...} or a string, got %S"
                                           node)))
    (let* ((name (plist-get n :component)) (def (and (stringp name) (eas-component-get name))))
      (unless def
        (eas-component--invalid (concat path "/component")
                                (format "Unknown readout component %S; components: %s" name
                                        (string-join (eas-component-names) ", "))
                                :component name))
      (cl-loop for (k _) on n by #'cddr
               unless (memq k eas-component--node-keys)
               do (eas-component--invalid (format "%s/%s" path (eas-key-name k))
                                          (format "Component %s: unknown key %s; keys: component, props, children, else"
                                                  name (eas-key-name k))))
      (when (and (or (plist-get n :children) (plist-get n :else)) (not (plist-get def :children)))
        (eas-component--invalid (concat path "/children") (format "Component %s takes no children" name)))
      (cons n def))))

(defun eas-component-validate (node &optional path)
  "Check the component tree NODE; signal INVALID_INPUT at the first fault.
PATH (default \"\") prefixes the JSON paths in errors.  Return t."
  (let* ((path (or path "")) (got (eas-component--node node path)) (n (car got)))
    (eas-component--props (plist-get n :component) (cdr got) (plist-get n :props) nil (concat path "/props"))
    (cl-loop for child in (eas-component--children-list (plist-get n :children)) for i from 0
             do (eas-component-validate child (format "%s/children/%d" path i)))
    (cl-loop for child in (eas-component--children-list (plist-get n :else)) for i from 0
             do (eas-component-validate child (format "%s/else/%d" path i)))
    t))

(defun eas-component-render (node ctx)
  "Atoms of component NODE rendered in CTX (see `eas-define-component').
CTX's :path is NODE's JSON path."
  (let* ((path (or (plist-get ctx :path) ""))
         (got (eas-component--node node path)) (n (car got)) (def (cdr got))
         (props (eas-component--props (plist-get n :component) def (plist-get n :props) ctx (concat path "/props")))
         (ctx (append (list :children (eas-component--children-list (plist-get n :children))
                            :else (eas-component--children-list (plist-get n :else))
                            :given (plist-get n :props) :path path)
                      ctx)))
    (seq-remove #'eas-component--empty-p (funcall (plist-get def :render) props ctx))))

(defun eas-component-render-children (ctx &optional which)
  "Atoms of CTX's children (WHICH :else: its else branch), in order.
Return a list with one list of atoms per child."
  (cl-loop for child in (plist-get ctx (or which :children)) for i from 0
           collect (eas-component-render
                    child (append (list :path (format "%s/%s/%d" (plist-get ctx :path)
                                                      (if (eq which :else) "else" "children") i))
                                  ctx))))

;;; Fit

(defun eas-component--variant (atom level)
  "ATOM's spans at abbreviation LEVEL (0 full, 1 short, 2 shorter)."
  (nth level (plist-get atom :variants)))

(defun eas-component--clean (atoms)
  "ATOMS without separators at either end or next to another separator."
  (let (out)
    (dolist (a atoms)
      (unless (and (eq (plist-get a :role) 'sep) (or (null out) (eq (plist-get (car out) :role) 'sep)))
        (push a out)))
    (while (and out (eq (plist-get (car out) :role) 'sep)) (pop out))
    (nreverse out)))

(defun eas-component--wrap (atoms level width)
  "ATOMS at LEVEL wrapped greedily into lines of WIDTH: a list of span lists."
  (let (lines line (w 0))
    (dolist (a atoms)
      (let* ((spans (eas-component--variant a level))
             (sep (and line (plist-get a :sep)))
             (piece (append sep spans)) (pw (eas-component--spans-width piece)))
        (cond ((and line (> (+ w pw) width))
               (push (nreverse line) lines)
               (if (eq (plist-get a :role) 'sep) (setq line nil w 0)
                 (setq line (reverse spans) w (eas-component--spans-width spans))))
              ((and (null line) (eq (plist-get a :role) 'sep)))
              (t (setq line (append (reverse piece) line) w (+ w pw))))))
    (when line (push (nreverse line) lines))
    (nreverse lines)))

(defun eas-component--fits (lines width max-lines)
  "Non-nil when LINES are at most MAX-LINES, each within WIDTH."
  (and (<= (length lines) max-lines)
       (cl-every (lambda (l) (<= (eas-component--spans-width l) width)) lines)))

(defun eas-component-elide (spans width)
  "SPANS cut to WIDTH columns, ending in an ellipsis when cut."
  (if (<= (eas-component--spans-width spans) width) spans
    (let ((room (max 0 (1- width))) out last)
      (cl-loop for s in spans
               for sw = (string-width (car s))
               do (setq last (cdr s))
               if (<= sw room) do (push s out) (setq room (- room sw))
               else do (when (> room 0) (push (cons (truncate-string-to-width (car s) room) (cdr s)) out))
               and return nil)
      (nreverse (if (> width 0) (cons (cons "…" last) out) out)))))

(defun eas-component-fit (atoms width &optional max-lines)
  "Lay ATOMS out in at most MAX-LINES (default 1) lines of WIDTH columns.
Return exactly MAX-LINES span lists (empty ones pad).  Until the atoms
fit: drop the lowest-priority droppable atom (the last of equals
first; never a :keep one, never the last one), then abbreviate labels,
then shorten numbers, then elide the last line with an ellipsis."
  (let* ((max-lines (max 1 (or max-lines 1)))
         (atoms (eas-component--clean atoms))
         (order (sort (cl-loop for a in atoms for i from 0
                               unless (or (plist-get a :keep) (eq (plist-get a :role) 'sep))
                               collect (cons i a))
                      (lambda (x y) (let ((px (plist-get (cdr x) :priority)) (py (plist-get (cdr y) :priority)))
                                      (if (= px py) (> (car x) (car y)) (< px py))))))
         (alive atoms) (level 0)
         (lines (eas-component--wrap alive level width)))
    (while (and (not (eas-component--fits lines width max-lines)) order
                (> (cl-count-if-not (lambda (a) (eq (plist-get a :role) 'sep)) alive) 1))
      (setq alive (eas-component--clean (remq (cdr (pop order)) alive))
            lines (eas-component--wrap alive level width)))
    (while (and (not (eas-component--fits lines width max-lines)) (< level 2))
      (setq level (1+ level) lines (eas-component--wrap alive level width)))
    (unless (eas-component--fits lines width max-lines)
      (let ((head (cl-subseq lines 0 (min (length lines) (1- max-lines))))
            (tail (apply #'append (mapcar (lambda (l) (cons (eas-component-span " ") l))
                                          (nthcdr (1- max-lines) lines)))))
        (setq lines (append (mapcar (lambda (l) (eas-component-elide l width)) head)
                            (list (eas-component-elide (cdr tail) width))))))
    (append lines (make-list (- max-lines (length lines)) nil))))

;;; Backends

(defun eas-component--face (style)
  "Emacs face for span STYLE, or nil."
  (when style
    (let ((face (append (and (plist-get style :color) (list :foreground (plist-get style :color)))
                        (and (plist-get style :background) (list :background (plist-get style :background)))
                        (and (eas-component--bool (plist-get style :bold)) (list :weight 'bold))
                        (and (eas-component--bool (plist-get style :italic)) (list :slant 'italic))
                        (and (eas-component--bool (plist-get style :underline)) (list :underline t))
                        (and (eas-component--bool (plist-get style :dim)) (list :inherit 'shadow)))))
      (or (and (plist-get style :face) (if face (list face (plist-get style :face)) (plist-get style :face)))
          face))))

(defun eas-component-propertize (spans)
  "SPANS as one propertized string: the terminal backend."
  (mapconcat (lambda (s) (let ((face (eas-component--face (cdr s))))
                           (if face (propertize (car s) 'face face) (copy-sequence (car s)))))
             spans ""))

(defun eas-component--svg-color (color)
  "COLOR (a name or #hex, or an Emacs face symbol) as an SVG color."
  (cond ((null color) nil)
        ((symbolp color) (face-foreground color nil t))
        (t color)))

(defun eas-component-svg-tspans (spans)
  "SPANS as SVG <tspan> elements: the GUI backend."
  (mapconcat
   (lambda (s)
     (let* ((st (cdr s))
            (face (plist-get st :face))
            (fill (eas-component--svg-color (or (plist-get st :color) (and (symbolp face) face))))
            (attrs (concat (and fill (format " fill=\"%s\"" (xml-escape-string fill)))
                           (and (eas-component--bool (plist-get st :bold)) " font-weight=\"bold\"")
                           (and (eas-component--bool (plist-get st :italic)) " font-style=\"italic\"")
                           (and (eas-component--bool (plist-get st :underline)) " text-decoration=\"underline\"")
                           (and (eas-component--bool (plist-get st :dim)) " opacity=\"0.6\""))))
       (format "<tspan%s>%s</tspan>" attrs (xml-escape-string (car s)))))
   spans ""))

(defun eas-component-from-string (string)
  "Spans of propertized STRING: each run of one face is a span."
  (let ((pos 0) (len (length string)) spans)
    (while (< pos len)
      (let* ((next (next-single-property-change pos 'face string len))
             (face (get-text-property pos 'face string))
             (style (cond ((null face) nil)
                          ((symbolp face) (list :face face))
                          ((and (consp face) (keywordp (car face)))
                           (append (and (plist-get face :foreground) (list :color (plist-get face :foreground)))
                                   (and (plist-get face :background) (list :background (plist-get face :background)))
                                   (and (eq (plist-get face :weight) 'bold) (list :bold t))
                                   (and (eq (plist-get face :slant) 'italic) (list :italic t))
                                   (and (plist-get face :underline) (list :underline t))))
                          (t (list :face face)))))
        (push (cons (substring-no-properties string pos next) style) spans)
        (setq pos next)))
    (nreverse spans)))

(provide 'eas-component)
;;; eas-component.el ends here

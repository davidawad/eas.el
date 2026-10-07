;;; eas-component-builtins.el --- built-in readout components -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (eas-anj).  The components every chart can use:
;;
;;   text       a literal, or a {field} template on the datum
;;   field      label, value (a datum field or any value), format,
;;              priority, conditional rules (color, bold, italic,
;;              dim, hide)
;;   row        children side by side, `sep' between them
;;   when       children when test holds (with style laid over them),
;;              else the else branch, else (with a style) the
;;              children unstyled
;;   sep        an explicit separator
;;   badge      a short label on a background
;;   sparkline  an array of numbers as ▁▂▃▄▅▆▇█
;;   fields     the view's own fields (the values strip, or the hovered
;;              datum's tooltip) as field atoms, later ones dropped first
;;
;; Formats (prop "format"): a d3-format string, or {"type": "number"
;; | "percent" | "currency" | "time" | "si", "decimals": N, "symbol":
;; "$", "pattern": "%b %d, %Y"}.  Each has a shorter form the fit uses.

;;; Code:

(require 'eas-component)

;;; Formats

(defun eas-component--number-string (value)
  "Return VALUE (a number, or a numeric string) as a number, or nil."
  (cond ((numberp value) value)
        ((and (stringp value)
              (string-match-p "\\`-?[0-9][0-9,]*\\(?:\\.[0-9]+\\)?\\'" value))
         (string-to-number (replace-regexp-in-string "," "" value)))))

(defun eas-component--plain (value)
  "Return VALUE printed as an unformatted readout field."
  (cond ((memq value '(nil :null)) "null")
        ((stringp value) value)
        ((eq value t) "true") ((eq value :false) "false")
        ((and (floatp value) (= value (ftruncate value)) (< (abs value) 1e15)) (format "%d" (truncate value)))
        (t (format "%s" value))))

(defun eas-component--short-number (n)
  "Number N in at most three significant digits."
  (if (< (abs n) 1) (eas-format-number ".2~f" n) (eas-format-number ".3~s" n)))

(defun eas-component-format (value format &optional short)
  "VALUE as text per FORMAT (see the commentary), SHORT for the shorter form."
  (let ((n (eas-component--number-string value)))
    (cond
     ((memq value '(nil :null)) "null")
     ((null format)
      (if (and short n) (eas-component--short-number n) (eas-component--plain value)))
     ((stringp format)
      (cond ((not n) (eas-component--plain value))
            (short (eas-component--short-number n))
            (t (eas-format-number format n))))
     (t
      (let* ((type (or (plist-get format :type) "number"))
             (d (plist-get format :decimals)))
        (pcase type
          ("time"
           (let ((ms (eas-time-parse value)) (system-time-locale "C"))
             (if ms (eas-time-format ms (if short (or (plist-get format :short) "%b %d")
                                          (or (plist-get format :pattern) "%b %d, %Y")))
               (eas-component--plain value))))
          ((guard (not n)) (eas-component--plain value))
          ("percent" (eas-format-number (format ".%d%%" (if short 0 (or d 1))) n))
          ("currency"
           (let ((sym (or (plist-get format :symbol) "$")))
             (concat (if (< n 0) "-" "") sym
                     (if short (eas-component--short-number (abs n))
                       (eas-format-number (format ",.%df" (or d 2)) (abs n))))))
          ("si" (eas-format-number (format ".%d~s" (if short 2 (or d 3))) n))
          (_ (cond (short (eas-component--short-number n))
                   (d (eas-format-number (format ",.%df" d) n))
                   (t (eas-component--plain value))))))))))

(defun eas-component-abbreviate (label)
  "LABEL abbreviated: its first three characters."
  (if (> (length label) 4) (substring label 0 3) label))

;;; Rules

(defun eas-component--rules (rules ctx path)
  "The style RULES (at PATH) give on CTX's datum, or `hide'.
Every rule whose test holds lays its style over the earlier ones."
  (let (style)
    (cl-loop for rule in (append rules nil) for i from 0
             for rpath = (format "%s/%d" path i)
             for test = (plist-get rule :test)
             do (cl-loop for (k _) on rule by #'cddr
                         unless (memq k (append '(:test :hide) eas-component--style-keys))
                         do (eas-component--invalid
                             (format "%s/%s" rpath (eas-key-name k))
                             (format "Rule key %s unknown; keys: test, hide, %s" (eas-key-name k)
                                     (mapconcat #'eas-key-name eas-component--style-keys ", "))))
             when (or (null test)
                      (eas-expr-truthy (if (stringp test) (eas-component--eval test ctx (concat rpath "/test"))
                                         (eas-component--invalid (concat rpath "/test") "A rule's test is an expression string"))))
             do (if (eas-component--bool (plist-get rule :hide)) (setq style 'hide)
                  (unless (eq style 'hide)
                    (setq style (eas-component--merge-style (eas-component--plist-without-keys rule '(:test :hide))
                                                            style)))))
    style))

(defun eas-component--plist-without-keys (plist keys)
  "PLIST without KEYS."
  (cl-loop for (k v) on plist by #'cddr unless (memq k keys) append (list k v)))

;;; Built-ins

(defconst eas-component--common-props
  '((priority :type number :default 50 :doc "Higher stays longer when the line is short.")
    (keep :type boolean :doc "Never drop this atom.")
    (style :type style :doc "Color, background, bold, italic, dim, underline."))
  "Props most built-ins share.")

(defun eas-component--atom-of (spans props &optional short shorter)
  "An atom of SPANS (SHORT, SHORTER) with PROPS' priority, keep and style."
  (car (eas-component-restyle
        (list (eas-component-atom spans :short short :shorter shorter
                                  :priority (plist-get props :priority)
                                  :keep (eas-component--bool (plist-get props :keep))))
        (plist-get props :style))))

(defun eas-component--template (template datum)
  "TEMPLATE with each {name} replaced by DATUM's field name."
  (replace-regexp-in-string
   "{\\([^{}]+\\)}"
   (lambda (m) (eas-component--plain (plist-get datum (eas-key (substring m 1 -1)))))
   template t t))

(eas-define-component
 "text"
 :doc "A literal TEXT, or TEMPLATE with {field} filled from the datum."
 :props (append '((text :type any) (template :type string)
                  (short :type string :doc "Shorter text the fit may use."))
                eas-component--common-props)
 :render (lambda (props ctx)
           (let ((text (if (plist-get props :template)
                           (eas-component--template (plist-get props :template) (plist-get ctx :datum))
                         (let ((v (plist-get props :text))) (if (memq v '(nil :null)) "" (eas-component--plain v))))))
             (list (eas-component--atom-of (list (eas-component-span text)) props
                                           (and (plist-get props :short)
                                                (list (eas-component-span (plist-get props :short)))))))))

(defun eas-component--field-atom (label value props ctx)
  "A field atom: LABEL, then VALUE formatted per PROPS, in CTX.
Return nil when a rule hides it."
  (let* ((rules (and (plist-get props :rules)
                     (eas-component--rules (plist-get props :rules) ctx (concat (plist-get ctx :path) "/props/rules")))))
    (unless (eq rules 'hide)
      (let* ((fmt (plist-get props :format))
             (sep (plist-get props :labelSep))
             (lstyle (plist-get props :labelStyle))
             (short-label (or (plist-get props :short) (and label (eas-component-abbreviate label))))
             (mk (lambda (l v)
                   (append (and l (not (equal l ""))
                                (list (eas-component-span l lstyle) (eas-component-span sep lstyle)))
                           (list (eas-component-span v)))))
             (full (eas-component-format value fmt))
             (shorter (eas-component-format value fmt t)))
        (car (eas-component-restyle
              (list (eas-component--atom-of (funcall mk label full) props
                                            (funcall mk short-label full)
                                            (funcall mk short-label shorter)))
              (and (consp rules) rules)))))))

(eas-define-component
 "field"
 :doc "A datum field (or VALUE) as LABEL, labelSep, then the formatted value."
 :props (append '((field :type string :doc "Datum field to read.")
                  (value :type any :doc "The value, when not a datum field.")
                  (label :type string :doc "Label; defaults to the field name.")
                  (short :type string :doc "Abbreviated label.")
                  (labelSep :type string :default "=")
                  (labelStyle :type style)
                  (format :type format)
                  (rules :type rules :doc "[{test, color, bold, italic, dim, underline, background, hide}]"))
                eas-component--common-props)
 :render (lambda (props ctx)
           (let* ((field (plist-get props :field))
                  (value (if (plist-member (plist-get ctx :given) :value)
                             (plist-get props :value)
                           (and field (plist-get (plist-get ctx :datum) (eas-key field)))))
                  (label (or (plist-get props :label) field)))
             (delq nil (list (eas-component--field-atom label value props ctx))))))

(eas-define-component
 "row"
 :doc "Children side by side, SEP between them."
 :children t
 :props '((sep :type string :default "  ")
          (style :type style))
 :render (lambda (props ctx)
           (let ((sep (list (eas-component-span (plist-get props :sep)))) out prev)
             (dolist (atoms (eas-component-render-children ctx))
               (when atoms
                 (let ((first (copy-sequence (car atoms))))
                   (unless (or (null prev) (eq (plist-get first :role) 'sep) (eq (plist-get prev :role) 'sep))
                     (setq first (plist-put first :sep sep)))
                   (setq out (append out (cons first (cdr atoms))) prev (car (last atoms))))))
             (eas-component-restyle out (plist-get props :style)))))

(eas-define-component
 "when"
 :doc "Children when TEST holds on the datum, else the else branch.
With STYLE and no else branch, the children always show, styled when
TEST holds: conditional styling.  Without STYLE: conditional omission."
 :children t
 :props '((test :type expr :required t) (style :type style))
 :render (lambda (props ctx)
           (let* ((hold (eas-expr-truthy (eas-component--eval (plist-get props :test) ctx
                                                              (concat (plist-get ctx :path) "/props/test"))))
                  (style (plist-get props :style)))
             (cond (hold (eas-component-restyle (apply #'append (eas-component-render-children ctx)) style))
                   ((plist-get ctx :else) (apply #'append (eas-component-render-children ctx :else)))
                   (style (apply #'append (eas-component-render-children ctx)))))))

(eas-define-component
 "sep"
 :doc "A separator, dropped at a line's ends or next to another."
 :props '((text :type string :default " │ ") (style :type style))
 :render (lambda (props _ctx)
           (eas-component-restyle (list (eas-component-atom (list (eas-component-span (plist-get props :text)))
                                                            :role 'sep :priority 1000))
                                  (plist-get props :style))))

(eas-define-component
 "badge"
 :doc "TEXT in a box of BACKGROUND, in COLOR."
 :props (append '((text :type any :required t)
                  (color :type color :default "white")
                  (background :type color :default "gray40"))
                eas-component--common-props)
 :render (lambda (props _ctx)
           (let ((style (list :color (plist-get props :color) :background (plist-get props :background) :bold t)))
             (list (eas-component--atom-of
                    (list (eas-component-span (concat " " (eas-component--plain (plist-get props :text)) " ") style))
                    props)))))

(defconst eas-component--bars "▁▂▃▄▅▆▇█" "Sparkline glyphs, low to high.")

(defun eas-component-sparkline-string (values)
  "VALUES (numbers) as a string of block glyphs."
  (let* ((nums (seq-filter #'numberp (append values nil))))
    (if (null nums) ""
      (let ((lo (apply #'min nums)) (hi (apply #'max nums)))
        (mapconcat (lambda (v)
                     (let ((k (if (= hi lo) 3 (min 7 (floor (* 8 (/ (- v lo) (float (- hi lo)))))))))
                       (string (aref eas-component--bars k))))
                   nums "")))))

(eas-define-component
 "sparkline"
 :doc "An inline sparkline of VALUES (an array, often an expression)."
 :props (append '((values :type array :required t)
                  (max :type integer :default 16 :doc "At most this many latest values."))
                eas-component--common-props)
 :render (lambda (props _ctx)
           (let* ((vs (append (plist-get props :values) nil))
                  (vs (last vs (plist-get props :max))))
             (list (eas-component--atom-of (list (eas-component-span (eas-component-sparkline-string vs))) props
                                           (list (eas-component-span (eas-component-sparkline-string (last vs 6)))))))))

(eas-define-component
 "fields"
 :doc "The view's own fields as field atoms, later ones dropped first.
SOURCE strip is the values strip; hover the hovered datum's tooltip;
auto the hovered datum's when one is hovered, else the strip."
 :props '((source :type string :default "auto" :enum ("auto" "strip" "hover"))
          (labelSep :type string :default "=")
          (labelStyle :type style)
          (sep :type string :default "  " :doc "Between two fields.")
          (priority :type number :default 50 :doc "The first field's; each next is 0.01 lower.")
          (style :type style))
 :render (lambda (props ctx)
           (let* ((src (plist-get props :source))
                  (fields (pcase src
                            ("strip" (plist-get ctx :strip-fields))
                            ("hover" (plist-get ctx :hover-fields))
                            (_ (or (plist-get ctx :hover-fields) (plist-get ctx :strip-fields))))))
             (cl-loop with sep = (list (eas-component-span (plist-get props :sep)))
                      for f across (vconcat fields) for i from 0
                      for atom = (eas-component--field-atom
                               (plist-get f :title) (plist-get f :value)
                               (list :labelSep (plist-get props :labelSep) :labelStyle (plist-get props :labelStyle)
                                     :priority (- (plist-get props :priority) (* i 0.01))
                                     :style (plist-get props :style))
                               ctx)
                      collect (if (> i 0) (plist-put (copy-sequence atom) :sep sep) atom)))))

(provide 'eas-component-builtins)
;;; eas-component-builtins.el ends here

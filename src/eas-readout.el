;;; eas-readout.el --- the hover readout: a reserved line of components -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (eas-anj).  Every live chart reserves its readout up
;; front: `max_lines' lines (default 1) under the plot, whether or not
;; anything is hovered.  The plot is compiled that much shorter
;; (`eas-readout-max-lines' in `eas-mode--window-size'), so hovering
;; never changes the layout: only the readout's own cells change.
;;
;; What the line says is a component tree (eas-component): the chart's
;; x-eas.readout, else the default, which is the values strip
;; (eas-strip) as it always read:
;;
;;   latest  date=Mar 11, 2026  value=109
;;
;;   "x-eas": {"readout": {"max_lines": 1, "component": "row",
;;     "children": [{"component": "field", "props": {"field": "close"}}]}}
;;
;; The tree renders against a context: the datum (the hovered row, or
;; the strip's row at the pointer's column or the latest), the strip's
;; and the hovered datum's fields, and the expression names `at'
;; ("cursor" or "latest"), `hovered' and `view'.  The atoms are fitted
;; to the line (`eas-component-fit') and drawn as propertized text in
;; terminals and as SVG tspans in GUI frames.  `eas-readout-functions'
;; lets Lisp say what a spec cannot.
;;
;; Textual tooltips use the same API: x-eas.tooltip is a component tree
;; for the hovered datum, and any tooltip shown in the echo area is
;; kept to one line (`eas-readout-echo-line'): a multi-line echo area
;; would resize the chart's window.

;;; Code:

(require 'eas-component)
(require 'eas-component-builtins)
(require 'eas-view)
(require 'eas-strip)
(require 'eas-crosshair)
(require 'eas-template)

(defvar eas-readout-functions nil
  "Abnormal hook: functions of DATUM, VIEW and CTX returning readout text.
The first non-nil result, a (propertized) string, is the readout,
fitted to the reserved line like a component's.  DATUM is the row the
readout reads (nil when the chart has none); CTX is the component
context (see `eas-define-component').  Add buffer-locally to change one
chart's readout.")

(defvar eas-readout-svg-font-size 12 "Font size of the GUI readout image, in pixels.")

(defconst eas-readout--option-keys '(:max_lines :maxLines)
  "Readout keys that are options, not part of the component node.")

(defconst eas-readout-default
  '(:component "row"
    :children [(:component "text" :props (:text (:expr "at") :priority 10))
               (:component "fields" :props (:source "strip"))])
  "The readout of a chart without x-eas.readout: the values strip.")

;;; Spec

(defvar eas-readout--declarations (make-hash-table :test 'eq :weakness 'key)
  "View -> the x-eas object its source spec declared (resolving strips it).")

(defun eas-readout--remember (view source)
  "Keep the x-eas readout and tooltip SOURCE declares for VIEW.
SOURCE is what `eas-view-open' was given; templates keep their own."
  (unless (eas-view-template view)
    (let* ((spec (cond ((eas-object-p source) source)
                       ((stringp source) (ignore-errors (eas-spec-parse source)))))
           (meta (and (eas-object-p spec) (plist-get spec :x-eas))))
      (when (or (plist-get meta :readout) (plist-get meta :tooltip))
        (puthash view (list :readout (plist-get meta :readout) :tooltip (plist-get meta :tooltip))
                 eas-readout--declarations)))))

(add-hook 'eas-view-open-functions #'eas-readout--remember)

(defun eas-readout--declared (view key)
  "VIEW's x-eas KEY (:readout or :tooltip): its spec's, else its template's."
  (let ((view (eas-view-get view)))
    (or (plist-get (gethash view eas-readout--declarations) key)
        (when-let* ((name (eas-view-template view)) (template (eas-template-get name)))
          (plist-get (plist-get template :meta) key)))))

(defun eas-readout--split (declared path)
  "DECLARED readout as (NODE . MAX-LINES); errors name PATH."
  (cond ((null declared) (cons nil 1))
        ((stringp declared) (cons declared 1))
        ((not (eas-object-p declared))
         (eas-signal "INVALID_INPUT" "A readout is an object {\"component\": ...} or a string" :path path))
        (t (let* ((lines (or (plist-get declared :max_lines) (plist-get declared :maxLines) 1))
                  (node (cl-loop for (k v) on declared by #'cddr
                                 unless (memq k eas-readout--option-keys) append (list k v))))
             (unless (and (integerp lines) (<= 1 lines 10))
               (eas-signal "INVALID_INPUT" (format "max_lines is an integer from 1 to 10, got %S" lines)
                           :path (concat path "/max_lines")))
             (cons node lines)))))

(defun eas-readout-max-lines (view &optional key)
  "Lines VIEW reserves for its readout (KEY :tooltip: for its tooltip)."
  (or (ignore-errors
        (cdr (eas-readout--split (eas-readout--declared view (or key :readout)) "")))
      1))

(defun eas-readout-validate (view &optional key)
  "Check VIEW's x-eas readout (KEY :tooltip: its tooltip); signal on a fault.
Return t."
  (let* ((key (or key :readout)) (path (concat "/x-eas/" (eas-key-name key)))
         (node (car (eas-readout--split (eas-readout--declared view key) path))))
    (or (null node) (eas-component-validate node path))))

;;; Context

(defun eas-readout--strip-datum (scene plan state)
  "Return the row of SCENE's values strip under STATE, or nil.
PLAN compiled SCENE."
  (let ((pointer (plist-get state :pointer)))
    (cl-loop for view across (vconcat (plist-get scene :views))
             thereis
             (cl-loop for mark across (vconcat (plist-get view :marks))
                      thereis
                      (when-let* (((member (plist-get mark :mark) eas-strip-marks))
                                  ((not (plist-get mark :interactive-off)))
                                  ((> (length (plist-get mark :items)) 0))
                                  (unit (eas-tip--unit plan (plist-get mark :id)))
                                  (key (eas-strip--key (plist-get unit :encoding)))
                                  (picked (eas-strip--pick mark key (eas-strip--column view key pointer))))
                        (aref (plist-get mark :rows) (car picked)))))))

(defun eas-readout-context (view &optional lean)
  "The component context of live VIEW's readout.
LEAN leaves out the datum and the hovered fields: the default readout,
which redraws on every pointer move, reads only the strip."
  (let* ((view (eas-view-get view))
         (scene (eas-view-scene view)) (plan (eas-view-plan view)) (state (eas-view-state view))
         (hover (plist-get state :hover))
         (strip (and scene (eas-strip scene plan state)))
         (hover-fields (and hover scene (not lean) (eas-crosshair-readout scene plan hover)))
         (row (cond (lean nil) (hover (plist-get hover :row))
                    (scene (eas-readout--strip-datum scene plan state))))
         (datum (and row (eas--plist-without row eas-params-row-key))))
    (list :datum datum :view view
          :theme (plist-get (eas-view-spec view) :config)
          :strip-fields (plist-get strip :fields) :hover-fields hover-fields
          :env (list :at (plist-get strip :at) :hovered (if hover t :false) :view (eas-view-id view)))))

;;; Render

(defun eas-readout--error-atoms (err)
  "Atoms that show eas error ERR in place of a readout."
  (let ((e (eas-error-plist err)))
    (list (eas-component-atom (list (eas-component-span (format "readout: %s%s" (plist-get e :message)
                                                                (if (plist-get e :path)
                                                                    (format " at %s" (plist-get e :path)) ""))
                                                        '(:color "red")))
                              :keep t))))

(defun eas-readout-atoms (view &optional key ctx)
  "Atoms of VIEW's readout (KEY :tooltip: its tooltip) in CTX.
A fault in the tree becomes an atom that names it: failures are data."
  (let* ((key (or key :readout))
         (ctx (or ctx (eas-readout-context view (and (eq key :readout) (null eas-readout-functions)
                                                     (null (eas-readout--declared view key))))))
         (path (concat "/x-eas/" (eas-key-name key))))
    (condition-case err
        (let ((hooked (and (eq key :readout)
                           (run-hook-with-args-until-success 'eas-readout-functions
                                                             (plist-get ctx :datum) (eas-view-get view) ctx))))
          (if hooked
              (list (eas-component-atom (eas-component-from-string hooked)))
            (let ((node (or (car (eas-readout--split (eas-readout--declared view key) path))
                            (if (eq key :tooltip)
                                '(:component "fields" :props (:source "hover" :labelSep ": "))
                              eas-readout-default))))
              (eas-component-render node (append (list :path path) ctx)))))
      (eas-error (eas-readout--error-atoms err)))))

(defun eas-readout-render (view width &optional key atoms)
  "VIEW's readout (KEY :tooltip: its tooltip) fitted to WIDTH columns.
ATOMS are its atoms when already rendered (`eas-readout-atoms').
Return exactly its max_lines span lists."
  (eas-component-fit (or atoms (eas-readout-atoms view key)) width (eas-readout-max-lines view key)))

(defun eas-readout-text (view width &optional key)
  "VIEW's readout as propertized lines of at most WIDTH columns, joined.
KEY :tooltip renders its tooltip instead."
  (mapconcat #'eas-component-propertize (eas-readout-render view width key) "\n"))

(defun eas-readout--char-px ()
  "Width of a monospace character in the GUI readout image, in pixels."
  (* 0.6 eas-readout-svg-font-size))

(defun eas-readout-svg (view width-px &optional line-px foreground atoms)
  "VIEW's readout as an SVG document WIDTH-PX wide, LINE-PX per line.
Its height is fixed: max_lines times LINE-PX (default 16).  Text is
monospace in FOREGROUND (default gray) with one tspan per span.  ATOMS
are the readout's atoms when already rendered."
  (let* ((line-px (or line-px 16))
         (lines (eas-readout-render view (max 1 (floor (- width-px 8) (eas-readout--char-px))) nil atoms))
         (height (* line-px (length lines))))
    (concat (format "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\">"
                    (round width-px) height)
            (cl-loop for spans in lines for i from 0
                     concat (format "<text x=\"4\" y=\"%s\" font-family=\"monospace\" font-size=\"%s\" fill=\"%s\" xml:space=\"preserve\">%s</text>"
                                    (+ (* i line-px) (round (* 0.75 line-px))) eas-readout-svg-font-size
                                    (or foreground "#666666") (eas-component-svg-tspans spans)))
            "</svg>")))

(defun eas-readout-width (view)
  "Columns the readout of VIEW may take.
A text chart's width; for an SVG chart, the width of the window
showing it (the line under the image), else the chart's."
  (let* ((view (eas-view-get view)) (size (eas-view-size view))
         (window (and (buffer-live-p (eas-view-buffer view)) (get-buffer-window (eas-view-buffer view) t))))
    (cond ((eq (eas-view-target view) 'text) (or (plist-get size :cols) 80))
          (window (max 10 (window-body-width window)))
          (t (let ((px (or (plist-get (plist-get (eas-view-scene view) :size) :w) (car-safe size) 640)))
               (max 10 (floor px (if (display-graphic-p) (frame-char-width) 7))))))))

(defun eas-readout-tooltip-string (view &optional width)
  "VIEW's x-eas.tooltip for its hovered datum, in WIDTH columns, or nil.
Nil too when the chart declares no x-eas.tooltip or nothing is hovered."
  (when (and (eas-readout--declared view :tooltip) (plist-get (eas-view-state (eas-view-get view)) :hover))
    (let ((text (string-trim-right (eas-readout-text view (or width (eas-readout--echo-width)) :tooltip))))
      (and (not (string-empty-p text)) text))))

;;; Echo area

(defun eas-readout--echo-width ()
  "Columns of the echo area."
  (max 20 (1- (frame-width))))

(defun eas-readout-echo-line (text &optional width)
  "TEXT on one line of WIDTH columns (default the echo area's).
Its lines become fields two spaces apart; later ones are dropped first,
then the rest is elided.  Text properties are kept."
  (if (null text) nil
    (let ((parts (split-string text "\n" t)))
      (if (and (<= (length parts) 1) (<= (string-width text) (or width (eas-readout--echo-width)))) text
        (let ((atoms (cl-loop for p in parts for i from 0
                              collect (let ((a (eas-component-atom (eas-component-from-string p) :priority (- i))))
                                        (if (> i 0) (plist-put a :sep (list (eas-component-span "  "))) a)))))
          (eas-component-propertize (car (eas-component-fit atoms (or width (eas-readout--echo-width))))))))))

(defun eas-readout--message (text)
  "Show TEXT (nil clears) in the echo area without logging it."
  (let ((message-log-max nil))
    (if text (message "%s" text) (message nil))))

(defun eas-readout-show-help (help)
  "Show HELP as `show-help-function' does, on one line in the echo area.
A tooltip frame (GUI `tooltip-mode') keeps HELP's lines: it does not
resize the chart's window."
  (let ((orig (default-value 'show-help-function))
        (frame-p (and (display-graphic-p) (bound-and-true-p tooltip-mode))))
    (when (and (stringp help) (not frame-p)) (setq help (eas-readout-echo-line help)))
    (if (and orig (not (eq orig #'eas-readout-show-help))) (funcall orig help) (eas-readout--message help))))

(defun eas-readout-mode-setup ()
  "Reserve the readout's lines in an eas buffer before it is first sized.
The header line exists from the start, so its first readout does not
shrink the window, and help echoes stay on one line."
  (unless header-line-format (setq header-line-format ""))
  (setq-local show-help-function #'eas-readout-show-help))

(add-hook 'eas-view-mode-hook #'eas-readout-mode-setup)

(provide 'eas-readout)
;;; eas-readout.el ends here

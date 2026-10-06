;;; eas-action.el --- click targets: the action registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.1, fc-qx1.5).  A click (mouse-1, RET at point,
;; or a click event an agent sends) that lands on a datum becomes a
;; click target, recorded in the view state as :click and shown by
;; `eas-inspect'.  The target then runs one named action from
;; `eas-actions'.  Which one, first match wins:
;;
;;   1. `eas-action-bind' on this view, keyed by mark id or param name
;;   2. the template's "x-eas": {"actions": {KEY: BINDING}}, same keys
;;   3. key "*" in either
;;   4. "open-href" when the datum has an encoding.href
;;
;; A BINDING is an action name, or {"action": NAME, ...} whose other
;; members reach the action as the target's :args (drill's template,
;; notes' directory).  A param key matches when the click sits in a
;; view whose point selection param of that name fires on click.
;;
;; A click on a legend entry toggles a bind: "legend" point selection
;; (the reducer's job) and becomes a legend target (:legend CHANNEL
;; :value V :param NAME :selected BOOL); it runs only an action bound
;; to that param's name or to "legend", never "*" or open-href.  GUI
;; frames reach legends through their :map areas, terminals through
;; RET on a legend cell: both are the same click event.
;;
;; Built-in actions: open-href (URLs in the browser, anything else as
;; an org link), echo (the tooltip in the echo area) and copy-row (the
;; row as JSON on the kill ring); eas-action-org.el adds goto-source
;; and open-notes, eas-action-drill.el adds drill.  Register more
;; with `eas-register-action'.  An action receives the target plist
;; and the view; what it returns (a string or number) is recorded as
;; :result, and a failure as :error (ENGINE_FAILED with :action), so a
;; click never breaks dispatch.  Binding an unknown action signals
;; NOT_FOUND with :action.
;; Set `eas-action-inhibit' to record the action without running it;
;; `eas-replay' never re-runs actions.

;;; Code:

(require 'eas-core)
(require 'eas-template)
(require 'eas-params)
(require 'eas-view)
(require 'eas-tip)
(require 'eas-hit)
(require 'eas-scene)

(defvar eas-actions nil
  "Registered actions: alist of (NAME . (:fn FN :doc DOC)).")

(defvar eas-action-inhibit nil
  "Non-nil records each click's action in the view state without running it.")

(defvar eas-action-browse-function #'browse-url
  "Function the open-href action calls with a URL.")

(defvar eas-action-org-link-function #'eas-action--org-link-open
  "Function the open-href action calls with an href that is not a URL.")

(defconst eas-action-url-regexp "\\`\\(?:[a-zA-Z][a-zA-Z0-9+.-]*://\\|mailto:\\)"
  "Hrefs matching this are URLs for the browser; any other href is an org link.")

(defvar eas-action--bindings (make-hash-table :test 'eq :weakness 'key)
  "Per-view action bindings: view -> plist of (KEY BINDING).")

(cl-defun eas-register-action (name &key fn doc)
  "Register action NAME (a string): :fn FN is called with target and view.
The target is the plist `eas-inspect' shows as :click, plus :args
from the binding.  FN's string or number return value is recorded as
the click's :result.  DOC is one line for describe.  Re-registering a
name replaces it."
  (unless (and (stringp name) (functionp fn))
    (eas-signal "INVALID_INPUT" "An action is a name (string) and a :fn function" :action name))
  (setf (alist-get name eas-actions nil nil #'equal) (list :fn fn :doc (or doc "")))
  name)

(cl-defun eas-action-define (name fn &key doc)
  "Register action NAME running FN; see `eas-register-action' (DOC too)."
  (eas-register-action name :fn fn :doc doc))

(defun eas-action-names ()
  "Sorted names of every registered action."
  (sort (mapcar #'car eas-actions) #'string<))

(defun eas-action-describe ()
  "Every action as (:name :doc), JSON-ready."
  (vconcat (mapcar (lambda (name) (list :name name :doc (plist-get (alist-get name eas-actions nil nil #'equal) :doc)))
                   (eas-action-names))))

(defun eas-action--check (name)
  "Signal NOT_FOUND (with :action) unless NAME is registered."
  (unless (alist-get name eas-actions nil nil #'equal)
    (eas-signal "NOT_FOUND"
                  (format "No action %S; define it with `eas-action-define' or use one of: %s"
                          name (string-join (eas-action-names) ", "))
                  :action name)))

(defun eas-action--binding (binding)
  "BINDING (a name, or an object with :action) as (NAME . ARGS), or nil."
  (cond ((stringp binding) (list binding))
        ((and (eas-object-p binding) (stringp (plist-get binding :action)))
         (cons (plist-get binding :action) (eas--plist-without binding :action)))))

(defun eas-action-bind (view key action)
  "Make clicks on mark or param KEY (or \"*\") in VIEW run ACTION.
ACTION is a name, or a plist (:action NAME ARG VALUE ...) whose ARGs
reach the action as :args.  nil removes the binding.  Returns VIEW's
bindings."
  (let ((view (eas-view-get view)))
    (when action
      (eas-action--check (or (car (eas-action--binding action))
                               (eas-signal "INVALID_INPUT" "An action binding is a name or (:action NAME ...)"
                                             :action action))))
    (puthash view (if action
                      (eas-plist-put (gethash view eas-action--bindings) (eas-key key) action)
                    (eas--plist-without (gethash view eas-action--bindings) (eas-key key)))
             eas-action--bindings)))

(defun eas-action--template-actions (view)
  "The x-eas actions of VIEW's template, as a plist, or nil."
  (when-let* ((name (eas-view-template view)))
    (plist-get (plist-get (eas-template-get name) :meta) :actions)))

(defun eas-action--keys (view target)
  "Binding keys TARGET in VIEW answers to, most specific first."
  (if (plist-get target :legend)
      (list (plist-get target :param) "legend")
    (eas-action--datum-keys view target)))

(defun eas-action--datum-keys (view target)
  "Binding keys a datum TARGET in VIEW answers to, most specific first."
  (append (list (plist-get target :mark))
          (mapcar (lambda (p) (plist-get p :name))
                  (seq-filter (lambda (p) (and (equal (plist-get p :view) (plist-get target :view))
                                               (equal (plist-get (plist-get p :def) :type) "point")
                                               (equal (plist-get (plist-get p :def) :on) "click")
                                               (not (equal (plist-get p :bind) "legend"))))
                              (eas-params-of (eas-view-scene view))))
          (list "*")))

(defun eas-action-binding-for (view target)
  "The action a click on TARGET in VIEW triggers, as (NAME . ARGS), or nil."
  (let ((tables (list (gethash view eas-action--bindings)
                      (eas-action--template-actions view))))
    (or (cl-loop for key in (eas-action--keys view target)
                 thereis (cl-loop for table in tables
                                  thereis (and key (eas-action--binding (plist-get table (eas-key key))))))
        (and (stringp (plist-get target :href)) (list "open-href")))))

(defun eas-action-for (view target)
  "The action NAME a click on TARGET in VIEW triggers, or nil."
  (car (eas-action-binding-for view target)))

(defun eas-action-run (view target)
  "Run TARGET's action in VIEW; return TARGET with :action and its outcome."
  (let* ((binding (eas-action-binding-for view target))
         (name (car binding))
         (target (if (cdr binding) (append target (list :args (cdr binding))) target)))
    (append target
            (list :action (or name :null))
            (cond
             ((null name) nil)
             ((or eas-action-inhibit eas-view-replaying) (list :ran :false))
             (t (condition-case err
                    (progn (eas-action--check name)
                           (let ((result (funcall (plist-get (alist-get name eas-actions nil nil #'equal) :fn)
                                                  target view)))
                             (append (list :ran t) (when (or (stringp result) (numberp result))
                                                     (list :result result)))))
                  (eas-error (list :ran :false :error (eas-error-plist err)))
                  (error (list :ran :false
                               :error (list :code "ENGINE_FAILED" :action name
                                            :message (format "Action %s failed: %s; fix the action function"
                                                             name (error-message-string err)))))))))))

(defun eas-action-legend-hit (scene px)
  "The symbol legend entry of SCENE at PX bound to a selection, or nil.
Returns (:view :legend CHANNEL :value V :label L :param NAME :field F)
for an entry of a legend whose view has a bind: \"legend\" param."
  (cl-loop
   for view across (plist-get scene :views)
   for param = (seq-find (lambda (p) (and (equal (plist-get p :view) (plist-get view :id))
                                          (equal (plist-get p :bind) "legend")))
                         (eas-params-of scene))
   when param
   thereis (cl-loop
            for legend across (plist-get view :legends)
            unless (equal (plist-get legend :type) "gradient")
            thereis (cl-loop
                     for entry across (plist-get legend :entries)
                     when (eas-hit--contains (plist-get entry :bounds) (aref px 0) (aref px 1))
                     return (list :view (plist-get view :id) :legend (plist-get legend :channel)
                                  :value (plist-get entry :value) :label (plist-get entry :label)
                                  :param (plist-get param :name)
                                  :field (eas-params-channel-field scene (plist-get view :id)
                                                                     (plist-get legend :channel)))))))

(defun eas-action--legend-target (view px old-scene)
  "The legend target a click at PX in OLD-SCENE made, given VIEW's new state."
  (when-let* ((hit (eas-action-legend-hit old-scene px)))
    (let ((store (plist-get (plist-get (eas-view-state view) :params) (eas-key (plist-get hit :param)))))
      (append hit (list :px px
                        :selected (if (and store (eas-params-contains
                                                  store (list (eas-key (plist-get hit :field)) (plist-get hit :value))))
                                      t :false))))))

(defun eas-action--source-row (scene target)
  "Index of TARGET's datum in its view's source rows, from SCENE, or nil."
  (let* ((mark (eas-scene-mark scene (plist-get target :mark)))
         (rows (plist-get mark :rows))
         (datum (plist-get target :datum)))
    (and (integerp datum) (< datum (length rows))
         (plist-get (aref rows datum) eas-params-row-key))))

(defun eas-action--on-dispatch (view event old-state old-scene)
  "Record and act on a click EVENT in VIEW made in OLD-SCENE under OLD-STATE.
A click on empty space clears :click; other events leave it alone."
  (when-let* ((px (eas-tip-click-px event old-state)))
    (let* ((datum (eas-tip-click-target event old-state old-scene (eas-view-plan view)))
           (target (if datum
                       (append datum (when-let* ((n (eas-action--source-row old-scene datum)))
                                       (list :source-row n)))
                     (eas-action--legend-target view px old-scene))))
      (setf (eas-view-state view)
            (eas-plist-put (eas-view-state view) :click (and target (eas-action-run view target)))))))

(add-hook 'eas-view-dispatch-functions #'eas-action--on-dispatch)

;;; Built-in actions

(declare-function org-link-open-from-string "ol" (s &optional arg))

(defun eas-action--org-link-open (link)
  "Open org LINK (\"[[file:x.org::*H]]\", \"id:...\", \"*Heading\")."
  (require 'ol)
  (org-link-open-from-string (if (string-prefix-p "[[" link) link (format "[[%s]]" link))))

(defun eas-action-open-link (href)
  "Open HREF: a URL in the browser, anything else as an org link.  Return HREF."
  (funcall (if (string-match-p eas-action-url-regexp href)
               eas-action-browse-function
             eas-action-org-link-function)
           href)
  href)

(eas-register-action
 "open-href"
 :fn (lambda (target _view)
       (let ((href (plist-get target :href)))
         (unless (stringp href)
           (eas-signal "ENGINE_FAILED" "This datum has no href; add encoding.href to the mark"
                         :action "open-href"))
         (eas-action-open-link href)))
 :doc "Open the datum's encoding.href: URLs in the browser, anything else as an org link.")

(eas-action-define
 "echo" (lambda (target _view)
          (let ((text (or (eas-tip-text (let ((tip (plist-get target :tooltip))) (and (vectorp tip) tip)))
                          (eas-json-encode (plist-get target :row)))))
            (message "%s" text)
            text))
 :doc "Show the datum's tooltip (or its row) in the echo area.")

(eas-action-define
 "copy-row" (lambda (target _view)
              (let ((json (eas-json-encode (plist-get target :row))))
                (kill-new json)
                json))
 :doc "Copy the datum's row as JSON to the kill ring.")

(provide 'eas-action)
;;; eas-action.el ends here

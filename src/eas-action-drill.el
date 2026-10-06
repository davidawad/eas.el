;;; eas-action-drill.el --- the drill action: open a detail view -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 (fc-qx1.5).  "drill" opens a detail view of the clicked
;; datum: finer-grained rows, drawn by another template or spec.
;; Binding arguments (x-eas.actions or `eas-action-bind'):
;;
;;   provider  a detail provider registered with `eas-register-drill',
;;             called with the target, the view and the args; it returns
;;             the detail rows (intraday bars for a clicked day, the
;;             transactions behind a monthly total).  Without one, the
;;             rows are the view's own source rows that share the
;;             datum's "match" fields (default: its x and color
;;             fields), i.e. the rows an aggregate was made of.
;;   template  the template drawing the detail, its rows bound to slot
;;             "slot" (default "data") plus "bindings"; or
;;   spec      an inline chart/v1 spec whose data the rows replace.
;;             Neither: the view's own resolved spec.
;;
;; The detail is a live view like any other, id PARENT>LABEL, sized
;; like its parent; it is shown with `eas-action-drill-show-function'
;; when the parent has a buffer.  The action answers the new view id,
;; so an agent drills by dispatching a click and then inspects it.

;;; Code:

(require 'eas-core)
(require 'eas-params)
(require 'eas-view)
(require 'eas-action)

(declare-function eas-show "eas-mode" (view &optional target))

(defvar eas-drill-providers nil
  "Registered detail providers: alist of (NAME . (:fn FN :doc DOC)).")

(defvar eas-action-drill-show-function #'eas-action--drill-show
  "Function called with the detail view and its parent to display it.")

(cl-defun eas-register-drill (name &key fn doc)
  "Register detail provider NAME whose :fn FN gives the rows to drill into.
FN is called with the click target, the view and the binding args and
returns a vector (or list) of row plists.  DOC is one line."
  (unless (and (stringp name) (functionp fn))
    (eas-signal "INVALID_INPUT" "A drill provider is a name (string) and a :fn function" :provider name))
  (setf (alist-get name eas-drill-providers nil nil #'equal) (list :fn fn :doc (or doc "")))
  name)

(defun eas-action--drill-show (detail parent)
  "Show DETAIL in a buffer when PARENT is shown in one."
  (when (and (fboundp 'eas-show) (buffer-live-p (eas-view-buffer parent)))
    (eas-show detail (eas-view-target parent))))

(defun eas-action--strip (row)
  "ROW without the engine's row identity."
  (eas--plist-without row eas-params-row-key))

(defun eas-action--match-fields (target view)
  "Fields a datum's detail rows must share with TARGET's row in VIEW."
  (or (append (plist-get (plist-get target :args) :match) nil)
      (let ((scene (eas-view-scene view)) (id (plist-get target :view)))
        (seq-filter (lambda (f) (plist-member (plist-get target :row) (eas-key f)))
                    (delq nil (list (eas-params-channel-field scene id "x")
                                    (eas-params-channel-field scene id "color")))))))

(defun eas-action--drill-rows (target view)
  "The detail rows for TARGET in VIEW."
  (let* ((args (plist-get target :args))
         (provider (plist-get args :provider)))
    (if provider
        (let ((entry (or (alist-get provider eas-drill-providers nil nil #'equal)
                         (eas-signal "NOT_FOUND"
                                       (format "No drill provider %S; register it with `eas-register-drill'" provider)
                                       :action "drill" :provider provider))))
          (vconcat (funcall (plist-get entry :fn) target view args)))
      (let ((fields (eas-action--match-fields target view)) (row (plist-get target :row)))
        (unless fields
          (eas-signal "NOT_FOUND" "Nothing to match detail rows on; bind drill with \"match\" fields or a \"provider\""
                        :action "drill"))
        (vconcat (seq-filter (lambda (r) (cl-loop for f in fields
                                                  always (eas-params--same (plist-get r (eas-key f))
                                                                             (plist-get row (eas-key f)))))
                             (plist-get (eas-view-data view) :rows)))))))

(defun eas-action-drill (target view)
  "Open a detail view of TARGET's datum in VIEW; return its id."
  (let* ((args (plist-get target :args))
         (rows (vconcat (mapcar #'eas-action--strip (eas-action--drill-rows target view))))
         (row (plist-get target :row))
         (label (mapconcat (lambda (f) (format "%s" (plist-get row (eas-key f))))
                           (or (eas-action--match-fields target view) (list (cadr row)))
                           ","))
         (common (list :id (format "%s>%s" (eas-view-id view) label)
                       :size (eas-view-size view) :target (eas-view-target view) :cell (eas-view-cell view))))
    (when (zerop (length rows))
      (eas-signal "NOT_FOUND" (format "No detail rows for %s" label) :action "drill"))
    (let ((detail
           (cond ((plist-get args :template)
                  (apply #'eas-view-open (plist-get args :template)
                         :bindings (eas-plist-put (copy-sequence (plist-get args :bindings))
                                                    (eas-key (or (plist-get args :slot) "data")) rows)
                         common))
                 (t (apply #'eas-view-open (or (plist-get args :spec) (eas-view-spec view)) :rows rows common)))))
      (funcall eas-action-drill-show-function detail view)
      (eas-view-id detail))))

(eas-register-action
 "drill" :fn #'eas-action-drill
 :doc "Open a detail view of the datum: a provider's finer rows, or the source rows behind it.")

(provide 'eas-action-drill)
;;; eas-action-drill.el ends here

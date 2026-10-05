;;; eas-agent-live.el --- live agent verbs: views, inspect, dispatch, log, selection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The verbs that read or drive views in a running Emacs, reached with
;; emacsclient --eval '(eas-agent-json "inspect" "line:daily")':
;;
;;   views                              live views and what each shows
;;   open SOURCE [--data B] [--show]    open a view (and show it to the human)
;;   close VIEW                         forget a view
;;   inspect VIEW                       domains, selections, hover, visible summary
;;   dispatch VIEW EVENT|[EVENT...]     apply event/v1 (array = replay), then inspect
;;   log VIEW [--n N]                   recent events, oldest first
;;   selection VIEW [--name P] [--as rows|json|org]   the selected data
;;
;; They wrap the runtime (eas-view.el) and add nothing to it, so a
;; feature that lands in the runtime is drivable here at once.

;;; Code:

(require 'eas-agent-core)
(require 'eas-agent-verbs)
(require 'eas-view)

(declare-function eas-show "eas-mode" (view &optional target))

(defun eas-agent--view-id (verb pos)
  "The VIEW positional of VERB in POS."
  (format "%s" (eas-agent-arg-required pos 0 "a view id (see views)" verb)))

(defun eas-agent--inspect-next (id)
  "The next[] commands after looking at view ID."
  (list (eas-agent-live-cmd "dispatch" id "{\"type\":\"key\",\"key\":\"+\"}")
        (eas-agent-live-cmd "log" id)
        (eas-agent-live-cmd "selection" id)))

(defun eas-agent--view-row (id)
  "One `views' row for view ID."
  (let* ((view (eas-view-get id)) (buffer (eas-view-buffer view))
         (last (car (eas-view-log view))))
    (list :id id :template (or (eas-view-template view) :null)
          :subject (or (eas-view-subject view) :null)
          :interactive (if (eas-view-interactive view) t :false)
          :target (symbol-name (eas-view-target view))
          :buffer (if (buffer-live-p buffer) (buffer-name buffer) :null)
          :size (or (plist-get (eas-view-scene view) :size) :null)
          :rows (length (plist-get (eas-view-data view) :rows))
          :last-event (if last (plist-get last :summary) :null))))

(defun eas-agent-views (_pos _opts)
  "Answer views: every live view."
  (let ((ids (eas-view-ids)))
    (apply #'eas-agent-ok (vconcat (mapcar #'eas-agent--view-row ids))
           (if ids (list (eas-agent-live-cmd "inspect" (car ids)))
             (list (eas-agent-live-cmd "open" "TEMPLATE" :data "BINDINGS.json")
                   (eas-agent-cmd "describe" "templates"))))))

(defun eas-agent-open (pos opts)
  "Answer open SOURCE (first of POS): register a live view.
With :show in OPTS, display it."
  (let* ((source (eas-agent-arg-required pos 0 "a template name or a chart/v1 spec" "open"))
         (template (eas-agent-template-name-p source))
         (backend (eas-agent-arg-choice opts :backend eas-agent-backends "svg"))
         (view (eas-view-open (if template source (eas-agent-arg-json source))
                                :bindings (and template (eas-agent-bindings opts))
                                :id (plist-get opts :id) :subject (plist-get opts :subject)
                                :size (eas-agent-size opts backend) :target (intern backend)))
         (id (eas-view-id view)))
    (when (and (eas-true-p (plist-get opts :show)) (not noninteractive))
      (require 'eas-mode)
      (eas-show view (and (plist-get opts :backend) (intern backend))))
    (apply #'eas-agent-ok (eas-inspect view) (eas-agent--inspect-next id))))

(defun eas-agent-close (pos _opts)
  "Answer close VIEW (first of POS): forget it."
  (let ((id (eas-agent--view-id "close" pos)))
    (eas-view-close id)
    (eas-agent-ok (list :closed id) (eas-agent-live-cmd "views"))))

(defun eas-agent-inspect (pos _opts)
  "Answer inspect VIEW (first of POS): what it draws right now."
  (let ((id (eas-agent--view-id "inspect" pos)))
    (apply #'eas-agent-ok (eas-inspect id) (eas-agent--inspect-next id))))

(defun eas-agent-dispatch (pos _opts)
  "Answer dispatch VIEW EVENT (POS): apply event/v1 and inspect.
An array of events replays them."
  (let* ((id (eas-agent--view-id "dispatch" pos))
         (event (eas-agent-arg-json
                 (eas-agent-arg-required pos 1 "an event/v1 object (see describe events)" "dispatch"))))
    (eas-view-get id)
    (apply #'eas-agent-ok
           (if (vectorp event) (eas-replay id event) (eas-dispatch id event))
           (eas-agent--inspect-next id))))

(defun eas-agent-log (pos opts)
  "Answer log VIEW (first of POS): recent events, oldest first.
OPTS :n limits the count."
  (let ((id (eas-agent--view-id "log" pos)))
    (eas-agent-ok (eas-view-log-entries id (eas-agent-arg-number opts :n))
                    (eas-agent-live-cmd "inspect" id))))

(defun eas-agent-selection (pos opts)
  "Answer selection VIEW (first of POS): the selected rows.
OPTS :name picks the param and :as is rows, json or org."
  (let* ((id (eas-agent--view-id "selection" pos))
         (as (eas-agent-arg-choice opts :as '("rows" "json" "org") "rows"))
         (name (plist-get opts :name))
         (rows (eas-selection id name 'rows)))
    (eas-agent-ok (append (list :view id :param (or name :null) :as as :n (length rows))
                            (if (equal as "org") (list :org (eas-selection id name 'org))
                              (list :rows rows)))
                    (eas-agent-live-cmd "inspect" id))))

(eas-agent-register-verb
 "views" #'eas-agent-views :live t :doc "Live views: id, template, buffer, size, last event"
 :usage "views")
(eas-agent-register-verb
 "open" #'eas-agent-open :live t :doc "Open SOURCE as a live view; --show displays it"
 :usage "open SOURCE [--data B] [--id ID] [--subject S] [--backend svg|text] [--show]"
 :options (append '(:id :subject :show) eas-agent--source-options) :flags '(:show))
(eas-agent-register-verb
 "close" #'eas-agent-close :live t :doc "Forget VIEW" :usage "close VIEW")
(eas-agent-register-verb
 "inspect" #'eas-agent-inspect :live t
 :doc "Domains, selections, hovered datum and visible-range summary of VIEW" :usage "inspect VIEW")
(eas-agent-register-verb
 "dispatch" #'eas-agent-dispatch :live t
 :doc "Apply an event/v1 (or replay an array of them) to VIEW; returns inspect"
 :usage "dispatch VIEW EVENT")
(eas-agent-register-verb
 "log" #'eas-agent-log :live t :doc "VIEW's recent events, oldest first"
 :usage "log VIEW [--n N]" :options '(:n))
(eas-agent-register-verb
 "selection" #'eas-agent-selection :live t :doc "Rows selected in VIEW"
 :usage "selection VIEW [--name PARAM] [--as rows|json|org]" :options '(:name :as))

(provide 'eas-agent-live)
;;; eas-agent-live.el ends here

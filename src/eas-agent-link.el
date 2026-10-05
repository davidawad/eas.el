;;; eas-agent-link.el --- live agent verbs for linked views: link, unlink, buses -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The param bus (eas-link-bus.el) on the agent surface (fc-qx1.6):
;;
;;   link VIEW BUS [--params a,b]   join VIEW to BUS (created on first use)
;;   unlink VIEW [--bus BUS]        leave BUS (default: every bus)
;;   buses                          every bus: params, members, last stores
;;
;; Once linked, `dispatch' on one member moves the others; their `log'
;; shows the link events they received.

;;; Code:

(require 'eas-agent-core)
(require 'eas-agent-live)
(require 'eas-link-bus)

(defun eas-agent--params-list (value)
  "VALUE (a list, vector or \"a,b\" string) as a list of param names."
  (cond ((null value) nil)
        ((stringp value) (split-string value "," t "[ \t]+"))
        (t (mapcar (lambda (v) (format "%s" v)) (append value nil)))))

(defun eas-agent-link (pos opts)
  "Answer link VIEW BUS (POS): join VIEW to BUS; OPTS :params limits it."
  (let* ((id (eas-agent--view-id "link" pos))
         (bus (format "%s" (eas-agent-arg-required pos 1 "a bus name" "link")))
         (data (eas-link-join id bus (eas-agent--params-list (plist-get opts :params)))))
    (eas-agent-ok data
                  (eas-agent-live-cmd "dispatch" id "{\"type\":\"pointermove\",\"px\":[200,100]}")
                  (eas-agent-live-cmd "buses"))))

(defun eas-agent-unlink (pos opts)
  "Answer unlink VIEW (first of POS); OPTS :bus names one bus."
  (let ((id (eas-agent--view-id "unlink" pos)))
    (eas-link-leave id (plist-get opts :bus))
    (eas-agent-ok (list :view id :buses (vconcat (ignore-errors (eas-link-buses-of id))))
                  (eas-agent-live-cmd "buses"))))

(defun eas-agent-buses (_pos _opts)
  "Answer buses: every param bus."
  (let ((names (eas-link-bus-names)))
    (apply #'eas-agent-ok (vconcat (mapcar #'eas-link-describe-bus names))
           (if names (list (eas-agent-live-cmd "inspect" (car (eas-link-bus-members (eas-link-get (car names))))))
             (list (eas-agent-live-cmd "link" "VIEW" "BUS"))))))

(eas-agent-register-verb
 "link" #'eas-agent-link :live t
 :doc "Join VIEW to param bus BUS: hover, brush and zoom follow across views"
 :usage "link VIEW BUS [--params NAME,NAME]" :options '(:params))
(eas-agent-register-verb
 "unlink" #'eas-agent-unlink :live t :doc "Take VIEW out of BUS (default: every bus)"
 :usage "unlink VIEW [--bus BUS]" :options '(:bus))
(eas-agent-register-verb
 "buses" #'eas-agent-buses :live t :doc "Param buses: params carried, members, last stores"
 :usage "buses")

(provide 'eas-agent-link)
;;; eas-agent-link.el ends here

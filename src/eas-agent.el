;;; eas-agent.el --- one verb set and envelope for agents: Lisp, shell, emacsclient -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L7, the agent surface (design section 2 L7 and section 3).  Every
;; verb answers with bin/chart's chart/v1 envelope
;;
;;   {"contract":"chart/v1","ok":BOOL,"data":...,"reason"?:CODE,
;;    "evidence"?:{...},"next":["command",...]}
;;
;; from three entry points:
;;
;;   (eas-agent "render" "line" :data BINDINGS :backend "text")  ; Lisp, a plist
;;   bin/eas render line --data b.json --backend text            ; shell, batch
;;   emacsclient --eval '(eas-agent-json "inspect" "line:daily")' ; live Emacs
;;
;; `(eas-agent "describe" "verbs")' lists the verbs.  Failures never
;; signal: they come back with ok false, a reason code, evidence and
;; at least one next command.

;;; Code:

(require 'eas)
(require 'eas-agent-core)
(require 'eas-agent-verbs)
(require 'eas-agent-live)
(require 'eas-agent-link)
(require 'eas-agent-health)

(defun eas-agent--failure-next (verb pos evidence)
  "The next[] commands for a VERB failure with EVIDENCE; POS are its positionals."
  (let* ((source (car pos))
         (live (plist-get (alist-get verb eas-agent-verbs nil nil #'equal) :live))
         (template (or (plist-get evidence :template)
                       (and (not live) (eas-agent-template-name-p source) source)))
         (label (and source (not live) (eas-agent-source-label source))))
    (pcase (plist-get evidence :code)
      ("VIEW_NOT_FOUND" (list (eas-agent-live-cmd "views")))
      ("EVENT_INVALID" (list (eas-agent-cmd "describe" "events")
                             (and source (eas-agent-live-cmd "inspect" source))))
      ((or "SLOT_MISSING" "SLOT_TYPE" "SHAPE_INVALID" "FIELD_MISSING")
       (list (and (stringp template) (eas-agent-cmd "example" template))
             (eas-agent-cmd "describe" "templates")))
      ("TRANSFORM_UNKNOWN" (list (eas-agent-cmd "describe" "transforms")))
      ("UNSUPPORTED_FEATURE" (list (eas-agent-cmd "describe" "supported")
                                   (and label (eas-agent-cmd "export" label "--vl"))))
      ("NOT_FOUND" (list (eas-agent-cmd "describe" (if (plist-get evidence :template) "templates"))))
      ("ENGINE_FAILED" (list (eas-agent-cmd "doctor")))
      (_ (list (and label (not (equal verb "check")) (eas-agent-cmd "check" label))
               (eas-agent-cmd "describe" "verbs"))))))

(defun eas-agent--json-safe (plist)
  "PLIST with every value that does not encode as JSON printed instead."
  (cl-loop for (k v) on plist by #'cddr
           append (list k (if (ignore-errors (eas-json-encode (list :v v))) v
                            (format "%S" v)))))

(defun eas-agent--error-envelope (verb pos err)
  "The failure envelope for VERB (positionals POS) after condition ERR."
  (let ((evidence (eas-agent--json-safe (eas-error-plist err))))
    (apply #'eas-agent-fail (plist-get evidence :code) evidence nil
           (or (delq nil (eas-agent--failure-next verb pos evidence))
               (list (eas-agent-cmd "describe"))))))

(defun eas-agent (verb &rest args)
  "Run agent VERB with ARGS; return the chart/v1 envelope as a plist.
VERB is a string or symbol (see `(eas-agent \"describe\" \"verbs\")').
ARGS are positionals followed by keyword options, e.g.
  (eas-agent \"render\" \"line\" :data BINDINGS :backend \"text\")
Never signals: failures are envelopes with ok `:false'."
  (let ((verb (format "%s" verb)) pos)
    (condition-case err
        (let* ((entry (eas-agent-verb verb))
               (split (eas-agent-split-args args)))
          (setq pos (car split))
          (eas-agent-check-options verb entry (cdr split))
          (funcall (plist-get entry :function) (car split) (cdr split)))
      (error (eas-agent--error-envelope verb pos err)))))

(defun eas-agent-encode (envelope)
  "ENVELOPE as compact JSON; an unencodable one becomes ENGINE_FAILED."
  (condition-case err
      (eas-json-encode envelope)
    (error (eas-json-encode
            (eas-agent-fail "ENGINE_FAILED"
                              (list :message (format "Verb result is not JSON (%s); report it"
                                                     (error-message-string err)))
                              nil (eas-agent-cmd "doctor"))))))

(defun eas-agent-json (verb &rest args)
  "Like `eas-agent' with VERB and ARGS, but return the envelope as JSON.
For emacsclient --eval, which prints the result as a Lisp string:
pipe it through jq -r . to get the JSON."
  (eas-agent-encode (apply #'eas-agent verb args)))

(provide 'eas-agent)
;;; eas-agent.el ends here

;;; eas-agent-core.el --- the chart/v1 envelope, verb registry and arguments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; L7 plumbing shared by every agent verb.  A verb is a function of
;; (POSITIONALS OPTIONS) returning an envelope, which `eas-agent-ok'
;; and `eas-agent-fail' build:
;;
;;   {"contract": "chart/v1", "ok": true|false, "data": ...,
;;    "reason": CODE?, "evidence": {...}?, "next": ["command", ...]}
;;
;; REASON is a section 5 reason code (shared with bin/chart), EVIDENCE
;; the failure plist (:message, plus :path :index :field ... when
;; known) and NEXT runnable commands: `bin/eas ...' for stateless
;; verbs, `emacsclient --eval ...' for live ones.  Every failure
;; carries at least one.
;;
;; Arguments look the same from every surface: positionals first, then
;; options.  The shell spells `--backend text' and `--vl'; Lisp spells
;; :backend "text" and :vl t.  Option values may be strings (the shell)
;; or parsed values (Lisp); the `eas-agent-arg-*' helpers take both.

;;; Code:

(require 'eas-core)
(require 'eas-template)

(defconst eas-agent-contract "chart/v1"
  "The envelope contract every verb returns (bin/chart's).")

(defvar eas-agent-verbs nil
  "Registered verbs: alist of (NAME . PLIST).
PLIST has :function :doc :usage :options :flags and :live.")

(defvar eas-agent-stdin-function nil
  "Function of no arguments returning stdin as a string, or nil.
bin/eas sets it so that an argument \"-\" reads stdin.")

(defvar eas-agent-shell-program "bin/eas"
  "How next[] commands spell the shell entry point.")

(cl-defun eas-agent-register-verb (name function &key doc usage options flags live)
  "Register verb NAME (a string) implemented by FUNCTION.
FUNCTION takes (POSITIONALS OPTIONS) and returns an envelope.  DOC is
one line, USAGE the shell synopsis.  OPTIONS are the keywords the verb
accepts; FLAGS the subset that take no value on the command line.
LIVE non-nil means the verb reads views in a running Emacs."
  (setf (alist-get name eas-agent-verbs nil nil #'equal)
        (list :function function :doc doc :usage usage :options options
              :flags flags :live live))
  name)

(defun eas-agent-verb-names ()
  "Sorted names of every registered verb."
  (sort (mapcar #'car eas-agent-verbs) #'string<))

(defun eas-agent-verb (name)
  "The registry entry of verb NAME or signal INVALID_INPUT."
  (or (alist-get name eas-agent-verbs nil nil #'equal)
      (eas-signal "INVALID_INPUT"
                    (format "Unknown verb %S; verbs: %s" name
                            (string-join (eas-agent-verb-names) ", "))
                    :verb name)))

;;; Envelope

(defun eas-agent-ok (data &rest next)
  "A successful envelope carrying DATA with NEXT commands (nil dropped)."
  (list :contract eas-agent-contract :ok t :data data
        :next (vconcat (delq nil next))))

(defun eas-agent-fail (reason evidence data &rest next)
  "A failed envelope: REASON code, EVIDENCE plist, DATA and NEXT commands."
  (list :contract eas-agent-contract :ok :false :data (or data :null)
        :reason reason :evidence evidence
        :next (vconcat (delq nil next))))

;;; next[] commands

(defun eas-agent--shell-word (word)
  "WORD quoted for a POSIX shell when it needs it."
  (let ((word (format "%s" word)))
    (if (string-match-p "\\`[-A-Za-z0-9_./:=@,+%]+\\'" word) word
      (concat "'" (replace-regexp-in-string "'" "'\\\\''" word t t) "'"))))

(defun eas-agent-cmd (verb &rest words)
  "A `bin/eas VERB WORDS...' command line; nil WORDS are dropped."
  (mapconcat #'eas-agent--shell-word
             (cons eas-agent-shell-program (cons verb (delq nil words)))
             " "))

(defun eas-agent-live-cmd (verb &rest args)
  "An emacsclient command line running (eas-agent-json VERB ARGS...)."
  (concat "emacsclient --eval "
          (eas-agent--shell-word
           (format "(eas-agent-json %s)"
                   (mapconcat #'prin1-to-string (cons verb (delq nil args)) " ")))))

;;; Arguments

(defun eas-agent-split-args (args)
  "Split ARGS into (POSITIONALS . OPTIONS).
Positionals are the leading non-keyword elements; the rest must be
keyword/value pairs."
  (let (pos)
    (while (and args (not (keywordp (car args))))
      (push (pop args) pos))
    (let ((opts args))
      (while opts
        (unless (and (keywordp (car opts)) (cdr opts))
          (eas-signal "INVALID_INPUT"
                        (format "Expected option keyword and value, got %S; positionals come first"
                                (car opts))))
        (setq opts (cddr opts))))
    (cons (nreverse pos) args)))

(defun eas-agent-check-options (verb entry opts)
  "Signal INVALID_INPUT when OPTS has a key VERB's ENTRY does not accept."
  (let ((allowed (plist-get entry :options)))
    (dolist (key (eas-plist-keys opts))
      (unless (memq key allowed)
        (eas-signal "INVALID_INPUT"
                      (format "%s takes no option --%s; options: %s" verb (eas-key-name key)
                              (if allowed
                                  (mapconcat (lambda (k) (concat "--" (eas-key-name k))) allowed " ")
                                "none"))
                      :option (eas-key-name key))))))

(defun eas-agent--stdin ()
  "Stdin as a string, or signal INVALID_INPUT when there is none."
  (if eas-agent-stdin-function (funcall eas-agent-stdin-function)
    (eas-signal "INVALID_INPUT"
                  "\"-\" reads stdin, which only bin/eas has; pass the value itself")))

(defun eas-agent-json-text-p (value)
  "Return non-nil when string VALUE is inline JSON (an object or array)."
  (and (stringp value) (string-match-p "\\`[ \t\n]*[[{]" value)))

(defun eas-agent-arg-json (value)
  "VALUE as a parsed JSON value.
A string is \"-\" (stdin), inline JSON or a readable JSON file;
anything else is returned as it is (already parsed)."
  (cond ((not (stringp value)) value)
        ((equal value "-") (eas-json-parse (eas-agent--stdin)))
        ((eas-agent-json-text-p value) (eas-json-parse value))
        ((file-readable-p value) (eas-json-read-file value))
        (t (eas-signal "INVALID_INPUT"
                         (format "%S is neither JSON nor a readable JSON file" value)
                         :value value))))

(defun eas-agent-arg-number (opts key)
  "OPTS's KEY as a number, or nil when absent."
  (let ((v (plist-get opts key)))
    (cond ((null v) nil)
          ((numberp v) v)
          ((and (stringp v) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" v))
           (string-to-number v))
          (t (eas-signal "INVALID_INPUT"
                           (format "--%s needs a number, got %S" (eas-key-name key) v)
                           :option (eas-key-name key))))))

(defun eas-agent-arg-choice (opts key choices default)
  "OPTS's KEY as a string from CHOICES (DEFAULT when absent)."
  (let ((v (plist-get opts key)))
    (if (null v) default
      (let ((s (format "%s" v)))
        (unless (member s choices)
          (eas-signal "INVALID_INPUT"
                        (format "--%s is one of %s, got %S" (eas-key-name key)
                                (string-join choices "|") s)
                        :option (eas-key-name key)))
        s))))

(defun eas-agent-arg-required (pos n what verb)
  "Positional N of POS, or signal INVALID_INPUT saying VERB needs WHAT."
  (or (nth n pos)
      (eas-signal "INVALID_INPUT"
                    (format "%s needs %s; usage: %s" verb what
                            (plist-get (eas-agent-verb verb) :usage))
                    :verb verb)))

;;; Sources: a template name with bindings, or a chart/v1 spec

(defun eas-agent-template-name-p (source)
  "Non-nil when SOURCE names a registered template."
  (eas-template-p source))

(defun eas-agent-source-label (source)
  "How next[] commands refer to SOURCE."
  (if (and (stringp source) (not (eas-agent-json-text-p source))) source "SPEC.json"))

(defun eas-agent-bindings (opts)
  "The template bindings in OPTS's :data, parsed (nil when absent)."
  (let* ((data (plist-get opts :data))
         (b (if (and (stringp data) (not (equal data "-")) (not (eas-agent-json-text-p data))
                     (file-readable-p data))
                (eas-template-read-bindings data)
              (eas-agent-arg-json data))))
    (unless (eas-object-p b)
      (eas-signal "INVALID_INPUT" "--data is a JSON object keyed by template slot"
                    :option "data"))
    b))

(provide 'eas-agent-core)
;;; eas-agent-core.el ends here

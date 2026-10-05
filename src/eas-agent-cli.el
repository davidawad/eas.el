;;; eas-agent-cli.el --- bin/eas: the stateless agent verbs from a shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; bin/eas runs
;;
;;   Emacs -Q --batch -L src/eas -l eas-agent-cli -f eas-agent-cli-main -- VERB ARGS...
;;
;; ARGS are positionals then options: `--backend text', `--backend=text'
;; or a bare flag such as `--vl'.  An argument "-" reads stdin.  The
;; chart/v1 envelope is printed as JSON on stdout; the exit status is 0
;; when ok and 1 otherwise.  `--raw' prints only data on success (the
;; text chart, the SVG, the Vega-Lite spec), still the envelope on
;; failure.  Live verbs need a running Emacs, so bin/eas answers them
;; with the emacsclient command to use instead.

;;; Code:

(require 'eas-agent)

(defun eas-agent-cli--stdin ()
  "All of stdin as a string."
  ;; Not `insert-file-contents' on /dev/stdin: Emacs 29.1 rejects a pipe
  ;; there.  In batch mode `read-from-minibuffer' reads one stdin line.
  (with-temp-buffer
    (let (line)
      (while (setq line (ignore-errors (read-from-minibuffer "")))
        (insert line "\n")))
    (buffer-string)))

(defun eas-agent-cli-args (verb argv)
  "Parse command-line ARGV for VERB into `eas-agent' arguments.
Return (ARGS . RAW), RAW non-nil when --raw was given."
  (let* ((flags (ignore-errors (plist-get (eas-agent-verb verb) :flags)))
         pos opts raw)
    (while argv
      (let ((arg (pop argv)))
        (cond
         ((equal arg "--raw") (setq raw t))
         ((string-match "\\`--\\([^=]+\\)=\\(.*\\)\\'" arg)
          (setq opts (append opts (list (eas-key (match-string 1 arg)) (match-string 2 arg)))))
         ((string-prefix-p "--" arg)
          (let ((key (eas-key (substring arg 2))))
            (setq opts (append opts (list key (if (or (memq key flags) (null argv)
                                                      (string-prefix-p "--" (car argv)))
                                                  t
                                                (pop argv)))))))
         (opts (eas-signal "INVALID_INPUT"
                             (format "Positional %S after options; put positionals first" arg)))
         (t (push arg pos)))))
    (cons (append (nreverse pos) opts) raw)))

(defun eas-agent-cli-run (argv)
  "Run bin/eas ARGV; return (EXIT . OUTPUT)."
  (let* ((verb (or (car argv) "describe"))
         (entry (alist-get verb eas-agent-verbs nil nil #'equal))
         (eas-agent-stdin-function #'eas-agent-cli--stdin)
         raw
         (envelope
          (cond
           ((member verb '("-h" "--help" "help")) (eas-agent "describe" "verbs"))
           ((plist-get entry :live)
            (eas-agent-fail "INVALID_INPUT"
                              (list :code "INVALID_INPUT" :verb verb
                                    :message (format "%s reads live views; ask the running Emacs" verb))
                              nil (apply #'eas-agent-live-cmd verb (cdr argv))))
           (t (condition-case err
                  (let ((parsed (eas-agent-cli-args verb (cdr argv))))
                    (setq raw (cdr parsed))
                    (apply #'eas-agent verb (car parsed)))
                (error (eas-agent--error-envelope verb nil err)))))))
    (if (eq (plist-get envelope :ok) t)
        (cons 0 (let ((data (plist-get envelope :data)))
                  (cond ((not raw) (eas-json-pretty (eas-json-parse (eas-agent-encode envelope))))
                        ((stringp data) (if (string-suffix-p "\n" data) data (concat data "\n")))
                        ((stringp (plist-get data :output)) (concat (plist-get data :output) "\n"))
                        (t (eas-json-pretty data)))))
      (cons 1 (eas-json-pretty (eas-json-parse (eas-agent-encode envelope)))))))

(defun eas-agent-cli-main ()
  "Entry point for bin/eas: run `command-line-args-left' and exit."
  (let ((result (eas-agent-cli-run (if (equal (car command-line-args-left) "--")
                                          (cdr command-line-args-left)
                                        command-line-args-left))))
    (setq command-line-args-left nil)
    (princ (cdr result))
    (kill-emacs (car result))))

(provide 'eas-agent-cli)
;;; eas-agent-cli.el ends here

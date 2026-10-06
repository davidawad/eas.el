;;; eas-countpattern.el --- the countpattern transform: word counts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L1.  Vega's countpattern transform as an x-eas domain
;; transform: every match of a regular expression in a text field,
;; case-folded as asked and filtered by a stopword expression, counted
;; into one {text, count} row per distinct match, in the order matches
;; first appear.  The word cloud template tokenizes documents with it.
;;
;;   {"x-eas:transform": "countpattern", "field": "abstract",
;;    "pattern": "[\\w']{3,}", "case": "upper", "stopwords": "(a|the)"}
;;
;; Patterns are JavaScript regular expressions (`eas-expr-regexp-js'),
;; matched case-sensitively; stopwords match a whole word, ignoring
;; case, as Vega's do.

;;; Code:

(require 'eas-core)
(require 'eas-transform-domain)
(require 'eas-expr-regexp)

(defun eas-countpattern-regexp (pattern)
  "Emacs regexp for JavaScript regexp PATTERN, class escapes included.
\\w, \\d and \\s inside a [...] class become their ASCII ranges, as in
JavaScript, before `eas-expr-regexp-js' translates the rest."
  (let ((out nil) (i 0) (n (length pattern)) (in-class nil))
    (while (< i n)
      (let ((c (aref pattern i)))
        (cond
         ((and (eq c ?\\) (< (1+ i) n))
          (let ((d (aref pattern (1+ i))))
            (push (or (and in-class (pcase d (?w "A-Za-z0-9_") (?d "0-9") (?s " \t\n\r\f")))
                      (string c d))
                  out)
            (setq i (1+ i))))
         ((and (eq c ?\[) (not in-class)) (setq in-class t) (push "[" out))
         ((and (eq c ?\]) in-class) (setq in-class nil) (push "]" out))
         (t (push (string c) out))))
      (setq i (1+ i)))
    (eas-expr-regexp-js (apply #'concat (nreverse out)))))

(defun eas-countpattern--case (text mode)
  "TEXT folded to case MODE: \"upper\", \"lower\" or \"mixed\" (as is)."
  (pcase mode ("upper" (upcase text)) ("lower" (downcase text)) (_ text)))

(defun eas-countpattern--transform (rows params)
  "The countpattern domain transform: match counts of ROWS per PARAMS."
  (let* ((field (eas-key (plist-get params :field)))
         (re (eas-countpattern-regexp (plist-get params :pattern)))
         (stop (let ((s (plist-get params :stopwords)))
                 (and (stringp s) (not (string-empty-p s))
                      (concat "\\`\\(?:" (eas-countpattern-regexp s) "\\)\\'"))))
         (as (plist-get params :as))
         (counts (make-hash-table :test 'equal)) (order nil))
    (seq-doseq (row rows)
      (let ((text (plist-get row field)) (start 0))
        (when (stringp text)
          (setq text (eas-countpattern--case text (plist-get params :case)))
          (while (and (< start (length text))
                      (let ((case-fold-search nil)) (string-match re text start)))
            (let ((word (match-string 0 text)))
              ;; An empty match moves on one character, as String.match does.
              (setq start (max (match-end 0) (1+ start)))
              (unless (or (string-empty-p word)
                          (and stop (let ((case-fold-search t)) (string-match-p stop word))))
                (unless (gethash word counts) (push word order))
                (puthash word (1+ (gethash word counts 0)) counts)))))))
    (vconcat (mapcar (lambda (word) (list (eas-key (aref as 0)) word (eas-key (aref as 1)) (gethash word counts)))
                     (nreverse order)))))

(eas-register-transform
 "countpattern"
 :doc "One {text, count} row per distinct regexp match in a text field (Vega's countpattern)."
 :schema '(:field (:type "string" :required t :doc "the text field to tokenize")
           :pattern (:type "string" :default "[\\w']+" :doc "JavaScript regexp a token matches")
           :case (:type "string" :default "mixed" :doc "upper, lower or mixed (as is)")
           :stopwords (:type "string" :default "" :doc "regexp of whole tokens to drop, any case")
           :as (:type "array" :default ["text" "count"] :doc "output token and count fields"))
 :fn #'eas-countpattern--transform)

(provide 'eas-countpattern)
;;; eas-countpattern.el ends here

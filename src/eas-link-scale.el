;;; eas-link-scale.el --- linked views in one spec: scale domains and concat params -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4 (fc-qx1.6).  Two Vega-Lite constructs link the views of
;; one spec at compile time:
;;
;;   scale domain from a selection   "scale": {"domain": {"param": "brush"}}
;;                                   (overview + detail): while the
;;                                   interval selection holds a range,
;;                                   that range is the scale's domain and
;;                                   the view clips its marks, as in Vega.
;;   params at a concat              {"params": [{"name": "hover", "select":
;;                                   ..., "views": ["price", "rsi"]}],
;;                                   "vconcat": [...]}: the selection is
;;                                   defined in every named view (every
;;                                   view without "views"), with one
;;                                   store, so one gesture drives them all.
;;
;; Everything here reads plain spec and state data; compile asks.

;;; Code:

(require 'eas-core)

;;; scale.domain {"param": ...}

(defun eas-link-domain-ref (def)
  "The {\"param\": P} object of channel DEF's scale domain, or nil."
  (let ((domain (and (eas-object-p def) (plist-get (plist-get def :scale) :domain))))
    (and (eas-object-p domain) (stringp (plist-get domain :param)) domain)))

(defun eas-link--store-range (store channel ref)
  "STORE's [LO HI] range for CHANNEL (a keyword), as REF picks it.
REF's \"encoding\" names the selection channel; by default the same
channel, else the selection's only one."
  (when (equal (plist-get store :type) "interval")
    (let* ((fields (plist-get store :fields))
           (key (cond ((stringp (plist-get ref :encoding)) (eas-key (plist-get ref :encoding)))
                      ((plist-get store channel) channel)
                      ((= (length fields) 2) (car fields))))
           (r (and key (plist-get store key))))
      (and (vectorp r) (= (length r) 2) (numberp (aref r 0)) (numberp (aref r 1))
           (/= (aref r 0) (aref r 1))
           (vector (min (aref r 0) (aref r 1)) (max (aref r 0) (aref r 1)))))))

(defun eas-link-param-domain (units channel state)
  "Domain [LO HI] for CHANNEL of UNITS taken from a selection in STATE, or nil.
A unit's CHANNEL scale whose domain is {\"param\": P} follows P's
interval while it is not empty; empty, the data decides (Vega-Lite)."
  (seq-some (lambda (unit)
              (when-let* ((ref (eas-link-domain-ref (plist-get (plist-get unit :encoding) channel)))
                          (store (plist-get (plist-get state :params) (eas-key (plist-get ref :param)))))
                (eas-link--store-range store channel ref)))
            units))

(defun eas-link-domain-params (units)
  "Names of the params UNITS' x/y scale domains follow."
  (delete-dups
   (cl-loop for unit in units
            append (cl-loop for ch in '(:x :y)
                            for ref = (eas-link-domain-ref (plist-get (plist-get unit :encoding) ch))
                            when ref collect (plist-get ref :param)))))

(defun eas-link-plan-domain-params (plan)
  "Names of the params any view's scale domain follows in compile PLAN."
  (delete-dups (cl-loop for g in (plist-get plan :groups)
                        append (eas-link-domain-params (plist-get g :units)))))

;;; params on a concat

(defvar eas-link--scope nil
  "Selection params inherited from enclosing concats while compile walks.
A list of (PARAM . ANCESTOR-NAMES).")

(defun eas-link-scope (node)
  "`eas-link--scope' extended with concat NODE's selection params."
  (let ((names (and (stringp (plist-get node :name)) (list (plist-get node :name)))))
    (append (mapcar (lambda (entry) (cons (car entry) (append names (cdr entry)))) eas-link--scope)
            (cl-loop for p across (or (plist-get node :params) [])
                     when (plist-get p :select) collect (cons p nil)))))

(defun eas-link--names (node)
  "Names NODE answers to in a param's \"views\": its own and its layers'."
  (append (and (stringp (plist-get node :name)) (list (plist-get node :name)))
          (cl-loop for child across (or (plist-get node :layer) [])
                   append (eas-link--names child))))

(defun eas-link-scoped-params (node)
  "Inherited selection params that apply to the view rooted at NODE.
A param without \"views\" applies to every view; with it, to views
named there (or inside a concat named there).  The \"views\" key is
dropped, and a name NODE defines itself wins."
  (let ((names (eas-link--names node))
        (own (mapcar (lambda (p) (plist-get p :name)) (append (plist-get node :params) nil)))
        out)
    (dolist (entry eas-link--scope (nreverse out))
      (let* ((p (car entry)) (views (append (plist-get p :views) nil)))
        (when (and (not (member (plist-get p :name) own))
                   (not (seq-find (lambda (q) (equal (plist-get q :name) (plist-get p :name))) out))
                   (or (null views)
                       (seq-some (lambda (v) (member v (append names (cdr entry)))) views)))
          (push (eas--plist-without p :views) out))))))

(provide 'eas-link-scale)
;;; eas-link-scale.el ends here

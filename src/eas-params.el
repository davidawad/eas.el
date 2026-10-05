;;; eas-params.el --- Vega-Lite selection params: stores and tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  Interactions are Vega-Lite params, not an invented API.
;; A selection's value in view state is a plain store:
;;
;;   point     (:type "point" :fields ["t"] :values [["2026-03-02"] ...])
;;   interval  (:type "interval" :fields (:x "t") :x [LO HI] [:y [LO HI]])
;;
;; nil is the empty selection.  `eas-params-contains' decides
;; membership the way Vega-Lite does; `eas-params-with-state' installs
;; it as the compile and filter hooks so conditional encodings and
;; {"param": ...} filters follow the state.  Point selections without
;; fields or encodings key on the row identity (:_eas_row), like
;; Vega-Lite's _vgsid_.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-encode)
(require 'eas-transform)
(require 'eas-params-index)

(defun eas-params-normalize (param)
  "Return PARAM's selection definition with Vega-Lite defaults filled."
  (let* ((select (plist-get param :select))
         (select (if (stringp select) (list :type select) select))
         (type (plist-get select :type)))
    (when select
      (append (list :type type
                    :on (or (plist-get select :on) (if (equal type "point") "click" "drag"))
                    :encodings (or (plist-get select :encodings) (if (equal type "interval") ["x" "y"]))
                    :fields (plist-get select :fields)
                    :nearest (eq (plist-get select :nearest) t)
                    :toggle (if (plist-member select :toggle) (plist-get select :toggle) t)
                    :clear (if (plist-member select :clear) (plist-get select :clear) "dblclick"))))))

(defun eas-params-of (scene &optional type)
  "SCENE's selection params (plists with :name :view :def :bind), of TYPE if given."
  (seq-filter (lambda (p) (and (plist-get p :def) (or (null type) (equal type (plist-get (plist-get p :def) :type)))))
              (mapcar (lambda (p) (list :name (plist-get p :name) :view (plist-get p :view)
                                        :bind (plist-get p :bind) :def (eas-params-normalize p)))
                      (append (plist-get scene :params) nil))))

(defun eas-params-channel-field (scene view-id channel)
  "Field mapped to CHANNEL (\"x\") in SCENE's view VIEW-ID."
  (when-let* ((view (seq-find (lambda (v) (equal (plist-get v :id) view-id)) (plist-get scene :views)))
              (scale (plist-get (plist-get view :scales) (eas-key channel))))
    (plist-get scale :field)))

(defun eas-params-point-fields (scene param)
  "The fields a point selection PARAM stores in SCENE."
  (let ((def (plist-get param :def)))
    (cond ((plist-get def :fields) (append (plist-get def :fields) nil))
          ((plist-get def :encodings)
           (delq nil (mapcar (lambda (ch) (eas-params-channel-field scene (plist-get param :view) ch))
                             (plist-get def :encodings))))
          (t (list (eas-key-name eas-params-row-key))))))

(defconst eas-params-row-key :_eas_row "Row identity key, as tagged by compile.")

(defun eas-params-point-store (fields rows)
  "A point selection store over FIELDS holding the tuples of ROWS."
  (when rows
    (list :type "point" :fields (vconcat fields)
          :values (vconcat (mapcar (lambda (row) (vconcat (mapcar (lambda (f) (plist-get row (eas-key f))) fields)))
                                   rows)))))

(defun eas-params-toggle (store fields row)
  "STORE with ROW's tuple over FIELDS toggled in or out."
  (let* ((tuple (vconcat (mapcar (lambda (f) (plist-get row (eas-key f))) fields)))
         (values (append (plist-get store :values) nil))
         (values (if (member tuple values) (delete tuple values) (append values (list tuple)))))
    (when values (list :type "point" :fields (vconcat fields) :values (vconcat values)))))

(defun eas-params--same (a b)
  "Equality for stored values: dates compare as instants."
  (or (equal a b)
      (and (numberp a) (numberp b) (= a b))
      (let ((ta (eas-time-parse a)) (tb (eas-time-parse b)))
        (and ta tb (stringp (if (stringp a) a b)) (= ta tb)))))

(defun eas-params--number (v)
  "V as a number for interval tests (dates become epoch ms)."
  (if (numberp v) v (eas-time-parse v)))

(defun eas-params-contains (store row)
  "Non-nil when ROW is in selection STORE (a non-empty store)."
  (pcase (plist-get store :type)
    ("point"
     (let ((fields (append (plist-get store :fields) nil)))
       (seq-some (lambda (tuple)
                   (cl-loop for f in fields for v across tuple
                            always (eas-params--same (plist-get row (eas-key f)) v)))
                 (plist-get store :values))))
    ("interval"
     (cl-loop for (channel field) on (plist-get store :fields) by #'cddr
              for range = (plist-get store channel)
              for v = (eas-params--number (plist-get row (eas-key field)))
              always (or (null range)
                         (and v (<= (min (aref range 0) (aref range 1)) v (max (aref range 0) (aref range 1)))))))
    (_ nil)))

(defun eas-params-test (state name row empty)
  "Whether ROW is in selection NAME under view STATE; EMPTY for empty selections."
  (let ((store (plist-get (plist-get state :params) (eas-key name))))
    (if (null store) empty (eas-params-contains store row))))

(defmacro eas-params-with-state (state &rest body)
  "Run BODY with selection hooks answering from view STATE."
  (declare (indent 1))
  (let ((s (make-symbol "state")))
    `(let* ((,s ,state)
            (eas-encode-param-test-function
             (lambda (name row empty) (eas-params-test ,s name row empty)))
            (eas-transform-param-predicate
             (lambda (name row _env empty) (eas-params-test ,s name row empty)))
            (eas-transform-param-filter-function
             (lambda (name rows empty)
               (eas-params-index-filter (plist-get (plist-get ,s :params) (eas-key name)) rows empty))))
       ,@body)))

(defun eas-params-summary (store)
  "A short human summary of selection STORE, or \"empty\"."
  (pcase (plist-get store :type)
    ("point" (format "%d value%s of %s" (length (plist-get store :values))
                     (if (= 1 (length (plist-get store :values))) "" "s")
                     (mapconcat #'identity (plist-get store :fields) ",")))
    ("interval" (mapconcat (lambda (ch)
                             (let ((r (plist-get store ch)))
                               (format "%s %s..%s" (plist-get (plist-get store :fields) ch)
                                       (eas-params--fmt (aref r 0)) (eas-params--fmt (aref r 1)))))
                           (seq-filter (lambda (ch) (plist-get store ch)) '(:x :y)) "; "))
    (_ "empty")))

(defun eas-params--fmt (v)
  "Format interval bound V (ISO date for epoch ms that look like dates)."
  (if (and (numberp v) (> (abs v) 1e11)) (eas-time-iso v) (format "%s" v)))

(provide 'eas-params)
;;; eas-params.el ends here

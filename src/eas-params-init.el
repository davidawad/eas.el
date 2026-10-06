;;; eas-params-init.el --- selection params' initial values -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6.  A Vega-Lite selection may start non-empty: "value" on a
;; point selection is an array of objects keyed by field or encoding
;; channel, on an interval selection an object of channel ranges.
;; Vega draws that initial state, so a static render and a freshly
;; opened view must too.  `eas-params-value-store' turns a param's
;; Vega-Lite value into the store of `eas-params'; date-time objects
;; ({"year": 2005, "month": 1, "date": 1}) become epoch ms.
;; `eas-params-initial-state' collects the stores of a scene's params.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-params)

(defconst eas-params-init--months
  '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
  "Month name prefixes, for date-time objects.")

(defun eas-params-datetime (value)
  "VALUE as epoch ms when it is a Vega-Lite date-time object, else VALUE."
  (if (not (and (consp value) (keywordp (car value))
                (seq-some (lambda (k) (plist-member value k)) '(:year :month :date :hours))))
      value
    (let* ((month (plist-get value :month))
           (month (if (stringp month)
                      (1+ (or (seq-position eas-params-init--months (downcase (substring month 0 3))) 0))
                    month))
           (eas-time-zone (if (eq (plist-get value :utc) t) nil eas-time-zone)))
      (eas-time-ms (or (plist-get value :year) 2012) month (plist-get value :date)
                   (plist-get value :hours) (plist-get value :minutes) (plist-get value :seconds)
                   (plist-get value :milliseconds)))))

(defun eas-params--key-field (scene param key)
  "The field a value KEY (field name or channel) names for PARAM in SCENE."
  (let ((name (eas-key-name key)))
    (or (and (member name '("x" "y" "color" "fill" "stroke" "size" "opacity" "shape"))
             (member name (append (plist-get (plist-get param :def) :encodings) nil))
             (eas-params-channel-field scene (plist-get param :view) name))
        name)))

(defun eas-params-value-store (scene param value)
  "The selection store for PARAM (from `eas-params-of') holding VALUE.
VALUE is in Vega-Lite's form; nil when it selects nothing.  SCENE
gives the fields of the param's keys and channels."
  (pcase (plist-get (plist-get param :def) :type)
    ("point"
     (let* ((tuples (cond ((vectorp value) (append value nil)) ((consp value) (list value))))
            (keys (and tuples (eas-plist-keys (car tuples))))
            (fields (mapcar (lambda (k) (eas-params--key-field scene param k)) keys)))
       (when tuples
         (list :type "point" :fields (vconcat fields)
               :values (vconcat (mapcar (lambda (tu) (vconcat (mapcar (lambda (k) (eas-params-datetime (plist-get tu k)))
                                                                      keys)))
                                        tuples))))))
    ("interval"
     (let (fields ranges)
       (dolist (ch '(:x :y))
         (when-let* ((r (plist-get value ch))
                     (field (eas-params-channel-field scene (plist-get param :view) (eas-key-name ch))))
           (setq fields (append fields (list ch field))
                 ranges (append ranges (list ch (vconcat (mapcar #'eas-params-datetime r)))))))
       (when fields (append (list :type "interval" :fields fields) ranges))))))

(defun eas-params-initial-state (scene)
  "View state holding the initial values of SCENE's selection params, or nil."
  (let (params)
    (dolist (p (append (plist-get scene :params) nil))
      (when (and (plist-get p :select) (plist-member p :value))
        (when-let* ((param (car (seq-filter (lambda (q) (equal (plist-get q :name) (plist-get p :name)))
                                            (eas-params-of (list :params (vector p))))))
                    (store (eas-params-value-store scene param (plist-get p :value))))
          (setq params (eas-plist-put params (eas-key (plist-get p :name)) store)))))
    (when params (list :params params))))

(defun eas-params-set (state scene name value)
  "Return STATE with param NAME of SCENE at VALUE, as a bound input would.
VALUE is in Vega-Lite's form; a selection's becomes its store.
Signals EVENT_INVALID for a param SCENE does not have."
  (let ((raw (seq-find (lambda (p) (equal (plist-get p :name) name)) (plist-get scene :params))))
    (unless raw
      (eas-signal "EVENT_INVALID" (format "No param %S in this view" name) :field "param"
                  :params (vconcat (delete-dups (mapcar (lambda (p) (plist-get p :name)) (plist-get scene :params))))))
    (eas-plist-put state :params
                   (eas-plist-put (plist-get state :params) (eas-key name)
                                  (if (plist-get raw :select)
                                      (eas-params-value-store scene (car (eas-params-of (list :params (vector raw))))
                                                              (if (eq value :null) nil value))
                                    value)))))

(provide 'eas-params-init)
;;; eas-params-init.el ends here

;;; eas-encode.el --- encoding channels: normalize, derive, evaluate -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A unit view's encoding becomes a plist of normalized
;; channel definitions.  Encoding-level timeUnit, bin and aggregate are
;; turned into the implicit transforms Vega-Lite derives, with
;; Vega-Lite's output field names, so every later step only reads plain
;; fields.  Conditions are evaluated through
;; `eas-encode-param-test-function', which the runtime replaces with
;; selection semantics.

;;; Code:

(require 'eas-core)
(require 'eas-data)
(require 'eas-expr)
(require 'eas-transform)
(require 'eas-bins)

(defvar eas-encode-param-test-function
  (lambda (_param _row empty) empty)
  "Function (PARAM ROW EMPTY) -> non-nil when ROW is in selection PARAM.
EMPTY is the answer for an empty selection.  The runtime binds this
around compile with the view's current param state.")

(defun eas-encode--infer-type (def _rows)
  "Return DEF's measurement type, defaulted as Vega-Lite's defaultType does.
Vega-Lite never looks at the data: a field with no hint is nominal."
  (or (plist-get def :type)
      (let ((agg (plist-get def :aggregate))
            (scale-type (plist-get (plist-get def :scale) :type)))
        (cond ((vectorp (plist-get def :sort)) "ordinal")
              ((plist-get def :timeUnit) "temporal")
              ((or (eas-true-p (plist-get def :bin))
                   (and agg (not (eas-encode--arg-op agg))))
               "quantitative")
              ((member scale-type '("linear" "log" "pow" "sqrt" "symlog" "quantize" "quantile" "threshold"))
               "quantitative")
              ((member scale-type '("time" "utc")) "temporal")
              (t "nominal")))))

(defun eas-encode--def (def rows)
  "Normalize one channel DEF against ROWS."
  (if (not (eas-object-p def)) def
    (let ((out def))
      (when (or (plist-get def :field) (plist-get def :aggregate))
        (setq out (eas-plist-put out :type (eas-encode--infer-type def rows))))
      ;; Vega-Lite types an untyped datum by its value.
      (when (and (plist-member def :datum) (not (plist-get def :type)))
        (let ((d (plist-get def :datum)))
          (cond ((numberp d) (setq out (eas-plist-put out :type "quantitative")))
                ((stringp d) (setq out (eas-plist-put out :type "nominal")))
                ((eas-object-p d) (setq out (eas-plist-put out :type "temporal"))))))
      (when-let* ((c (plist-get def :condition)))
        (setq out (eas-plist-put out :condition
                                   (if (vectorp c) (vconcat (mapcar (lambda (d) (eas-encode--def d rows)) c))
                                     (eas-encode--def c rows)))))
      out)))

(defun eas-encode-normalize (encoding rows)
  "Normalize ENCODING's channel definitions against ROWS."
  (cl-loop for (channel def) on encoding by #'cddr
           append (list channel
                        (if (vectorp def)
                            (vconcat (mapcar (lambda (d) (eas-encode--def d rows)) def))
                          (eas-encode--def def rows)))))

;;; Implicit transforms

(defun eas-encode--bin-name (def)
  "Vega-Lite's output field for binned DEF."
  (let ((opts (plist-get def :bin)))
    (format "bin_maxbins_%s_%s" (or (and (eas-object-p opts) (plist-get opts :maxbins)) 10)
            (plist-get def :field))))

(defun eas-encode-derive (encoding rows env)
  "Apply ENCODING's implicit timeUnit, bin and aggregate to ROWS.
Return (ENCODING' . ROWS') where ENCODING' points at derived fields.
ENV holds param values for expressions."
  (let ((enc encoding) (transforms nil))
    ;; timeUnit and bin
    (cl-loop for (channel def) on encoding by #'cddr
             when (and (eas-object-p def) (plist-get def :field))
             do (cond
                 ((plist-get def :timeUnit)
                  (let ((as (format "%s_%s" (plist-get def :timeUnit) (plist-get def :field))))
                    (push (list :timeUnit (plist-get def :timeUnit) :field (plist-get def :field) :as as)
                          transforms)
                    (setq enc (eas-plist-put enc channel
                                               (append (list :field as :source (plist-get def :field)
                                                             :derived "timeUnit")
                                                       (eas--plist-without
                                                        (eas--plist-without def :field) :timeUnit))))))
                 ((eas-bins-binned-p def)
                  (setq enc (eas-plist-put enc channel
                                           (eas-bins-binned-def def (plist-get encoding (if (eq channel :y) :y2 :x2))))))
                 ((eas-true-p (plist-get def :bin))
                  (let ((as (eas-encode--bin-name def)))
                    (push (list :bin (plist-get def :bin) :field (plist-get def :field) :as as) transforms)
                    (setq enc (eas-plist-put enc channel
                                               (append (list :field as :source (plist-get def :field)
                                                             :derived "bin" :bin-end (concat as "_end"))
                                                       (eas--plist-without
                                                        (eas--plist-without def :field) :bin))))))))
    (let ((rows (eas-transform-run (vconcat (nreverse transforms)) rows env "/encoding")))
      (eas-encode--aggregate enc rows))))

(defun eas-encode--arg-op (op)
  "(OP-NAME . FIELD) of an {\"argmin\": FIELD} or {\"argmax\": FIELD} aggregate, or nil."
  (and (consp op) (keywordp (car op))
       (cond ((plist-get op :argmin) (cons "argmin" (plist-get op :argmin)))
             ((plist-get op :argmax) (cons "argmax" (plist-get op :argmax))))))

(defun eas-encode--flatten-args (rows args)
  "ROWS with each ARGS (AS OBJECT-KEY FIELD-KEY) read out of its arg row."
  (if (null args) rows
    (vconcat (seq-map (lambda (row)
                        (let ((out row))
                          (pcase-dolist (`(,as ,obj ,field) args)
                            (setq out (eas-plist-put out as (let ((o (plist-get row obj)))
                                                              (if (consp o) (plist-get o field) :null)))))
                          (dolist (obj (delete-dups (mapcar #'cadr args))) (setq out (eas--plist-without out obj)))
                          out))
                      rows))))

(defun eas-encode--aggregate (encoding rows)
  "Apply ENCODING's aggregate channels to ROWS; return (ENCODING . ROWS).
An argmin or argmax aggregate reads its field from the group's row
with the least or greatest argument, as Vega-Lite's argmin_ARG.FIELD."
  (let (ops groupby args (enc encoding))
    (cl-loop for (channel def) on encoding by #'cddr
             for defs = (if (vectorp def) (append def nil) (list def))
             do (dolist (d defs)
                  (when (eas-object-p d)
                    (if-let* ((op (plist-get d :aggregate)))
                        (let* ((arg (eas-encode--arg-op op))
                               (as (cond (arg (format "%s_%s_%s" (car arg) (cdr arg) (plist-get d :field)))
                                         ((and (equal op "count") (null (plist-get d :field))) "__count")
                                         (t (format "%s_%s" op (plist-get d :field))))))
                          (if (null arg)
                              (push (list :op op :field (plist-get d :field) :as as) ops)
                            (let ((obj (format "%s_%s" (car arg) (cdr arg))))
                              (push (list :op (car arg) :field (cdr arg) :as obj) ops)
                              (push (list (eas-key as) (eas-key obj) (eas-key (plist-get d :field))) args)))
                          (unless (vectorp def)
                            (setq enc (eas-plist-put
                                       enc channel (append (list :field as :source (plist-get d :field)
                                                                 :derived "aggregate" :op op
                                                                 :arg (let ((a (plist-get d :aggregate)))
                                                                        (and (eas-object-p a)
                                                                             (or (plist-get a :argmax) (plist-get a :argmin)))))
                                                           (eas--plist-without
                                                            (eas--plist-without d :field) :aggregate))))))
                      (dolist (f (let ((d (eas-encode-data-def d))) (list (plist-get d :field) (plist-get d :bin-end))))
                        (when (and f (not (member f groupby))) (push f groupby)))))))
    (if (null ops)
        (cons encoding rows)
      (cons enc (eas-encode--flatten-args
                 (eas-transform-aggregate
                  (list :aggregate (vconcat (delete-dups (nreverse ops)))
                        :groupby (vconcat (nreverse groupby)))
                  rows "/encoding")
                 args)))))

;;; Evaluation

(defun eas-encode-field (def)
  "The row key DEF reads, or nil."
  (and (eas-object-p def) (plist-get def :field) (eas-key (plist-get def :field))))

(defun eas-encode-raw (def row)
  "DEF's raw data value for ROW (field, datum or nil)."
  (cond ((not (eas-object-p def)) nil)
        ((plist-get def :field) (plist-get row (eas-encode-field def)))
        ((plist-member def :datum) (plist-get def :datum))))

(defun eas-encode--condition-holds (cond row env)
  "Non-nil when condition COND applies to ROW."
  (cond
   ((plist-get cond :param)
    (funcall eas-encode-param-test-function (plist-get cond :param) row
             (not (eq (plist-get cond :empty) :false))))
   ((plist-get cond :test) (eas-transform-predicate (plist-get cond :test) row env))
   (t nil)))

(defun eas-encode-active (def row env)
  "Return the definition that applies to ROW: a matching condition or DEF."
  (let ((conds (plist-get def :condition)))
    (or (seq-find (lambda (c) (eas-encode--condition-holds c row env))
                  (cond ((vectorp conds) conds) (conds (list conds))))
        def)))

(defun eas-encode-data-def (def)
  "DEF, or for a value DEF its first field condition (which owns the scale)."
  (if (or (not (eas-object-p def)) (plist-get def :field) (plist-member def :datum)) def
    (let ((c (plist-get def :condition)))
      (seq-find (lambda (d) (and (eas-object-p d) (plist-get d :field)))
                (if (vectorp c) c (list c))))))

(defun eas-encode-discrete-p (def)
  "Non-nil when DEF's type is nominal or ordinal."
  (member (plist-get def :type) '("nominal" "ordinal")))

;;; Titles and tooltips

(defvar eas-encode-count-title nil
  "config.countTitle while compiling (set by `eas-compile-scene'), for callers
that have no config at hand.")

(defun eas-encode-title (def &optional config)
  "Vega-Lite's default title for DEF, or DEF's explicit :title.
CONFIG's countTitle names a count of records."
  (let ((title (plist-get def :title)))
    (cond
     ((stringp title) title)
     ((memq title '(:null :false)) nil)
     ((and (equal (plist-get def :derived) "aggregate") (plist-get def :arg))
      (format "%s for %s %s" (plist-get def :source) (if (equal (plist-get def :op) "argmax") "max" "min")
              (plist-get def :arg)))
     ((equal (plist-get def :derived) "aggregate")
      (if (and (equal (plist-get def :op) "count") (null (plist-get def :source)))
          (let ((c (plist-get config :countTitle)))
            (cond ((stringp c) c) (eas-encode-count-title) (t "Count of Records")))
        ;; Vega-Lite's verbal title: titleCase(op) of field, FIELD for min ARG.
        (let* ((op (plist-get def :op)) (arg (eas-encode--arg-op op)))
          (if arg (format "%s for %s %s" (plist-get def :source) (if (equal (car arg) "argmin") "min" "max") (cdr arg))
            (format "%s%s of %s" (upcase (substring op 0 1)) (substring op 1) (plist-get def :source))))))
     ((equal (plist-get def :derived) "bin") (format "%s (binned)" (plist-get def :source)))
     ((equal (plist-get def :derived) "timeUnit")
      (format "%s (%s)" (plist-get def :source)
              (string-join (eas-time-unit-components (car (split-string (plist-get def :field) "_"))) "-")))
     (t (plist-get def :field)))))

(defun eas-encode-format-value (def value)
  "Format VALUE for a tooltip per DEF (temporal as \"%b %d, %Y\")."
  (cond
   ((memq value '(nil :null)) "null")
   ((and (equal (plist-get def :type) "temporal") (eas-time-parse value))
    (let ((system-time-locale "C"))
      (eas-time-format (eas-time-parse value) "%b %d, %Y")))
   (t (eas-expr--string value))))

(defun eas-encode-tooltip (encoding mark row)
  "Return ROW's tooltip under ENCODING and MARK, or nil.
The tooltip is a vector of (:title TITLE :value FORMATTED) in order."
  (let* ((tip (plist-get encoding :tooltip))
         (defs (cond ((vectorp tip) (append tip nil))
                     ((and tip (eas-object-p tip) (plist-get tip :field)) (list tip))
                     ((eas-true-p (plist-get mark :tooltip))
                      (cl-loop for (ch d) on encoding by #'cddr
                               when (and (memq ch '(:x :y :x2 :y2 :color :fill :stroke :size :text :theta :radius))
                                         (eas-object-p d) (plist-get d :field))
                               collect d)))))
    (when defs
      (vconcat (mapcar (lambda (d)
                         ;; A stacked channel shows its own value, not the stack's end.
                         (when (plist-get d :stack-field)
                           (setq d (plist-put (copy-sequence d) :field (plist-get d :stack-field))))
                         (list :title (or (eas-encode-title d) (plist-get d :field))
                               :value (eas-encode-format-value d (eas-encode-raw d row))))
                       defs)))))

(provide 'eas-encode)
;;; eas-encode.el ends here

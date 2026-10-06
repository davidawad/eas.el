;;; eas-layer.el --- empty inherited definitions and numeric coercion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Two Vega-Lite rules for the encodings a layer inherits:
;;
;; - A definition that encodes nothing (an inherited {"type": ...,
;;   "scale": ...} with no field, datum, value, condition or aggregate)
;;   is dropped, as Vega-Lite drops it.
;; - Numeric strings in a quantitative field become numbers, as Vega's
;;   quantitative scales coerce them.

;;; Code:

(require 'eas-core)
(require 'eas-encode)

(defun eas-layer-drop-empty (encoding)
  "ENCODING without definitions that encode nothing.
A layer's inherited {\"type\": ..., \"scale\": ...} with no field, datum,
value, condition or aggregate is dropped, as Vega-Lite drops it, and
so is a channel set to null."
  (cl-loop for (ch d) on encoding by #'cddr
           unless (or (eq d :null)
                      (and (eas-object-p d) (not (vectorp d))
                       (not (or (plist-get d :field) (plist-member d :datum) (plist-member d :value)
                                (plist-get d :condition) (plist-get d :aggregate)))))
           append (list ch d)))

(defun eas-layer-coerce (rows encoding)
  "ROWS with numeric strings in ENCODING's quantitative fields made numbers.
Vega's quantitative scales coerce \"1565\" to 1565; so does compile."
  (let ((keys (cl-loop for (_ d) on encoding by #'cddr
                       when (and (eas-object-p d) (stringp (plist-get d :field))
                                 (or (equal (plist-get d :type) "quantitative")
                                     (eas-true-p (plist-get d :bin))))
                       collect (eas-key (plist-get d :field)))))
    (if (null keys) rows
      (vconcat (seq-map (lambda (row)
                          (let ((out row))
                            (dolist (k keys out)
                              (let ((v (plist-get out k)))
                                (when (and (stringp v) (string-match-p "\\`[ \t]*[-+]?[0-9.]+\\(?:[eE][-+]?[0-9]+\\)?[ \t]*\\'" v))
                                  (when (eq out row) (setq out (copy-sequence row)))
                                  (setq out (plist-put out k (string-to-number v))))))))
                        rows)))))

(provide 'eas-layer)
;;; eas-layer.el ends here

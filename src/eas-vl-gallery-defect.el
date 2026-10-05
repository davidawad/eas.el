;;; eas-vl-gallery-defect.el --- reference defects the gallery oracle masks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A committed reference is bin/chart's picture, and bin/chart is
;; Vega-Lite 6.4.1: where Vega-Lite itself mis-draws an example, the
;; reference is wrong and a correct native render cannot match it.
;; Such an example's status.json entry may record the defect:
;;
;;   "ref_defect": {"why": "what Vega-Lite gets wrong, and how it was verified",
;;                  "set": [{"path": "/layer/0/mark/opacity", "value": 0}]}
;;
;; Each "set" writes VALUE at the JSON pointer PATH of the spec (objects
;; on the way are created), so the image oracle compares a native render
;; that leaves out exactly what Vega-Lite drops, while the full spec
;; still renders natively for both backends and is checked for overlap.
;; A defect without a "why" is refused: it is a written, per-spec
;; reason, never a looser threshold.

;;; Code:

(require 'eas-core)

(defun eas-vl-gallery-defect--set (value keys new)
  "VALUE with NEW at the path KEYS (keywords or indices)."
  (if (null keys) new
    (let ((k (car keys)))
      (if (and (vectorp value) (integerp k))
          (let ((v (copy-sequence value)))
            (aset v k (eas-vl-gallery-defect--set (aref v k) (cdr keys) new))
            v)
        ;; Mark shorthand "bar" is the object {"type": "bar"}.
        (let ((obj (cond ((eas-object-p value) value) ((stringp value) (list :type value)))))
          (eas-plist-put obj k
                         (eas-vl-gallery-defect--set (plist-get obj k) (cdr keys) new)))))))

(defun eas-vl-gallery-defect--keys (path)
  "JSON pointer PATH as a list of keywords and indices."
  (mapcar (lambda (s) (if (string-match-p "\\`[0-9]+\\'" s) (string-to-number s) (eas-key s)))
          (cdr (split-string path "/"))))

(defun eas-vl-gallery-defect-apply (spec defect)
  "SPEC with reference DEFECT's \"set\" entries written into it."
  (unless (and (stringp (plist-get defect :why)) (not (string-empty-p (plist-get defect :why))))
    (eas-signal "INVALID_INPUT" "A ref_defect needs a \"why\": the written reason the reference is wrong"
                :path "/ref_defect/why"))
  (seq-reduce (lambda (s set)
                (eas-vl-gallery-defect--set s (eas-vl-gallery-defect--keys (plist-get set :path))
                                            (plist-get set :value)))
              (plist-get defect :set) spec))

(defun eas-vl-gallery-defect (status name)
  "The ref_defect of NAME in parsed STATUS, or nil."
  (plist-get (plist-get status (eas-key name)) :ref_defect))

(provide 'eas-vl-gallery-defect)
;;; eas-vl-gallery-defect.el ends here

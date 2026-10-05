;;; eas-overlay.el --- line and point overlays on line and area marks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite draws mark.point (on line and area) and
;; mark.line (on area) as extra marks: its pathoverlay normalizer
;; rewrites the unit into a layer of the path mark, a line overlay and
;; a point overlay.  `eas-overlay-expand' does the same rewrite before
;; compile, so the overlays are ordinary units: the area takes opacity
;; 0.7 unless the spec sets one, overlays follow the stacked measure,
;; and the point overlay is filled at opacity 1.  An overlay value of
;; true means {}, "transparent" means {opacity: 0}, an object is merged
;; into the overlay mark.

;;; Code:

(require 'eas-core)

(defun eas-overlay--def (value)
  "The overlay mark properties VALUE asks for, or nil for none."
  (cond ((eq value t) '(:_eas_overlay t))
        ((equal value "transparent") '(:opacity 0))
        ((eas-object-p value) value)))

(defun eas-overlay--pick (mark keys)
  "MARK's properties among KEYS."
  (cl-loop for k in keys when (plist-member mark k) append (list k (plist-get mark k))))

(defun eas-overlay--encoding (type encoding)
  "ENCODING for the overlays of a TYPE mark: no x2/y2, stacked like the path."
  (let* ((enc (eas--plist-without (eas--plist-without encoding :x2) :y2))
         (quant (lambda (ch) (let ((d (plist-get enc ch))) (and (eas-object-p d) (equal (plist-get d :type) "quantitative")))))
         (measure (cond ((funcall quant :y) :y) ((funcall quant :x) :x)))
         (by (seq-some (lambda (ch) (let ((d (plist-get enc ch)))
                                      (and (eas-object-p d) (plist-get d :field)
                                           (member (plist-get d :type) '("nominal" "ordinal" nil)))))
                       '(:color :fill :detail)))
         (mdef (and measure (plist-get enc measure))))
    (if (and (equal type "area") mdef by (not (plist-member mdef :stack)))
        (eas-plist-put enc measure (append mdef (list :stack "zero")))
      enc)))

(defun eas-overlay--unit (node)
  "NODE rewritten as a layer when its mark asks for overlays, else nil."
  (let* ((mark (plist-get node :mark))
         (type (and (eas-object-p mark) (plist-get mark :type)))
         (point (and (member type '("line" "area" "trail")) (eas-overlay--def (plist-get mark :point))))
         (line (and (equal type "area") (eas-overlay--def (plist-get mark :line)))))
    (when (or point line)
      (let* ((path (eas--plist-without (eas--plist-without mark :point) :line))
             (path (if (and (equal type "area") (not (plist-member path :opacity))
                            (not (plist-member path :fillOpacity)))
                       (append path (list :opacity 0.7))
                     path))
             (encoding (plist-get node :encoding))
             (over (eas-overlay--encoding type encoding))
             (strip (lambda (def) (eas--plist-without def :_eas_overlay)))
             (layers (delq nil
                           (list (append (let ((p (plist-get node :params))) (and p (list :params p)))
                                         (list :mark path :encoding encoding))
                                 (when line
                                   (list :mark (append (list :type "line")
                                                       (eas-overlay--pick path '(:clip :interpolate :tension :tooltip))
                                                       (funcall strip line))
                                         :encoding over))
                                 (when point
                                   (list :mark (append (list :type "point" :opacity 1 :filled t)
                                                       (eas-overlay--pick path '(:clip :tooltip))
                                                       (funcall strip point))
                                         :encoding over))))))
        (append (cl-reduce #'eas--plist-without '(:mark :encoding :params) :initial-value node)
                (list :layer (vconcat layers)))))))

(defun eas-overlay-expand (spec)
  "SPEC with every path mark's point/line overlays expanded into layers."
  (cond
   ((not (eas-object-p spec)) spec)
   (t (let ((node (or (eas-overlay--unit spec) spec)))
        (cl-loop for (k v) on node by #'cddr
                 append (list k (if (and (memq k '(:layer :vconcat :hconcat)) (vectorp v))
                                    (vconcat (mapcar #'eas-overlay-expand v))
                                  v)))))))

(provide 'eas-overlay)
;;; eas-overlay.el ends here

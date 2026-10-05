;;; eas-spec-props-lower.el --- per-legend and title styles lowered to config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L2 (fc-qx1.43), an `eas-spec-rewrite-functions' entry.  The
;; renderers style legends and the chart title from config.legend and
;; config.title.  A legend property (labelColor, symbolType, ...) set on
;; every legend of a spec with one value means exactly what the same
;; property in config.legend means, so it moves there; likewise the
;; chart title's anchor when no view below carries a title of its own.
;; Legends that disagree keep their properties, and check names them.
;; The rewrite is idempotent.

;;; Code:

(require 'eas-core)

(defconst eas-spec-props-lower--legend-keys
  '(:gradientThickness :labelColor :labelFontSize :labelFontWeight :labelFont :labelFontStyle :labelOpacity
    :labelOffset :rowPadding :symbolSize :symbolStrokeWidth :symbolType :titleColor :titleFontSize
    :titleFontWeight :titleFont :titleFontStyle :titleOpacity :titlePadding)
  "Legend properties the renderers read from config.legend.")

(defconst eas-spec-props-lower--title-keys '(:anchor)
  "Title properties compile reads from config.title.")

(defconst eas-spec-props-lower--legend-channels
  '(:color :fill :stroke :size :shape :opacity :fillOpacity :strokeOpacity :strokeWidth :strokeDash)
  "Channels that draw a legend.")

(defun eas-spec-props-lower--views (spec)
  "SPEC and every view nested in it."
  (cons spec (cl-loop for key in '(:layer :vconcat :hconcat)
                      for children = (plist-get spec key)
                      when (vectorp children)
                      append (cl-loop for c across children when (eas-object-p c)
                                      append (eas-spec-props-lower--views c)))))

(defun eas-spec-props-lower--legend-defs (spec)
  "The channel definitions of SPEC that draw a legend."
  (cl-loop for view in (eas-spec-props-lower--views spec)
           for enc = (plist-get view :encoding)
           when (eas-object-p enc)
           append (cl-loop for ch in eas-spec-props-lower--legend-channels
                           for def = (plist-get enc ch)
                           when (and (eas-object-p def) (or (plist-get def :field) (plist-get def :aggregate))
                                     (not (memq (plist-get def :legend) '(:null :false))))
                           collect def)))

(defun eas-spec-props-lower--map-legends (spec fn)
  "SPEC with FN applied to every legend-drawing channel definition."
  (let ((out (copy-sequence spec)))
    (when (eas-object-p (plist-get out :encoding))
      (let ((enc (copy-sequence (plist-get out :encoding))))
        (dolist (ch eas-spec-props-lower--legend-channels)
          (let ((def (plist-get enc ch)))
            (when (and def (eas-object-p def) (plist-get def :legend) (eas-object-p (plist-get def :legend)))
              (setq enc (plist-put enc ch (funcall fn def))))))
        (setq out (plist-put out :encoding enc))))
    (dolist (key '(:layer :vconcat :hconcat))
      (when (vectorp (plist-get out key))
        (setq out (plist-put out key (vconcat (mapcar (lambda (c) (if (eas-object-p c) (eas-spec-props-lower--map-legends c fn) c))
                                                      (plist-get out key)))))))
    out))

(defun eas-spec-props-lower--config (spec block key value)
  "SPEC with config.BLOCK.KEY set to VALUE."
  (let* ((config (copy-sequence (and (eas-object-p (plist-get spec :config)) (plist-get spec :config))))
         (obj (copy-sequence (and (eas-object-p (plist-get config block)) (plist-get config block)))))
    (plist-put (copy-sequence spec) :config (plist-put config block (plist-put obj key value)))))

(defun eas-spec-props-lower--legends (spec)
  "SPEC with the legend properties every legend shares moved to config.legend."
  (let ((defs (eas-spec-props-lower--legend-defs spec)))
    (dolist (key eas-spec-props-lower--legend-keys)
      (let ((values (mapcar (lambda (d) (let ((l (plist-get d :legend))) (and (eas-object-p l) (plist-member l key)
                                                                               (list (plist-get l key)))))
                            defs)))
        (when (and values (car values) (seq-every-p (lambda (v) (equal v (car values))) values))
          (setq spec (eas-spec-props-lower--config
                      (eas-spec-props-lower--map-legends
                       spec (lambda (def) (plist-put (copy-sequence def) :legend
                                                     (eas--plist-without (plist-get def :legend) key))))
                      :legend key (car (car values)))))))
    spec))

(defun eas-spec-props-lower--title (spec)
  "SPEC with its title's anchor moved to config.title, when no nested view
has a title of its own."
  (let ((title (plist-get spec :title)))
    (if (or (not (eas-object-p title))
            (seq-some (lambda (v) (plist-get v :title)) (cdr (eas-spec-props-lower--views spec))))
        spec
      (dolist (key eas-spec-props-lower--title-keys)
        (when (plist-member title key)
          (let ((value (plist-get title key)))
            (setq title (eas--plist-without title key)
                  spec (eas-spec-props-lower--config (plist-put (copy-sequence spec) :title title)
                                                     :title key value)))))
      spec)))

(defun eas-spec-props-lower (spec)
  "SPEC with shared legend and title styles moved into its config."
  (if (not (eas-object-p spec)) spec
    (eas-spec-props-lower--title (eas-spec-props-lower--legends spec))))

(defvar eas-spec-rewrite-functions)
(add-hook 'eas-spec-rewrite-functions #'eas-spec-props-lower t)

(provide 'eas-spec-props-lower)
;;; eas-spec-props-lower.el ends here

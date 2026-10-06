;;; eas-mode-strip.el --- the values strip under a live chart -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 glue (fc-qx1.34).  Every eas buffer ends with one line,
;; under the image (GUI) or the character grid (terminal), that reads
;; the chart's series at the pointer's column, or their latest values
;; (`eas-strip').  It updates on every pointer move, before the idle
;; redraw, and needs no mode switch.  The line carries `eas-strip' and
;; a keymap that makes pointer motion over it leave the chart.

;;; Code:

(require 'eas-view)
(require 'eas-strip)

(defface eas-strip '((t :inherit shadow))
  "Face of the values strip under a live chart."
  :group 'eas)

(defvar eas-mode-strip-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-movement] #'eas-mode-strip-leave)
    (dolist (k '([down-mouse-1] [mouse-1] [drag-mouse-1] [double-mouse-1])) (define-key map k #'ignore))
    map)
  "Keymap on the strip: the pointer there is off the chart.")


(defvar eas-mode--view)
(declare-function eas-mode--readout "eas-mode" ())

(defun eas-mode-strip-string (view)
  "VIEW's values strip as a propertized line."
  (let* ((text (concat " " (eas-strip-format (eas-strip (eas-view-scene view) (eas-view-plan view)
                                                        (eas-view-state view)))))
         (size (eas-view-size view))
         ;; In a terminal the strip is no wider than the chart it reads
         ;; (fc-qx1.52): a longer line ends in a truncation glyph.
         (text (if (and (eq (eas-view-target view) 'text) (plist-get size :cols))
                   (truncate-string-to-width text (plist-get size :cols) nil nil "…")
                 text)))
    (propertize text 'face 'eas-strip 'eas-strip t 'keymap eas-mode-strip-map
                'help-echo nil 'pointer 'arrow)))

(defun eas-mode-strip-update (view)
  "Rewrite the strip line of the current buffer (showing VIEW) when it changed."
  (when-let* ((beg (text-property-any (point-min) (point-max) 'eas-strip t)))
    (let ((end (or (text-property-not-all beg (point-max) 'eas-strip t) (point-max)))
          (new (eas-mode-strip-string view)))
      (unless (equal-including-properties (buffer-substring beg end) new)
        (let ((inhibit-read-only t))
          (save-excursion
            (goto-char beg)
            (delete-region beg end)
            (insert new)))))))

(defun eas-mode-strip-leave (_event)
  "The pointer moved onto the strip: it left the chart."
  (interactive "e")
  (let ((view eas-mode--view))
    (when (and view (plist-get (eas-view-state view) :pointer))
      (eas-dispatch view '(:type "pointerleave"))
      (eas-mode--readout))))

(provide 'eas-mode-strip)
;;; eas-mode-strip.el ends here

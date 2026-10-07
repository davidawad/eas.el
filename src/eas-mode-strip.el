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
(require 'eas-readout)

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
  "VIEW's readout (by default the values strip) as propertized lines.
It takes exactly the lines the chart reserves (`eas-readout-max-lines')
and is no wider than the chart (fc-qx1.52, eas-anj): the fit drops,
abbreviates, then elides rather than wrap.  In a GUI frame the lines
are drawn as one SVG image of fixed size."
  (let* ((width (eas-readout-width view))
         (atoms (eas-readout-atoms view))
         (lines (eas-readout-render view (max 1 (1- width)) nil atoms))
         (text (mapconcat (lambda (spans) (concat " " (eas-component-propertize spans))) lines "\n"))
         (image (and (eq (eas-view-target view) 'svg) (display-graphic-p) (image-type-available-p 'svg)
                     (create-image (eas-readout-svg view (plist-get (plist-get (eas-view-scene view) :size) :w)
                                                    (default-line-height) (face-foreground 'eas-strip nil t) atoms)
                                   'svg t))))
    (add-face-text-property 0 (length text) 'eas-strip t text)
    (add-text-properties 0 (length text)
                         (append (list 'eas-strip t 'keymap eas-mode-strip-map 'help-echo nil 'pointer 'arrow)
                                 (and image (list 'display image)))
                         text)
    text))

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

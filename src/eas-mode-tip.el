;;; eas-mode-tip.el --- hover tooltips in GUI and terminal buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L6 glue (fc-qx1.1).  Where a tooltip comes from:
;;
;;   GUI, discrete marks  the image's :map areas carry help-echo, so
;;                        Emacs shows them itself (a tooltip frame, or
;;                        the echo area when `tooltip-mode' is off)
;;   GUI, series          no map area per point (fc-qx1.14 spike): the
;;                        hovered datum comes from the hit index and is
;;                        shown with `tooltip-show', or in the echo area
;;                        when `tooltip-mode' is off
;;   terminal             moving point over the chart is hovering: the
;;                        hovered datum's tooltip goes to the echo area;
;;                        cells also carry help-echo for xterm-mouse
;;
;; It runs from `eas-view-dispatch-functions' whenever the hovered
;; datum changes in a view that has a buffer.

;;; Code:

(require 'eas-view)
(require 'eas-tip)
(require 'eas-readout)

(defconst eas-mode-tip-map-marks '("bar" "rect" "point" "circle" "square" "text")
  "Marks `eas-svg-hot-spots' gives :map areas, whose help-echo Emacs shows.")

(defvar eas-mode-tip-display-function #'eas-mode-tip-display
  "Function called with TEXT (nil to clear) and VIEW to show a hover tooltip.")

(defvar-local eas-mode-tip--shown nil "Non-nil while this buffer's tooltip is showing.")

(defun eas-mode-tip-display (text view)
  "Show TEXT for VIEW: a tooltip in a GUI frame with `tooltip-mode'.
Elsewhere, or with `tooltip-mode' off, TEXT goes to the echo area."
  (if (and (eq (eas-view-target view) 'svg) (display-graphic-p)
           (bound-and-true-p tooltip-mode) (fboundp 'tooltip-show))
      (if text (tooltip-show text) (tooltip-hide))
    (let ((message-log-max nil))
      ;; One line: a taller echo area would resize the chart (eas-anj).
      (cond (text (message "%s" (eas-readout-echo-line text)))
            (eas-mode-tip--shown (message nil)))))
  (setq eas-mode-tip--shown (and text t)))

(defun eas-mode-tip--key (hover)
  "What identifies HOVER's datum."
  (and hover (list (plist-get hover :view) (plist-get hover :mark) (plist-get hover :datum))))

(defun eas-mode-tip-text (view)
  "Tooltip text of VIEW's hovered datum, or nil.
The chart's x-eas.tooltip component tree says it when it has one."
  (or (eas-readout-tooltip-string view)
      (eas-tip-text (eas-tip-tooltip (eas-view-scene view) (eas-view-plan view)
                                     (plist-get (eas-view-state view) :hover)))))

(defun eas-mode-tip--own-p (view)
  "Non-nil unless Emacs displays VIEW's hovered datum itself via a :map area."
  (let ((hover (plist-get (eas-view-state view) :hover)))
    (not (and hover (eq (eas-view-target view) 'svg)
              (member (plist-get (eas-tip--mark (eas-view-scene view) (plist-get hover :view)
                                                  (plist-get hover :mark))
                                 :mark)
                      eas-mode-tip-map-marks)))))

(defun eas-mode-tip--on-dispatch (view event old-state _old-scene)
  "Show VIEW's tooltip when EVENT moved the hover off OLD-STATE's datum."
  (let ((buffer (eas-view-buffer view)))
    (when (and (member (plist-get event :type) '("pointermove" "pointerleave"))
               (not eas-view-replaying) (buffer-live-p buffer)
               (not (equal (eas-mode-tip--key (plist-get old-state :hover))
                           (eas-mode-tip--key (plist-get (eas-view-state view) :hover)))))
      (with-current-buffer buffer
        (let ((text (and (eas-mode-tip--own-p view) (eas-mode-tip-text view))))
          (when (or text eas-mode-tip--shown)
            (funcall eas-mode-tip-display-function text view)
            (setq eas-mode-tip--shown (and text t))))))))

(add-hook 'eas-view-dispatch-functions #'eas-mode-tip--on-dispatch)

(provide 'eas-mode-tip)
;;; eas-mode-tip.el ends here

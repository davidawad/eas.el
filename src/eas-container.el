;;; eas-container.el --- width and height "container" -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  Vega-Lite's width or height "container" sizes the view
;; so the whole chart, padding included, fills its container (autosize
;; fit-x / fit-y with contains "padding").  A chart compiled with
;; :size (an Emacs window) is already fitted to it.  Without one, the
;; container is `eas-container-width' by `eas-container-height': 480
;; is the width bin/chart renders a container-wide chart at (the
;; gallery's bar_size_responsive reference); no reference covers a
;; container height, so it takes the same 300 the gallery's middle
;; size uses.

;;; Code:

(require 'eas-core)

(declare-function eas-place-chrome "eas-compile-place")

(defvar eas-container-width 480
  "Total pixel width of a width \"container\" chart compiled without a size.")

(defvar eas-container-height 300
  "Total pixel height of a height \"container\" chart compiled without a size.")

(defun eas-container-p (group key)
  "Non-nil when GROUP's spec size KEY (:spec-w or :spec-h) is \"container\"."
  (equal (plist-get group key) "container"))

(defun eas-container-fit (tree metrics title-h shared)
  "Fit TREE's single view to the container on its \"container\" dimensions.
METRICS are the layout metrics, TITLE-H the chart title's height and
SHARED the width of legends shared beside the block.  A concatenation
keeps its natural size: Vega-Lite sizes only a single view to its
container."
  (when-let* ((g (plist-get tree :group))
              ((or (eas-container-p g :spec-w) (eas-container-p g :spec-h))))
    (let ((pad (plist-get metrics :pad)))
      ;; Labels depend on the plot size, so settle the chrome twice.
      (dotimes (_ 2)
        (let ((c (plist-get g :chrome)) (min (aref (plist-get metrics :cell) 0)))
          (when (eas-container-p g :spec-w)
            (plist-put g :w (max min (- eas-container-width (* 2 pad) shared
                                        (plist-get c :left) (plist-get c :right)))))
          (when (eas-container-p g :spec-h)
            (plist-put g :h (max min (- eas-container-height (* 2 pad) title-h
                                        (plist-get c :top) (plist-get c :bottom))))))
        (eas-place-chrome g metrics)))))

(provide 'eas-container)
;;; eas-container.el ends here

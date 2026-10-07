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
;;
;; Vega-Lite's autosize "fit" (and "fit-x", "fit-y") fits a single view
;; the same way, its container being the spec's own width and height:
;; the axes, title and legends come out of them instead of adding to
;; them.  With contains "content" (the default) the padding lies
;; outside that size; with "padding" it lies inside.

;;; Code:

(require 'eas-core)

(declare-function eas-place-chrome "eas-compile-place")

(defvar eas-container-width 480
  "Total pixel width of a width \"container\" chart compiled without a size.")

(defvar eas-container-height 300
  "Total pixel height of a height \"container\" chart compiled without a size.")

(defvar eas-container-autosize nil
  "The autosize of the chart being laid out: a cons of TYPE and WITHIN, or nil.
TYPE is \"fit\", \"fit-x\" or \"fit-y\"; WITHIN, its contains, is \"content\" or
\"padding\".  `eas-compile-plan' binds it from the spec.")

(defun eas-container-autosize-of (spec)
  "Return the fitting autosize of SPEC, a cons of TYPE and WITHIN, or nil.
TYPE and WITHIN are as in `eas-container-autosize'."
  (let* ((a (plist-get spec :autosize))
         (type (if (stringp a) a (and (eas-object-p a) (plist-get a :type))))
         (contains (or (and (eas-object-p a) (plist-get a :contains)) "content")))
    (and (member type '("fit" "fit-x" "fit-y")) (cons type contains))))

(defun eas-container-p (group key)
  "Non-nil when GROUP's spec size KEY (:spec-w or :spec-h) is \"container\"."
  (equal (plist-get group key) "container"))

(defun eas-container--fit-target (group key metrics)
  "Total size GROUP's dimension KEY (:spec-w or :spec-h) is fitted to, or nil.
It is the container's size less the padding for \"container\", and the
spec's own size under an autosize fit (less the padding when it
contains it).  METRICS are the layout metrics."
  (let ((pad (plist-get metrics :pad)) (spec (plist-get group key))
        (fit (car eas-container-autosize)))
    (cond ((eas-container-p group key)
           (- (if (eq key :spec-w) eas-container-width eas-container-height) (* 2 pad)))
          ((and (numberp spec)
                (member fit (list "fit" (if (eq key :spec-w) "fit-x" "fit-y"))))
           (- spec (if (equal (cdr eas-container-autosize) "padding") (* 2 pad) 0))))))

(defun eas-container-fit (tree metrics title-h shared)
  "Fit TREE's single view to the container on its \"container\" dimensions.
The same holds for the dimensions an autosize fit names
\(`eas-container-autosize').  METRICS are the layout metrics, TITLE-H
the chart title's height and SHARED the width of legends shared beside
the block.  A concatenation keeps its natural size: Vega-Lite sizes
only a single view to its container."
  (when-let* ((g (plist-get tree :group))
              (tw (or (eas-container--fit-target g :spec-w metrics) t))
              (th (or (eas-container--fit-target g :spec-h metrics) t))
              ((or (numberp tw) (numberp th))))
    ;; Labels depend on the plot size, so settle the chrome twice.
    (dotimes (_ 2)
      (let ((c (plist-get g :chrome)) (min (aref (plist-get metrics :cell) 0)))
        (when (numberp tw)
          (plist-put g :w (max min (- tw shared (plist-get c :left) (plist-get c :right)))))
        (when (numberp th)
          (plist-put g :h (max min (- th title-h (plist-get c :top) (plist-get c :bottom))))))
      (eas-place-chrome g metrics))))

(defun eas-container-refit (tree total metrics)
  "Correct TREE's autosize fit by how far canvas TOTAL (W . H) misses it.
The fit sizes the plot from the axes; the canvas also holds what the
marks and the outer axis labels overhang, which Vega's fit counts too.
Resize the single view's plot by the miss and return non-nil when it
changed, so the caller computes its items again.  METRICS are the
layout metrics.  Only an autosize fit refits: a \"container\" size
keeps its plot."
  (when-let* ((g (plist-get tree :group))
              ((car eas-container-autosize)))
    (let ((pad (plist-get metrics :pad)) (min (aref (plist-get metrics :cell) 0)) changed)
      (dolist (dim '((:spec-w :w car) (:spec-h :h cdr)))
        (when-let* (((not (eas-container-p g (nth 0 dim))))
                    (target (eas-container--fit-target g (nth 0 dim) metrics))
                    (miss (- (funcall (nth 2 dim) total) target (* 2 pad)))
                    ((/= miss 0)))
          (plist-put g (nth 1 dim) (max min (- (plist-get g (nth 1 dim)) miss)))
          (setq changed t)))
      (when changed (eas-place-chrome g metrics))
      changed)))

(provide 'eas-container)
;;; eas-container.el ends here

;;; eas-title.el --- the chart title: lines, size, anchor and frame -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4.  A spec's title is a string, an array of lines, or an
;; object {text, anchor, frame, fontSize, fontWeight, color, offset}
;; over config.title.  Vega stacks title lines fontSize + 2 apart.
;; `eas-title-height' is the room the chart title takes above the plots
;; (compile reads its anchor and frame itself); a concat cell's own
;; title is drawn by `eas-title-view-mark' above the cell's axes.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-title-extra)

(defun eas-title--object (spec)
  "SPEC's title as an object, or nil when it has none."
  (let ((title (plist-get spec :title)))
    (cond ((or (stringp title) (vectorp title)) (list :text title))
          ((eas-object-p title) title))))

(defun eas-title-lines (spec)
  "SPEC's title lines as a list of non-empty strings, or nil."
  (let ((text (plist-get (eas-title--object spec) :text)))
    (seq-remove #'string-empty-p
                (cond ((stringp text) (list text))
                      ((vectorp text) (mapcar (lambda (s) (format "%s" s)) text))))))

(defun eas-title-text (spec)
  "SPEC's title as one string (lines joined by spaces), or nil."
  (when-let* ((lines (eas-title-lines spec))) (string-join lines " ")))

(defun eas-title--get (spec metrics key metric)
  "Title property KEY of SPEC, else the METRIC from METRICS (config.title)."
  (let ((v (plist-get (eas-title--object spec) key))) (if (and v (not (eq v :null))) v (plist-get metrics metric))))

(defun eas-title--line-height (size)
  "Height Vega bounds a one-line title of font SIZE with.
Vega puts titles on a bottom baseline, round(0.8 size) - round(-0.21
size) above it."
  (- (eas-layout--round (* 0.8 size)) (eas-layout--round (* -0.21 size))))

(defun eas-title-height (spec metrics)
  "Height the title of SPEC takes above the plots, its offset included.
METRICS gives the target."
  (let ((lines (eas-title-lines spec)))
    (cond ((null lines) 0)
          ((eas-title-extra-subtitle-lines spec)
           (+ (eas-title-height (eas-plist-put spec :title (eas-plist-put (eas-title--object spec) :subtitle :null)) metrics)
              (eas-title-extra-height spec metrics)))
          ((eas-layout-text-p metrics) (* (length lines) (plist-get metrics :chart-title-size)))
          (t (let ((size (eas-title--get spec metrics :fontSize :chart-title-size)))
               (+ (eas-title--line-height size) (* (1- (length lines)) (+ size 2))
                  (eas-title--get spec metrics :offset :chart-title-pad)))))))

(defun eas-title-place (spec groups metrics total)
  "The scene title of SPEC over placed GROUPS in a TOTAL (W . H) canvas, or nil.
METRICS gives the target."
  (when-let* ((lines (eas-title-lines spec)))
    (let* ((text (eas-layout-text-p metrics))
           (frame (or (plist-get (eas-title--object spec) :frame) "bounds"))
           (bounds (not (equal frame "group")))
           ;; Frame bounds start at the content's exact left edge, as Vega's do.
           (edge (lambda (g side) (cond ((not bounds) 0)
                                        ((and (eq side :left) (not text) (plist-get g :content-x1))
                                         (- (plist-get g :content-x1)))
                                        (t (or (plist-get (plist-get g :chrome) side) 0)))))
           (x1 (apply #'min (mapcar (lambda (g) (- (plist-get g :x0) (funcall edge g :left))) groups)))
           (x2 (apply #'max (mapcar (lambda (g) (+ (plist-get g :x0) (plist-get g :w) (funcall edge g :right))) groups)))
           (anchor (if text "middle" (eas-title--get spec metrics :anchor :chart-title-anchor)))
           (size (if text (plist-get metrics :chart-title-size) (eas-title--get spec metrics :fontSize :chart-title-size))))
      (append
       (list :text (string-join lines " ")
             :x (pcase anchor ("start" x1) ("end" x2) (_ (if text (/ (car total) 2.0) (/ (+ x1 x2) 2.0))))
             ;; The first line's top, so that its bottom baseline lands where Vega's does.
             :y (+ (plist-get metrics :pad)
                   (if text 0 (- (eas-layout--round (* 0.8 size)) (eas-layout--round (* 0.79 size)))))
             :align (pcase anchor ("start" "left") ("end" "right") (_ "center")) :baseline "top"
             :fontSize size
             :fontWeight (eas-title--get spec metrics :fontWeight :chart-title-weight))
       (when (cdr lines) (list :lines (vconcat lines) :lineHeight (if text size (+ size 2))))
       (let ((color (plist-get (eas-title--object spec) :color))) (when (stringp color) (list :color color)))))))


(defun eas-title-anchor (spec metrics)
  "SPEC's title anchor: its own, else config.title's (METRICS)."
  (let ((a (plist-get (eas-title--object spec) :anchor)))
    (if (member a '("start" "middle" "end")) a (plist-get metrics :chart-title-anchor))))

(defun eas-title-style (spec metrics)
  "Scene title properties beyond size and weight.
These are :dx :dy :font :fontStyle, from SPEC's title object over
config.title (METRICS)."
  (unless (eas-layout-text-p metrics)
    (let ((obj (eas-title--object spec)) (config (eas-theme-get (plist-get metrics :config) :title)) out)
      (dolist (k '(:dx :dy :font :fontStyle))
        (let ((v (if (plist-member obj k) (plist-get obj k) (plist-get config k))))
          (when (or (numberp v) (stringp v)) (setq out (append out (list k v))))))
      out)))

;;; Titles of concat cells

(defun eas-title-view-mark (group metrics)
  "A text mark drawing GROUP's own title (a concat cell's), or nil.
The title sits above the cell's axes; `eas-place-chrome' reserved its room.
METRICS gives the target."
  (when-let* ((node (plist-get group :title-node))
              (title (eas-title-place node (list group) metrics (cons 0 0))))
    (let* ((text (eas-layout-text-p metrics))
           (y (+ (- (plist-get group :y0) (or (plist-get group :axis-top) 0) (eas-title-height node metrics))
                 (- (plist-get title :y) (plist-get metrics :pad)))))
      (list :id (format "%s/title" (plist-get group :id)) :mark "text" :interactive-off t :rows []
            :items (vector (append (list :x (if text (+ (plist-get group :x0) (/ (plist-get group :w) 2.0)) (plist-get title :x))
                                         :y y :text (plist-get title :text) :fontSize (plist-get title :fontSize)
                                         :align (if text "center" (plist-get title :align)) :baseline "top"
                                         :fontWeight (plist-get title :fontWeight)
                                         :fill (or (plist-get title :color)
                                                   (eas-theme-get (plist-get metrics :config) :title :color) "black")
                                         :opacity 1)
                                   (when (plist-get title :lines) (list :lines (plist-get title :lines)))))))))

(declare-function eas-theme-get "eas-theme")

(provide 'eas-title)
;;; eas-title.el ends here

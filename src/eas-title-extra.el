;;; eas-title-extra.el --- chart subtitles and title offsets -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Part of L4, beside eas-title.el (fc-qx1.42).  A title object's
;;
;;   subtitle subtitleColor subtitleFontSize subtitleFontWeight
;;   subtitlePadding dx dy
;;
;; (over config.title) are laid out as Vega's titleLayout does: the
;; subtitle shares the title's anchor and sits the title's bounds height
;; plus subtitlePadding (default 3) below its top, so the title group,
;; and the room `eas-title-height' reserves, grows by subtitlePadding and
;; the subtitle's own bounds.  dx and dy shift both lines in pixels.
;; The text target gives the subtitle the row under the title.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-theme)

(declare-function eas-title--object "eas-title")
(declare-function eas-title--line-height "eas-title")
(declare-function eas-title-style "eas-title")

(defconst eas-title-extra-subtitle-padding 3 "Vega's default title.subtitlePadding.")

(defun eas-title-extra--get (spec metrics key default)
  "Title property KEY of SPEC, else config.title's, else DEFAULT."
  (let ((v (plist-get (eas-title--object spec) key)))
    (cond ((and v (not (eq v :null))) v)
          ((eas-theme-get (plist-get metrics :config) :title key))
          (t default))))

(defun eas-title-extra-subtitle-lines (spec)
  "SPEC's subtitle lines as non-empty strings, or nil."
  (let ((sub (plist-get (eas-title--object spec) :subtitle)))
    (seq-remove #'string-empty-p
                (cond ((stringp sub) (list sub))
                      ((vectorp sub) (mapcar (lambda (s) (format "%s" s)) sub))))))

(defun eas-title-extra--size (spec metrics)
  "The subtitle's font size for SPEC under METRICS."
  (eas-title-extra--get spec metrics :subtitleFontSize 12))

(defun eas-title-extra-height (spec metrics)
  "Height SPEC's subtitle adds to its title's room (0 without one)."
  (let ((lines (eas-title-extra-subtitle-lines spec)))
    (cond ((null lines) 0)
          ((eas-layout-text-p metrics) (* (length lines) (plist-get metrics :chart-title-size)))
          (t (let ((size (eas-title-extra--size spec metrics)))
               (+ (eas-title-extra--get spec metrics :subtitlePadding eas-title-extra-subtitle-padding)
                  (eas-title--line-height size) (* (1- (length lines)) (+ size 2))))))))

(defun eas-title-extra-apply (title spec metrics)
  "Scene TITLE of SPEC with its dx/dy applied and its :subtitle placed."
  (if (null title) title
    (let* ((text (eas-layout-text-p metrics))
           (dx (if text 0 (let ((v (eas-title-extra--get spec metrics :dx 0))) (if (numberp v) v 0))))
           (dy (if text 0 (let ((v (eas-title-extra--get spec metrics :dy 0))) (if (numberp v) v 0))))
           (x (+ (plist-get title :x) dx)) (y (+ (plist-get title :y) dy))
           (lines (eas-title-extra-subtitle-lines spec))
           (n (length (or (plist-get title :lines) [t])))
           (size (plist-get title :fontSize)))
      (append (eas-plist-put (eas-plist-put title :x x) :y y)
              ;; Its font and fontStyle (eas-title-style), drawn by eas-svg.
              (let ((style (eas-title-style spec metrics)))
                (cl-loop for k in '(:font :fontStyle) when (plist-get style k) append (list k (plist-get style k))))
              (when lines
                (let ((sub (eas-title-extra--size spec metrics)))
                  (list :subtitle
                        (append
                         (list :text (string-join lines " ") :x x :align (plist-get title :align) :baseline "top"
                               :y (if text (+ y (* n size))
                                    (+ y (eas-title--line-height size) (* (1- n) (+ size 2))
                                       (eas-title-extra--get spec metrics :subtitlePadding eas-title-extra-subtitle-padding)))
                               :fontSize (if text size sub)
                               :fontWeight (eas-title-extra--get spec metrics :subtitleFontWeight "normal")
                               :color (eas-title-extra--get spec metrics :subtitleColor "black"))
                         ;; subtitleFont and subtitleFontStyle, drawn by eas-svg as the title's.
                         (unless text
                           (cl-loop for (k . key) in '((:subtitleFont . :font) (:subtitleFontStyle . :fontStyle))
                                    for v = (eas-title-extra--get spec metrics k nil)
                                    when (stringp v) append (list key v)))
                         (when (cdr lines) (list :lines (vconcat lines) :lineHeight (if text size (+ sub 2))))))))))))

(provide 'eas-title-extra)
;;; eas-title-extra.el ends here

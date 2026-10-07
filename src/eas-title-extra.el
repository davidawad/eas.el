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
;; the subtitle's own bounds.  dx shifts both lines in pixels.  dy
;; shifts them too, and Vega's autosize grows the canvas by the title
;; group's bounds, so dy moves the plots by -dy instead: the lines keep
;; the canvas top until dy would push them below the plots' top.
;; The text target gives the subtitle the row under the title.

;;; Code:

(require 'eas-core)
(require 'eas-layout)
(require 'eas-theme)

(declare-function eas-title--object "eas-title")
(declare-function eas-title--line-height "eas-title")
(declare-function eas-title--get "eas-title")
(declare-function eas-title--height "eas-title")
(declare-function eas-title-lines "eas-title")
(declare-function eas-title-style "eas-title")

(defconst eas-title-extra-subtitle-padding 3 "Vega's default title.subtitlePadding.")

(defun eas-title-extra--get (spec metrics key default)
  "Title property KEY of SPEC, else METRICS' config.title's, else DEFAULT."
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
  "Return the height SPEC's subtitle needs in the title's room, 0 if none.
METRICS measures the subtitle."
  (let ((lines (eas-title-extra-subtitle-lines spec)))
    (cond ((null lines) 0)
          ((eas-layout-text-p metrics) (* (length lines) (plist-get metrics :chart-title-size)))
          (t (let* ((size (eas-title-extra--size spec metrics))
                    (title (eas-title--get spec metrics :fontSize :chart-title-size))
                    (n (length (eas-title-lines spec))))
               ;; Vega's room is the union of the title's bounds and the
               ;; subtitle's, which sits the title's bounds height plus
               ;; subtitlePadding below it; less the title's own room.
               (- (+ (eas-title-extra--bounds-height title n)
                     (eas-title-extra-subtitle-padding spec metrics)
                     (eas-title-extra--bounds-top size)
                     (eas-title-extra--bounds-height size (length lines)))
                  (eas-title-extra--bounds-top title)
                  (eas-title--line-height title) (* (1- n) (+ title 2))))))))

(defun eas-title-extra-subtitle-padding (spec metrics)
  "SPEC's subtitlePadding under METRICS (config.title), default 3."
  (eas-title-extra--get spec metrics :subtitlePadding eas-title-extra-subtitle-padding))

(defun eas-title-extra--dy (spec metrics)
  "SPEC's title dy in pixels under METRICS (config.title), 0 on text."
  (let ((v (unless (eas-layout-text-p metrics) (eas-title-extra--get spec metrics :dy 0))))
    (if (numberp v) v 0)))

(defun eas-title-extra-dy-room (spec metrics height)
  "Return the room that SPEC's title dy gives a title of HEIGHT, under METRICS.
Vega's title group sits HEIGHT above the plots and its lines dy below
the group, so the room is HEIGHT - dy, never less than 0."
  (if (zerop height) 0 (max (- height) (- (eas-title-extra--dy spec metrics)))))

(defun eas-title-extra--bounds-top (size)
  "Top of Vega's bounds of top-baseline text of font SIZE, from its y."
  (- (eas-layout--round (* 0.79 size)) (eas-layout--round (* 0.8 size))))

(defun eas-title-extra--bounds-height (size n)
  "Height of Vega's bounds of N lines of font SIZE, lines size + 2 apart."
  (+ size (* (1- n) (+ size 2))))

(defun eas-title-extra-apply (title spec metrics)
  "Scene TITLE of SPEC with its dx/dy applied and its :subtitle placed.
METRICS gives the config; under text METRICS dx/dy do not apply."

  (if (null title) title
    (let* ((text (eas-layout-text-p metrics))
           (dx (if text 0 (let ((v (eas-title-extra--get spec metrics :dx 0))) (if (numberp v) v 0))))
           ;; dy already moved the plots (`eas-title-extra-dy-room'); the
           ;; lines move only by what of it the room could not absorb.
           (dy (let ((h (eas-title--height spec metrics)))
                 (+ (eas-title-extra--dy spec metrics) (eas-title-extra-dy-room spec metrics h))))
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
                                    (+ y (eas-title-extra--bounds-height size n)
                                       (eas-title-extra-subtitle-padding spec metrics)))
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

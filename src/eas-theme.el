;;; eas-theme.el --- the default look: bin/chart's Vega config -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; A theme is a Vega config object, the JSON bin/chart accepts.  The
;; native default is exactly bin/chart's default theme (`chart theme
;; --json'), so a native rendering and its static door agree; ERT checks
;; it against the vendored copy in test/conformance and, when bin/chart
;; is installed, against bin/chart itself.
;;
;; Configs layer: `eas-theme-merge' merges objects key by key and
;; replaces everything else.  Compile reads Vega-Lite's built-in mark
;; defaults overlaid with the theme and then the spec's own "config",
;; for sizes, fonts and mark defaults, and puts that config in the
;; scene; renderers read the scene's config overlaid with the caller's
;; theme for colors.

;;; Code:

(require 'eas-core)

(defconst eas-theme-default
  '(:arc (:stroke "#fcfcfb" :strokeWidth 2)
    :area (:opacity 0.85)
    :axis (:domainColor "#c3c2b7" :domainWidth 1 :gridColor "#e1e0d9" :gridWidth 1
           :labelColor "#52514e" :labelFontSize 11 :labelPadding 4 :tickColor "#c3c2b7" :tickSize 4
           :titleColor "#52514e" :titleFontSize 12 :titleFontWeight "normal")
    :axisX (:grid :false)
    :background "#fcfcfb"
    :bar (:cornerRadiusEnd 4)
    :circle (:size 64)
    :font "sans-serif"
    :legend (:labelColor "#52514e" :labelFontSize 11 :symbolSize 80 :symbolType "circle"
             :titleColor "#52514e" :titleFontSize 11 :titleFontWeight "normal")
    :line (:strokeCap "round" :strokeJoin "round" :strokeWidth 2)
    :mark (:color "#2a78d6")
    :point (:filled t :size 64)
    :range (:category ["#2a78d6" "#eb6834" "#1baf7a" "#eda100" "#e87ba4" "#008300" "#4a3aa7" "#e34948"]
            :diverging ["#256abf" "#6da7ec" "#cde2fb" "#f0efec" "#f6c3bd" "#ee7d78" "#c0392b"]
            :heatmap ["#cde2fb" "#9ec5f4" "#6da7ec" "#3987e5" "#256abf" "#184f95" "#0d366b"]
            :ordinal ["#cde2fb" "#9ec5f4" "#6da7ec" "#3987e5" "#256abf" "#184f95" "#0d366b"]
            :ramp ["#cde2fb" "#9ec5f4" "#6da7ec" "#3987e5" "#256abf" "#184f95" "#0d366b"])
    :rect (:stroke "#fcfcfb" :strokeWidth 1)
    :title (:anchor "start" :color "#0b0b0b" :fontSize 16 :fontWeight 600 :offset 12
            :subtitleColor "#52514e" :subtitleFontSize 12)
    :view (:continuousHeight 300 :continuousWidth 480 :stroke :null))
  "The default theme of bin/chart, the native default.
It is the \"config\" of `chart theme --json'.")

(defconst eas-theme-vega-lite
  '(:mark (:color "#4c78a8") :text (:color "black" :fontSize 11) :rule (:color "black")
    :point (:size 30) :circle (:size 30) :square (:size 30) :tick (:thickness 1))
  "Vega-Lite's own mark config defaults, beneath any theme.")

(defun eas-theme-merge (base &rest overlays)
  "BASE config with each of OVERLAYS merged over it in turn.
Objects merge key by key; any other value (arrays included) replaces."
  (dolist (overlay overlays)
    (when (and (eas-object-p overlay) overlay)
      (cl-loop for (k v) on overlay by #'cddr
               do (setq base (eas-plist-put
                              base k (let ((old (plist-get base k)))
                                       (if (and old (eas-object-p old) v (eas-object-p v))
                                           (eas-theme-merge old v)
                                         v)))))))
  base)

(defun eas-theme-get (config &rest keys)
  "The value at KEYS in CONFIG, or nil.
A JSON null counts as nil."
  (let ((v config))
    (dolist (k keys) (setq v (and (eas-object-p v) (plist-get v k))))
    (unless (eq v :null) v)))

(defun eas-theme-axis (config channel key)
  "Axis property KEY for CHANNEL (:x or :y) under CONFIG.
axisX/axisY override axis, as in Vega-Lite."
  (let ((specific (plist-member (eas-theme-get config (if (eq channel :x) :axisX :axisY)) key)))
    (if specific (let ((v (cadr specific))) (unless (eq v :null) v))
      (eas-theme-get config :axis key))))

(provide 'eas-theme)
;;; eas-theme.el ends here

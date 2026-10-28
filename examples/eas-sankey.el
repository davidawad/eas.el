;;; eas-sankey.el --- example: a sankey chart, alone and on a vui page -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The sankey template over examples/sankey.data.json (an illustrative
;; energy flow), shown twice:
;;
;; 1. in its own eas buffer (`eas-show'): hover a node or a band for
;;    every field it has, including the ones the template never names
;;    (kind, renewable, efficiency);
;; 2. when vui.el is on the load path, on a vui page: the chart widget,
;;    and under it a card with the fields of the last datum clicked
;;    (RET or mouse-1 on the chart).
;;
;;   emacs -Q -L src -l examples/eas-sankey.el
;;   emacs -Q -L src -L /path/to/vui.el -l examples/eas-sankey.el
;;
;; Your own data is two tables, nodes and links:
;;
;;   (eas-view-open "sankey" :bindings
;;     '(:nodes [(:name "a" :p 0.7) (:name "b")]
;;       :links [(:source "a" :target "b" :value 3)]))

;;; Code:

(require 'eas)

(defvar eas-sankey-example-bindings (eas-template-example "sankey")
  "The example's nodes and links.")

(eas-show (eas-view-open "sankey" :bindings eas-sankey-example-bindings :id "energy"))

(when (require 'vui nil t)
  (require 'eas-vui)
  (vui-defcomponent eas-sankey-example-page ()
    "A page: the flow chart, then the clicked datum's fields."
    :state ((selected nil))
    :render
    (vui-vstack
     (vui-text "Energy flow (RET or click a node or band)" :face 'bold)
     (vui-component 'eas-vui-chart
                    :source "sankey" :bindings eas-sankey-example-bindings
                    :size (and (not (display-graphic-p)) '(:cols 90 :rows 24))
                    :on-click (vui-async-callback (row _click) (vui-set-state :selected row)))
     (if selected
         (vui-component 'eas-vui-card :title "Selected" :row selected)
       (vui-text "Nothing selected yet."))))
  (vui-mount (vui-component 'eas-sankey-example-page) "*eas sankey page*"))

;;; eas-sankey.el ends here

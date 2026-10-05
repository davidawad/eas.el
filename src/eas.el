;;; eas.el --- Interactive, agent-drivable charts from declarative JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: data, multimedia, tools

;;; Commentary:

;; eas draws charts configured ahead of time as JSON (a Vega-Lite
;; subset) from data Emacs pipes in, as SVG in GUI frames and text in
;; terminals.  See docs/design/engine.md for the layer contracts.
;;
;; This file loads every layer.  Each layer is also usable alone.

;;; Code:

(require 'eas-core)
(require 'eas-time)
(require 'eas-data)
(require 'eas-adapters)
(require 'eas-data-org)
(require 'eas-expr)
(require 'eas-transform)
(require 'eas-lttb)
(require 'eas-spec)
(require 'eas-spec-props)
(require 'eas-vl-lower)
(require 'eas-transform-domain)
(require 'eas-template)
(require 'eas-resolve)
(require 'eas-describe)
(require 'eas-scale)
(require 'eas-compile)
(require 'eas-hit)
(require 'eas-scene)
(require 'eas-glyph)
(require 'eas-svg)
(require 'eas-text)
(require 'eas-params)
(require 'eas-event)
(require 'eas-zoom)
(require 'eas-reduce)
(require 'eas-view)
(require 'eas-tip)
(require 'eas-action)
(require 'eas-action-org)
(require 'eas-action-drill)
(require 'eas-mode)
(require 'eas-mode-tip)
(require 'eas-crosshair)
(require 'eas-brush)
(require 'eas-stream)
(require 'eas-link-bus)
(require 'eas-link-demo)
(require 'eas-tty)
(require 'eas-parity)
(require 'eas-chart)
(require 'eas-conformance)

(provide 'eas)
;;; eas.el ends here

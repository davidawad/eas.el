;;; eas.el --- Interactive, agent-drivable charts from declarative JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad

;; Author: David Awad <me@davidaw.ad>
;; Maintainer: David Awad <me@davidaw.ad>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: data, multimedia, tools, hypermedia
;; URL: https://github.com/davidawad/eas.el
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; eas draws charts configured ahead of time as JSON (a Vega-Lite
;; subset) from data Emacs pipes in, as SVG in GUI frames and text in
;; terminals.  Charts are interactive in place (hover, crosshair,
;; zoom, brush, drill, linked views, live data) and an agent can read
;; and drive them as data: `eas-agent' in Lisp, bin/eas in a shell,
;; `eas-agent-json' through emacsclient.  See README.md and
;; docs/design/engine.md for the layer contracts.
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
(require 'eas-countpattern)
(require 'eas-wordcloud)
(require 'eas-hierarchy)
(require 'eas-hierarchy-tree)
(require 'eas-hierarchy-tile)
(require 'eas-hierarchy-pack)
(require 'eas-linkpath)
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
(require 'eas-action-callback)
(require 'eas-hierarchy-zoom)
(require 'eas-mode)
(require 'eas-mode-tip)
(require 'eas-crosshair)
(require 'eas-brush)
(require 'eas-stream)
(require 'eas-play)
(require 'eas-link-bus)
(require 'eas-tty)
(require 'eas-parity)
(require 'eas-chart)
(require 'eas-conformance)

(provide 'eas)
;;; eas.el ends here

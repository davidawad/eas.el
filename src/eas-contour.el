;;; eas-contour.el --- contours, density grids, heatmaps and geopaths -*- lexical-binding: t; -*-

;; Copyright (C) 2026 David Awad
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The Vega transforms behind contour plots, density heatmaps, raster
;; maps and filled contour maps (eas-7r1.7), as x-eas domain
;; transforms that resolve materializes into flat rows:
;;
;;   kde2d       eas-kde2d.el          density grids per group
;;   isocontour  eas-contour-iso.el    marching squares, thresholds
;;   heatmap     eas-contour-heatmap.el  grid -> PNG data: URL image
;;   geopath     eas-contour-geo.el    geometry -> SVG path data
;;   geopoints   eas-contour-geo.el    geometry -> vertex rows
;;
;; plus the "grid" and "topojson" adapters and the character grid's
;; drawing of path symbols and PNG images (eas-contour-text.el).

;;; Code:

(require 'eas-contour-grid)
(require 'eas-kde2d)
(require 'eas-contour-iso)
(require 'eas-contour-heatmap)
(require 'eas-contour-geo)
(require 'eas-contour-text)

(provide 'eas-contour)
;;; eas-contour.el ends here

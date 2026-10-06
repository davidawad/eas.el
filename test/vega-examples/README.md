# Vega example gallery

The 94 examples of https://vega.github.io/vega/examples/, vendored as the
coverage target for eas templates.

- `specs/<name>.vg.json`: the official Vega specs (vega/vega, BSD-3-Clause).
- `data/`: the datasets they load (vega-datasets, mixed licenses as upstream).
- `ref/<name>.png`: reference renders from `vg2svg` + `rsvg-convert`.
  `contour-plot`, `density-heatmaps` and `projections` need node-canvas or
  d3-geo-projection, so their refs are the Vega site's 360px thumbnails.
- `manifest.json`: one row per example; `template` names the eas template
  that reproduces it (`templates/vega/<name>.json`) and `status` is
  `todo`, `pass`, `partial` or `unsupported` (with a reason).

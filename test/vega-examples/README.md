# Vega example gallery

The 94 examples of https://vega.github.io/vega/examples/, vendored as the
coverage target for eas templates.

- `specs/<name>.vg.json`: the official Vega specs (vega/vega, BSD-3-Clause).
- `data/`: the datasets they load (vega-datasets, mixed licenses as upstream).
- `ref/<name>.png`: reference renders from `vg2png` (node-canvas, so text
  is measured with real font metrics). `projections` needs
  d3-geo-projection, so its ref is the Vega site's 360px thumbnail.
- `manifest.json`: one row per example; `template` names the eas template
  that reproduces it (`templates/vega/<template>.json`, mostly the
  example's name; `heatmap` is `hourly-heatmap` and `histogram` is
  `rug-histogram`, as eas has its own of both) and `status` is
  `todo`, `pass`, `partial` or `unsupported` (with a reason).

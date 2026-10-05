# Vega-Lite gallery coverage

Native (pure Emacs Lisp) rendering of the official Vega-Lite 6.4.1 gallery
against bin/chart's reference PNGs (`test/vl-examples/GROUP/ref/`).  Counts
are per group's `status.json`, restricted to the 188 non-map examples (the 12
topojson map examples have no reference and are out of scope).

- pass: both backends render, the native SVG is within the example's
  differing-pixel threshold of the reference (default 0.03, canvas within 8 px),
  and it lays out at three sizes without overlap
- partial: it renders, but misses one of those (the reason is in `status.json`)
- unsupported: native compile refuses it (the reason names the feature)

| bead | group | pass | partial | unsupported | examples |
|------|-------|-----:|--------:|------------:|---------:|
| fc-qx1.25 | bar | 15 | 9 | 0 | 24 |
| fc-qx1.26 | distributions | 15 | 4 | 0 | 19 |
| fc-qx1.27 | scatter-table | 18 | 3 | 1 | 22 |
| fc-qx1.28 | line | 19 | 1 | 0 | 20 |
| fc-qx1.29 | area-circular | 13 | 0 | 0 | 13 |
| fc-qx1.30, .43 | calculations | 21 | 0 | 0 | 21 |
| fc-qx1.31 | layered | 17 | 2 | 0 | 19 |
| fc-qx1.32 | multiview | 2 | 9 | 8 | 19 |
| fc-qx1.33 | interactive | 23 | 7 | 1 | 31 |
| **total** | | **143** | **35** | **10** | **188** |

143 of 188 (76%) pass, 178 of 188 (95%) render natively.

## Polish: calculations (fc-qx1.43)

Every calculations example passes.  Two of bin/chart's references are
known to lack marks (Vega-Lite 6.4.1 drops bars under the theme's
`config.bar.cornerRadiusEnd`); their status.json entries carry
`refOmits`, so the oracle compares everything but those marks, at the
default 0.03, and the harness requires the native rendering to draw
them.  Legends placed in a plot corner (`orient: "top-left"`, ...) sit
in the plot on purpose and are not counted as overlaps.

- Customizability: `eas-spec-props.el` lists, per scope (mark type, axis,
  legend, scale, title, config blocks), the Vega-Lite properties eas
  draws; ERT re-measures the list (a property counts only if it changes
  the SVG).  `check` reports every other property as
  `UNSUPPORTED_FEATURE` with its path and `:property t`; the chart still
  draws natively without it.  `calculations/custom/` holds one
  customization spec per chart type (bar, line, area, point, rule, tick,
  text), checked clean, with text goldens; their bin/chart references
  are built by the harness where bin/chart is installed.
- Cost: `calculations/bench.json` (scripts/eas-gallery-bench.sh) has
  each example's bench-verb stages, byte-compiled, against the earlier
  build.  Layers now share their parent's transforms (run once per
  compile), zone-local time math uses a per-week cached offset instead
  of a zone lookup per row, and HCL stop colors are cached: the group's
  summed stage means went from 8.7 s to 3.6 s.

Second passes: area-circular (fc-qx1.42) keeps 13 of 13. Its
`bench.json` records the bench verb's numbers per example, before and
after. Its `custom/` specs, one per chart type (area, arc, radial),
set non-default properties throughout and must pass `check` with no
warnings. `check` names every property the renderer draws without as
an ignored `UNSUPPORTED_FEATURE` warning (`eas-spec-props.el`;
engine-spikes section 12).

## What is left

- multiview (fc-qx1.32) is the weakest group.  The engine lowers a single
  row or column facet to concatenated cells; it does not draw the facet's
  field title, and it lacks a row-and-column facet grid, wrapped facets
  (`columns`), `repeat` with `columns`, `bounds: "flush"` and the quantize
  scale.  The box's own grid architecture (eas-grid.el, a second eas-facet.el
  with Vega's gridLayout) was not merged: it replaces the layout the other
  groups share.
- Small gallery canvases (320x200): nominal axes keep every label, as Vega-Lite
  does, so several wide bar charts and stacked concats overlap there.
- bar: legend `orient: "top"`, axis label placement (`titleX`, `labelOffset`),
  `width: "container"`, the heat lane's size-driven bar height.
- Projections and topojson (all maps).

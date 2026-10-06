# eas: an Emacs-native, interactive, agent-drivable chart engine

Status: design, epic `fc-qx1`. Engine name `eas` (prefix `eas-`) was
confirmed free by `fc-qx1.14` (`engine-spikes.md`). Never `chart-`: the built-in
`chart.el` owns that prefix.

## 1. What this is, in one paragraph

A chart is a declarative JSON document, configured ahead of time.
Emacs pipes data into it and draws it in the buffer, as an image in a
GUI frame or as text in a terminal. You can work with it in place:
hover, crosshair, zoom, pan, brush, drill, linked views, live updates.
The document is a subset of Vega-Lite, so every chart also renders
statically through a Vega-Lite command-line renderer into SVG, PNG, PDF or
reports with no translation step. Domain packages such as
financial-chart.el and health-charts.el stop drawing anything. They
ship templates (chart JSON with typed data slots) and transforms
(indicators, reference-range logic), and nothing more.

Static rendering is a commodity. What nothing else in Emacs does
(survey in `fc-qx1`) is the interactive layer, a single spec that
drives both GUI and terminal frames, and an agent being able to read
and drive a live chart as data.

## 2. The tower

Each layer has one contract. Each contract is plain data (JSON, with a
plist mirror in Lisp), and each layer only reads the layer below it.
An agent can stop at any layer, dump what is there, and know exactly
where a wrong pixel came from.

```
 L7 surfaces    Emacs commands/modes · org-babel · CLI · emacsclient bridge
                    all return the chart/v1 envelope
 L6 runtime     view/v1 state + event/v1 log; pure reducers; params
 L5 renderers   scene -> SVG image (+ :map hot spots)  |  scene -> text (+ props)
 L4 compile     resolved spec + rows -> scene/v1 (marks, scales, axes, datum refs)
 L3 resolve     template + bindings + defaults + domain transforms -> pure Vega-Lite
 L2 spec        chart/v1 = Vega-Lite subset + "x-eas" extension namespace
 L1 transforms  Vega-Lite transforms subset + registered domain transforms
 L0 data        data/v1 tidy rows from adapters (plist, JSON, CSV, org table, bar/v1)
                         |
     export: the L3 output is a complete Vega-Lite spec -> bin/chart build/check/diff
```

### L0 data/v1

Tidy rows: a vector of flat objects plus a column schema
`{name, type: quantitative|temporal|ordinal|nominal, unit?}`. Adapters
are registered once and auto-discovered:

| adapter | input |
|---|---|
| `plist` | list of plists (Lisp callers) |
| `json`, `csv`, `tsv` | file, string or stdin |
| `org-table` | table at point, named table, or babel result |
| `bar/v1` | market-data.el OHLCV bars (financial-chart's shape) |
| `series`, `labeled`, `matrix`, `order-book`, ... | financial-chart's existing shapes, lowered to rows |

Every adapter validates and fails as data: `{code, index, field, message}`.
That is the same rule the existing `financial-chart-shapes` validators
follow. Streaming is part of the contract (`eas-push VIEW ROWS`), so
live data is not a separate code path. A keyed push (`eas-push VIEW ROWS
:key FIELD`, eas-keyed.el) replaces rows by FIELD in place, appends new
keys and deletes rows marked `"_eas_delete": true`, so a live table
changes only the rows a delta names.

### L1 transforms

These are the Vega-Lite transforms the engine implements natively:
`filter`, `calculate` (a small, safe expression subset), `aggregate`,
`window`, `fold`, `timeUnit`, `bin`, `joinaggregate`. On top of those
are domain transforms, registered with a schema:

```json
{"x-eas:transform": "indicator", "name": "rsi", "field": "close", "period": 14, "as": "rsi"}
{"x-eas:transform": "reference-band", "marker": "ldl-c", "as": ["lo", "hi"]}
{"x-eas:transform": "lttb", "x": "t", "y": "close", "pixels": "auto"}
```

financial-chart's 28 indicators become transforms instead of chart
kinds. This is the main accretion win: one new indicator is available
to every template.

Two Vega transforms with no Vega-Lite counterpart are domain
transforms too (`eas-7r1.8`, the word-cloud template):

```json
{"x-eas:transform": "countpattern", "field": "abstract", "case": "upper",
 "pattern": "[\\w']{3,}", "stopwords": "(the|and)"}
{"x-eas:transform": "wordcloud", "size": [800, 400], "text": "text",
 "fontSize": {"field": "count"}, "fontSizeRange": [12, 56],
 "rotate": [-45, 0, 45], "padding": 2, "seed": 1}
```

`countpattern` (eas-countpattern.el) counts regexp matches into
`{text, count}` rows in order of first appearance; patterns are
JavaScript regexps, stopwords match whole words in any case.
`wordcloud` (eas-wordcloud.el) is d3-cloud's layout: sizes on a sqrt
scale onto `fontSizeRange`, largest word first, each walked along an
archimedean (or rectangular) spiral from a random start until it fits.
It adds `x`, `y` (the centred, alphabetic-baseline anchor), `font`,
`fontSize`, `fontStyle`, `fontWeight` and `angle`; a word with no room
gets null `x`/`y` and is not drawn. Per-word parameters take a
constant, `{"field"}` or `{"expr"}`, and `rotate` also an array to pick
from. Two deliberate differences from Vega: the start points,
directions and picked angles come from a generator seeded by `seed`,
so a cloud is the same on every run; and words collide as their
measured boxes (`eas-font-text-width` advance, ascent and descent from
the glyphs, turned and grown by `padding`), rasterized onto d3-cloud's
occupancy bitmap, not as glyph sprites (batch Emacs has no canvas). A
cloud is a little looser than Vega's, never overlapping. The geometry
is word-agnostic: `eas-wordcloud-text-box`, `eas-wordcloud-box` and
`eas-wordcloud-overlap-p` (exact separating-axis test of two turned
boxes) suit any rotated-text collision. Two candidates exist: the
gallery's label check (`eas-vl-gallery--rotated-box` only compares
parallel labels) and hover on text (`eas-intersect` reaches half a font
size around the anchor, so the ends of a long or turned word miss).


Hierarchies (`eas-7r1.4`) are Vega's tree transforms as domain
transforms, ported from d3-hierarchy and held to Vega's own numbers.
eas rows carry no hidden tree, so a hierarchy is always a key column
and a parent-key column (`key`/`parentKey`, default `id`/`parent`) and
each layout rebuilds the tree from them:

| transform | does (eas-hierarchy*.el) |
|---|---|
| `stratify` | checks the rows form one tree (`SHAPE_INVALID` with the row's index), adds `depth`, `children` |
| `nest` | groups rows by `keys` into a tree: a root row, one row per group, the rows under them |
| `tree` | `method` `tidy` (Reingold-Tilford) or `cluster`; `size` or `nodeSize`, `separation`; `x`, `y` |
| `treemap` | `squarify` (`ratio`), `resquarify`, `binary`, `dice`, `slice`, `slicedice`, paddings, `round`; `x0`..`y1` |
| `partition` | icicle bands (`x0`..`y1`); a sunburst reads x as angle and y as radius |
| `pack` | circle packing, `x`, `y`, `r` (d3's front chain and enclosing circle, same random stream) |
| `treelinks` | one `{source, target}` row per edge, the rows nested |
| `treepath` | for each link of `links` (a data slot), the tree rows on its path (Vega's `treePath`) |
| `subtree` | the rows under `root`, which becomes the root; `level` keeps each row's depth in the whole tree |
| `formula` | a Vega expression into a field among the domain transforms (`params` names constants) |

Layouts take `field` (summed up the tree; leaves count 1 without it)
and `sort` (Vega's compare; `value`, `depth`, `height` are the node's),
and write their fields back in row order (`as` renames them). Laid-out
rows are drawn on identity scales (`domain` = `range`, no axes); a
sunburst draws partition rows as arcs spanning `theta`..`theta2` and
`radius2`..`radius` (conformance spec `encoding-theta2-radius2`).

`linkpath` (eas-linkpath.el) is the link geometry of node-link
diagrams: `shape` `line`, `arc`, `curve`, `diagonal` or `orthogonal`,
`orient` `vertical`, `horizontal` or `radial` (x an angle in radians, y
a radius). The transform turns each link row into vertex rows (`x`,
`y`, `step`, `link`), which a line mark draws with `detail: link` and
`order: step`; curves are sampled, so renderers stay polyline-only.
Other code asks for the geometry directly: `(eas-linkpath-path SHAPE
ORIENT SX SY TX TY)` is Vega's SVG path data and `(eas-linkpath-points
...)` the same path as `((X Y) ...)`. Hierarchical edge bundling is
`treepath` rows on a line mark with `interpolate: "bundle"` and
`tension`. The `zoom-subtree` click action re-opens a template's view
with slot `focus` set to the clicked node, which a `subtree` transform
lays out alone: d3's layouts place every subtree independently, so
that is the zoomed layout.


Where Vega has a transform Vega-Lite lacks, the engine ports it as a
domain transform (`eas-7r1.2`). `dotbin` (eas-dotbin.el) is Vega's
DotBin, Wilkinson's dot plot binning: within each `groupby` group,
sorted by `field`, a bin opens at a value and takes every value less
than `step` above it, and each row gets its bin's center in `as`
(default `bin`; `step` defaults to a thirtieth of the field's span,
`smooth` evens out adjacent stacks as Vega does). A window
`row_number` grouped by the bin then stacks the dots:

```json
{"x-eas:transform": "dotbin", "field": "minutes", "step": 1.25, "smooth": false, "as": "bin"}
```

Vega's distribution functions are in the expression subset too
(eas-expr-dist.el, ported from vega-statistics): `densityNormal`,
`cumulativeNormal`, `densityLogNormal`, `cumulativeLogNormal`,
`quantileLogNormal`, `densityUniform` and `cumulativeUniform`, beside
`quantileNormal` and `quantileUniform`. `density` takes Vega-Lite's
`resolve`: `"independent"` samples each group over its own extent (a
violin ends at its group's extremes), the default `"shared"` over one.


Three domain transforms lay out networks and point sets (`eas-7r1.5`),
natively and deterministically:

```json
{"x-eas:transform": "force", "iterations": 300, "output": "both",
 "forces": [{"force": "center", "x": 350, "y": 250}, {"force": "collide", "radius": 8},
            {"force": "nbody", "strength": -30}, {"force": "link", "links": [...], "distance": 30}]}
{"x-eas:transform": "graph", "links": [...], "sort": "group", "output": "both", "cross": true}
{"x-eas:transform": "voronoi", "x": "x", "y": "y", "polygon": "cell"}
```

- `force` (eas-force.el) is Vega's force transform: a port of d3-force 3
  (simulation, center, collide, nbody, link, x and y forces, and the
  d3-quadtree they search) that matches d3's layout digit for digit.
  Nodes start at their x/y fields or on d3's phyllotaxis, fx/fy pin
  them, and the jiggle for coincident nodes draws from d3's seeded
  generator (`seed`, 1). A static render runs `iterations` ticks;
  `eas-force-simulation`/`eas-force-tick` advance one live. A per-node
  parameter is a number, a field, `{"expr": E}` or `{"band": F,
  "range": [lo, hi]}` (the center of F's band, Vega's `xfocus`).
  `"output": "both"` appends one row per link with its endpoints in
  x, y, x2, y2, so rule marks draw the links (`eas_kind` tells rows
  apart). A node that diverges to infinity stays out of the quadtree
  (d3 skips only NaN), so it cannot grow the tree without bound.
- `graph` (eas-force-graph.el) joins node rows to link rows, the
  aggregate and lookup Vega does across datasets: node `order` (stable
  by `sort`), `degree` and `count`; link rows carrying every node field
  as `source_*`/`target_*`; with `cross`, a row per node pair.
- `voronoi` (eas-voronoi.el) is Vega's voronoi transform: each point's
  cell within `extent` as SVG path data (`as`) and, with `polygon`, as
  [x, y] vertices a line mark draws after a flatten; `key` makes rows
  sharing a value one site. Sites are sorted by x once and each cell
  clips against neighbours walking out in x until they are too far to
  matter, so memory stays linear in the number of sites.

Each takes `"transform": [...]`, Vega-Lite transforms run on its rows
first, because a template's domain transforms precede its native ones.
`project` (eas-geo.el) takes `"fit": false` to keep the projection's
own scale and translate, as a Vega projection does.


Vega's label layout, which Vega-Lite lacks, is two such transforms
(eas-label.el, eas-7r1.1). Both work in plot pixels, so they take the
plot size and the linear scales' domains (default: the data extent,
with zero):

```json
{"x-eas:transform": "label", "x": "X", "y": "Y", "text": "T", "width": 800, "height": 600,
 "anchor": ["top", "bottom", "right", "left"], "markSize": 25, "avoidRegression": "quad"}
{"x-eas:transform": "arc-label", "field": "value", "width": 200, "outerRadius": 100}
```

`label` draws every point (and, with `avoidRegression`, the fitted
trend line) into an occupancy bitmap, then gives each row in turn the
first anchor whose text box stays in bounds and touches no mark and no
earlier label; `label_anchor` holds it (null when none fits), and a
template draws one text layer per anchor. `arc-label` gives each wedge
of a pie in data order its leader line (`arc_x1` `arc_y1` to `arc_x2`
`arc_y2`, across to `arc_x3`, then to the label at `arc_x4` `arc_y4`)
and side (`arc_side`), and pushes labels on a side at least
`labelHeight` apart. Output columns of a domain transform must not start
with `x-eas`: resolve strips such keys.


Two layout transforms draw what Vega does with its own scales and
signals and Vega-Lite cannot express on one view (`eas-7r1.3`):

```json
{"x-eas:transform": "parallel-coordinates", "fields": ["Cylinders", "Horsepower"], "format": {"Year": "d"}}
{"x-eas:transform": "serpentine", "field": "year", "domain": [1926, 2026], "diameter": 125, "arcs": 2.2}
```

`parallel-coordinates` (src/eas-parallel.el) folds each row into one
row per field with `norm`, its 0..1 position on that field's own
linear domain (niced as Vega nices it), and appends one `axis` row per
field and one `tick` row per tick with Vega's tick values and labels,
so a template draws lines, rules and text on one point x and one
[0, 1] y. `serpentine` (src/eas-serpentine.el) lays a numeric domain
along a path of straight runs joined by half circles: input rows
become `milestone` rows, and the path (`serpentine`), its ticks and
its two ends are appended, each with pixel `x`/`y`, the segment, the
arc angle, `side` and a tangent `labelAngle`.

### L2 spec: chart/v1

It is Vega-Lite (pinned to the version `bin/chart` pins, 6.4.1).
Anything engine-specific lives under the `"x-eas"` key or as an
`x-eas:` transform, which the L3 resolve step strips out. Interaction
is not an invented API. It is Vega-Lite's own `params`/selection
grammar (section 4).

### L3 resolve and template/v1

A template is a chart/v1 spec with declared slots:

```json
{
  "x-eas": {
    "template": "ohlc",
    "version": "1.0.0",
    "doc": "Candlesticks with optional volume panel and indicator overlays.",
    "slots": {
      "bars":       {"shape": "bar/v1", "required": true},
      "indicators": {"type": "array", "items": "indicator-ref", "default": []},
      "volume":     {"type": "boolean", "default": true}
    },
    "example": "examples/ohlc.data.json"
  },
  "data": {"name": "bars"},
  "vconcat": [ ... ]
}
```

`resolve(template, bindings)` does five things: bind data to slots,
fill defaults, expand domain transforms into materialized columns,
inline the data, and drop `x-eas`. The output is a complete,
standalone Vega-Lite spec. That one function is what makes the static
export path free. Resolve is pure and deterministic, so its output is
content-hashed (the same hash scheme as `bin/chart describe`).

Templates are JSON files in `templates/`, one per kind, each with a
golden fixture. Adding a kind means writing one file. Lisp code is
needed only if the kind needs a new transform. `templates/vega/` holds
one template per Vega gallery example (`eas-7r1`, target
`test/vega-examples/`), its bindings in `examples/vega/` (the template
names them as `../examples/vega/NAME.data.json`), and its `x-eas.vega`
records `{"status": "pass|partial|unsupported", "note"}` against the
vendored reference.

`templates/vega/` reproduces the Vega gallery (`eas-7r1`), namespace
`vega` (`eas-template-add-directory` loads it). Each template's
`x-eas.vega` records `status` (pass, partial, unsupported), a `note`
and the `ratio` its example may differ from
`test/vega-examples/ref/NAME.png` by; `examples/vega/NAME.data.json`
binds the gallery's data. Those references were drawn by vg2svg
without node-canvas, so Vega padded their canvas by estimated text
widths: the comparison aligns the plot origins first.

The Vega gallery's examples (`test/vega-examples/`, epic `eas-7r1`)
are templates in `templates/vega/`, registered under the `vega`
namespace (`vega/clock`), each with its binding in `examples/vega/`
(the template's `example` is `../examples/vega/NAME.data.json`).
`x-eas.vega` records the verdict against the Vega reference PNG:
`{"status": "pass"|"partial"|"unsupported", "note", "ratio",
"threshold"}`, where ratio is `eas-png-compare`'s differing-pixel
ratio and the `:gallery` test holds each example within its
threshold.

`templates/vega/` holds one template per example of the Vega gallery
(vega.github.io/vega/examples, epic `eas-7r1`), namespaced `vega`
(`vega/histogram`), each with its binding in `examples/vega/` that
reproduces the gallery chart from `test/vega-examples/data`. A
template's `x-eas.vega` records how close it comes to
`test/vega-examples/ref/NAME.png`: `{"status": "pass"|"partial"|
"unsupported", "note", "ratio", "threshold"}`, `ratio` being the
differing-pixel ratio measured at the best alignment and `threshold`
what `make test-gallery-vega` holds it to. The references were drawn
by Vega with estimated text widths (0.8 em per character), so a
binding may set `label_extent` (an axis `minExtent`) to keep the
gallery's layout.

`templates/vega/` holds one template per Vega gallery example
(`test/vega-examples/`), namespaced `vega/NAME` and loaded with
`(eas-template-add-directory "templates/vega")`; its bindings live in
`examples/vega/` (an `example` path is relative to the parent of the
template's directory, so they read `../examples/vega/NAME.data.json`).
Each records in `x-eas.vega` how it compares with Vega's reference
PNG: `status` (pass, partial, unsupported), the differing-pixel
`ratio` at `offset`, and a `note`.

A slot that holds an array can expand into views:
`{"x-eas:each": SLOT, "spec": X}` in any array becomes one X per item,
and inside X `{"x-eas:item": KEY, "default": D}` reads the item (`"."`
is the item itself; a missing KEY with no default drops its key). The
`ohlc` template draws one overlay layer and one pane per entry of its
`indicators` and `oscillators` slots this way (`fc-qx1.36`).
Eaches nest: the inner one's array may be `{"x-eas:item": KEY}` of the
outer item, and `"../KEY"` reads the enclosing item (`eas-agt.3`).

The templates of the Vega example gallery live in `templates/vega/`
under the `vega` namespace (`vega/bar-chart`, eas-7r1), each with
`x-eas.vega` recording how it compares with the gallery's reference
image (`status`, `note`, `ratio`, `threshold`; `make
test-gallery-vega` holds them to it). In an example's bindings a data
slot bound to `{"file": F}` reads a relative F beside the example file
(`eas-template-read-bindings`), so the examples point at
`test/vega-examples/data/` instead of copying it.

Slots reach further than whole nodes (`eas-agt.3`):
`{"x-eas:slot": S, "key": "a.b"}` reads into an object slot, and
`{"x-eas:expr": "..."}` / `{"x-eas:text": "..."}` substitute `{{S}}`
references inside a string, as JSON literals or as raw text. A slot
whose value is null leaves its property out. Template names may carry
a namespace (`eas-template-add-directory DIR NAMESPACE`), and a
template file that fails to load is skipped and reported rather than
stopping the rest.

### L4 compile: scene/v1

`compile(resolved, rows, size, view-state)` produces the scene graph,
the single artifact that every renderer, hit-test and agent query
reads:

```json
{
  "size": {"w": 800, "h": 420, "cell": [7, 14]},
  "background": "#fcfcfb", "config": {..},
  "views": [{
    "id": "price", "bounds": [0, 0, 800, 300],
    "scales": {"x": {"type": "time", "domain": [..], "range": [40, 790]},
               "y": {"type": "linear", "domain": [..], "range": [290, 10]}},
    "axes": [..], "legends": [..],
    "marks": [{"mark": "rect", "id": "candles", "items": [
      {"datum": 17, "x": 120.5, "y": 80, "w": 4, "h": 31, "fill": "up", "tooltip": {..}}
    ]}]
  }],
  "index": {"price/candles": {"kind": "x-sorted"}}
}
```

A layer whose `resolve.axis` makes a channel independent while its
scale stays shared draws, besides the first layer's axis, the axis each
later layer declares (eas-independent.el): a top axis with grid and
labels plus a bottom one with only a title, or the same labels on both
sides of a trellis cell.

Every item carries `datum`, a back-reference to the row, which is how
hover, drill, export and describe find the data behind a pixel. Every
scale carries an inverse, which is how zoom, brush and crosshair work
in data space. Compile also builds the hit-test index (x-sorted bisect
for series, a uniform grid for scatter, rectangles for bars) and runs
LTTB decimation when a series has more points than pixel columns.

### L5 renderers

- SVG: scene to svg.el, `create-image ... 'svg`, plus `:map` hot spots
  generated from the same items for discrete marks (bars, legend
  entries, annotations). Continuous series hit-test by inverting
  `posn-object-x-y` through the scale, never with one map area per
  point.
- Text: scene to a character grid (braille, eighths or block glyphs
  per mark type, which financial-chart-text.el already has) where every
  cell carries text properties `eas-datum`, `eas-view` and
  `help-echo`. Moving point over the chart is the terminal's hover.
  Glyphs per mark (fc-qx1.49): lines and trails are braille; arcs
  are braille only where their edge cuts a cell, and full blocks in
  the color of the wedge holding most of a cell inside, so wedges meet
  without a seam (fc-qx1.51); bars and areas are eighth blocks,
  the value end partial and the baseline end full (stacked slices
  that share a cell compose: the lower slice's block on the upper
  slice's color); a ranged bar whose
  y2 lies below its y (a falling candle) is shaded `▒`, a rising one
  solid; ticks and rules are box lines; points are shape glyphs. One
  glyph fits a cell, so marks keep Vega's painter's order within three
  tiers (fills, strokes, symbols); a stroke drawn before an opaque fill
  sits under it (a wick under its body). Tick labels that would
  overwrite one another are dropped, text runs back onto the canvas,
  and wide characters take two cells. Text cannot rotate, so axis
  labels run along their axis whatever their labelAngle; a log axis
  offers 1, 2, 3 and 5 per decade, kept by rank as rows allow. A
  gradient legend draws its ramp in half blocks; a brush shades every
  cell of its rectangle, and an empty interval draws nothing. Colors
  pass `eas-text-ink-legible` (fc-qx1.51): kept when their WCAG
  contrast with the frame's background is at least 3, neutral ink
  becomes the default face's foreground, other hues move their
  lightness until legible, so a dark terminal draws light rules.
  Text views follow their window's size. `eas-text-check` judges a text
  rendering against its scene (every visible item lands in a cell,
  baselines reach zero, labels and legend entries show, labels never
  share a cell and sit on their side of the axis line, every color
  holds 3:1 contrast on a light and a dark background); `eas-text-gallery` holds every non-map example and
  template to it at three sizes (test/vl-examples/text-status.json).
- Static: resolved spec to `bin/chart build`. Not part of Emacs and
  never a runtime dependency: it is the conformance oracle (section 6)
  and the explicit export door (`export --vl`, babel .png/.pdf).

The renderers are dumb. Anything a renderer would need to decide
belongs in compile, which keeps both backends equivalent by
construction.

### L6 runtime: view/v1 and event/v1

A view is `{id, spec-hash, bindings, state, log}`, where state holds
the current scale domains, param values (selections), hover, and
streaming cursor. Events are data:

```json
{"type": "pointermove", "view": "price", "px": [311, 140]}
{"type": "wheel", "view": "price", "px": [311, 140], "delta": -3}
{"type": "brush", "view": "price", "x": ["2026-03-01", "2026-03-15"]}
{"type": "key", "key": "+"}
{"type": "params", "values": {"pac": {"x": 7, "y": 6}, "score": 10}}
```

A `params` event sets several params in one step (one redraw); a
`param` event sets one, as a bound input widget does.

Timers and keys that drive params (`eas-7r1.9`, eas-play.el) follow
Vega's signal `on` handlers. A template declares

```json
"x-eas": {"timer": {"interval": 1000},
          "on": [{"events": "timer", "param": "now", "update": "event.local"},
                 {"events": ["key:down", "key:j"], "param": "row",
                  "update": "min(row + 1, event.rows)"}]}
```

Each handler names a top-level param and a Vega expression for its
next value; it sees every param by name and `event` (`type`, `key`,
`n`, `time`, `local`, `rows`), and `random()` is deterministic per
handler and play event. The handlers an event matches run in order, each seeing
what the earlier ones set, and their changed values reach the view as
one `param`/`params` event, so the reducer stays pure and a replay of
the log redraws the animation without a timer. `eas-play-tick` and
`eas-play-key` fire them headlessly. `eas-show` starts the timer (at
least `eas-play-min-interval`, 50 ms), which skips ticks while no
visible window shows the buffer and stops with it, and binds the
handlers' keys (`eas-play-keys-mode`, ahead of the view mode's arrows;
names are `kbd` keys plus up, down, left, right, home, end, pageup,
pagedown and space). Emacs reports no key release, so a held key is
its auto-repeat. `supported.json` lists both as the extensions
`x-eas/timer` and `x-eas/on`.

Reducers are pure: `(state, event, scene) -> state'`. Then
`compile + render` runs only if the visible output changed. GUI and
terminal glue only translate native Emacs events (posn, keys,
xterm-mouse) into event/v1. So every interaction can be tested in
`--batch`, replayed from a log, and driven by an agent by sending the
same JSON a mouse would produce.

The log is a bounded ring. It is how an agent learns what the human
just did ("brushed Mar 1 to Mar 15 on TSM price") without a
screenshot.

Clicks become click targets (eas-action.el): a datum, a legend entry,
and (eas-action-callback.el, eas-7r1.10) an axis label or tick
(`:area "axis" :axis CH :value V`), an axis title, the chart title, a
facet header, or the empty plot background (`:area "background"` with
data-space `:x`/`:y` through the view's scale inverses). Each target
runs at most one binding, looked up by key (mark id, click or legend
param, `legend`, `axis:CH`, `axis`, `title`, `background`, `*`) in, from
the most specific: the view's `eas-action-bind`, global
`eas-action-default-bindings` entries for the view's template, the
template's `x-eas.actions`, global entries for any view. A binding is
a registered action name, a function, or `(:action|:fn ... :when P)`;
a list of bindings runs the first whose `:when` (a Vega expression on
the datum, or an elisp predicate on the target) holds. Area geometry
is computed from the scene for its target (font-measured for SVG, the
renderer's own cells for text), and the SVG image gets the same areas
as `:map` hot spots, so GUI clicks, terminal `RET` and agent click
events land on the same target. Areas never answer `*`, and an area
click is recorded only when something is bound to it. Callback errors
are recorded on the target as `:error`; dispatch never fails.

### L7 surfaces

The same verbs are available everywhere, and all of them return the
`bin/chart` envelope:
`{"contract":"chart/v1","ok","data","reason"?,"evidence"?,"next":[...]}`.

| verb | answer |
|---|---|
| `describe [template\|transform\|adapter]` | the registries: every template with slots, example and file path; every transform with schema; supported Vega-Lite features |
| `example TEMPLATE` | bindings that render as-is |
| `check SPEC\|TEMPLATE+DATA` | stable reason codes (section 5), including `UNSUPPORTED_FEATURE` with the JSON path |
| `explain ... --stage resolve\|compile\|scene` | the exact intermediate artifact at that layer |
| `render ... --backend svg\|text` | text is the agent's own eyes and is deterministic |
| `export ... --vl` | resolved pure Vega-Lite for `bin/chart` and documents |
| `views` | live views: id, buffer, template, size, last event |
| `inspect VIEW` | current domains, selection, hovered datum, visible-range summary (min, max, first, last, change, n) |
| `dispatch VIEW EVENT` | applies an event/v1 and returns the new inspect |
| `log VIEW` | the recent event log |
| `selection VIEW --as rows\|org\|json` | the selected data |
| `bench [SPEC]` | measured compile, render and hover latency as JSON; with no SPEC the 1k/10k/100k ladder, `--budget` checks it for regressions (`fc-qx1.9`) |
| `doctor` | eager `(:name :status :detail :remediation)` rows |

Entry points:
- Lisp: `(eas-agent VERB &rest ARGS)`.
- Shell: `bin/eas VERB ...`, which runs Emacs in batch for the
  stateless verbs.
- A running Emacs: `emacsclient --eval '(eas-agent-json "inspect" "tsm-price")'`,
  for the live verbs `views`, `inspect`, `dispatch`, `log` and
  `selection`.

### Maps: geoshape, TopoJSON and projections (`eas-7r1.6`)

Vega-Lite maps draw natively. The spherical half is a port of d3-geo's
stream pipeline, so a path is the one Vega draws (checked against d3
to 0.01 px for every projection in `test/eas/golden/vega-geo-d3.json`):

```
GeoJSON -> radians -> 3-axis rotation -> preclip (antimeridian cut, or the
small circle of clipAngle) -> adaptive resampling -> postclip (clipExtent;
mercator's own square) -> pixel rings, lines and point circles
```

| file | what |
|---|---|
| `eas-geo-stream.el` | streams, rotation, resampling, the path sink, planar area and centroid, d3's graticule |
| `eas-geo-clip.el` | the polygon clip and rejoin, spherical point-in-polygon, antimeridian, circle and rectangle clips |
| `eas-geo-raw.el`, `eas-geo-polyhedral.el` | raw projections: d3-geo's, and d3-geo-projection's the Vega gallery registers (airy, armadillo, baker, berghaus, bottomley, collignon, eckert1, guyou, hammer, littrow, mollweide, wagner6, wiechel, winkel3, aitoff, sinusoidal, the three interrupted ones, polyhedralButterfly, peirceQuincuncial) |
| `eas-geo-proj.el` | d3's projectionMutator (scale, translate, center, rotate, clipAngle, clipExtent, precision, reflectX/Y), albersUsa's three insets, fitExtent, a Newton inverse |
| `eas-topojson.el` | format `{"type": "topojson", "feature"|"mesh": NAME}` for data.url and inline values; the `geojson` adapter for template slots |
| `eas-geoshape.el` | the lowering, the compile hook, geoshape items, the `geo-point` and `geo-measure` transforms |
| `eas-geoshape-render.el` | SVG paths, text fills and outlines, hit tests |
| `eas-geoshape-interact.el` | wheel and drag on a map change the params its projection reads |

`eas-geoshape-lower` (a rewrite function) takes every view whose
projection the point-only lowerings (`eas-geo.el`, `eas-projection.el`)
do not already draw: a geoshape mark, `{"graticule": ...}` or
`{"sphere": true}` data (inlined; a graticule's geoshape is unfilled),
a type beyond theirs, or projection properties. The projection moves
onto each unit under `x-eas.geo` with the path of the view that
declared it; longitude/latitude units get x and y on fixed scales. At
compile, `eas-geoshape-ranges` resolves each projection once per
layered view: `{"expr": ...}` values read the params; with neither
scale nor translate the projection is fitted to every layer's shapes
and points (Vega-Lite's `fit`), else translate defaults to the view's
centre. A view laid out at another size than its spec declares (a
text window, a container) scales a fixed scale and translate by
min(w/W, h/H), so the same map fills it. Geoshape items keep their path
relative to an anchor (the centroid) with a bounding box, so moving an
item moves its shape; the scene mark carries the projection (`:geo`,
with expressions and resolved), which the reducer reads.

Text draws a filled shape as full blocks over the cells whose centre
it holds (nonzero rule) and an unfilled one (a graticule) in braille;
hover picks the smallest shape under the pointer. A wheel tick over a
map whose `scale` is `{"expr": "S"}` multiplies param S by the zoom step;
a drag adds the inverted longitude delta to the param `rotate`'s first
element names and the latitude delta to `center`'s second, each clamped
by its bound range input (the Vega gallery's zoomable world map).

Two domain transforms join the vocabulary: `geo-measure` (projected
centroid and area per feature, Vega's `geoCentroid` and `geoArea` with
a projection; materialized at resolve, so export stays pure Vega-Lite)
and `geo-point` (internal: the lowering's placeholder positions).
`templates/vega/` holds the Vega gallery's maps as templates (namespace
`vega`, loaded with `eas-template-add-directory` or `eas-template-load`);
each records `x-eas.vega.status` and a note against its
`test/vega-examples/ref` image. Not drawn: the `identity` projection,
`clip: {"sphere": ...}` (Vega's, which Vega-Lite cannot express), Vega's
`pointRadius` (a circle mark of area pi r^2 stands in), and gradient
strokes.

## 3. Driving it as an agent (the intended loop)

1. `describe` and pick a template, or `check` a hand-written Vega-Lite
   spec.
2. `example ohlc` gives the binding shape. Write the bindings and
   `check` them. A failure names `code`, `path` and `index`, plus
   `next`.
3. `render --backend text` to see the chart in your own context at
   near-zero cost. `explain --stage scene` only when something looks
   wrong.
4. To show the human, open the view in their Emacs. Then `inspect`,
   `log` and `selection` tell you what they are looking at and what
   they picked, as data.
5. To use the chart in a deliverable, `export --vl` goes to
   `bin/chart build`, or into a `::: {.chart}` block in render.

You never need to read engine source to know its state, never need a
screenshot to verify a chart, and never need to restate a chart in a
second language.

## 4. Interaction is Vega-Lite's params grammar

| feature (bead) | Vega-Lite construct | engine mechanism |
|---|---|---|
| tooltip (`.1`) | `encoding.tooltip` | item `tooltip` -> help-echo / `:map` / echo area |
| click target (`.1`, `.5`, `7r1.10`) | `encoding.href` + `x-eas.actions` | action registry keyed by mark, param, axis, title or background; global callbacks with `:when`; `RET` in text |
| crosshair (`.2`) | `point` selection, `on: pointermove`, `nearest: true`, plus a rule layer filtered by it | hit-test index -> datum -> state.hover |
| zoom/pan (`.3`) | `interval` selection with `bind: "scales"` | reducer edits scale domains; wheel, drag, keys |
| brush (`.4`) | `interval` selection with `encodings: ["x"]` | state.params[name] = range; selection verb |
| linked views (`.6`) | the same param across `vconcat`/`hconcat` (top-level `params` with `views`), shared scale binds, `scale.domain: {"param": ...}` | one state per spec; cross-buffer views join a named param bus that delivers `link` events (spikes section 11) |
| legend toggle (`.5`) | `point` selection with `bind: "legend"` | legend `:map` areas |
| live data (`.7`) | `x-eas.stream` | `eas-push` (keyed: latest row per key per frame), frame cap, pause while pointer/brush active |
| values strip (`.34`) | none: always on, no mode | state.pointer column (else latest datum) -> a line under the plot; inspect `strip` |
| timers and keys (`eas-7r1.9`) | variable params + `x-eas.timer`, `x-eas.on` | eas-play: handlers -> `param`/`params` events; paused while not visible |

Hover is touch-only (`.34`): state.hover, tooltips and `pointermove`
selections without `nearest` need the pointer on a drawn mark (within
the stroke, symbol or box, plus half a character cell); `nearest`
selections, and so the crosshair, still follow the nearest datum. What
a chart reads where the pointer merely is goes in the values strip.

Two pointer interactions of the Vega network examples rebind a
template's data instead (eas-force-drag.el, run from
`eas-view-dispatch-functions`): a template declaring
`"interaction": {"force-drag": {"slot": "nodes"}}` pins a dragged node
where it is dropped (dblclick frees it) and reruns its force layout
from the current positions; one declaring `"matrix-reorder"` moves the
node whose row or column label is dragged, as Vega's reorderable
matrix does. Both are deterministic, so `eas-replay` redraws them.

Semantics follow the Vega-Lite docs (Selection, Bind, Parameter,
Tooltip). The static path renders the initial state, which is what
Vega itself does with no interaction.

## 5. Failure as data: reason codes

`INVALID_INPUT`, `PARSE_ERROR`, `NOT_FOUND`, `SLOT_MISSING`,
`SLOT_TYPE`, `SHAPE_INVALID` (with `index`), `FIELD_MISSING`,
`UNSUPPORTED_FEATURE` (with path), `TRANSFORM_UNKNOWN`,
`VIEW_NOT_FOUND`, `EVENT_INVALID`, `ENGINE_FAILED`, `BUDGET_EXCEEDED`
(a benchmark stage over its regression limit). Codes shared with
`bin/chart` mean the same thing in both. Every failure carries at least
one `next` command. In Lisp they map to `define-error` children of
`eas-error` with data `(MESSAGE :code CODE ...)`, the convention
AGENTS.md already sets.

`UNSUPPORTED_FEATURE` comes in two strengths. A feature outside the
native subset (a mark, channel, transform or scale type) decides the
backend: the view is static. A documented property the native
renderers do not draw (an axis `zindex`, a legend `orient: "top"`, a
title `subtitle`) comes back with `property: true` and its path: `check`
lists it under `warnings` but keeps `native: true`, and the chart
renders natively without it (`eas-spec-props.el`, fc-qx1.43).

## 6. Conformance: bin/chart is the oracle

- A gallery of `test/conformance/*.vl.json` specs, one or more per
  supported feature. For each one: native compile, then SVG, then PNG
  via rsvg, compared with bin/chart's PNG and a pixel threshold.
  Geometry has to match Vega's, not just look similar.
- bin/chart's output is committed: `test/conformance/ref/NAME.png` with
  `ref/manifest.json` (spec hash without usermeta, PNG hash, and the
  time zone the references were built in), so the oracle needs only
  rsvg-convert. With bin/chart on PATH the references are rebuilt and a
  stale manifest hash fails; `eas-conformance-update-refs` rewrites
  them.
- Canvas sizes differ by Vega's few pixels of overhang padding, and
  `bin/chart diff` scores any size mismatch as total, so images are
  compared in Elisp: aligned on their union canvas, the size delta
  reported (and bounded) separately from the differing-pixel ratio.
- The native default theme is bin/chart's (`chart theme --json`,
  vendored in `test/conformance/bin-chart-default-theme.json` and
  checked against bin/chart when it is installed); a spec's `config`
  and a caller's theme override it.
- Text is measured in the font it is drawn in: built-in Arial, Times
  and monospace tables (`eas-font.el`), or the advance widths of a
  registered user font file (`eas-font-file.el`; `eas-font-register`
  or a chart's `x-eas.fonts`, registered at resolve). The scene's
  `:fonts` lists the registered faces a chart names, and the SVG
  renderer writes their `@font-face` rules from it. The text backend
  has one cell per glyph and ignores fonts.
- `supported.json` is generated from the passing gallery. It is the
  machine-readable answer to "can the native engine draw this?", and
  `check` and `describe` read it. A feature is supported only if a
  conformance spec proves it.
- Styling properties are checked too (fc-qx1.38): every key of an
  axis, legend, title, view or config object is either one the engine
  draws (the lists in `eas-spec-props.el`) or an `UNSUPPORTED_FEATURE`
  finding with its path, so no chart is drawn silently differently
  from Vega-Lite.  Each gallery group's `custom/` specs exercise its
  chart types' non-default properties.
- Specs that use unsupported features still open (fc-qx1.37): a
  static view with `:interactive false` in `inspect` and an
  `UNSUPPORTED_FEATURE` warning naming each path. By default the view
  shows those findings as text and `render --backend svg` fails with
  `UNSUPPORTED_FEATURE`. eas never runs bin/chart to display a chart
  unless `eas-static-fallback` is t; then the view (and svg render)
  uses bin/chart's image. Properties inside the subset that are drawn
  without (eas-spec-props.el, fc-qx1.43) are `check` warnings with
  `property: true` and do not make a view static. The native subset
  grows one gallery entry at a time.
- One level finer (fc-qx1.44): a guide, title, scale or config
  *property* Vega-Lite documents but the renderers ignore (say
  `axis.titleAngle`, `legend.columns`, `config.axisBand`) is an
  `UNSUPPORTED_FEATURE` warning in `check` with its path and a
  `:property` flag (`eas-spec-props.el`). It does not make the chart
  static: the chart draws natively with that property at its default,
  and `check` keeps `native: true`.
- bin/chart's role is decided (fc-qx1.37): test oracle (dev/CI, refs
  committed so CI needs only rsvg-convert) and static export. It is
  never a runtime dependency.
- Text-backend goldens are exact strings. Scene goldens are JSON. Both
  are reviewed as diffs (`EAS_UPDATE_GOLDEN=1 make test`; the
  conformance gallery's goldens and supported.json with
  `EAS_UPDATE_GOLDEN=1 make test-gallery-conformance`).
- `make test` stays fast (unit, golden, runtime). ERT tests tagged
  `:gallery` (the official Vega-Lite gallery and the conformance
  oracle) run in `make test-gallery`, one Emacs per group, so `-j`
  parallelizes it (fc-qx1.47).
- Measured results: engine-spikes.md section 8.

## 7. Accretion: how the system grows

| to add | write | auto-picked-up by |
|---|---|---|
| a chart kind | `templates/NAME.json` + `examples/NAME.data.json` | describe, check, golden + conformance suites |
| a domain transform | one `eas-register-transform` with schema + ERT test | describe, every template |
| a data source | one adapter entry with validator | describe, org-babel, CLI |
| an action | one `eas-register-action` | drill on any mark |
| Vega-Lite coverage | conformance spec + compile support | `supported.json`, check |

Templates for the Vega gallery (https://vega.github.io/vega/examples/)
live in `templates/vega/` under the namespace `vega` (`vega/heatmap`,
since `heatmap` is already a template), with their bindings in
`examples/vega/`; they are not in the default template directories
(load them with `eas-template-add-directory`). Each records its
verdict against `test/vega-examples/ref/NAME.png` in `x-eas.vega`
(`status` pass, partial or unsupported, the measured `ratio`, a
`note`, and `size` when it fits a container). The references were
built by vg2svg without node-canvas, so Vega laid their text out with
its estimate of 0.8 em a character: the comparison
(`make test-gallery-vega`, test/eas/eas-vega-*-test.el) measures text
that way too, in America/Chicago, and passes at a differing-pixel
ratio of 0.03 within 8 px of the reference's size. What those
templates added to the engine besides the two transforms:

- A quantitative color scale honours `nice`, `zero`, an explicit
  `domain`, `domainMin`/`domainMax`, `reverse` and `clamp`
  (src/eas-scale-sequential.el); `round` snaps band, point and
  continuous scales to whole pixels as Vega does (src/eas-scale-round.el).
- Gradient legend labels take the ticks' precision under a `%` format
  (`−6%`, not `−6.000000%`), and a horizontal gradient with
  `titleOrient: "left"` puts its title beside the bar, cut to 180 px.
- Axis tick formats `+%` sign their labels; the expression function
  `indexof` exists.
- A rotated text mark's turned box counts toward the canvas, and a
  rule's `x2`/`y2` `{"value": N}` is pixels, as `x`/`y` already were.

A domain package is only a template directory and a transform file.
financial-chart.el and health-charts.el become exactly that, and new
domains (research indicators, KPIs, sales pipeline) start the same way.

## 8. Where it sits in the wider system

- `bin/chart` (any Vega-Lite CLI renderer, e.g. one built on vl-convert): the static export door and
  conformance oracle, never a runtime dependency (section 6). Same IR,
  same envelope, same reason codes.
- financial-chart.el: its public API (`financial-chart-plot`, kinds,
  presets, `bin/financial-chart`) stays unchanged. Every kind has a
  template (`fc-qx1.36`), and `financial-chart-eas-parity` checks as
  data that the template plots the kind's own numbers. With
  `financial-chart-eas-route` set, `financial-chart-plot` draws the
  kinds at parity (or the listed kinds) with their templates; it is nil
  by default, so the old renderers and their goldens stay until the
  switch is made.
- health-charts.el already proves the "Lisp never draws, fill a
  template" model with Vega-Lite and gnuplot templates. It converges on
  eas templates (`fc-qx1.20`). The deferred medical presets (`fc-8yx`)
  are health-charts.el's kinds and are not re-invented here.
- org ("org is the workbench"):
  `#+begin_src eas :template ohlc :data tbl` shows an interactive
  chart inline. The same block exports through ob-vega or `bin/chart`.
  There is one block type, not two.

## 9. Emacs facts the design depends on (verify in `fc-qx1.14`)

- `:map` hot spots on images take help-echo and pointer per area
  (Elisp manual, Image Descriptors).
- `posn-object-x-y` gives pixel coordinates inside an image (Elisp
  manual, Accessing Mouse). Motion events need `track-mouse` (Motion
  Events).
- Emacs cannot composite images. Any change re-rasterizes the whole
  SVG, and the image cache churns (Image Cache). So crosshair and
  streaming designs depend on measured re-raster latency.
- librsvg in Emacs ignores SVG `<title>`, and `:map` hover costs under
  5 ms even at 10k areas (measured on Linux/Xvfb in `fc-qx1.23`;
  engine-spikes.md section 8).
- The latency budget is hover feedback under 50 ms. The first measurement
  (spikes section 8.8) met it at 1k rows on Linux/Xvfb only once GC was
  controlled, and missed it at 10k rows. `fc-qx1.9` made the engine's
  part of a hover independent of N for point selections (0.24 ms at
  10k, 0.26 ms at 100k) and defers GC until idle while a chart is in
  use. What remains is rasterization in a GUI frame and the text redraw
  in a terminal. Redraws stay idle-coalesced, and `make bench` guards
  the numbers in CI (spikes section 9).

## 10. Layout and extraction

The engine lives in `src/` of its own repository (eas.el, extracted
from financial-chart.el by `fc-qx1.11` with its history), with the
`eas-` prefix, its tests beside the code (`src/*-test.el`) and shared
test helpers and goldens in `test/eas/`. Templates live in
`templates/` with their bindings in `examples/`, conformance specs in
`test/conformance/`, the official Vega-Lite gallery in
`test/vl-examples/`. The repository root is one level above `src/`
(`eas-template--root`). financial-chart.el keeps its financial
templates, the candle and linked-tickers demos and the indicator
transform, and depends on eas.el.

## 11. Bead map

Foundations, in order:
- `.14` name + spikes
- `.15` spec, template and resolve
- `.16` data and transforms
- `.17` compile to scene
- `.18` renderers
- `.19` runtime
- `.21` conformance

Interactions on the runtime: `.1` `.2` `.3` `.4` `.5` `.6` `.7` `.8` `.9`
Surfaces: `.10` agent surface, `.13` org-babel
Migration: `.12` financial kinds to templates; `.20` health-charts.el convergence; `.11` extraction
Acceptance: `.22` demo gallery and recordings

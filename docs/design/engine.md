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
statically through `bin/chart` door into SVG, PNG, PDF or
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
live data is not a separate code path.

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
needed only if the kind needs a new transform.

A slot that holds an array can expand into views:
`{"x-eas:each": SLOT, "spec": X}` in any array becomes one X per item,
and inside X `{"x-eas:item": KEY, "default": D}` reads the item (`"."`
is the item itself; a missing KEY with no default drops its key). The
`ohlc` template draws one overlay layer and one pane per entry of its
`indicators` and `oscillators` slots this way (`fc-qx1.36`).

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
```

Reducers are pure: `(state, event, scene) -> state'`. Then
`compile + render` runs only if the visible output changed. GUI and
terminal glue only translate native Emacs events (posn, keys,
xterm-mouse) into event/v1. So every interaction can be tested in
`--batch`, replayed from a log, and driven by an agent by sending the
same JSON a mouse would produce.

The log is a bounded ring. It is how an agent learns what the human
just did ("brushed Mar 1 to Mar 15 on TSM price") without a
screenshot.

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
| click target (`.1`, `.5`) | `encoding.href` + `x-eas.actions` | action registry keyed by mark or param; `RET` in text |
| crosshair (`.2`) | `point` selection, `on: pointermove`, `nearest: true`, plus a rule layer filtered by it | hit-test index -> datum -> state.hover |
| zoom/pan (`.3`) | `interval` selection with `bind: "scales"` | reducer edits scale domains; wheel, drag, keys |
| brush (`.4`) | `interval` selection with `encodings: ["x"]` | state.params[name] = range; selection verb |
| linked views (`.6`) | the same param across `vconcat`/`hconcat` (top-level `params` with `views`), shared scale binds, `scale.domain: {"param": ...}` | one state per spec; cross-buffer views join a named param bus that delivers `link` events (spikes section 11) |
| legend toggle (`.5`) | `point` selection with `bind: "legend"` | legend `:map` areas |
| live data (`.7`) | `x-eas.stream` | `eas-push`, frame cap, pause while pointer/brush active |
| values strip (`.34`) | none: always on, no mode | state.pointer column (else latest datum) -> a line under the plot; inspect `strip` |

Hover is touch-only (`.34`): state.hover, tooltips and `pointermove`
selections without `nearest` need the pointer on a drawn mark (within
the stroke, symbol or box, plus half a character cell); `nearest`
selections, and so the crosshair, still follow the nearest datum. What
a chart reads where the pointer merely is goes in the values strip.

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
  without (eas-spec-props.el, fc-qx1.42) are `check` warnings with
  `ignored: true` and do not make a view static. The native subset
  grows one gallery entry at a time.
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

A domain package is only a template directory and a transform file.
financial-chart.el and health-charts.el become exactly that, and new
domains (research indicators, KPIs, sales pipeline) start the same way.

## 8. Where it sits in the wider system

- `bin/chart` (chart-runtime): the static export door and
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
- org (artifact-system "org is the workbench"):
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

The engine lives in `src/eas/` with the `eas-` prefix, its own
tests and no reference to financial-chart from the first commit.
Templates live in `templates/`, conformance specs in `test/conformance/`.
Extraction (`fc-qx1.11`) is then mechanical: move the directory to its
own repo with package headers, register it in the package registry
`projects/registry.json`, and load it via `external-packages.txt`.

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

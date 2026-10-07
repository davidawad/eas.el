# Live frames, SVG half (eas-b2s.2)

How fast a live SVG view can draw a frame, where the time goes, and what
0.2.4 changes. Box (2) of eas-b2s: the SVG output and the GUI glue
(`eas-svg.el`, `eas-svg-retain.el`, the image path of `eas-mode.el`).
The text renderer (eas-b2s.1) and the compile/update path (eas-b2s.3)
are measured and changed by their own boxes.

## Method

`scripts/bench-frame-svg.el` times the live SVG workloads headless:

```sh
emacs -Q --batch -l scripts/bench-frame-svg.el -- MODE [SRC] [FRAMES]
```

MODE is `interpreted`, `byte` or `native`. Byte and native copy SRC to a
temporary directory and compile it there (eight compilers at a time), so
an older checkout (`git worktree add /tmp/base-repo 85d350e`) is benched
the same way. A frame is

- **update**: the push, tick, slider step or pointermove (`eas-dispatch`,
  `eas-play-tick`), the compile/update path;
- **draw**: what the GUI glue does with the new scene, `eas-mode-redraw`
  in an `eas-view-mode` buffer: `eas-svg-image` (the SVG string and the
  :map hot spots), the image inserted and its hot-spot keys bound.

The rasterization librsvg does in a GUI frame is not in a batch Emacs.
fc-qx1.24 measured it on NS (about 90 ms for an unchanged 1000x640
hover before 0.2.3 kept the image cache entry); it is the next budget to
measure on a display (see Phase 3).

Workloads: a 25-level order-book ladder (bars and size labels, 2x25
bands, fixed size domain) and a depth chart (stepped areas, fixed
domains) over the same book, 10 levels changing per push; clock and
pacman timer ticks; a pi-monte-carlo slider step (10 more samples,
1000 points); an airport-connections hover (298 airports, 51 states).

## Where the time went (0.2.3, byte-compiled)

CPU profile of `eas-svg-image` over recorded ladder frames, warm cache
(every mark fragment a hit):

| stage | share |
|---|---:|
| axes rebuilt as DOM nodes every frame (`eas-svg--axis`, theme lookups) | 37% |
| `svg-print` (one `format` per tag and attribute) | 28% |
| hot spots: per-item `intern (format ...)`, and axis label areas | 17% |
| legends, header, the rest | 18% |

Cold (the realistic case: 10 of 50 bars changed) the bar and text marks
missed the mark-level fragment cache as a whole, so all 100 items were
rebuilt and printed: 4.1 ms of `eas-svg-image` against 2.0 ms warm.

In the glue, `eas-mode--hot-spot-keys` built a fresh keymap each frame
with three `define-key` per area: 60 ms of a 95 ms pi-monte-carlo draw
(1000 points, 3000 bindings), 2-3 ms of pacman's and airport's.

## What changed

All of it keeps every SVG byte-identical (the goldens are untouched, and
`eas-svg-retain-random-frames-equal-fresh-renders` checks random push,
tick, slider and hover sequences against `svg-print` of the plain DOM).

1. **Retained items** (`eas-svg-retain-item`). Each mark item's printed
   node is kept, found by content: the mark kind and the item plist. A
   push re-prints only the items that changed, and a state seen again (a
   hover going back) hits. The table hashes with
   `eas-svg-retain--hash`: `sxhash-equal` looks 3 levels down and at
   the first 7 elements of a vector, so every version of a series whose
   first points stay would share one bucket, and a long stream would
   compare against a growing chain of old versions on each lookup. The
   hash adds every point of a series item; other items cost what
   `sxhash-equal` does. Items with a gradient fill record a
   definition as they print, so they are never retained; image marks
   (file reads) neither.
2. **Retained axes and legends** (`eas-svg-retain-part`). An axis prints
   to (GRID . REST) strings, a legend to one string, each kept in a slot
   of its own (view id and axis index or legend channel) with the axis
   or legend and the theme it was printed for: a hit is one `equal`
   against the last version, and a slot holds one value, so a panning
   axis does not pile up versions. Grids still go behind the marks and
   zindex still decides.
3. **A printer without per-node `format`** (`eas-svg-retain--pieces`):
   strings are pushed onto a list and joined once; output equals
   `svg-print`'s (a test checks colon attributes, raw strings, numbers).
4. **Cheaper nodes**: attribute symbols are looked up, not interned from
   a substring, per attribute; `eas-svg--escape` skips the regexp
   replace when there is nothing to escape.
5. **Retained marks** (`eas-svg-retain-mark`). A mark's printed SVG
   (and the gradient definitions it records) is kept in a slot of its
   own, view id and mark id, last version only. It replaces
   `eas-render-cache-svg-fragment` for SVG, which keyed on the whole
   mark: every pushed version of a mark landed in one bucket, so over
   2000 ladder pushes 0.2.3's draw crept from 5.1 to 7.1 ms per frame
   (byte-compiled, 250-frame means). With slots it stays flat at about
   0.9 ms. Hits still count in `eas-render-cache-stats`.
6. **Hot spots**: areas are retained per mark, in a slot like the axes
   (a frame that changed other marks reuses them), and ids
   `eas:VIEW|MARK|I` are interned once per index. A per-item area
   cache was measured and dropped: hashing an
   item costs what computing its area does.
7. **Glue** (`eas-mode.el`): the buffer keeps one hot-spot keymap and a
   redraw binds only ids no earlier frame bound. A redraw whose SVG data
   and :map equal the shown image's leaves the image and rewrites only
   the values strip (no flush, no new image spec, nothing for redisplay
   to re-rasterize).

Retained items live in two generations of 8 MB
(`eas-svg-retain-max-bytes`), parts in at most 4096 slots
(`eas-svg-retain-max-parts`); all of it is off with
`eas-render-cache-enabled`.

## Before and after

Same machine, same run (`git worktree` of 0.2.3 at 85d350e for
"before"), Emacs 30.1, 8 cores; mean ms per frame over 40 frames,
averaged over two alternating rounds. "draw" is the SVG and glue half
this box owns; "frame" adds the update, which this change does not
touch (its variation between before and after is noise of the compile
path).

| workload | mode | draw before | draw after | draw speedup | frame before | frame after |
|---|---|---:|---:|---:|---:|---:|
| ladder push (25 levels) | byte | 4.42 | 1.04 | 4.3x | 5.72 | 2.33 |
| depth push (25 levels) | byte | 1.53 | 0.96 | 1.6x | 3.09 | 2.61 |
| clock tick | byte | 0.53 | 0.33 | 1.6x | 1.31 | 1.11 |
| pacman tick | byte | 4.21 | 1.01 | 4.2x | 6.67 | 3.52 |
| pi-monte-carlo slider step | byte | 134.64 | 24.03 | 5.6x | 739.50 | 624.33 |
| airport-connections hover | byte | 3.74 | 0.65 | 5.8x | 4.73 | 1.41 |
| ladder push (25 levels) | native | 3.73 | 0.76 | 4.9x | 4.63 | 1.65 |
| depth push (25 levels) | native | 1.23 | 0.71 | 1.8x | 2.25 | 1.79 |
| clock tick | native | 0.41 | 0.24 | 1.7x | 0.98 | 0.83 |
| pacman tick | native | 3.67 | 0.73 | 5.1x | 5.22 | 2.56 |
| pi-monte-carlo slider step | native | 128.72 | 17.49 | 7.4x | 538.59 | 427.11 |
| airport-connections hover | native | 3.15 | 0.42 | 7.4x | 3.80 | 0.95 |

Draw is now about a millisecond or less for every live workload but
the 1000-point slider: 0.3-1.0 ms byte-compiled, 0.2-0.8 ms native.
Native compilation is worth another 25-35% on draw and about 30% on the
update.

Byte identity, beyond the goldens: every conformance spec, every
vl-example and every template example (396 scenes) rendered by 0.2.3
and by this change, twice each (cold, then retained), give the same SVG
and the same :map, all 396. The one difference found was a gradient id
(`eas-paint--id` hashed a plist of keywords, which `sxhash-equal`
hashes by address, so the id changed between Emacs sessions, at 0.2.3
too); it now hashes the printed gradient and holds across sessions.

## What is left, per frame

After this change the SVG draw is the smaller half of every workload:
the update (compile/patch, eas-b2s.3) is the larger.
Remaining draw costs, byte-compiled:

- **Axis click areas** (`eas-action-callback-hot-spots`, an
  `eas-svg-hot-spot-functions` member): about 10% of a ladder draw. They
  depend on the scene's target, size and config and on the axis, so they
  can be retained per axis like the axis nodes; the file belongs to the
  action layer, left for its owner.
- **Item misses**: a pi-monte-carlo step resamples every point, so every
  item misses; 1000 circles cost about 15 ms to build as DOM nodes and
  print. Emitting each common item kind (rect, circle, text, path)
  straight to a string, with no DOM node and no `apply`, is the next 2x
  there.
- **Change detection by `equal`**: a mark slot compares the new mark
  with the last one; when the compile path reuses the mark object that
  is one `eq`, otherwise a walk to the first difference. A dirty set
  from the update path (which marks it rebuilt) would make it free.

## Phase 2 (eas-b2s.6): large scenes, direct printers, retained click areas

The perf suite (`make bench`, docs/perf.md) found the next hot spots:
a county-map hover at about 270 MB and 1.3 s a frame, the projections
template's first render at 3.6 s, a pi-monte-carlo step at 17-24 ms of
draw, and axis click areas at about 10% of a ladder draw.

### Where the time went

Profiled per stage (`memory-use-counts` deltas and the CPU profiler,
byte-compiled; `scripts/bench-frame-svg.el` now reports KB consed per
half of a frame too):

- **County hover**: 298 MB and 950 ms of a frame were the *update*, not
  the SVG: a pointermove patches the plan, and the patch rebuilds every
  item of a geoshape unit (`eas-compile-patch.el` lists geoshape with
  the series kinds), and `eas-geoshape-items` re-projected all 3,000
  counties through the d3 stream pipeline to restyle one of them. The
  SVG draw was 12 MB and 18 ms of retained-item lookups (one
  `sxhash-equal` and `equal` per county).
- **Projections first render**: 24 maps of the 110m world (about
  20,000 points each, resampled), about 600,000 projected points at
  about 3 us each: 60% of the open is the stream pipeline (closures and
  boxed floats per point), 24% is `eas-resolve-hash` serializing the
  resolved spec, inline GeoJSON included, to JSON to hash it.
- **Item misses**: a pi-monte-carlo step changes every circle; building
  1,000 DOM nodes and printing them (one `format` per tag and attribute
  name, one `format` per float) was most of its draw.

### What changed (every SVG byte-identical)

1. **Projected shapes are retained** (`eas-geoshape--project`): per
   projection (its plain :spec, the last 4 kept) a weak table maps each
   GeoJSON object, by identity, to its anchor and anchor-relative rings.
   A frame that restyles a map re-projects nothing; a new projection (a
   resize, a changed projection param) projects afresh.
2. **Items reused by position** (`eas-svg--mark-strings`): a mark's
   slot keeps its last items and their printed text; an item `equal` to
   the last version's item at its index takes that text. A patch keeps
   the unchanged items themselves, so the test is mostly one `eq`, and
   only changed items are printed or looked up.
3. **Direct printers** (`eas-svg--item-string`) for rect (no corners),
   rule and tick, point/circle/square symbols (circle, square, Vega
   symbol paths) and one-line text: the attribute pieces are pushed
   straight onto a list and joined, no DOM node, no `apply`, no
   `format` of attribute names. A changed item of these kinds skips the
   content cache too (its hashing cost what printing does).
   `eas-svg-retain-direct-printers-print-nodes` checks 3,000 random
   items (every optional property in or out) against the printed node.
4. **Float formatting cache** (`eas-svg--n`): a float's text is looked
   up by value (`eql`), the table emptied at 65,536 entries.
5. **One concat for the document** (`eas-svg-retain-to-string`):
   `eas-svg-render` no longer inserts thousands of pieces into a temp
   buffer and copies it out.
6. **Axis click areas retained** (`eas-action-callback--axis-areas`):
   per axis slot, keyed on the axis and the scene's target, size and
   axis config (what the label boxes are measured from); their :map
   areas are retained while the areas are the same, so ids are not
   interned again each frame.
7. A rotation with both a longitude shift and a tilt built two closures
   per point (`eas-geo-rotation`); they are built once.

The random-sequence property test (`eas-svg-retain-random-frames-equal-
fresh-renders`, now 60 frames) also checks the action click areas
retained against fresh ones, and that items are reused by position.

### Before and after

Same machine and run (8 cores, Emacs 30.1), "before" a worktree of
1d3cd8f (phase 1), `emacs -Q --batch -l scripts/bench-frame-svg.el --
MODE SRC 20`: mean ms per frame over 20 frames (10 for the county sweep,
2 opens for the first render), KB consed per frame.

| workload | mode | draw ms before | draw ms after | frame ms before | frame ms after | KB/frame before | KB/frame after |
|---|---|---:|---:|---:|---:|---:|---:|
| ladder push (25 levels) | byte | 1.50 | 0.82 | 3.08 | 2.18 | 394 | 281 |
| depth push (25 levels) | byte | 1.55 | 1.01 | 3.63 | 3.11 | 415 | 347 |
| clock tick | byte | 0.72 | 0.63 | 1.59 | 1.42 | 188 | 182 |
| pacman tick | byte | 3.85 | 2.78 | 7.30 | 5.21 | 813 | 810 |
| pi-monte-carlo slider step | byte | 26.05 | 14.57 | 617.27 | 601.33 | 89978 | 88378 |
| airport-connections hover | byte | 0.86 | 0.93 | 1.62 | 1.85 | 363 | 348 |
| county-unemployment hover | byte | 26.80 | 6.16 | 1147.98 | 54.69 | 269980 | 5665 |
| airport-connections sweep | byte | 2.25 | 2.78 | 11.31 | 12.27 | 3113 | 3092 |
| projections first render | byte | 299.67 | 368.68 | 3965.47 | 3809.61 | 865921 | 860513 |
| ladder push (25 levels) | native | 1.06 | 0.50 | 2.11 | 1.37 | 394 | 281 |
| depth push (25 levels) | native | 1.21 | 0.54 | 2.66 | 1.66 | 415 | 347 |
| clock tick | native | 0.65 | 0.50 | 1.42 | 1.14 | 188 | 182 |
| pacman tick | native | 1.99 | 2.58 | 3.83 | 5.06 | 813 | 809 |
| pi-monte-carlo slider step | native | 17.97 | 10.80 | 390.86 | 383.83 | 89978 | 88378 |
| airport-connections hover | native | 0.85 | 0.50 | 1.58 | 1.00 | 363 | 348 |
| county-unemployment hover | native | 16.95 | 4.19 | 962.74 | 33.36 | 269980 | 5665 |
| airport-connections sweep | native | 1.68 | 1.36 | 8.81 | 9.05 | 3113 | 3092 |
| projections first render | native | 303.51 | 291.18 | 3008.58 | 3039.20 | 865921 | 860513 |

Allocation is deterministic; times at 20 frames move by about 0.3 ms
between runs of the same tree (the pacman, airport and projections
draw rows go both ways between byte and native: noise of that size).
The county hover is 21x (byte) and 29x (native) cheaper per frame, and
allocates 48x less; draw is 4-6 ms. Draw falls by 35-55% on the
ladder, depth and slider workloads. The byte-compiled projections draw
(a cold render: every float new to the format cache) measured 300 to
369 ms over 2 opens, native 304 to 291. Rendering that cold scene alone
with the float cache and without it (the plain `format`, byte-compiled,
three alternating runs, retained parts and the cache emptied each time)
gave 621-665 ms against 598-683 ms: the cache costs a cold scene about
nothing, so that row is noise of a 2-open sample.

### What is left, and whose it is

- **County hover update, 29-49 ms**: with projection retained, the
  update is `eas-marks--style` (condition tests per county) and
  `eas-marks--extras` (every county's tooltip formatted again,
  `eas-encode-format-value` / `format`) for all 3,000 rows, because the
  patch rebuilds every geoshape item. Owner: the compile/update path
  (eas-b2s.3): patch geoshape units item by item as `eas-patch--items`
  does for points (the projection is now retained, so a rebuilt item
  costs only its style), or memoize the tooltip per row when the
  tooltip encoding reads no param. That reaches the 20 ms target.
- **Projections first render, 3-3.8 s**: 24% is `eas-resolve-hash`
  (eas-resolve.el: JSON-serializing the resolved spec with its inline
  GeoJSON to hash it; hashing the data by reference or by its source
  would remove it). The 60% in the stream pipeline is about 3 us per
  projected point through four closure stages with boxed floats;
  inlining `eas-geo-point` and friends measured no gain. A real cut
  needs the pipeline fused per projection (rotate, project and
  resample in one function on unboxed locals) or a dynamic module: a
  rewrite of the d3 port, measured and not started here.
- **pi-monte-carlo update, 370-590 ms**: the compile path (eas-b2s.3).
- **Changed-mark list from the update path** (plan item 5): not
  needed. A patch keeps unchanged items `eq`, `equal` returns at once
  on `eq`, and the positional reuse above makes an unchanged item one
  comparison; a list from `eas-compile-patch.el` would save only the
  mark-level `equal` walk to its first changed item.
- **GUI image strips** (Phase 3): still needs a display to measure;
  unchanged.

## Phased plan

- **Phase 1 (this change)**: retained items, marks, axes, legends,
  hot spots and hot-spot keys; faster printer; unchanged image kept;
  no slowdown over a long stream. Done, measured above.
- **Phase 2 (eas-b2s.6, done)**: direct string printers per item kind,
  items reused by position, retained axis click areas, retained geoshape
  projection; measured above. The dirty-mark set proved unnecessary.
- **Phase 3 (GUI raster, needs a display to measure)**: librsvg
  rasterizes the whole image on every changed frame, and that is the
  largest GUI cost left (tens of ms at 1000x640, fc-qx1.24). Splitting a
  live view into horizontal image strips (static ones keep their image
  cache entry, a strip whose SVG changed re-rasterizes alone), with
  :map areas and `posn-object-x-y` offset per strip, bounds the raster
  to the changed band. Measure on X and NS before building it.
- **Native-comp / dynamic module**: native compilation already gives
  most of what a module would on these loops (see the table); with the
  draw at about a millisecond, a Rust module is not worth its build
  and distribution cost.

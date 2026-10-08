# Geo first render: where the time goes, and the floor (eas-gzi)

The goal set in eas-gzi was a first render under 200 ms for the
projections template (24 world maps) and for every map template, with
SVG and text output byte-identical and no on-disk cache.  The first
render of the projections template takes about 2 s byte-compiled in a
fresh Emacs.  This note says where that time goes, what was cut, and
why 200 ms cannot be reached without changing the output.  The last
section is the decision that followed: an optional native module
(module/, src/eas-geo-native.el) for the geo hot path, Elisp kept as
the source of truth.

## Measuring

- `cold/NAME` in `make bench` (src/eas-perf.el) opens and draws a map
  template with every cache emptied first: files read, projected
  shapes, recorded spherical streams, memos and SVG fragments.  It
  covers projections, county-unemployment, map-with-tooltip and
  world-map.  `render/NAME` warms up first, so for a map it measures a
  re-render from the projected shapes (projections: about 150 ms).
- A fresh `emacs -Q --batch` (byte-compiled copy, default
  `gc-cons-threshold`) that opens and draws once also counts the GC that
  the perf suite's 64 MB threshold hides.  All numbers below are from an
  8-core Linux box with Emacs 30.1.  Timings vary by about 5% from run to
  run, so before/after numbers are alternating A/B runs.

## The projections template, after this pass

24 maps of 178 shapes each (177 countries and the graticule) plus the
sphere.  The spherical half of the pipeline (rotation, preclip) is
recorded once per rotation and clip, 10 settings, and gives 116k
points.  Each projection replays those points through d3's adaptive
resampling.  Together the maps have about 315k output points, and the
SVG is 4.7 MB.  The compile conses about 550 MB, nearly all of it
boxed floats.

| stage (fresh Emacs, byte) | ms | notes |
|---|---:|---|
| recording the spherical streams | ~330 | clip and rotate ~180, polygon-contains ~90, recorder ~55 |
| replay: resample and project | ~800 | raw projection math ~600 (guyou and peirceQuincuncial ~150, the interrupted Mollweides ~85) |
| anchor and relative coordinates | ~250 | ring areas, the largest ring's centroid, the relative copy |
| SVG path numbers | ~230 | 630k numbers |
| rest of the SVG and the compile | ~450 | eas-svg, eas-marks, eas-compile (not geo code) |
| GC | ~300 | at the default threshold; see below |

## What this pass changed (output unchanged)

- Path numbers: `eas-geoshape--push-n` prints a number from two tables
  of strings, the whole part and the hundredths, chosen by `round` of
  100 V.  It does not call `format "%.2f"` and then trim.  The result is
  provably the same: the rounding error of 100 V is below 1e-9 under
  8191, so only values within 1e-4 of a tie fall back to `format`.
  NaNs and large values fall back too.  A test compares it with
  `eas-svg--n` on 20k values and on the edge cases.  The pieces are
  joined with one `concat` per item.  Inserting them into a buffer is
  10 times slower: about 0.27 us per `insert` of a short string,
  against 0.03 us per piece for `concat` over a list.
- The resampler checks inline whether a segment is short enough to
  stop, so most segments cost no call to `eas-geo--resample-line`.
- `eas-geo-elliptic-f` precomputes, per M, the steps of its
  arithmetic-geometric mean.  These are the same floats as before.
- `eas-geo-raw-mollweide` remembers its last latitude.  An interrupted
  Mollweide projects every point twice at one latitude, and the Newton
  iteration now runs once.
- Small helpers are inlined (`defsubst`): `eas-geo-point`, `eas-geo--xy`,
  `eas-geo-asin` and friends, `eas-geo--longitude`.  The projection
  closure multiplies by a precomputed `k*sx` (exact, since sx is 1 or
  -1).  The degrees-to-radians stream takes one closure per point
  instead of two.
- `eas-geoshape-items` calls `eas-gc-defer`.  In an interactive session,
  a map's first compile no longer collects every few megabytes.  The
  collection waits until Emacs is idle, as it does for interactions.
  Batch Emacs is unaffected.  In batch, a 64 MB threshold took 100-200
  ms off the projections first render, out of about 300 ms of GC.

Results (byte-compiled, alternating fresh-Emacs runs):

| template, first render | before (ms) | after (ms) |
|---|---:|---:|
| projections (fresh Emacs, min of 3) | 2282 | 2035 |
| county-unemployment | 1403 | 1380 |
| map-with-tooltip | 820 | 820 |
| world-map | 188 | 173 |
| projections, `cold/` SVG, byte (64 MB GC threshold) | 1680 | 1547 |
| projections, `cold/` SVG, native | 1374 | 1245 |

Path printing went from about 480 to 390 ms for the projections draw.
The geo compile lost about 150 ms.  county-unemployment and
map-with-tooltip spend their time in albersUsa's three inset streams
and their clips, which this pass did not restructure.

## The floor

Times per operation, from microbenchmarks of the byte-compiled engine:

| operation | cost |
|---|---:|
| `funcall` of a 2-argument closure returning a fresh `[x y]` | ~0.1 us |
| the projection closure (raw call, scale, translate, vector) | ~0.24 us |
| a raw projection: sinusoidal / Mollweide / guyou | 0.09 / 2 / 5.8 us |
| `format "%.2f"` of one float | ~0.32 us |
| a path number from the tables (round, two lookups, pushes) | ~0.15 us |
| a boxed float result (every float operation allocates 16 bytes) | ~0.02-0.05 us |

At 200 ms for 315k output points, each point would get about 0.63 us
for everything: projecting it, resampling, collecting it, its anchor
and relative copy, and printing its two numbers.  That is less than
two `format "%.2f"` calls, or than the projection closure plus a
single Mollweide.  Even with every stage reduced to its float
arithmetic and nothing else, the per-point work is roughly 1.5-2 us
in byte code: about 30 float operations for the projection, the
resampling test, the shoelace sums and the relative copy, and 2 x 0.15
us to print.  So the projections grid has a floor of about 500-600 ms
in byte code with no GC.  Its 550 MB of boxed floats adds GC on top,
unless collection is deferred.  Native compilation removes the bytecode
dispatch but still boxes every float: measured, it takes 15-20% off.

The single-map templates hit the same floor per point.  world-map is
about 13k output points.  It takes ~120 ms cold in the perf suite and
~200 ms in a fresh batch Emacs, where GC is included.  county-unemployment
has 3.2k counties projected through albersUsa's three insets: every
point is resampled and clipped three times, as in d3.
map-with-tooltip is a similar case.  They stay near 1.3 s and 0.7 s.

What would get under 200 ms changes either the output or the
architecture:

- Fewer points.  Pre-simplify the TopoJSON, or use a coarser `precision`
  than d3's 0.707 px resampling.  Either one changes the paths.
- Fewer digits: one decimal instead of two.  This changes the SVG.
- Draw only the visible maps.  A 940x1740 grid shows a few maps per
  window, but the scene (scene/v1) is complete by contract, and hit
  testing and the text renderer read all of it.
- Skip off-sheet shapes.  This only pays when shapes are off-sheet.
  Every shape in these templates is drawn.  For albersUsa, proving
  that an inset's rectangle clip emits nothing for a shape takes the
  very projection it would save.
- A persistent cache of projected paths, which eas-gzi rules out, or a
  dynamic module.

## Third pass: output allowed to change, visually lossless

eas-gzi was reopened with the output allowed to change, as long as it
stays visually lossless.  Two changes, both precision or
simplification only:

- Path numbers have one decimal, a tenth of a pixel
  (`eas-geoshape--push-n`).  The error is at most 0.05 px, or a tenth
  of a device pixel at 2x.  Circles and every non-map mark keep
  `eas-svg--n`.
- Geoshape items simplify their projected lines in the path sink
  (`eas-geo-path-sink`, `eas-geoshape-tolerance` = 0.06 px).  A point
  is dropped when it lies closer than the tolerance to the segment
  from the last kept point to the point after it.  At most
  `eas-geo--simplify-run` points are dropped in a row, and a line's
  ends stay.  A ring of under 16 points that would be left with fewer
  than four keeps them all.  The geo-measure transform still measures
  every point.

The bound is measured, not proved.  The full-precision render
(hundredths, no simplification) and the new one are rasterized with
rsvg-convert at 1x and 2x, and pixels are compared with the oracle's
pixelmatch threshold (`eas-png--count`, YIQ 0.1).  The projections
grid is the worst case.

| projections, tolerance | differing pixels 1x | 2x | SVG bytes |
|---|---:|---:|---:|
| none (one decimal only) | 0.000% | 0.008% | 4.04 MB |
| 0.05 px | 0.0004% | 0.015% | 3.09 MB |
| 0.06 px (chosen) | 0.0015% | 0.028% | 2.98 MB |
| 0.08 px | 0.006% | 0.073% | 2.77 MB |
| 0.1 px | 0.019% | 0.150% | 2.57 MB |
| 0.125 px | 0.062% | 0.303% | 2.36 MB |

`eas-vega-geo-gallery-simplified-paths-rasterize-as-the-full-paths`
(:gallery, about a minute) asserts the 0.1% bound for every map
template.  The Vega reference ratios
(`eas-vega-geo-templates-match-their-references`) do not move beyond
0.0001.  No text or SVG golden in the repository changed: none of
them draws a projected geoshape path.

### Time: the floor holds

Halving the points saves less than it looks.  The points are dropped
after they are projected, and the projection (recording, resampling,
raw math) is two thirds of the time.  The drop test costs about as
much per point as collecting the point did.  What gets cheaper is the
anchor and relative copy, the SVG numbers and the SVG's size.

First render, byte-compiled, fresh `emacs -Q --batch`, alternating
old/new runs, minimum of 3 (timings vary 5-15% run to run):

| template | before (ms) | after (ms) | SVG before | SVG after |
|---|---:|---:|---:|---:|
| projections | 2175 | 2087 | 4.72 MB | 2.98 MB |
| county-unemployment | 1437 | 1453 | 583 KB | 473 KB |
| map-with-tooltip | 813 | 863 | 596 KB | 485 KB |
| world-map | 174 | 161 | 189 KB | 131 KB |

The perf suite's alloc counts (`make bench-check`): cold/projections
svg +4.8%, text -7.9%; render/projections svg -6.7%; the other cold
maps +2.5 to +4.6%, inside the 10% gate.  The drop test boxes a few
floats per point.  The 200 ms goal is not met.

Points cannot be dropped before projection without a bound on the
projection's local stretch.  For the projections grid the safe
spherical tolerance (0.06 px over the scale, about 50 px per radian,
with a stretch margin) is about 0.02 degrees.  world-110m's points are
0.1 to 1 degree apart, so nothing would go.  The 300 x 200 maps
really do have about one point per half pixel.

Still out of reach without changing the architecture:

- Lazy facets in live views.  The projections grid is a vconcat of
  hconcats, not a facet, and the scene (scene/v1) must be complete for
  hit testing, the text renderer and export.  Drawing only the visible
  maps would need a scene with deferred views, which is not in
  eas-gzi's file scope (eas-compile, eas-mode).
- A disk cache per (projection, size).  The projections grid's paths
  are about 3 MB of SVG per size, and the cache key would have to
  cover the code, the data file and every projection parameter.  It
  only helps a second first render, and it was not built.

## Fourth pass: the optional native module

The decision recorded in eas-gzi: reach the target with an optional
compiled module for the projection math, keeping pure Elisp as the
source of truth and as a fully supported fallback.  eas keeps no
dependencies; the module only accelerates.

### What it is

- `module/` is a Rust crate (package eas-geo-module, library eas_geo,
  a cdylib).  It ports eas-geo-stream.el (rotation, adaptive resampling,
  the recorded spherical streams and their replay, the path and bounds
  sinks, the planar measures), eas-geo-clip.el (antimeridian, small
  circle and rectangle clips, rejoin, spherical point-in-polygon),
  eas-geo-raw.el and eas-geo-polyhedral.el (all 35 raw projections: the
  24 of the projections gallery, the d3-geo ones, the interrupted ones,
  their outlines), eas-geo-proj.el (scale, translate, centre, three-axis
  rotation, angle, reflection, clipAngle, clipExtent, mercator's own
  extent, albersUsa's three insets, fitting) and the path printer of
  eas-geoshape-render.el (one-decimal numbers, the 0.06 px
  simplification).
- It has no crates at all.  Its binding to emacs-module.h is
  hand-written (module/src/ffi.rs, about 200 lines): the 27 function
  pointers of `struct emacs_env_25`, which every later environment
  (26 to 30) begins with in the same order, so one layout serves every
  Emacs with modules.  emacs-module-rs was the alternative; it is not
  smaller (its macros pull in syn, quote, proc-macro2, ctor and more, a
  dozen crates and their version churn) and not stabler (the module
  ABI is what is stable, and the hand binding uses only it).  The
  module checks the runtime and environment sizes before using them.
- src/eas-geo-native.el loads it lazily, on the first map, from
  `<root>/lib/eas-geo-module<suffix>`, `<root>/module/`, or beside the
  library (a flat install), the suffix being the running Emacs's
  `module-file-suffix`; a cargo build in place
  (`module/target/release/libeas_geo.so` or `.dylib`) is found too.
- `eas-geo-backend` (group eas): `auto` (default) uses the module when
  it loads, else Elisp; `native` requires it (a `user-error` saying why
  when it is missing or fails to load, and module errors are signalled);
  `lisp` never loads it.  `eas-geo-backend-active` returns `lisp` or
  `native`.  Under `auto`, a module error at run time drops the module
  with one message, and Elisp draws on (and draws the same).

### Its interface: batches, not points

A geometry is read once into a handle (`eas-geo-module-geometry`, kept
weakly per coordinates object, as the Elisp keeps its recordings), and
then one call per map unit, `eas-geo-module-shapes`, takes the
projection's resolved parameters (`:native`, which `eas-geo-proj` now
adds to every projection) and every shape of the unit.  For each shape
it returns what `eas-geoshape--project` makes, the anchor and the paths,
circles and box relative to it, plus the SVG path data of the paths at
the item's position, which `eas-geoshape-svg-d` uses when the item has
not moved.  `eas-geo-module-fit` returns a fit's bounds.  So a map costs
one crossing per unit, not one per point, and the scene (scene/v1)
holds the same float vectors as before: the text renderer and hit
testing read them unchanged.

### Exact, not approximately equal

The goal was byte-identical SVG between backends, and that is what the
module achieves: every map template draws byte-identical SVG with both
backends (`eas-geo-native-map-templates-svg-is-identical`, :gallery),
so the 0.1%-of-pixels fallback bound is not needed.  The projected
values themselves are compared bit for bit
(`eas-geo-native-projections-match-lisp`: every projection type, and
rotation, clipAngle, clipExtent, reflection with angle, precision 0 and
large rotations on ten of them, over sphere, graticule, points, lines,
a geometry collection and countries, plus fit bounds).  What it took:

- Each expression keeps the Elisp's order: `(* a b c)` is `(a*b)*c`,
  integers convert as Elisp converts them, nothing is simplified.
- Elisp's `min` and `max` let a NaN win and keep the first argument on
  ties (`(max 0 -0.0)` is 0); `eql` on floats compares bits.  The port
  has helpers with those semantics.
- Transcendental functions are glibc's libm, as Emacs's.  One trap:
  `(expt x 2)` calls `pow`, which glibc does not always round
  correctly, while LLVM folds `x.powf(2.0)` into `x*x`; the module calls
  `pow` with an exponent the optimizer cannot see.
- Path numbers take the same fast path as `eas-geoshape--push-n` and,
  for ties, NaNs and large values, the C library's `snprintf("%.1f")`,
  which is what Emacs's `format` calls.

Rust unit tests (`cargo test` in module/) compare the raw projections
(3,010 points), the sphere outlines and the clips (about 300k stream
events, the rotated world-110m under ten clips) with values Emacs
prints, by bits.

### First render

A fresh `emacs -Q --batch` per run, best of 6, alternating
configurations; "before" is the engine before this pass, "after lisp"
this pass with `eas-geo-backend` `lisp`.  Total is what a fresh Emacs
spends: the template registry and example bindings (about 80-100 ms,
the same for every template and paid once per session), opening the
view and printing its SVG.  Render is opening and printing alone.  GC
runs at the default threshold.  Timings on this shared 8-core box vary
by 20-40% from run to run.

| template | compiled | before (lisp) total / render | after, lisp | after, native | native render vs before |
|---|---|---:|---:|---:|---:|
| projections | byte | 2480 / 2379 | 2484 / 2421 | 537 / 453 | 5.3x |
| projections | native | 1848 / 1787 | 1880 / 1832 | 300 / 251 | 7.1x |
| county-unemployment | byte | 1560 / 1502 | 1605 / 1526 | 505 / 421 | 3.6x |
| county-unemployment | native | 1285 / 1240 | 1217 / 1164 | 263 / 219 | 5.7x |
| map-with-tooltip | byte | 976 / 919 | 772 / 714 | 315 / 263 | 3.5x |
| map-with-tooltip | native | 898 / 828 | 626 / 578 | 338 / 273 | 3.0x |
| world-map | byte | 225 / 172 | 201 / 143 | 89 / 27 | 6.4x |
| world-map | native | 235 / 173 | 164 / 114 | 71 / 20 | 8.7x |

The SVG md5 is the same in every column.

### Where the native first render's time goes now

The perf suite's `cold-native/NAME` (every cache emptied, 64 MB GC
threshold as `eas-gc-defer` gives an interactive session), against
`cold/NAME` with the Elisp backend, ms, median of 2 frames, from the
`make bench-check` run of this pass:

| template, SVG | cold/ byte | cold-native/ byte | cold/ native-comp | cold-native/ native-comp | allocated, lisp / native |
|---|---:|---:|---:|---:|---:|
| projections | 1814 | 325 | 1400 | 201 | 552 MB / 40 MB |
| county-unemployment | 1550 | 215 | 1027 | 159 | 314 MB / 30 MB |
| map-with-tooltip | 571 | 206 | 533 | 141 | 132 MB / 28 MB |
| world-map | 101 | 43 | 107 | 25 | 32 MB / 5 MB |

A CPU profile of the projections template's native first render (fresh
Emacs, byte-compiled), in shares of its render:

- the module call, about 30%: projecting, resampling, clipping and
  printing 24 x 178 shapes is about 70-120 ms in Rust, and building
  their Lisp float vectors for the scene about 45 ms (400k floats);
  reading world-110m into handles once is about 14 ms;
- the SVG document, about 25-30% (`eas-svg-render`: retained item
  printing, `eas-svg--node`, the mark styles; not geo code);
- the rest of the compile, about 20% (layout, scales, styles of 4,300
  items; not geo code);
- GC, 50-90 ms at the default threshold in batch, which an interactive
  session defers.

county-unemployment and map-with-tooltip spend most of what is left
outside geo code: resolving their data (TopoJSON decode, the CSV join,
`eas-resolve`), per-row styles and tooltips of 3,200 counties, and the
SVG document.

### Also in this pass

- The view's spec hash (`eas-resolve-hash`, the sha256 of the resolved
  spec's canonical JSON, geometry included: 24 copies of world-110m for
  the projections grid) is made on first use (`eas-view-spec-hash`),
  not when a view opens.  It was 70-330 ms of every map's first render.
- `eas-resolve--strip` returns a vector of atoms (a coordinate pair) as
  it is, without copying it into a list first.

### What would still be needed for 200 ms

Where the target is met, with the native backend:

- world-map, everywhere: 20-27 ms to open and draw in a fresh Emacs
  (71-89 ms counting the template registry), 25-43 ms in the perf suite.
- With native-compiled Lisp and GC deferred (the perf suite, as an
  interactive session runs): projections 201 ms, county-unemployment
  159 ms, map-with-tooltip 141 ms.
- Byte-compiled with GC deferred: county-unemployment 215 ms and
  map-with-tooltip 206 ms are at the target, projections (325 ms) is
  not.
- In a fresh batch Emacs, where GC runs at the default threshold
  (50-90 ms of collections), projections (251 ms native-compiled, 453
  ms byte), county-unemployment (219 / 421 ms) and map-with-tooltip
  (263-273 ms) are not under 200 ms.
- projections at 201 ms (p95 212 ms) is at the target, not under it.

The other map templates (annual-precipitation, airport-connections,
dorling-cartogram, volcano-contours, distortion-comparison) spend their
time in contours, Voronoi cells, force layouts and data, not in
projection; the module leaves them about where they were, and they draw
the same SVG with it.

The geo work itself is no longer where the time goes: what remains is
the non-geo pipeline that every chart pays per item (the scene's
styles, the SVG document, data resolution) and GC.  Getting the
projections grid under 200 ms everywhere would need, besides the
module:

- a cheaper scene build per item (styles, tooltips and SVG nodes are
  computed per item in Elisp: 4,300 for the grid, 3,200 counties);
- the SVG document printed without the retained-fragment bookkeeping
  on a first render;
- fewer Lisp floats: the scene holds every path point as a boxed float
  (400k for the grid), which the module must allocate and GC must
  trace.  A packed representation would change scene/v1.

Threads in the module were tried (one shape per task over 8 cores) and
did not pay on the measured box: the batch per map is a few
milliseconds and the first render got slower.  They stay available
behind EAS_GEO_MODULE_THREADS, off by default.

## Fifth pass: the scene around the module

The goal was restated as the user sees it: under 200 ms for the
projections template and every map template, native-compiled, in a
fresh batch Emacs, garbage collection included, and as far under as
possible byte-compiled.  Output stays byte-identical: the same SVG
with both backends and as before (the md5 of every template's SVG is
unchanged), and the parity tests hold.

### Measuring

`scripts/eas-spikes/geo-first-render.sh [N]` compiles a copy of src/
(byte, or byte and native), then for each template, compile mode and
backend starts `emacs -Q --batch` N times, alternating, opens the view
and prints its SVG once at the default `gc-cons-threshold`, and keeps
the best render (open and print), the total (with the template
registry) and the collection a session would run once idle
(`garbage-collect` right after: 25-70 ms).  `EAS_FR_PROFILE=FILE`
writes a flat CPU profile.  This box (8 cores, a 4-CPU cgroup quota)
is 15-30% slower than the one of the fourth pass, so both sides of
every comparison below were measured here, one after the other.

### Where the time went (native-compiled, module, before)

- projections, 317 ms: the module call ~45% (Rust projecting,
  resampling, clipping and printing ~120-146 ms; 400k Lisp floats for
  the scene ~45 ms), collections during the render 45-60 ms, the SVG
  document ~12%;
- county-unemployment, 289 ms: collections ~30%, the module ~18%
  (Rust ~50-60 ms over albersUsa's three insets), data ~20% (TopoJSON
  decode ~40 ms, `eas-resolve--strip`), per-item styles and tooltips
  ~10%.

### What changed

- Collection waits for the render in batch too.  A view's open and
  its SVG or text print call `eas-gc-defer-render`: in a session that is
  `eas-gc-defer` (collection when idle, as for interactions); in batch,
  which is never idle, `gc-cons-threshold` is raised to
  `eas-gc-cons-threshold` (64 MB) and stays.  A scoped binding does not
  do it: the view opens in one call and prints in another, and the
  collection pending at the end of the first would run inside the
  second.  `eas-gc-defer` itself still leaves batch alone.
- Paths stay packed in the module.  `eas-geo-module-shapes` takes a
  seventh argument, PACKED: a shape's relative paths then come back as
  a module handle instead of [CLOSED XY...] vectors of Lisp floats.
  The SVG needs only the path data the module printed; the text
  renderer, hit tests and an item drawn away from its anchor read the
  coordinates through `eas-geoshape-paths`, which unpacks a handle once
  (`eas-geo-module-paths`, memoized weakly per handle) to the same
  values.  Anchors, boxes and circles stay Lisp numbers.  A module
  built before this pass (six arguments) is still used, unpacked.
- Geoshape items print without a DOM node (`eas-geoshape-svg-string`,
  inside the retained-fragment memo, so a re-render still hits): no
  node, no escaping of the path data (it cannot hold XML specials), no
  copy of it.  A test compares it with the node's print over the
  style attributes.
- Data: TopoJSON arcs, lines and rings are decoded into vectors
  directly, not through lists (the same points, the same edge cases,
  checked against the old code on every TopoJSON in examples/data);
  `eas-resolve--strip` copies an array only once an element changes,
  not every ring of a map; a unit computes its tooltip's fields and
  titles once (`eas-encode-tooltip-defs`), not per row.

### Results

Best of 6, render (open and print) in ms, GC included:

| template | compiled | Elisp before | Elisp after | module before | module after |
|---|---|---:|---:|---:|---:|
| projections | byte | 2837 | 2592 | 414 | 313 |
| projections | native | 2314 | 2196 | 317 | 237 |
| county-unemployment | byte | 1675 | 1571 | 360 | 262 |
| county-unemployment | native | 1499 | 1397 | 289 | 196 |
| map-with-tooltip | byte | 988 | 751 | 362 | 230 |
| map-with-tooltip | native | 736 | 601 | 292 | 178 |
| world-map | byte | 179 | 141 | 31 | 27 |
| world-map | native | 154 | 118 | 26 | 22 |

The collection a session then runs once idle is 25-45 ms with the
module and 30-70 ms with Elisp; it is not in the render.  The template
registry and example bindings add 260-290 ms to a fresh Emacs's total
on this box, paid once per session.

So, native-compiled with the module: county-unemployment (196 ms),
map-with-tooltip (178) and world-map (22) are under 200 ms on this
box; projections (237) is not.  Scaled by this box's measured
slowdown against the fourth pass's (1.25-1.3 for these runs),
projections would be about 185-190 ms there.  Byte-compiled, every map
template is 230-313 ms except world-map.

### The floor that remains

What is left of projections' 237 ms: the module ~150 ms, the SVG
document ~30 ms, the compile around it ~40 ms, resolution ~15 ms.  The
module's time is the projection itself: about 400k points through
d3's resampling and 24 raw projections, kept bit-exact with Elisp
(glibc's libm, Elisp's order of operations).  The elliptic ones
(guyou, peirceQuincuncial) alone are ~35 ms.  No shape dominates (the
largest is at most a fifth of its map), the spherical streams are already
recorded once per rotation and clip with their unit vectors, and the
SVG numbers cost ~20 ms.  Threads were measured again: on this box's
4-CPU quota, 8 threads took the projection from 143 to 120 ms, so
they stay off.  Under 200 ms here would need the Rust projection
itself faster without changing a bit, or the grid's maps drawn lazily
(see the third pass).

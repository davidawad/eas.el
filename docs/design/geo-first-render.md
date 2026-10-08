# Geo first render: where the time goes, and the floor (eas-gzi)

The goal set in eas-gzi was a first render under 200 ms for the
projections template (24 world maps) and for every map template, with
SVG and text output byte-identical and no on-disk cache.  The first
render of the projections template takes about 2 s byte-compiled in a
fresh Emacs.  This note says where that time goes, what was cut, and
why 200 ms cannot be reached without changing the output.

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

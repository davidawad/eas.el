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

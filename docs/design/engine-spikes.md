# easel spikes (fc-qx1.14)

Measured numbers, and the decision each one forces. The scripts that
produced them are in `scripts/easel-spikes/` and can be rerun.

Box: Linux 6.8, 4 vCPU AMD EPYC-Rome, GNU Emacs **30.1** (not 30.2). The
build is `--without-x`, and `image-type-available-p 'svg` is nil.
tmux 3.x is present. There is no GUI frame, no librsvg, no macOS and no
iTerm2/kitty. Every number below that needs one of those is listed
under **Unmeasured** and has to be taken on a GUI machine before the
`.2` crosshair redraw strategy is final.

## 1. Name: `easel` is free

`scripts/easel-spikes/name-check.el` read the archive-contents files
downloaded on 2026-10-05:

| archive | packages | names containing "easel" |
|---|---|---|
| MELPA | 6331 | none |
| MELPA stable | 3488 | none |
| GNU ELPA | 508 | none |
| NonGNU ELPA | 291 | none |

`emacs -Q` 30.1 with chart, svg, image, xt-mouse, org, ox, eww, shr,
dom and json loaded interns no `easel*` symbol. On GitHub,
`zonuexe/easel.el` (last push 2016-08-07, 0 stars, not on any archive)
is the only prior art.

**Decision:** the name is `easel` with prefix `easel-`. The design doc
drops "provisional".

## 2. Re-raster latency: the Lisp half, measured

`scripts/easel-spikes/render-cost.el` (mean ms per call, batch):

| N marks | svg.el `svg-rectangle` build | direct DOM build | move crosshair + `svg-print` | one `<path>` of N vertices, `svg-print` |
|---|---|---|---|---|
| 100 | 0.35 | 0.18 | 1.66 | 0.04 |
| 1,000 | 5.64 | 1.48 | 16.97 | 0.14 |
| 10,000 | 428.81 | 18.35 | 126.33 | 2.44 |

The SVG for 10k separate rects is 726 KB. A `mapconcat`+`format`
serializer is slower than `svg-print` (218.8 ms at 10k), so it is not
an option.

**Decisions:**
- The SVG renderer builds the DOM by consing children directly and
  never calls `svg-rectangle`/`dom-append-child` per mark, because
  that cost grows as O(n²).
- Continuous marks (line, area) emit **one `<path>`** per series.
  Per-item hit-testing comes from the scene index, not from SVG
  elements.
- Discrete marks are capped. Compile runs LTTB, keeping datum
  back-refs, whenever a series has more points than pixel columns.
  That keeps a redraw at about 1k elements or fewer, which is about
  17 ms of Lisp before rasterization.

## 3. Hit-test cost

An x-sorted bisect is 7–8 µs per query at 100, 1k and 10k items
(1000 queries in 6.7–7.9 ms). The cost is flat and the call overhead
dominates.

**Decision:** pointer → datum always goes through the scene index
(bisect for series). It is never the redraw bottleneck.

## 4. `:map` hot spots: build cost only

Building a `:map` list of N rect areas with help-echo and pointer
takes 0.23 ms at 100, 3.07 ms at 1k and 25.1 ms at 10k.

**Decision:** `:map` is only for discrete marks, legend entries and
annotations, which are bounded by pixel columns as in section 2.
Continuous series hit-test by scale inversion.

## 5. Text frames (tmux, `emacs -nw`)

`scripts/easel-spikes/tty-mouse.sh` runs Emacs inside a detached tmux
session (TERM `tmux-256color`), captures the bytes Emacs writes and
injects SGR mouse reports:

- With `xterm-mouse-mode` on, Emacs 30.1 enables `?1000h` (click),
  `?1003h` (any-motion), `?1004h` (focus) and `?1006h` (SGR) tracking.
  Motion with no button held is therefore requested.
- Decoded from injected reports: `down-mouse-1`, `mouse-1`,
  `mouse-movement` with a button held, `mouse-movement` with no button
  (with `track-mouse` t), and `wheel-up`. `posn-col-row` is 0-based and
  window-relative (the menu-bar row is subtracted).
- A press, a motion with the button held, then a release at a new
  column decoded as `down-mouse-1`, `mouse-movement`, `mouse-1`. **No
  `drag-mouse-1`** event arrived. The runtime therefore builds drags
  itself from down, motion and up instead of relying on drag events.
- Full rewrite of a 100x30 propertized grid plus `(redisplay t)`:
  **47.3 ms**. Changing one column of 30 cells plus redisplay:
  **5.5 ms**. Batch insert of the same grid with no redisplay:
  **10.9 ms**.

**Decisions:** terminal hover (crosshair, readout) patches only the
changed cells, about 5.5 ms. A full re-render happens only on domain
changes (zoom, pan, push), and those are idle-coalesced. Drags are
synthesized from down, motion and up.

## Unmeasured (needs a GUI Emacs, librsvg, macOS or other terminals)

| item | why not measured here | what it gates |
|---|---|---|
| librsvg re-raster of 100/1k/10k-mark SVG after a one-element change, image-cache growth, `image-flush` | no X/NS build, no librsvg | `.2`: full re-render per pointer move vs idle-coalesced redraw with a header-line readout |
| `posn-object-x-y` event rate under `track-mouse`; whether `:scale`/HiDPI coordinates need dividing by the scale | no GUI frame | `.18`/`.19` GUI glue (`easel-glue` divides by the image `:scale` provisionally) |
| `:map` hover cost at 100/1k/10k areas; help-echo and pointer per area | no GUI frame | `.1`, `.5` |
| Whether librsvg in Emacs shows SVG `<title>` tooltips (assumed ignored) | no librsvg | `.1` (the renderer emits no `<title>`; tooltips go through help-echo) |
| xterm-mouse with real iTerm2 and kitty clients, and tmux forwarding real (not injected) mouse input | no macOS, no kitty, no attached client | `.8` |

Until the librsvg number exists, the runtime **defaults to
idle-coalesced GUI redraws** (one redraw per idle tick, latest state
wins) with the readout in the header line. That stays correct whether
re-rasterization turns out fast or slow, and per-move redraw can be
switched on once it is measured. Hypothesis to verify: hover feedback
under 50 ms at 10k points. The Lisp half (bisect plus one-path
serialize) is about 2.5 ms at 10k.

## 6. Runtime hover through the real engine (fc-qx1.19)

`scripts/easel-spikes/run-compiled.sh scripts/easel-spikes/dispatch-cost.el`
uses byte-compiled sources, as an installed package runs. Interpreted
`.el` runs 4–10x slower, and the first measurements below were taken that
way by mistake. Mean ms in batch at 800x400. "hover" is one
`easel-dispatch` of a pointermove that moves the crosshair: the reducer,
the hit-test, the incremental compile and the inspect it returns.

| spec | N | full compile | hover | SVG serialize | text compile+render 100x30 |
|---|---|---|---|---|---|
| rule per datum, opacity condition | 1k | 14.2 | 4.5 | 4.9 | 23.0 |
| | 10k | 101.2 | 38.7 | 5.6 | 131.2 |
| | 100k | 932.3 | 355.0 | 29.8 | 1042.7 |
| Vega-Lite idiom: rule layer filtered by the param | 1k | 7.3 | 3.7 | 5.9 | 18.0 |
| | 10k | 40.2 | 25.1 | 3.5 | 71.9 |
| | 100k | 338.9 | 215.0 | 3.3 | 392.7 |

How hover got there: a profile showed 35% of the time in GC and the
rest in per-row generic lookups. The fixes were a memoized
`easel-key`, precompiled scale functions, per-unit channel accessors,
a shared style plist, and a 64 MB `gc-cons-threshold` during compile.
That took a 10k compile from 139 to 98 ms. The bigger change is that
selection-only changes no longer recompile the scene:
`easel-compile-patch` rebuilds only the items whose selection
membership changed, or the one unit a param filters. A test asserts
that the patched scene equals a full compile after every event.

**Decisions:**
- Hover stays inside the 50 ms hypothesis on the Lisp side up to about
  10k rows. Above that, templates should decimate or aggregate before
  compile (LTTB already applies to line and area marks). Bringing 100k
  hover under budget belongs to `fc-qx1.9`, the performance bead.
- The SVG for a hover redraw serializes in 3–30 ms, because invisible
  items are skipped and series are single paths. Total GUI latency
  still depends on librsvg rasterization, which is unmeasured here
  (section "Unmeasured").

## 7. Geometry against Vega-Lite 6.4.1 (a stand-in, not bin/chart)

bin/chart is not installed on this box, so the conformance oracle has
not run, and supported.json marks every feature `"oracle":
"unverified"`. To catch geometry bugs before it runs,
`scripts/easel-spikes/vega-standin/run.sh` renders every gallery spec
with real Vega-Lite 6.4.1 + Vega 6 (node, no canvas). Both SVGs are
rasterized by resvg with DejaVu Sans and compared with pixelmatch
(threshold 0.1). This is a stand-in. Its ratios seed each spec's
`usermeta.easel.threshold` (ratio + 0.02), which must be re-measured
with `bin/chart diff`, whose metric may differ.

Result over 47 specs: median mismatch 0.028; 27 at 3% or less,
40 at 5% or less. The remaining specs:

| spec | mismatch | easel size | Vega size |
|---|---|---|---|
| composition-vconcat | 0.092 | 356x254 | 362x258 |
| scale-log | 0.076 | 380x347 | 384x347 |
| mark-tick | 0.065 | 340x82 | 341x84 |
| transform-filter-predicates | 0.058 | 340x347 | 347x349 |
| mark-point | 0.053 | 340x347 | 347x349 |
| param-interval | 0.053 | 340x347 | 347x349 |
| mark-circle | 0.053 | 340x347 | 399x351 |

Known causes of what remains: Vega pads the canvas for marks that
overhang the plot (points on the domain edge); size and opacity legends
are not drawn natively (mark-circle, encoding-opacity); log-axis and
vconcat label placement are a few pixels off.

What the stand-in found and fixed in compile, each one an assumption
replaced by a measurement:
- Vega-Lite 6 sizes continuous views 300x300, not 200x200.
- Vega's headless text width is floor(0.8 * fontSize * chars). Axis
  extents use that estimate, and the plot origin then matched to the
  pixel.
- autosize pads only the part of the top y label that overhangs the
  plot.
- `zero` is off for the dimension axis of bar, area and line marks.
- log axes tick every k*10^i and label only small mantissas (d3).
- quantitative color on rect marks uses the yellowgreenblue scheme;
  gradient legends are clamp(height, 64, 200) long with real gradients.
- a bar's continuous dimension is padded by continuousBandSize (5px).
- timeUnit ordinal axes keep labels horizontal and title the unit
  hyphenated, e.g. "date (year-month-date)".

## 8. Live data: the cost of one push frame (fc-qx1.7)

`scripts/easel-spikes/run-compiled.sh scripts/easel-spikes/push-cost.el`
uses byte-compiled sources. The spec is a line with a pointermove
crosshair (the filter idiom from section 6) at 800x400, holding N rows
in a full window. A frame is one `easel-dispatch` of a windowed push of
K new rows: append, trim, reduce and a full recompile. The data
changed, so the selection patch does not apply. Mean ms in batch:

| N | K | push frame | SVG serialize | text compile+render 100x30 |
|---|---|---|---|---|
| 1,000 | 1 | 8.0 | 5.1 | 17.7 |
| 1,000 | 50 | 7.0 | 4.2 | 15.9 |
| 10,000 | 1 | 41.5 | 2.4 | 63.3 |
| 10,000 | 50 | 39.4 | 2.4 | 61.5 |

K barely matters, so batching rows costs nothing. What costs is the
number of frames. A terminal frame also pays the full grid rewrite
plus redisplay from section 5 (47.3 ms). That puts a whole frame at
about 13 ms of Lisp in a GUI frame (librsvg not included) and 65 ms in
a terminal at 1k rows, and 44 ms and 110 ms at 10k.

**Decisions:**
- `x-easel.stream.max-fps` defaults to **5**. That is a 200 ms
  interval. The worst measured case is a 10k-row terminal chart at
  110 ms, so Emacs stays about 45% idle for input. A 1k-row GUI chart
  stays about 94% idle. At 10 fps the 10k terminal case would use the
  whole interval. Values above 30 are rejected.
- Pushes are queued and coalesced. A frame takes everything queued
  since the last frame, as one windowed push event, so the log replays
  exactly the frames that were drawn.
- Streams pause while a drag is in progress (brush or pan) and for
  `easel-stream-hover-hold` (2 s) after the last pointer event. Neither
  glue reports the pointer leaving reliably, and in a terminal, point
  is always over the chart, so the hold has to lapse. On pointerleave,
  or once the hold lapses, the queue catches up in one frame.
- The buffer redraw stays idle-coalesced, as section 5 decided. The
  cap limits recompiles, and the idle timer still collapses redraws.
  Re-measure once the GUI numbers (fc-qx1.23) exist. A GUI-only cap
  could then be higher.

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

## Unmeasured on the fc-qx1.14 box

This box had no GUI build, so the table below was left open. Section 8
(fc-qx1.23) measured every row that an X server can reach, on
**Linux/Xvfb**. The **Status** column says what is still open.

| item | status |
|---|---|
| librsvg re-raster of 100/1k/10k-mark SVG after a one-element change, image-cache growth, `image-flush` | Linux/Xvfb: measured (§8.1, §8.2). macOS/NS: open |
| `posn-object-x-y` event rate under `track-mouse`; `:scale` division | Linux/Xvfb: measured (§8.3, §8.5). True HiDPI (NS backing scale, GTK `GDK_SCALE`): open |
| `:map` hover cost at 100/1k/10k areas; help-echo and pointer per area | Linux/Xvfb: measured (§8.4) |
| whether librsvg in Emacs shows SVG `<title>` tooltips | Linux/Xvfb: it does not (§8.6). NS: open, but NS renders through the same librsvg |
| xterm-mouse through a tmux client | injected at the client's terminal (§8.7). Real iTerm2/kitty clients and a real mouse: open |

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
## 8. GUI frame on Linux/Xvfb (fc-qx1.23)

**Linux/Xvfb**: Debian trixie, 4 vCPU Intel Xeon (Skylake), Xvfb
21.1.16 at 1600x1000x24 with no window manager, and GNU Emacs 30.1
`emacs-lucid` (X11, Lucid, **Cairo**, librsvg 2.60) with DejaVu fonts.
`scripts/easel-spikes/gui/setup-linux.sh` installs all of this without
root. `run.sh SPIKE.el OUT` runs one spike against byte-compiled easel.
Raw outputs are `raster.el`, `hover.el`, `motion.el` and
`../tmux-forward.sh`.

This box is slower than the fc-qx1.14 box. The batch `dispatch-cost.el`
hover here is 12.0 ms at 1k and 91.0 ms at 10k, against 4.5 and 38.7 in
section 6, so it is about 2.5x slower. Divide the Lisp-side numbers below
by about 2.5 to compare them with sections 2 and 6. Rasterization was
not measured on the faster box. Xvfb rasterizes in software, which a
real X server does too, since Emacs rasterizes SVG on the CPU through
librsvg and Cairo.

### 8.1 librsvg re-raster (`raster.el`)

These are the ms to show a new 800x400 image: `create-image`, swap the
`display` property, `(redisplay t)`. Each image is new, so each one
rasterizes. A forced redisplay of an image already in the cache takes
**0.8–1.9 ms**, so nearly all of the cost below is librsvg.

| SVG content | raster + redisplay, mean (p95) |
|---|---|
| empty `<svg>` | 12.6 (24.1) |
| 30 `<text>` labels | 21.2 (29.8) |
| one `<path>`, 800 vertices | 15.2 (21.2) |
| 100 `<rect>` | 15.6 (29.9) |
| 1,000 `<rect>` | 74.2 (105.5) |
| 10,000 `<rect>` | 809 (1170) |
| 100 bars + crosshair, crosshair moved | 14.4 (18.5) |
| 1,000 bars + crosshair, crosshair moved | 90.7 (169.0) |
| 10,000 bars + crosshair, crosshair moved | 613.5 (700.3) |

The cost is a fixed ~11 ms plus ~0.065 ms per discrete element. A
single path is nearly free. The whole SVG is rasterized again whichever
element changed.

### 8.2 Image cache (`raster.el`)

Every 800x400 image adds **1.28 MB** to `image-cache-size`. After 100
distinct images it held 129 MB, after 200 it held 257 MB (152 MB after
300, after a partial eviction), and RSS grew by up to 274 MB.
`clear-image-cache` returned it. When the replaced image is passed to
`image-flush` on each step, the cache stays at **2.56 MB** (two images)
and RSS grows by 0.4 MB over 300 steps.

**Fixed:** `easel-mode-redraw` now calls `image-flush` on the image it
replaces. Before this, every hover leaked one image until eviction.

### 8.3 Pointer coordinates and `:scale` (`hover.el`)

The pointer was warped to known offsets inside the image:

- `posn-object-x-y` is in **display** pixels: at `:scale 2` a warp to
  display (150, 75) reads (150, 75). Dividing by a numeric `:scale`
  gives scene pixels.
- The C hit test reads `:map` in **display** pixels: at `:scale 2`, area
  (100,50)-(200,100) was hit at display (150, 75).
- **Bug, fixed:** when `create-image` is given no `:scale`, it adds
  `:scale default`. `easel-mode-redraw` passed none, and the glue
  computed `(float (or :scale 1))`, which signalled
  `wrong-type-argument` on **every** GUI pointer event, so GUI hover
  never worked. `easel-svg-image` now pins `:scale` (default 1, because
  the scene is compiled at window pixels), scales `:map` to display
  pixels and passes `:original-map`. `easel-mode-event-px` divides only
  by a numeric scale. `easel-glue-test.el` covers all three, and those
  tests fail on the old code.
- **Bug, fixed:** `create-image` given `:map` without `:original-map`
  calls `image-size` twice (`image--compute-original-map`), which
  rasterizes the SVG twice more on every redraw. Before the fix, a
  redraw of 100 points (SVG, `:map`, insert) took 125.7 ms; after it,
  47.8 ms. At 1k
  points it went from 888.6 to 247.0 ms, and the 1k redisplay went from
  902.7 to 614.4 ms.
- Xvfb setup only: until an X frame is focused and has received one
  button press, Emacs reports **no** `mouse-movement`, from warps or
  from XTEST motion, even with `track-mouse`. Clicks and keys do
  arrive. The harness focuses the frame and clicks once in `*scratch*`.
  A desktop with a window manager focuses frames itself.
- Not measured: true HiDPI. Neither Xvfb nor Lucid has a device scale.
  NS (backing scale factor) and GTK (`GDK_SCALE`) remain open. The glue
  now asks only for a numeric `:scale`, which it sets itself.

### 8.4 `:map` hover cost (`hover.el`)

This is the time from a pointer warp to the `mouse-movement` that Lisp
reads. The C side looks up the hot spot and delivers help-echo in that
interval. 200 warps per row:

| `:map` areas | mean ms | p95 ms | help-echo deliveries |
|---|---|---|---|
| 0 | 3.38 | 5.66 | 0 |
| 100 | 3.35 | 5.05 | 188 |
| 1,000 | 3.37 | 7.31 | 194 |
| 10,000 | 4.13 | 9.23 | 198 |

The lookup is linear but cheap: 10k areas add under 1 ms. What `:map`
really costs is building it in Lisp (section 4) and the extra
rasterization `create-image` did before the fix (8.3).

### 8.5 Real motion through the engine (`motion.el`, xdotool)

This used a real `easel-view-mode` buffer: the shipped keymap, glue and
reducer, with a crosshair (filter idiom) on an 800x400 line chart.
xdotool, as a separate X client, sent 150 moves across the plot, either
at 125 Hz (8 ms apart, a typical mouse) or as a flood. Each redraw was
followed by `(redisplay t)` so that rasterization falls inside the timing.
"Lag" is the time from xdotool's last move to the end of the frame that
shows it. Results at 125 Hz:

| `gc-cons-threshold` | rows | redraw | events handled | redraws during motion | handler mean ms | redraw+raster mean ms | lag ms |
|---|---|---|---|---|---|---|---|
| 800 KB (default) | 1k | idle | 14 | 12 | 24.5 | 76.0 | 200 |
| 800 KB | 1k | per-move | 14 | 11 | 116.5 | 90.2 | 229 |
| 800 KB | 10k | idle | 6 | 3 | 191.0 | 109.7 | 409 |
| 800 KB | 10k | per-move | 7 | 4 | 256.0 | 87.2 | 416 |
| 64 MB | 1k | idle | 29 | 27 | 8.5 | 34.8 | **21** |
| 64 MB | 1k | per-move | 30 | 27 | 44.4 | 33.8 | **45** |
| 64 MB | 10k | idle | 13 | 10 | 70.3 | 46.8 | 202 |
| 64 MB | 10k | per-move | 13 | 10 | 113.1 | 50.9 | 138 |

A flood (all 150 moves within 39–101 ms) was handled as 2–3 events in
every configuration. The final position was always reached, with lag
from 33 to 50 ms at 1k/64 MB and from 141 to 286 ms elsewhere.

What this shows:
- **Emacs already coalesces pointer motion.** X motion is not queued.
  While Lisp is busy, the next `mouse-movement` carries the latest
  position, so a slow redraw lowers the update rate (about 10 Hz at the
  default GC threshold, about 22 Hz at 64 MB) but never builds a
  backlog. The idle timer fires between events during motion, so idle
  and per-move redraw draw almost the same frames.
- **GC is the biggest single cost.** At the default threshold a 1k
  hover collects 2.5–3 times per move, which is 80–97 ms of a 123–141 ms
  move (`raster.el`, 15 moves). Every collection traces the whole heap,
  including the view's rows and scene. At 64 MB the same move is
  **41.7 ms (p95 50.8)**: dispatch 5.6, redraw 7.1, raster 29.0. A
  10k-row move is 116.5 ms: dispatch 48, raster 45.

### 8.6 SVG `<title>` (`hover.el`)

The same 20-bar SVG with a `<title>` in every bar was hovered for
0.6 s on each of 5 bars, with `tooltip-mode` on and a 0.1 s delay:

- without `:map`, `x-show-tip` was called 0 times and help-echo was
  delivered 0 times. librsvg does not draw the title text either: see
  `title-map-nil.png` in the run artifacts.
- with a `:map` (positive control), the area help-echo tooltips showed
  ("area 14", "area 15").

**Confirmed:** Emacs ignores `<title>`. Tooltips go through `:map`
help-echo, and the renderer keeps emitting no `<title>`.

### 8.7 Mouse through a tmux client (`tmux-forward.sh`)

`tty-mouse.sh` injected reports into Emacs's own pane. This test runs
Emacs in an inner tmux server, attaches an inner tmux **client** from a
pane of an outer tmux (standing in for the user's terminal) and injects
SGR reports as that client's terminal input. The inner server therefore
has to parse them and re-encode them for Emacs. With the inner
`mouse on`, and again with `mouse off`, Emacs decoded exactly what it
decoded in section 5: `down-mouse-1`, `mouse-1`, `mouse-movement` with a
button held, `mouse-movement` with no button, `wheel-up`. Positions
were the same, (9 . 3) and (19 . 3). The inner server turned on
`?1000h ?1002h ?1003h ?1006h` on the client's terminal in both modes,
because when tmux's own mouse handling is off it forwards the modes the
pane asks for. Drags again arrived as down/motion/up with no
`drag-mouse-1`.

### 8.8 Decisions

- **The redraw default stays idle-coalesced. It is not flipped.**
  Per-move redraw misses the 50 ms budget at Emacs's default GC
  threshold: 123–141 ms per move at 1k rows on this box. Even if all of
  it scaled down with the CPU, that would be 50–56 ms on the fc-qx1.14
  box. At 10k rows it misses at any threshold.
  The two strategies also draw almost the same frames, because Emacs
  coalesces motion itself (8.5). Idle-coalescing is never worse and
  gives up only one idle tick.
- The budget is reachable at 1k rows once GC is controlled (21–45 ms
  lag). That work belongs to `fc-qx1.9`, which should bind a large
  `gc-cons-threshold` around dispatch+redraw and collect on idle, in the
  style of gcmh. Setting it globally is the user's choice, not a
  package's.
- In a GUI frame, discrete marks cost ~0.065 ms each to rasterize
  (100 → 15 ms, 1k → 74 ms). For hover inside the budget, keep a redraw
  at about 300 discrete elements or fewer. Section 2 set the cap at
  ~1k. Series stay single paths, which are nearly free.
- The glue fixes found here (pinned `:scale`, `:map` in display pixels
  with `:original-map`, `image-flush` of the replaced image) are in
  `easel-svg.el` and `easel-mode.el`.

### 8.9 Still open (needs macOS/NS or real terminals)

- NS (macOS) Emacs: raster and redisplay cost through the NS port,
  coordinates under a Retina backing scale, and whether the NS port also
  drops motion until the first click.
- A real HiDPI X or GTK setup (`GDK_SCALE=2`, a pgtk build).
- Real iTerm2 and kitty clients with a physical mouse, including the
  report rate and SGR-pixel (1016) support, and tmux forwarding from
  them. Here tmux forwarding was checked only with injected input.
- A recorded human hover session for the perceived update rate.

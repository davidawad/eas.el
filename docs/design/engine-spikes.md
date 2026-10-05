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

Superseded by section 8: against the real bin/chart this stand-in's
gallery passed 0 of 47, because its defaults were not bin/chart's.

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

## 9. Terminal parity through a real terminal (fc-qx1.8)

`scripts/easel-spikes/tty-parity.sh` opens a point chart (brush, click
selection, legend) in `emacs -nw` inside tmux (TERM `tmux-256color`).
It injects SGR mouse reports at the cells of real data and then types
keys. Showing the chart turned on `xterm-mouse-mode` by itself
(`easel-tty-xterm-mouse`). Motion with no button, a press, motion with
the button held, and a release reached the reducer as pointermove,
pointerdown, pointermove and pointerup, and the drag brushed. `z`, `[`,
`n`, RET and a wheel report followed. The recorded log, replayed in
lockstep on a fresh text view and a fresh SVG view
(`easel-parity-replay`), gave 9 steps with 0 mismatches, and the fresh
text view's state equalled the live one.

What the run and the parity tests found and fixed in the text glue:
- A text cell is 7x14 px, wider than the 3 px click slop, so a click
  at a cell's centre could miss the point drawn in it. Press, release,
  click and RET now snap to the datum the cell shows (allowing one cell
  of slack, because the renderer pulls edge glyphs inward). Hover does
  not snap: what a cell shows changes as the crosshair redraws, and
  snapping made the hover oscillate between the line and the rule.
- Point-as-pointer re-hovered after every command, which broke
  xterm-mouse drags and undid mouse hovers. Hover now follows point only
  when point moves to another cell.
- A redraw restored point by buffer position. Line lengths change with
  the axis labels, so after a zoom point landed on another cell and the
  hover jumped. Point now keeps its line and column.

Pixel logs are geometry-specific, because the text target snaps the
layout to cells. Parity is therefore defined through data space:
`easel-parity-translate` maps each event's pixels from one scene to the
other through the scales (legend entries map to the same entry), and
`easel-parity-state` compares domains, selections, the hovered and
clicked datum, history depth and drag mode, within a relative 1e-9.
## 8. Geometry against bin/chart's references (fc-qx1.21)

The oracle compares native SVG, rasterized by rsvg-convert, with the
PNGs bin/chart built for every gallery spec. Those PNGs are committed
in test/conformance/ref with a manifest of spec and PNG hashes and the
zone they were built in, so the oracle runs wherever rsvg-convert does.
`bin/chart diff` scores any canvas size difference as 1.0, so images
are compared in Elisp (easel-png.el). Both are padded onto their union
canvas and aligned by ink profiles, then by a local search. The size
delta is reported apart from the differing-pixel ratio and bounded at
8px. A pixel differs at pixelmatch's YIQ threshold 0.1.

Result: 47 of 47 within their unchanged thresholds, every canvas the
reference's size to the pixel. Worst ratios: encoding-bin 0.017/0.05,
composition-vconcat 0.016/0.12, mark-rect 0.013/0.05; median 0.005.
These were measured on Linux, where rsvg-convert draws Arial as
Liberation Sans. The remainder is glyph rasterization. vl-convert with
Liberation Sans in the same zone redraws the references at 0.2–1.0%,
also at the same sizes.

What it took, each one a measurement of bin/chart (read off its
scenegraph via vl-convert and Vega's source):
- The theme is bin/chart's (`chart theme --json`, vendored with its
  hash). It gives a 480x300 continuous view, 11/12px axis fonts, tick 4,
  label padding 4, no x grid, no view stroke and 4px bar end radii.
- Text is measured with Arial's advance widths (vl-convert resolves
  sans-serif to Arial), not Vega's headless 0.8em guess. The SVG names
  Arial (then Liberation Sans) for the generic family.
- Canvas extents follow Vega's autosize "pad": ceil of the union of axis
  bounds (ticks with their 1px stroke, visible labels, titles), legend
  boxes and mark bounds (symbols sqrt(size)/2, strokes their full width).
  Clipped views count no marks: zoomed views, and any view with a param
  bound to scales.
- Text bounds use vega-scenegraph's baseline offsets (top 0.79em,
  middle 0.30em, bottom -0.21em, rounded).
- Axis lines sit on the half pixel. Band axes are offset -0.5
  (axisBand.tickOffset) with ticks rounded and labels not.
- Overlap removal is Vega's: parity or greedy (log), and the last label
  is restored when fewer than three survive. Nominal axes are never
  thinned.
- Legends follow Vega's layout. An entry is max(ceil(sqrt(size) +
  strokeWidth), labelFontSize) wide. Rows are separated by their bounds
  plus rowPadding 2, entries start titlePadding 5 below the title, and
  legends sit 18px right of the plot, or of faceted series marks
  overhanging it. Gradients get max(2, 2*floor(length/100)) labels.
  Size and opacity legends are drawn.
- Marks: stacks round only their end (cornerRadiusEnd) and ranged bars
  all corners; binned bars keep a 1px gap; ticks span the band (paddings
  0.25/0.125) or 5px; aggregate titles are titleCase(op) of field.
- The references were built in America/Chicago, and Vega draws "time"
  scales and timeUnits in local time. A UTC date-only string reads as
  1 March there, not 2 March. `easel-time-zone` (nil = UTC, the
  default) gives native compile Vega's local-time semantics. The
  manifest records the zone, and the oracle compiles SVG in it.

Cost: the oracle adds about 12 s to `make test` for 47 specs. Most of
that is decoding PNGs in Elisp (about 80 ms per 600x350 image).
Comparison skips identical runs with `compare-strings` and is about
20 ms.
## 9. The crosshair as shipped (fc-qx1.2)

Same box as the fc-qx1.14 sections (4 vCPU EPYC-Rome, Emacs 30.1, no
X), byte-compiled. The `line` template's `"crosshair": true` slot adds
Vega-Lite's idiom: a point selection `{on: pointermove, nearest: true,
encodings: ["x"]}` and a rule and point layer filtered by it.

Which layer holds the param is a measured choice. Vega-Lite has no
`nearest` for line marks, so the param can't sit on the line. Mean
dispatch ms per pointermove at 10k rows and 800x400, with each variant
adding layers to the same line:

| param on | other layers | before parse cache | after |
|---|---|---|---|
| line (not valid Vega-Lite) | filtered rule | 90.7 | 23.9 |
| point per datum, opacity condition | filtered rule | 215.0 | 72.8 |
| rule per datum, opacity condition | none | n/a | 38.3 |
| invisible rule per datum (**shipped**) | filtered rule + point | 166.3 | 51.8 |

- **Points hit-test linearly, rules by bisect.** A point mark's grid
  index scans every item when `nearest` measures only x. Rules are
  x-sorted, so they bisect. The param therefore sits on an invisible
  rule layer (`crosshair-hit`).
- **Parsing dates was most of the cost.** Each move re-tests every
  row's date string against the selection (the filter, and conditions),
  and parsed each string every time: 37% of a 10k move. `easel-time-parse`
  now memoizes strings in a bounded table (200k entries, then cleared).

Terminal redraw (`scripts/easel-spikes/crosshair-cost.el`, 100x30
cells, one pointermove one cell to the right plus one redraw, batch
and so without redisplay):

| rows | redraw | ms/move | cells written/move |
|---|---|---|---|
| 1k | patch (`easel-mode-patch-text`) | 29.2 | 51 |
| 1k | full rewrite (before) | 23.5 | 2,888 |
| 10k | patch | 106.7 | 51 |
| 10k | full rewrite | 94.0 | 2,875 |

Breakdown of a 1k move: dispatch 4.7, text render 18.8, patch 3.7,
inspect for the header readout 0.7. At 10k: 45.0, 50.5, 5.0 and 7.3.

**Decisions:**
- Terminal hover patches only changed cells, as section 5 decided. The
  diff costs about 5 ms of Lisp per move. That buys back the redisplay
  difference section 5 measured: 47.3 ms for a full grid against 5.5 ms
  for one column.
- GUI redraws stay idle-coalesced (8.8). The header-line readout
  updates on each move, before the redraw, and lists every field in
  `encoding.tooltip` (`easel-crosshair-readout`).
- The 10k text move is now dominated by rendering the whole grid
  (50 ms). Rendering only the units that changed belongs to `fc-qx1.9`.

## 10. Performance budget (fc-qx1.9)

Box: the fc-qx1.14 box class (Linux 6.8, 4 vCPU AMD EPYC-Rome, GNU
Emacs 30.1 `--without-x`). The ladder ran byte-compiled in batch, so
these are Lisp-side numbers. librsvg and redisplay are not included
(see section 8 for those).

### 10.1 The ladder: `bench` with no SOURCE

`bin/easel bench` (Lisp: `(easel-agent "bench")`, `easel-bench.el`)
measures two fixed workloads at 1k, 10k and 100k points and reports
JSON: `easel-bench/v1` with one rung per size, and each stage as
`{mean, max, reps}` in ms. The workloads are an 800x400 line with the
Vega-Lite crosshair idiom (a rule layer filtered by a nearest
pointermove point selection), the same chart as text at 100x30, and a
scatter of the same rows. `make bench` runs it against byte-compiled
copies and checks it with `--budget`. Mean ms from `make bench`, GC
deferred as the glue runs it (10.4):

| stage | 1k | 10k | 100k |
|---|---|---|---|
| compile-svg (whole scene) | 4.4 | 31.5 | 357 |
| render-svg (scene -> SVG string) | 3.1 | 3.1 | 3.1 |
| compile-text | 3.7 | 30.3 | 357 |
| render-text (scene -> grid) | 7.7 | 14.8 | 22.6 |
| hover-first (first pointermove, builds indexes) | 1.2 | 7.7 | 82 |
| **hover** (pointermove: reduce, hit-test, patch, inspect) | **0.26** | **0.24** | **0.26** |
| hover-svg (hover + SVG redraw, before librsvg) | 3.7 | 3.7 | 3.4 |
| hover-text (hover + text redraw) | 8.5 | 16.0 | 24.9 |
| hit-line (one easel-hit, x-sorted index) | 0.007 | 0.007 | 0.007 |
| compile-points (scatter scene) | 3.9 | 38 | 379 |
| hit-points (one easel-hit, grid index) | 0.030 | 0.10 | 0.92 |
| lttb (N points -> 800) | 0.70 | 3.2 | 26.0 |

The hypothesis from fc-qx1.14, hover feedback under 50 ms at 10k
points, holds on the Lisp side: 0.24 ms for the hover and 3.7 ms with
the SVG redraw. Adding the section 8.1 raster cost for one path plus a
few labels (about 15–21 ms on the slower Xvfb box) gives an estimated
20–25 ms per GUI move at 10k. That estimate has not been measured end
to end in a GUI frame. hover-first at 100k (82 ms) is paid once per
view, by the first move.

### 10.2 What made hover flat

Before, measured on this box with `scripts/easel-spikes/dispatch-cost.el`
(section 6's script), then after:

| spec | N | hover before | hover after |
|---|---|---|---|
| rule per datum, opacity condition | 1k / 10k / 100k | 4.6 / 39.0 / 348.9 | 0.3 / 0.3 / 0.4 |
| Vega-Lite idiom: rule layer filtered by the param | 1k / 10k / 100k | 2.4 / 24.4 / 219.0 | 0.3 / 0.2 / 0.3 |

A profile of the 100k filter idiom put 77% of a hover in the
`{"param": "hover"}` filter, which tested every row against the store,
and 22% in `inspect`, which re-summarised every visible row.

- `easel-params-index.el` answers a bare `{"param": NAME}` filter on a
  point selection from a hash of each row's tuple over the store's
  fields. The hash is built once per rows vector and held in a weak
  table. Keys normalise values the way `easel-params--same` compares
  them (numbers as floats, date strings as epoch ms). The same index
  bounds which rows `easel-compile-patch` re-tests for conditional
  encodings. Interval stores, and integers past 2^53, fall back to the
  row-by-row test. ERT checks the index against that test over mixed
  numbers, floats, -0.0, dates and strings, and checks that patched
  scenes equal full compiles.
- `easel-view--visible-summary` caches its summary per mark rows
  vector and x domain, so a hover reuses it and a zoom recomputes it.

### 10.3 Hit-testing and LTTB

The grid index for point marks was built at compile time and then
never read: `easel-hit-mark` scanned every item and allocated a
candidate per item. One query cost **1.88 ms at 1k, 13.5 ms at 10k and
113.6 ms at 100k**. `easel-hit--grid` now searches square rings of
cells outwards (columns only for x-only), stopping once no unvisited
cell can hold anything nearer. It returns exactly what the scan
returns, ties included (ERT compares the two over 100 pointers in both
modes), and costs 0.03 / 0.10 / 0.92 ms. Items with a width keep the
scan, because a box's distance can undercut its cell's bound. The
x-sorted bisect stays at 7 µs, matching section 3.

LTTB costs about 0.26 µs per input point. Compile runs it only when a
series has more points than pixel columns, so it is part of compile at
large N, not of hover.

Text render grows with N on this workload because the drawing does:
at 100k the sine swings about 430 times across 100 columns, so each of
the 192 decimated segments is a full-height braille line. Looking up a
dot's text properties once per braille column instead of once per dot
cut render-text from 22.1 to 14.8 ms at 10k and from 40.9 to 22.6 ms at
100k. Goldens are unchanged.

### 10.4 GC: deferred until idle

Section 8.5 found GC to be the largest single GUI cost at the default
threshold. Binding the threshold around the handler alone does not
fix that, because when the binding unwinds the collection simply runs
right after the handler. `easel-gc.el` therefore works like gcmh. The
first chart event raises `gc-cons-threshold` to
`easel-gc-cons-threshold` (64 MB). After `easel-gc-idle-delay` (1 s)
of idle time it collects once and puts the user's value back. It never
lowers a larger value and leaves alone a value someone else changed in
the meantime. Setting `easel-gc-cons-threshold` to nil turns it off.
Batch runs are left alone. The raise lasts only while a chart is in
use, so the design rule that a permanent global threshold is the
user's choice still holds.

In batch the effect is small, because the heap is small: hover-svg at
1k is 4.1 ms mean (max 5.0) deferred against 5.7 (max 16.6) at the
default 800 KB (`bench --gc default`). A long-running GUI session
traces a much larger heap on every collection, which is where section
8.5 measured 80–97 ms of GC per move. The end-to-end gain in a GUI
frame is **unmeasured** on this box, which has no display. Re-run
`scripts/easel-spikes/gui/motion.el` to measure it.

### 10.5 The regression budget in CI

`src/easel/bench-budget.json` holds this box's reference means, a
`tolerance` (2.5), a `floor-ms` (2) and the machine's calibration time
(best of five runs of a fixed Lisp workload, 34.7 ms here). A stage
fails when its mean is above
`max(reference x tolerance, floor) x factor`, where `factor` is this
run's calibration over the reference's, clamped to [0.5, 8]. A slower
runner therefore gets proportionally looser limits. Results are
compared only when both are byte-compiled or both interpreted; a
skipped comparison fails `make bench`. A failure is
`BUDGET_EXCEEDED`, which names the stage and size in its evidence.
`make bench-budget` re-measures the references.
Targets are reported, not enforced: `hover` at 10k under 50 ms
(`met: true`, 0.24 ms).

Checked by hand: an `EMACS` wrapper that switched the index off (the
pre-fc-qx1.9 hover path) made `make bench` exit 1 with "hover at 10000
points 16.0 ms > 1.9 ms; hover-svg at 10000 points 19.4 ms > 8.3 ms".
CI runs `make bench` on the Emacs 30.1 job and uploads the JSON.

**Decisions:**
- Hover cost no longer depends on N for point selections. The 50 ms
  budget is spent on rasterization (GUI) or the text redraw
  (terminal), not on the engine.
- The idle-coalesced redraw from section 8.8 stays. With hover at
  0.3 ms, per-move redraw is worth re-measuring on a GUI box against
  `motion.el`.
- Still open: hover-first at 100k (82 ms) could be built at open or on
  idle; brushing (interval stores) still tests every row; compile at
  100k (357 ms) is linear and is paid on zoom, pan and push; the text
  redraw at 100k (25 ms) could patch only changed cells (section 5).

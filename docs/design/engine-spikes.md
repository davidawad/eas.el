# eas spikes (fc-qx1.14)

Measured numbers, and the decision each one forces. The scripts that
produced them are in `scripts/eas-spikes/` and can be rerun.

Box: Linux 6.8, 4 vCPU AMD EPYC-Rome, GNU Emacs **30.1** (not 30.2). The
build is `--without-x`, and `image-type-available-p 'svg` is nil.
tmux 3.x is present. There is no GUI frame, no librsvg, no macOS and no
iTerm2/kitty. Every number below that needs one of those is listed
under **Unmeasured** and has to be taken on a GUI machine before the
`.2` crosshair redraw strategy is final.

## 1. Name: `eas` is free

`scripts/eas-spikes/name-check.el` read the archive-contents files
downloaded on 2026-10-05:

| archive | packages | names containing "eas" |
|---|---|---|
| MELPA | 6331 | none |
| MELPA stable | 3488 | none |
| GNU ELPA | 508 | none |
| NonGNU ELPA | 291 | none |

`emacs -Q` 30.1 with chart, svg, image, xt-mouse, org, ox, eww, shr,
dom and json loaded interns no `eas*` symbol. On GitHub,
`zonuexe/eas.el` (last push 2016-08-07, 0 stars, not on any archive)
is the only prior art.

**Decision:** the name is `eas` with prefix `eas-`. The design doc
drops "provisional".

## 2. Re-raster latency: the Lisp half, measured

`scripts/eas-spikes/render-cost.el` (mean ms per call, batch):

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

`scripts/eas-spikes/tty-mouse.sh` runs Emacs inside a detached tmux
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

`scripts/eas-spikes/run-compiled.sh scripts/eas-spikes/dispatch-cost.el`
uses byte-compiled sources, as an installed package runs. Interpreted
`.el` runs 4–10x slower, and the first measurements below were taken that
way by mistake. Mean ms in batch at 800x400. "hover" is one
`eas-dispatch` of a pointermove that moves the crosshair: the reducer,
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
`eas-key`, precompiled scale functions, per-unit channel accessors,
a shared style plist, and a 64 MB `gc-cons-threshold` during compile.
That took a 10k compile from 139 to 98 ms. The bigger change is that
selection-only changes no longer recompile the scene:
`eas-compile-patch` rebuilds only the items whose selection
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
`scripts/eas-spikes/vega-standin/run.sh` renders every gallery spec
with real Vega-Lite 6.4.1 + Vega 6 (node, no canvas). Both SVGs are
rasterized by resvg with DejaVu Sans and compared with pixelmatch
(threshold 0.1). This is a stand-in. Its ratios seed each spec's
`usermeta.eas.threshold` (ratio + 0.02), which must be re-measured
with `bin/chart diff`, whose metric may differ.

Result over 47 specs: median mismatch 0.028; 27 at 3% or less,
40 at 5% or less. The remaining specs:

| spec | mismatch | eas size | Vega size |
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

`scripts/eas-spikes/run-compiled.sh scripts/eas-spikes/push-cost.el`
uses byte-compiled sources. The spec is a line with a pointermove
crosshair (the filter idiom from section 6) at 800x400, holding N rows
in a full window. A frame is one `eas-dispatch` of a windowed push of
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
- `x-eas.stream.max-fps` defaults to **5**. That is a 200 ms
  interval. The worst measured case is a 10k-row terminal chart at
  110 ms, so Emacs stays about 45% idle for input. A 1k-row GUI chart
  stays about 94% idle. At 10 fps the 10k terminal case would use the
  whole interval. Values above 30 are rejected.
- Pushes are queued and coalesced. A frame takes everything queued
  since the last frame, as one windowed push event, so the log replays
  exactly the frames that were drawn.
- Streams pause while a drag is in progress (brush or pan) and for
  `eas-stream-hover-hold` (2 s) after the last pointer event. Neither
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
`scripts/eas-spikes/gui/setup-linux.sh` installs all of this without
root. `run.sh SPIKE.el OUT` runs one spike against byte-compiled eas.
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

**Fixed:** `eas-mode-redraw` now calls `image-flush` on the image it
replaces. Before this, every hover leaked one image until eviction.

### 8.3 Pointer coordinates and `:scale` (`hover.el`)

The pointer was warped to known offsets inside the image:

- `posn-object-x-y` is in **display** pixels: at `:scale 2` a warp to
  display (150, 75) reads (150, 75). Dividing by a numeric `:scale`
  gives scene pixels.
- The C hit test reads `:map` in **display** pixels: at `:scale 2`, area
  (100,50)-(200,100) was hit at display (150, 75).
- **Bug, fixed:** when `create-image` is given no `:scale`, it adds
  `:scale default`. `eas-mode-redraw` passed none, and the glue
  computed `(float (or :scale 1))`, which signalled
  `wrong-type-argument` on **every** GUI pointer event, so GUI hover
  never worked. `eas-svg-image` now pins `:scale` (default 1, because
  the scene is compiled at window pixels), scales `:map` to display
  pixels and passes `:original-map`. `eas-mode-event-px` divides only
  by a numeric scale. `eas-glue-test.el` covers all three, and those
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

This used a real `eas-view-mode` buffer: the shipped keymap, glue and
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
  `eas-svg.el` and `eas-mode.el`.

### 8.9 Still open (needs macOS/NS or real terminals; the macOS items are answered in 8.10-8.13)

- NS (macOS) Emacs: raster and redisplay cost through the NS port,
  coordinates under a Retina backing scale, and whether the NS port also
  drops motion until the first click.
- A real HiDPI X or GTK setup (`GDK_SCALE=2`, a pgtk build).
- Real iTerm2 and kitty clients with a physical mouse, including the
  report rate and SGR-pixel (1016) support, and tmux forwarding from
  them. Here tmux forwarding was checked only with injected input.
- A recorded human hover session for the perceived update rate.

## 8. GUI frame on macOS/NS and through real terminals (fc-qx1.24)

Leftovers of section 8.9. **macOS**: GNU Emacs 30.2 emacs-plus (NS
build, appkit 2685, librsvg), Apple M5 Max, macOS 26.5.1, a 1000x640
chart window (a 1020x704 frame), `frame-scale-factor` **2.0**,
`image-scaling-factor` `auto`. The laptop was in use: its load average
was 17-20 during every run, so read the milliseconds as upper bounds.
Scripts (all byte-compile eas first and run niced with a timeout):
`scripts/eas-spikes/gui/run-ns.sh ns-raster.el|ns-scale.el OUT` (GUI)
and `scripts/eas-spikes/tty-marks.sh` (terminal, private tmux server
`-L eas-spike`). Raw outputs of this run are in
`scripts/eas-spikes/out/`: `ns-raster.out`, `ns-scale.out`,
`tty-marks.out`, `tty-marks-iterm.out`.

### 8.10 NS re-raster and the image cache (`ns-raster.el`)

Each frame is the real pipeline: change (resize, push or pointermove),
`eas-mode-redraw` (SVG + `create-image` + insert), `(redisplay t)`. A
forced redisplay of an image already in the cache is **1.4-1.7 ms**
(`hit-redisplay`), so the rest is librsvg plus the NS draw. Image size
is 1000x640 pixels, equal to the window in points, whatever the backing
scale (8.11). Mean ms per frame (12-20 frames after 2 warm-up frames;
the frames are all distinct unless the row says otherwise). "Old code"
flushes the replaced image every frame, as before this section; "no
flush" never flushes:

| view, change | shipped now: total (raster) | render cache off: total | old code: total | no flush: total |
|---|---|---|---|---|
| line, resize | 110.9 (92.6) | 93.7 | 100.0 | 85.3 |
| treemap, resize | 143.7 (111.4) | 176.6 | 175.4 | 179.5 |
| airport-connections, resize | 335.8 (140.8; compile ~156) | 324.5 | 321.5 | 279.1 |
| order-book (2x25), resize | 81.8 (59.6) | 111.2 | 78.3 | 79.9 |
| order-book (2x25), keyed push | 125.4 (103.6) | 110.4 | 120.2 | 110.6 |
| airport-connections, hover (15 distinct frames) | 128.6 (118.6) | 197.2 | 147.7 | 136.7 |
| scatter-plot, hover (hover changes nothing: 1 distinct SVG) | **17.0 (2.9)** | 32.9 | **634.1 (622)** | 25.0 |
| airport-connections, pointer parked (1 distinct SVG) | **8.3 (1.8)** | 50.8 | **98.5 (91)** | 7.8 |

What this shows:

- **A new SVG costs 60-140 ms to rasterize and draw on NS** at 1000x640
  (60-104 ms for the 50-bar ladder, 93 for the line, 111-141 for the
  treemap and the airport map). That is the same order as Linux
  (section 8.1: 75 ms for 1k rects at 800x400), not better. A frame
  whose SVG is new cannot meet a 50 ms budget at any of these sizes,
  with or without a cache, so section 8.8 stands: **the redraw default
  stays idle-coalesced**.
- **The Emacs image cache must be flushed**, as 8.2 found. Without
  `image-flush` it held **115.5 MB after 14 frames** (resize), 146 MB
  after 17 (hover) and 195 MB after 22 (pushes): about 8 MB per frame,
  with RSS +207 to +362 MB. With it the cache is a constant **9.75 MB**
  (two images) and RSS does not move. The cache earns nothing for a
  changing chart: the no-flush totals equal the flushed ones within noise.
- **Bug, fixed:** `eas-mode-redraw` flushed the image it replaced even
  when the new image had the same `:data`. The two share one cache
  entry, so every unchanged frame rasterized again: a hover that
  changes nothing cost **634 ms** on scatter-plot (622 ms of it in
  `redisplay`) and a parked pointer **98.5 ms** on the airport map.
  `eas-mode--flush-replaced` now flushes only when the data differs:
  **17.0 and 8.3 ms**, and the cache stays flat (9.75 MB, RSS +0).
- The eas render cache (`eas-render-cache-enabled`) is worth keeping
  on: hover on the airport map 128.6 vs 197.2 ms, parked 8.3 vs
  50.8 ms, treemap resize 143.7 vs 176.6 ms. Where the whole scene
  changes (line resize, pushes) it is neutral within noise.

### 8.11 Retina: pointer coordinates and `:scale` (`ns-scale.el`)

No real pointer was moved. Events come from `posn-at-x-y` on the live
window, which is the geometry the NS port reports for a real click. For
each image `:scale` S a scene item (x, y) is placed at display pixel
S*(x, y), the event goes through `eas-mode-event-px`, and the result is
compared with (x, y). On `frame-scale-factor` 2.0, 8 items per row, two
charts (airport-connections, scatter-plot):

| image | image-size (px) | max error, scene px |
|---|---|---|
| no `:scale` given (`:scale default`) | 1000x640 | 0.50 |
| `eas-svg-image` `:scale 1` (shipped) | 1000x640 | 0.50 |
| `:scale 0.5` | 500x320 | 0.96 / 1.00 |
| `:scale 2`, scene compiled at 450x290 | 900x580 | 0.24 |

- **NS pixels are points.** `posn-object-x-y`, `window-body-width` and
  the `:map` hit test all count points, `image-size` in pixels equals
  the display size, and `:scale default` resolves to 1 (`auto` is 1.0
  here). There is no device-pixel factor to apply: **the pinned
  `:scale 1` and the divide-by-numeric-`:scale` in `eas-mode-event-px`
  are correct under backing scale factor 2, and no `:scale` code
  changed.** The error is rounding to integer pixels (under 1 px), far
  inside any mark's hit radius.
- End to end, `pointermove` at the 8 item positions hovered a datum 8
  of 8 times on both charts.
- `:map` hit test in points: the airport map reports its own area id at
  39 of 39 area centres at `:scale 1` and 39 of 39 at `:scale 2`; the
  scatter plot 37 of 40 and 26 of 40, the misses being points that
  overlap a neighbour (the later area wins). One run reported 0 of 39 for
  the first probe right after launch, before the frame had drawn; the
  repeat gave 39.
- `posn-at-x-y` counts the header line in y and `posn-at-point` does
  not (a harness detail: the spike adds `window-header-line-height`).
- **Raster density is 1x.** librsvg rasterizes at the display size: a
  600x200 SVG at `:scale 0.5` and a 300x100 SVG at `:scale 1` both give
  `image-size` 300x100, so Emacs cannot be asked for a 2x raster of an
  SVG, and the NS backing store upscales the 1x bitmap. I could not
  look at the pixels: the one screenshot attempt captured another
  application's window and was deleted, and I did not retry. Whether it
  looks soft on Retina is for a person to judge.
- **Not tested:** GTK `GDK_SCALE` (no GTK or pgtk Emacs here) and a
  non-Retina external display. By the same logic a pgtk build counts
  logical pixels and an X build with a scaled Xft.dpi counts device
  pixels; compiling at window pixels with `:scale 1` fits both.
- **Not tested:** whether the NS port drops `mouse-movement` until the
  first click (8.3's Xvfb finding): it needs a real pointer.

### 8.12 xterm-mouse through tmux and iTerm2 (`tty-marks.sh`)

`emacs -nw -Q` in `tmux -L eas-spike` (120x40, TERM xterm-256color)
showed the `bars` template as text. For each of the 5 bars the driver
sent SGR (1006) reports at the bar's centre cell: motion with no
button (hover), press, release (click); then one motion over the empty
margin. Emacs logged every event, the view's hovered datum and
`eas-mode-event-px`.

- **Result, both runs: 5 of 5 bars hovered and clicked on the right
  datum (0-4), and the margin hovered nothing.** Per cell Emacs decoded
  `mouse-movement`, `down-mouse-1`, `mouse-1`, in the expected cell.
- Run 1: reports written into Emacs's own pane (`tmux send-keys -l`).
  Run 2: the same session attached to a real **iTerm2** window
  (`create window ... command "tmux -L eas-spike attach"`, 120x40),
  with the reports typed into that window's session by AppleScript
  `write text`, so they travel iTerm2, tmux client, tmux server, Emacs.
  Identical output.
- **Bug, fixed:** `xterm-mouse-mode` was **not** turned on when the
  chart opened. `eas-tty-mouse-capable-p` read `$TERM`, which this NS
  build reports as `"dumb"` inside `emacs -nw` under tmux while
  `tty-type` says `xterm-256color`; the capability test failed and the
  mouse stayed dead until it was enabled by hand. It now asks
  `tty-type` first (`eas-tty--frame-term`) and `$TERM` second;
  `eas-tty-keymap-and-mouse-capability` covers both.
- **kitty is not installed** (/Applications has iTerm, Ghostty, WezTerm
  and Alacritty); nothing was run in the others.
- **Needs a person:** real clicks and a physical mouse in iTerm2 (the
  AppleScript path types the report bytes; it does not exercise iTerm2's
  own mouse encoding, report rate or SGR-pixel 1016), a real mouse in
  kitty, and a recorded hover session for the perceived rate.
- Setup notes: the login `~/.tmux.conf` hangs a fresh private server, so
  the script runs `tmux -f /dev/null`, and Emacs is the pane's command
  because the login shell takes tens of seconds to start. iTerm2 did not
  quit on request after its window closed, so a run that has to launch
  it leaves it open with no windows.

### 8.13 Decisions

- **The redraw default stays idle-coalesced** (confirmed): a changed
  frame costs 60-140 ms on NS even on an M5 Max, 128.6 ms for an airport
  hover.
- **`:scale` handling is right** under backing scale factor 2; GTK
  `GDK_SCALE` is untested.
- Changed: `eas-mode--flush-replaced` (flush a replaced image, not a
  repeated one) and `eas-tty--frame-term` (mouse detection), each with a
  test (`eas-glue-test.el`, `eas-tty-test.el`).
- Still open: a real pointer on NS (motion before the first click,
  report rate, how soft the 1x raster looks on Retina), real iTerm2 and
  kitty clicks, GTK `GDK_SCALE`, a human hover session.

### 8.14 GUI (NS) after 0.2.4 (`ns-frames.el`, eas-poo)

Same machine and window as 8.10 (Emacs 30.2 NS, `frame-scale-factor` 2.0,
1000x640 chart, 1020x704 frame), byte-compiled eas 0.2.4, niced, the
laptop in use (load average 10-12), so read the milliseconds as upper
bounds. `scripts/eas-spikes/gui/run-ns.sh ns-frames.el OUT`
(`NSF_EXTRAS=1` for the two extra measurements); raw output in
`scripts/eas-spikes/out/ns-frames.out` and `ns-frames-extras.out`. Each
frame is the change, the readout line (`eas-mode-strip-string`, an SVG
image line in a GUI), `eas-mode-redraw`, then `(redisplay t)`; 8 warm-up
frames dropped, 50 timed. Milliseconds, median / p95 per frame, shipped
config (render cache on, replaced images flushed):

| workload | step | readout | redraw | raster (redisplay) | total | image cache after |
|---|---:|---:|---:|---:|---:|---:|
| ladder push, 25 levels | 0.9 / 14.3 | 0.01 / 0.1 | 0.7 / 0.8 | 83.2 / 103.7 | **92.8 / 107.9** | +1.1 MB |
| depth push, 25 levels | 1.0 / 13.1 | 0.2 / 12.3 | 0.6 / 0.7 | 73.5 / 85.2 | **83.7 / 87.9** | +12.8 MB |
| clock tick | 0.5 / 14.8 | 0.2 / 13.3 | 0.5 / 0.6 | 92.4 / 107.0 | **93.7 / 109.3** | +13.0 MB |
| pacman tick | 1.2 / 16.9 | 5.1 / 21.3 | 0.6 / 0.7 | 74.4 / 169.4 | **96.5 / 176.2** | +0 |
| airport-connections hover | 0.5 / 1.0 | 0.15 / 0.19 | 0.6 / 0.7 | 105.3 / 131.3 | **106.6 / 133.3** | +12.5 MB |
| airport-connections, pointer parked | 0.2 / 0.3 | 0.01 / 0.01 | 0.1 / 28.0 | 1.6 / 1.8 | **1.9 / 30.1** | +0.2 MB |

Every row but the last changes the chart SVG on every frame (the clock
moves its hands at 1 s per tick; pacman revisits 14 distinct frames in
50). The p95 spikes of 12-28 ms in step, readout and redraw are garbage
collections: this loop never goes idle, which `eas-gc-defer` waits for.
"Image cache after" is `image-cache-size` after the run minus before
it, and is bounded: it is one extra window-sized image (about 9-13 MB on
this Retina frame).

By config (median total ms; the airport hover and ladder rows are the
spread):

| workload | shipped | render cache off | no image-flush | readout as text, no image |
|---|---:|---:|---:|---:|
| ladder push | 92.8 | 107.0 | 98.9 | 89.3 |
| depth push | 83.7 | 94.9 | 105.1 | 95.5 |
| clock tick | 93.7 | 122.4 | 118.2 | 114.7 |
| pacman tick | 96.5 | 119.6 | **14.3** | 105.9 |
| airport-connections hover | 106.6 | 163.6 | 120.7 | 117.2 |
| airport-connections parked | 1.9 | 5.1 | 2.1 | 2.1 |

- **The chart raster dominates: 74-105 ms of every changed frame, 80-95%
  of the total.** The engine (step, readout, redraw) is 1-3 ms at the
  median, in line with the batch benches (bench/frames-0.2.4.md). The
  raster is the same for a trivial clock (92 ms) and the airport map
  (105 ms) because it scales with image pixels, not with marks: the clock
  rasterizes in **25.8 ms at 500x290, 61.0 at 800x500 and 103.0 at
  1000x640** (`ns-frames-extras.out`). A live chart in a window this size
  tops out near 10 frames a second whatever eas does; 0.2.4 moved the
  engine's share, not this one. Filed as eas-e3s with the ideas (cap the
  rasterized size, a bounded ring of unflushed frames, a smaller redraw
  area); none is small.
- **The readout line is not the problem.** The image is 1020x16 px and
  costs **5.0 ms** (p95 5.7) to rasterize when its text changes (a
  distinct readout every frame), about 5% of a frame; building the
  string is 0.01-0.2 ms (memoized per scene, state and width). Turning
  the readout image into plain text saves nothing outside noise (ladder
  89 vs 93, airport hover 117 vs 107 ms). When the readout does not
  change, the same image is reused: parked pointer **1.9 ms** total. Pacman is the
  one slow readout (5.1 ms median, 21 at p95): its strip lists many
  fields and changes every tick.
- **The eas render cache earns its place on NS**, as in 8.10: hover
  106.6 vs 163.6 ms, clock 93.7 vs 122.4, ladder 92.8 vs 107.0; the
  parked pointer 1.9 vs 5.1 ms (its redraw 0.1 vs 3.0 ms).
- **The Emacs image cache must still be flushed**: without `image-flush`
  it grows by **12 MB a frame** (595-617 MB over 50 frames, RSS +1.1 to
  +1.2 GB); with it, 9-13 MB in total. No-flush is not faster for
  charts that never repeat (ladder 98.9 vs 92.8, depth 105.1 vs 83.7,
  hover 120.7 vs 106.6). It is faster only for an animation that revisits
  frames: **pacman 14.3 ms median unflushed against 96.5 flushed**,
  because 36 of its 50 frames are in the cache already, at the price of
  133 MB for the 14 distinct images. A bounded ring would need to hold
  the whole loop to win, which is the 133 MB; left out.
- **Fixed: nothing.** No one cost is both dominant and small to fix:
  the 8.10 repeated-image bug stays fixed (parked 1.9 ms, hover with an
  unchanged chart 1.6 ms of raster), the readout does not re-rasterize
  when unchanged, and the cache does not thrash. The raster is filed.

## 9. Terminal parity through a real terminal (fc-qx1.8)

`scripts/eas-spikes/tty-parity.sh` opens a point chart (brush, click
selection, legend) in `emacs -nw` inside tmux (TERM `tmux-256color`).
It injects SGR mouse reports at the cells of real data and then types
keys. Showing the chart turned on `xterm-mouse-mode` by itself
(`eas-tty-xterm-mouse`). Motion with no button, a press, motion with
the button held, and a release reached the reducer as pointermove,
pointerdown, pointermove and pointerup, and the drag brushed. `z`, `[`,
`n`, RET and a wheel report followed. The recorded log, replayed in
lockstep on a fresh text view and a fresh SVG view
(`eas-parity-replay`), gave 9 steps with 0 mismatches, and the fresh
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
`eas-parity-translate` maps each event's pixels from one scene to the
other through the scales (legend entries map to the same entry), and
`eas-parity-state` compares domains, selections, the hovered and
clicked datum, history depth and drag mode, within a relative 1e-9.
## 8. Geometry against bin/chart's references (fc-qx1.21)

The oracle compares native SVG, rasterized by rsvg-convert, with the
PNGs bin/chart built for every gallery spec. Those PNGs are committed
in test/conformance/ref with a manifest of spec and PNG hashes and the
zone they were built in, so the oracle runs wherever rsvg-convert does.
`bin/chart diff` scores any canvas size difference as 1.0, so images
are compared in Elisp (eas-png.el). Both are padded onto their union
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
  1 March there, not 2 March. `eas-time-zone` (nil = UTC, the
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
  and parsed each string every time: 37% of a 10k move. `eas-time-parse`
  now memoizes strings in a bounded table (200k entries, then cleared).

Terminal redraw (`scripts/eas-spikes/crosshair-cost.el`, 100x30
cells, one pointermove one cell to the right plus one redraw, batch
and so without redisplay):

| rows | redraw | ms/move | cells written/move |
|---|---|---|---|
| 1k | patch (`eas-mode-patch-text`) | 29.2 | 51 |
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
  `encoding.tooltip` (`eas-crosshair-readout`).
- The 10k text move is now dominated by rendering the whole grid
  (50 ms). Rendering only the units that changed belongs to `fc-qx1.9`.

## 10. Performance budget (fc-qx1.9)

Box: the fc-qx1.14 box class (Linux 6.8, 4 vCPU AMD EPYC-Rome, GNU
Emacs 30.1 `--without-x`). The ladder ran byte-compiled in batch, so
these are Lisp-side numbers. librsvg and redisplay are not included
(see section 8 for those).

### 10.1 The ladder: `bench` with no SOURCE

`bin/eas bench` (Lisp: `(eas-agent "bench")`, `eas-bench.el`)
measures two fixed workloads at 1k, 10k and 100k points and reports
JSON: `eas-bench/v1` with one rung per size, and each stage as
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
| hit-line (one eas-hit, x-sorted index) | 0.007 | 0.007 | 0.007 |
| compile-points (scatter scene) | 3.9 | 38 | 379 |
| hit-points (one eas-hit, grid index) | 0.030 | 0.10 | 0.92 |
| lttb (N points -> 800) | 0.70 | 3.2 | 26.0 |

The hypothesis from fc-qx1.14, hover feedback under 50 ms at 10k
points, holds on the Lisp side: 0.24 ms for the hover and 3.7 ms with
the SVG redraw. Adding the section 8.1 raster cost for one path plus a
few labels (about 15–21 ms on the slower Xvfb box) gives an estimated
20–25 ms per GUI move at 10k. That estimate has not been measured end
to end in a GUI frame. hover-first at 100k (82 ms) is paid once per
view, by the first move.

### 10.2 What made hover flat

Before, measured on this box with `scripts/eas-spikes/dispatch-cost.el`
(section 6's script), then after:

| spec | N | hover before | hover after |
|---|---|---|---|
| rule per datum, opacity condition | 1k / 10k / 100k | 4.6 / 39.0 / 348.9 | 0.3 / 0.3 / 0.4 |
| Vega-Lite idiom: rule layer filtered by the param | 1k / 10k / 100k | 2.4 / 24.4 / 219.0 | 0.3 / 0.2 / 0.3 |

A profile of the 100k filter idiom put 77% of a hover in the
`{"param": "hover"}` filter, which tested every row against the store,
and 22% in `inspect`, which re-summarised every visible row.

- `eas-params-index.el` answers a bare `{"param": NAME}` filter on a
  point selection from a hash of each row's tuple over the store's
  fields. The hash is built once per rows vector and held in a weak
  table. Keys normalise values the way `eas-params--same` compares
  them (numbers as floats, date strings as epoch ms). The same index
  bounds which rows `eas-compile-patch` re-tests for conditional
  encodings. Interval stores, and integers past 2^53, fall back to the
  row-by-row test. ERT checks the index against that test over mixed
  numbers, floats, -0.0, dates and strings, and checks that patched
  scenes equal full compiles.
- `eas-view--visible-summary` caches its summary per mark rows
  vector and x domain, so a hover reuses it and a zoom recomputes it.

### 10.3 Hit-testing and LTTB

The grid index for point marks was built at compile time and then
never read: `eas-hit-mark` scanned every item and allocated a
candidate per item. One query cost **1.88 ms at 1k, 13.5 ms at 10k and
113.6 ms at 100k**. `eas-hit--grid` now searches square rings of
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
right after the handler. `eas-gc.el` therefore works like gcmh. The
first chart event raises `gc-cons-threshold` to
`eas-gc-cons-threshold` (64 MB). After `eas-gc-idle-delay` (1 s)
of idle time it collects once and puts the user's value back. It never
lowers a larger value and leaves alone a value someone else changed in
the meantime. Setting `eas-gc-cons-threshold` to nil turns it off.
Batch runs are left alone. The raise lasts only while a chart is in
use, so the design rule that a permanent global threshold is the
user's choice still holds.

In batch the effect is small, because the heap is small: hover-svg at
1k is 4.1 ms mean (max 5.0) deferred against 5.7 (max 16.6) at the
default 800 KB (`bench --gc default`). A long-running GUI session
traces a much larger heap on every collection, which is where section
8.5 measured 80–97 ms of GC per move. The end-to-end gain in a GUI
frame is **unmeasured** on this box, which has no display. Re-run
`scripts/eas-spikes/gui/motion.el` to measure it.

### 10.5 The regression budget in CI

`src/bench-budget.json` holds this box's reference means, a
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

## 11. Linked views (fc-qx1.6)

Measured on Linux, Emacs 30.1, byte-compiled, `gc-cons-threshold`
64 MB, 59 pointer moves across two 800x300 line charts with the line
template's crosshair (a nearest x point selection plus a rule filtered
by it) and a scales-bound zoom. "Linked" joins both to one bus, so each
move also dispatches a link event to the second view and redraws its
rule.

| rows per view | hover, one view (mean / max ms) | hover, two linked views (mean / max ms) |
|---:|---:|---:|
| 1,000 | 0.39 / 0.61 | 0.69 / 0.89 |
| 10,000 | 0.33 / 0.48 | 1.02 / 1.57 |

A linked hover costs one more view's hover plus snapping the x to the
receiver's nearest datum (one hit-test). `make bench` stays within
`bench-budget.json` (hover at 100k: 0.53 ms mean).

**Decisions:**
- One spec: Vega-Lite's own constructs, no engine API. A select param
  on a concat (optionally limited with `views`) is defined in every
  named view with one store; a param bound to scales held by several
  views zooms them together (`eas-link-scales`, after every reducer
  step, keeps a wheel gesture as one history entry); a scale domain of
  `{"param": NAME}` follows that interval and clips the view, as
  Vega-Lite does (overview + detail).
- Across buffers: a named bus delivers changes as `link` event/v1,
  keyed by channel, not field name, so two tickers whose date fields
  differ still share "the same x". Receivers log link events and never
  forward them, so a bus cannot echo and a receiver's log replays alone.
- Still open: `resolve.scale.x: "shared"` on a concat (one unioned
  scale, so bar panes pad like line panes) is not implemented; panes
  with independent scales keep Vega-Lite's per-view padding. Pixel
  conformance of the gallery's linked examples could not be run in the
  fc-qx1.6 box (no `rsvg-convert`); they are tested as behaviour.
## 11. Vega-Lite gallery: area and circular (fc-qx1.29)

The 13 official examples of the "Area Charts & Streamgraphs" and
"Circular Plots" sections (`test/vl-examples/area-circular/`) render
natively, SVG and text, from the unmodified specs. `eas-vl-gallery.el`
inlines their url data (JSON as is, CSV/TSV with numbers inferred),
compiles them in the references' zone (America/Chicago), compares the
SVG with the committed bin/chart PNG through `eas-png-compare`, and
checks the layout at 320x200, 480x300 and 900x560 px and 50x14, 80x24
and 120x36 cells: no view overlapping another or a legend, nothing off
the canvas, no colliding axis labels. `status.json` holds the verdict
and threshold per example; `eas-vl-gallery-area-circular-holds-its-status`
re-runs them and the text renderings are goldens
(`test/eas/golden/vl-area-circular/`).

All 13 pass. Differing pixels against the references, at identical
canvas sizes, were 0.0002 to 0.0177 (area_horizon). rsvg-convert was
not installed in this box. The native SVG was rasterized with
resvg-js 2.6 (the renderer vl-convert itself uses) and Liberation Sans
(Arial metrics), through an `rsvg-convert` stand-in on PATH. The
existing gallery scored as before under the stand-in (all 47 pass).
Thresholds leave room for librsvg's antialiasing: max(0.02, 2.5 x
measured).

What the examples needed, all shared code:

| feature | where |
|---|---|
| arc mark; theta (stacked by default, in order/color order, normalize) and radius channels; sqrt/pow scales; polar text at mid-angle; wedge hit-testing through the centroid, `:map` polygons, shaded wedges in text | `eas-polar.el`, `eas-arc.el`, `eas-scale.el` |
| `mark.line` / `mark.point` overlays, as Vega-Lite's pathoverlay normalizer | `eas-overlay.el` |
| gradient fills (`mark.color` gradient) | `eas-paint.el` |
| `interpolate: monotone` (d3 curveMonotoneX, sampled) | `eas-curve.el` |
| `stack: center` | `eas-marks.el` |
| named categorical schemes (`category20b` ...) | `eas-scheme.el` |
| explicit discrete color `scale.domain`; `legend.orient: none` at legendX/legendY; `axis.domain: false`, `axis.tickSize` | compile, legend, layout |
| primitive data values as `{"data": v}`; `"Mon D YYYY"` dates; timeUnit titles read coarse to fine ("year-month") | compile, `eas-time.el`, `eas-transform.el` |

Fitted to a window, a symbol legend now keeps the entries that fit
beside its plot and says so in its title ("series, 9 of 14";
`eas-legend-fit.el`). Before, a 14-entry legend at 240x160 ran off the
canvas and squeezed the plot to a sliver. At its own size the canvas
still grows to hold the whole legend, as Vega's does. Still below
320x200 (or 50x14 cells): stacked_area's 14 long legend labels leave the
plot about 40 px. Its two x labels then collide, as Vega's do with two
labels.

`mark-arc`, `encoding-radius` and `encoding-order` joined the
conformance gallery (arc_pie, arc_radial, arc_pie_pyramid) and prove
`mark/arc`, `encoding/theta`, `encoding/radius`, `encoding/order` and
`scale/sqrt` in `supported.json`.

## 12. Test suite split (fc-qx1.47)

After the gallery wave `make test` took about 8.4 min. Measured on a
4-core box, Emacs 30.1, no rsvg-convert (so the image oracle skipped):
`eas-vl-gallery-groups-hold-their-status` alone took 460 s (it walks
all 188 official examples) and the conformance oracle 194 s (every
test touching `eas-conformance-gallery` loads all 190 entries, about
4 s each time; `supported-json-is-current` reruns the whole gallery,
87 s).

Those tests are tagged `:gallery` and run in `make test-gallery`
instead: one Emacs per gallery group (`EAS_GALLERY_GROUPS`) plus one
for the conformance tests, so `make -j` parallelizes it.

| target                   | tests                    | wall    |
|--------------------------|--------------------------|---------|
| `make test` before       | 594 (4 skipped)          | ~8.4 min |
| `make test` after        | 583 (3 skipped)          | 47 s    |
| `make -j4 test-gallery`  | 19 = 9 groups + 10 conformance (1 skipped) | 6.5 min |

The gallery's makespan is its two largest groups (240 s and 201 s
under 4-way contention) and the conformance target (182 s); sharding
inside a group is the next step if it matters.
## 12. Area and circular, second pass (fc-qx1.42)

All 13 examples of `test/vl-examples/area-circular/` already passed, and
they still do with unchanged thresholds. The rasterizer stand-in was
again resvg-js 2.6 with Liberation Sans; under it, differing pixels
against the references are 0.0002 (layer_arc_label) to 0.0177
(area_horizon). The second pass was about cost and customizability.

### 12.1 Render cost

`scripts/eas-gallery-bench.sh area-circular` runs the bench verb on
every example, byte-compiled, in the references' zone, and writes
`test/vl-examples/area-circular/bench.json`. That file holds the means
of 50 reps per stage, `baseline_ms` (the same script run on the parent
commit) and the calibration of both runs. Profiles found six costs:

| cost | fix | where |
|---|---|---|
| timeUnit flooring: a `decode-time` and an `encode-time` in a named zone per row (1708 rows, 123 dates) | memoized per (unit, value, zone) | `eas-transform.el` |
| the stack sort ranked both rows with `seq-position` on every comparison | ranks computed once per row | `eas-marks-series.el` |
| series keys: the split defs looked up again and `assoc` per row | defs once per unit, a hash of series | `eas-marks-series.el`, `eas-marks.el` |
| SVG numbers trimmed with a regexp; text areas rebuilt the polyline's x vector per column | suffix checks; x vector once per item | `eas-svg.el`, `eas-text.el` |
| time-axis ticks and labels recomputed for every tick count layout tries | memoized per zone | `eas-scale-time.el` |
| a hash table allocated per expression evaluation (per row in a filter) | made on `random()`'s first call | `eas-expr.el` |

Byte-compiled, America/Chicago, mean of 50 reps, ms (before -> after):

| example | compile-svg | compile-text | render-svg | render-text |
|---|---:|---:|---:|---:|
| area (1708 rows) | 108 -> 7.5 | 113 -> 6.5 | 2.6 -> 1.2 | 3.3 -> 2.5 |
| stacked_area | 133 -> 29 | 136 -> 30 | 16.8 -> 8.7 | 14.7 -> 7.0 |
| stacked_area_normalize | 138 -> 35 | 152 -> 34 | 16.5 -> 8.5 | 15.3 -> 7.0 |
| stacked_area_stream | 140 -> 34 | 145 -> 33 | 15.9 -> 8.1 | 14.6 -> 5.4 |
| area_gradient (560) | 20 -> 5.5 | 5.1 -> 3.8 | 3.3 -> 2.0 | 3.1 -> 2.6 |
| area_overlay (560) | 22 -> 6.5 | 7.2 -> 5.0 | 4.4 -> 3.1 | 3.3 -> 2.6 |
| area_horizon (20) | 3.2 -> 3.2 | 2.7 -> 2.6 | 6.4 -> 3.2 | 6.0 -> 2.0 |

The arcs were already under 3 ms per stage and stay there, within the
noise of a collection. bench.json has every example's numbers. The
ladder in `bench-budget.json` is unaffected: it has no timeUnit. Compile in UTC was already cheaper (no named zone),
but stacking and series keys cost the same in every zone.

### 12.2 Properties: honored, or reported

An audit rendered each documented Vega-Lite property of the group's
marks (area, arc, the text of arc labels) and its axes, legends, title,
scales and config, once with the property and once without. Anything
that changed neither the SVG nor the text was either a correct no-op
(an encoding overriding a mark color, `tension` on a linear area) or a
gap. The gaps this pass closed:

- area and arc: `fillOpacity`, `strokeOpacity`, `strokeDash`,
  `strokeDashOffset`, `strokeMiterLimit`, `strokeCap`, `strokeJoin`,
  `blend`, `href`, `filled: false`, an area's outline (`stroke`), arc
  `cornerRadius` and `padAngle` as d3.arc draws them (`eas-arc-d3.el`,
  checked against d3-shape), mark `theta`/`theta2`/`radius2`
  (`eas-mark-style.el`)
- `interpolate` basis, basis-open, bundle, cardinal, cardinal-open,
  catmull-rom and natural, with `tension` (`eas-curve-extra.el`, equal
  to d3-shape's sampled points)
- legends: their own label, title and symbol properties over
  config.legend, plus `values`, `format` and `labelExpr`
  (`eas-legend-style.el`)
- titles: `subtitle` and its color, size, weight and padding, `anchor`,
  `dx`, `dy` (`eas-title-extra.el`)
- scales: theta/radius `domainMin`, `domainMax`, `reverse`, theta
  `rangeMin`/`rangeMax`; position `domainMin`/`domainMax` win over
  `zero` and turn `nice` off, as in Vega-Lite
- axes: `titlePadding` on bottom and left axes, `values` given as date
  strings on a time axis (it crashed); polar text keeps `dx`/`dy`

Not honored, and now named by `check` (`eas-spec-props.el`) as
non-blocking `UNSUPPORTED_FEATURE` warnings with `ignored: true` and
the JSON path: fonts and font styles, label/title opacity and
alignment overrides, `zindex`, `aria`, legend layout (`orient` other
than right/none, `columns` > 1, `direction` on symbol legends,
`padding`, `fillColor`, `strokeColor`), position-scale `range`,
`rangeMin`, `rangeMax` and `clamp`, the axis-type config sections, and
`config.locale`/`numberFormat`/`timeFormat`/`style`. Across the
gallery, 17 specs set one of these.

`custom/custom_area`, `custom/custom_arc` and `custom/custom_radial`
set non-default properties of every kind above. Each must pass `check`
with no warnings, render on both backends without overlap at the three
sizes, and match its text golden. With bin/chart on PATH,
`eas-vl-gallery-custom.el` builds `custom/ref/NAME.png` and holds each
spec to its `usermeta.eas.threshold`. bin/chart was not in this box, so
no reference is committed yet. A local Vega 6 / Vega-Lite 6.4.1 render
(node, canvas-free text metrics) differed from the native one by 1.6 to
2.0% of pixels. That is about what the canvas-free metrics alone
account for: the same oracle is 0.5 to 17% off bin/chart's own
references.

## 12. Vega-Lite gallery: calculations render cost (fc-qx1.43)

Byte-compiled bench verb, 5 runs per example, at each spec's own size,
Emacs 30.1 in batch on the fc-qx1.43 box (`scripts/eas-gallery-bench.sh
calculations`; every number is in `test/vl-examples/calculations/bench.json`
with the earlier build as its baseline).  Calibration was 84.6 ms for the
baseline run and 99.8 ms for the new one, so the new numbers are if
anything pessimistic.

| example | compile-svg before | after |
|---|---:|---:|
| joinaggregate_mean_difference_by_year | 1351 ms | 107 ms |
| joinaggregate_residual_graph | 1013 ms | 280 ms |
| parallel_coordinate | 504 ms | 189 ms |
| layer_line_rolling_mean_point_raw | 282 ms | 109 ms |
| layer_point_line_loess | 344 ms | 316 ms |

The sum of all stage means over the group went from 8.7 s to 3.6 s.  What
it took:

- A layered unit ran its ancestors' transforms on their rows before its
  own, once per layer (seven times for parallel_coordinate).  Within a
  compile, a transform array on the same rows vector now runs once
  (`eas-compile-memo`), as Vega's shared dataflow does.
- `decode-time`/`encode-time` with a named zone look the zone up on
  every call: a year timeUnit over 3201 movies cost a second.  The
  zone's offset is now cached per epoch-aligned week and applied with
  UTC arithmetic; a week holding a transition asks Emacs directly
  (`eas-time-offset.el`; agrees with Emacs on 40,000 random instants
  over 160 years).
- A sequential color ramp converted its stop colors to HCL for every
  item; conversions are cached by hex.

Loess remains Vega's O(n * bandwidth) fit over three robustness
iterations, about 8M kernel evaluations here; inlining the kernel did not
measurably help, so it was left as it is.

## 12. Vega-Lite gallery: bar, second pass (fc-qx1.38)

All 24 bar examples pass (before: 15 pass, 9 partial).  What the nine
partials needed, all in shared code:

| example | what was missing | where |
|---|---|---|
| bar_aggregate, stacked_bar_normalize | fitted to 320x200, nominal labels collided | `eas-axis-fit.el`: a fitted chart thins default (unset labelOverlap) nominal labels by parity |
| stacked_bar_h_normalized_labeled | stacked text at `bandPosition` 0.5 sat at the segment end; same collisions | `eas-stack-band.el` |
| bar_negative_horizontal_label | Vega pads a continuous domain after zero and before nice; native padded the niced domain | `eas-compile-scales.el` (`:raw-domain`) |
| bar_size_responsive | `width: "container"` | `eas-container.el` (480 px without a size, bin/chart's) |
| bar_grouped_repeated | untyped datums (a repeat's fields) were neither nominal colors nor xOffset bands; datum domains keep layer order | `eas-encode.el`, `eas-compile-channels.el` |
| bar_heatlane | y/y2 on a band scale span band centre to centre; a non-position channel took y2's values into its domain; bar offsets | `eas-marks.el`, `eas-compile-scales.el`, `eas-bar-extra.el` |
| bar_axis_space_saving | titleX/titleY/titleAngle/titleAlign, axis bandPosition, ticks false dropping tickSize from the label offset, mark height `{band}` and yOffset | `eas-axis-pos.el`, `eas-layout.el`, `eas-bar-extra.el` |
| bar_diverging_stack_population_pyramid | legend orient top (horizontal, one row) | `eas-legend-orient.el` |

The pyramid passes at a recorded threshold of 0.05: Vega-Lite's own
SVG of it, rasterized here, scores 0.0357 against the reference and
native scores 0.0031 against that SVG, so the 0.036 residual is
anti-aliasing of its 38 band edges 1.05 px apart, not geometry.

**How it was measured without bin/chart or rsvg-convert.**  This box
had neither.  `scripts/eas-spikes/vega-oracle` stands in for both:
Vega-Lite 6.4.1 + Vega 6 with bin/chart's vendored theme and text
measured as Arial (Liberation Sans, via node-canvas), and an
`rsvg-convert` that rasterizes through the librsvg node-canvas
bundles.  Calibration: its SVG of 7 bar examples scores 0.0031-0.0079
against the committed references at identical canvas sizes.  Its
rasterizer is stricter than CI's on native SVG: unchanged SVGs score
1-2.5x their recorded ratios (bar 0.0022 -> 0.0054), so the new
verdicts are conservative.  Five examples of other groups whose SVG is
byte-identical before and after this work exceed their thresholds under
it (boxplot_preaggregated 0.0315, histogram_nonlinear 0.0406,
layer_likert 0.0359, layer_text_heatmap 0.0385, rect_heatmap 0.0312);
that is the rasterizer, not a regression.  Native against the oracle's
SVG through the same rasterizer is at most 0.0098 for all 24 bar
examples, with identical canvas sizes.

Shared fixes changed five more examples outside the group, none for
the worse except nested_concat_align (partial before and after):
angled x labels now take Vega-Lite's default align and baseline
(layer_candlestick 0.017 -> 0.006, interactive_concat_layer 0.024 ->
0.011), rect_mosaic_labelled_with_offset 0.634 -> 0.015;
nested_concat_align now draws countTitle "Count" and truncates at
axisX.titleLimit as Vega does, which scores worse (0.113 -> 0.196)
only because its concat section titles are still missing (multiview).

**Customizability.**  `check` names every axis, legend, title, view
and config property (`eas-spec-props.el`); the unhonored ones are
`UNSUPPORTED_FEATURE` with their path.  To keep the gallery and
templates clean, these were implemented: axis titleX/titleY/
titleAngle/titleAlign/titleBaseline/titlePadding/titleLimit/
bandPosition/zindex/domainDash, label and title font sizes in layout,
legend orient top/bottom/left, columns, values and format,
config.legend orient/direction, config.view fill/strokeWidth/
strokeDash/opacity, config.countTitle, config.scale
barBandPaddingInner (and bandWithNestedOffsetPaddingInner's
precedence), bar width/height, xOffset/x2Offset/yOffset/y2Offset,
strokeDash/strokeCap/strokeJoin and orient.  The six customization
specs (`test/vl-examples/bar/custom/`) score against the oracle:
bar_custom 0.0319, bar_horizontal_custom 0.0000, stacked_bar_custom
0.0106, bar_grouped_custom 0.0000, bar_ranged_custom 0.0222,
bar_labels_custom 0.0232.

**Latency** (`scripts/eas-bench-gallery.sh bar 20`, the bench verb on
byte-compiled eas, means over 20 runs, `test/vl-examples/bar/bench.json`).
The four examples over seattle-weather's 1,461 rows spent 85% of
compile in `decode-time`/`encode-time` through a named zone, once per
row per timeUnit.  Zone conversions are now memoized
(`eas-time--zone-cache`, the pattern of the existing parse cache), time
unit strings parse once, and aggregation groups with one hash lookup:
those four compile in 21-30 ms instead of 194-207 ms, and the group's
summed compile-svg mean fell from 976 to about 300 ms.  The other 20
examples compile in 3-29 ms either way (the new property checks and bar
hooks add up to about 1 ms on the smallest).  In batch, GC is 35-40%
of render time; interactive use defers it (`eas-gc.el`, section 10.4).

## 12. Distributions: render cost and the property sweep (fc-qx1.39)

Box: Linux, Emacs 30.1 batch, byte-compiled, calibration 96 ms
(`eas-bench-calibrate`; the fc-qx1.9 reference machine's is in
bench-budget.json). rsvg-convert was not installed; librsvg 2.60 from
Debian's packages (unpacked, no root) with Liberation Sans stood in. It
scores the committed references 0.003-0.007 higher than the box that
recorded them (antialiasing only), so ratios recorded here carry that
bias. A local Vega-Lite 6.4.1 + Vega 6 + node-canvas pipeline
(`scripts/eas-vega-ref.mjs`) reproduces bin/chart's distributions
references at 0.0005-0.017 (histogram 0.0030; rect_binned_heatmap 0.14
and the emoji chart are the exceptions), which made it usable as an oracle for
references bin/chart gets wrong and for the property sweep.

Mean ms per stage, 20 runs (`scripts/eas-gallery-bench.sh distributions 20`;
compile before -> after this pass; cold is one svg compile with every memo
table empty):

| example | cold | compile svg | compile text | render svg | render text | hover |
|---|---:|---:|---:|---:|---:|---:|
| area_cumulative_freq | 21.8 | 25.8 → 22.0 | 24.0 → 20.0 | 2.9 | 4.9 | 0.13 |
| area_density | 322.3 | 280.9 → 17.0 | 272.0 → 17.3 | 4.2 | 6.3 | 0.16 |
| area_density_stacked | 52.0 | 59.3 → 26.8 | 57.0 → 29.8 | 13.9 | 12.8 | 0.38 |
| circle_binned | 29.0 | 43.6 → 45.9 | 46.7 → 46.6 | 6.6 | 1.6 | 0.38 |
| histogram | 21.1 | 28.8 → 30.0 | 26.2 → 27.9 | 1.6 | 2.5 | 0.14 |
| layer_cumulative_histogram | 33.3 | 50.7 → 45.4 | 49.2 → 49.8 | 3.6 | 2.3 | 0.20 |
| layer_line_errorband_ci | 312.5 | 718.7 → 17.5 | 700.3 → 9.1 | 4.3 | 2.1 | 0.47 |
| layer_point_errorbar_ci | 92.7 | 189.5 → 4.1 | 182.8 → 2.3 | 5.3 | 3.9 | 0.17 |
| rect_binned_heatmap | 61.9 | 84.7 → 73.6 | 82.2 → 71.7 | 25.6 | 4.6 | 3.80 |
| the other 10 | 1.6-5.9 | ≤ 5.2 | ≤ 4.7 | ≤ 4.5 | ≤ 4.9 | ≤ 0.24 |

Where the time went, measured with elp (rarely-called functions only, so
instrumentation stays out of the numbers):

- ci0/ci1: 74% of layer_line_errorband_ci's compile. Each op ran the
  full 1000-resample bootstrap on the same group, and each recompile ran
  it again. The xorshift loop itself is about 0.6 µs per draw in
  bytecode; unrolling or integer sums gained at most 25%, so the fix is
  exact memoization (`eas-memo.el`), not a faster loop.
- timeUnit floors: the rest of it. `decode-time` and `encode-time` with
  a zone cost ~60 µs per row; 824 calls cover 24 distinct values.
- density: one Gaussian per value per sample (3201 x 201 here), ~0.3 µs
  each with `exp`. Precomputing 1/bw saved 8% and changes results in the
  last bit, so it was left; the kernel sums are memoized instead.
- 3201-row charts (histogram, circle_binned, heatmap) spend their time
  in bin, filter and aggregate at 2-6 µs per row, mostly the row copy
  each transform makes; no single hot spot.

Property sweep: 1293 cases over mark (9 types), axis, config.axis,
legend (symbol and gradient), config.legend, scale, title, config.title,
header, config.header, bin, errorbar, errorband, config.view, top-level
config and config.<mark>; 2.5 min for Vega, 9 s native, 9 min for the
image comparisons. Pixel ratios alone missed faint changes (a light
gray domain line is under pixelmatch's color threshold), so "native
ignores it" is decided from the SVGs (Vega's changed, native's did not),
and the ratios only judge correctness. Verdicts and their use:
gallery-coverage.md.

## 12. Vega-Lite gallery: scatter-table polish (fc-qx1.40)

All 22 scatter-table examples now pass (they were 18 pass, 3 partial
and 1 unsupported). Each fix is in shared code:

| example | was | fix |
|---|---|---|
| rect_mosaic_labelled_with_offset | 0.6344 | `eas-compile--values` read the y2 partner for every channel, so `ny2` leaked into the color and opacity domains (`eas-compile-scales.el`); `resolve.scale.x: "shared"` now unions concatenated views' positional domains (`eas-compile-shared-pos.el`); a layer's `axis.title` titles the merged axis (`eas-bins.el`); `config.concat.spacing` is read; ticks that are off take no room before the title (`eas-layout.el`). Ratio 0.0137, canvas within 1 px. |
| point_angle_windvector | unsupported | equalEarth, mercator and equirectangular projections of longitude/latitude points (`eas-projection.el`): d3-geo's raw formulas, lowered at compile to projected x/y, fitted after layout as d3's `fitSize` does (one scale factor, centred). Ratio 0.0003; the same chart with every wedge turned 90 degrees scores 0.307. |
| rect_lasagna | overlap at 320x200 | `labelOverlap: false` asks for every label. The yearly labels' advance boxes overlap by about 1 px at 320x200, as Vega's do. The gallery overlap check now leaves axes with an explicit `labelOverlap: false` alone. |
| point_offset_random | 0.0646 > 0.03 | Vega's `random()` is `Math.random`, so its jitter cannot be reproduced. Two native renders that differ only in their pseudo-random jitter differ by 0.066 to 0.068, which is as much as native and reference differ. The threshold is 0.08, with that reason in `status.json`. |

This box had no rsvg-convert. librsvg 2.60 and its libraries were
unpacked from Debian trixie, with DejaVu and Liberation fonts. Text
here antialiases slightly lighter than the references, so text-heavy
examples score 0.005 to 0.01 higher than where they were recorded. At
the parent commit, five recorded passes exceed their threshold here
for that reason alone (scatter-table layer_text_heatmap 0.0385 and
rect_heatmap 0.0312; layered/layer_text_heatmap;
distributions/boxplot_preaggregated and histogram_nonlinear). The full
gallery was re-scored before and after this change on the same box:
no verdict changed outside the group. Seven examples outside it scored
better through the ticks-off and spacing fixes (bar_negative 0.0112 to
0.0065, its canvas now exact; bar_heatlane 0.1927 to 0.1737;
concat_layer_voyager_result 0.0723 to 0.0662). One partial,
multiview/trellis_anscombe, moved from 0.0214 to 0.0221, and its canvas
went from 2 px too wide to 4 px too narrow.

Efficiency (`test/vl-examples/scatter-table/bench.json`, the bench verb
byte-compiled, means of 20 runs). The five stages summed over the 21
examples that rendered before took 3362 ms at the parent commit and
1613 ms after this change (-52%; -54% with the run order reversed):

| example | before ms | after ms | what |
|---|---:|---:|---|
| rect_heatmap_weather | 842 | 143 | time units: a zone's UTC offset cached per 15-minute bucket (exact; checked against `decode-time` at 126k instants in 7 zones), `encode-time` memoized by fields, unit strings parsed once |
| circle_github_punchcard | 545 | 75 | same |
| rect_lasagna | 483 | 125 | same |
| point_invalid_color | 332 | 230 | SVG: symbol paths built once, not twice; numbers and escapes without regexps; attribute names interned once |
| circle | 73 | 43 | same; circles no longer copy their unit per row |

`svg-print` stays: a hand-rolled printer produced identical bytes but
was slower (84 ms against 51 ms on windvector's 2016 wedges). The new
windvector chart costs 715 ms over all five stages. Most of that is
`format "%.2f"` per vertex and GC.

Customizability. Each documented axis, legend, title, config.view and
mark property was set to a non-default value on a small chart, and the
native SVG was compared with the default one. The properties that
changed nothing, and were not cheap to honor, are now reported by
`check` as UNSUPPORTED_FEATURE with their path and `ignored: true`;
the chart still renders natively (`eas-spec-props.el`). Newly honored:
legend objects override config.legend for their own legend (labels,
symbols, title and padding), axis label and title font, font style and
opacity, tick and domain opacity, `domainDash` and caps, axis
`titlePadding` on left and bottom axes, title `anchor`, `dx`, `dy`,
`font` and `fontStyle`, the `config.view` frame's `strokeWidth`,
`strokeDash` and `strokeOpacity`, text-mark `font`, `fontStyle` and
`angle`, and rect `strokeWidth`. Four customization specs
(`custom/custom_{point,rect,tick,text}.vl.json`) use only honored
properties. bin/chart is not in this box, so their references are
built by `eas-vl-custom-build-refs` (and the ERT oracle test) wherever
it is installed. Until then that test reports itself skipped.

## 12. Layered gallery polish (fc-qx1.44)

Box without rsvg-convert or bin/chart: images were compared through a
`rsvg-convert` stand-in over resvg 0.48 with Liberation Sans (Arial's
metrics), and Vega's own geometry read from vega-lite 6.4.1 + vega 6 under
node (SVG, scenegraph bounds). The stand-in reads a few thousandths
above librsvg on text-heavy charts (concat_layer_voyager_result: 0.0728
here, 0.0689 recorded), so the recorded ratios of the two new passes are
upper bounds. Every gallery group was re-scored before and after: the
changes moved no other example up by more than 0.0007, and
bar/bar_negative_horizontal_label went from 0.0558 to 0.0105.

layered: 19 pass, 0 partial (was 17 and 2).

| example | was | now | what was wrong (all shared code) |
|---|---|---|---|
| concat_layer_voyager_result | 0.0689, canvas +3 -9 px | 0.0273, canvas exact | multi-line text marks were centred on the anchor (Vega draws line 1 on it, the rest below, and bounds them all); labels of an axis with `ticks: false` sat past the tick length; a stroked view frame (`"transparent"` too) grows a concat cell's group bounds by half the stroke all round, which `gridLayout` places by; a cell title starts from the content's exact edges |
| wheat_wages | 0.3358, canvas +21 +15 px | 0.0137 with its ref_defect, canvas exact | `config.axis.title: null` lost to the title joined from the layers' fields; `fontStyle` and `font` were dropped; a row missing the fill field is Vega's `undefined` category, first in the domain; a binned first layer left the x scale un-niced, where Vega-Lite merges nice from the next layer; the wheat bars are Vega-Lite's defect (below) |

wheat_wages' reference has no wheat bars: under bin/chart's
`config.bar.cornerRadiusEnd`, vega-lite 6.4.1 compiles the ranged binned
bar into a group whose y reads `min_wheat_start`/`max_wheat_end`, fields
no transform makes, so every bar is NaN. Its status.json `ref_defect`
records that and the oracle compares with that layer transparent
(`eas-vl-gallery-defect.el`).

Customization (`test/vl-examples/layered/custom/`, six specs: bar with
text labels, line + point + mean rule, area band, candlestick, text
heatmap, ranged dot over a multi-line note) checked against Vega's
render turned up more shared bugs, now fixed: a title object's `anchor`
was ignored (only `config.title.anchor` was read); axis formats such as
`"$.0f"` or `".0f"` went through the `,.Nf` path (no symbol, always
grouped); a padded linear domain was niced before padding, where Vega
pads then nices; rect marks ignored `cornerRadius`; corner legends
ignored their own `offset`. Newly honored: axis `zindex`, mark `limit`,
`fontStyle`, `font`, `config.scale.barBandPaddingInner` and
`tickBandPaddingInner`. `check` now names every documented property
still ignored (`eas-spec-props.el`; gallery-coverage.md lists them).

Latency, `scripts/eas-gallery-bench.sh layered` (byte-compiled, mean of
7, ms; `test/vl-examples/layered/bench.json`). Dates in a named zone
were the cost: `decode-time` with a zone string loads the zone's rules
each call (0.060 ms) where the local zone takes 0.0013 ms, so
`eas-compile` makes the chart's zone Emacs's local one for its duration
(`eas-time-with-local-zone`; TZ is restored on exit, conversions outside
it are unchanged). `eas-expr-evaluate` also stopped allocating a hash
table per call for `random()`.

| example | rows | compile-svg was | now | compile-text was | now |
|---|---:|---:|---:|---:|---:|
| layer_dual_axis | 2922 | 413 | 83 | 406 | 70 |
| layer_line_co2_concentration | 741 | 405 | 118 | 359 | 118 |
| layer_precipitation_mean | 1461 | 188 | 30 | 194 | 27 |
| layer_line_rolling_mean_point_raw | 1461 | 156 | 134 | 119 | 127 |
| layer_falkensee | 38 | 75 | 9 | 42 | 10 |
| layer_histogram_global_mean | 3201 | 47 | 34 | 35 | 36 |
| layer_line_mean_point_raw | 560 | 28 | 12 | 24 | 8 |
| all 19 | | 1429 | 538 | | |

The rest compile in under 25 ms. What remains in co2 is the expression
interpreter (four calculate transforms over 741 rows) and in rolling mean
the window frames (31 rows each) and 1462 point items.

## 12. Interactive gallery latency (fc-qx1.46)

`scripts/eas-gallery-bench.sh interactive` runs the bench verb over every
example of the group, byte-compiled, and writes
`test/vl-examples/interactive/bench.json` (mean and max ms per stage, 5
runs).  Summed over the 30 examples that rendered before and after
(Emacs 30.1, batch, one box; selection_type_point_zorder is new):

| stage | before | after | |
|-------|-------:|------:|--|
| compile-svg | 2954 ms | 1126 ms | -62% |
| render-svg | 458 ms | 296 ms | -35% |
| compile-text | 2893 ms | 1033 ms | -64% |
| render-text | 225 ms | 207 ms | -8% |

What it took, measured with ELP before each change:

- Local calendar fields cost a `decode-time` (and the way back an
  `encode-time`) per value, and a named zone makes each call switch the
  process zone.  eas-time-offset.el remembers a zone's UTC offset per
  15-minute bucket and does the calendar arithmetic in UTC, falling back to
  the system calls within three hours of a transition; `eas-time-format`
  formats with the remembered fixed offset unless the format prints the
  zone.  Checked against `decode-time`/`encode-time` on 320,000 random
  instants in eight zones (Lord Howe's half-hour DST, Apia's skipped day)
  and against `format-time-string` on 80,000 in four: no difference
  (eas-time-offset-test.el keeps a smaller sample).  interactive_bin_extent compile went
  634 -> 117 ms, interactive_layered_crossfilter 399 -> 89 ms.
- `eas-expr-evaluate` made a hash table per row for `random()`'s call
  counter; it is now made by the first `random()` call only.
- Repeat substitution walked the inlined data rows (110k calls per
  interactive_splom compile, and a row with a "repeat" field would have
  been rewritten); data is now left alone.
- `eas-svg--n` trims "%.2f" without a regexp (identical strings).

The heaviest examples now: interactive_multi_line_tooltip (167 ms svg
compile+render: 4400 tooltips formatting dates), interactive_splom
(136 ms: 10,752 points), interactive_bin_extent (123 ms: a `calculate`
expression over 10,000 rows, interpreted per row).

## 12. Vega-Lite gallery: multiview, second pass (fc-qx1.45)

Multiview went from 2 pass / 9 partial / 8 unsupported to 19 / 0 / 0 (the
11 topojson maps stay out of scope).  The box had no librsvg, so the image
oracle ran through a stand-in (resvg with Arimo for Arial); it scores the
committed passing examples within about 0.01 of their recorded ratios,
never better.  What the group needed, all shared code:

| feature | where |
|---|---|
| every facet form (row, column, row x column, wrapped `facet` + `columns`, top-level `facet`) lowered to a vconcat of hconcat cells; levels sorted as Vega-Lite does (null first, by an aggregate, data order when the cells aggregate the sort field away), rows with invalid x/y dropped first | `eas-facet-grid.el` |
| Vega's trellis inside the shared concat layout: cells' x/y scales and bins shared, spacing between cell contents (align "all", bounds "full"; align "none" along an independent scale), axes only on the outer cells (a wrapped column's axis moved under the last row), row/column labels and bold field titles as text marks | `eas-facet-layout.el` |
| `repeat` with `columns`, row x column repeats kept as a grid placed by Vega's gridLayout (spacing honoured) | `eas-vl-lower.el`, `eas-compile-grid.el` |
| `bounds: "flush"` (a nested concat counts 0x0, as in Vega) | `eas-concat-flush.el` |
| titles of nested concats | `eas-concat-title.el` |
| quantize, quantile and threshold scales; bucket legends ("< t1", "t1 – t2", "≥ tn"), merged with a same-field size legend, or Vega's discrete gradient for color alone | `eas-scale-discretize.el`, `eas-legend.el` |
| shared legends with orient top or bottom, in a row (titleOrient left), side by side; config.legend orient/direction; horizontal gradients clamp(width, 100, 200) | `eas-legend-row.el`, `eas-compile-shared.el` |
| albersUsa (d3's composite, exact to 1e-15 against d3-geo), albers, mercator, equirectangular, equalEarth for longitude/latitude points, fitted to the view | `eas-geo.el` |
| `ticks: false` drops the tick length from the label offset; `sort: "descending"` reverses a continuous scale; a stacked measure keeps `title: null`; `config.countTitle`; axis `titleLimit` | layout, scales, marks, encode |
| no plot fitted so small that its axis labels collide (the canvas grows instead) | `eas-place-fit`, grid and facet fits |
| `check` reports the facet properties the trellis ignores (`header.labelOrient`, `facet.align`, ...) with their paths | `eas-spec-feature-functions` |

Export keeps facets, projections and nested titles as Vega-Lite wrote
them (the lowerings skip while resolving), so `export --vl` still goes to
bin/chart unchanged.

Efficiency (`bin/eas bench SPEC --n 5`, byte-compiled, mean ms; all
examples in `test/vl-examples/multiview/bench.json`):

| example | rows | compile-svg before -> after | render-svg | hover |
|---|---:|---|---:|---:|
| trellis_area_seattle (24 cells) | 8759 | (unsupported; the first native cut took 6366) -> 952 | 111 | 9.7 |
| vconcat_weather | 2922 | 380 -> 67 | 6.7 | 0.34 |
| area_density_facet | 344 | 280 -> 123 | 14 | 0.53 |
| trellis_area | 560 | 117 -> 50 | 9.3 | 1.5 |
| geo_circle | 42049 | (unsupported) -> 2569 | 2030 | 73 |

Three cuts made the difference: a facet's shared transforms run once and
its rows are partitioned among the cells in one pass (they keep their
source index, so drill-to-row still works) instead of 24 filters over all
rows; parse, check and compile share one lowering; and calendar fields in
a named zone use an offset cached per quarter hour (exact: transitions
fall on quarter hours, and a bucket holding one decodes directly; 81 600
random instants in three zones matched `decode-time`) with encoded local
fields memoized, which took `decode-time`/`encode-time` from 80% of a
temporal compile to nothing.  geo_circle's 42k circles stay heavy: one
SVG element each.

## 13. Integrated verification (fc-qx1.48)

The first run of the engine with all eleven polish branches integrated
(515bfd3), on one 4-core Linux box, Emacs 30.1, rsvg-convert 2.62.4
(cairo 1.18.6, conda-forge) and Liberation Sans 2.1.5; bin/chart absent,
so the oracle compared against the committed references.

| run | before fixes | after |
|---|---|---|
| `make test` | 710 tests: 682 pass, 25 fail, 3 skip; 72 s | 710: 708 pass, 0 fail, 2 skip (bin/chart absent); 89 s |
| `make -j4 test-gallery` | 7 of 10 targets fail | 10 of 10 pass (19 tests, 0 skip); 6 min 17 s wall |
| `make compile` | clean | clean |
| `make bench` | | budget pass: 36 stages, 0 violations; hover 0.98 ms at 10k points (target 50) |

Rasterizer drift.  Re-measuring every passing gallery example against
its recorded ratio: median +0.0024, 39 of 188 up by more than 0.005,
9 down by more.  Diffing the PNGs (differing pixels painted) shows glyph
edges only for every one checked, including `line_bump`, recorded at
0.0000: this cairo/freetype draws text a little differently from the box
that recorded the ratios.  Hinting settings made no difference.  Five
examples crossed their threshold that way and carry a written note
(gallery-coverage.md); none moved geometry.

Two genuine regressions hid among them (ratio 0.87 and 0.096: a
projected view painted with `config.view.fill`, and quantile legend
labels reformatted from their lower bounds); the per-example sweep of
fresh ratio against recorded ratio is what told them apart from drift.

# Fast live text frames (eas-b2s.1)

What a live text frame costs, where the time goes, what 0.2.4 changed in
the text renderer and the terminal glue, and what is left. Scope: the
text side only (`src/eas-text*.el`, `src/eas-render-cache.el`,
`src/eas-mode-patch.el`, `src/eas-mode-strip.el`, the text branch of
`eas-mode-redraw`). The update path (compile, scales, reducers) and the
SVG path belong to eas-b2s.3 and eas-b2s.2.

Box: Linux 6.8, 8 vCPU, GNU Emacs 30.1 with native compilation, tmux
3.x, no GUI. Every number is ms per frame on a 100x40 canvas unless
stated, from `scripts/bench-frame-text.el` (batch) or
`scripts/bench-frame-tty.el` (`emacs -nw` in tmux, with redisplay).

## Workloads

| workload | what a frame is |
|---|---|
| order-book ladder push | keyed push of 10 of 50 levels to a bar+text ladder |
| depth-live push | keyed push of all 50 levels to a stepped two-sided depth area (fixed price window) |
| clock tick | `eas-play-tick` of templates/vega/clock |
| pacman tick | `eas-play-tick` of templates/vega/pacman |
| pi-monte-carlo step | the sample-size slider moved by 40 points |
| airport-connections hover | a pointermove sweeping the map |

The order book is synthetic (financial-charts.el is not a dependency)
but has the shape of the owner's ladder + depth view. A frame is the
update plus the redraw the terminal glue does: `eas-mode-redraw`
(render, values strip, buffer patch, readout). The bench splits it into
update, render (`eas-text-render-lines`) and patch (the rest).

## Finding 1: garbage collection is most of a frame

Batch Emacs starts with `gc-cons-percentage` 1.0; an interactive one
uses 0.1. At 0.1 a collection runs about every 0.8 MB allocated, and on
this heap each takes about 20 ms: **every KB a frame allocates costs
about 25 µs**. Profiling `emacs -nw` on the clock showed 65% of frame
time in "Automatic GC". A batch bench that keeps 1.0 sees almost none of
it, which is why 0.2.2's numbers looked better than the terminal felt.
The bench now binds `gc-cons-percentage` to 0.1 and reports the
collection time and the KB allocated per frame. Allocation, not
instruction count, is the budget to spend.

## Finding 2: in a terminal, redisplay is the other half

`scripts/bench-frame-tty.el` times update + redraw + `(redisplay t)` in
a real `emacs -nw` (110x45, xterm-256color, byte-compiled, async native
compilation off). Before this work a ladder push took 51.8 ms there,
against 42.8 ms in the batch bench at the interactive GC policy: about
9 ms of redisplay on top. Redisplay lays out every line that changed,
so writing only the cells that changed also keeps it small. (With async
native compilation on, as in a fresh interactive session, it competes
with the frames; the bench turns it off.)

## What the old frame did (byte-compiled, profiled)

Per frame, before (0.2.3):

- **Patch, 3 to 7 ms.** `eas-mode-patch-text` read every buffer cell back
  (`char-after`, `text-properties-at`, plist set comparison): 4000 cells,
  80 000 comparisons per frame, whether or not a line changed. The
  rendered text was joined with newlines and split again on the way.
- **Strip, 3 to 4 times per frame.** The values strip was computed by
  the redraw and again by the readout (0.5 ms each on pacman).
- **Snapshot copy, 290 KB.** A frame restarting from a grid snapshot
  copied 9 vectors of 4000 cells; the fresh grid it allocated first was
  thrown away (another 290 KB).
- **Row keys, 220 KB.** Deciding which rows to recompose took 7
  substrings of every row.
- **Compose, 20 KB per changed row on a map.** A propertized string per
  run, then `concat`.
- **Ink lookups** parsed the color (`eas-color-hex`) before looking in
  the memo: 14% of a clock render.
- **Axes** re-scanned the whole grid once per grid line to dot it: the
  depth chart's axes step (13 grid lines) took 5.8 ms.
- **Bars and areas** did float arithmetic for every interior cell and
  sent every area cell through the general band resolver.

## What changed

| change | file | effect |
|---|---|---|
| retained-line cell diff: patch against the lines written last, skip `eq` rows, diff changed lines run by run on strings; buffer diff only when the buffer's chars changed under it | eas-mode-patch.el | patch 3–7 ms → 0.3–1.6 ms |
| `eas-text-render-lines`: rows as a list, a reused row is the same string | eas-text.el, eas-mode.el | patch skips it with `eq`; no join/split |
| strip memoized on (scene plan state size target) by `eq`; the readout updates it through the model | eas-mode-strip.el | 1 strip per frame |
| grid allocated only when no snapshot is reused | eas-text.el, eas-render-cache.el | −290 KB |
| snapshot restored in place over the state of two renders ago (double buffer) | eas-render-cache.el | −290 KB; a frame allocates no grid |
| rows compared in place with the last composed grid, no keys | eas-render-cache.el, eas-text.el | −220 KB |
| one string per composed row, run props set on it in `concat`'s order | eas-text.el | output identical, property order included |
| legible-color memo per (background . ink), looked up before parsing | eas-text-ink.el | ink lookups off the profile |
| grid lines dotted in their own cells only | eas-text.el | depth axes step 5.8 → 2.1 ms |
| bars: full cells skip end arithmetic, cells put inline | eas-text.el | fewer floats per bar cell |
| areas: a cell its newest slice covers whole skips the band resolver | eas-text.el | band resolver off most area cells |
| line/area props looked up per datum, not per braille column | eas-text.el | fewer plists |

Every change keeps the output identical. Checked three ways: the text
goldens and gallery (unchanged files, same pass/fail as the base), a
sweep rendering every conformance spec and template example in the base
and in this tree and comparing the printed strings with their
properties, and `src/eas-text-frames-test.el`, which runs random push,
tick and hover sequences and checks that every frame's lines equal an
uncached render and that the patched buffer equals the frame inserted
afresh.

## Results

Same machine, same run: the base commit (0.2.3, 85d350e) in a git
worktree, then this tree, each copied and
compiled by `scripts/bench-frame-text.el` (`-- byte ROOT 40` and
`-- native ROOT 40`), 3 rounds of 40 frames, interactive GC policy.
Stages exclude garbage collection, which has its own column (number of
collections in parentheses). pi-monte-carlo runs 3x6 frames.

**Byte-compiled**, ms per frame:

| workload | before total | after total | before text (render+patch) | after text | before GC | after GC | KB/frame before → after |
|---|---|---|---|---|---|---|---|
| order-book ladder push | 42.83 | **15.30** | 7.61 | 4.01 | 33.38 | 9.39 | 1435 → 358 |
| depth-live push | 81.64 | **30.08** | 10.56 | 7.08 | 68.96 | 20.76 | 2768 → 904 |
| clock tick | 37.74 | **10.03** | 4.91 | 2.89 | 31.76 | 6.12 | 1262 → 240 |
| pacman tick | 71.91 | **28.64** | 11.43 | 6.14 | 57.41 | 19.50 | 2130 → 686 |
| pi-monte-carlo step | 1267.74 | 1195.82 | 23.07 | 11.68 | 910.92 | 848.93 | 44097 → 41923 |
| airport-connections hover | 68.98 | **32.38** | 10.68 | 6.37 | 47.53 | 15.42 | 4928 → 3263 |

**Native-compiled**, ms per frame:

| workload | before total | after total | before text (render+patch) | after text | before GC | after GC | KB/frame before → after |
|---|---|---|---|---|---|---|---|
| order-book ladder push | 39.46 | **12.42** | 4.30 | 2.08 | 33.73 | 9.02 | 1435 → 358 |
| depth-live push | 84.39 | **27.14** | 8.04 | 4.57 | 74.77 | 20.97 | 2768 → 904 |
| clock tick | 36.79 | **7.87** | 3.10 | 1.47 | 32.82 | 5.67 | 1262 → 240 |
| pacman tick | 64.54 | **24.27** | 7.08 | 3.87 | 55.41 | 18.31 | 2130 → 686 |
| pi-monte-carlo step | 1118.42 | 1035.12 | 19.10 | 8.34 | 888.04 | 815.61 | 44097 → 41923 |
| airport-connections hover | 66.22 | **28.66** | 7.12 | 4.05 | 50.95 | 15.91 | 4928 → 3263 |

The update stage (compile, reducers) is unchanged, as it should be: it
is eas-b2s.3's. pi-monte-carlo is all update (its slider recompiles
thousands of points, 330 ms and 41 MB a step); the text side halves
but cannot move its total.

**In a real terminal** (`scripts/bench-frame-tty.el`, `emacs -nw`
110x45 in `tmux -L eas`, byte-compiled, update + redraw + `redisplay`,
40 frames; the buffer equalled a render from scratch after every
workload, before and after):

| workload | before | after |
|---|---|---|
| order-book ladder push | 51.79 | **18.54** |
| depth-live push | 100.13 | **39.90** |
| clock tick | 48.89 | **16.60** |
| pacman tick | 76.63 | **35.95** |
| airport-connections hover | 79.61 | **43.97** |

At the owner's 4 pushes a second, a ladder plus depth view went from
about 61% of a core ((51.8 + 100.1) x 4 ms a second) to about 23%
((18.5 + 39.9) x 4).

## What is left, per stage (after)

Byte-compiled, after, ms per frame on a 100x40 canvas (measured
above; render is paint plus compose):

| stage | ladder | clock | depth | pacman | airport hover |
|---|---|---|---|---|---|
| update (eas-b2s.3's) | 1.90 | 1.02 | 2.24 | 3.00 | 10.59 |
| render | 3.62 | 2.47 | 6.02 | 5.28 | 4.55 |
| patch (diff, strip, readout) | 0.39 | 0.42 | 1.06 | 0.86 | 1.82 |
| GC | 9.39 | 6.12 | 20.76 | 19.50 | 15.42 |
| total | 15.30 | 10.03 | 30.08 | 28.64 | 32.38 |

Measured separately with the update's code unchanged, the update
allocates about 100 KB (clock) to 250 KB (depth) a frame. The text side
therefore still allocates roughly 140 KB (clock) to 650 KB (depth):
item props, braille dot floats, area slices, pacman's tile records and
the strings of changed rows. GC is still the largest line, so the next
phase is about allocation, not instructions.

## Phased plan for the rest

1. **Allocation-free paint (text).** What still allocates in my scope:
   item props (a plist and a face list per item per frame), braille
   dots (two boxed floats per dot), area slices (a list per cell), tile
   records (pacman: about 440 KB, a vector per cell plus props), and
   composed strings for changed rows. Interning props per (view mark
   datum color tooltip) and an integer Bresenham in dot space would take
   most of it. Target: under 100 KB per frame, about 2.5 ms of GC.
2. **Row-granular dirtiness.** The paint restarts at the first changed
   step and repaints everything after it; rows are compared afterwards.
   Recording the rows each step touched (one bool-vector per step, set
   by `eas-text--put` and `eas-text--dot`) would let a step that changed
   repaint only its own rows, and compose skip the comparison. Needs
   every write site to go through the two functions first (tiles, bands
   and arcs write the vectors directly today).
3. **Strip off the frame.** The values strip (eas-strip.el) is computed
   once per frame now, but on pacman it is 12% of a frame. It can be
   computed lazily when the readout is visible, or from the scene's
   index the hover already built.
4. **Native compilation** helps the elisp loops (row compare, restore,
   patch) the most; it does nothing for GC. It is not worth a dynamic
   module: with allocation gone, the remaining text-side cost is
   1–4 ms per frame.
5. **Frame pacing.** A redraw is already idle-coalesced and hidden views
   idle. With GC under control, the next limit in a terminal is
   redisplay of the changed lines; `bench-frame-tty.el` is the gauge.
   The text side should not write a line whose cells did not change
   (done) and should keep `face` values `eq` across frames so that
   redisplay's face cache hits (phase 1 does this).

## Phase 2 (eas-b2s.5): allocation and the GC policy

Same box. Base is phase 1 as it landed with the readout (a8de075), in a
git worktree; both trees benched in the same run. Since phase 1 the
fixed readout line landed, so pacman's base frame is dearer than in
the tables above (its strip is formatted through components).

### What changed

| change | file | effect |
|---|---|---|
| item props interned per (ink, view, mark, datum, color, tooltip, ink function): a lookup with a reused key, no plist, face or tooltip string per item | eas-text.el | equal props are `eq` across items and frames |
| label clash test as plain loops (it ran a closure per span per text item, in the update) | eas-text-labels.el | ladder update −12 KB |
| tile records pooled, the tile table reused; tile-derived props (fill face, hover) shared per interned props | eas-text-tile.el | pacman render −170 KB |
| area slices built from reused conses, freed when the mark resolves; the resolve's neighbour tests without closures | eas-text.el | depth render −240 KB with the compose change |
| row compose: chars through reused vectors, runs set as they end (no run list), reversed plists memoized per interned props | eas-text.el | a row allocates its string and intervals only |
| arc dots memoized per (item, cell size) as a packed vector | eas-text-arc.el | clock render 198 → 53 KB |
| strip memoized per view: the second ask in a frame is the same string; a frame whose strip context (fields, env, theme) is `equal` reuses it | eas-mode-strip.el | one strip per change, not two per frame |
| live text redraws call `eas-gc-defer`, as events already did | eas-mode.el | see below |

Output is unchanged: goldens untouched, `make test` passes, and
`src/eas-text-frames-test.el` now also renders the "full" frame with
every memo and pool fresh (props, reversed plists, arcs, tile records,
slices), and adds random pacman ticks, depth pushes, a strip-memo check
and an arc-memo check.

### Allocation per frame (byte-compiled, KB, batch)

| workload | update before → after | render before → after | patch before → after |
|---|---|---|---|
| order-book ladder push | 221 → 209 | 120 → 77 | 53 → 29 |
| depth-live push | 279 → 279 | 566 → 327 | 101 → 84 |
| clock tick | 107 → 104 | 228 → 53 | 72 → 45 |
| pacman tick | 291 → 290 | 333 → 164 | 445 → 228 |
| airport-connections hover | 3608 → 3608 | 503 → 332 | 188 → 150 |

The text side (render + patch) is now 98 KB (clock) to 411 KB (depth);
the update (eas-b2s.3's) allocates 100 KB to 3.6 MB a frame and is
now the larger share on every workload. Under 100 KB a frame for the
text side holds for the clock only: depth composes most of its rows
every push (its area moves everywhere), and pacman's strip changes
every tick.

### Batch frames (interactive GC policy, gc-cons-percentage 0.1)

**Byte-compiled**, ms per frame (GC with collections in parentheses):

| workload | total before → after | render+patch before → after | GC before → after | KB/frame before → after |
|---|---|---|---|---|
| order-book ladder push | 18.01 → **13.13** | 4.73 → 3.92 | 11.35 (60) → 7.55 (45) | 382 → 301 |
| depth-live push | 32.76 → **26.72** | 7.59 → 7.55 | 23.02 (129) → 17.06 (94) | 932 → 677 |
| clock tick | 13.91 → **10.50** | 4.03 → 3.55 | 8.71 (42) → 5.78 (28) | 289 → 189 |
| pacman tick | 44.75 → **29.62** | 9.06 → 8.23 | 32.64 (153) → 18.11 (78) | 1069 → 609 |
| pi-monte-carlo step | 1184.77 → **1132.59** | 12.62 → 11.83 | 873.14 (563) → 812.30 (499) | 42054 → 41441 |
| airport-connections hover | 33.04 → **28.34** | 6.99 → 6.55 | 16.34 (43) → 12.32 (31) | 3311 → 3106 |

**Native-compiled**, ms per frame (GC with collections in parentheses):

| workload | total before → after | render+patch before → after | GC before → after | KB/frame before → after |
|---|---|---|---|---|
| order-book ladder push | 13.57 → **11.45** | 2.36 → 2.09 | 9.96 (60) → 8.10 (45) | 382 → 301 |
| depth-live push | 30.65 → **24.52** | 4.87 → 4.95 | 24.19 (126) → 17.95 (92) | 932 → 677 |
| clock tick | 11.39 → **7.87** | 2.15 → 1.72 | 8.38 (39) → 5.55 (26) | 289 → 189 |
| pacman tick | 34.34 → **22.09** | 5.07 → 4.46 | 27.36 (140) → 15.63 (72) | 1069 → 609 |
| airport-connections hover | 24.48 → **22.26** | 3.83 → 3.85 | 13.80 (43) → 11.21 (31) | 3307 → 3102 |

With the threshold eas-gc sets (`gc-cons-threshold` 64 MB in the batch Emacs, byte-compiled, same run), ms per frame:

| workload | base total | now total | base render+patch | now render+patch |
|---|---|---|---|---|
| order-book ladder push | 5.98 | 5.80 | 4.25 | 4.00 |
| depth-live push | 9.91 | 10.80 | 7.50 | 8.13 |
| clock tick | 4.87 | 4.59 | 3.80 | 3.50 |
| pacman tick | 11.41 | 10.50 | 8.18 | 7.11 |
| airport-connections hover | 16.73 | 16.46 | 6.30 | 5.96 |

Without collections the two trees cost the same instructions within noise (depth's render is 0.7 ms dearer, the price of freeing its slices); the batch gain above is fewer collections, and in a terminal most of it comes from the policy.

### In a real terminal

`scripts/bench-frame-tty.el`, `emacs -nw` 110x45 in `tmux -L eas`,
byte-compiled, update + redraw + `redisplay`, 40 frames; the buffer
equalled a render from scratch after every workload in every run.

| workload | base | allocation only (`eas-gc-cons-threshold` nil) | now (deferred GC) | base / now |
|---|---|---|---|---|
| order-book ladder push | 21.89 | 18.71 | **9.15** | 2.4x |
| depth-live push | 43.82 | 39.15 | **15.72** | 2.8x |
| clock tick | 22.40 | 13.91 | **8.51** | 2.6x |
| pacman tick | 50.93 | 34.77 | **16.09** | 3.2x |
| airport-connections hover | 43.05 | 36.39 | **26.05** | 1.7x |

### Why the GC policy

A collection traces the live heap, so its cost does not depend on how
much garbage it frees: fewer collections is cheaper, whatever the
frame allocates. eas-gc.el (fc-qx1.9) already raises
`gc-cons-threshold` to `eas-gc-cons-threshold` (64 MB) on the first
event of an interaction and collects once Emacs has been idle a second.
Live frames, a push or a tick arriving from a process or a timer, never
went through it: at 4 pushes a second the default threshold collected
every 0.8 MB, every other frame. A text redraw now calls
`eas-gc-defer` too. Batch Emacs is left alone (so the batch table
above still shows collections), and `eas-gc-cons-threshold` nil turns
it off. Frames that arrive while Emacs stays idle keep the raised
threshold, so collection happens about every 64 MB (a couple of
hundred frames), each one tracing the same live heap as before.

### Frame pacing

A redraw is already coalesced: `eas-mode--schedule` arms one idle
timer per buffer and pushes that arrive before it fires change the
view only, so a burst draws once, after input and redisplay. Hidden
views do not draw (eas-live). Nothing was added.

### Not done, and why

- **Row-granular dirtiness** (paint only the rows a step touched).
  Every write site (tiles, bands, arcs, the inline bar loop) would
  have to record its rows first. With allocation down, paint is 2–6 ms
  of instructions; it is the next step for the ladder and depth.
- **Writing the diff straight into the buffer**: the patch
  (eas-mode-patch.el) is the SVG box's file this round; it already
  writes only changed runs of changed rows (0.3–1.2 ms).
- **Strip**: pacman's strip reads values that change every tick;
  eas-strip.el and the component formatter (not in this scope) are
  most of what remains of its patch stage.

## Phase 3 (eas-b2s.7): renderers consume dirty marks

The update path now keeps every unchanged mark (and, in a changed
mark, every unchanged item) `eq` to the last frame's, and lists the
marks it changed (`eas-view-dirty`, docs/design/update-path.md). This
phase makes both renderers use that.

### Text: row-granular repaint

Before, a frame restarted from a grid snapshot taken before the first
changed step and repainted *every* row from there, then compared every
row with the last grid to find which to compose again. A ladder push
that moved 10 bars repainted all 50 bars and their labels.

Now (`eas-text--frame`, `src/eas-text.el`):

1. **Every write site records the rows it tries** (`eas-text--touch`):
   `eas-text--put`, `eas-text--dot`, the inline bar loop, the brush,
   area slices (`eas-text--under`, `eas-text--resolve-bands`), arc dots,
   tile fills and edges (`eas-text-tile.el`) and the axis grid dotting.
   A row is recorded whether or not the write wins its cell, so a later
   frame knows every step that has a say in it. Per step that is a
   bool-vector; per mark item a packed row range.
2. **The last frame is retained per canvas**: the grid itself (no copy
   per frame), the steps' rows, each mark's items and their ranges, the
   composed row strings and a grid snapshot before the first step that
   changes (moved there after two frames in a row change from a later
   step).
3. **A frame whose steps differ only in mark items** (axes, legends,
   titles, the step list and each mark's other keys unchanged; the
   hit-test's `:rows` and `:index` are ignored, text never reads them)
   computes the dirty rows: the rows each changed item (not `eq` to
   last frame's at its index) held, plus the rows it will hold, guessed
   from its geometry (`eas-text--item-rows`: bars and tiles by the same
   cell rounding the painter uses, rules, text, series points, symbols).
   Area marks widen by a row each way (a cell's slices read their
   neighbours). Those rows are restored from the snapshot (or cleared)
   and the steps from the snapshot on that try them run with writes
   limited to them (`eas-text--mask`); within a step, an item `eq` to
   last frame's whose rows miss the mask is not drawn at all.
4. **Exactness does not depend on the guess.** After the pass, rows the
   changed items actually tried that were not repainted (the guess was
   short, or unknown) are repainted by a second pass. Labels whose
   overlap a full paint decided replay that decision
   (`eas-text--label-replay`), since a partial repaint sees none of the
   claims of labels outside its rows.
5. **Compose skips comparison**: only repainted rows are composed; the
   others are the last frame's strings (`eq`, which the buffer patch
   skips). Nothing compares grids any more (`eas-text--same-row-p`,
   `eas-render-cache-grid-rows` and the row-key cache are gone).
6. **Braille lines with constant props** (rules, ticks, diagonals) no
   longer compute a float per dot for a props function that ignores it.

`eas-render-cache-dirty`, bound by `eas-mode-redraw` to the view's
dirty marks, is a hint: a listed mark is taken as changed without the
`equal` walk; any other is compared, `eq` first. A wrong or stale hint
(coalesced frames) costs time, never output.

### SVG: no comparison for listed marks

`eas-svg-retain-mark` and the retained hot-spot areas take the same
hint: a mark the update path listed is reprinted from its items
(unchanged items still reuse their text by position, one `eq` each)
without first walking it against the slot's mark; any other mark hits
on `eq`. Phase 2 measured that this walk was the only comparison left,
so this saves little time but removes the last per-mark `equal`.

### Tests

`src/eas-text-frames-test.el` checks every frame against a render from
scratch with the true hint, every mark listed and no hint; adds a run
with the row guess disabled and one with a wrong guess (the second pass
must carry exactness), and a test that a one-level push composes at
most 4 rows. `eas-svg-retain-random-frames-equal-fresh-renders` renders
with the three hints too.

### Before and after

Same box and run, base 6d49ffe in a git worktree,
`emacs -Q --batch -l scripts/bench-frame-text.el -- MODE ROOT 30`, 3
rounds of 30 frames, interactive GC policy. The bench now splits the
KB allocated per frame into update, render and patch. Render is
`eas-text-render-lines`; "text KB" is render plus patch (the glue:
strip, buffer patch, readout).

| workload | mode | render ms before | after | total ms before | after | render KB before | after | text KB before | after |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|
| order-book ladder push | byte | 3.15 | **1.07** | 10.62 | 7.21 | 65 | 33 | 94 | **62** |
| depth-live push | byte | 5.66 | 4.25 | 21.86 | 17.58 | 314 | 308 | 399 | 393 |
| clock tick | byte | 2.43 | **1.21** | 7.80 | 5.96 | 40 | 46 | 85 | **91** |
| pacman tick | byte | 4.40 | **1.64** | 18.75 | 14.57 | 87 | 71 | 315 | 299 |
| airport-connections hover | byte | 4.21 | **2.79** | 13.03 | 10.65 | 241 | 180 | 390 | 329 |
| order-book ladder push | native | 1.51 | **0.63** | 7.50 | 6.56 | 65 | 33 | 94 | 62 |
| depth-live push | native | 3.13 | 2.86 | 16.74 | 16.38 | 314 | 308 | 399 | 393 |
| clock tick | native | 1.07 | **0.69** | 5.63 | 5.17 | 40 | 46 | 85 | 91 |
| pacman tick | native | 2.56 | **1.08** | 15.14 | 12.90 | 87 | 71 | 315 | 299 |
| airport-connections hover | native | 2.39 | **1.75** | 11.49 | 9.03 | 241 | 180 | 390 | 329 |

Render falls 2-3x on the ladder (10 rows of 40 repainted and composed)
and pacman (6-7 rows), 2x on the clock, a third on the airport hover.
Under 100 KB of text-side allocation per frame holds for the ladder
and the clock. It does not for two workloads, and neither is in this
render path:

- **Pacman, 299 KB**: 228 KB is the patch stage, and most of that is
  the values strip, which changes every tick: `eas-readout-context`
  (~49 KB), then `eas-component-render`/`eas-component-fit` and
  `eas-strip` (eas-component.el and eas-strip.el). Two more
  `eas-strip` calls per tick come from `eas-inspect` in the update.
  The strip is memoized per frame already; making it cheaper means
  changing the component formatter and eas-strip, out of this box.
- **Depth, 393 KB**: every push moves both cumulative areas over all
  their rows, so all 20 plot rows are dirty and repainted. Row
  granularity cannot help; the cost is the area paint's floats (cell
  coverage per column and row) and composing 20 changed rows. An
  integer coverage pass in `eas-text--series` is the next step there.

SVG (`scripts/bench-frame-svg.el -- byte SRC 20`, same run): draw is
unchanged within noise (ladder 0.46/0.47 ms, clock 0.39/0.37, pacman
1.38/1.41, airport 0.42/0.46, county 3.80/3.58); the bench calls
`eas-svg-image` without the glue's hint, and Phase 2 had already shown
the per-mark `equal` walk to be the only comparison left.

`make bench-check` passes byte and native (228 of 228 each; the text
`render/*` workloads, which render an unchanged scene repeatedly, now
reuse the retained frame and allocate 7-70% less).

## Phase 4 (eas-b2s.9): strip and readout, direct buffer diff, area arithmetic

The three items Phase 3 left: pacman's values strip, depth's area paint
and composing changed rows only to diff them against the buffer.

### What changed

| change | file | effect |
|---|---|---|
| a repainted row may stay an `eas-text-row` record (`eas-text-render-rows`); the patch reads its cells from the grid and writes the runs that differ straight into the buffer: chars by `insert-char`, one `set-text-properties` per run of `eq` props, one modification-hook call per run (`combine-change-calls`) | eas-text.el, eas-mode-patch.el, eas-mode.el | no row string, no substring per run |
| `eas-text-render-lines` composes the records, so its callers see strings as before | eas-text.el | same lines, `eq` across frames |
| eas buffers keep no undo list | eas-mode.el | a frame records no undo entries |
| the strip line the redraw just wrote is not patched again by the readout | eas-mode-patch.el | one write per frame |
| `eas-strip` memoized per (scene plan state) by `eq`: the inspect result, the redraw and the readout share one strip | eas-strip.el | 3 strips a frame → 1 |
| readout atoms make their short and shorter variants lazily (only when the fit needs them); no number string for an unformatted value; prop paths interned; spans joined once | eas-component.el, eas-component-builtins.el | pacman patch 228 → 181 KB |
| areas: a cell the slice covers whole skips the edge arithmetic, column centers from a vector, props once per column; a lone slice that meets no neighbour skips the band resolver's lists; a braille step's props once per dot column | eas-text.el | depth render+patch 393 → 169 KB |

Modification hooks still run (`eas-crosshair-point-motion-moves-the-column`
counts the cells a crosshair move writes through `after-change-functions`;
binding `inhibit-modification-hooks`, as a first cut did, hid every write
from it and from any other hook). Output is unchanged: goldens untouched,
`src/eas-text-frames-test.el` now patches two frames in three from
records and checks the buffer against a fresh render.

### Batch frames (same run, base d13d020 vs this tree, 3x40 frames, gc-cons-percentage 0.1)

The base's render column is `eas-text-render-lines` (paint and compose);
this tree's glue calls `eas-text-render-rows`, so composing moves into
the patch column. Compare render+patch.

| workload | mode | total base → now | render+patch base → now | GC base → now | KB/frame base → now |
|---|---|---|---|---|---|
| order-book ladder push | byte | 9.98 → 9.99 | 1.58 → 1.71 | 6.76 (40) → 6.72 (40) | 274 → 267 |
| depth-live push | byte | 23.03 → **18.05** | 5.96 → 5.66 | 15.11 (94) → 10.31 (60) | 675 → 452 |
| clock tick | byte | 7.86 → **6.38** | 1.87 → 2.02 | 5.20 (28) → 3.70 (22) | 191 → 149 |
| pacman tick | byte | 19.80 → **14.18** | 3.89 → 3.10 | 13.49 (65) → 9.37 (48) | 568 → 402 |
| airport-connections hover | byte | 12.99 → **10.35** | 4.64 → 4.88 | 7.31 (22) → 4.54 (14) | 437 → 281 |
| order-book ladder push | native | 8.72 → 8.91 | 0.92 → 1.11 | 6.70 (40) → 6.67 (40) | 274 → 267 |
| depth-live push | native | 24.43 → **14.81** | 4.66 → 3.26 | 18.16 (93) → 10.18 (60) | 675 → 452 |
| clock tick | native | 6.86 → **5.71** | 1.22 → 1.25 | 5.02 (26) → 3.91 (20) | 191 → 149 |
| pacman tick | native | 17.09 → **13.49** | 2.39 → 2.07 | 13.35 (61) → 10.06 (45) | 568 → 402 |
| airport-connections hover | native | 12.33 → **8.69** | 3.51 → 3.25 | 8.00 (21) → 4.71 (14) | 437 → 281 |

The win is allocation: 7 KB (ladder) to 223 KB (depth) less a frame,
hence fewer collections. Instructions are flat; the ladder's and the
clock's patch are a little dearer (cell-by-cell `insert-char` and
property reads cost more than the string diff when most of a row's
cells change).

### In a real terminal

`scripts/bench-frame-tty.el`, `emacs -nw` 110x45 in `tmux -L eas`,
byte-compiled, two runs each, alternating order (ms per frame with
redisplay; the buffer equalled a render from scratch in every run):

| workload | base (run 1, run 2) | now (run 1, run 2) |
|---|---|---|
| order-book ladder push | 6.84, 6.25 | 6.26, 6.61 |
| depth-live push | 13.89, 13.62 | 12.27, 12.93 |
| clock tick | 6.17, 6.47 | 8.21, 7.42 |
| pacman tick | 11.24, 13.15 | 10.17, 11.32 |
| airport-connections hover | 14.55, 14.06 | 14.70, 16.91 |

Depth and pacman gain about 1 ms; ladder is within noise; the clock
loses about 1 ms and the airport hover up to 2 ms, the per-cell writes
costing redisplay more than the strings they replaced. Next step there:
write a changed run as one propertized `insert` when most of its cells
change (the clock's hand and the airport's highlighted routes), and keep
the per-cell path for the sparse runs it wins on.

# The update path: pushes, ticks and hovers before rendering (eas-b2s.3)

Scope: what a live frame costs **before** a renderer runs. That is the
event dispatched to a view (reduce, then plan patch or full compile,
then scene assembly) for order-book pushes, timer ticks, a slider and
hover on a large scene. The text and SVG renderers belong to
eas-b2s.1 and eas-b2s.2.

Bench: `scripts/bench-frame-update.el` (headless; mode `interp`,
`byte` or `native`; builds are cached per source hash):

```sh
emacs -Q --batch -l scripts/bench-frame-update.el -- byte   [SRC] [FRAMES]
emacs -Q --batch -l scripts/bench-frame-update.el -- native [SRC] [FRAMES]
EAS_BENCH_ONLY='pi-mc\|tick' emacs -Q --batch -l scripts/bench-frame-update.el -- byte
```

Workloads (100x40 cells for text, 640x360 px for the pushes):

| workload | what one frame is |
|---|---|
| ladder push | keyed push of 10 of 50 levels into a 2x25 ladder (bars and labels, fixed size domain) |
| ladder-100 push | the same ladder with 2x50 levels, 20 changed per frame |
| depth push | the same push into cumulative depth areas (window sum, fixed domains) |
| depth-fixed push | the depth chart with every domain literal, colour too |
| candle stream | 60 OHLC candles and a 10-candle moving average (window mean): the last candle updates every frame, a new one opens every fourth (keyed push, window 60) |
| clock tick | the clock template's timer handler, then the param event |
| pacman tick | the pacman template's timer |
| pi-mc step | pi-monte-carlo's slider (500 points and up, 5000 generated) |
| airport hover | a pointermove sweep over airport-connections (3000+ routes) |

## Where the time went (0.2.3, byte-compiled)

Measured with `elp` (the sampling profiler stalls in `--batch` on this
box) and per-frame timings:

- **Pushes and ticks were already about 1 ms** in the update path
  (ladder 1.4, depth 1.7, clock 0.9, pacman 2.5 ms). The 4.8 ms ladder
  and 28 ms clock-text frames the owner sees go mostly to the
  renderers. The update path is not where a 4 Hz ladder pins a core.
- **airport hover: a 14 ms mean was really 1 ms frames plus a 220-260 ms
  full compile every ~20th frame.** Hovering an airport whose routes end
  outside the explicit `[0, width]` x domain (Anchorage, Honolulu) made
  `eas-patch--fits` refuse the patch. An explicit domain cannot move,
  so the refusal was never needed.
- **pi-mc: every slider step is a full compile (360 ms)**, which is
  right, because the estimate panel's x domain follows `num_points`.
  But the 5000-row root transforms (filter, three random calculates,
  the num_points filter) ran about 7 times per frame: once per unit in
  the failed patch attempt (no memo was bound), then once per distinct
  transform list in the full compile (the memo matched whole lists
  only).

## What changed, first pass

1. **Patch by scale equality, not containment** (`eas-compile-patch.el`).
   A unit refiltered by a changed param is kept when the group's scales,
   axis defs and legend specs rebuilt over the new units equal the
   plan's (`eas-patch--same-scales-p`), and either its values fit the
   scales or the layout cannot grow around marks (sized plan, text,
   autosize none, clipped view: `eas-patch--fixed-layout-p`). The plan
   now records `:size`.
2. **Transform prefixes run once** (`eas-compile-memo.el`). Transforms
   run one step at a time, and every prefix is remembered per rows
   vector, so layers share their ancestors' work. Prefixes also outlive
   a compile: rows tagged from a stable source (the view's data, a
   spec's values) are remembered against that source in a weak table,
   keyed by prefix, the params the prefix text names, and
   `eas-time-zone`. Registered transforms, selection lookups and
   `now` are never kept across compiles. The patch attempt runs under
   the memo too.
3. **Two correctness bugs found by the new property tests** (both
   present at 0.2.3):
   - a param filter that *shrinks* a data domain kept the old, wider
     scale (`fits` only checked containment). This affected pi-mc when
     the slider moved down;
   - a push after a param change filtered with the plan's stale `:env`
     (`eas-compile-rows-patch` now derives env from the state, and
     `eas-compile-patch` stores the new env).

Property tests (`src/eas-update-test.el`): random hover sweeps with
rules leaving an explicit domain (svg, text, sized, autosize none,
data domains), random slider sequences, a stepwise-vs-plain transform
run check, random mixes of keyed push, param and hover events, and
clock ticks. Each frame must equal a full compile from nothing, with
no kept runs.

## Second pass: the phases, built

The first pass left five phases as a design: a cheaper scale probe,
specialized update closures, a retained scene with dirty marks,
incremental (index-backed) filters and fixed-domain streams. The owner
asked for them built, in order of measured win, with the tick
regression the first pass introduced fixed first. Every step is gated
by the incremental-equals-full-compile property tests
(`src/eas-update-test.el`), and every golden stays byte-identical.

New property tests: compiled expressions against `eas-expr-eval` (a
corpus over random rows and envs, errors included); indexed filters
against row tests (monotone, shuffled, duplicate and non-numeric
fields, every operator on both sides); running windows against bounded
ones; random param filters over literal and data-driven domains
(counting probes); pacman ticks and pi-monte-carlo slider steps up,
down and repeated, past the refused-patch skip; random keyed pushes
into a 2x50 ladder and a windowed candle stream with moving averages;
pushes into an all-literal stream (counting scale builds). Every frame
of every sequence also checks that the dirty marks are sound.

### 0. The tick regression (clock 12.5k to 17.3k conses, pacman 35k to 41k)

Two causes, both from the first pass:

- **The scale probe ran on every refiltered unit.** `eas-patch--same-scales-p`
  rebuilds the group's scales to prove a param filter moved no domain;
  that was a third of a clock tick. A unit whose encoding is unchanged
  and whose every scaled channel has a literal domain (or `scale: null`,
  and no bins or discretizing scale) cannot move a scale, so
  `eas-patch--fixed-scales-p` skips the probe for it. Clock and pacman
  qualify; a data-driven channel still probes.
- **The memo copied rows at every step.** Built-in transforms never
  change their input (`eas-plist-put` copies), so a step now reads the
  previous step's run as it is; only a registered transform gets copies,
  and the unit still gets fresh rows at the end. The memo also builds
  each prefix and kept-run key once per call, looks runs up without
  consing a key, and trims its kept list in place.

The same profile showed `eas-patch--mentions` printing and regexp-matching
a unit's transforms and encoding on every frame (5k conses of a 13k
clock tick, in 0.2.3 too). It is now cached per value (they live as long
as the unit). Ticks now allocate less than 0.2.3 did.

### 1. pi-monte-carlo: 380 ms to about 30 ms per slider step (byte)

Where the remaining 85 ms went, measured with per-function wall clocks
(`elp` inflates small calls):

| cost per step (byte) | ms | why |
|---|---:|---|
| refused patch attempt | ~40 | the estimate panel's x domain follows `num_points`, so every patch is refused, after recompiling every unit |
| window `sum` over N rows | ~35 | the cumulative frame `[null, 0]` re-aggregated rows 0..i for each row i: O(N^2) |
| random calculates rerun every 4th step | ~15 avg | the kept-run table dropped its oldest entry first, so the slider's one-off runs pushed out the prefix every step needs |
| `datum.data <= num_points` over 5000 rows | ~3 | one expression walk per row |
| expression walks elsewhere | ~10 | `eas-expr-eval` walked the AST for every row |

Fixes, each with a property test:

- **Refused patches are remembered** (`eas-patch--refusals`). After two
  refusals in a row for the same changed params, a view's patch goes
  straight to the full compile; it is retried every `eas-patch-retry`
  (8) steps, and a patch that holds resets the count. Skipping a patch is
  always correct (the full compile is the reference), so this only
  trades work.
- **Running windows** (`eas-agg--window-prefixes`). A window framed from
  the first row (`[null, k]`) with count, valid, missing, sum or mean is
  one pass: each frame extends the last. The running sums add the same
  numbers in the same order as `eas-agg-apply` over the prefix, so the
  results are identical, floats included.
- **The kept-run table is LRU**: a hit moves to the front.
- **Index-backed param filters** (`src/eas-transform-index.el`). A filter
  `datum.F op param` (`<`, `<=`, `>`, `>=`, `==`, `===`, either side) over
  at least 256 rows is answered from an index of its input vector, built
  once: a slice when F never decreases in row order (sequences,
  timestamps), else positions sorted by value and a binary search. The
  input is the kept run of the transforms before it, so it is the same
  vector on every step. A non-number anywhere falls back to row tests.
- **Compiled expressions** (`eas-expr-function`). Each expression string
  becomes nested closures once (names, operators, functions and
  `datum.field` lookups resolved ahead) instead of an AST walk per row.
  This is the "specialized update closure" at the level where the time
  was: every calculate, filter and test condition runs it, so pushes,
  ticks and full compiles all gain. A test checks it against
  `eas-expr-eval` over a corpus, errors included.

What a step costs now (byte, svg, per-function wall clock): full compile
~27 ms, of which marks for ~1000 points (two panels, a colour test per
point) ~12 ms, units and transforms ~10 ms, layout ~3 ms; scene ~2.5 ms.

### 2. Retained scene with dirty marks

`eas-compile--view` keeps, on each unit, the mark plist it made last
frame (`:scene-mark`). When the unit's items, rows and hit index are the
same objects (a patch or push left the unit alone), the scene gets that
very plist back. `eas-scene-dirty` (`src/eas-scene-dirty.el`) reads the
identity back:

- `t`: redraw everything (no last frame, size, title, params, view count
  or a view's chrome changed);
- `nil`: nothing changed;
- `((VIEW-ID MARK-ID ...) ...)`: per view, the marks that changed.

The view records it after every compile (`eas-view-dirty`). It may over-
but never under-report: the property tests assert after every frame that
every mark not named is `eq` to the last frame's. Renderers (eas-b2s.1,
eas-b2s.2) can now redraw only the named marks; this box does not touch
them.

### 3. Fixed-domain streams skip scales

A push already reused the layout (`eas-compile-rows-patch`). When every
unit of the view passes `eas-patch--fixed-scales-p`, the push now keeps
the old scales, axis definitions and legends without building them
again to compare (`depth-fixed push`: 18.5k to 14.8k conses). A
property test pushes random rows, sizes past the domain included, and
counts that no scales are built.

### What was measured and left alone

- **Native compilation** pays 1.3-1.7x over byte code on every workload
  (table below). It is the cheapest win left, and needs no code here.
- **A dynamic module (Rust)**: no hot loop is numeric enough to pay for
  marshalling plists across the FFI. The costs are plist walks and
  allocation, which a module would have to do through the same objects.
- **Pushes with data-driven scales** (ladder, candles) still rebuild
  their scales to compare. The candle stream's x domain slides with
  every new candle, which needs a full compile (about 5 ms byte) every
  fourth frame. That is correct, not waste.

## Before and after

Same box, same session, one run after another with nothing else
running. Base is 85d350e (0.2.3); after is this branch. 30 frames per
workload, GC deferred as the glue defers it. Wall-clock numbers under
about 3 ms carry ±0.3 ms of noise (more for the candle stream, whose
full compile every fourth frame lands unevenly in 30 frames);
allocation counts are exact. Base was benched from a worktree of
85d350e: `emacs -Q --batch -l scripts/bench-frame-update.el -- byte
BASE/src 30`.

| workload | target | byte before | byte after | native before | native after | alloc before | alloc after |
|---|---|---:|---:|---:|---:|---:|---:|
| ladder push | svg | 1.39 | 1.47 | 0.96 | 1.13 | 13752 | 13943 |
| ladder push | text | 1.63 | 1.85 | 1.24 | 1.59 | 19013 | 19204 |
| ladder-100 push | svg | 2.16 | 2.35 | 2.04 | 1.90 | 23135 | 23326 |
| ladder-100 push | text | 2.98 | 2.72 | 2.03 | 2.05 | 30814 | 31005 |
| depth push | svg | 1.56 | 1.36 | 1.38 | 0.86 | 18575 | 16099 |
| depth push | text | 1.62 | 1.40 | 1.03 | 0.93 | 20323 | 17847 |
| depth-fixed push | svg | 1.54 | 1.11 | 1.01 | 0.90 | 18463 | 14839 |
| depth-fixed push | text | 1.60 | 1.20 | 1.07 | 0.97 | 20211 | 16587 |
| candle stream | svg | 4.19 | 4.22 | 2.84 | 3.54 | 51446 | 49341 |
| candle stream | text | 3.93 | 3.14 | 2.03 | 2.23 | 42933 | 40827 |
| clock tick | svg | 1.00 | 0.83 | 0.67 | 0.51 | 12526 | 8259 |
| clock tick | text | 1.04 | 0.77 | 0.78 | 0.56 | 13376 | 9109 |
| pacman tick | svg | 2.90 | 1.93 | 1.61 | 1.56 | 35320 | 21762 |
| pacman tick | text | 2.96 | 2.21 | 1.68 | 1.48 | 35715 | 22157 |
| pi-mc step | svg | 399.52 | 30.27 | 241.16 | 20.54 | 3062169 | 353211 |
| pi-mc step | text | 395.75 | 25.16 | 237.98 | 19.23 | 3007071 | 298659 |
| airport hover | svg | 16.34 | 0.84 | 12.30 | 0.59 | 269928 | 8139 |
| airport hover | text | 14.90 | 0.84 | 13.40 | 0.70 | 261732 | 8431 |

"alloc" is the sum of `memory-use-counts` per frame (conses, vector
cells, strings and the rest), byte-compiled. The ladder's +191 is the
first pass's correctness fix (a push derives its env from the state,
not the plan's stale `:env`, about 150) and the literal-domain check;
three reruns of the ladder alone put its time within noise of 0.2.3.
The candle stream's 30-frame row is noisy; two 80-frame native runs
give 2.96 and 3.28 ms before, 2.69 and 2.82 ms after (svg), 2.18 before
and 2.05 ms after (text).
Pacman and clock now allocate a third less than 0.2.3; pi-monte-carlo
and airport-connections an order of magnitude less.

## Validation

- `make test`: 710 tests, 705 passed, 0 unexpected, 5 skipped (no
  `bin/chart`, no `rsvg-convert`, no Vega-Lite schema on the box).
- `make compile`: checkdoc and byte-compile clean.
- `make test-gallery-calculations`: 2 of 2 pass.
- All 103 templates rendered as text (`eas-text-gallery-run`), base and
  after: the same verdicts, template for template; after is 10% faster
  (418 to 377 s, byte).
- Three gallery failures are present at 0.2.3 too, with identical
  output on both trees: the conformance text golden of
  `interactive/interactive_concat_layer` (and so
  `eas-conformance-supported-json-is-current`), text contrast at 60x16
  in `interactive/interactive_brush`, and `templates/vega/` entries
  missing from `text-status.json`.

## Per-frame budget (update path, native, after)

| frame | budget | 0.2.3 | now |
|---|---:|---:|---:|
| push (ladder 2x25, depth) | 1 ms | 1.0-1.4 ms | 0.9-1.6 ms |
| push (ladder 2x50, 20 levels) | 2 ms | 2.0 ms | 1.9-2.1 ms |
| push (candles, x domain slides every 4th frame) | 3 ms | 2.2-3.3 ms | 2.0-2.8 ms |
| tick (clock) | 1 ms | 0.7-0.8 ms | 0.5-0.6 ms |
| tick (pacman) | 2 ms | 1.6-1.7 ms | 1.5-1.6 ms |
| hover (3000+ items) | 1 ms | 12-13 ms | 0.6-0.7 ms |
| slider over 5000 rows (data domain moves) | 20 ms | 238-241 ms | 19-21 ms |

Every frame but the pi-mc slider is under 4 ms before rendering, so at
60 Hz the update path leaves most of the 16 ms frame to the renderer;
the slider runs at 50 steps a second.

## What is next

1. **Renderers read `eas-view-dirty`** (eas-b2s.1, eas-b2s.2): the SVG
   backend patches the DOM nodes of named marks, the text backend
   re-rasterizes their cells. The scene side is done.
2. **Partial relayout for concats.** pi-mc's left panel (literal
   domains) could patch while only the estimate panel recompiles; that
   needs the layout of one concat cell to be redone in place, which today
   is all or nothing.
3. **Items kept across full compiles.** A pi-mc step that adds 5 points
   rebuilds the items of the 500 it already drew (about 12 ms of the
   step). Per-row marks could keep the item of every row equal to the
   last plan's at the same position, as pushes already do
   (`eas-compile-rows--items`), when the unit's encoding, the params it
   names, its scales and its bounds are unchanged. Not built: on an
   unsized SVG the first layout's bounds differ from the final,
   translated ones whenever marks overhang, so the check rarely holds
   without also keeping the pre-translation items.
4. **Per-row transform reuse for pushes.** A keyed push re-runs row-local
   transforms (calculate, filter, timeUnit) over every row; an output
   kept per source row would make that proportional to the rows pushed.

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

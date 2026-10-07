# Live frames, SVG half (eas-b2s.2)

How fast a live SVG view can draw a frame, where the time goes, and what
0.2.4 changes. Box (2) of eas-b2s: the SVG output and the GUI glue
(`eas-svg.el`, `eas-svg-retain.el`, the image path of `eas-mode.el`).
The text renderer (eas-b2s.1) and the compile/update path (eas-b2s.3)
are measured and changed by their own boxes.

## Method

`scripts/bench-frame-svg.el` times the live SVG workloads headless:

```sh
emacs -Q --batch -l scripts/bench-frame-svg.el -- MODE [SRC] [FRAMES]
```

MODE is `interpreted`, `byte` or `native`. Byte and native copy SRC to a
temporary directory and compile it there (eight compilers at a time), so
an older checkout (`git worktree add /tmp/base-repo 85d350e`) is benched
the same way. A frame is

- **update**: the push, tick, slider step or pointermove (`eas-dispatch`,
  `eas-play-tick`), the compile/update path;
- **draw**: what the GUI glue does with the new scene, `eas-mode-redraw`
  in an `eas-view-mode` buffer: `eas-svg-image` (the SVG string and the
  :map hot spots), the image inserted and its hot-spot keys bound.

The rasterization librsvg does in a GUI frame is not in a batch Emacs.
fc-qx1.24 measured it on NS (about 90 ms for an unchanged 1000x640
hover before 0.2.3 kept the image cache entry); it is the next budget to
measure on a display (see Phase 3).

Workloads: a 25-level order-book ladder (bars and size labels, 2x25
bands, fixed size domain) and a depth chart (stepped areas, fixed
domains) over the same book, 10 levels changing per push; clock and
pacman timer ticks; a pi-monte-carlo slider step (10 more samples,
1000 points); an airport-connections hover (298 airports, 51 states).

## Where the time went (0.2.3, byte-compiled)

CPU profile of `eas-svg-image` over recorded ladder frames, warm cache
(every mark fragment a hit):

| stage | share |
|---|---:|
| axes rebuilt as DOM nodes every frame (`eas-svg--axis`, theme lookups) | 37% |
| `svg-print` (one `format` per tag and attribute) | 28% |
| hot spots: per-item `intern (format ...)`, and axis label areas | 17% |
| legends, header, the rest | 18% |

Cold (the realistic case: 10 of 50 bars changed) the bar and text marks
missed the mark-level fragment cache as a whole, so all 100 items were
rebuilt and printed: 4.1 ms of `eas-svg-image` against 2.0 ms warm.

In the glue, `eas-mode--hot-spot-keys` built a fresh keymap each frame
with three `define-key` per area: 60 ms of a 95 ms pi-monte-carlo draw
(1000 points, 3000 bindings), 2-3 ms of pacman's and airport's.

## What changed

All of it keeps every SVG byte-identical (the goldens are untouched, and
`eas-svg-retain-random-frames-equal-fresh-renders` checks random push,
tick, slider and hover sequences against `svg-print` of the plain DOM).

1. **Retained items** (`eas-svg-retain-item`). Each mark item's printed
   node is kept, found by content: the mark kind and the item plist. A
   push re-prints only the items that changed, and a state seen again (a
   hover going back) hits. The table hashes with
   `eas-svg-retain--hash`: `sxhash-equal` looks 3 levels down and at
   the first 7 elements of a vector, so every version of a series whose
   first points stay would share one bucket, and a long stream would
   compare against a growing chain of old versions on each lookup. The
   hash adds every point of a series item; other items cost what
   `sxhash-equal` does. Items with a gradient fill record a
   definition as they print, so they are never retained; image marks
   (file reads) neither.
2. **Retained axes and legends** (`eas-svg-retain-part`). An axis prints
   to (GRID . REST) strings, a legend to one string, each kept in a slot
   of its own (view id and axis index or legend channel) with the axis
   or legend and the theme it was printed for: a hit is one `equal`
   against the last version, and a slot holds one value, so a panning
   axis does not pile up versions. Grids still go behind the marks and
   zindex still decides.
3. **A printer without per-node `format`** (`eas-svg-retain--pieces`):
   strings are pushed onto a list and joined once; output equals
   `svg-print`'s (a test checks colon attributes, raw strings, numbers).
4. **Cheaper nodes**: attribute symbols are looked up, not interned from
   a substring, per attribute; `eas-svg--escape` skips the regexp
   replace when there is nothing to escape.
5. **Retained marks** (`eas-svg-retain-mark`). A mark's printed SVG
   (and the gradient definitions it records) is kept in a slot of its
   own, view id and mark id, last version only. It replaces
   `eas-render-cache-svg-fragment` for SVG, which keyed on the whole
   mark: every pushed version of a mark landed in one bucket, so over
   2000 ladder pushes 0.2.3's draw crept from 5.1 to 7.1 ms per frame
   (byte-compiled, 250-frame means). With slots it stays flat at about
   0.9 ms. Hits still count in `eas-render-cache-stats`.
6. **Hot spots**: areas are retained per mark, in a slot like the axes
   (a frame that changed other marks reuses them), and ids
   `eas:VIEW|MARK|I` are interned once per index. A per-item area
   cache was measured and dropped: hashing an
   item costs what computing its area does.
7. **Glue** (`eas-mode.el`): the buffer keeps one hot-spot keymap and a
   redraw binds only ids no earlier frame bound. A redraw whose SVG data
   and :map equal the shown image's leaves the image and rewrites only
   the values strip (no flush, no new image spec, nothing for redisplay
   to re-rasterize).

Retained items live in two generations of 8 MB
(`eas-svg-retain-max-bytes`), parts in at most 4096 slots
(`eas-svg-retain-max-parts`); all of it is off with
`eas-render-cache-enabled`.

## Before and after

Same machine, same run (`git worktree` of 0.2.3 at 85d350e for
"before"), Emacs 30.1, 8 cores; mean ms per frame over 40 frames,
averaged over two alternating rounds. "draw" is the SVG and glue half
this box owns; "frame" adds the update, which this change does not
touch (its variation between before and after is noise of the compile
path).

| workload | mode | draw before | draw after | draw speedup | frame before | frame after |
|---|---|---:|---:|---:|---:|---:|
| ladder push (25 levels) | byte | 4.42 | 1.04 | 4.3x | 5.72 | 2.33 |
| depth push (25 levels) | byte | 1.53 | 0.96 | 1.6x | 3.09 | 2.61 |
| clock tick | byte | 0.53 | 0.33 | 1.6x | 1.31 | 1.11 |
| pacman tick | byte | 4.21 | 1.01 | 4.2x | 6.67 | 3.52 |
| pi-monte-carlo slider step | byte | 134.64 | 24.03 | 5.6x | 739.50 | 624.33 |
| airport-connections hover | byte | 3.74 | 0.65 | 5.8x | 4.73 | 1.41 |
| ladder push (25 levels) | native | 3.73 | 0.76 | 4.9x | 4.63 | 1.65 |
| depth push (25 levels) | native | 1.23 | 0.71 | 1.8x | 2.25 | 1.79 |
| clock tick | native | 0.41 | 0.24 | 1.7x | 0.98 | 0.83 |
| pacman tick | native | 3.67 | 0.73 | 5.1x | 5.22 | 2.56 |
| pi-monte-carlo slider step | native | 128.72 | 17.49 | 7.4x | 538.59 | 427.11 |
| airport-connections hover | native | 3.15 | 0.42 | 7.4x | 3.80 | 0.95 |

Draw is now about a millisecond or less for every live workload but
the 1000-point slider: 0.3-1.0 ms byte-compiled, 0.2-0.8 ms native.
Native compilation is worth another 25-35% on draw and about 30% on the
update.

Byte identity, beyond the goldens: every conformance spec, every
vl-example and every template example (396 scenes) rendered by 0.2.3
and by this change, twice each (cold, then retained), give the same SVG
and the same :map, all 396. The one difference found was a gradient id
(`eas-paint--id` hashed a plist of keywords, which `sxhash-equal`
hashes by address, so the id changed between Emacs sessions, at 0.2.3
too); it now hashes the printed gradient and holds across sessions.

## What is left, per frame

After this change the SVG draw is the smaller half of every workload:
the update (compile/patch, eas-b2s.3) is the larger.
Remaining draw costs, byte-compiled:

- **Axis click areas** (`eas-action-callback-hot-spots`, an
  `eas-svg-hot-spot-functions` member): about 10% of a ladder draw. They
  depend on the scene's target, size and config and on the axis, so they
  can be retained per axis like the axis nodes; the file belongs to the
  action layer, left for its owner.
- **Item misses**: a pi-monte-carlo step resamples every point, so every
  item misses; 1000 circles cost about 15 ms to build as DOM nodes and
  print. Emitting each common item kind (rect, circle, text, path)
  straight to a string, with no DOM node and no `apply`, is the next 2x
  there.
- **Change detection by `equal`**: a mark slot compares the new mark
  with the last one; when the compile path reuses the mark object that
  is one `eq`, otherwise a walk to the first difference. A dirty set
  from the update path (which marks it rebuilt) would make it free.

## Phased plan

- **Phase 1 (this change)**: retained items, marks, axes, legends,
  hot spots and hot-spot keys; faster printer; unchanged image kept;
  no slowdown over a long stream. Done, measured above.
- **Phase 2**: direct string emitters per item kind; retained axis click
  areas; a dirty-mark set from the update path (eas-b2s.3) so unchanged
  marks are reused by identity, not `equal`.
- **Phase 3 (GUI raster, needs a display to measure)**: librsvg
  rasterizes the whole image on every changed frame, and that is the
  largest GUI cost left (tens of ms at 1000x640, fc-qx1.24). Splitting a
  live view into horizontal image strips (static ones keep their image
  cache entry, a strip whose SVG changed re-rasterizes alone), with
  :map areas and `posn-object-x-y` offset per strip, bounds the raster
  to the changed band. Measure on X and NS before building it.
- **Native-comp / dynamic module**: native compilation already gives
  most of what a module would on these loops (see the table); with the
  draw at about a millisecond, a Rust module is not worth its build
  and distribution cost.

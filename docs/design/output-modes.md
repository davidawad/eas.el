# Output modes: svg, text, kitty, sixel (eas-rhy)

Every eas chart can reach the screen four ways. A GUI frame shows its
SVG as an Emacs image. Any frame can show the text backend. A terminal
that supports it can also show the SVG as **pixels**, through the Kitty
graphics protocol or DEC sixel. You choose the output declaratively.
eas never guesses behind your back, and text stays first-class. The
code is in `src/eas-output.el` (detection, resolution, strict errors,
rasterizers, placement) and `src/eas-mode.el` (the chart buffer).

This mirrors graphics.el's `graphics-output.el` and its
`docs/design/output-modes.md` (beads gfx-e7j, gfx-rfr): the same
vocabulary, precedence and strictness, so the two libraries feel like
one. The layer is public (`eas-output-*`), so graphics.el can drop its
copy and use eas's. (That repository could not be read from the pod
this was built in. This design follows the eas-rhy bead's statement of
it; check the two side by side when graphics.el switches over.)

## The option

`eas-output` is a defcustom that may be set buffer-locally. A spec can
override it with `x-eas.output` (a template's `x-eas.output` counts as
the spec's). Precedence is **spec > buffer > global**. The buffer is the
chart buffer (its local value survives `eas-view-mode`, so `g` picks up
a change) or the buffer `eas-show` was called from.

| value | meaning |
|---|---|
| `svg` | an SVG image; needs a graphical frame with SVG support |
| `text` | text cells (eas-text); works everywhere |
| `kitty` | the SVG as pixels via the Kitty graphics protocol |
| `sixel` | the SVG as pixels via DEC sixel |
| `auto` | per window: GUI gives svg, then kitty, then sixel, else text |
| a list, e.g. `(kitty sixel text)` or `["kitty","sixel","text"]` | the first mode the window can show |

Modes may be symbols or strings, and lists may be lists or JSON arrays.
Anything else signals `eas-output-invalid` (`OUTPUT_INVALID`), naming
the value and where it came from.

**Default: `(svg text)`.** That is eas's behaviour from before output
modes: images in GUI frames and text in terminals. Pixels are opt-in
(`(setq eas-output 'auto)`). graphics.el defaults to `auto`. eas does
not, for three reasons:

- `auto` in a terminal queries the terminal. It writes bytes and waits
  up to `eas-output-query-timeout` (0.4 s) for replies on the first
  chart.
- Pixels need the module's optional raster feature, which needs crates
  that the default build does not fetch.
- Text is what eas's interaction and parity guarantees are tested
  against.

David opts in. Switching the default to `auto` is a one-line change once
the raster module ships prebuilt.

An explicit `target` argument to `eas-show` (`'svg` or `'text`, as in
the tests, `eas-agent-live --backend` and drill-downs from a targeted
parent) bypasses `eas-output`, as before.

## auto, per window

`eas-output-detect` reads the window's frame:

- **GUI frame**: svg (or text when Emacs cannot display SVG).
- **Kitty graphics**: any of these counts as a terminal that speaks the
  Kitty graphics protocol:
  - `TERM_PROGRAM` is `WezTerm`, `ghostty` or `kitty`;
  - `TERM` is `xterm-kitty`, `xterm-ghostty` or `wezterm*`;
  - `KITTY_WINDOW_ID` is set (outside tmux);
  - the terminal answers the graphics query (`a=q`) with `OK`.
- **sixel**: DA1's reply includes 4, or `TERM_PROGRAM` is `WezTerm`.
- **Otherwise**: text.

The query sends a Kitty graphics query, `CSI 16t` (cell size), `CSI 14t`
(text area) and DA1 in one round trip, then reads until DA1's reply. It
runs once per terminal, and only when `auto`, `kitty` or `sixel` is
under consideration, so the default `(svg text)` never writes a byte.
Set `eas-output-query-terminal` to nil to skip it.

A reply that is not read becomes keys: bytes such as `[6;28;14t` run
commands (eas-o3c). So the query never leaks:

- It is not sent when the environment already says Kitty graphics (the
  list above). Kitty places images by cells (`c=`, `r=`), so the cell
  size there only sets the resolution: `eas-output-cell-size`, else
  the default.
- It is not sent from a timer (a command may be reading a key
  sequence), with input pending, or while another query reads. A later
  chart asks again.
- DA1 goes last as the sentinel. Replies are read raw (`read-event`,
  keyboard translation off) until DA1's reply; once the first byte
  arrived the read waits up to `eas-output-query-drain` (2 s) for it.
  Without it the partial reply is discarded, never queued.
- Under tmux the Kitty query is not sent: the outer terminal's reply
  would come after tmux's own DA1.

**tmux** counts only when the frame's tty really is a tmux pane. The
device of the frame's terminal must be the `#{pane_tty}` of a pane of
the server that `$TMUX` names. A `TMUX` variable left in the environment
of an Emacs that is not in that server does not count: it neither
blocks pixels nor wraps escapes (a bug seen live). In a real pane, the
pane must have `allow-passthrough` `on` or `all`. Escapes are wrapped in
`ESC P tmux; ... ESC \`. The outer terminal comes from
`#{client_termname}` and `#{client_termtype}`. Cursor moves are counted
from the screen, offset by `#{pane_top}` and `#{pane_left}`, which are
read on every sync.

## Strict errors

A mode asked for explicitly that the window cannot show signals
`eas-output-unsupported` (`OUTPUT_UNSUPPORTED`). It is a child of both
`user-error` and `eas-error`. It is signalled **before any byte is
written**, and a list that does not end in `text` or `auto` fails the
same way, naming every mode it tried. `auto`, and lists ending in `text`
or `auto`, never error. The chart buffer shows the error:

```
eas: chart line:daily cannot be drawn with output kitty

  asked:    kitty (from x-eas.output of the spec)
  frame:    terminal TERM=xterm-256color TERM_PROGRAM=Apple_Terminal; tmux: no
  kitty:    the terminal does not speak the Kitty graphics protocol (...)
            fix: run Emacs in Kitty, WezTerm or Ghostty, or ask for (kitty sixel text) or auto

Nothing was sent to the terminal.  ...
```

The error data is `(MESSAGE :code CODE :view :mode :source :facts
:reasons)`. Each reason is `(:mode :reason :fix)`. The reasons are:

| case | reason | fix |
|---|---|---|
| svg in a terminal | SVG images need a graphical frame | auto, kitty or sixel; or text |
| svg, no librsvg | this Emacs cannot display SVG images | text, or an Emacs with librsvg |
| kitty/sixel in a GUI frame | terminal pixel protocols apply only to terminals | svg or auto |
| tmux without passthrough | allow-passthrough is off | `tmux set -g allow-passthrough on` |
| kitty, other terminal | does not speak the Kitty graphics protocol | Kitty, WezTerm or Ghostty; or a list |
| sixel, no DA1 4 | does not report sixel | a sixel terminal; or a list |
| no rasterizer | module lacks raster, no rsvg-convert | `make module FEATURES=raster`, or install rsvg-convert |
| sixel via rsvg-convert | img2sixel not installed | build the module, or install img2sixel |

`M-x eas-output-describe` shows the value in effect and its source, the
terminal facts, each mode's verdict and fix, what `auto` picks, and
what this setting draws with (or how it fails).

## Rasterizing

A pixel chart is compiled for the svg target, at the pixel size of the
cells a text chart would fill: COLS × cell width by ROWS × cell height.
The cell size comes from `eas-output-cell-size`, else the `CSI 16t`
reply, else the `CSI 14t` reply divided by the frame size, else 10×20.
The SVG is themed from the faces when the terminal reports a background
colour. Then it is drawn:

- **The module** (`module/`, Rust, `--features raster`) uses resvg with
  the system fonts, loaded once. `eas-raster-module-encode` returns the
  terminal bytes directly:
  - Kitty: `a=t,f=32,o=z` (zlib RGBA), in 4096-byte base64 chunks,
    `q=2` (no replies);
  - Kitty PNG (`f=100`);
  - sixel: up to 256 registers, chosen by popularity over 15-bit
    colours with nearest-colour mapping, run-length encoded and
    composited over the face background;
  - PNG (for goldens and export).

  Build it with `make module FEATURES=raster`, or set
  `eas-geo-build-features` to `("raster")` and run `M-x
  eas-geo-build-module`. The geo code alone still needs no crates.
- **rsvg-convert**, when the module lacks the feature, writes PNG. Kitty
  sends that as `f=100` (`eas-output-kitty-transmit`, the same chunking
  as the module). sixel pipes it through `img2sixel`.

With neither, kitty and sixel are unsupported, with the fix above.
`eas-output-rasterizer` forces one rasterizer (`module` or
`rsvg-convert`).

## Placement

The chart buffer holds ROWS lines of COLS spaces, then the values-strip
line. Those blank cells are rewritten only when their size changes, so
redisplay never paints over the image. After redisplay (a 0 s timer that
calls `redisplay`, then `eas-output-sync`), each surface (one per
buffer) is brought up to date in every terminal window that shows it:

- **Changed SVG** (compared by hash): rasterize once, transmit under a
  fresh image id, place it in each window, then delete the previous
  image. A live frame never blinks.
- **Unchanged SVG, moved window** (scroll, resize, split, another
  window): place again with the same placement id (one per window), with
  the source rectangle cropped to the visible rows (`y=`, `h=`). Nothing
  is re-sent.
- **The window stops showing the buffer**: delete that placement (`a=d,
  d=i, i, p`).
- **The buffer is killed, or switched to text**: delete the image and
  its data (`a=d, d=I`).
- **`recenter`/`redraw-display`** (which clear a terminal screen):
  place again.

Each placement saves and restores the cursor (`ESC 7` / `ESC 8`) around
`CSI row;col H`, so Emacs's idea of the cursor stays true.

Sixel pixels live in the cells, so there is no placement: a change or
move re-emits the bytes at the new position. A move away from an old
position repaints the frame (`redraw-frame`) to clear what was there.
Sixel cannot crop, so a partly scrolled sixel chart is not drawn until
it shows whole.

`eas-output-surface-placements` holds, per window, the `(TOP LEFT Y0
ROWS)` last drawn. Tests check the bookkeeping through
`eas-output-write-function`, the one door to the terminal.

## Interaction

Interaction is unchanged, because a pixel buffer is a text buffer to the
glue. Point is the pointer. Moving it hovers. `n`/`p` step the hover and
move point onto the datum's cell. RET clicks, and xterm-mouse motion,
clicks and drags arrive as cells. Brush, zoom and pan keys work as in
text buffers. `eas-mode--cell` maps a cell to scene pixels by the
terminal's cell size. That is the same `(col + ½) × w, (row + ½) × h`
that text mode uses with its scene cell, and `eas-hit` picks from those
pixels. The header-line readout and the values strip show the same data
in every mode. Live frames and streams (eas-b2s) redraw through the same
path: each frame that changes the SVG is rasterized and re-sent once.

## Cost

`scripts/bench-frame-output.el` measures 200×60 cells, which is
2000×1200 px at the default 10×20 cell. The figures below are from a
byte-compiled run on a 4-CPU Linux pod with Emacs 30.1, resvg 0.49 and
the module built `--release --features raster`, 20 frames after one
warm-up frame (`bench/output-frames.md`):

| workload | update ms | svg ms | kitty ms | sixel ms | changed | kitty frame ms | sixel frame ms | kitty KB | sixel KB |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| line with crosshair, hover | 0.9 | 0.1 | 31.4 | 75.4 | 20/20 | 32.4 | 76.4 | 150 | 103 |
| bars, hover | 0.2 | 0.1 | 29.2 | 58.5 | 0/20 | 0.3 | 0.3 | 148 | 61 |
| heatmap, hover | 0.3 | 0.2 | 29.6 | 72.1 | 0/20 | 0.4 | 0.4 | 131 | 80 |
| candle stream, push | 9.6 | 5.4 | 51.0 | 145.7 | 20/20 | 66.0 | 160.7 | 271 | 275 |

The kitty and sixel columns are rasterize plus encode, measured on every
frame. The frame columns are what a sync spends: an unchanged SVG
(a hover over bars or a heatmap moves no mark) sends nothing.

- Kitty costs about 30 ms a frame at 2000×1200: resvg plus level-1
  zlib. A live stream runs at about 15 frames a second.
- sixel costs 2 to 3 times as much: palette, mapping and band encoding.
- Cost scales with pixels. A HiDPI terminal with 20×40 cells sends 4×
  the pixels, about 120 ms a Kitty frame at 200×60.
- Writing the bytes to the terminal is not included. That cost is the
  terminal's, about 150 KB a Kitty frame here.

Possible next steps, if a live chart needs them:
- rasterize at a lower scale and let Kitty scale up (`c=`/`r=` already
  fix the cell box);
- PNG for small charts;
- dirty-rectangle updates (Kitty frames, `a=f`).

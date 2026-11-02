# Terminal pixel frames, kitty and sixel (eas-rhy)

Measured 2026-11-02 in the eas-rhy pod: Linux, 4 CPUs, Emacs 30.1,
byte-compiled src/ (`make compile`), module built `cargo build --release
--features raster` (resvg 0.49, miniz_oxide 0.9), system fonts DejaVu,
Liberation and Inter.  Command:

    make compile
    make module FEATURES=raster
    emacs -Q --batch -L src -l scripts/bench-frame-output.el -- 20

200x60 cells, 2000x1200 px, 20 frames after one warm-up frame:

| workload | update ms | svg ms | kitty ms | sixel ms | changed | kitty frame ms | sixel frame ms | kitty KB | sixel KB |
|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| line with crosshair, hover | 0.9 | 0.1 | 31.4 | 75.4 | 20/20 | 32.4 | 76.4 | 150 | 103 |
| bars, hover | 0.2 | 0.1 | 29.2 | 58.5 | 0/20 | 0.3 | 0.3 | 148 | 61 |
| heatmap, hover | 0.3 | 0.2 | 29.6 | 72.1 | 0/20 | 0.4 | 0.4 | 131 | 80 |
| candle stream, push | 9.6 | 5.4 | 51.0 | 145.7 | 20/20 | 66.0 | 160.7 | 271 | 275 |

What each stage covers:

- **update**: the event through `eas-dispatch`, including the recompile.
- **svg**: `eas-svg-render`.
- **kitty, sixel**: rasterize plus encode, measured on every frame. That
  is the cost of a frame that changed the picture.
- **frame**: update + svg, plus the encode only on the frames whose SVG
  changed, which is what `eas-output-sync` spends.
- **KB**: the bytes sent per frame that changed.

The bars and heatmap templates have no hover-driven mark, so a hover
sends nothing.  Writing the bytes to the terminal is not measured.  See
docs/design/output-modes.md ("Cost").

# eas-geo-module

An optional native accelerator for eas's map projections: an Emacs
dynamic module in Rust, with no crates.  eas never needs it; with it, a
map's first render projects, clips, resamples and prints its paths
natively, and draws byte-identical SVG.  See
docs/design/geo-first-render.md ("Fourth pass") for what it ports, how
it stays exact, and what it measures.

## Build and install

```sh
cd module
cargo build --release
mkdir -p ../lib
# Copy then rename: never overwrite a module a running Emacs has loaded.
cp target/release/libeas_geo.so ../lib/.eas-geo-module.so.tmp
mv -f ../lib/.eas-geo-module.so.tmp ../lib/eas-geo-module.so
```

On macOS the cargo output is `libeas_geo.dylib`; install it under the
name `eas-geo-module` plus the running Emacs's `module-file-suffix`
(ask that Emacs: `(princ module-file-suffix)`).  The loader also finds
`module/target/release/libeas_geo.*` directly.

`eas-geo-backend` picks the backend: `auto` (the module if it loads),
`native` (required) or `lisp` (never loaded).

## The raster feature (terminal pixels)

`cargo build --release --features raster` adds `eas-raster-module-encode`.
It draws SVG with resvg (system fonts) and encodes it as a Kitty
graphics transmit (zlib RGBA or PNG), as sixel, or as PNG. The kitty
and sixel output modes use it (`src/eas-output.el`,
`docs/design/output-modes.md`). This is the only part of the crate that
uses crates (resvg, miniz_oxide). From the root, run
`make module FEATURES=raster`. In Emacs, set `eas-geo-build-features`
to `("raster")`, then run `M-x eas-geo-build-module`.

## Test

```sh
cargo test --release            # bit-exact against values Emacs prints
cargo test --release --features raster   # also the rasterizer and encoders
EAS_GEO_BACKEND=native make test   # the Elisp suite with the module (from the root)
```

The reference tests run batch Emacs on `../src`; without Emacs they
skip and say so (and append to `TEST_SKIP_LOG` when it is set).

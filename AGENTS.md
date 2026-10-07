# AGENTS.md: working on eas.el

eas is an Emacs-native, interactive chart engine. Its specs are a subset
of Vega-Lite 6.4.1, it renders as SVG in GUI frames and as text in
terminals, and agents can drive it. Read `docs/design/engine.md` first:
it describes the layer tower (L0 data through L7 surfaces) and the
contract of each layer.

## Layout

| path | what |
|---|---|
| `src/eas.el` | package entry point and headers; requires every layer |
| `src/eas-*.el` | the engine, one concern per file, prefix `eas-` (never `chart-`: built-in chart.el owns it) |
| `src/*-test.el` | ERT tests, next to the code they test |
| `src/supported.json` | generated from the conformance gallery; what the engine can draw |
| `src/bench-budget.json` | latency references for `make bench-ladder` |
| `bench/` | `baseline.json` (gated alloc and call counts per workload, target and mode) and `history.json` (per release), from `make bench-record` |
| `test/eas/` | `eas-test-support.el` (shared helpers) and `golden/` (exact text, JSON and SVG goldens) |
| `test/conformance/` | conformance specs, text goldens, bin/chart reference PNGs (`ref/`) |
| `test/vl-examples/` | the official Vega-Lite gallery by group: specs, `status.json`, reference PNGs, `custom/` specs |
| `templates/`, `examples/` | template specs and their example bindings; `examples/data/` holds every file an example reads |
| `recipes/eas` | the MELPA recipe; MELPA installs it flat (libraries, `templates/`, `examples/` in one directory) |
| `bin/eas` | shell entry point for the stateless verbs |
| `scripts/` | bench, gallery bench, tty check, checkdoc runner, screenshots, design spikes |
| `docs/design/` | engine design, measured spikes, gallery coverage |

The repository root sits one level above `src/` (`eas-template--root`).
Every data path hangs off that root.

## Commands

```sh
make test                         # fast suite (no :gallery tests), about 2 min
make compile                      # checkdoc + byte-compile, warnings are errors
make test-gallery-GROUP           # one gallery group: area-circular bar calculations
                                  # distributions interactive layered line multiview scatter-table
make test-gallery-conformance     # conformance oracle + template text gallery
make bench                        # perf suite (byte + native) vs bench/baseline.json, report only
make bench-check                  # the same, fails on an alloc/call-count regression (CI)
make bench-record                 # re-record bench/baseline.json, bench/history.json, docs/perf.md
make bench-report                 # regenerate docs/perf.md from the committed baseline
make bench-ladder                 # latency ladder vs src/bench-budget.json
make tty-check                    # real emacs -nw inside tmux -L eas
make melpa-check                  # recipe installed flat: compile, doctor, package-lint (network)
```

- Run the gallery one group at a time. Never use `make -j` on
  `test-gallery`: a gallery group takes minutes and a lot of memory.
- tmux: only use the private server `tmux -L eas`. Never run
  `tmux kill-server`, `killall tmux` or `pkill tmux`, and never touch
  the default tmux server.
- `TEST_SKIP_LOG=FILE` appends every skipped test with its reason. A
  skip is not a pass. Report skips with their reasons.
- `EAS_UPDATE_GOLDEN=1 make test` (or `make test-gallery-conformance`)
  rewrites goldens. Review the diff before you commit it.

## Rules

- Lisp never knows about a domain. Financial or health code belongs in
  its own package (financial-chart.el, health-charts.el) as templates
  and `eas-register-transform` transforms. `eas-has-no-references-to-its-host-package`
  enforces this.
- Renderers are dumb. Anything a renderer would need to decide belongs
  in compile (scene/v1), so the SVG and text backends agree by
  construction (`eas-renderers-read-only-the-scene`).
- Failures are data. Signal through `eas-signal` with a reason code from
  `eas-reason-codes`. Each error is a `define-error` child of
  `eas-error` with data `(MESSAGE :code CODE ...)`. Agent verbs never
  signal: they return an envelope with `next` commands.
- A feature counts as supported only when a conformance spec proves it.
  Documented Vega-Lite properties that the renderers do not draw must
  show up as `UNSUPPORTED_FEATURE` warnings (`eas-spec-props.el`).
- Interaction uses Vega-Lite's params grammar. Reducers are pure
  `(state, event, scene) -> state`, so every interaction can be tested
  in `--batch` through `eas-dispatch`.
- The code must stay checkdoc-clean and byte-compile-clean, with
  warnings as errors. Keep docstring lines within 80 columns.
- `lexical-binding: t`, the `eas-` prefix (`eas--` for internals), and
  a GPL-3.0-or-later SPDX header on every file.
- bin/chart is the test oracle and the export door. It is never a
  runtime dependency. Tests that need it or `rsvg-convert` skip and give
  the reason.
- Commits use conventional commit messages. Never push to main.

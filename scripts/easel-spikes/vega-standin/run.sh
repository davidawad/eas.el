#!/usr/bin/env bash
# Measure easel's native SVG against Vega-Lite 6.4.1 for every gallery
# spec, both rasterized by resvg with DejaVu Sans, diffed by pixelmatch.
# A STAND-IN for bin/chart, used to find geometry bugs; thresholds in
# the gallery come from it until bin/chart itself is run.
#   (cd scripts/easel-spikes/vega-standin && npm install) && scripts/easel-spikes/vega-standin/run.sh OUT-DIR
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
out="$(realpath "${1:-/tmp/easel-standin}")"
mkdir -p "$out"
"${EMACS:-emacs}" -Q --batch -L "$root/src/easel" -l "$here/native-svgs.el" "$out"
for spec in "$root"/test/conformance/*.vl.json; do
  n=$(basename "$spec" .vl.json)
  node "$here/vl2svg.mjs" "$spec" "$out/$n.ref.svg"
  node "$here/svg2png.js" "$out/$n.ref.svg" "$out/$n.ref.png"
  node "$here/svg2png.js" "$out/$n.native.svg" "$out/$n.native.png"
  printf '%s %s\n' "$n" "$(node "$here/pngdiff.mjs" "$out/$n.native.png" "$out/$n.ref.png" "$out/$n.diff.png")"
done

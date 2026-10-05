#!/usr/bin/env bash
# Run an easel spike against byte-compiled sources, as an installed
# package would run (interpreted .el is several times slower).
#   scripts/easel-spikes/run-compiled.sh scripts/easel-spikes/dispatch-cost.el
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
find "$root/src/easel" -name '*.el' ! -name '*-test.el' -exec cp {} "$tmp" \;
mkdir -p "$tmp/templates" && cp "$root"/templates/*.json "$tmp/templates/" 2> /dev/null || true
(cd "$tmp" && "${EMACS:-emacs}" -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
"${EMACS:-emacs}" -Q --batch -L "$tmp" -l "$1"

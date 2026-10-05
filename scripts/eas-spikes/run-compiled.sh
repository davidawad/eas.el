#!/usr/bin/env bash
# Run an eas spike against byte-compiled sources, as an installed
# package would run (interpreted .el is several times slower).
#   scripts/eas-spikes/run-compiled.sh scripts/eas-spikes/dispatch-cost.el
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
find "$root/src/eas" -name '*.el' ! -name '*-test.el' -exec cp {} "$tmp" \;
mkdir -p "$tmp/templates" && cp "$root"/templates/*.json "$tmp/templates/" 2> /dev/null || true
(cd "$tmp" && "${EMACS:-emacs}" -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
"${EMACS:-emacs}" -Q --batch -L "$tmp" -l "$1"

#!/usr/bin/env bash
# Run one NS GUI spike in the macOS GUI Emacs against byte-compiled eas sources.
# The spike writes results to $SPIKE_OUT and calls `kill-emacs'.
#   scripts/eas-spikes/gui/run-ns.sh ns-raster.el /tmp/ns-raster.out
# Light by design: nice 19, one Emacs with -Q (no server), a timeout.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
EMACS_GUI="${EMACS_GUI:-/opt/homebrew/Cellar/emacs-plus@30/30.2/Emacs.app/Contents/MacOS/Emacs}"
tmp="$(mktemp -d)"
trap 'trash "$tmp" 2> /dev/null || true' EXIT
# eas finds templates/ one level above the directory it loads from.
mkdir -p "$tmp/src" "$tmp/templates"
find "$root/src" -name '*.el' ! -name '*-test.el' -exec cp {} "$tmp/src" \;
cp -R "$root"/templates/* "$tmp/templates/"
cp -R "$root/examples" "$tmp/examples"
mkdir -p "$tmp/test" && ln -s "$root/test/vega-examples" "$tmp/test/vega-examples"
(cd "$tmp/src" && nice -n 19 emacs -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
out="${2:-$here/$(basename "$1" .el).out}"
: > "$out"
SPIKE_ROOT="$root" SPIKE_OUT="$out" nice -n 19 timeout "${SPIKE_TIMEOUT:-300}" "$EMACS_GUI" -Q -L "$tmp/src" \
  -l "$here/ns-common.el" -l "$here/$1" > "$out.stderr" 2>&1 || echo "emacs exited $?" >&2
cat "$out"

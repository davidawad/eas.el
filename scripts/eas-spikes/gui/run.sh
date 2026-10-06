#!/usr/bin/env bash
# Run one GUI spike in emacs-lucid on an Xvfb display, against
# byte-compiled eas sources.  The spike writes its results to
# $SPIKE_OUT and calls `kill-emacs' when done.
#   scripts/eas-spikes/gui/run.sh raster.el /work/artifacts/raster.out
# Needs setup-linux.sh (or any X Emacs with librsvg: set EMACS_GUI).
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
# shellcheck disable=SC1091
[ -f "${PREFIX:-$HOME/gui}/env.sh" ] && . "${PREFIX:-$HOME/gui}/env.sh"
export DISPLAY="${DISPLAY:-:99}"
mkdir -p /tmp/xb && ln -sf "$(command -v xkbcomp)" /tmp/xb/xkbcomp
if ! xdotool getdisplaygeometry > /dev/null 2>&1; then
  Xvfb "$DISPLAY" -screen 0 1600x1000x24 -nolisten tcp ${XKBDIR:+-xkbdir "$XKBDIR"} > /tmp/xvfb.log 2>&1 &
  sleep 2
fi
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
find "$root/src" -name '*.el' ! -name '*-test.el' -exec cp {} "$tmp" \;
mkdir -p "$tmp/templates" && cp "$root"/templates/*.json "$tmp/templates/" 2> /dev/null || true
(cd "$tmp" && emacs -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
out="$(realpath "${2:-$here/$(basename "$1" .el).out}")"
: > "$out"
SPIKE_OUT="$out" timeout "${SPIKE_TIMEOUT:-900}" $EMACS_GUI -Q -L "$tmp" -l "$here/common.el" -l "$here/$1" > "$out.stderr" 2>&1 || echo "emacs exited $?" >&2
cat "$out"

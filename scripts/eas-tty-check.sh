#!/usr/bin/env bash
# Real-terminal check of eas text charts (fc-qx1.53).
#
# Runs `emacs -nw' in a detached tmux pane (private server `-L eas',
# never the default one) at COLSxROWS (default 189x56), shows
#   full:  a candlestick chart (the gallery's layered/layer_candlestick
#          at container width) alone in the frame;
#   split, split-mirror: 2x2 splits of that chart and the gallery
#          examples layered/layer_bar_annotations and
#          distributions/layer_point_errorbar_ci, each in a window
#          with and without a right neighbour,
# captures the pane and asserts:
#   - no window line ends in the truncation (`$') or continuation (`\')
#     glyph, the batch fake window cannot see this;
#   - full: the candlestick plot (its x axis rule, y axis included) spans at
#     least 90% of the window width.
# Exit 0 when every assertion holds, 1 otherwise.  Needs tmux and python3.
#
#   scripts/eas-tty-check.sh            # from the repository root
#   EMACS=emacs-30 scripts/eas-tty-check.sh 120 40
#   EAS_TTY_CHECK_KEEP=DIR keeps each layout's capture as DIR/LAYOUT.txt
set -euo pipefail

cd "$(dirname "$0")/.."
EMACS=${EMACS:-emacs}
COLS=${1:-189}
ROWS=${2:-56}
TMUX_EAS=(tmux -L eas)
SESSION=eas-tty-check
WORK=$(mktemp -d)
trap '"${TMUX_EAS[@]}" kill-session -t "$SESSION" 2>/dev/null || true; rm -rf "$WORK"' EXIT

fail=0

run_layout() {
  local layout=$1 edges="$WORK/$1.edges" capture="$WORK/$1.txt"
  "${TMUX_EAS[@]}" kill-session -t "$SESSION" 2>/dev/null || true
  "${TMUX_EAS[@]}" new-session -d -s "$SESSION" -x "$COLS" -y "$ROWS" \
    "TERM=xterm-256color $EMACS -nw -Q -L src -L scripts -l eas-tty-check \
       --eval '(setq eas-tty-check-edges-file \"$edges\")' -f eas-tty-check-$layout"
  for _ in $(seq 1 100); do
    [ -s "$edges" ] && break
    sleep 0.2
  done
  if [ ! -s "$edges" ]; then
    echo "FAIL $layout: no window edges written (emacs did not start the layout)"
    "${TMUX_EAS[@]}" capture-pane -p -t "$SESSION" || true
    fail=1
    return
  fi
  sleep 0.5
  "${TMUX_EAS[@]}" capture-pane -p -t "$SESSION" > "$capture"
  [ -n "${EAS_TTY_CHECK_KEEP:-}" ] && cp "$capture" "$EAS_TTY_CHECK_KEEP/$layout.txt"
  "${TMUX_EAS[@]}" kill-session -t "$SESSION" 2>/dev/null || true
  if ! python3 -I - "$layout" "$capture" "$edges" "$COLS" <<'PY'
import sys
layout, capture, edges, cols = sys.argv[1], sys.argv[2], sys.argv[3], int(sys.argv[4])
lines = [l.ljust(cols) for l in open(capture, encoding="utf-8").read().split("\n")]
ok = True
for row in open(edges, encoding="utf-8").read().split("\n"):
    if not row.strip():
        continue
    l, t, r, b, vid = row.split(None, 4)
    l, t, r, b = int(l), int(t), int(r), int(b)
    # A window with a neighbour to its right ends in the border column.
    last = r - 1 if r >= cols else r - 2
    width = last - l + 1
    body = lines[t:b - 1]  # without the mode line
    for i, line in enumerate(body):
        if line[last] in "$\\":
            print(f"FAIL {layout} {vid}: line {t + i} ends in {line[last]!r}: {line[l:last + 1].rstrip()!r}")
            ok = False
    spans = []
    for line in body:
        seg = line[l:last + 1]
        k = seg.find("└")
        if k >= 0 and seg[k + 1:k + 2] in ("─", "┬"):
            spans.append(len(seg[k:].rstrip(" ")))
    span = max(spans) if spans else 0
    print(f"{layout} {vid}: {width} cols, {len(body)} rows, x axis spans {span} ({100 * span / width:.0f}%)")
    if layout == "full" and vid.startswith("candlestick") and span < 0.9 * width:
        print(f"FAIL {layout} {vid}: plot spans {span} < 90% of {width} cols")
        ok = False
sys.exit(0 if ok else 1)
PY
  then
    echo "--- $layout capture:"
    cat "$capture"
    fail=1
  fi
}

run_layout full
run_layout split
run_layout split-mirror
if [ "$fail" = 0 ]; then echo "eas-tty-check: ok (${COLS}x${ROWS})"; else echo "eas-tty-check: FAILED"; fi
exit "$fail"

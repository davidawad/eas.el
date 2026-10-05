#!/usr/bin/env bash
# fc-qx1.23: mouse through tmux as a *client* sees it.  tty-mouse.sh
# injects reports straight into Emacs's pane; here Emacs runs in an
# inner tmux server and an inner tmux *client* is attached from a pane
# of an outer tmux, which stands in for the user's terminal.  Reports
# are injected as terminal input to that client, so the inner server has
# to parse them and re-encode them for Emacs's pane, as it would for a
# real terminal.  Run once per inner `mouse` option (on, off).
#   scripts/easel-spikes/tmux-forward.sh OUT
set -euo pipefail
out="$(realpath "${1:-tmux-forward.out}")"
here="$(cd "$(dirname "$0")" && pwd)"
: > "$out"
for mouse in on off; do
  inner="easel-in-$$-$mouse" outer="easel-out-$$-$mouse" log="$out.$mouse"
  : > "$log"
  tmux -L "$inner" -f /dev/null new-session -d -s in -x 118 -y 38 \
    "SPIKE_OUT='$log' emacs -nw -Q -l '$here/tty-mouse.el'"
  tmux -L "$inner" set -g mouse "$mouse"
  tmux -L "$outer" -f /dev/null new-session -d -s out -x 120 -y 40 "tmux -L $inner attach -t in"
  tmux -L "$outer" pipe-pane -t out -o "cat > '$log.raw'"
  sleep 4
  # Same reports as tty-mouse.sh: click at (10,5), press-drag-release to
  # (20,5), no-button motion to (30,8), wheel up.
  for seq in '\e[<0;10;5M' '\e[<0;10;5m' '\e[<0;10;5M' '\e[<32;20;5M' '\e[<0;20;5m' \
    '\e[<35;30;8M' '\e[<64;30;8M'; do
    tmux -L "$outer" send-keys -t out -l "$(printf %b "$seq")"
    sleep 0.3
  done
  # What the inner server asked the "terminal" (outer pane) to enable.
  modes="$(grep -ao $'\e\\[?[0-9;]*[hl]' "$log.raw" | tr -d '\033' | sort -u | tr '\n' ' ' || true)"
  sleep 1
  { echo "## inner tmux mouse=$mouse; inner server enabled on the client terminal: ${modes:-none}"
    cat "$log"; } >> "$out"
  tmux -L "$outer" kill-server 2> /dev/null || true
  tmux -L "$inner" kill-server 2> /dev/null || true
done
cat "$out"

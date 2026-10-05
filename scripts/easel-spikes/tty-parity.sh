#!/usr/bin/env bash
# End-to-end: easel in emacs -nw under tmux, real xterm-mouse decoding of
# injected SGR reports plus keyboard, then parity of the recorded log.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
out="$(realpath "${1:-tty-parity.out}")"; rm -f "$out" "$out.cells"
sess="easel-e2e-$$"
tmux new-session -d -s "$sess" -x 120 -y 40 -e TERM=tmux-256color
tmux send-keys -t "$sess" "E2E_OUT='$out' emacs -nw -Q -l '$here/tty-parity.el'" Enter
for _ in $(seq 30); do [ -f "$out.cells" ] && break; sleep 0.5; done
read -r a b c < "$out.cells"
sleep 1
# hover (motion, no button) over datum 2's cell; press on datum 1, drag
# with button held to datum 4, release (brush); then keyboard: z (zoom to
# brush), [ (undo), n (step hover), RET (click at point), wheel up at datum 2.
for seq in "\e[<35;${c}M" "\e[<0;${a}M" "\e[<32;${b}M" "\e[<0;${b}m"; do
  tmux send-keys -t "$sess" -l "$(printf %b "$seq")"; sleep 0.4
done
for k in z '[' n Enter; do tmux send-keys -t "$sess" "$k"; sleep 0.4; done
tmux send-keys -t "$sess" -l "$(printf %b "\e[<64;${c}M")"; sleep 0.4
tmux capture-pane -t "$sess" -p > "$out.screen"
tmux send-keys -t "$sess" F5
for _ in $(seq 30); do [ -f "$out" ] && break; sleep 0.5; done
tmux kill-session -t "$sess" 2> /dev/null || true
cat "$out"

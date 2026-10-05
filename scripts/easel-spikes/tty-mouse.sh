#!/usr/bin/env bash
# Spike: what Emacs -nw asks the terminal for under xterm-mouse-mode,
# which injected mouse reports it decodes, and grid redisplay cost.
# Runs Emacs inside a detached tmux session. Output: $1 (default
# ./tty-mouse.out) plus the raw terminal byte stream in $1.raw.
set -euo pipefail
out="$(realpath "${1:-tty-mouse.out}")"
here="$(cd "$(dirname "$0")" && pwd)"
sess="easel-spike-$$"
: > "$out"
tmux new-session -d -s "$sess" -x 120 -y 40
tmux pipe-pane -t "$sess" -o "cat > '$out.raw'"
tmux send-keys -t "$sess" "SPIKE_OUT='$out' emacs -nw -Q -l '$here/tty-mouse.el'" Enter
sleep 3
# SGR (1006) reports: press/release button 1 at col 10,row 5; motion with
# button 1 held (32+0) to col 20; motion with no button (32+3); wheel up (64).
for seq in '\e[<0;10;5M' '\e[<0;10;5m' '\e[<0;10;5M' '\e[<32;20;5M' '\e[<0;20;5m' \
  '\e[<35;30;8M' '\e[<64;30;8M'; do
  tmux send-keys -t "$sess" -l "$(printf %b "$seq")"
  sleep 0.3
done
tmux send-keys -t "$sess" F5
sleep 8
tmux kill-session -t "$sess" 2> /dev/null || true

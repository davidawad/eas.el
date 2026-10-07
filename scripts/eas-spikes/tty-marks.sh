#!/usr/bin/env bash
# Spike (fc-qx1.24): do xterm-mouse clicks and hover land on the right
# marks?  Runs `emacs -nw -Q` in a private tmux server (-L eas-spike),
# opens the "bars" template as text, then sends SGR (1006) mouse reports
# for each bar's centre cell: a motion report (hover), a press and a
# release (click).  Output: OUT (default tty-marks.out), with the plan
# (what each bar's cell is) and what Emacs decoded.
#
#   tty-marks.sh [OUT]
#
# With TTY_CLIENT=iterm an iTerm2 window is attached to the same tmux
# server and the reports are typed into that window's session (iTerm's
# AppleScript `write text`), so they travel iTerm -> tmux client ->
# tmux server -> Emacs.  Real clicks in that window need a person.
#
# Light: one Emacs, one tmux server, killed at the end (only -L eas-spike).
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../.." && pwd)"
out="${1:-$here/tty-marks.out}"
case "$out" in /*) ;; *) out="$PWD/$out" ;; esac
plan="$out.plan"
sock=eas-spike
# The binary itself: `emacs' is a shell alias in the login shell.
EMACS_TTY="${EMACS_TTY:-/opt/homebrew/Cellar/emacs-plus@30/30.2/Emacs.app/Contents/MacOS/Emacs}"
# -f /dev/null: the login ~/.tmux.conf hangs a fresh private server here.
t="tmux -L $sock -f /dev/null"
tmp="$(mktemp -d)"
iterm_win=
iterm_was_running=0
pgrep -x iTerm2 > /dev/null && iterm_was_running=1
cleanup() {
  if [ -n "$iterm_win" ]; then
    osascript -e "tell application \"iTerm\" to close (first window whose id is $iterm_win)" > /dev/null 2>&1 || true
    [ "$iterm_was_running" = 0 ] && osascript -e 'tell application "iTerm" to quit' > /dev/null 2>&1
  fi
  $t kill-server 2> /dev/null || true
  trash "$tmp" 2> /dev/null || true
}
trap cleanup EXIT
mkdir -p "$tmp/src"
find "$root/src" -name '*.el' ! -name '*-test.el' -exec cp {} "$tmp/src" \;
cp -R "$root/templates" "$tmp/templates"
cp -R "$root/examples" "$tmp/examples"
(cd "$tmp/src" && nice -n 19 emacs -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
: > "$out"
: > "$plan"
$t kill-server 2> /dev/null || true
# Emacs is the pane's command: no login shell to wait for (its startup is slow here).
$t new-session -d -s s -x 120 -y 40 \
  "env TERM=xterm-256color SPIKE_OUT='$out' SPIKE_PLAN='$plan' nice -n 19 '$EMACS_TTY' -nw -Q -L '$tmp/src' -l '$here/tty-marks.el'"
for _ in $(seq 40); do
  [ -s "$plan" ] && break
  sleep 0.5
done
sleep 1

if [ "${TTY_CLIENT:-}" = iterm ]; then
  # The pane keeps its 120x40 whatever the client's size.
  $t set-option -g window-size manual
  iterm_win="$(
    osascript << 'APPLESCRIPT'
tell application "iTerm"
  set w to (create window with default profile command "/opt/homebrew/bin/tmux -L eas-spike attach -t s")
  tell current session of w
    set columns to 120
    set rows to 40
  end tell
  return id of w
end tell
APPLESCRIPT
  )"
  echo "iterm window $iterm_win: reports are typed into iTerm's session, not clicked" >> "$out"
  sleep 4
  send() {
    osascript - "$(printf '\033[<%s;%s;%s%s' "$1" "$2" "$3" "$4")" << 'APPLESCRIPT'
on run argv
  tell application "iTerm" to tell current session of current window to write text (item 1 of argv) newline NO
end run
APPLESCRIPT
    sleep 0.25
  }
else
  send() {
    $t send-keys -t s -l "$(printf '\033[<%s;%s;%s%s' "$1" "$2" "$3" "$4")"
    sleep 0.25
  }
fi

while read -r datum col row; do
  echo "plan datum=$datum col=$col row=$row" >> "$out"
  send 35 "$col" "$row" M # motion, no button: hover
  send 0 "$col" "$row" M  # press button 1
  send 0 "$col" "$row" m  # release: click
done < "$plan"
# A report over the margin (outside any bar) must not hover a bar.
echo "plan datum=none col=1 row=3" >> "$out"
send 35 1 3 M
$t send-keys -t s F5
sleep 1
cat "$out"

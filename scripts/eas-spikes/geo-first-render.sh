#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# eas-gzi: first render of the map templates in a fresh Emacs, per
# compile mode and geo backend, best of N alternating runs.
#   scripts/eas-spikes/geo-first-render.sh [N] [MODES] [BACKENDS] [TEMPLATES]
# MODES "byte native", BACKENDS "lisp native", N 6 by default.  The
# compiled copies are kept in $EAS_FR_BUILD (default a temporary dir).
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
n="${1:-6}"
modes="${2:-byte native}"
backends="${3:-lisp native}"
templates="${4:-projections county-unemployment map-with-tooltip world-map}"
emacs="${EMACS:-emacs}"
build="${EAS_FR_BUILD:-$(mktemp -d)}"
export TZ=UTC
for mode in $modes; do
  b="$build/$mode"
  rm -rf "$b"; mkdir -p "$b/src" "$b/eln"
  find "$root/src" -maxdepth 1 \( -name '*.el' ! -name '*-test.el' -o -name '*.json' \) -exec cp {} "$b/src" \;
  for d in templates examples lib module; do if [ -e "$root/$d" ]; then ln -s "$root/$d" "$b/$d"; fi; done
  (cd "$b/src" && "$emacs" -Q --batch -L . -f batch-byte-compile ./*.el > "$b/compile.log" 2>&1)
  if [ "$mode" = native ]; then
    (cd "$b/src" && ls ./*.el | xargs -P "$(nproc)" -n 16 "$emacs" -Q --batch -L . \
      --eval "(setq native-comp-eln-load-path (list \"$b/eln/\"))" \
      --eval '(dolist (f command-line-args-left) (native-compile f))' >> "$b/compile.log" 2>&1)
  fi
done
run() { # MODE BACKEND TEMPLATE
  local b="$build/$1"
  EAS_FR_TEMPLATE="$3" EAS_GEO_BACKEND="$2" "$emacs" -Q --batch \
    --eval "(setq native-comp-eln-load-path (cons \"$b/eln/\" native-comp-eln-load-path) native-comp-jit-compilation nil)" \
    -L "$b/src" -l "$root/scripts/eas-spikes/geo-first-render.el" 2>/dev/null
}
out="$(mktemp)"
for i in $(seq "$n"); do
  for t in $templates; do for mode in $modes; do for be in $backends; do
    echo "$mode $be $(run "$mode" "$be" "$t")" >> "$out"
  done; done; done
done
# Best of N: the minimum total, and the minimum render, per configuration.
awk '{k=$3" "$1" "$2; if (!(k in tot) || $5<tot[k]) tot[k]=$5; if (!(k in ren) || $7<ren[k]) ren[k]=$7;
      if (!(k in tg) || $12<tg[k]) tg[k]=$12; md[k]=$NF}
     END {for (k in tot) printf "%-40s total %5d render %5d trailing-gc %4d md5 %s\n", k, tot[k], ren[k], tg[k], md[k]}' "$out" | sort

#!/usr/bin/env bash
# The standing performance suite (eas-b2s.4, src/eas-perf.el).
#   scripts/eas-perf.sh report [REGEX]   run, compare with bench/baseline.json, never fail
#   scripts/eas-perf.sh check  [REGEX]   run, compare, exit 1 on a gated regression
#   scripts/eas-perf.sh record           run, rewrite bench/baseline.json and
#                                        bench/history.json, regenerate docs/perf.md
#   scripts/eas-perf.sh doc              regenerate docs/perf.md from the baseline only
# PERF_MODES (default "byte native") picks the compilation modes; a mode this
# Emacs cannot build is skipped with its reason (native needs native-comp).
# REGEX selects workloads (e.g. '^ladder' or '^render/'); check then ignores the
# workloads it did not run.  EAS_PERF_FRAMES overrides the frames per live
# workload (record keeps the default).  PERF_OUT=DIR keeps the run files;
# PERF_BASELINE=FILE compares with (or records into) another baseline.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
verb="${1:?usage: scripts/eas-perf.sh report|check|record|doc [REGEX]}"
only="${2:-}"
modes="${PERF_MODES:-byte native}"
emacs="${EMACS:-emacs}"
baseline="${PERF_BASELINE:-$root/bench/baseline.json}"
history="$root/bench/history.json"
doc="$root/docs/perf.md"
# Date and time functions read the zone; fix it so every run draws the same.
export TZ=UTC
gate() { "$emacs" -Q --batch -L "$root/src" -l eas-perf-gate -f "$@"; }

if [ "$verb" = doc ]; then
  gate eas-perf-batch-report "$baseline" "$history" "$doc"
  exit 0
fi
case "$verb" in report|check|record) ;; *) echo "unknown verb $verb" >&2; exit 2 ;; esac
if [ "$verb" = record ] && [ -n "$only" ]; then
  echo "record runs every workload; drop REGEX" >&2; exit 2
fi
if [ "$verb" = record ]; then unset EAS_PERF_FRAMES; fi

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
out="${PERF_OUT:-$tmp}"
mkdir -p "$out"
status=0
start=$(date +%s)
for mode in $modes; do
  build="$tmp/$mode"
  mkdir -p "$build/src" "$build/eln"
  find "$root/src" -maxdepth 1 \( -name '*.el' ! -name '*-test.el' -o -name '*.json' \) \
    -exec cp {} "$build/src" \;
  # The engine finds templates/ and examples/ one level above its files.
  ln -s "$root/templates" "$build/templates"
  ln -s "$root/examples" "$build/examples"
  # The native geo module, when built, is found under the root (cold-native/).
  for d in lib module; do if [ -e "$root/$d" ]; then ln -s "$root/$d" "$build/$d"; fi; done
  (cd "$build/src" && "$emacs" -Q --batch -L . -f batch-byte-compile ./*.el > "$build/compile.log" 2>&1)
  # No JIT: a byte run must not turn native halfway through.
  eln=(--eval "(when (boundp 'native-comp-jit-compilation) (setq native-comp-jit-compilation nil))")
  case "$mode" in
    byte) ;;
    native)
      if ! "$emacs" -Q --batch --eval '(kill-emacs (if (native-comp-available-p) 0 1))'; then
        echo "eas-perf native: skipped: this Emacs has no native compilation" >&2
        continue
      fi
      (cd "$build/src" && ls ./*.el | xargs -P "$(nproc 2> /dev/null || echo 2)" -n 16 \
        "$emacs" -Q --batch -L . --eval "(setq native-comp-eln-load-path (list \"$build/eln/\"))" \
        --eval '(dolist (f command-line-args-left) (native-compile f))' >> "$build/compile.log" 2>&1)
      eln=(--eval "(setq native-comp-eln-load-path (cons \"$build/eln/\" native-comp-eln-load-path) native-comp-jit-compilation nil)")
      ;;
    *) echo "unknown mode $mode" >&2; exit 2 ;;
  esac
  echo "eas-perf $mode: running $(if [ -n "$only" ]; then echo "workloads matching $only"; else echo "every workload"; fi)" >&2
  "$emacs" -Q --batch "${eln[@]}" -L "$build/src" -l eas-perf-gate \
    -f eas-perf-batch-run "$out/run-$mode.json" "$only"
  ran="$(grep -o '"mode": *"[a-z]*"' "$out/run-$mode.json" | head -1 | sed 's/.*"\([a-z]*\)"$/\1/')"
  if [ "$ran" != "$mode" ]; then
    echo "eas-perf $mode: the engine ran $ran, not $mode" >&2; exit 1
  fi
  case "$verb" in
    report) gate eas-perf-batch-check "$baseline" "$out/run-$mode.json" report ;;
    check) gate eas-perf-batch-check "$baseline" "$out/run-$mode.json" ${only:+partial} || status=1 ;;
    record) gate eas-perf-batch-record "$baseline" "$history" "$out/run-$mode.json" ;;
  esac
done
if [ "$verb" = record ]; then gate eas-perf-batch-report "$baseline" "$history" "$doc"; fi
echo "eas-perf: $verb took $(( $(date +%s) - start ))s" >&2
exit "$status"

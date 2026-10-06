#!/usr/bin/env bash
# Bench every example of a test/vl-examples group with the bench verb,
# byte-compiled (fc-qx1.42), and write GROUP/bench.json.
#   scripts/eas-gallery-bench.sh GROUP [REPS] [BASELINE.json]
# BASELINE is an earlier bench.json-shaped result (e.g. this script run
# on the parent commit with BENCH_OUT set) whose means become baseline_ms.
# BENCH_OUT=FILE writes the result there instead of GROUP/bench.json.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
group="${1:?usage: scripts/eas-gallery-bench.sh GROUP [REPS] [BASELINE.json]}"
reps="${2:-10}"
baseline="${3:-}"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/src"
find "$root/src" -maxdepth 1 \( -name '*.el' ! -name '*-test.el' -o -name '*.json' \) \
  -exec cp {} "$tmp/src" \;
ln -s "$root/templates" "$tmp/templates"
ln -s "$root/test" "$tmp/test"
(cd "$tmp/src" && "${EMACS:-emacs}" -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
"${EMACS:-emacs}" -Q --batch -L "$tmp/src" -l eas -l eas-vl-gallery-bench --eval "
(let* ((base (and (> (length \"$baseline\") 0) \"$baseline\"))
       (out (getenv \"BENCH_OUT\")))
  (if out
      (with-temp-file out (insert (eas-json-pretty (eas-vl-gallery-bench \"$group\" $reps)) \"\n\"))
    (eas-vl-gallery-bench-write \"$group\" $reps base))
  (princ (format \"wrote %s\n\" (or out (eas-vl-gallery-bench-file \"$group\")))))"

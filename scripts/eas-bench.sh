#!/usr/bin/env bash
# The fc-qx1.9 performance ladder against byte-compiled eas, as an
# installed package runs (interpreted .el is 4-10x slower).
#   scripts/eas-bench.sh [bench options]   check src/bench-budget.json;
#                                            exit 1 on BUDGET_EXCEEDED
#   scripts/eas-bench.sh --update          re-measure the budget's references
# The chart/v1 envelope goes to stdout; BENCH_OUT=FILE also writes it there.
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT
mkdir -p "$tmp/src" "$tmp/templates"
find "$root/src" -maxdepth 1 \( -name '*.el' ! -name '*-test.el' -o -name '*.json' \) \
  -exec cp {} "$tmp/src" \;
cp "$root"/templates/*.json "$tmp/templates/" 2> /dev/null || true
(cd "$tmp/src" && "${EMACS:-emacs}" -Q --batch -L . -f batch-byte-compile ./*.el > /dev/null 2>&1)
budget="$root/src/bench-budget.json"
if [ "${1:-}" = "--update" ]; then
  shift
  "${EMACS:-emacs}" -Q --batch -L "$tmp/src" -l eas-agent --eval "
(let* ((budget (eas-bench-read-budget \"$budget\"))
       (result (eas-bench-ladder)))
  (with-temp-file \"$budget\"
    (insert (eas-json-pretty (eas-bench-budget-from result budget)) \"\n\"))
  (princ (eas-json-pretty result)))"
  echo "updated $budget; review the diff" >&2
  exit 0
fi
status=0
out="$("${EMACS:-emacs}" -Q --batch -L "$tmp/src" -l eas-agent-cli -f eas-agent-cli-main -- \
  bench --budget-file "$budget" "$@")" || status=$?
printf '%s\n' "$out"
if [ -n "${BENCH_OUT:-}" ]; then printf '%s\n' "$out" > "$BENCH_OUT"; fi
# A skipped comparison (interpreted run) must not pass as a checked one.
if [ "$status" = 0 ] && printf '%s' "$out" | grep -q '"status": *"skipped"'; then
  echo "eas-bench: budget not checked (see budget.reason)" >&2
  exit 1
fi
exit "$status"

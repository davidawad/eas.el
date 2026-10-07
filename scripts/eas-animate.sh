#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# An animated GIF of a template playing: batch Emacs opens TEMPLATE with
# its example bindings, sets PARAMS (a JSON object, e.g. '{"month": 1}'),
# and writes one SVG per x-eas timer tick; scripts/eas-animate-gif.mjs
# rasterizes the frames with @resvg/resvg-js and encodes them with
# gifenc (pure JS).  Install those two once anywhere and point NODE_PATH
# at its node_modules; FONT names a TTF for the text (default: the first
# of DejaVuSans or Arimo found by fc-match).
#   scripts/eas-animate.sh TEMPLATE OUT.gif [FRAMES [DELAY_MS [PARAMS]]]
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
template="$1"; out="$2"; frames="${3:-48}"; delay="${4:-250}"; params="${5:-{\}}"
dir="$(mktemp -d)"
trap 'rm -rf "$dir"' EXIT
"${EMACS:-emacs}" -Q --batch -L "$root/src" -l eas --eval "
(let* ((name (pop command-line-args-left))
       (dir (pop command-line-args-left))
       (frames (string-to-number (pop command-line-args-left)))
       (params (eas-json-parse (pop command-line-args-left)))
       (view (eas-view-open name :bindings (eas-template-example name) :id \"animate\")))
  (when params (eas-dispatch view (list :type \"params\" :values params)))
  (dotimes (i frames)
    (with-temp-file (expand-file-name (format \"frame-%04d.svg\" i) dir)
      (insert (eas-svg-render (eas-view-scene view))))
    (eas-play-tick view (float (1+ i)))))" "$template" "$dir" "$frames" "$params"
node "$root/scripts/eas-animate-gif.mjs" "$dir" "$out" "$delay" ${FONT:+"$FONT"}
echo "$out: $frames frames"

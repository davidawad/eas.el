#!/usr/bin/env bash
# README screenshots: native SVG of a few gallery examples, rendered by
# eas itself (no Vega), into docs/images/NAME.svg, plus a text-backend
# capture of the first one into docs/images/NAME.txt.  With rsvg-convert
# on PATH (or RASTERIZE="CMD IN OUT"), each SVG is also rasterized to
# docs/images/NAME.png and the SVG removed.
#   scripts/eas-screenshots.sh [GROUP/NAME ...]
set -euo pipefail
root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out="$root/docs/images"
mkdir -p "$out"
if [ "$#" = 0 ]; then
  set -- layered/layer_candlestick line/line_color area-circular/arc_donut bar/bar_grouped
fi
"${EMACS:-emacs}" -Q --batch -L "$root/src" -l eas -l eas-text-gallery --eval "
(let ((first t))
  (dolist (example command-line-args-left)
    (pcase-let* ((\`(,group ,name) (split-string example \"/\"))
                 (spec (eas-text-gallery-spec group name)))
      (with-temp-file (expand-file-name (concat name \".svg\") \"$out\")
        (insert (eas-vl-gallery-svg spec)))
      (when first
        (setq first nil)
        (with-temp-file (expand-file-name (concat name \".txt\") \"$out\")
          (set-buffer-file-coding-system 'utf-8-unix)
          (insert (eas-vl-gallery-text spec '(:cols 80 :rows 24)))))
      (princ (format \"wrote %s.svg\n\" name))))
  (setq command-line-args-left nil))" "$@"
rasterize="${RASTERIZE:-}"
if [ -z "$rasterize" ] && command -v rsvg-convert > /dev/null; then
  for f in "$out"/*.svg; do rsvg-convert -b white -z 2 -o "${f%.svg}.png" "$f" && rm "$f"; done
elif [ -n "$rasterize" ]; then
  for f in "$out"/*.svg; do $rasterize "$f" "${f%.svg}.png" && rm "$f"; done
fi

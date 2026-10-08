#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# Fetch the oracle's fonts into DIR, once: Liberation Sans, Serif and
# Mono (Arial's, Times New Roman's and Courier New's metrics) and TeX
# Gyre Cursor (Courier's metrics and slab-serif shapes, which
# Liberation Mono lacks; fonts.conf draws Courier New with it).
#   scripts/eas-spikes/vega-oracle/fetch-fonts.sh DIR
set -euo pipefail
dir="$1"
mkdir -p "$dir"
if [ ! -f "$dir/LiberationSans-Regular.ttf" ]; then
  curl -sfL https://github.com/liberationfonts/liberation-fonts/files/7261482/liberation-fonts-ttf-2.1.5.tar.gz |
    tar -xz -C "$dir" --strip-components=1
fi
for face in regular bold italic bolditalic; do
  f="$dir/texgyrecursor-$face.otf"
  [ -f "$f" ] || curl -sfL -o "$f" "https://mirrors.ctan.org/fonts/tex-gyre/opentype/texgyrecursor-$face.otf"
done

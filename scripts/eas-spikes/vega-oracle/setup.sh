#!/usr/bin/env bash
# Install the oracle's node packages and Liberation fonts, and put a
# bin/rsvg-convert on the PATH it prints.  Needs node, npm and network.
#   eval "$(scripts/eas-spikes/vega-oracle/setup.sh)"
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cache="${EAS_ORACLE_CACHE:-$HOME/.cache/eas-vega-oracle}"
mkdir -p "$cache/bin" "$cache/fonts"
(cd "$here" && npm install --silent > /dev/null)
if [ ! -f "$cache/fonts/LiberationSans-Regular.ttf" ]; then
  curl -sL https://github.com/liberationfonts/liberation-fonts/files/7261482/liberation-fonts-ttf-2.1.5.tar.gz |
    tar -xz -C "$cache/fonts" --strip-components=1
fi
sed "s#FONTDIR#$cache/fonts#g" "$here/fonts.conf" > "$cache/fonts.conf"
printf '#!/bin/sh\nFONTCONFIG_FILE=%s exec node %s "$@"\n' "$cache/fonts.conf" "$here/rsvg-convert.cjs" > "$cache/bin/rsvg-convert"
chmod +x "$cache/bin/rsvg-convert"
echo "export PATH=\"$cache/bin:\$PATH\" FONTCONFIG_FILE=\"$cache/fonts.conf\" TZ=America/Chicago"

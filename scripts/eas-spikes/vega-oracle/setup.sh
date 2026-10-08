#!/usr/bin/env bash
# Install the oracle's node packages and fonts (fetch-fonts.sh), and put a
# bin/rsvg-convert on the PATH it prints.  Needs node, npm and network.
#   eval "$(scripts/eas-spikes/vega-oracle/setup.sh)"
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cache="${EAS_ORACLE_CACHE:-$HOME/.cache/eas-vega-oracle}"
mkdir -p "$cache/bin" "$cache/fonts"
(cd "$here" && npm install --silent > /dev/null)
"$here/fetch-fonts.sh" "$cache/fonts"
sed "s#FONTDIR#$cache/fonts#g" "$here/fonts.conf" > "$cache/fonts.conf"
printf '#!/bin/sh\nFONTCONFIG_FILE=%s exec node %s "$@"\n' "$cache/fonts.conf" "$here/rsvg-convert.cjs" > "$cache/bin/rsvg-convert"
chmod +x "$cache/bin/rsvg-convert"
echo "export PATH=\"$cache/bin:\$PATH\" FONTCONFIG_FILE=\"$cache/fonts.conf\" TZ=America/Chicago"

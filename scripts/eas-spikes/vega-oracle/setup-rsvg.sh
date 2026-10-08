#!/usr/bin/env bash
# SPDX-License-Identifier: GPL-3.0-or-later
# Install the distribution's rsvg-convert without root on a Debian or
# Ubuntu box, fetch the oracle's fonts (fetch-fonts.sh), and put a
# bin/rsvg-convert on the PATH it prints.  Fonts resolve through
# fonts.conf, as setup.sh's do.
# Needs apt-get, dpkg-deb and network.
#   eval "$(scripts/eas-spikes/vega-oracle/setup-rsvg.sh)"
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cache="${EAS_ORACLE_CACHE:-$HOME/.cache/eas-vega-oracle}/rsvg"
root="$cache/root"
if [ ! -x "$root/usr/bin/rsvg-convert" ]; then
  mkdir -p "$cache/apt/lists/partial" "$cache/apt/cache/archives/partial" "$cache/debs" "$cache/apt/conf.d" "$root"
  cat > "$cache/apt.conf" <<EOF
Dir::State "$cache/apt";
Dir::State::Lists "$cache/apt/lists";
Dir::State::status "/var/lib/dpkg/status";
Dir::Cache "$cache/apt/cache";
Dir::Cache::archives "$cache/apt/cache/archives/";
Dir::Etc::Parts "$cache/apt/conf.d";
Debug::NoLocking "true";
EOF
  export APT_CONFIG="$cache/apt.conf"
  apt-get -qq update > /dev/null
  pkgs=$(apt-get -s install --no-install-recommends librsvg2-bin | awk '/^Inst/ {print $2}')
  (cd "$cache/debs" && apt-get -qq download $pkgs > /dev/null)
  for deb in "$cache"/debs/*.deb; do dpkg-deb -x "$deb" "$root"; done
fi
fonts="$cache/fonts"
"$here/fetch-fonts.sh" "$fonts"
sed "s#FONTDIR#$fonts#g" "$here/fonts.conf" > "$cache/fonts.conf"
lib=$(dirname "$(find "$root" -name 'librsvg-2.so*' | head -1)")
mkdir -p "$cache/bin"
printf '#!/bin/sh\nLD_LIBRARY_PATH=%s${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH} FONTCONFIG_FILE=%s exec %s "$@"\n' \
  "$lib" "$cache/fonts.conf" "$root/usr/bin/rsvg-convert" > "$cache/bin/rsvg-convert"
chmod +x "$cache/bin/rsvg-convert"
echo "export PATH=\"$cache/bin:\$PATH\" FONTCONFIG_FILE=\"$cache/fonts.conf\" TZ=America/Chicago"

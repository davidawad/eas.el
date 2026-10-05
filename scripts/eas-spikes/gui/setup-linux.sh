#!/usr/bin/env bash
# fc-qx1.23: a GUI Emacs (X, Lucid, Cairo, librsvg) plus Xvfb and
# xdotool on a Debian trixie box WITHOUT root.  Packages are downloaded
# with a private apt state dir and unpacked under $PREFIX (default
# ~/gui); nothing is installed system-wide.  Writes $PREFIX/env.sh,
# which run.sh sources.
#
# Xvfb runs xkbcomp from a compiled-in /usr/bin, which an unpacked
# package cannot satisfy, so the copy under $PREFIX is patched to look
# in /tmp/xb (same string length) and /tmp/xb/xkbcomp is a symlink.
set -euo pipefail
PREFIX="${PREFIX:-$HOME/gui}"
APT="${APT_ROOT:-$HOME/apt}"
mkdir -p "$APT"/{state/lists/partial,cache/archives/partial,etc} "$PREFIX/fc"
cat > "$APT/etc/sources.list" <<SRC
deb http://deb.debian.org/debian trixie main
deb http://deb.debian.org/debian trixie-updates main
deb http://deb.debian.org/debian-security trixie-security main
SRC
opts=(-o "Dir::State=$APT/state" -o "Dir::Cache=$APT/cache" -o "Dir::Etc::SourceList=$APT/etc/sources.list"
      -o Dir::Etc::SourceParts=/dev/null -o Debug::NoLocking=1)
apt-get "${opts[@]}" update -q
apt-get "${opts[@]}" install -y -q --download-only --no-install-recommends \
  emacs-lucid xvfb xauth xdotool librsvg2-2
(cd "$APT/cache/archives" && apt-get "${opts[@]}" download fonts-dejavu-core)
for deb in "$APT"/cache/archives/*.deb; do dpkg-deb -x "$deb" "$PREFIX"; done
python3 - "$PREFIX/usr/bin/Xvfb" <<'PY'
import sys
p = sys.argv[1]; b = open(p, 'rb').read()
if b'\0/tmp/xb\0' not in b:
    assert b.count(b'\0/usr/bin\0') == 1, 'unexpected Xvfb layout'
    open(p, 'wb').write(b.replace(b'\0/usr/bin\0', b'\0/tmp/xb\0\0'))
PY
cat > "$PREFIX/fc/fonts.conf" <<FC
<?xml version="1.0"?><!DOCTYPE fontconfig SYSTEM "fonts.dtd">
<fontconfig><dir>$PREFIX/usr/share/fonts</dir><cachedir>$PREFIX/fc/cache</cachedir>
<include ignore_missing="yes">$PREFIX/etc/fonts/conf.d</include></fontconfig>
FC
pdmp="$(ls "$PREFIX"/usr/libexec/emacs/*/x86_64-linux-gnu/*.pdmp)"
cat > "$PREFIX/env.sh" <<ENV
export LD_LIBRARY_PATH="$PREFIX/usr/lib/x86_64-linux-gnu" PATH="$PREFIX/usr/bin:\$PATH"
export FONTCONFIG_FILE="$PREFIX/fc/fonts.conf" XKBDIR="$PREFIX/usr/share/X11/xkb"
EMACS_GUI="$PREFIX/usr/bin/emacs-lucid --dump-file $pdmp"
ENV
echo "wrote $PREFIX/env.sh"

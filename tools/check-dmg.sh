#!/bin/bash
# tools/check-dmg.sh DMG -- that a disk image holds an application that works.
#
# Mounts DMG read-only, checks what is in it and the application's signature,
# then starts the application from the mounted image -- as someone who has
# just downloaded it would, short of copying it out -- and talks FTP to it, in
# the clear and over TLS, through a settings file of its own so that nothing of
# the real user's is touched.  Used by CI and by hand.

set -euo pipefail

dmg="${1:?usage: tools/check-dmg.sh DMG}"
# A port nobody has, so that an application left running from before -- or
# anything else -- cannot answer in its place.
port="${PORT:-$(python3 -c 'import socket; s = socket.socket(); s.bind(("127.0.0.1", 0)); print(s.getsockname()[1])')}"
work="$(mktemp -d "${TMPDIR:-/tmp}/check-dmg.XXXXXX")"
mount="$work/mount"
app_pid=""

cleanup() {
  if [ -n "$app_pid" ]; then
    kill "$app_pid" 2>/dev/null || true
    wait "$app_pid" 2>/dev/null || true
  fi
  # The image is busy until the application has let go of it; and nothing is
  # removed while it is still mounted, which would be removing from it.
  for attempt in 1 2 3 4 5; do
    hdiutil detach -quiet "$mount" 2>/dev/null && break
    sleep 1
  done
  hdiutil detach -quiet -force "$mount" 2>/dev/null || true
  if mount | grep -q "$work"; then
    echo "warning: $mount is still mounted; leaving $work" >&2
  else
    rm -rf "$work"
  fi
}
trap cleanup EXIT

mkdir -p "$mount" "$work/shared"
hdiutil attach -readonly -nobrowse -noautoopen -mountpoint "$mount" "$dmg" > /dev/null

# The real path: lsof names files by it, and a temporary directory is
# usually reached through a link (/var is /private/var).
mount="$(cd "$mount" && pwd -P)"
app="$mount/FTP Server.app"
test -d "$app" || { echo "error: no FTP Server.app in $dmg" >&2; exit 1; }
test -L "$mount/Applications" || { echo "error: no link to /Applications in $dmg" >&2; exit 1; }
codesign --verify --deep --strict "$app"
echo "ok: the image holds a signed FTP Server.app and a link to /Applications"

# Built for the machine it was built on, which is this one.
arch="$(uname -m)"
file "$app/Contents/MacOS/ftp-server" | grep -q "$arch" \
  || { echo "error: the executable is not for $arch" >&2; exit 1; }
echo "ok: the executable is for $arch"

echo "from the disk image" > "$work/shared/hello.txt"
head -c 3000000 /dev/urandom > "$work/big.bin"
cat > "$work/settings.lisp" <<SETTINGS
(:version 2 :port $port
 :mappings ((:name "shared" :path "$work/shared"))
 :users ((:name "u" :password "p4ss" :access (("shared" . :read-write)))))
SETTINGS

# env -u SBCL_HOME: a shell with Homebrew's sbcl on its path may export it,
# and the application would then load the wrong core.
env -u SBCL_HOME MACOS_APP_LOG="$work/app.log" FTP_SERVER_SETTINGS="$work/settings.lisp" \
    FTP_SERVER_SELFTEST=60 "$app/Contents/MacOS/ftp-server" > /dev/null 2>&1 &
app_pid=$!

url="ftp://127.0.0.1:$port"
curl -sS --retry 30 --retry-connrefused --retry-delay 1 --user u:p4ss "$url/" > /dev/null 2>"$work/curl.err" \
  || { echo "error: the application did not start serving on $port" >&2
       cat "$work/curl.err" "$work/app.log" >&2 2>/dev/null || true; exit 1; }
test "$(curl -sS --max-time 30 --user u:p4ss "$url/shared/hello.txt")" = "from the disk image"
curl -sS --max-time 60 --ssl-reqd -k --user u:p4ss -T "$work/big.bin" "$url/shared/big.bin"
cmp "$work/big.bin" "$work/shared/big.bin"
echo "ok: downloaded in the clear and uploaded 3 MB over TLS"

# OpenSSL from inside the image, not from wherever the build machine had it.
lsof -p "$app_pid" | grep libssl | grep -q "$mount/FTP Server.app/Contents/Frameworks" \
  || { echo "error: the application is not using its own OpenSSL" >&2
       lsof -p "$app_pid" | grep -i 'libssl\|libcrypto' >&2 || true; exit 1; }
echo "ok: OpenSSL is the copy inside the image"

if curl -sS --max-time 20 --user u:wrong "$url/" > /dev/null 2>&1; then
  echo "error: a wrong password was accepted" >&2; exit 1
fi
echo "ok: a wrong password is refused"
echo "$dmg is good"

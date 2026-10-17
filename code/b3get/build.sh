#!/bin/sh
# Builds b3get.jar from src/ with a JDK 17 or later:
#   code/b3get/build.sh <folder with netlogo-7.0.4.jar>   (lib/app in a NetLogo 7.0.4 install)
# perf/nlperf runs this in its container whenever src/ is newer than the jar.
set -eu
here=$(cd "$(dirname "$0")" && pwd)
app=${1:?usage: build.sh <folder with netlogo-7.0.4.jar>}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
javac -nowarn --release 17 -cp "$app/*" -d "$tmp/classes" "$here"/src/b3get/*.java
# A fixed date makes the jar the same bytes on every build.
jar --create --file "$tmp/b3get.jar" --manifest "$here/manifest.txt" --date 2026-01-01T00:00:00Z -C "$tmp/classes" .
mv "$tmp/b3get.jar" "$here/b3get.jar"

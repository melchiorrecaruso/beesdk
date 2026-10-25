#!/usr/bin/env bash
# Builds beeopt with lazbuild into ../build, for the native target.
# LAZARUS may point to the Lazarus folder; otherwise lazbuild is taken from
# ../../fpcup/lazarus or from PATH.
set -euo pipefail

cd "$(dirname "$0")"
LAZARUS="${LAZARUS:-../../fpcup/lazarus}"
if [ -x "$LAZARUS/lazbuild" ] || [ -x "$LAZARUS/lazbuild.exe" ]; then
  LAZBUILD="$LAZARUS/lazbuild"
  [ -x "$LAZBUILD" ] || LAZBUILD="$LAZARUS/lazbuild.exe"
  "$LAZBUILD" --lazarusdir="$LAZARUS" --build-mode=Release beeopt.lpi
elif command -v lazbuild >/dev/null; then
  lazbuild --build-mode=Release beeopt.lpi
else
  echo "lazbuild not found: set LAZARUS." >&2
  exit 1
fi

#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"
lazbuild --build-mode=Release beeopt.lpi

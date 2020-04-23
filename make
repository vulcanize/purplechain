#!/usr/bin/env bash
set -euo pipefail

DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

cd $DIR && nix-shell --run "$(nix-build make.nix -A $1 --no-out-link)"

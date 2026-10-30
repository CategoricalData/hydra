#!/usr/bin/env bash
# Refresh the grammar-test corpus from the live kernel .hydra sources, then
# regenerate TextMate snapshots. Run this whenever the kernel sources or the
# grammar change; commit the resulting tests/corpus/*.hydra and *.snap.
#
# The corpus is a COPY of packages/hydra-kernel/src/main/hydra/*.hydra so that
# vscode-tmgrammar-snap can write its .snap files alongside without polluting
# the kernel source tree. CI re-runs this and fails if the committed snapshots
# drift, which is the intended coupling (#806): a change to syntax.md / the
# kernel sources breaks the grammar tests until the grammar catches up.
#
# EXCLUSIONS: Defaults.hydra is skipped. It is a generated data blob (a single
# ~100KB `defaults` map literal on one line), not representative syntax, and the
# vscode-tmgrammar-snap renderer throws `RangeError: Invalid string length` trying
# to render per-token scope annotations for a line that long. The grammar itself
# tokenizes it fine; only the snapshot tool cannot serialize it. Any new file with
# a pathologically long single line should be added to EXCLUDE below.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
vscode="$(cd "$here/.." && pwd)"
kernel="$(cd "$vscode/../../packages/hydra-kernel/src/main/hydra" && pwd)"

EXCLUDE=("Defaults.hydra")

rm -f "$here/corpus"/*.hydra
for f in "$kernel"/*.hydra; do
  base="$(basename "$f")"
  skip=""
  for ex in "${EXCLUDE[@]}"; do [ "$base" = "$ex" ] && skip=1; done
  [ -n "$skip" ] && { echo "Skipping $base (excluded)"; continue; }
  cp "$f" "$here/corpus/"
done
cd "$vscode"
npx vscode-tmgrammar-snap --updateSnapshot -g syntaxes/hydra.tmLanguage.json "tests/corpus/*.hydra"
echo "Corpus refreshed from: $kernel"

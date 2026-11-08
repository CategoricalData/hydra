#!/usr/bin/env bash
# Refresh the grammar-test corpus from the live kernel .hydra sources, then
# regenerate TextMate snapshots. Run this whenever the kernel sources or the
# grammar change; commit the resulting tests/corpus/*.hydra and *.snap.
#
# The corpus is a flat COPY of every *.hydra file found recursively under
# packages/hydra-kernel/src/main/hydra/ (where each file sits at the path
# spelled by its own declared module namespace, dots as directories, e.g.
# hydra.core.lib.regex -> hydra/core/lib/regex.hydra) so that
# vscode-tmgrammar-snap can write its .snap files alongside without polluting
# the kernel source tree. CI re-runs this and fails if the committed
# snapshots drift, which is the intended coupling (#806): a change to
# syntax.md / the kernel sources breaks the grammar tests until the grammar
# catches up.
#
# Corpus filenames are the source's path relative to the kernel root, with '/'
# replaced by '-' (e.g. hydra/core/lib/regex.hydra ->
# hydra-core-lib-regex.hydra) -- NOT just the basename. The same last
# namespace segment can legitimately recur under different parents (e.g.
# hydra.core.lib.regex's primitives vs. hydra.core.regex's AST model, both
# named regex.hydra) -- a plain basename copy would silently drop one of the
# two to a last-writer-wins collision.
#
# EXCLUSIONS: defaults.hydra (hydra.core.lib.defaults) is skipped. It is a
# generated data blob (a single ~100KB `defaults` map literal on one line),
# not representative syntax, and the vscode-tmgrammar-snap renderer throws
# `RangeError: Invalid string length` trying to render per-token scope
# annotations for a line that long. The grammar itself tokenizes it fine;
# only the snapshot tool cannot serialize it. Any new file with a
# pathologically long single line should be added to EXCLUDE below.
set -euo pipefail
here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
vscode="$(cd "$here/.." && pwd)"
kernel="$(cd "$vscode/../../packages/hydra-kernel/src/main/hydra" && pwd)"

EXCLUDE=("defaults.hydra")

rm -f "$here/corpus"/*.hydra "$here/corpus"/*.hydra.snap
while IFS= read -r f; do
  rel="${f#"$kernel"/}"
  base="$(basename "$rel")"
  skip=""
  for ex in "${EXCLUDE[@]}"; do [ "$base" = "$ex" ] && skip=1; done
  [ -n "$skip" ] && { echo "Skipping $rel (excluded)"; continue; }
  cp "$f" "$here/corpus/${rel//\//-}"
done < <(find "$kernel" -name '*.hydra')
cd "$vscode"
npx vscode-tmgrammar-snap --updateSnapshot -g syntaxes/hydra.tmLanguage.json "tests/corpus/*.hydra"
echo "Corpus refreshed from: $kernel"

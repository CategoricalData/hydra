#!/usr/bin/env bash
# Assemble a self-contained Hackage sdist for the `hydra` Haskell package.
#
# TEMPORARY for 0.15. The hydra-haskell head's package.yaml declares many
# `hs-source-dirs:` entries that live at `../../packages/...`,
# `../../dist/haskell/...`, and `../../demos/...`. `stack sdist` rejects those
# (Cabal won't allow `../../` in published packages — Hackage requires each
# package to be self-contained). To produce a valid Hackage tarball we copy
# every referenced source dir into a flat staging tree, rewrite package.yaml
# to point at the in-staging paths, and run `stack sdist` from there.
#
# Starting with 0.16, the plan is to drop this assembler and run
# `stack sdist` directly from each `dist/haskell/<pkg>/` (mirroring the
# per-package Java + Python layout shipped in 0.15). This script will be
# removed at that point.
#
# Usage:
#   heads/haskell/bin/assemble-hackage-sdist.sh [--out <dir>]
#
# Output: <out>/hydra-<version>.tar.gz, ready to upload to Hackage.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

OUT_DIR="$HYDRA_ROOT/build/hackage"
while [ $# -gt 0 ]; do
    case "$1" in
        --out) OUT_DIR="$2"; shift 2 ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

mkdir -p "$OUT_DIR"

VERSION="$(cat "$HYDRA_ROOT/VERSION")"

STAGE="$(mktemp -d -t hydra-hackage-sdist-XXXXXX)"
trap 'rm -rf "$STAGE"' EXIT

echo "=== Assembling Hackage sdist for hydra-$VERSION ==="
echo "  Staging:   $STAGE"
echo "  Output:    $OUT_DIR/hydra-$VERSION.tar.gz"
echo ""

# Top-level files Stack/Cabal need at the package root.
mkdir -p "$STAGE/src/main/haskell" "$STAGE/src/test/haskell" "$STAGE/src/exec"
cp "$HYDRA_HASKELL_DIR/LICENSE" "$STAGE/LICENSE"
cp "$HYDRA_HASKELL_DIR/stack.yaml" "$STAGE/stack.yaml"

# Copy every exec subdirectory verbatim (these only reference the in-tree
# `hydra` library, no external dirs).
for exec_dir in "$HYDRA_HASKELL_DIR"/src/exec/*/; do
    [ -d "$exec_dir" ] || continue
    name="$(basename "$exec_dir")"
    cp -R "$exec_dir" "$STAGE/src/exec/$name"
done

# Helper: merge a source tree into the staging tree at a given relative path.
# Refuses to overwrite files (would mean two source-dirs declared the same
# Hydra.X module — a real conflict to surface).
merge_into() {
    local src="$1"
    local dest_rel="$2"
    if [ ! -d "$src" ]; then
        echo "  WARN: missing source dir: $src" >&2
        return 0
    fi
    local dest="$STAGE/$dest_rel"
    mkdir -p "$dest"
    while IFS= read -r -d '' file; do
        local rel="${file#$src/}"
        local target="$dest/$rel"
        if [ -e "$target" ]; then
            echo "  ERROR: collision in $dest_rel: $rel (both in already-merged source and $src)" >&2
            exit 1
        fi
        mkdir -p "$(dirname "$target")"
        cp "$file" "$target"
    done < <(find "$src" -type f -name '*.hs' -print0)
}

# Main source: heads/haskell/src/main/haskell first, then everything else.
# Order is by the existing package.yaml's source-dirs list — we honor it for
# stability, even though the `merge_into` collision check would catch any real
# overlap.
echo "Copying main sources..."
merge_into "$HYDRA_HASKELL_DIR/src/main/haskell"            "src/main/haskell"

# Generated dist trees + DSL sources from packages/.
for sub in \
    dist/haskell/hydra-kernel/src/main/haskell \
    dist/haskell/hydra-haskell/src/main/haskell \
    dist/haskell/hydra-coq/src/main/haskell \
    dist/haskell/hydra-javascript/src/main/haskell \
    dist/haskell/hydra-java/src/main/haskell \
    dist/haskell/hydra-python/src/main/haskell \
    dist/haskell/hydra-scala/src/main/haskell \
    dist/haskell/hydra-lisp/src/main/haskell \
    dist/haskell/hydra-ext/src/main/haskell \
    dist/haskell/hydra-pg/src/main/haskell \
    dist/haskell/hydra-rdf/src/main/haskell \
    dist/haskell/hydra-wasm/src/main/haskell \
    packages/hydra-coq/src/main/haskell \
    packages/hydra-haskell/src/main/haskell \
    packages/hydra-java/src/main/haskell \
    packages/hydra-javascript/src/main/haskell \
    packages/hydra-kernel/src/main/haskell \
    packages/hydra-lisp/src/main/haskell \
    packages/hydra-ext/src/main/haskell \
    packages/hydra-pg/src/main/haskell \
    packages/hydra-python/src/main/haskell \
    packages/hydra-rdf/src/main/haskell \
    packages/hydra-scala/src/main/haskell \
    packages/hydra-wasm/src/main/haskell \
    demos/src/main/haskell \
    ; do
    merge_into "$HYDRA_ROOT/$sub" "src/main/haskell"
done

# Test sources: heads/haskell test tree + the kernel-test dist.
echo "Copying test sources..."
merge_into "$HYDRA_HASKELL_DIR/src/test/haskell"                       "src/test/haskell"
merge_into "$HYDRA_ROOT/dist/haskell/hydra-kernel/src/test/haskell"    "src/test/haskell"

# Rewrite package.yaml to a self-contained version: drop every `../../...`
# source-dirs entry, leaving only the flat in-staging dirs. Python is the
# easiest way to do this surgery without a yaml dependency.
echo "Writing flattened package.yaml..."
python3 - "$HYDRA_HASKELL_DIR/package.yaml" "$STAGE/package.yaml" <<'PY'
import re
import sys

src_path, dst_path = sys.argv[1], sys.argv[2]
with open(src_path) as f:
    text = f.read()

# Match `- ../../...` lines under any `source-dirs:` block, and drop them.
# Keep `- src/...` lines (the in-package ones) untouched.
out_lines = []
for line in text.splitlines():
    stripped = line.strip()
    if stripped.startswith("- ../../"):
        continue
    out_lines.append(line)

flattened = "\n".join(out_lines) + "\n"

# Drop the consecutive duplicate empty source-dirs entries that may result
# from removing every external dir. A package.yaml line like:
#   source-dirs:
#     - src/main/haskell
# stays valid; one with no entries left would break.
# Sanity check: if any `source-dirs:` is now followed by no `- ` line, fail.
checked = []
buf = flattened.splitlines(keepends=True)
i = 0
while i < len(buf):
    line = buf[i]
    checked.append(line)
    if line.strip() == "source-dirs:":
        # Look ahead — must have at least one `    - ...` entry.
        j = i + 1
        saw_entry = False
        while j < len(buf):
            l2 = buf[j]
            if l2.strip().startswith("- "):
                saw_entry = True
                break
            if l2.strip() and not l2.startswith(" "):
                # Different top-level key, no entries.
                break
            j += 1
        if not saw_entry:
            print("ERROR: source-dirs ended up empty after flattening", file=sys.stderr)
            sys.exit(1)
    i += 1

with open(dst_path, "w") as f:
    f.write("".join(checked))
PY

# Run stack sdist from the staging dir. We point Stack at a project-local
# .stack-work to avoid disturbing the worktree's main build cache.
echo ""
echo "Running stack sdist..."
cd "$STAGE"
export STACK_WORK_DIR="$STAGE/.stack-work"
stack sdist 2>&1 | tee "$STAGE/sdist.log" || true

# Locate the produced tarball — stack prints its path on the last "Wrote..."
# line; fall back to scanning .stack-work if that pattern moves.
TARBALL="$(grep -oE '/[^ ]+/hydra-[^/]+\.tar\.gz' "$STAGE/sdist.log" | tail -n 1 || true)"
if [ -z "$TARBALL" ] || [ ! -f "$TARBALL" ]; then
    TARBALL="$(find "$STAGE/.stack-work" -name "hydra-${VERSION}.tar.gz" | head -n 1)"
fi
if [ -z "$TARBALL" ] || [ ! -f "$TARBALL" ]; then
    echo "ERROR: could not locate produced sdist tarball" >&2
    exit 1
fi

mkdir -p "$OUT_DIR"
cp "$TARBALL" "$OUT_DIR/hydra-${VERSION}.tar.gz"
echo ""
echo "=== Done ==="
echo "  Tarball:   $OUT_DIR/hydra-${VERSION}.tar.gz"
echo ""
echo "Next: upload via the Hackage UI at https://hackage.haskell.org/upload"

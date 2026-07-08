#!/usr/bin/env bash
# Layer 2 assembler: produce a publishable Hackage sdist for one Hydra Haskell
# distribution package (hydra-kernel, any generated coder package such as
# hydra-haskell / hydra-coq / hydra-scala / …, or the hydra umbrella).
#
# This is the Haskell analog of heads/java/bin/assemble-distribution.sh. It
# replaces the monolithic heads/haskell/bin/assemble-hackage-sdist.sh (#418):
# instead of flattening every package into one `hydra` tarball, each package is
# assembled and published independently, mirroring Maven/PyPI.
#
# Each package is assembled UNIFORMLY from its complete dist/haskell/<pkg>/ tree.
# sync-haskell.sh has already made every dist tree a complete distribution package
# (generated output + any hand-written source copied in), so this assembler is a
# pure dist/ consumer: it copies dist/haskell/<pkg>/src into a staging tree, writes
# the build files, and runs `stack sdist`. No per-package special-casing, no
# import-closure heuristic.
#
#   hydra-kernel  : dist/haskell/hydra-kernel/src/main/haskell  — generated kernel +
#                   the hand-written runtime sync merged in (Hydra.Settings,
#                   Hydra.Kernel, Hydra.Overlay.Haskell.Lib.*, Hydra.Overlay.Haskell.Dsl.{Terms,Literals,Typed.Common})
#   hydra-haskell : dist/haskell/hydra-haskell/src/main/haskell — generated coder;
#                   the kernel runtime resolves via the hydra-kernel dependency
#   hydra         : dist/haskell/hydra/src/main/haskell — the umbrella Hydra.hs,
#                   overlaid there by sync-haskell.sh from overlay/hydra/
#
# Then bin/lib/generate-haskell-package-build.py writes package.yaml + stack.yaml
# into the staging tree, and `stack sdist` produces the tarball.
#
# (Canonical hand-written sources live, uncompiled, under the top-level overlay/haskell/
# tree; sync-haskell.sh overlays them onto dist/. The head compiles the kernel runtime
# only from the dist/ copy, never from overlay/ — so it has no second local copy to
# conflict with a future hydra-kernel package dependency. See docs/build-system.md.)
#
# Usage:
#   assemble-haskell-distribution.sh <pkg> [--out <dir>] [--no-sdist]
#
# <pkg> is any Hydra Haskell distribution package: hydra-kernel, the umbrella
# hydra, or a generated coder (hydra-haskell hydra-coq hydra-typescript hydra-jvm
# hydra-java hydra-python hydra-scala hydra-lisp hydra-go hydra-wasm hydra-rdf
# hydra-pg hydra-ext hydra-bench).
# --out      : where to place the produced tarball (default: build/hackage/)
# --no-sdist : stage + generate build files only; skip `stack sdist` (for inspection)
#
# Output: <out>/<pkg>-<version>.tar.gz

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_HASKELL_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_HASKELL_DIR/../.." && pwd )"

PKG=""
OUT_DIR="$HYDRA_ROOT/build/hackage"
DO_SDIST=true

while [ $# -gt 0 ]; do
    case "$1" in
        --out) OUT_DIR="$2"; shift 2 ;;
        --no-sdist) DO_SDIST=false; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        -*) echo "Unknown argument: $1" >&2; exit 1 ;;
        *) PKG="$1"; shift ;;
    esac
done

if [ -z "$PKG" ]; then
    echo "ERROR: package name required (a Hydra Haskell distribution package)" >&2
    exit 1
fi

# Publishable Haskell distribution packages. The generated coder packages plus
# hydra-kernel (with its overlaid runtime) and the hand-written `hydra` umbrella.
# Each is assembled UNIFORMLY from its complete dist/haskell/<pkg>/ tree, so no
# per-package special-casing is needed below — only membership is checked here.
# (#376: widened from the 0.16 trio hydra-kernel|hydra-haskell|hydra.)
case "$PKG" in
    hydra-kernel|hydra-build|hydra-haskell|hydra-coq|hydra-typescript|hydra-jvm|hydra-java|\
    hydra-python|hydra-scala|hydra-lisp|hydra-go|hydra-wasm|hydra-rdf|hydra-pg|\
    hydra-ext|hydra-bench|hydra) ;;
    *) echo "ERROR: unsupported package '$PKG' (not a known Hydra Haskell distribution package)" >&2; exit 1 ;;
esac

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"
mkdir -p "$OUT_DIR"
# Resolve OUT_DIR to an absolute path: the sdist step `cd`s into the staging
# dir before copying the tarball out, so a relative --out would break there.
OUT_DIR="$( cd "$OUT_DIR" && pwd )"

STAGE="$(mktemp -d -t hydra-sdist-${PKG}-XXXXXX)"
trap 'rm -rf "$STAGE"' EXIT

echo "=== Assembling Haskell sdist: $PKG-$VERSION ==="
echo "  Staging: $STAGE"
echo "  Output:  $OUT_DIR/$PKG-$VERSION.tar.gz"
echo ""

MAIN_DST="$STAGE/src/main/haskell"
mkdir -p "$MAIN_DST"

# Top-level files Cabal/Stack need at the package root.
cp "$HYDRA_HASKELL_DIR/LICENSE" "$STAGE/LICENSE"
# Bundle the shared repo CHANGELOG so Hackage shows release notes per package
# (declared in extra-source-files by generate-haskell-package-build.py).
cp "$HYDRA_ROOT/CHANGELOG.md" "$STAGE/CHANGELOG.md"
# Bundle the repo NOTICE alongside LICENSE — an Apache-2.0 source distribution
# must carry both (also declared in extra-source-files by the build generator).
cp "$HYDRA_ROOT/NOTICE" "$STAGE/NOTICE"

# Copy a source tree into the staging main dir, refusing collisions (a collision
# means two source dirs declared the same module — a real conflict to surface).
copy_tree() {
    local src="$1"
    if [ ! -d "$src" ]; then
        echo "  ERROR: missing source dir: $src" >&2
        exit 1
    fi
    while IFS= read -r -d '' file; do
        local rel="${file#$src/}"
        local target="$MAIN_DST/$rel"
        if [ -e "$target" ]; then
            echo "  ERROR: module collision while staging $PKG: $rel" >&2
            exit 1
        fi
        mkdir -p "$(dirname "$target")"
        cp "$file" "$target"
    done < <(find "$src" -type f -name '*.hs' -print0)
}


echo "Staging sources..."
# Every package is assembled UNIFORMLY from its complete dist/haskell/<pkg>/ tree.
# sync-haskell.sh has already made each dist tree complete: the generated output
# plus, for hydra-kernel, the hand-written runtime overlaid from
# overlay/haskell/hydra-kernel/, and for hydra, the umbrella module overlaid from
# overlay/haskell/hydra/. So the assembler is a pure dist/ consumer with no
# special cases — it just tarballs what sync produced. Coder packages
# (hydra-haskell, hydra-coq, …) resolve the kernel runtime via their hydra-kernel
# dependency, so each is just its own generated coder dist tree.
copy_tree "$HYDRA_ROOT/dist/haskell/$PKG/src/main/haskell"

MODCOUNT="$(find "$MAIN_DST" -name '*.hs' | wc -l | tr -d ' ')"
echo "  staged $MODCOUNT module(s)"
echo ""

echo "Generating build files..."
HYDRA_ROOT_DIR="$HYDRA_ROOT" python3 "$HYDRA_ROOT/bin/lib/generate-haskell-package-build.py" \
    "$PKG" --out-dir "$STAGE"
echo ""

if [ "$DO_SDIST" = false ]; then
    echo "=== Staged (no sdist). Inspect at: $STAGE ==="
    # Don't delete the staging dir if the user asked to inspect it.
    trap - EXIT
    echo "  (staging dir left in place for inspection)"
    exit 0
fi

echo "Running stack sdist..."
cd "$STAGE"
export STACK_WORK_DIR="$STAGE/.stack-work"
stack sdist 2>&1 | tee "$STAGE/sdist.log" || true

TARBALL="$(grep -oE '/[^ ]+/'"$PKG"'-[^/]+\.tar\.gz' "$STAGE/sdist.log" | tail -n 1 || true)"
if [ -z "$TARBALL" ] || [ ! -f "$TARBALL" ]; then
    TARBALL="$(find "$STAGE/.stack-work" -name "${PKG}-${VERSION}.tar.gz" | head -n 1)"
fi
if [ -z "$TARBALL" ] || [ ! -f "$TARBALL" ]; then
    echo "ERROR: could not locate produced sdist tarball for $PKG (see staging sdist.log)" >&2
    exit 1
fi

cp "$TARBALL" "$OUT_DIR/$PKG-$VERSION.tar.gz"
echo ""
echo "=== Done: $OUT_DIR/$PKG-$VERSION.tar.gz ==="

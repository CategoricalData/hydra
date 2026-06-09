#!/usr/bin/env bash
# Build (and optionally upload) the Hydra Python per-package wheels + sdists.
#
# Python analog of heads/haskell/bin/publish-hackage.sh and
# heads/java/bin/publish-maven.sh. The 0.16.0 PyPI publish set is:
#   hydra-kernel, hydra-rdf, hydra-pg, hydra-python
# (matching what 0.15 actually shipped to PyPI). hydra-ext is intentionally
# excluded — it is not in the standard sync matrix and was not in the 0.15 PyPI
# release, mirroring its Java exclusion; the docs' Python wheel table lists it
# aspirationally. See docs/release-workflow.md.
#
# Unlike Hackage/Maven, PyPI uploads need no dependency ordering (PyPI accepts
# each wheel independently; inter-package deps resolve at install time), and
# there is no local-cache staleness trap: each wheel is built directly from its
# dist/python/<pkg>/ source tree, not resolved from a repo. So this script just
# builds every wheel+sdist into a shared output dir, then twine-uploads them.
#
# DEPENDENCY-CLOSURE guard (as in the sibling scripts): every Hydra dep of a
# published package must itself be in the publish set.
#
# Usage:
#   publish-pypi.sh [--out <dir>] [--upload]
#
#   (default)   build wheel+sdist for each package into <out> (wheels/); no upload.
#   --upload    after building, `twine upload` everything in <out>.
#               Prompts for PyPI credentials (or uses ~/.pypirc / a token).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_PYTHON_DIR/../.." && pwd )"

OUT_DIR="$HYDRA_ROOT/wheels"
DO_UPLOAD=false

while [ $# -gt 0 ]; do
    case "$1" in
        --out) OUT_DIR="$2"; shift 2 ;;
        --upload) DO_UPLOAD=true; shift ;;
        --help|-h) sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

VERSION="$(cat "$HYDRA_ROOT/VERSION")"
PUBLISH_SET=(hydra-kernel hydra-rdf hydra-pg hydra-python)

# --- Guard: dependency closure -----------------------------------------------
echo "=== Checking dependency closure of PyPI publish set ==="
in_set() { local n="$1" x; for x in "${PUBLISH_SET[@]}"; do [ "$x" = "$n" ] && return 0; done; return 1; }
pkg_deps() {
    local json="$HYDRA_ROOT/packages/$1/package.json"
    [ -f "$json" ] || { echo "ERROR: no package.json for $1" >&2; return 1; }
    python3 -c "import json,sys; print(' '.join(json.load(open(sys.argv[1])).get('dependencies') or []))" "$json"
}
CLOSURE_OK=true
for pkg in "${PUBLISH_SET[@]}"; do
    for d in $(pkg_deps "$pkg"); do
        if in_set "$d"; then echo "  OK: $pkg -> $d"; else
            echo "  ERROR: $pkg depends on '$d' not in publish set" >&2; CLOSURE_OK=false; fi
    done
done
[ "$CLOSURE_OK" = true ] || { echo "FAIL: publish set not dependency-closed." >&2; exit 1; }
echo ""

# --- Pick a builder ----------------------------------------------------------
# Prefer `uv build` (hermetic; needs no preinstalled `build` module — robust
# across whatever `python3` resolves to). Fall back to `python3 -m build` if
# that module is importable. Fail with a clear hint otherwise.
if command -v uv >/dev/null 2>&1; then
    BUILD_CMD="uv"
elif python3 -c "import build" >/dev/null 2>&1; then
    BUILD_CMD="pymb"
else
    echo "ERROR: no builder available. Install one of:" >&2
    echo "       uv  (https://docs.astral.sh/uv/)  — preferred, or" >&2
    echo "       python3 -m pip install build" >&2
    exit 1
fi
echo "  Builder: $([ "$BUILD_CMD" = uv ] && echo 'uv build' || echo 'python3 -m build')"

# --- Build each wheel + sdist ------------------------------------------------
mkdir -p "$OUT_DIR"
echo "=== Building wheels + sdists into $OUT_DIR ==="
for pkg in "${PUBLISH_SET[@]}"; do
    pkgdir="$HYDRA_ROOT/dist/python/$pkg"
    pp="$pkgdir/pyproject.toml"
    [ -f "$pp" ] || { echo "ERROR: missing $pp (run a sync first)" >&2; exit 1; }
    grep -q "version = \"$VERSION\"" "$pp" || { echo "ERROR: $pkg version != $VERSION (stale dist?)" >&2; exit 1; }
    echo "--- $pkg @ $VERSION ---"
    if [ "$BUILD_CMD" = uv ]; then
        ( cd "$pkgdir" && uv build --wheel --sdist --out-dir "$OUT_DIR" )
    else
        ( cd "$pkgdir" && python3 -m build --wheel --sdist --outdir "$OUT_DIR" )
    fi
done
echo ""

echo "=== Artifacts in $OUT_DIR ==="
ls -1 "$OUT_DIR"/*.whl "$OUT_DIR"/*.tar.gz 2>/dev/null | sed 's/^/  /'
echo ""

if [ "$DO_UPLOAD" != true ]; then
    echo "Build-only (no upload). To upload to PyPI: --upload"
    exit 0
fi

# --- Upload (order does not matter for PyPI) ---------------------------------
echo "=== twine upload $OUT_DIR/* ==="
twine upload "$OUT_DIR"/*.whl "$OUT_DIR"/*.tar.gz
echo "=== Uploaded ${#PUBLISH_SET[@]} packages to PyPI at $VERSION. ==="

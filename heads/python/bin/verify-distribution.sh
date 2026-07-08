#!/usr/bin/env bash
# Verify the Hydra Python distribution is self-contained across the
# source -> wheel packaging boundary.
#
# THE GATE THAT 0.16.0 LACKED. The 0.16.0 hydra-kernel wheel shipped kernel
# modules that import `hydra.python.util`, but the wheel did not contain that
# package (it lived only in heads/python/, outside the packaged dist/python/
# tree). Every LOCAL build and test passed because heads/python/ was always on
# sys.path; the break existed only ACROSS the source -> wheel packaging
# boundary, which no local test exercised. A clean `pip install` of the wheel
# then failed with `ModuleNotFoundError: No module named 'hydra.python.util'`.
# See #472.
#
# This is the Python analog of heads/haskell/bin/verify-distribution.sh and
# heads/java/bin/verify-distribution.sh: each builds its host's publish-set
# packages from the dist/ tree ALONE and proves they are self-contained in
# isolation from the worktree. Uniform name across heads so prepare-release.sh /
# CI can run the same step per host.
#
# What it does (zero-arg form):
#   1. Builds the PyPI publish-set wheels (hydra-kernel hydra-rdf hydra-pg
#      hydra-python) from dist/python/<pkg>/ via `uv build --wheel`.
#   2. Installs ONLY those wheels into a FRESH, ISOLATED venv (--no-index, no
#      PyPI), then imports the top-level kernel modules from a NEUTRAL cwd so the
#      worktree's hydra/ namespace cannot leak onto sys.path and mask a gap.
# If a wheel is missing a package its own code imports, the import fails here and
# the script exits non-zero — exactly what an external `pip install hydra-kernel`
# user would hit, but caught BEFORE anything is uploaded.
#
# Side-effect-free: its only writes are to temp dirs it removes on exit, so it is
# safe to run in CI, locally, or as a hard gate inside publish-pypi.sh.
#
# Usage:
#   verify-distribution.sh                 # build publish-set wheels, then verify
#   verify-distribution.sh --wheels <dir>  # skip the build; verify wheels in <dir>
#                                          # (CI / publish-pypi.sh reuse pre-built
#                                          #  wheels via this override)
#
# Exit status: 0 = every wheel installs and every kernel import succeeds.
#              non-zero = a wheel is missing a package it imports, or any
#                         other install/import failure (with the traceback).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_PYTHON_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_PYTHON_DIR/../.." && pwd )"

# The PyPI publish set (mirror of publish-pypi.sh PUBLISH_SET). Order is
# leaves-first so a --no-index install can resolve inter-package deps.
PUBLISH_SET=(hydra-kernel hydra-build hydra-rdf hydra-pg hydra-python)

WHEELS_DIR=""
while [ $# -gt 0 ]; do
    case "$1" in
        --wheels) WHEELS_DIR="$2"; shift 2 ;;
        --help|-h) sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

# Temp dirs (wheels build dir + venv), removed on exit regardless of outcome.
TMP_BUILD=""
TMP_VENV=""
cleanup() {
    [ -n "$TMP_BUILD" ] && rm -rf "$TMP_BUILD"
    [ -n "$TMP_VENV" ] && rm -rf "$TMP_VENV"
}
trap cleanup EXIT

# --- Build the publish-set wheels from dist/python/, unless --wheels given. ---
if [ -z "$WHEELS_DIR" ]; then
    command -v uv >/dev/null 2>&1 || {
        echo "ERROR: 'uv' not found; needed to build wheels (or pass --wheels <dir>)." >&2
        exit 1
    }
    TMP_BUILD="$(mktemp -d "${TMPDIR:-/tmp}/hydra-verify-py-wheels.XXXXXX")"
    WHEELS_DIR="$TMP_BUILD"
    echo "=== Building PyPI publish-set wheels from dist/python/ ==="
    for pkg in "${PUBLISH_SET[@]}"; do
        pkgdir="$HYDRA_ROOT/dist/python/$pkg"
        [ -d "$pkgdir" ] || { echo "ERROR: missing dist package: $pkgdir" >&2; exit 1; }
        echo "--- building $pkg ---"
        ( cd "$pkgdir" && uv build --wheel --out-dir "$WHEELS_DIR" )
    done
    echo ""
else
    [ -d "$WHEELS_DIR" ] || { echo "ERROR: no such directory: $WHEELS_DIR" >&2; exit 1; }
fi

# Resolve to an absolute path so the cd-into-tmp import step below can't be
# fooled by a relative wheels dir.
WHEELS_DIR="$( cd "$WHEELS_DIR" && pwd )"

shopt -s nullglob
wheels=( "$WHEELS_DIR"/*.whl )
shopt -u nullglob
[ "${#wheels[@]}" -gt 0 ] || { echo "ERROR: no *.whl files in $WHEELS_DIR" >&2; exit 1; }

# Top-level kernel modules every consumer reaches transitively. These are the
# import surfaces that would have caught 0.16.0 (codegen.py was the first to
# blow up; the others share the same `from hydra.python.util import ...` head).
# Keep this list broad: each entry is a distinct module file, so a wheel that
# omits a package any of them import is caught.
KERNEL_IMPORTS=(
    hydra.codegen
    hydra.rewriting
    hydra.encoding
    hydra.arity
    hydra.analysis
    hydra.query
    hydra.predicates
    hydra.validate.core
    hydra.validate.packaging
    hydra.overlay.python.util
)

# Pick a Python interpreter that satisfies the wheels' requires-python (>=3.12).
PYTHON=""
for cand in python3.12 python3.13 python3.14 python3; do
    if command -v "$cand" >/dev/null 2>&1; then
        ver="$("$cand" -c 'import sys; print("%d.%d" % sys.version_info[:2])' 2>/dev/null || echo "")"
        case "$ver" in
            3.1[2-9]|3.[2-9][0-9]) PYTHON="$cand"; break ;;
        esac
    fi
done
[ -n "$PYTHON" ] || {
    echo "ERROR: no Python >=3.12 interpreter found (need one to match the wheels' requires-python)." >&2
    exit 1
}

echo "=== Packaging-boundary wheel smoke test (#472 gate) ==="
echo "  interpreter: $PYTHON ($("$PYTHON" --version 2>&1))"
echo "  wheels dir:  $WHEELS_DIR"
echo "  wheels:      ${#wheels[@]}"

# Isolated venv in a temp dir, removed on exit regardless of outcome.
TMP_VENV="$(mktemp -d "${TMPDIR:-/tmp}/hydra-wheel-smoke.XXXXXX")"

"$PYTHON" -m venv "$TMP_VENV"
VPIP="$TMP_VENV/bin/pip"
VPY="$TMP_VENV/bin/python"

# Install ONLY from the local wheels: --no-index forbids PyPI, so a wheel that
# depends on a sibling missing from the dir fails loudly rather than silently
# pulling a published (possibly-broken) version. This mirrors a hermetic,
# offline install of exactly what we just built.
echo "--- installing wheels into isolated venv (--no-index) ---"
"$VPIP" install --quiet --disable-pip-version-check \
    --no-index --find-links "$WHEELS_DIR" "${wheels[@]}"

# Import from a NEUTRAL cwd (the temp venv dir), never the repo root, so the
# worktree's hydra/ namespace can't leak onto sys.path and mask a wheel gap.
# This is the exact failure mode that hid 0.16.0: locally, heads/python was on
# the path; here it is provably not.
echo "--- importing kernel modules from the installed wheels ---"
import_script="$TMP_VENV/_smoke_imports.py"
{
    echo "import importlib, sys"
    echo "mods = ["
    for m in "${KERNEL_IMPORTS[@]}"; do echo "    \"$m\","; done
    echo "]"
    echo "failed = []"
    echo "for m in mods:"
    echo "    try:"
    echo "        importlib.import_module(m)"
    echo "        print('  OK   ' + m)"
    echo "    except Exception as e:"
    echo "        print('  FAIL ' + m + ': ' + type(e).__name__ + ': ' + str(e))"
    echo "        failed.append(m)"
    echo "if failed:"
    echo "    print()"
    echo "    print('SMOKE TEST FAILED: ' + str(len(failed)) + ' module(s) could not be imported from the built wheels:')"
    echo "    for m in failed: print('  - ' + m)"
    echo "    sys.exit(1)"
    echo "print()"
    echo "print('SMOKE TEST PASSED: all ' + str(len(mods)) + ' kernel modules import cleanly from the wheels.')"
} > "$import_script"

# Run with cwd = the temp dir (not the repo) to guarantee no local-tree leakage.
( cd "$TMP_VENV" && "$VPY" "$import_script" )

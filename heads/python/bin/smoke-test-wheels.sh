#!/usr/bin/env bash
# Packaging-boundary smoke test for the Hydra Python wheels.
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
# This script reproduces what an external `pip install hydra-kernel` user does:
# it installs the just-built wheels into a FRESH, ISOLATED venv (no worktree on
# the path, no PyPI index — only the local wheels) and imports the top-level
# kernel modules. If a wheel is missing a package its own code imports, the
# import fails here and the script exits non-zero — BEFORE anything is uploaded.
#
# It is deliberately self-contained and side-effect-free (its only writes are to
# a temp dir it removes on exit), so it is safe to run in CI, locally, or as a
# hard gate inside publish-pypi.sh.
#
# Usage:
#   smoke-test-wheels.sh --wheels <dir>     # install + import-test wheels in <dir>
#
#   <dir> is a directory containing the *.whl files produced by
#   publish-pypi.sh (default output: <repo>/wheels/). sdists in the dir are
#   ignored; only wheels are installed.
#
# Exit status: 0 = every wheel installs and every kernel import succeeds.
#              non-zero = a wheel is missing a package it imports, or any
#                         other install/import failure (with the traceback).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

WHEELS_DIR=""
while [ $# -gt 0 ]; do
    case "$1" in
        --wheels) WHEELS_DIR="$2"; shift 2 ;;
        --help|-h) sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

[ -n "$WHEELS_DIR" ] || { echo "ERROR: --wheels <dir> is required" >&2; exit 1; }
[ -d "$WHEELS_DIR" ] || { echo "ERROR: no such directory: $WHEELS_DIR" >&2; exit 1; }

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
    hydra.python.util
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
cleanup() { rm -rf "$TMP_VENV"; }
trap cleanup EXIT

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

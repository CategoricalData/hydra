#!/usr/bin/env bash
# Generate dist/json/hydra-python entirely from the Python DSL sources.
#
# This is the user-callable wrapper around bin/update-python-json.py:
# it checks whether the Python host is built (kernel JSON + kernel Python
# runtime + hydra-python dist helpers), runs `bin/sync-python.sh` to build
# them if not, then invokes the driver. Originally introduced for issue
# #344 (the "Python self-host demo"); now the canonical Python DSL → JSON
# step in the regular sync pipeline (Phase 5).
#
# Usage:
#   bin/generate-hydra-python-from-python.sh                  # CPython
#   bin/generate-hydra-python-from-python.sh --pypy           # PyPy (~4x faster)
#   bin/generate-hydra-python-from-python.sh --out-root DIR   # Override output
#   bin/generate-hydra-python-from-python.sh --force-rebuild  # Run sync-python.sh
#                                                              # even if host present
#   bin/generate-hydra-python-from-python.sh --compare        # After generation,
#                                                              # byte-compare to the
#                                                              # Haskell-generated
#                                                              # canonical
#   bin/generate-hydra-python-from-python.sh --help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

# Defaults
INTERP="uv"
CANON_ROOT="$HYDRA_ROOT/dist/json/hydra-python/src/main/json"
OUT_ROOT="$CANON_ROOT"
USER_SET_OUT_ROOT=0
FORCE_REBUILD=0
DO_COMPARE=0
EXTRA_ARGS=()

while [ $# -gt 0 ]; do
    case "$1" in
        --pypy) INTERP="pypy3"; shift ;;
        --cpython) INTERP="uv"; shift ;;
        --out-root) OUT_ROOT="$2"; USER_SET_OUT_ROOT=1; shift 2 ;;
        --out-root=*) OUT_ROOT="${1#--out-root=}"; USER_SET_OUT_ROOT=1; shift ;;
        --force-rebuild) FORCE_REBUILD=1; shift ;;
        --compare) DO_COMPARE=1; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) EXTRA_ARGS+=("$1"); shift ;;
    esac
done

# Ensure the dist trees this script reads (dist/json/hydra-kernel,
# dist/python/hydra-kernel, dist/python/hydra-python) are present and
# up to date. sync-python.sh covers all three. Warm-cache is fast;
# cold-cache is self-healing.
#
# HYDRA_IN_SYNC=1 indicates sync.sh is already calling us (Phase 5);
# don't recurse.
if [ "${HYDRA_IN_SYNC:-0}" != "1" ]; then
    if [ "$FORCE_REBUILD" = "1" ]; then
        echo "=== Forcing Python host rebuild via bin/sync-python.sh ==="
    else
        echo "=== Running bin/sync-python.sh to ensure dist trees are current ==="
    fi
    "$HYDRA_ROOT/bin/sync-python.sh"
fi

# Build PYTHONPATH — same set the demo expects.
PP="$HYDRA_ROOT/packages/hydra-python/src/main/python"
PP="$PP:$HYDRA_ROOT/dist/python/hydra-kernel/src/main/python"
PP="$PP:$HYDRA_ROOT/dist/python/hydra-python/src/main/python"
PP="$PP:$HYDRA_ROOT/heads/python/src/main/python"

echo ""
echo "=== Running update-python-json.py (interp: $INTERP) ==="

# If --compare was requested without an explicit --out-root, write to a
# temp directory so we can compare against the in-tree canonical (which
# would otherwise overwrite itself).
ACTUAL_OUT_ROOT="$OUT_ROOT"
COMPARE_CLEANUP=""
if [ "$DO_COMPARE" = "1" ] && [ "$USER_SET_OUT_ROOT" = "0" ]; then
    ACTUAL_OUT_ROOT="$(mktemp -d)/json"
    COMPARE_CLEANUP="$(dirname "$ACTUAL_OUT_ROOT")"
    mkdir -p "$ACTUAL_OUT_ROOT"
fi

case "$INTERP" in
    pypy3)
        PYTHONPATH="$PP" pypy3 "$HYDRA_ROOT/bin/update-python-json.py" \
            --out-root "$ACTUAL_OUT_ROOT" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
        ;;
    uv)
        # uv resolves its own venv; pass PYTHONPATH explicitly so the driver
        # picks up packages/hydra-python and the dist trees.
        PYTHONPATH="$PP" \
            uv --directory "$HYDRA_ROOT/heads/python" run python \
            "$HYDRA_ROOT/bin/update-python-json.py" \
            --out-root "$ACTUAL_OUT_ROOT" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
        ;;
    *)
        echo "Unknown interpreter: $INTERP" >&2
        exit 2 ;;
esac

if [ "$DO_COMPARE" = "1" ]; then
    # Per-package driver writes under <dist-json-root>/hydra-python/src/main/json/.
    # When --out-root was the per-package tail path (canonical mode), the demo
    # strips four segments to recover the dist-json root and writes back
    # through the same tail; ours_path matches the original out-root. When
    # --out-root was a tmp directory (the default --compare-without-out-root
    # flow), the demo treats it as the dist-json root, so the actual
    # per-package output lives at <out-root>/hydra-python/src/main/json/.
    OURS_PATH="$ACTUAL_OUT_ROOT"
    if [ "$USER_SET_OUT_ROOT" = "0" ]; then
        OURS_PATH="$ACTUAL_OUT_ROOT/hydra-python/src/main/json"
    fi
    echo ""
    echo "=== Byte-comparing $OURS_PATH against the Haskell-generated canonical at $CANON_ROOT ==="
    "$HYDRA_ROOT/bin/compare-dsl-json-output.py" --ours "$OURS_PATH" --canon "$CANON_ROOT"
    if [ -n "$COMPARE_CLEANUP" ]; then
        rm -rf "$COMPARE_CLEANUP"
    fi
fi

#!/usr/bin/env bash
# Generate dist/json/hydra-python entirely from the Python DSL sources.
#
# This is the user-callable wrapper around bin/update-python-json.py. It runs the
# Python DSL→JSON driver against the hydra-python coder runtime, in one of two
# classpath modes (#370):
#
#   --published-host  (DEFAULT)  Import the hydra-python coder runtime from the
#                                PUBLISHED PyPI wheels (hydra-python==<hostVersion>,
#                                which pulls hydra-kernel) into a managed venv at
#                                heads/python/.venv-published-host/. No local Python
#                                host build is required — edits to the Python DSL
#                                sources flow straight through. This is the path the
#                                regular sync uses. Needs only dist/json/hydra-kernel.
#
#   --local-host                 BOOTSTRAP SHIM. Import the coder runtime from the
#                                LOCAL dist/python/hydra-{kernel,python} build (built
#                                by bin/sync-python.sh). Use this only for a
#                                backward-INCOMPATIBLE kernel change the last published
#                                host cannot handle yet; build a local interim host,
#                                publish it, and bump hydra.json:hostVersion(Overrides)
#                                so the default published-host path picks it up.
#
# Originally introduced for issue #344 (the "Python self-host demo"); now the
# canonical Python DSL → JSON step in the regular sync pipeline (Phase 5).
#
# Usage:
#   bin/generate-hydra-python-from-python.sh                  # published-host, CPython
#   bin/generate-hydra-python-from-python.sh --local-host     # bootstrap shim (local)
#   bin/generate-hydra-python-from-python.sh --pypy           # PyPy (~4x faster)
#   bin/generate-hydra-python-from-python.sh --out-root DIR   # Override output
#   bin/generate-hydra-python-from-python.sh --force-rebuild  # Recreate host env
#   bin/generate-hydra-python-from-python.sh --compare        # After generation,
#                                                              # byte-compare to the
#                                                              # Haskell-generated
#                                                              # canonical
#   bin/generate-hydra-python-from-python.sh --help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$HYDRA_ROOT/bin/lib/common.sh"
raise_open_file_limit 4096

# Defaults
INTERP="uv"
MODE="published-host"
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
        --published-host) MODE="published-host"; shift ;;
        --local-host) MODE="local-host"; shift ;;
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

# Assemble PYTHONPATH. The Python DSL sources (packages/hydra-python) and the
# driver (heads/python) are always local; the coder RUNTIME comes from either the
# published wheels (default) or the local dist/python build (shim).
PP="$HYDRA_ROOT/packages/hydra-python/src/main/python"
if [ "$MODE" = "published-host" ]; then
    # Published host: resolve hydra-python==<hostVersion> into a managed venv and
    # put its site-packages on PYTHONPATH. No local Python host build needed; the
    # driver only also needs dist/json/hydra-kernel (produced by Phase 1).
    PREP_ARGS=()
    [ "$FORCE_REBUILD" = "1" ] && PREP_ARGS+=("--force")
    HOST_SITE="$("$HYDRA_ROOT/bin/lib/python-published-host.sh" "${PREP_ARGS[@]+"${PREP_ARGS[@]}"}")"
    PP="$PP:$HOST_SITE"
else
    # Bootstrap shim: ensure the local dist trees (dist/json/hydra-kernel,
    # dist/python/hydra-kernel, dist/python/hydra-python) are present + current.
    # sync-python.sh covers all three but is a full recursive `sync.sh --hosts
    # python --targets python`, so HYDRA_IN_SYNC=1 (sync.sh is already calling us)
    # must NOT trigger it — that would recurse.
    if [ "${HYDRA_IN_SYNC:-0}" != "1" ]; then
        if [ "$FORCE_REBUILD" = "1" ]; then
            echo "=== Forcing Python host rebuild via bin/sync-python.sh ==="
        else
            echo "=== Running bin/sync-python.sh to ensure dist trees are current ==="
        fi
        "$HYDRA_ROOT/bin/sync-python.sh"
    else
        # Under HYDRA_IN_SYNC (Phase 1.5 in bin/sync.sh): the local-host runtime
        # we are about to put on PYTHONPATH must match the current kernel layout
        # before update-python-json.py imports hydra.codegen. We can't recurse
        # via sync-python.sh, but the per-package assembler is non-recursive,
        # reads dist/json/hydra-kernel (already produced by Phase 1), and has
        # its own digest-based freshness gate — calling it warm is a fast no-op.
        # Calling it unconditionally fixes #480: presence-only checks let a stale
        # dist/python/hydra-kernel/ (from an older kernel layout) silently sit on
        # PYTHONPATH and break update-python-json.py with an ImportError before
        # any later phase could regenerate it. (#446/#472 hostOverrides interim
        # — see staging-plan.md.)
        echo "=== Ensuring dist/python runtime matches current kernel layout (assembling hydra-kernel + hydra-python; #480) ==="
        "$HYDRA_ROOT/heads/python/bin/assemble-distribution.sh" hydra-kernel
        "$HYDRA_ROOT/heads/python/bin/assemble-distribution.sh" hydra-python
    fi
    PP="$PP:$HYDRA_ROOT/dist/python/hydra-kernel/src/main/python"
    PP="$PP:$HYDRA_ROOT/dist/python/hydra-python/src/main/python"
fi
PP="$PP:$HYDRA_ROOT/heads/python/src/main/python"

echo ""
echo "=== Running update-python-json.py (mode: $MODE, interp: $INTERP) ==="

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

if [ "$MODE" = "published-host" ]; then
    # The published coder runtime is installed (as CPython wheels) into the
    # managed venv; run with that venv's interpreter so the wheels import. PyPy
    # cannot load a CPython venv's site-packages, so --pypy is not honored here
    # (a PyPy published-host venv could be added later if speed demands it).
    if [ "$INTERP" = "pypy3" ]; then
        echo "Note: --pypy is ignored in --published-host mode (using the venv interpreter)." >&2
    fi
    VENV_DIR="$HYDRA_ROOT/heads/python/.venv-published-host"
    HYDRA_PYTHON_HOST_MODE=published PYTHONPATH="$PP" \
        "$VENV_DIR/bin/python" "$HYDRA_ROOT/bin/update-python-json.py" \
        --out-root "$ACTUAL_OUT_ROOT" "${EXTRA_ARGS[@]+"${EXTRA_ARGS[@]}"}"
else
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
fi

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

# #469: Refresh dist/json/hydra-python/build/main/digest.json so the
# JSON we just wrote is reflected in the Phase-2 freshness gate. Without
# this, the next 'assemble-distribution.sh hydra-python' compares the
# stale input digest (referencing pre-write JSON content) against the
# output digest's recorded inputs (also stale), reports a cache hit, and
# skips the JSON->Haskell render — which is exactly the recurrence
# pattern #469 closes. Skip when we wrote to a tmp dir (compare-only
# mode without --out-root); in that case the canonical dist/json is
# untouched.
if [ "$ACTUAL_OUT_ROOT" = "$HYDRA_ROOT/dist/json" ]; then
    echo ""
    echo "=== Refreshing per-package input digest for hydra-python (#469) ==="
    (cd "$HYDRA_ROOT/heads/haskell" && \
     stack build hydra:exe:digest-check >/dev/null && \
     stack exec digest-check -- refresh-input \
       --package hydra-python \
       --dist-json-root "$HYDRA_ROOT/dist/json")
fi

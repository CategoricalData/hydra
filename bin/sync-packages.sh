#!/usr/bin/env bash
# Layer 3 orchestrator: bring dist/ into sync with packages/ and heads/.
#
# Usage:
#   bin/sync-packages.sh                              # all packages, all targets
#   bin/sync-packages.sh <pkg>                        # one package, all targets
#   bin/sync-packages.sh <pkg> --targets L1,L2,...    # one package, named targets
#   bin/sync-packages.sh --targets L1,L2,...          # all packages, named targets
#   bin/sync-packages.sh --from <pkg>                 # <pkg> + reverse-dep closure
#   bin/sync-packages.sh --list                       # print packages and exit
#   bin/sync-packages.sh --no-tests                   # skip Layer 2.5 testers
#   bin/sync-packages.sh --help
#
# Targets: haskell, java, python, scala, clojure, scheme, common-lisp,
#          emacs-lisp. Default: all.
#
# Per-package layered tool. Sibling of bin/sync.sh (the symmetric
# matrix tool, --hosts/--targets) and bin/sync-all.sh (exhaustive run
# with tests). Work is dispatched through the three-layer stack:
#
#   Layer 1 (transforms) — bin wrappers in heads/haskell/bin/transform-*.sh
#   Layer 2 (assemblers)  — bin wrappers in heads/<lang>/bin/assemble-*.sh
#   Layer 2.5 (testers)   — bin wrappers in heads/<lang>/bin/test-*.sh
#
# See feature_290_packaging-plan.md, "Sync system redesign", for the full
# layered design rationale.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

# Source the common.sh helpers if available (for banner, step, etc.).
if [ -f "$HYDRA_ROOT_DIR/bin/lib/common.sh" ]; then
    source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
fi

# -----------------------------------------------------------------------
# Package and target configuration
# -----------------------------------------------------------------------

# Packages that go through the JSON pipeline. Read from the project-level
# manifest (hydra.json) which is the authoritative registry; dependency
# order is derived from each package's own package.json.
HYDRA_PACKAGES_PY="$HYDRA_ROOT_DIR/bin/lib/hydra-packages.py"
ALL_PACKAGES=$("$HYDRA_PACKAGES_PY" list)

# Valid target languages.
ALL_TARGETS="haskell java python scala clojure scheme common-lisp emacs-lisp"

# -----------------------------------------------------------------------
# Argument parsing
# -----------------------------------------------------------------------

PACKAGES=""          # explicit package arg (positional); empty means "all"
FROM_PACKAGE=""      # --from <pkg>
TARGETS=""           # --targets <list>; empty means "all"
NO_TEST=false
LIST_ONLY=false

while [ $# -gt 0 ]; do
    case "$1" in
        --targets) TARGETS=$(echo "$2" | tr ',' ' '); shift 2 ;;
        --targets=*) TARGETS=$(echo "${1#--targets=}" | tr ',' ' '); shift ;;
        --from) FROM_PACKAGE="$2"; shift 2 ;;
        --from=*) FROM_PACKAGE="${1#--from=}"; shift ;;
        --no-tests) NO_TEST=true; shift ;;
        --list) LIST_ONLY=true; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# //;s/^#$//'
            exit 0
            ;;
        --*)
            echo "Error: unknown flag '$1'" >&2
            echo "Try: $0 --help" >&2
            exit 1
            ;;
        *)
            PACKAGES="${PACKAGES}${PACKAGES:+ }$1"
            shift
            ;;
    esac
done

# -----------------------------------------------------------------------
# Dependency resolution
# -----------------------------------------------------------------------

# Read a package's dependencies from its package.json.
# Returns space-separated list, or empty if none.
pkg_deps() {
    "$HYDRA_PACKAGES_PY" deps "$1"
}

# Topologically sort the given list of packages by dependency order.
topo_sort() {
    # Word-split intentionally: $1 is a space-separated list of packages.
    "$HYDRA_PACKAGES_PY" topo $1
}

# Compute reverse-dependency closure: all packages that transitively depend on <root_pkg>.
reverse_dep_closure() {
    "$HYDRA_PACKAGES_PY" reverse-closure "$1"
}

# -----------------------------------------------------------------------
# Environment validation
# -----------------------------------------------------------------------

validate_environment() {
    local targets="$1"
    local errors=""

    for tgt in $targets; do
        case "$tgt" in
            haskell)
                if ! command -v stack >/dev/null 2>&1; then
                    errors="${errors}  - stack not found (required for Haskell target)\n"
                fi
                ;;
            java)
                # Prefer heads/haskell/bin/common.sh's check_native_jdk if available;
                # otherwise, do a minimal check.
                if ! command -v java >/dev/null 2>&1; then
                    errors="${errors}  - java not found (required for Java target)\n"
                fi
                ;;
            python)
                if ! command -v uv >/dev/null 2>&1 && ! command -v python3 >/dev/null 2>&1; then
                    errors="${errors}  - uv or python3 not found (required for Python target)\n"
                fi
                ;;
            scala)
                if ! command -v sbt >/dev/null 2>&1; then
                    errors="${errors}  - sbt not found (required for Scala target)\n"
                fi
                ;;
            clojure)
                if ! command -v clojure >/dev/null 2>&1; then
                    errors="${errors}  - clojure not found (required for Clojure target)\n"
                fi
                ;;
            scheme)
                if ! command -v guile >/dev/null 2>&1; then
                    errors="${errors}  - guile not found (required for Scheme target)\n"
                fi
                ;;
            common-lisp)
                if ! command -v sbcl >/dev/null 2>&1; then
                    errors="${errors}  - sbcl not found (required for Common Lisp target)\n"
                fi
                ;;
            emacs-lisp)
                if ! command -v emacs >/dev/null 2>&1; then
                    errors="${errors}  - emacs not found (required for Emacs Lisp target)\n"
                fi
                ;;
        esac
    done

    if [ -n "$errors" ]; then
        echo "Environment validation failed:" >&2
        printf "%b" "$errors" >&2
        return 1
    fi
    return 0
}

# -----------------------------------------------------------------------
# Pipeline dispatch
# -----------------------------------------------------------------------

# Map target language to its head directory.
head_dir_for_target() {
    case "$1" in
        haskell) echo "$HYDRA_ROOT_DIR/heads/haskell" ;;
        java) echo "$HYDRA_ROOT_DIR/heads/java" ;;
        python) echo "$HYDRA_ROOT_DIR/heads/python" ;;
        scala) echo "$HYDRA_ROOT_DIR/heads/scala" ;;
        clojure|scheme|common-lisp|emacs-lisp) echo "$HYDRA_ROOT_DIR/heads/lisp" ;;
        *) echo "" ;;
    esac
}

# Invoke Layer 2 assembler. For Lisp, pass the dialect as a second arg.
invoke_assembler() {
    local pkg="$1"
    local target="$2"
    local head_dir
    head_dir=$(head_dir_for_target "$target")
    local script="$head_dir/bin/assemble-distribution.sh"
    if [ ! -x "$script" ]; then
        echo "  [skip] $target assembler not available at $script" >&2
        return 0
    fi
    case "$target" in
        clojure|scheme|common-lisp|emacs-lisp)
            "$script" "$pkg" "$target"
            ;;
        *)
            "$script" "$pkg"
            ;;
    esac
}

# Invoke Layer 2.5 tester.
invoke_tester() {
    local pkg="$1"
    local target="$2"
    local head_dir
    head_dir=$(head_dir_for_target "$target")
    local script="$head_dir/bin/test-distribution.sh"
    if [ ! -x "$script" ]; then
        echo "  [skip] $target tester not available at $script" >&2
        return 0
    fi
    case "$target" in
        clojure|scheme|common-lisp|emacs-lisp)
            "$script" "$pkg" "$target"
            ;;
        *)
            "$script" "$pkg"
            ;;
    esac
}

# -----------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------

# Resolve the effective package list.
if [ -n "$FROM_PACKAGE" ]; then
    if [ -n "$PACKAGES" ]; then
        echo "Error: --from and a positional package argument are mutually exclusive" >&2
        exit 1
    fi
    EFFECTIVE_PACKAGES=$(reverse_dep_closure "$FROM_PACKAGE")
elif [ -n "$PACKAGES" ]; then
    EFFECTIVE_PACKAGES="$PACKAGES"
else
    EFFECTIVE_PACKAGES="$ALL_PACKAGES"
fi

# Sort in dependency order (deps first).
EFFECTIVE_PACKAGES=$(topo_sort "$EFFECTIVE_PACKAGES")

# Resolve the effective target list.
if [ -n "$TARGETS" ]; then
    # Validate each target is in ALL_TARGETS.
    for t in $TARGETS; do
        if ! echo " $ALL_TARGETS " | grep -q " $t "; then
            echo "Error: unknown target '$t'" >&2
            echo "Valid targets: $ALL_TARGETS" >&2
            exit 1
        fi
    done
    EFFECTIVE_TARGETS="$TARGETS"
else
    EFFECTIVE_TARGETS="$ALL_TARGETS"
fi

if [ "$LIST_ONLY" = true ]; then
    echo "Packages (in dependency order):"
    for pkg in $EFFECTIVE_PACKAGES; do
        deps=$(pkg_deps "$pkg")
        if [ -n "$deps" ]; then
            echo "  $pkg -> $deps"
        else
            echo "  $pkg"
        fi
    done
    echo ""
    echo "Targets: $EFFECTIVE_TARGETS"
    exit 0
fi

# Environment validation — fail fast before any long-running work.
if ! validate_environment "$EFFECTIVE_TARGETS"; then
    exit 1
fi

# Banner.
echo "=========================================="
echo "Hydra sync"
echo "=========================================="
echo "  Packages: $EFFECTIVE_PACKAGES"
echo "  Targets:  $EFFECTIVE_TARGETS"
echo "  Tests:    $([ "$NO_TEST" = true ] && echo "skipped (--no-tests)" || echo "enabled")"
echo "=========================================="
echo ""

# Phase 1: JSON sources. When operating on every package, invoke the
# transform in batch mode (single Haskell-universe load writing
# per-package JSON via namespaceToPackage). When scoped to a subset,
# fall back to per-package invocation so we don't rewrite JSON for
# packages the caller didn't ask about.
HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "[Phase 1] Regenerating JSON sources for each package..."
# Normalize both lists to sorted space-joined strings for comparison.
effective_sorted=$(echo $EFFECTIVE_PACKAGES | tr ' ' '\n' | sort | tr '\n' ' ')
all_sorted=$(echo $ALL_PACKAGES | tr ' ' '\n' | sort | tr '\n' ' ')
# Warm-cache short-circuit: check DSL hashes vs recorded digests.
# Sub-second; skips 20+ seconds of Haskell startup + JSON writes when
# nothing has changed.
if python3 "$HYDRA_ROOT_DIR/bin/lib/check-dsl-fresh.py" \
        "$HYDRA_ROOT_DIR" \
        "$HYDRA_ROOT_DIR/dist/json/digest.main.json" >/dev/null 2>&1; then
    echo "  Cache hit: every DSL source fresh; skipping Phase 1."
else
    if [ "$effective_sorted" = "$all_sorted" ]; then
        "$HASKELL_BIN/transform-haskell-dsl-to-json.sh" --all main
        "$HASKELL_BIN/transform-haskell-dsl-to-json.sh" --all test
    else
        for pkg in $EFFECTIVE_PACKAGES; do
            "$HASKELL_BIN/transform-haskell-dsl-to-json.sh" "$pkg" main
            "$HASKELL_BIN/transform-haskell-dsl-to-json.sh" "$pkg" test
        done
    fi
    # Also regenerate per-package manifests so readers see the updated module list.
    # stdout is suppressed (manifest exec is chatty about per-package writes); stderr
    # stays visible so build/runtime errors surface instead of silently aborting the
    # script via `set -euo pipefail`.
    cd "$HYDRA_ROOT_DIR/heads/haskell"
    stack build hydra:exe:update-json-manifest >/dev/null
    stack exec update-json-manifest >/dev/null
    cd "$HYDRA_ROOT_DIR"
fi
echo ""

# Returns 0 if $target is in the package's declared targetLanguages (or if
# the package has no targetLanguages field, meaning it targets every host).
# Returns 1 otherwise. The targetLanguages field is read from each
# package's package.json; coder-only packages like hydra-coq,
# hydra-javascript, and hydra-wasm list just ["haskell"] because their
# coders are implemented only against the Haskell runtime.
pkg_supports_target() {
    "$HYDRA_PACKAGES_PY" supports-target "$1" "$2"
}

# Phase 2: Assemblers. When generating every package for a target and
# the head has a batch assembler (assemble-all.sh), invoke it once per
# target — one Haskell universe load per target instead of one per
# (package, target). Falls back to per-package invocation for scoped
# package sets or targets lacking a batch assembler.
echo "[Phase 2] Assembling distributions..."
batch_assembler_for() {
    local target="$1"
    case "$target" in
        haskell) echo "$HYDRA_ROOT_DIR/heads/haskell/bin/assemble-all.sh" ;;
        java)    echo "$HYDRA_ROOT_DIR/heads/java/bin/assemble-all.sh" ;;
        python)  echo "$HYDRA_ROOT_DIR/heads/python/bin/assemble-all.sh" ;;
        scala)   echo "$HYDRA_ROOT_DIR/heads/scala/bin/assemble-all.sh" ;;
        *) echo "" ;;
    esac
}
for target in $EFFECTIVE_TARGETS; do
    batch_script=$(batch_assembler_for "$target")
    if [ "$effective_sorted" = "$all_sorted" ] && [ -n "$batch_script" ] && [ -x "$batch_script" ]; then
        echo ""
        echo "--- all packages -> $target (batch mode) ---"
        "$batch_script" || {
            echo "ERROR: batch assembly failed for $target" >&2
            exit 1
        }
    else
        for pkg in $EFFECTIVE_PACKAGES; do
            if ! pkg_supports_target "$pkg" "$target"; then
                echo ""
                echo "--- $pkg -> $target (skipped: not in targetLanguages) ---"
                continue
            fi
            echo ""
            echo "--- $pkg -> $target ---"
            invoke_assembler "$pkg" "$target" || {
                echo "ERROR: assembly failed for $pkg / $target" >&2
                exit 1
            }
        done
    fi
done
echo ""

# Phase 3: Testers (unless --no-tests).
if [ "$NO_TEST" = true ]; then
    echo "[Phase 3] Skipped (--no-tests)."
else
    echo "[Phase 3] Running tests..."
    for target in $EFFECTIVE_TARGETS; do
        # Today the testers are monolithic (they run every package's tests as a
        # single target-language test suite), so we only need to invoke the
        # tester once per target, not once per (package, target). We use the
        # last package in dep order as the nominal "test subject" since that
        # minimizes re-running equivalent work.
        last_pkg=$(echo "$EFFECTIVE_PACKAGES" | awk '{print $NF}')
        echo ""
        echo "--- tests for $target (via $last_pkg) ---"
        invoke_tester "$last_pkg" "$target" || {
            echo "ERROR: tests failed for $target (see log above)" >&2
            exit 1
        }
    done
fi

echo ""
echo "=========================================="
echo "Hydra sync: DONE"
echo "=========================================="

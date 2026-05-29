#!/usr/bin/env bash
# Run target-language test suites against the dist/ tree.
#
# Complements bin/sync.sh, which regenerates dist/ but does NOT run
# target-language tests (only Haskell's stack test, via sync-haskell.sh
# Step 6, because Haskell hosts the kernel and its tests are
# preconditional). This script closes that gap: a single command that
# pre-syncs and then runs each requested target's tests, so local
# validation matches what CI does.
#
# Resolves #387.
#
# Usage:
#   bin/test.sh                          # bootstrapping triad: haskell,java,python
#   bin/test.sh java                     # Java only
#   bin/test.sh lang1,lang2[,...]        # comma-separated list
#   bin/test.sh lisp                     # expands to all four Lisp dialects
#   bin/test.sh all                      # every target
#   bin/test.sh --no-sync <targets>      # skip the pre-sync; tests only
#   bin/test.sh --help
#
# Cache behavior: each per-target test-distribution.sh has its own
# dist/<lang>/test-cache.json keyed on generated sources + test infra.
# On warm trees the whole run is near-instant (sync is no-op + every
# per-language test cache hits).
#
# Compared with /sync (defaults to all × all):
#   /test defaults to the bootstrapping triad — matches /bootstrap's
#   default. /test all is a deliberate ask for pre-release validation.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

# Languages whose test-distribution.sh we know how to call. Order is the
# default-triad-first convention used by /bootstrap.
ALL_TARGETS="haskell java python scala go typescript clojure scheme common-lisp emacs-lisp"

usage() {
    sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
}

# Parse args.
DO_SYNC=1
TARGETS_ARG=""

while [ $# -gt 0 ]; do
    case "$1" in
        --no-sync) DO_SYNC=0; shift ;;
        --help|-h) usage; exit 0 ;;
        --*) echo "Unknown flag: $1" >&2; exit 2 ;;
        *)
            if [ -n "$TARGETS_ARG" ]; then
                echo "Unexpected extra argument: $1" >&2
                exit 2
            fi
            TARGETS_ARG="$1"
            shift
            ;;
    esac
done

# Expand the scope argument.
#   (no arg)    -> haskell,java,python (the bootstrapping triad)
#   "all"       -> every target
#   "lisp"      -> all four Lisp dialects
#   "X,Y,Z"     -> verbatim
expand_targets() {
    local arg="$1"
    if [ -z "$arg" ]; then
        echo "haskell java python"
        return
    fi
    local out=""
    IFS=',' read -ra TOKENS <<< "$arg"
    for t in "${TOKENS[@]}"; do
        case "$t" in
            all) out="$out $ALL_TARGETS" ;;
            lisp) out="$out clojure scheme common-lisp emacs-lisp" ;;
            haskell|java|python|scala|go|typescript|clojure|scheme|common-lisp|emacs-lisp)
                out="$out $t" ;;
            *)
                echo "Unknown target '$t'. Valid: $ALL_TARGETS, lisp, all" >&2
                exit 2
                ;;
        esac
    done
    # Dedupe while preserving order.
    local seen=""
    local result=""
    for x in $out; do
        case " $seen " in
            *" $x "*) ;;
            *) seen="$seen $x"; result="$result $x" ;;
        esac
    done
    echo "$result"
}

TARGETS=$(expand_targets "$TARGETS_ARG")

echo "=== /test scope: $TARGETS ==="
echo ""

# Pre-sync, unless --no-sync.
# We pre-sync via the per-language wrappers when the requested targets are
# a small set, to keep cold-start cost minimal. For larger sets, use the
# full bin/sync.sh.
#
# Cold-checkout caveat (#387 design note): standalone /test java needs
# python/lisp/typescript dists too because the Java rollup's
# compileHeadsExtrasJava imports from them. To stay safe and simple, we
# always go through bin/sync.sh, which knows how to populate those
# dependencies; warm-cache runs are nearly free anyway.
if [ "$DO_SYNC" = "1" ]; then
    echo "=== Pre-sync via bin/sync.sh (warm-cache fast) ==="
    "$HYDRA_ROOT/bin/sync.sh" --no-tests --hosts haskell --targets "$(echo $TARGETS | tr ' ' ',')"
    echo ""
fi

# Per-language test invocation.
declare -a RESULTS=()
declare -a FAILED=()
OVERALL_RC=0

run_test() {
    local lang="$1"
    local cmd="$2"
    echo "--- $lang ---"
    echo "  \$ $cmd"
    if eval "$cmd"; then
        RESULTS+=("$lang: PASS")
    else
        RESULTS+=("$lang: FAIL")
        FAILED+=("$lang")
        OVERALL_RC=1
    fi
    echo ""
}

for t in $TARGETS; do
    case "$t" in
        haskell)
            # Haskell tests already ran inside sync-haskell.sh Step 6,
            # invoked by bin/sync.sh above. /test haskell with --no-sync
            # asks for an explicit re-run; otherwise skip (the sync just
            # ran them).
            if [ "$DO_SYNC" = "0" ]; then
                run_test "haskell" "(cd $HYDRA_ROOT/heads/haskell && stack test)"
            else
                echo "--- haskell ---"
                echo "  (already run inside sync-haskell.sh Step 6; skipping)"
                RESULTS+=("haskell: PASS (via sync)")
                echo ""
            fi
            ;;
        java)
            run_test "java" "$HYDRA_ROOT/heads/java/bin/test-distribution.sh hydra-kernel"
            ;;
        python)
            run_test "python" "$HYDRA_ROOT/heads/python/bin/test-distribution.sh hydra-kernel"
            ;;
        scala)
            run_test "scala" "$HYDRA_ROOT/heads/scala/bin/test-distribution.sh hydra-kernel"
            ;;
        go)
            run_test "go" "$HYDRA_ROOT/heads/go/bin/test-distribution.sh hydra-kernel"
            ;;
        typescript)
            run_test "typescript" "$HYDRA_ROOT/heads/typescript/bin/test-distribution.sh hydra-kernel"
            ;;
        clojure|scheme|common-lisp|emacs-lisp)
            run_test "$t" "$HYDRA_ROOT/heads/lisp/$t/bin/test-distribution.sh hydra-kernel"
            ;;
        *)
            echo "Internal error: unhandled target '$t'" >&2
            exit 2
            ;;
    esac
done

# Per-target summary.
echo "==========================================="
echo "  /test summary"
echo "==========================================="
for r in "${RESULTS[@]}"; do
    echo "  $r"
done
echo ""

if [ "$OVERALL_RC" = "0" ]; then
    echo "All target test suites passed."
else
    echo "Failed targets: ${FAILED[*]}"
    echo "Re-run a specific target with: bin/test.sh --no-sync <lang>"
fi

exit "$OVERALL_RC"

#!/usr/bin/env bash
set -euo pipefail

# Exhaustive sync: every package × every target language, with tests.
#
# Where bin/sync.sh prepares the (host, target) matrix needed for
# bootstrapping (skipping packages and combinations not on that critical
# path), this script regenerates EVERY package into EVERY target and
# runs each target's test suite. It exists to surface broken
# combinations that bin/sync.sh deliberately avoids.
#
# Use periodically — once a release cycle, before a major refactor lands,
# or after touching the code generator. Expect long runtime.
#
# Hydra's house rule: fail fast on the first broken combination. This
# script does not aggregate failures or run-to-completion; it stops the
# moment something doesn't generate cleanly. The first failure is the
# bug to fix.
#
# Package list comes from the single source of truth: hydra.json's
# "packages" array. To add or remove a package, edit hydra.json.
#
# Usage:
#   bin/sync-all.sh           # exhaustive run with tests
#   bin/sync-all.sh --no-tests   # skip target-language tests (still runs
#                             # Phase 1's stack test)
#   bin/sync-all.sh --help

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$HYDRA_ROOT/bin/lib/common.sh"

NO_TESTS=false

while [ $# -gt 0 ]; do
    case "$1" in
        --no-tests) NO_TESTS=true ;;
        --help|-h)
            cat <<'EOF'
Usage: bin/sync-all.sh [--no-tests] [--help]

Exhaustive regeneration: every package in hydra.json × every target
language, with tests. Fails fast on the first broken combination.

Options:
  --no-tests   Skip target-language test suites. Phase 1's 'stack test'
            still runs.
  --help    Show this help.

Targets covered: every supported language.
Packages covered: every entry in hydra.json's "packages" array.

Use periodically to surface combinations that bin/sync.sh skips for
the bootstrapping path. For routine work, prefer:
  bin/sync.sh --hosts <H,...> --targets <T,...>      (matrix prep)
  bin/sync-default.sh                                 (haskell,java,python)
  bin/sync-<lang>.sh                                  (single language)
  bin/sync-packages.sh <pkg> [--target <lang>]        (per-package)
EOF
            exit 0
            ;;
        *)
            die "Unknown argument: $1 (try --help)"
            ;;
    esac
    shift
done

# Single source of truth.
ALL_PACKAGES=$(python3 -c "import json; print(' '.join(json.load(open('$HYDRA_ROOT/hydra.json'))['packages']))")
ALL_TARGETS="haskell java python scala clojure scheme common-lisp emacs-lisp"

# Defer to bin/sync-packages.sh for the actual orchestration; it already
# handles Phase 1 (DSL → JSON), per-package assembly, dependency-ordering,
# and Layer 2.5 testers. We just hand it the full (pkg, target) cross
# product. fail-fast is implicit via 'set -euo pipefail' inside that
# script.

NO_TEST_FLAG=""
if [ "$NO_TESTS" = true ]; then
    NO_TEST_FLAG="--no-test"
fi

START_TIME=$SECONDS
trap 'echo ""; ELAPSED=$((SECONDS - START_TIME)); echo "Total elapsed time: $((ELAPSED / 60))m $((ELAPSED % 60))s"' EXIT

banner1 "Hydra exhaustive sync (sync-all.sh)"
echo "  Packages: $ALL_PACKAGES"
echo "  Targets:  $ALL_TARGETS"
echo "  Tests:    $([ "$NO_TESTS" = true ] && echo "skipped (--no-tests)" || echo "enabled")"
echo ""

# Hand off to the per-package layered tool.
"$SCRIPT_DIR/sync-packages.sh" $NO_TEST_FLAG

banner1_done "sync-all.sh complete!"

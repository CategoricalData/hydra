#!/usr/bin/env bash
# Layer 2.5 tester: run go build + go test against an already-assembled
# Go distribution. The Go head is a "head bud" today; this is the minimal
# verification step until per-package go.mod scaffolding lands.
#
# Strategy: stitch the generated package's source tree together with the
# heads/go/ runtime under a temporary workspace so go can resolve both
# `github.com/CategoricalData/hydra/heads/go/...` (hand-written runtime)
# and the generated module imports.

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package>" >&2
    exit 1
fi

PACKAGE="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
THIS_SCRIPT="$SCRIPT_DIR/$(basename "${BASH_SOURCE[0]}")"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_GO_HEAD="$HYDRA_ROOT_DIR/heads/go"
DIST_DIR="$HYDRA_ROOT_DIR/dist/go/$PACKAGE/src/main/go"

if [ ! -d "$DIST_DIR" ]; then
    echo "No generated Go sources at $DIST_DIR; skipping." >&2
    exit 0
fi

source "$HYDRA_ROOT_DIR/bin/lib/test-cache.sh"
if test_cache_check go "$HYDRA_ROOT_DIR/dist/go" "$HYDRA_GO_HEAD/src" "$THIS_SCRIPT"; then
    echo "  Cache hit: no changes since last successful Go test run; skipping."
    echo "=== Done (cache hit). ==="
    exit 0
fi

echo "=== Testing Go distribution: $PACKAGE ==="
echo "  Source: $DIST_DIR"
echo ""

cd "$DIST_DIR"

if [ ! -f go.mod ]; then
    echo "Initializing per-package go.mod for $PACKAGE..."
    cat > go.mod <<EOF
module github.com/CategoricalData/hydra/dist/go/$PACKAGE

go 1.22
EOF
fi

echo "Running go build ./..."
go build ./...

if find . -name '*_test.go' -print -quit | grep -q .; then
    echo "Running go test ./..."
    go test ./...
else
    echo "(no _test.go files in $PACKAGE; skipping go test)"
fi

test_cache_record go "$HYDRA_ROOT_DIR/dist/go" "$HYDRA_GO_HEAD/src" "$THIS_SCRIPT"

echo ""
echo "=== Done. ==="

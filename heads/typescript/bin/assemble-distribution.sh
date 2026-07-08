#!/usr/bin/env bash
# assemble-distribution.sh — TypeScript distribution assembler.
#
# Called by bin/sync.sh during Phase 3 (target=typescript). Generates
# `<pkg>` into dist/typescript/<pkg>/src/main/typescript/ and, for the
# kernel, copies the hand-written TS runtime alongside. Also calls
# bin/lib/generate-typescript-package-build.py to emit the publishable
# package.json + tsconfig.build.json for each package (#492).
#
# Usage:
#   assemble-distribution.sh <pkg>

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "usage: assemble-distribution.sh <pkg>" >&2
    exit 2
fi

PKG="$1"

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_TS_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_TS_HEAD/../.." && pwd )"

# Generate the npm package.json + tsconfig.build.json for any package that has
# a packages/<pkg>/package.json entry (i.e. all non-skipped packages).
generate_npm_build() {
    local pkg="$1"
    HYDRA_ROOT_DIR="$HYDRA_ROOT" python3 \
        "$HYDRA_ROOT/bin/lib/generate-typescript-package-build.py" "$pkg"
}

# Symlink all .ts files from a source hydra/ tree into a target hydra/ tree,
# skipping files that already exist (own or previously symlinked sources win).
symlink_hydra_tree() {
    local src_hydra="$1"
    local dest_hydra="$2"
    if [ -d "$dest_hydra" ] && [ -d "$src_hydra" ]; then
        while IFS= read -r sfile; do
            rel="${sfile#$src_hydra/}"
            dest="$dest_hydra/$rel"
            if [ ! -e "$dest" ]; then
                mkdir -p "$(dirname "$dest")"
                ln -sf "$sfile" "$dest"
            fi
        done < <(find "$src_hydra" -name "*.ts" -not -path "*/node_modules/*")
    fi
}

case "$PKG" in
    hydra-kernel)
        echo "  Generating $PKG -> typescript (main + test)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" test \
            --output "$HYDRA_ROOT/dist/typescript"
        "$HYDRA_TS_HEAD/bin/copy-kernel-runtime.sh" \
            --dist-root "$HYDRA_ROOT/dist/typescript"
        generate_npm_build "$PKG"
        ;;
    hydra-rdf)
        echo "  Generating $PKG -> typescript (main)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        # Symlink kernel files so relative imports resolve at compile time.
        PKG_HYDRA="$HYDRA_ROOT/dist/typescript/$PKG/src/main/typescript/hydra"
        KERNEL_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra"
        symlink_hydra_tree "$KERNEL_HYDRA" "$PKG_HYDRA"
        generate_npm_build "$PKG"
        ;;
    hydra-pg)
        echo "  Generating $PKG -> typescript (main)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        # Symlink kernel and hydra-rdf files so all relative imports resolve.
        # hydra-rdf must be assembled before hydra-pg (sync.sh ensures this order).
        PKG_HYDRA="$HYDRA_ROOT/dist/typescript/$PKG/src/main/typescript/hydra"
        KERNEL_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra"
        RDF_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-rdf/src/main/typescript/hydra"
        symlink_hydra_tree "$KERNEL_HYDRA" "$PKG_HYDRA"
        symlink_hydra_tree "$RDF_HYDRA" "$PKG_HYDRA"
        generate_npm_build "$PKG"
        ;;
    hydra-build)
        echo "  Generating $PKG -> typescript (main + test)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" test \
            --output "$HYDRA_ROOT/dist/typescript"
        # Cross-package self-containment: symlink kernel files into this package.
        PKG_HYDRA="$HYDRA_ROOT/dist/typescript/$PKG/src/main/typescript/hydra"
        KERNEL_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra"
        symlink_hydra_tree "$KERNEL_HYDRA" "$PKG_HYDRA"
        # hydra-kernel's own testSuite.ts imports hydra.test.build.* by relative
        # path (hydra.test.testSuite is kernel-owned per #546/#547 and still
        # references these test groups), expecting them alongside its own test
        # tree. Symlink hydra-build's test/build subtree into hydra-kernel's test
        # tree so that import resolves. hydra-kernel is assembled before
        # hydra-build in sync.sh's Phase 3 loop, so its test tree already exists.
        BUILD_TEST_HYDRA="$HYDRA_ROOT/dist/typescript/$PKG/src/test/typescript/hydra"
        KERNEL_TEST_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/test/typescript/hydra"
        symlink_hydra_tree "$BUILD_TEST_HYDRA" "$KERNEL_TEST_HYDRA"
        # The build test files themselves import their sibling main modules via
        # relative paths into hydra/build/*.js. Symlink hydra-build's own (i.e.
        # non-symlinked) hydra/build/ main subtree into hydra-kernel's main tree
        # too, so those imports resolve from the test files' new home.
        KERNEL_BUILD_DEST="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra/build"
        if [ -d "$PKG_HYDRA/build" ] && [ ! -e "$KERNEL_BUILD_DEST" ]; then
            ln -sf "$PKG_HYDRA/build" "$KERNEL_BUILD_DEST"
        fi
        generate_npm_build "$PKG"
        ;;
    hydra-coq|hydra-wasm|hydra-ext)
        echo "  skipping: $PKG -> typescript (not yet supported)"
        ;;
    *)
        echo "  Generating $PKG -> typescript (main)"
        "$HYDRA_ROOT/heads/haskell/bin/transform-json-to-target.sh" \
            typescript "$PKG" main \
            --output "$HYDRA_ROOT/dist/typescript"
        # Cross-package self-containment: symlink kernel files into this package.
        PKG_HYDRA="$HYDRA_ROOT/dist/typescript/$PKG/src/main/typescript/hydra"
        KERNEL_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-kernel/src/main/typescript/hydra"
        symlink_hydra_tree "$KERNEL_HYDRA" "$PKG_HYDRA"
        # hydra-java and hydra-scala both import ../jvm/serde.js from hydra-jvm.
        # Symlink hydra-jvm's jvm/ subdir into each so the relative import resolves.
        JVM_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-jvm/src/main/typescript/hydra/jvm"
        JVM_DEST="$PKG_HYDRA/jvm"
        if [ "$PKG" = "hydra-java" ] || [ "$PKG" = "hydra-scala" ]; then
            if [ -d "$JVM_HYDRA" ] && [ ! -e "$JVM_DEST" ]; then
                ln -sf "$JVM_HYDRA" "$JVM_DEST"
                echo "  Symlinked hydra-jvm/jvm/ into $PKG for cross-coder import"
            fi
        fi
        # hydra-scala's serde also imports ../java/serde.js (a sibling coder package).
        # Symlink hydra-java's java/ subdir into hydra-scala so the import resolves.
        if [ "$PKG" = "hydra-scala" ]; then
            JAVA_HYDRA="$HYDRA_ROOT/dist/typescript/hydra-java/src/main/typescript/hydra/java"
            SCALA_JAVA_DEST="$PKG_HYDRA/java"
            if [ -d "$JAVA_HYDRA" ] && [ ! -e "$SCALA_JAVA_DEST" ]; then
                ln -sf "$JAVA_HYDRA" "$SCALA_JAVA_DEST"
                echo "  Symlinked hydra-java/java/ into $PKG for cross-coder import"
            fi
        fi
        generate_npm_build "$PKG"
        ;;
esac

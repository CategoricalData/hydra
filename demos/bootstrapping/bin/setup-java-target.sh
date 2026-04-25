#!/bin/bash
# Clean output directory and copy static resources for a Java bootstrap target.
# This is host-language-independent: the same static resources are needed
# regardless of which host (Haskell, Java, Python) generated the code.
#
# Usage: ./setup-java-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT/heads/java"
JAVA_RESOURCES="$SCRIPT_DIR/../resources/java"

# Clean and create output directory
echo "Preparing output directory: $OUTPUT_DIR"
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"

# Copy static resources
echo "Copying static resources for Java target..."

# Build files
echo "  Copying build files..."
cp "$JAVA_RESOURCES/build.gradle" "$OUTPUT_DIR/"
cp "$JAVA_RESOURCES/settings.gradle" "$OUTPUT_DIR/"
cp "$JAVA_RESOURCES/README.md" "$OUTPUT_DIR/"
cp "$HYDRA_ROOT/gradlew" "$OUTPUT_DIR/"
cp -r "$HYDRA_ROOT/gradle" "$OUTPUT_DIR/"

# Hand-written source files
echo "  Copying hand-written source files..."
JAVA_SRC="$HYDRA_JAVA_DIR/src/main/java/hydra"
JAVA_DST="$OUTPUT_DIR/src/main/java/hydra"
mkdir -p "$JAVA_DST"

for f in Adapters.java Bootstrap.java Coders.java Generation.java HydraTestBase.java; do
    cp "$JAVA_SRC/$f" "$JAVA_DST/"
done

for d in lib dsl util; do
    cp -r "$JAVA_SRC/$d" "$JAVA_DST/"
done

mkdir -p "$JAVA_DST/tools"
for f in Function3.java Function4.java LList.java MapperBase.java PrettyPrinter.java PrimitiveFunction.java; do
    cp "$JAVA_SRC/tools/$f" "$JAVA_DST/tools/"
done

# Hand-written test files
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/java/hydra"
for f in ReductionTest.java VisitorTest.java TestSuiteRunner.java TestEnv.java; do
    if [ -f "$HYDRA_JAVA_DIR/src/test/java/hydra/$f" ]; then
        cp "$HYDRA_JAVA_DIR/src/test/java/hydra/$f" "$OUTPUT_DIR/src/test/java/hydra/"
    fi
done
# Also copy hydra/test/TestEnv.java — the generated TestGraph.java references
# hydra.test.TestEnv (the package-qualified name), not hydra.TestEnv. The DSL
# emits this reference directly (post-2026-04 TestEnv handoff change), and
# bootstrap-from-json filters hydra.test.testEnv from generation so the
# hand-written file is the source of truth.
mkdir -p "$OUTPUT_DIR/src/test/java/hydra/test"
if [ -f "$HYDRA_JAVA_DIR/src/test/java/hydra/test/TestEnv.java" ]; then
    cp "$HYDRA_JAVA_DIR/src/test/java/hydra/test/TestEnv.java" "$OUTPUT_DIR/src/test/java/hydra/test/"
fi

# Ensure every coder package Generation.java references has a Java distribution.
# Generation.java imports hydra.{java,python,haskell,lisp,scala}.{Coder,Language,Syntax};
# its writeLispDialect references require hydra-lisp's Java output, and its
# scala dispatch references require hydra-scala's Java output. sync-default()
# does not generate these (since lisp/scala aren't in its target set), so on a
# clean checkout they can be missing. Assemble them on demand — warm-cache
# short-circuits in seconds.
echo "  Ensuring Java coder packages are present (warm cache: seconds)..."
# Redirect assembler output to a log file: it prints its own "Done: N main..."
# line per package, which would otherwise pollute the bootstrap-all log
# parser (which extracts host fileCount/timing from "Done:" lines).
ASSEMBLE_LOG="$OUTPUT_DIR/.coder-assembly.log"
mkdir -p "$OUTPUT_DIR"
: > "$ASSEMBLE_LOG"
for coder_pkg in hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp; do
    coder_base="$HYDRA_ROOT/dist/java/$coder_pkg/src/main/java"
    if [ ! -d "$coder_base/hydra" ]; then
        echo "    $coder_pkg not present; generating into Java (see $ASSEMBLE_LOG)..."
        (cd "$HYDRA_ROOT" && heads/java/bin/assemble-distribution.sh "$coder_pkg") >> "$ASSEMBLE_LOG" 2>&1
    fi
done

# Copy coder packages from baseline.
# Generation.java imports hydra.{java,python,haskell,lisp,scala}.{Coder,Language,Syntax}
# which are generated in each coder package's own per-package dist dir now. The
# bootstrap target only generates the SELECTED target language's kernel; we pull in
# the OTHER coder packages' Java output from dist/java/hydra-<lang>/src/main/java/
# so Generation.java compiles.
echo "  Copying coder packages from baseline..."
JAVA_GEN="$OUTPUT_DIR/src/main/java"
mkdir -p "$JAVA_GEN/hydra"
for coder_pkg in hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp; do
    coder_base="$HYDRA_ROOT/dist/java/$coder_pkg/src/main/java"
    if [ -d "$coder_base/hydra" ]; then
        for ns in $(ls "$coder_base/hydra"); do
            src="$coder_base/hydra/$ns"
            dst="$JAVA_GEN/hydra/$ns"
            [ -d "$src" ] || continue
            if [ ! -e "$dst" ]; then
                cp -r "$src" "$dst"
            fi
        done
        echo "    Copied $coder_pkg coder packages"
    fi
done
# Copy ext/domain modules (pg, rdf, cypher, tinkerpop, etc.) from hydra-pg and hydra-rdf.
# These don't collide with the target-language generated kernel because the kernel
# generation doesn't produce them.
for ext_pkg in hydra-pg hydra-rdf; do
    ext_base="$HYDRA_ROOT/dist/java/$ext_pkg/src/main/java"
    if [ -d "$ext_base/hydra" ]; then
        for ns in $(ls "$ext_base/hydra"); do
            src="$ext_base/hydra/$ns"
            dst="$JAVA_GEN/hydra/$ns"
            [ -d "$src" ] || continue
            if [ ! -e "$dst" ]; then
                cp -r "$src" "$dst"
            fi
        done
        echo "    Copied $ext_pkg ext/domain packages"
    fi
done

# Create symlink to hydra-kernel so that relative paths (../hydra-kernel/...)
# used by TestSuiteRunner.java to find JSON modules resolve correctly.
echo "  Creating hydra-kernel symlink..."
if [ ! -e "$OUTPUT_DIR/../hydra-kernel" ]; then
    ln -s "$HYDRA_ROOT/packages/hydra-kernel" "$OUTPUT_DIR/../hydra-kernel"
    echo "    Symlinked ../hydra-kernel -> $HYDRA_ROOT/packages/hydra-kernel"
fi

# Summary
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
echo "  Static resources: $STATIC_COUNT files"
echo ""

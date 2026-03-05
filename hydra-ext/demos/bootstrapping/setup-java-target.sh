#!/bin/bash
# Copy static resources and run tests for a Java bootstrap target directory.
# This is host-language-independent: the same static resources are needed
# regardless of which host (Haskell, Java, Python) generated the code.
#
# Usage: ./setup-java-target.sh <output-dir>
#
# The output directory should already contain src/gen-main/java/ with generated code.

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_JAVA_DIR="$HYDRA_ROOT/hydra-java"
JAVA_RESOURCES="$SCRIPT_DIR/resources/java"

# Step: Copy static resources
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

for f in Adapters.java Bootstrap.java Coders.java Generation.java HydraTestBase.java Lexical.java; do
    cp "$JAVA_SRC/$f" "$JAVA_DST/"
done

for d in lib dsl compute json util; do
    cp -r "$JAVA_SRC/$d" "$JAVA_DST/"
done

mkdir -p "$JAVA_DST/tools"
for f in Function3.java Function4.java LList.java MapperBase.java PrettyPrinter.java PrimitiveFunction.java; do
    cp "$JAVA_SRC/tools/$f" "$JAVA_DST/tools/"
done

# Hand-written test files
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/java/hydra"
for f in ReductionTest.java VisitorTest.java TestSuiteRunner.java; do
    if [ -f "$HYDRA_JAVA_DIR/src/test/java/hydra/$f" ]; then
        cp "$HYDRA_JAVA_DIR/src/test/java/hydra/$f" "$OUTPUT_DIR/src/test/java/hydra/"
    fi
done

JAVA_GEN="$OUTPUT_DIR/src/gen-main/java"
JAVA_BASELINE="$HYDRA_JAVA_DIR/src/gen-main/java"

# Copy ext modules from baseline, replacing any generated versions.
# The bootstrap may generate ext modules with incorrect file paths for the
# Java target (e.g. hydra/ext/syntax/ instead of hydra/ext/java/syntax/).
# Using the baseline ensures correct package/path alignment.
echo "  Copying ext modules from baseline..."
if [ -d "$JAVA_BASELINE/hydra/ext" ]; then
    # Remove any incorrectly-generated ext directories first
    rm -rf "$JAVA_GEN/hydra/ext"
    cp -r "$JAVA_BASELINE/hydra/ext" "$JAVA_GEN/hydra/"
    echo "    Copied hydra/ext from baseline"
fi

# Create symlink to hydra-haskell so that relative paths (../hydra-haskell/...)
# used by TestSuiteRunner.java to find JSON modules resolve correctly.
echo "  Creating hydra-haskell symlink..."
if [ ! -e "$OUTPUT_DIR/../hydra-haskell" ]; then
    ln -s "$HYDRA_ROOT/hydra-haskell" "$OUTPUT_DIR/../hydra-haskell"
    echo "    Symlinked ../hydra-haskell -> $HYDRA_ROOT/hydra-haskell"
fi

# Summary
MAIN_COUNT=$(find "$OUTPUT_DIR/src/gen-main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
TEST_COUNT=$(find "$OUTPUT_DIR/src/gen-test" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
echo "  Generated main modules:   $MAIN_COUNT files"
echo "  Generated test modules:   $TEST_COUNT files"
echo "  Static resources:         $STATIC_COUNT files"
echo ""

# Build and run tests
echo "Building and running Java tests..."
STEP_START=$(date +%s)
cd "$OUTPUT_DIR"
./gradlew compileJava compileTestJava 2>&1
BUILD_END=$(date +%s)
echo "  Build time: $((BUILD_END - STEP_START))s"
./gradlew test 2>&1
TEST_EXIT=$?
TEST_END=$(date +%s)
echo "  Test time: $((TEST_END - BUILD_END))s"
echo ""

exit $TEST_EXIT

#!/bin/bash
# Bootstrap Hydra to Java from JSON modules.
# Generates code into a standalone /tmp directory, copies static resources,
# then builds and runs the Java test suite.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_EXT_DIR="$HYDRA_ROOT/hydra-ext"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HYDRA_JAVA_DIR="$HYDRA_ROOT/hydra-java"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/haskell-to-java"
RTS_FLAGS="+RTS -K512M -A64M -RTS"

echo "=========================================="
echo "Bootstrap: Haskell -> JSON -> Java"
echo "=========================================="
echo ""
echo "Output directory: $OUTPUT_DIR"
echo ""

# Step 1: Ensure JSON is up to date
echo "Step 1: Ensuring JSON exports are up to date..."
echo "  Building hydra-haskell..."
cd "$HYDRA_HASKELL_DIR"
stack build 2>&1 | tail -3
echo "  Exporting main modules to JSON..."
stack exec update-json-main -- $RTS_FLAGS
echo "  Exporting test modules to JSON..."
stack exec update-json-test -- $RTS_FLAGS
echo "  Building hydra-ext..."
cd "$HYDRA_EXT_DIR"
stack build 2>&1 | tail -3
echo "  Exporting ext modules to JSON..."
stack exec update-json-main -- $RTS_FLAGS
echo ""

# Step 2: Clean output directory
echo "Step 2: Preparing output directory..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
echo "  Cleaned: $OUTPUT_DIR"
echo ""

# Step 3: Bootstrap from JSON (generate code to /tmp)
echo "Step 3: Generating Java code from JSON..."
cd "$HYDRA_EXT_DIR"
stack exec bootstrap-from-json -- --target java --output "$OUTPUT_BASE" $RTS_FLAGS
echo ""

# Step 4: Copy static resources
echo "Step 4: Copying static resources..."

# Build files
echo "  Copying build files..."
cp "$HYDRA_JAVA_DIR/build.gradle" "$OUTPUT_DIR/"
cp "$HYDRA_JAVA_DIR/settings.gradle" "$OUTPUT_DIR/"
# Copy Gradle wrapper if it exists
if [ -f "$HYDRA_JAVA_DIR/gradlew" ]; then
  cp "$HYDRA_JAVA_DIR/gradlew" "$OUTPUT_DIR/"
  cp "$HYDRA_JAVA_DIR/gradlew.bat" "$OUTPUT_DIR/" 2>/dev/null || true
  cp -r "$HYDRA_JAVA_DIR/gradle" "$OUTPUT_DIR/" 2>/dev/null || true
fi
# Copy gradle.properties from root if it exists
if [ -f "$HYDRA_ROOT/gradle.properties" ]; then
  cp "$HYDRA_ROOT/gradle.properties" "$OUTPUT_DIR/"
fi

# Hand-written core classes
echo "  Copying hand-written core classes..."
mkdir -p "$OUTPUT_DIR/src/main/java/hydra"
for f in HydraTestBase.java Adapters.java Coders.java Lexical.java; do
  if [ -f "$HYDRA_JAVA_DIR/src/main/java/hydra/$f" ]; then
    cp "$HYDRA_JAVA_DIR/src/main/java/hydra/$f" "$OUTPUT_DIR/src/main/java/hydra/"
  fi
done

# Hand-written test files
echo "  Copying test infrastructure..."
mkdir -p "$OUTPUT_DIR/src/test/java/hydra"
for f in ReductionTest.java VisitorTest.java TestSuiteRunner.java; do
  if [ -f "$HYDRA_JAVA_DIR/src/test/java/hydra/$f" ]; then
    cp "$HYDRA_JAVA_DIR/src/test/java/hydra/$f" "$OUTPUT_DIR/src/test/java/hydra/"
  fi
done
echo ""

# Summary
echo "Step 5: Summary of generated files..."
MAIN_COUNT=$(find "$OUTPUT_DIR/src/gen-main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
TEST_COUNT=$(find "$OUTPUT_DIR/src/gen-test" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
echo "  Generated main modules:   $MAIN_COUNT files"
echo "  Generated test modules:   $TEST_COUNT files"
echo "  Static resources:         $STATIC_COUNT files"
echo ""

echo "=========================================="
echo "Bootstrap to Java: COMPLETE"
echo "Output: $OUTPUT_DIR"
echo "=========================================="

#!/bin/bash
# Bootstrap Hydra to Java from JSON modules (via Java host).
# Generates code into a standalone /tmp directory, copies static resources,
# then builds and runs the Java test suite.
#
# Usage: ./java-to-java.sh [--types-only] [--kernel-only]

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/hydra-haskell"
HYDRA_JAVA_DIR="$HYDRA_ROOT/hydra-java"

OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"
OUTPUT_DIR="$OUTPUT_BASE/java-to-java"

# Collect extra flags
EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --types-only|--kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS $arg" ;;
    esac
done

echo "=========================================="
echo "Bootstrap: Java -> JSON -> Java"
echo "=========================================="
echo ""
echo "  Host language:   Java"
echo "  Target language: Java"
echo "  Output:          $OUTPUT_DIR"
if [ -n "$EXTRA_FLAGS" ]; then
    echo "  Flags:          $EXTRA_FLAGS"
fi
echo ""

TOTAL_START=$(date +%s)

# Step 1: Verify JSON modules exist
# JSON is generated once by bin/update-json-kernel.sh from the Haskell host
# and checked in. The demo simply consumes these files.
echo "Step 1: Verifying JSON modules..."
JSON_DIR="$HYDRA_HASKELL_DIR/src/gen-main/json"
JSON_COUNT=$(find "$JSON_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
if [ "$JSON_COUNT" -eq 0 ]; then
    echo "  ERROR: No JSON files found in $JSON_DIR"
    echo "  Run bin/update-json-kernel.sh first to generate JSON modules."
    exit 1
fi
echo "  JSON input files: $JSON_COUNT (in $JSON_DIR)"
echo ""

# Step 2: Clean output directory
echo "Step 2: Preparing output directory..."
rm -rf "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR"
echo "  Cleaned: $OUTPUT_DIR"
echo ""

# Step 3: Bootstrap from JSON via Java host
echo "Step 3: Generating Java code from JSON (via Java host)..."
STEP_START=$(date +%s)
"$SCRIPT_DIR/java-bootstrap.sh" --target java --output "$OUTPUT_BASE" $EXTRA_FLAGS 2>&1
STEP_END=$(date +%s)
echo "  Time: $((STEP_END - STEP_START))s"
echo ""

# Step 4: Copy static resources
echo "Step 4: Copying static resources..."

# Build files (standalone build.gradle, not the multi-project one)
echo "  Creating standalone build files..."
cp "$HYDRA_JAVA_DIR/settings.gradle" "$OUTPUT_DIR/"
cp "$HYDRA_ROOT/gradlew" "$OUTPUT_DIR/"
cp -r "$HYDRA_ROOT/gradle" "$OUTPUT_DIR/"
cat > "$OUTPUT_DIR/build.gradle" << 'GRADLE_EOF'
// Standalone build.gradle for bootstrapped Hydra Java
plugins {
    id 'java-library'
}

group = 'net.fortytwo.hydra'
version = '0.12.0'

repositories {
    mavenCentral()
}

sourceSets {
    main {
        java {
            srcDirs = ['src/gen-main/java', 'src/main/java']
        }
    }
    test {
        java {
            srcDirs = ['src/gen-test/java', 'src/test/java']
        }
    }
}

dependencies {
    api group: 'org.apache.commons', name: 'commons-text', version: '1.10.0'
    api group: 'com.cedarsoftware', name: 'json-io', version: '4.14.1'

    api(platform(group: 'org.junit', name: 'junit-bom', version: '5.9.2'))
    api('org.junit.jupiter:junit-jupiter')
    api group: 'org.junit.jupiter', name: 'junit-jupiter-params', version: '5.9.2'
}

test {
    useJUnitPlatform()
    testLogging {
        events "passed", "failed", "skipped"
        exceptionFormat "short"
    }
}
GRADLE_EOF

# Hand-written source files: explicit inventory of what the Hydra
# implementation needs beyond generated code.
echo "  Copying hand-written source files..."
JAVA_SRC="$HYDRA_JAVA_DIR/src/main/java/hydra"
JAVA_DST="$OUTPUT_DIR/src/main/java/hydra"
mkdir -p "$JAVA_DST"

# Top-level files (excludes Bootstrap.java, Generation.java which have
# external or host-specific dependencies)
for f in Adapters.java Coders.java HydraTestBase.java Lexical.java; do
    cp "$JAVA_SRC/$f" "$JAVA_DST/"
done

# Directories copied wholesale
for d in lib dsl compute json util; do
    cp -r "$JAVA_SRC/$d" "$JAVA_DST/"
done

# tools/ — individual files (excludes AntlrReaderBase.java which depends on ANTLR)
mkdir -p "$JAVA_DST/tools"
for f in FlowException.java Function3.java Function4.java LList.java MapperBase.java PrettyPrinter.java PrimitiveFunction.java; do
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

echo ""

# Step 6: Run tests
echo "Step 6: Running tests..."
STEP_START=$(date +%s)
cd "$OUTPUT_DIR"
./gradlew test 2>&1
TEST_EXIT=$?
STEP_END=$(date +%s)
echo "  Test time: $((STEP_END - STEP_START))s"
echo ""

TOTAL_END=$(date +%s)
TOTAL_SECS=$((TOTAL_END - TOTAL_START))

echo "=========================================="
if [ $TEST_EXIT -eq 0 ]; then
    echo "Bootstrap Java-to-Java: COMPLETE (tests PASSED)"
else
    echo "Bootstrap Java-to-Java: COMPLETE (tests FAILED)"
fi
echo "  Output:     $OUTPUT_DIR"
echo "  Total time: ${TOTAL_SECS}s"
echo "=========================================="

exit $TEST_EXIT

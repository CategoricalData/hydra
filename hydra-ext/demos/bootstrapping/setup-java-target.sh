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

# Step: Copy static resources
echo "Copying static resources for Java target..."

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

# Hand-written source files
echo "  Copying hand-written source files..."
JAVA_SRC="$HYDRA_JAVA_DIR/src/main/java/hydra"
JAVA_DST="$OUTPUT_DIR/src/main/java/hydra"
mkdir -p "$JAVA_DST"

for f in Adapters.java Coders.java HydraTestBase.java Lexical.java; do
    cp "$JAVA_SRC/$f" "$JAVA_DST/"
done

for d in lib dsl compute json util; do
    cp -r "$JAVA_SRC/$d" "$JAVA_DST/"
done

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

# Copy all ext modules from baseline if not generated (needed for tests in kernel-only mode)
echo "  Copying ext modules from baseline (if missing)..."
JAVA_GEN="$OUTPUT_DIR/src/gen-main/java"
JAVA_BASELINE="$HYDRA_JAVA_DIR/src/gen-main/java"
if [ ! -d "$JAVA_GEN/hydra/ext" ] && [ -d "$JAVA_BASELINE/hydra/ext" ]; then
    cp -r "$JAVA_BASELINE/hydra/ext" "$JAVA_GEN/hydra/"
    echo "    Copied hydra/ext from baseline"
fi

# Summary
MAIN_COUNT=$(find "$OUTPUT_DIR/src/gen-main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
TEST_COUNT=$(find "$OUTPUT_DIR/src/gen-test" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
STATIC_COUNT=$(find "$OUTPUT_DIR/src/main" -name "*.java" 2>/dev/null | wc -l | tr -d ' ')
echo "  Generated main modules:   $MAIN_COUNT files"
echo "  Generated test modules:   $TEST_COUNT files"
echo "  Static resources:         $STATIC_COUNT files"
echo ""

# Run tests
echo "Running Java tests..."
STEP_START=$(date +%s)
cd "$OUTPUT_DIR"
./gradlew test 2>&1
TEST_EXIT=$?
STEP_END=$(date +%s)
echo "  Test time: $((STEP_END - STEP_START))s"
echo ""

exit $TEST_EXIT

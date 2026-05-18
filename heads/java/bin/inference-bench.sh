#!/bin/bash
# Cross-host inference benchmark — Java runner wrapper.
#
# Builds the Java head (if needed) and invokes hydra.BenchInference, passing
# any --sizes / --out args through. HYDRA_ROOT is set so the Java runner can
# locate the kernel JSON.
#
# Usage:
#   heads/java/bin/inference-bench.sh [--sizes 10,25,50,100] [--out path/to/result.json]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HYDRA_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

cd "$HYDRA_ROOT"

# headsExtras compiles hydra/Generation.java + friends, which import
# hydra.{python,haskell,lisp,scala,go}.* from dist/java/hydra-*/. Run a
# full sync first to ensure every per-language tree is current; warm-cache
# is fast (~3 min) and cold-cache is self-healing. HYDRA_IN_SYNC=1 means
# sync.sh is already running us and we should not recurse.
if [ "${HYDRA_IN_SYNC:-0}" != "1" ]; then
    echo "=== Running bin/sync.sh to ensure all dist trees are current ==="
    "$HYDRA_ROOT/bin/sync.sh"
fi

# Ensure headsExtras is compiled.
./gradlew --quiet :hydra-java:compileHeadsExtrasJava

# Resolve the classpath from gradle (kernel + coders + headsExtras outputs +
# every transitive jar).
JAVA_CP=$(./gradlew --quiet :hydra-java:printHeadsExtrasRuntimeClasspath)

# -Xss large for deeply nested type inference; -Xmx large for many bindings.
# Note: Generation.loadModulesFromJson prints "Loaded: <namespace>" lines on
# stdout. When --out is set, BenchInference writes the JSON result to the
# file and only useful messages go to stderr; the noisy "Loaded:" lines
# still hit stdout. Redirect stdout to /dev/null when --out is present so
# the wrapper can compose with the dashboard. If --out is missing, leave
# stdout alone so users running this script directly see the JSON.
if [[ "$*" == *"--out"* ]]; then
  HYDRA_ROOT="$HYDRA_ROOT" exec java -Xss64m -Xmx2g -cp "$JAVA_CP" hydra.BenchInference "$@" > /dev/null
else
  HYDRA_ROOT="$HYDRA_ROOT" exec java -Xss64m -Xmx2g -cp "$JAVA_CP" hydra.BenchInference "$@"
fi

#!/usr/bin/env bash
set -euo pipefail
#
# One-shot reproduction of the translingual Neo4j JSON-artifact validation demo.
#
# Authors a Neo4j schema + a family of graphs ONCE in the Java DSL, encodes them
# to Hydra's canonical term-JSON, then validates the SAME JSON in four host
# languages (Java, Python, Haskell, TypeScript) using the SAME generated
# validator, and diffs the outputs to prove they are identical. No running Neo4j
# is required.
#
# Just run it: ./bin/demo.sh   (or scope: ./bin/demo.sh --hosts python,typescript)

# A JDK 17+ is needed for the Java host and data generation. jdk-19 is fine here;
# override JAVA_HOME_17PLUS if your JDK lives elsewhere.
JAVA_HOME_17PLUS="${JAVA_HOME_17PLUS:-/Library/Java/JavaVirtualMachines/jdk-19.jdk/Contents/Home}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export JAVA_HOME="$JAVA_HOME_17PLUS"
export PATH="$JAVA_HOME/bin:$PATH"

exec "$SCRIPT_DIR/run-json.sh" "$@"

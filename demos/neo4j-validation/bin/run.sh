#!/usr/bin/env bash
set -euo pipefail
#
# Orchestrator for the Neo4j validation translingual demo.
#
# Runs the same Hydra Neo4j validation in Java and Python, each connecting to a
# running Neo4j via that language's Neo4j client driver. Both hosts skip
# gracefully (exit 0 with a notice) when no Neo4j is reachable, so this script is
# safe to run offline.
#
# It prepares everything the demo needs:
#   - generates the Hydra kernel + hydra-pg into dist/{java,python} if absent;
#   - downloads the Neo4j Java driver (and its netty/reactor runtime deps) into a
#     local cache;
#   - selects a JDK 17+ for the Java host (the Neo4j driver needs it).
# The Neo4j Python driver must be importable (pip install neo4j); the script
# uses whatever python3 is on PATH (use a venv if you prefer).
#
# Usage: ./run.sh [OPTIONS]
#   --hosts LANG,...   Run only specified hosts (default: java,python)
#   --uri URI          Neo4j Bolt URI (default: bolt://localhost:7687)
#   --user USER        Neo4j user (default: neo4j)
#   --password PASS    Neo4j password (default: neo4j)
#   --seed             Seed fixture.cypher first (needs cypher-shell on PATH or --neo4j-home)
#   --neo4j-home DIR   Neo4j install dir, so its bin/cypher-shell is used for --seed
#   --help             Show this help

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEMO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

HOSTS="java,python"
URI="bolt://localhost:7687"
USER="neo4j"
PASSWORD="neo4j"
SEED=0
NEO4J_HOME=""

# Driver versions. The plain driver jar does not bundle netty/reactor, so we
# fetch them explicitly.
DRIVER_VER="5.26.0"
NETTY_VER="4.1.115.Final"
REACTOR_VER="3.6.11"
RS_VER="1.0.4"
CACHE="$DEMO_DIR/.cache/lib"

while [ $# -gt 0 ]; do
  case "$1" in
    --hosts) HOSTS="$2"; shift 2 ;;
    --uri) URI="$2"; shift 2 ;;
    --user) USER="$2"; shift 2 ;;
    --password) PASSWORD="$2"; shift 2 ;;
    --seed) SEED=1; shift ;;
    --neo4j-home) NEO4J_HOME="$2"; shift 2 ;;
    --help|-h) sed -n '4,26p' "$0" | sed 's/^# //; s/^#//'; exit 0 ;;
    *) echo "Unknown argument: $1 (try --help)" >&2; exit 1 ;;
  esac
done

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

# ----------------------------------------------------------------------------
# Helpers
# ----------------------------------------------------------------------------

# A JDK 17+ home (the Neo4j driver needs it; the default java may be older).
java_home_17plus() {
  if command -v /usr/libexec/java_home >/dev/null 2>&1; then
    /usr/libexec/java_home -v 17+ 2>/dev/null && return 0
  fi
  # Fall back to JAVA_HOME or PATH java if it is already 17+.
  local jh="${JAVA_HOME:-}"
  if [ -n "$jh" ] && "$jh/bin/java" -version 2>&1 | grep -qE 'version "(1[7-9]|2[0-9])'; then
    echo "$jh"; return 0
  fi
  return 1
}

fetch() {  # fetch URL DEST  (no-op if DEST exists)
  local url="$1" dest="$2"
  [ -f "$dest" ] && return 0
  curl -fsSL -o "$dest" "$url"
}

# Download the Neo4j Java driver and its runtime deps into the cache.
fetch_java_driver() {
  mkdir -p "$CACHE"
  local mc="https://repo1.maven.org/maven2"
  fetch "$mc/org/neo4j/driver/neo4j-java-driver/$DRIVER_VER/neo4j-java-driver-$DRIVER_VER.jar" "$CACHE/neo4j-java-driver.jar"
  fetch "$mc/org/reactivestreams/reactive-streams/$RS_VER/reactive-streams-$RS_VER.jar" "$CACHE/reactive-streams.jar"
  fetch "$mc/io/projectreactor/reactor-core/$REACTOR_VER/reactor-core-$REACTOR_VER.jar" "$CACHE/reactor-core.jar"
  local n
  for n in common buffer resolver transport codec handler transport-native-unix-common; do
    fetch "$mc/io/netty/netty-$n/$NETTY_VER/netty-$n-$NETTY_VER.jar" "$CACHE/netty-$n.jar"
  done
}

# Ensure the generated Hydra Java kernel exists (the published hydra-java jar is
# coder-only and lacks hydra.core / hydra.validation).
ensure_java_kernel() {
  if [ ! -f "$REPO_ROOT/dist/java/hydra-kernel/src/main/java/hydra/validation/ValidationProfile.java" ]; then
    echo "  Generating the Hydra Java kernel (dist/java/hydra-kernel)..."
    "$REPO_ROOT/heads/java/bin/assemble-distribution.sh" hydra-kernel >/dev/null
  fi
}

ensure_python_kernel() {
  if [ ! -f "$REPO_ROOT/dist/python/hydra-kernel/src/main/python/hydra/core.py" ]; then
    echo "  Generating the Hydra Python kernel (dist/python/hydra-kernel)..."
    "$REPO_ROOT/heads/python/bin/assemble-distribution.sh" hydra-kernel >/dev/null
  fi
}

# ----------------------------------------------------------------------------
# Seed (optional)
# ----------------------------------------------------------------------------

if [ "$SEED" = "1" ]; then
  echo "=== Seeding fixture ==="
  CS="cypher-shell"
  [ -n "$NEO4J_HOME" ] && CS="$NEO4J_HOME/bin/cypher-shell"
  if ! command -v "$CS" >/dev/null 2>&1 && [ ! -x "$CS" ]; then
    echo "  cypher-shell not found (set --neo4j-home). Skipping seed."
  else
    "$CS" -a "$URI" -u "$USER" -p "$PASSWORD" --file "$DEMO_DIR/fixture.cypher" \
      && echo "  Seeded $DEMO_DIR/fixture.cypher"
  fi
  echo ""
fi

# ----------------------------------------------------------------------------
# Hosts
# ----------------------------------------------------------------------------

run_java() {
  echo "=== Java ==="
  local jh; jh="$(java_home_17plus || true)"
  if [ -z "$jh" ]; then
    echo "  SKIPPED: no JDK 17+ found (the Neo4j Java driver needs it)."
    return
  fi
  ensure_java_kernel
  fetch_java_driver

  local out; out="$(mktemp -d)"
  local driver_cp; driver_cp="$(ls "$CACHE"/*.jar | tr '\n' ':')"
  # Compile the kernel, only the Neo4j hydra-pg modules (the full hydra-pg pulls
  # in hydra-rdf, which is not generated into Java here), and the demo.
  local sources="$out/sources.txt"
  {
    find "$REPO_ROOT/dist/java/hydra-kernel/src/main/java" -name '*.java'
    find "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/neo4j" -name '*.java'
    find "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/error/neo4j" -name '*.java'
    find "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/validate" -name 'Neo4j.java'
    find "$REPO_ROOT/demos/src/main/java/hydra/demos/neo4jvalidation" -name '*.java'
  } | sort -u > "$sources"

  if ! "$jh/bin/javac" -cp "$driver_cp" -d "$out/classes" @"$sources" 2>"$out/javac.err"; then
    echo "  Java compile failed:"; sed 's/^/    /' "$out/javac.err" | head -20
    return
  fi
  "$jh/bin/java" -cp "$out/classes:$driver_cp" \
    hydra.demos.neo4jvalidation.Neo4jValidationDemo "$URI" "$USER" "$PASSWORD" 2>/dev/null
}

run_python() {
  echo "=== Python ==="
  if ! python3 -c "import neo4j" 2>/dev/null; then
    echo "  SKIPPED: the 'neo4j' Python package is not installed (pip install neo4j)."
    return
  fi
  ensure_python_kernel
  local pp="$REPO_ROOT/dist/python/hydra-pg/src/main/python"
  pp="$pp:$REPO_ROOT/dist/python/hydra-kernel/src/main/python"
  pp="$pp:$REPO_ROOT/overlay/python/hydra-kernel/src/main/python"
  pp="$pp:$REPO_ROOT/demos/src/main/python"
  PYTHONPATH="$pp${PYTHONPATH:+:$PYTHONPATH}" python3 \
    "$REPO_ROOT/demos/src/main/python/hydra/demos/neo4jvalidation/demo.py" "$URI" "$USER" "$PASSWORD"
}

for host in "${ENABLED_HOSTS[@]}"; do
  case "$host" in
    java) run_java ;;
    python) run_python ;;
    *) echo "Unknown host: $host" >&2 ;;
  esac
  echo ""
done

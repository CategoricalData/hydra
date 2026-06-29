#!/usr/bin/env bash
set -euo pipefail
#
# Orchestrator for the Neo4j JSON-artifact validation translingual demo.
#
# Authors a Neo4j schema and a family of graphs (several valid, several invalid) ONCE in
# the Java DSL, encodes them to Hydra's canonical term-JSON, then validates the
# SAME JSON files in each host language (Java, Python, Haskell) using the SAME
# generated validator. Because every host reads identical JSON and runs a
# validator generated from one Hydra source, the data and the logic are identical
# across languages by construction; this script diffs the outputs to prove it.
#
# With --cypher, the user instead provides plain Neo4j Cypher CREATE files (the same
# Cypher they would type into cypher-shell); the demo ingests each into the Hydra Neo4j
# model and validates it against the reference schema — the user never touches Hydra.
#
# Unlike ./run.sh (the live-server variant), this needs no running Neo4j.
#
# Usage: ./run-json.sh [OPTIONS]
#   --hosts LANG,...   Run only specified hosts (default: java,python,haskell,typescript)
#   --cypher FILE      Ingest a Neo4j Cypher CREATE file and validate it (repeatable).
#                      Replaces the bundled sample graphs with the Cypher-derived ones.
#   --data-dir DIR     Reuse an existing data directory instead of generating fresh
#   --keep             Print the data directory and leave it in place
#   --help             Show this help

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DEMO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"

HOSTS="java,python,haskell,typescript"
DATA_DIR=""
KEEP=0
CYPHER_FILES=()

while [ $# -gt 0 ]; do
  case "$1" in
    --hosts) HOSTS="$2"; shift 2 ;;
    --data-dir) DATA_DIR="$2"; shift 2 ;;
    --cypher) CYPHER_FILES+=("$2"); shift 2 ;;
    --keep) KEEP=1; shift ;;
    --help|-h) sed -n '4,23p' "$0" | sed 's/^# //; s/^#//'; exit 0 ;;
    *) echo "Unknown argument: $1 (try --help)" >&2; exit 1 ;;
  esac
done

IFS=',' read -ra ENABLED_HOSTS <<< "$HOSTS"

GREEN='\033[0;32m'; RED='\033[0;31m'; YELLOW='\033[1;33m'; NC='\033[0m'
header() { echo ""; echo "=== $1 ==="; }

# A JDK 17+ home (the Java host kernel targets a modern JDK).
java_home_17plus() {
  if command -v /usr/libexec/java_home >/dev/null 2>&1; then
    /usr/libexec/java_home -v 17+ 2>/dev/null && return 0
  fi
  local jh="${JAVA_HOME:-}"
  if [ -n "$jh" ] && "$jh/bin/java" -version 2>&1 | grep -qE 'version "(1[7-9]|2[0-9])'; then
    echo "$jh"; return 0
  fi
  return 1
}

ensure_java_kernel() {
  if [ ! -f "$REPO_ROOT/dist/java/hydra-kernel/src/main/java/hydra/json/Encode.java" ]; then
    echo "  Generating the Hydra Java kernel (dist/java/hydra-kernel)..."
    "$REPO_ROOT/heads/java/bin/assemble-distribution.sh" hydra-kernel >/dev/null
  fi
  # The demo also needs the generated Neo4j model + validator (hydra-pg).
  if [ ! -f "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/validate/Neo4j.java" ]; then
    echo "  Generating the Hydra Java hydra-pg (dist/java/hydra-pg)..."
    "$REPO_ROOT/heads/java/bin/assemble-distribution.sh" hydra-pg >/dev/null
  fi
}

ensure_python_kernel() {
  if [ ! -f "$REPO_ROOT/dist/python/hydra-kernel/src/main/python/hydra/core.py" ]; then
    echo "  Generating the Hydra Python kernel (dist/python/hydra-kernel)..."
    "$REPO_ROOT/heads/python/bin/assemble-distribution.sh" hydra-kernel >/dev/null
  fi
  if [ ! -f "$REPO_ROOT/dist/python/hydra-pg/src/main/python/hydra/validate/neo4j.py" ]; then
    echo "  Generating the Hydra Python hydra-pg (dist/python/hydra-pg)..."
    "$REPO_ROOT/heads/python/bin/assemble-distribution.sh" hydra-pg >/dev/null
  fi
}

# Generate the TypeScript hydra-pg dist (kernel + rdf + pg) if absent, and install
# the TypeScript head's dev dependencies (tsx) if needed.
ensure_typescript_dist() {
  local pgts="$REPO_ROOT/dist/typescript/hydra-pg/src/main/typescript/hydra/validate/neo4j.ts"
  if [ ! -f "$pgts" ]; then
    echo "  Generating the Hydra TypeScript dist (hydra-kernel, hydra-rdf, hydra-pg)..."
    "$REPO_ROOT/heads/typescript/bin/assemble-distribution.sh" hydra-kernel >/dev/null
    "$REPO_ROOT/heads/typescript/bin/assemble-distribution.sh" hydra-rdf >/dev/null
    "$REPO_ROOT/heads/typescript/bin/assemble-distribution.sh" hydra-pg >/dev/null
  fi
  if [ ! -x "$REPO_ROOT/heads/typescript/node_modules/.bin/tsx" ]; then
    echo "  Installing the TypeScript head's dev dependencies (tsx)..."
    ( cd "$REPO_ROOT/heads/typescript" && npm install >/dev/null 2>&1 )
  fi
}

# ----------------------------------------------------------------------------
# Build the Java demo classes (kernel + Neo4j model/encode/validate + demo).
# ----------------------------------------------------------------------------

JH="$(java_home_17plus || true)"
CLASSES=""

build_java() {
  [ -n "$CLASSES" ] && return 0
  if [ -z "$JH" ]; then
    echo "  no JDK 17+ found; Java host and data generation unavailable" >&2
    return 1
  fi
  ensure_java_kernel
  CLASSES="$(mktemp -d)"
  local srcs; srcs="$(mktemp)"
  {
    find "$REPO_ROOT/dist/java/hydra-kernel/src/main/java" -name '*.java'
    find "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/neo4j/model" -name '*.java'
    find "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/encode/neo4j" -name '*.java'
    find "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/error/neo4j" -name '*.java'
    echo "$REPO_ROOT/dist/java/hydra-pg/src/main/java/hydra/validate/Neo4j.java"
    echo "$REPO_ROOT/heads/java/src/main/java/hydra/Generation.java"
    find "$REPO_ROOT/demos/src/main/java/hydra/demos/neo4jvalidation" -name '*.java' \
      ! -name 'Neo4jValidationDemo.java'
  } | sort -u > "$srcs"
  "$JH/bin/javac" -d "$CLASSES" @"$srcs"
}

# ----------------------------------------------------------------------------
# Generate the shared JSON data.
# ----------------------------------------------------------------------------

if [ -n "$DATA_DIR" ]; then
  header "Using existing data from $DATA_DIR"
elif [ "${#CYPHER_FILES[@]}" -gt 0 ]; then
  # Cypher mode: the user provides plain Neo4j Cypher files; we ingest each into the
  # Hydra Neo4j model and emit its graph JSON, then validate against the reference schema.
  build_java || { echo "Cannot ingest Cypher without Java." >&2; exit 1; }
  DATA_DIR="$(mktemp -d)"
  header "Ingesting Cypher into shared JSON in $DATA_DIR"
  # The schema is the reference movie graph type (authored on our side for now). GenerateData
  # writes schema.json plus sample graphs; keep only the schema, then add the Cypher graphs.
  "$JH/bin/java" -cp "$CLASSES" hydra.demos.neo4jvalidation.GenerateData "$DATA_DIR" >/dev/null
  find "$DATA_DIR" -name '*.json' ! -name 'schema.json' -delete
  # Prefix the ingested files so they never collide with the canonical fixture names; the hosts
  # then auto-discover all non-schema *.json in the directory.
  for cf in "${CYPHER_FILES[@]}"; do
    base="$(basename "$cf")"; base="${base%.cypher}"
    "$JH/bin/java" -cp "$CLASSES" hydra.demos.neo4jvalidation.CypherIngest "$cf" "$DATA_DIR/cypher_$base.json"
  done
else
  build_java || { echo "Cannot generate data without Java." >&2; exit 1; }
  DATA_DIR="$(mktemp -d)"
  header "Generating shared JSON data (Java DSL) into $DATA_DIR"
  "$JH/bin/java" -cp "$CLASSES" hydra.demos.neo4jvalidation.GenerateData "$DATA_DIR"
fi

echo ""
echo "Data files:"
for f in "$DATA_DIR"/*.json; do [ -f "$f" ] && echo "  $(basename "$f")"; done

# ----------------------------------------------------------------------------
# Run hosts.
# ----------------------------------------------------------------------------

RESULTS_DIR="$(mktemp -d)"

run_java() {
  build_java || { echo "  SKIPPED (no JDK 17+)"; return; }
  "$JH/bin/java" -cp "$CLASSES" hydra.demos.neo4jvalidation.Neo4jJsonValidateDemo "$DATA_DIR"
}

run_python() {
  if ! python3 -c "import sys" 2>/dev/null; then echo "  SKIPPED (no python3)"; return; fi
  ensure_python_kernel
  local pp="$REPO_ROOT/dist/python/hydra-pg/src/main/python"
  pp="$pp:$REPO_ROOT/dist/python/hydra-kernel/src/main/python"
  pp="$pp:$REPO_ROOT/overlay/python/hydra-kernel/src/main/python"
  pp="$pp:$REPO_ROOT/demos/src/main/python"
  PYTHONPATH="$pp${PYTHONPATH:+:$PYTHONPATH}" python3 \
    "$REPO_ROOT/demos/src/main/python/hydra/demos/neo4jvalidation/json_demo.py" "$DATA_DIR"
}

run_haskell() {
  local driver="$REPO_ROOT/demos/src/main/haskell/Hydra/Demos/Neo4jValidation/JsonDemo.hs"
  if [ ! -f "$driver" ]; then echo "  SKIPPED (Haskell driver not present)"; return; fi
  cd "$REPO_ROOT/heads/haskell"
  printf ':load %s\n:set args %s\nmain\n:quit\n' "$driver" "$DATA_DIR" \
    | stack ghci hydra:lib --no-load --ghci-options='-v0' 2>/dev/null \
    | grep -vE '^(Ok|Loaded|\*|$)'
}

run_typescript() {
  if ! command -v node >/dev/null 2>&1; then echo "  SKIPPED (no node)"; return; fi
  ensure_typescript_dist
  local tsx="$REPO_ROOT/heads/typescript/node_modules/.bin/tsx"
  local pgroot="$REPO_ROOT/dist/typescript/hydra-pg/src/main/typescript"
  local cfg; cfg="$(mktemp -d)/tsconfig.json"
  cat > "$cfg" <<JSON
{
  "compilerOptions": {
    "module": "NodeNext", "moduleResolution": "NodeNext", "target": "ES2022",
    "strict": false, "skipLibCheck": true, "types": ["node"],
    "baseUrl": "$pgroot", "paths": { "hydra/*": ["./hydra/*"] }
  }
}
JSON
  "$tsx" --tsconfig "$cfg" \
    "$REPO_ROOT/demos/src/main/typescript/hydra/demos/neo4jvalidation/jsonDemo.ts" "$DATA_DIR"
}

for host in "${ENABLED_HOSTS[@]}"; do
  header "$host"
  case "$host" in
    java) run_java | tee "$RESULTS_DIR/$host.txt" ;;
    python) run_python | tee "$RESULTS_DIR/$host.txt" ;;
    haskell) run_haskell | tee "$RESULTS_DIR/$host.txt" ;;
    typescript) run_typescript | tee "$RESULTS_DIR/$host.txt" ;;
    *) echo "Unknown host: $host" >&2 ;;
  esac
done

# ----------------------------------------------------------------------------
# Compare outputs across hosts.
# ----------------------------------------------------------------------------

header "Cross-host comparison"
MATCH=true
NUM=${#ENABLED_HOSTS[@]}
i=0
while [ $i -lt $NUM ]; do
  j=$((i + 1))
  while [ $j -lt $NUM ]; do
    h1="${ENABLED_HOSTS[$i]}"; h2="${ENABLED_HOSTS[$j]}"
    f1="$RESULTS_DIR/$h1.txt"; f2="$RESULTS_DIR/$h2.txt"
    if [ -s "$f1" ] && [ -s "$f2" ]; then
      if diff -q "$f1" "$f2" >/dev/null 2>&1; then
        echo -e "  ${GREEN}$h1 == $h2${NC}"
      else
        echo -e "  ${RED}$h1 != $h2${NC}"; diff "$f1" "$f2" | sed 's/^/    /' || true; MATCH=false
      fi
    fi
    j=$((j + 1))
  done
  i=$((i + 1))
done

echo ""
if [ "$MATCH" = true ]; then
  echo -e "${GREEN}All host outputs match: the same validation logic, run identically in every language.${NC}"
else
  echo -e "${RED}Output mismatch detected.${NC}"
fi

if [ "$KEEP" = "1" ]; then
  echo ""
  echo "Data directory kept at: $DATA_DIR"
fi

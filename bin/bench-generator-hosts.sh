#!/usr/bin/env bash
# #459 benchmark: Haskell vs. Java generator-host performance for the JSON->target
# Layer 1 transform. Measures wall-clock separately for:
#   - setup time  (one-time cost to get the generator ready: stack build for Haskell,
#                  gradle resolve+compile for Java — analogous to "download/build time"
#                  in the issue's original framing)
#   - transform time (per-invocation cost of the actual JSON->target generation, run
#                  N times per (host, package, target) after setup, to get a stable
#                  median unaffected by JVM/GHC warmup)
#
# Usage:
#   bin/bench-generator-hosts.sh [--packages hydra-jvm,hydra-java,hydra-kernel]
#                                 [--target python] [--runs 3] [--tag TAG]
#
# Output: benchmark/generator-host-runs/run_<TS>[_<tag>]/results.json + a printed
# dashboard comparing Haskell vs. Java setup + transform time per package.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

PACKAGES="hydra-jvm,hydra-java,hydra-kernel"
TARGET="python"
RUNS=3
TAG=""

while [ $# -gt 0 ]; do
    case "$1" in
        --packages) PACKAGES="$2"; shift 2 ;;
        --target) TARGET="$2"; shift 2 ;;
        --runs) RUNS="$2"; shift 2 ;;
        --tag) TAG="_$2"; shift 2 ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0 ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

TS="$(date -u +%Y-%m-%dT%H%M%SZ 2>/dev/null || date +%Y-%m-%dT%H%M%SZ)"
RUN_DIR="$HYDRA_ROOT_DIR/benchmark/generator-host-runs/run_${TS}${TAG}"
mkdir -p "$RUN_DIR"
RESULTS_JSON="$RUN_DIR/results.json"

echo "=== #459 generator-host benchmark ==="
echo "  Packages: $PACKAGES"
echo "  Target:   $TARGET"
echo "  Runs per (host,package): $RUNS"
echo "  Run dir:  $RUN_DIR"
echo ""

# Millisecond wall-clock helper (portable across GNU/BSD date).
now_ms() {
    if command -v python3 >/dev/null 2>&1; then
        python3 -c "import time; print(int(time.time()*1000))"
    else
        date +%s%3N
    fi
}

OUT_BASE="$RUN_DIR/output"
mkdir -p "$OUT_BASE"

echo "{" > "$RESULTS_JSON"
echo "  \"target\": \"$TARGET\"," >> "$RESULTS_JSON"
echo "  \"runs\": $RUNS," >> "$RESULTS_JSON"
echo "  \"hosts\": {" >> "$RESULTS_JSON"

FIRST_HOST=1
for HOST in haskell java; do
    if [ "$FIRST_HOST" -eq 0 ]; then echo "    ," >> "$RESULTS_JSON"; fi
    FIRST_HOST=0
    echo "--- Host: $HOST ---"

    # --- Setup time: one-time cost to get the generator ready. ---
    # NOTE: as of this writing hydra.json's hostOverrides pins java/python to
    # "local" (the #417 breaking-wave shim, pending 0.17.2 republish), which
    # blocks target-driver's normal Maven resolution. Rather than measure a
    # forced-failure or an artificial local rebuild that doesn't represent the
    # real published-host cold-start cost, JAVA_CP below uses whatever is
    # already resolved in the Gradle cache (from before the shim took effect)
    # as a stand-in for "the published artifacts are already on disk" — this
    # is the fair, steady-state comparison once 0.17.2 republishes and the
    # shim lifts. Re-run this benchmark post-shim-lift for the true first-ever
    # cold-download number if that matters for the final decision.
    echo "  Setup..."
    SETUP_START=$(now_ms)
    if [ "$HOST" = "haskell" ]; then
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && stack build hydra:exe:bootstrap-from-json >/dev/null 2>&1)
    else
        JAVA_CP=$(find ~/.gradle/caches -iname "hydra-*-0.17.1.jar" 2>/dev/null | grep -v sources | grep -v javadoc | tr '\n' ':')
        JAVA_CP="${JAVA_CP}$HYDRA_ROOT_DIR/heads/java/target-driver/build/classes/java/main"
        if [ -z "$(find "$HYDRA_ROOT_DIR/heads/java/target-driver/build/classes/java/main" -name '*.class' 2>/dev/null)" ]; then
            javac -cp "$JAVA_CP" -d "$HYDRA_ROOT_DIR/heads/java/target-driver/build/classes/java/main" \
                "$HYDRA_ROOT_DIR/heads/java/src/main/java/hydra/TransformJsonToTarget.java" \
                "$HYDRA_ROOT_DIR/heads/java/src/main/java/hydra/Bootstrap.java" \
                "$HYDRA_ROOT_DIR/heads/java/src/main/java/hydra/GenerationTargets.java" \
                "$HYDRA_ROOT_DIR/dist/java/hydra-build/src/main/java/hydra/build/"*.java \
                "$HYDRA_ROOT_DIR/overlay/java/hydra-build/src/main/java/hydra/overlay/java/build/Generation.java" \
                >/dev/null 2>&1
        fi
        echo "$JAVA_CP" > "$RUN_DIR/java-classpath.txt"
    fi
    SETUP_END=$(now_ms)
    SETUP_MS=$((SETUP_END - SETUP_START))
    echo "    Setup time: ${SETUP_MS}ms"

    echo "    \"$HOST\": {" >> "$RESULTS_JSON"
    echo "      \"setupMs\": $SETUP_MS," >> "$RESULTS_JSON"
    echo "      \"packages\": {" >> "$RESULTS_JSON"

    IFS=',' read -ra PKG_ARR <<< "$PACKAGES"
    FIRST_PKG=1
    for PKG in "${PKG_ARR[@]}"; do
        if [ "$FIRST_PKG" -eq 0 ]; then echo "        ," >> "$RESULTS_JSON"; fi
        FIRST_PKG=0
        echo "  Package: $PKG"
        TIMES=()
        for i in $(seq 1 "$RUNS"); do
            RUN_OUT="$OUT_BASE/${HOST}-${PKG}-run${i}"
            mkdir -p "$RUN_OUT"
            T0=$(now_ms)
            if [ "$HOST" = "haskell" ]; then
                "$HYDRA_ROOT_DIR/heads/haskell/bin/transform-json-to-target.sh" "$TARGET" "$PKG" main \
                    --output "$RUN_OUT" --include-dsls >/dev/null 2>&1
            else
                JAVA_CP=$(cat "$RUN_DIR/java-classpath.txt")
                java -Xss64m -Xmx8g -cp "$JAVA_CP" hydra.TransformJsonToTarget \
                    --target "$TARGET" --package "$PKG" \
                    --dist-json-root "$HYDRA_ROOT_DIR/dist/json" --output "$RUN_OUT" \
                    --include-dsls >/dev/null 2>&1
            fi
            T1=$(now_ms)
            ELAPSED=$((T1 - T0))
            TIMES+=("$ELAPSED")
            echo "    Run $i: ${ELAPSED}ms"
        done
        # Median of TIMES (RUNS is small; a simple sort suffices).
        SORTED=($(printf '%s\n' "${TIMES[@]}" | sort -n))
        MID=$(( (RUNS - 1) / 2 ))
        MEDIAN="${SORTED[$MID]}"
        echo "    Median: ${MEDIAN}ms"
        TIMES_JOINED=$(printf '%s,' "${TIMES[@]}")
        TIMES_JOINED="${TIMES_JOINED%,}"
        echo "        \"$PKG\": {\"runsMs\": [$TIMES_JOINED], \"medianMs\": $MEDIAN}" >> "$RESULTS_JSON"
    done
    echo "      }" >> "$RESULTS_JSON"
    echo "    }" >> "$RESULTS_JSON"
done

echo "  }" >> "$RESULTS_JSON"
echo "}" >> "$RESULTS_JSON"

echo ""
echo "Results written to $RESULTS_JSON"
echo ""
python3 "$SCRIPT_DIR/generator-host-bench-dashboard.py" "$RESULTS_JSON"

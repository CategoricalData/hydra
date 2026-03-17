#!/bin/bash
# Run all bootstrapping paths and compare output against baselines.
#
# This script demonstrates 9 bootstrapping paths:
#   3 host languages (Haskell, Java, Python) × 3 target languages (Haskell, Java, Python)
#
# Each path: loads Hydra modules from language-independent JSON, generates code
# for a target language, builds, runs tests, and compares the output against
# the canonical baselines in the repository.
#
# Subcommands:
#   (none)                                 # Full run: generate + build + test (default)
#   generate [options]                     # Generation + build only (no tests)
#   test [options]                         # Re-run tests on existing generated code
#
# Options:
#   --hosts LANG,...        Run only specified host languages
#   --targets LANG,...      Generate only specified target languages
#   --types-only            Only generate type-defining modules
#   --kernel-only           Only generate kernel modules
#   --tag TAG               Append a tag to the run directory name
#   --runs DIR              Override runs directory (default: $HYDRA_ROOT/bootstrap/runs)
#   --output DIR            Override output directory for generated code (default: /tmp/hydra-bootstrapping-demo)
#   --repeat N              Run tests N times (default: 1)
#   --run RUN               Use a specific run directory (test mode)
#   --paths p1,p2,...       Test only specific paths (test mode)
#
# All generated code goes to /tmp/hydra-bootstrapping-demo with subdirectories:
#   {host}-to-{target}/
# Run metadata and results are persisted to bootstrap/runs/.

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../.." && pwd )"
OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"

# Parse subcommand
SUBCOMMAND="all"
case "${1:-}" in
    generate|test|display)
        SUBCOMMAND="$1"
        shift
        ;;
esac

# Parse arguments
HOSTS="haskell,java,python"
TARGETS="haskell,java,python"
EXTRA_FLAGS=""
TAG=""
RUNS_DIR="$HYDRA_ROOT/bootstrap/runs"
REPEAT=1
RUN_SPEC=""
PATH_FILTER=""

while [ $# -gt 0 ]; do
    case "$1" in
        --hosts) HOSTS="$2"; shift ;;
        --hosts=*) HOSTS="${1#--hosts=}" ;;  # legacy
        --targets) TARGETS="$2"; shift ;;
        --targets=*) TARGETS="${1#--targets=}" ;;  # legacy
        --types-only) EXTRA_FLAGS="$EXTRA_FLAGS --types-only" ;;
        --kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS --kernel-only" ;;
        --tag) TAG="$2"; shift ;;
        --tag=*) TAG="${1#--tag=}" ;;
        --runs) RUNS_DIR="$2"; shift ;;
        --runs=*) RUNS_DIR="${1#--runs=}" ;;
        --output) OUTPUT_BASE="$2"; shift ;;
        --output=*) OUTPUT_BASE="${1#--output=}" ;;
        --repeat) REPEAT="$2"; shift ;;
        --repeat=*) REPEAT="${1#--repeat=}" ;;
        --run) RUN_SPEC="$2"; shift ;;
        --run=*) RUN_SPEC="${1#--run=}" ;;
        --paths) PATH_FILTER="$2"; shift ;;
        --paths=*) PATH_FILTER="${1#--paths=}" ;;
    esac
    shift
done

# Expand "all" for hosts and targets
case "$HOSTS" in
    all) HOSTS="haskell,java,python" ;;
esac
case "$TARGETS" in
    all) TARGETS="haskell,java,python" ;;
esac

IFS=',' read -ra HOST_LIST <<< "$HOSTS"
IFS=',' read -ra TARGET_LIST <<< "$TARGETS"

# ============================================================
# Environment checks
# ============================================================

# Determine which tool families are needed based on selected hosts and targets.
NEED_STACK=false
NEED_JAVA=false
NEED_PYTHON=false
for lang in "${HOST_LIST[@]}" "${TARGET_LIST[@]}"; do
    case "$lang" in
        haskell) NEED_STACK=true ;;
        java)    NEED_JAVA=true ;;
        python)  NEED_PYTHON=true ;;
    esac
done

ENV_ERRORS=()
ENV_WARNINGS=()

# --- Stack / GHC (for Haskell host or target) ---
if $NEED_STACK; then
    if ! command -v stack > /dev/null 2>&1; then
        ENV_ERRORS+=("stack is not installed. Install from https://docs.haskellstack.org/")
    else
        STACK_VER=$(stack --numeric-version 2>/dev/null || echo "0")
        STACK_MAJOR=$(echo "$STACK_VER" | cut -d. -f1)
        if [ "$STACK_MAJOR" -lt 2 ]; then
            ENV_ERRORS+=("stack version $STACK_VER is too old; version 2.x or later is required")
        fi
    fi
fi

# --- Java 17+ (for Java host or target) ---
if $NEED_JAVA; then
    # Try to locate a Java 17+ JDK and set JAVA_HOME.
    if [ -n "${JAVA_HOME:-}" ] && "$JAVA_HOME/bin/java" -version 2>&1 | grep -qE '"(17|18|19|2[0-9])\.' ; then
        export JAVA_HOME
    elif command -v /usr/libexec/java_home > /dev/null 2>&1; then
        JAVA_HOME=$(/usr/libexec/java_home -v 17 2>/dev/null || true)
        if [ -n "${JAVA_HOME:-}" ]; then
            export JAVA_HOME
        fi
    fi

    # Validate that java is available and is 11+.
    JAVA_CMD="${JAVA_HOME:+$JAVA_HOME/bin/}java"
    if ! command -v "$JAVA_CMD" > /dev/null 2>&1; then
        ENV_ERRORS+=("java is not installed. JDK 11 or later is required.")
    else
        JAVA_VER=$("$JAVA_CMD" -version 2>&1 | head -1 | sed -E 's/.*"([0-9]+).*/\1/')
        if [ -n "$JAVA_VER" ] && [ "$JAVA_VER" -lt 11 ] 2>/dev/null; then
            ENV_ERRORS+=("Java $JAVA_VER is too old; version 11 or later is required (found: $("$JAVA_CMD" -version 2>&1 | head -1))")
        fi
    fi

    # Warn if running an x86_64 JDK under Rosetta on Apple Silicon (causes ~20x slowdown)
    if [ "$(uname -s)" = "Darwin" ] && [ "$(uname -m)" = "arm64" ]; then
        if command -v "$JAVA_CMD" > /dev/null 2>&1 && file "$(command -v "$JAVA_CMD")" | grep -q x86_64; then
            ENV_WARNINGS+=("x86_64 JDK detected on Apple Silicon. This runs under Rosetta 2 and will be ~20x slower than a native arm64 JDK. Current: $("$JAVA_CMD" -version 2>&1 | head -1)")
        fi
    fi
fi

# --- Python 3.12+ (for Python host or target) ---
if $NEED_PYTHON; then
    # Check for any usable Python 3.12+.
    # Look on PATH first, then in the hydra-python venvs (matching invoke-python-host.sh).
    HYDRA_PYTHON_DIR="$HYDRA_ROOT/hydra-python"
    PYTHON_CMD=""
    for candidate in pypy3 python3 python \
        "$HYDRA_PYTHON_DIR/.venv-pypy/bin/pypy3" \
        "$HYDRA_PYTHON_DIR/.venv/bin/python3"; do
        if command -v "$candidate" > /dev/null 2>&1 || [ -x "$candidate" ]; then
            PY_VER=$("$candidate" -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")' 2>/dev/null || echo "0.0")
            PY_MAJOR=$(echo "$PY_VER" | cut -d. -f1)
            PY_MINOR=$(echo "$PY_VER" | cut -d. -f2)
            if [ "$PY_MAJOR" -ge 3 ] && [ "$PY_MINOR" -ge 12 ] 2>/dev/null; then
                PYTHON_CMD="$candidate"
                break
            fi
        fi
    done

    if [ -z "$PYTHON_CMD" ]; then
        ENV_ERRORS+=("Python 3.12 or later is required but not found. Create a venv: cd hydra-python && python3.12 -m venv .venv")
    fi

    # Check for pytest (needed for Python target testing).
    for t in "${TARGET_LIST[@]}"; do
        if [ "$t" = "python" ]; then
            if [ -n "$PYTHON_CMD" ] && ! "$PYTHON_CMD" -m pytest --version > /dev/null 2>&1; then
                ENV_WARNINGS+=("pytest is not installed; Python target tests will fail. Install with: pip install pytest")
            fi
            break
        fi
    done

    # Check for pypy3 (recommended for Python host).
    for h in "${HOST_LIST[@]}"; do
        if [ "$h" = "python" ]; then
            if ! command -v pypy3 > /dev/null 2>&1 && [ ! -x "$HYDRA_PYTHON_DIR/.venv-pypy/bin/pypy3" ]; then
                ENV_WARNINGS+=("pypy3 is not installed. PyPy3 is strongly recommended for Python host (CPython is very slow for term-level generation).")
            fi
            break
        fi
    done
fi

# Report results.
if [ ${#ENV_ERRORS[@]} -gt 0 ]; then
    echo "Environment check FAILED:"
    for msg in "${ENV_ERRORS[@]}"; do
        echo "  ERROR: $msg"
    done
    if [ ${#ENV_WARNINGS[@]} -gt 0 ]; then
        for msg in "${ENV_WARNINGS[@]}"; do
            echo "  WARNING: $msg"
        done
    fi
    exit 1
fi
if [ ${#ENV_WARNINGS[@]} -gt 0 ]; then
    echo "Environment check:"
    for msg in "${ENV_WARNINGS[@]}"; do
        echo "  WARNING: $msg"
    done
    echo ""
fi

# ============================================================
# Shared utilities
# ============================================================

baseline_dir_for_target() {
    case "$1" in
        haskell) echo "hydra-haskell" ;;
        java)    echo "hydra-java" ;;
        python)  echo "hydra-python" ;;
    esac
}

ext_for_target() {
    case "$1" in
        haskell) echo ".hs" ;;
        java)    echo ".java" ;;
        python)  echo ".py" ;;
    esac
}

format_time() {
    local secs=$1
    if [ "$secs" -lt 60 ]; then
        echo "${secs}s"
    else
        local mins=$((secs / 60))
        local rem=$((secs % 60))
        echo "${mins}m ${rem}s"
    fi
}

# Convert a time string like "3.6s", "1m 36.1s", or "1m 36.1" to total seconds (decimal)
parse_time_to_secs() {
    local raw=$1
    # Strip trailing 's' if present
    raw="${raw%s}"
    if [[ "$raw" =~ ^([0-9]+)m\ (.+)$ ]]; then
        # "Xm Y.Z" format
        local mins="${BASH_REMATCH[1]}"
        local secs="${BASH_REMATCH[2]}"
        python3 -c "print(${mins} * 60 + ${secs})"
    else
        # Plain seconds (e.g. "3.6")
        echo "$raw"
    fi
}

capitalize() {
    local first="${1:0:1}"
    local rest="${1:1}"
    printf '%s%s' "$(echo "$first" | tr '[:lower:]' '[:upper:]')" "$rest"
}

# Create a run directory with timestamp
create_run_dir() {
    local ts
    ts=$(python3 -c 'from datetime import datetime,timezone; t=datetime.now(timezone.utc); print(t.strftime("%Y-%m-%d_%H%M%S") + "_" + f"{t.microsecond//1000:03d}")')
    local dirname="run_${ts}"
    if [ -n "$TAG" ]; then
        dirname="${dirname}_${TAG}"
    fi
    local run_dir="$RUNS_DIR/$dirname"
    mkdir -p "$run_dir"
    echo "$run_dir"
}

# If the output base directory already exists, rename it to <dir>.1, .2, etc.
preserve_existing_output() {
    if [ -d "$OUTPUT_BASE" ]; then
        local n=1
        while [ -e "${OUTPUT_BASE}.${n}" ]; do
            n=$((n + 1))
        done
        mv "$OUTPUT_BASE" "${OUTPUT_BASE}.${n}"
        echo "  Preserved existing output as ${OUTPUT_BASE}.${n}"
    fi
}

# Find the latest (or specified) run directory
find_run_dir() {
    if [ -n "$RUN_SPEC" ]; then
        if [ -d "$RUNS_DIR/$RUN_SPEC" ]; then
            echo "$RUNS_DIR/$RUN_SPEC"; return
        fi
        if [ -d "$RUNS_DIR/run_$RUN_SPEC" ]; then
            echo "$RUNS_DIR/run_$RUN_SPEC"; return
        fi
        local match
        match=$(ls -1d "$RUNS_DIR"/run_* 2>/dev/null | grep "$RUN_SPEC" | tail -1)
        if [ -n "$match" ]; then
            echo "$match"; return
        fi
        echo "Error: No run directory found matching '$RUN_SPEC'" >&2
        return 1
    fi
    local latest
    latest=$(ls -1d "$RUNS_DIR"/run_* 2>/dev/null | sort | tail -1)
    if [ -z "$latest" ]; then
        echo "Error: No run directories found in $RUNS_DIR" >&2
        return 1
    fi
    echo "$latest"
}

json_escape() {
    python3 -c "import json,sys; print(json.dumps(sys.argv[1]))" "$1"
}

# Compare generated output against baseline
compare_output() {
    local host=$1
    local target=$2
    local demo_dir="$OUTPUT_BASE/${host}-to-${target}"
    local ext
    ext=$(ext_for_target "$target")
    local baseline_proj
    baseline_proj=$(baseline_dir_for_target "$target")
    local baseline_dir="$HYDRA_ROOT/$baseline_proj/src/gen-main"

    local total=0 identical=0 different=0 missing_in_baseline=0
    local diff_files=()

    while IFS= read -r f; do
        local rel="${f#$demo_dir/}"
        local lang_dir
        case "$target" in
            haskell) lang_dir="haskell" ;;
            java)    lang_dir="java" ;;
            python)  lang_dir="python" ;;
        esac
        local mod_path="${rel#src/gen-main/${lang_dir}/}"
        local ref="$baseline_dir/${lang_dir}/$mod_path"

        if [ -f "$ref" ]; then
            total=$((total + 1))
            if diff -q "$f" "$ref" > /dev/null 2>&1; then
                identical=$((identical + 1))
            else
                different=$((different + 1))
                diff_files+=("$mod_path")
            fi
        else
            missing_in_baseline=$((missing_in_baseline + 1))
        fi
    done < <(find "$demo_dir" -name "*${ext}" 2>/dev/null | sort)

    echo "  Comparison against baseline ($baseline_proj/src/gen-main):"
    echo "    Matched to baseline:  $total"
    echo "    Identical:            $identical"
    if [ "$different" -gt 0 ]; then
        echo "    Different:            $different"
        for df in "${diff_files[@]}"; do
            echo "      DIFF: $df"
        done
    fi
    if [ "$missing_in_baseline" -gt 0 ]; then
        echo "    No baseline found:    $missing_in_baseline"
    fi

    # Return JSON fragment
    local diff_json="[]"
    if [ "${#diff_files[@]}" -gt 0 ]; then
        diff_json=$(python3 -c "import json,sys; print(json.dumps(sys.argv[1:]))" "${diff_files[@]}")
    fi
    echo "{\"total\":${total},\"identical\":${identical},\"different\":${different},\"diffFiles\":${diff_json}}"
}

# Parse timing and file counts from a bootstrap log file → JSON
parse_bootstrap_log() {
    local logfile=$1

    local done_line
    done_line=$(grep "Done:" "$logfile" 2>/dev/null || true)
    local main_files test_files
    main_files=$(echo "$done_line" | sed -n 's/.*Done: *\([0-9]*\) main.*/\1/p')
    test_files=$(echo "$done_line" | sed -n 's/.*+ *\([0-9]*\) test.*/\1/p')
    main_files="${main_files:-0}"
    test_files="${test_files:-0}"

    # Main gen time: "Generated N files." followed by "Time: Xs" on next line
    local main_time_raw
    main_time_raw=$(grep -A1 "Generated.*files" "$logfile" | grep -v "test" | grep -v "generation" | grep "Time:" | head -1 | sed 's/.*Time: *//')
    local main_time_secs=""
    if [ -n "$main_time_raw" ]; then
        main_time_secs=$(parse_time_to_secs "$main_time_raw")
    fi

    # Test gen time: "Generated N test files." followed by "Time: Xs" on next line
    local test_time_raw
    test_time_raw=$(grep -A1 "Generated.*test files" "$logfile" | grep "Time:" | head -1 | sed 's/.*Time: *//')
    local test_time_secs=""
    if [ -n "$test_time_raw" ]; then
        test_time_secs=$(parse_time_to_secs "$test_time_raw")
    fi

    local main_json="{\"fileCount\":${main_files}"
    if [ -n "$main_time_secs" ] && [ "$main_time_secs" != "" ]; then
        main_json="${main_json},\"timeSeconds\":${main_time_secs}"
    fi
    main_json="${main_json}}"

    local test_json="{\"fileCount\":${test_files}"
    if [ -n "$test_time_secs" ] && [ "$test_time_secs" != "" ]; then
        test_json="${test_json},\"timeSeconds\":${test_time_secs}"
    fi
    test_json="${test_json}}"

    echo "{\"main\":${main_json},\"test\":${test_json}}"
}

# Write metadata.json to a run directory
write_metadata() {
    local run_dir=$1
    local overall_start=$2
    local overall_secs=$3

    local branch commit commit_msg start_time
    branch=$(cd "$HYDRA_ROOT" && git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "unknown")
    commit=$(cd "$HYDRA_ROOT" && git rev-parse --short HEAD 2>/dev/null || echo "unknown")
    commit_msg=$(cd "$HYDRA_ROOT" && git log -1 --format=%s 2>/dev/null || echo "")
    start_time=$(python3 -c "from datetime import datetime,timezone; print(datetime.fromtimestamp($overall_start, timezone.utc).isoformat())")

    cat > "$run_dir/metadata.json" <<ENDJSON
{
  "startTime": "${start_time}",
  "totalTimeSeconds": ${overall_secs},
  "branch": $(json_escape "$branch"),
  "commit": $(json_escape "$commit"),
  "commitMessage": $(json_escape "$commit_msg"),
  "hosts": $(python3 -c "import json; print(json.dumps('${HOSTS}'.split(',')))" ),
  "targets": $(python3 -c "import json; print(json.dumps('${TARGETS}'.split(',')))" ),
  "outputBase": $(json_escape "$OUTPUT_BASE")
}
ENDJSON
}

# Run tests for a single path, writing benchmark JSON to run_dir
run_tests_for_path() {
    local path_key=$1
    local run_dir=$2
    local repeat=$3

    local target="${path_key##*-to-}"
    local demo_dir="$OUTPUT_BASE/${path_key}"

    for ((i=1; i<=repeat; i++)); do
        local bench_file
        if [ "$repeat" -eq 1 ]; then
            bench_file="$run_dir/${path_key}.benchmark.json"
        else
            bench_file="$run_dir/${path_key}_${i}.benchmark.json"
        fi

        if [ "$repeat" -gt 1 ]; then
            echo "    Test run $i/$repeat..."
        fi

        local test_exit=0
        set +e
        case "$target" in
            haskell)
                cd "$demo_dir"
                HYDRA_BENCHMARK_OUTPUT="$bench_file" stack test 2>&1
                test_exit=$?
                ;;
            java)
                cd "$demo_dir"
                HYDRA_BENCHMARK_OUTPUT="$bench_file" ./gradlew test 2>&1
                test_exit=$?
                ;;
            python)
                cd "$demo_dir"
                if [ -d "src/gen-test" ]; then
                    HYDRA_BENCHMARK_OUTPUT="$bench_file" pytest src/test/ src/gen-test/ 2>&1
                else
                    HYDRA_BENCHMARK_OUTPUT="$bench_file" pytest src/test/ 2>&1
                fi
                test_exit=$?
                ;;
        esac
        set -e

        if [ "$test_exit" -ne 0 ]; then
            echo "    Tests FAILED (exit $test_exit)"
            return $test_exit
        fi
    done
    return 0
}

# ============================================================
# Generate subcommand (generate + build, no tests)
# ============================================================

do_generate() {
    local run_tests=$1  # "true" or "false"
    local TOTAL_PATHS=$(( ${#HOST_LIST[@]} * ${#TARGET_LIST[@]} ))
    local OVERALL_START=$(date +%s)
    local RUN_DIR
    RUN_DIR=$(create_run_dir)

    preserve_existing_output

    local mode_label="Generate"
    if [ "$run_tests" = "true" ]; then
        mode_label="Generate + Test"
    fi

    echo "=========================================================================="
    echo "  Hydra Bootstrapping — ${mode_label} — $(date '+%Y-%m-%d %H:%M:%S')"
    echo "=========================================================================="
    echo ""
    echo "  Host languages:   ${HOST_LIST[*]}"
    echo "  Target languages: ${TARGET_LIST[*]}"
    echo "  Total paths:      $TOTAL_PATHS"
    echo "  Extra flags:     ${EXTRA_FLAGS:- (none)}"
    echo "  Output base:      $OUTPUT_BASE"
    echo "  Run directory:    $RUN_DIR"
    if [ "$run_tests" = "true" ] && [ "$REPEAT" -gt 1 ]; then
        echo "  Test repeats:     $REPEAT"
    fi
    echo ""
    echo "=========================================================================="
    echo ""

    local PATH_NUM=0
    local ANY_FAIL=0

    for host in "${HOST_LIST[@]}"; do
        for target in "${TARGET_LIST[@]}"; do
            PATH_NUM=$((PATH_NUM + 1))
            local path_key="${host}-to-${target}"

            echo "----------------------------------------------------------------------"
            echo "  Path ${PATH_NUM}/${TOTAL_PATHS}: ${path_key}"
            echo "----------------------------------------------------------------------"
            echo ""

            local path_start
            path_start=$(date +%s)

            local exit_code=0
            local demo_dir="$OUTPUT_BASE/${path_key}"
            local logfile="$RUN_DIR/${path_key}.log"

            # Set up target directory (clean + copy static files)
            set +e
            "$SCRIPT_DIR/setup-${target}-target.sh" "$demo_dir" 2>&1 | tee "$logfile"
            exit_code=${PIPESTATUS[0]}

            # Generate code
            if [ $exit_code -eq 0 ]; then
                "$SCRIPT_DIR/invoke-${host}-host.sh" --target "$target" --output "$OUTPUT_BASE" --include-tests --kernel-only $EXTRA_FLAGS 2>&1 | tee -a "$logfile"
                exit_code=${PIPESTATUS[0]}
            fi

            # Build and test
            if [ $exit_code -eq 0 ] && [ "$run_tests" = "true" ]; then
                local bench_file="$RUN_DIR/${path_key}.benchmark.json"
                HYDRA_BENCHMARK_OUTPUT="$bench_file" "$SCRIPT_DIR/test-${target}-target.sh" "$demo_dir" 2>&1 | tee -a "$logfile"
                exit_code=${PIPESTATUS[0]}
            fi
            set -e

            local path_end
            path_end=$(date +%s)
            local path_secs=$((path_end - path_start))

            echo ""
            if [ "$exit_code" -ne 0 ]; then
                echo "  Status: FAILED (exit code $exit_code)"
                ANY_FAIL=1

                cat > "$RUN_DIR/${path_key}.json" <<ENDJSON
{"status":"fail","pathTimeSeconds":${path_secs}}
ENDJSON
            else
                echo "  Status: completed"

                local gen_json
                gen_json=$(parse_bootstrap_log "$logfile")

                # Capture compare_output: print to stdout AND capture the last line (JSON)
                local comp_output
                comp_output=$(compare_output "$host" "$target")
                # Print all lines (the human-readable part is already printed by compare_output)
                local comp_json
                comp_json=$(echo "$comp_output" | tail -1)

                local status="pass"
                local diff_count
                diff_count=$(echo "$comp_json" | python3 -c "import json,sys; print(json.load(sys.stdin).get('different',0))" 2>/dev/null || echo "0")
                if [ "$diff_count" -gt 0 ]; then
                    status="diff"
                fi

                # Extract test run time from the log
                local test_run_time
                test_run_time=$(grep "Test time:" "$logfile" 2>/dev/null | tail -1 | sed 's/.*Test time: *//' | sed 's/s$//')

                # Build per-path JSON
                local test_results_json=""
                if [ -n "$test_run_time" ] && [ "$test_run_time" != "" ]; then
                    test_results_json=",\"testRunTimeSeconds\":${test_run_time}"
                fi

                cat > "$RUN_DIR/${path_key}.json" <<ENDJSON
{
  "status": "${status}",
  "generation": ${gen_json},
  "comparison": ${comp_json},
  "pathTimeSeconds": ${path_secs}${test_results_json}
}
ENDJSON

                # For repeat > 1, run additional test iterations
                if [ "$run_tests" = "true" ] && [ "$REPEAT" -gt 1 ]; then
                    # First run already happened via setup script (wrote .benchmark.json)
                    # Rename it to _1.benchmark.json and run remaining repeats
                    local bench_file="$RUN_DIR/${path_key}.benchmark.json"
                    if [ -f "$bench_file" ]; then
                        mv "$bench_file" "$RUN_DIR/${path_key}_1.benchmark.json"
                    fi
                    for ((ri=2; ri<=REPEAT; ri++)); do
                        echo "    Additional test run $ri/$REPEAT..."
                        local extra_bench="$RUN_DIR/${path_key}_${ri}.benchmark.json"
                        local test_exit=0
                        set +e
                        case "$target" in
                            haskell)
                                cd "$demo_dir"
                                HYDRA_BENCHMARK_OUTPUT="$extra_bench" stack test 2>&1
                                test_exit=$?
                                ;;
                            java)
                                cd "$demo_dir"
                                HYDRA_BENCHMARK_OUTPUT="$extra_bench" ./gradlew test 2>&1
                                test_exit=$?
                                ;;
                            python)
                                cd "$demo_dir"
                                if [ -d "src/gen-test" ]; then
                                    HYDRA_BENCHMARK_OUTPUT="$extra_bench" pytest src/test/ src/gen-test/ 2>&1
                                else
                                    HYDRA_BENCHMARK_OUTPUT="$extra_bench" pytest src/test/ 2>&1
                                fi
                                test_exit=$?
                                ;;
                        esac
                        set -e
                        if [ "$test_exit" -ne 0 ]; then
                            echo "    Tests FAILED on repeat $ri"
                            break
                        fi
                    done
                fi
            fi

            echo "  Path time: $(format_time $path_secs)"
            echo ""
        done
    done

    local OVERALL_END=$(date +%s)
    local OVERALL_SECS=$((OVERALL_END - OVERALL_START))

    write_metadata "$RUN_DIR" "$OVERALL_START" "$OVERALL_SECS"

    # Display summary matrix
    display_matrix "$RUN_DIR"

    echo "  Total time:      $(format_time $OVERALL_SECS)"
    echo "  Run directory:   $RUN_DIR"
    echo "  Dashboard:       bin/run-bootstrapping-demo.sh dashboard"
    echo ""

    if [ "$ANY_FAIL" -ne 0 ]; then
        return 1
    fi
}

# ============================================================
# Test subcommand (re-run tests on existing generated code)
# ============================================================

do_test() {
    local RUN_DIR
    RUN_DIR=$(find_run_dir) || exit 1

    local meta_file="$RUN_DIR/metadata.json"
    if [ ! -f "$meta_file" ]; then
        echo "Error: No metadata.json found in $RUN_DIR" >&2
        exit 1
    fi

    local output_base
    output_base=$(python3 -c "import json; print(json.load(open('$meta_file')).get('outputBase', '/tmp/hydra-bootstrapping-demo'))")

    # Determine paths to test
    local paths_to_test=()
    if [ -n "$PATH_FILTER" ]; then
        IFS=',' read -ra paths_to_test <<< "$PATH_FILTER"
    else
        for f in "$RUN_DIR"/*.json; do
            local base
            base=$(basename "$f" .json)
            if [ "$base" = "metadata" ]; then continue; fi
            if [[ "$base" == *.benchmark ]]; then continue; fi
            paths_to_test+=("$base")
        done
    fi

    echo "=========================================================================="
    echo "  Hydra Bootstrapping — Test Phase — $(date '+%Y-%m-%d %H:%M:%S')"
    echo "=========================================================================="
    echo ""
    echo "  Run directory:  $RUN_DIR"
    echo "  Output base:    $output_base"
    echo "  Paths:          ${paths_to_test[*]}"
    echo "  Repeat:         $REPEAT"
    echo ""
    echo "=========================================================================="
    echo ""

    OUTPUT_BASE="$output_base"

    for path_key in "${paths_to_test[@]}"; do
        local path_json="$RUN_DIR/${path_key}.json"
        if [ -f "$path_json" ]; then
            local status
            status=$(python3 -c "import json; print(json.load(open('$path_json')).get('status','?'))")
            if [ "$status" = "fail" ]; then
                echo "  Skipping $path_key (generation failed)"
                echo ""
                continue
            fi
        fi

        local target="${path_key##*-to-}"
        local demo_dir="$output_base/${path_key}"

        if [ ! -d "$demo_dir" ]; then
            echo "  Skipping $path_key (directory not found: $demo_dir)"
            echo ""
            continue
        fi

        echo "  Testing $path_key ($REPEAT repeat(s))..."
        run_tests_for_path "$path_key" "$RUN_DIR" "$REPEAT" || true
        echo ""
    done

    display_matrix "$RUN_DIR"

    echo "  Run directory: $RUN_DIR"
    echo "  Dashboard:     bin/run-bootstrapping-demo.sh dashboard"
    echo ""
}

# ============================================================
# Display matrix from run directory
# ============================================================

display_matrix() {
    local run_dir=$1

    python3 - "$run_dir" <<'PYEOF'
import json, sys, os, glob, statistics

run_dir = sys.argv[1]

meta_path = os.path.join(run_dir, "metadata.json")
meta = json.load(open(meta_path)) if os.path.exists(meta_path) else {}

hosts = meta.get("hosts", ["haskell", "java", "python"])
targets = meta.get("targets", ["haskell", "java", "python"])

def capitalize(s):
    return s[0].upper() + s[1:]

def fmt_time(secs):
    if secs is None: return "?"
    return f"{secs:.1f}s" if secs < 60 else f"{int(secs)//60}m {secs%60:.0f}s"

def load_benchmark_timing(path_key):
    """Load test run time from benchmark JSON files, or fall back to testRunTimeSeconds."""
    single = os.path.join(run_dir, f"{path_key}.benchmark.json")
    if os.path.exists(single):
        try:
            d = json.load(open(single))
            tms = (d.get("summary") or {}).get("totalTimeMs")
            return fmt_time(tms / 1000.0) if tms else "?"
        except: return "?"

    files = sorted(glob.glob(os.path.join(run_dir, f"{path_key}_*.benchmark.json")))
    if files:
        times = []
        for fp in files:
            try:
                d = json.load(open(fp))
                tms = (d.get("summary") or {}).get("totalTimeMs")
                if tms is not None: times.append(tms)
            except: pass
        if times:
            med = statistics.median(times)
            result = fmt_time(med / 1000.0)
            if len(times) > 1:
                sd = statistics.stdev(times)
                result += f" +/-{fmt_time(sd / 1000.0)}"
            return result

    # Fall back to testRunTimeSeconds from per-path JSON
    pj = os.path.join(run_dir, f"{path_key}.json")
    if os.path.exists(pj):
        try:
            d = json.load(open(pj))
            trs = d.get("testRunTimeSeconds")
            if trs is not None:
                return fmt_time(trs)
        except: pass

    return "\u2014"

data = {}
for h in hosts:
    for t in targets:
        key = f"{h}-to-{t}"
        path = os.path.join(run_dir, f"{key}.json")
        if os.path.exists(path):
            with open(path) as f:
                data[key] = json.load(f)

header = ["Host \\ Target"] + [capitalize(t) for t in targets]
rows = [header]
for h in hosts:
    r1 = [capitalize(h)]
    r2 = [""]
    r3 = [""]
    for t in targets:
        key = f"{h}-to-{t}"
        d = data.get(key)
        if d is None:
            r1.append("(not run)")
            r2.append("")
            r3.append("")
        elif d.get("status") == "fail":
            r1.append("FAILED")
            r2.append("")
            r3.append("")
        else:
            gen = d.get("generation", {})
            m = gen.get("main", {})
            te = gen.get("test", {})
            mf = m.get("fileCount", "?")
            mt = fmt_time(m.get("timeSeconds"))
            tf = te.get("fileCount", "?")
            tt = fmt_time(te.get("timeSeconds"))
            r1.append(f"{mf} files ({tf} test)")
            r2.append(f"gen: {mt} ({tt})")
            r3.append(f"test: {load_benchmark_timing(key)}")
    rows.append(r1)
    rows.append(r2)
    rows.append(r3)

num_cols = len(header)
widths = [max(len(row[c]) if c < len(row) else 0 for row in rows) + 2 for c in range(num_cols)]

def rule(left, mid, right):
    parts = [f"  {left}"]
    for c in range(num_cols):
        if c > 0: parts.append(mid)
        parts.append("\u2500" * widths[c])
    parts.append(right)
    print("".join(parts))

def draw_row(row):
    parts = ["  \u2502"]
    for c in range(num_cols):
        val = row[c] if c < len(row) else ""
        parts.append(f" {val:<{widths[c]-1}}\u2502")
    print("".join(parts))

print()
print("  Each cell: main files (test files) / gen: main time (test time) / test: run time")
print()

rule("\u250c", "\u252c", "\u2510")
draw_row(rows[0])
rule("\u251c", "\u253c", "\u2524")
for i in range(1, len(rows)):
    if i > 1 and (i - 1) % 3 == 0:
        rule("\u251c", "\u253c", "\u2524")
    draw_row(rows[i])
rule("\u2514", "\u2534", "\u2518")
print()
PYEOF
}

# ============================================================
# Main dispatch
# ============================================================

case "$SUBCOMMAND" in
    generate)
        do_generate "false"
        ;;
    test)
        do_test
        ;;
    all)
        do_generate "true"
        ;;
    display)
        RUN_DIR=$(find_run_dir) || exit 1
        echo "Displaying results from: $RUN_DIR"
        display_matrix "$RUN_DIR"
        ;;
esac

#!/bin/bash
# Run all bootstrapping paths and compare output against baselines.
#
# This script demonstrates 9 bootstrapping paths:
#   3 host languages (Haskell, Java, Python) × 3 target languages (Haskell, Java, Python)
#
# Each path: loads Hydra modules from language-independent JSON, generates code
# for a target language, and compares the output against the canonical baselines
# in the repository.
#
# Usage:
#   ./bootstrap-all.sh                    # Run all 9 paths
#   ./bootstrap-all.sh --hosts java,python  # Run only Java and Python host paths
#   ./bootstrap-all.sh --targets haskell    # Generate only Haskell target
#   ./bootstrap-all.sh --types-only         # Only generate type-defining modules
#   ./bootstrap-all.sh --kernel-only        # Only generate kernel modules
#
# All output goes to /tmp/hydra-bootstrapping-demo with subdirectories:
#   {host}-to-{target}/

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
OUTPUT_BASE="/tmp/hydra-bootstrapping-demo"

# Parse arguments
HOSTS="haskell,java,python"
TARGETS="haskell,java,python"
EXTRA_FLAGS=""
for arg in "$@"; do
    case "$arg" in
        --hosts=*) HOSTS="${arg#--hosts=}" ;;
        --targets=*) TARGETS="${arg#--targets=}" ;;
        --types-only) EXTRA_FLAGS="$EXTRA_FLAGS --types-only" ;;
        --kernel-only) EXTRA_FLAGS="$EXTRA_FLAGS --kernel-only" ;;
    esac
done

IFS=',' read -ra HOST_LIST <<< "$HOSTS"
IFS=',' read -ra TARGET_LIST <<< "$TARGETS"

TOTAL_PATHS=$(( ${#HOST_LIST[@]} * ${#TARGET_LIST[@]} ))
OVERALL_START=$(date +%s)

echo "=========================================================================="
echo "  Hydra Bootstrapping Demo — $(date '+%Y-%m-%d %H:%M:%S')"
echo "=========================================================================="
echo ""
echo "  Host languages:   ${HOST_LIST[*]}"
echo "  Target languages: ${TARGET_LIST[*]}"
echo "  Total paths:      $TOTAL_PATHS"
echo "  Extra flags:     ${EXTRA_FLAGS:- (none)}"
echo "  Output base:      $OUTPUT_BASE"
echo ""
echo "=========================================================================="
echo ""

# Baseline directories
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
        # Strip src/gen-main/{lang}/ prefix to get the module path
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

    local file_count
    file_count=$(find "$demo_dir" -name "*${ext}" 2>/dev/null | wc -l | tr -d ' ')

    echo "  Comparison against baseline ($baseline_proj/src/gen-main):"
    echo "    Output files:         $file_count"
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

    # Return result for summary
    if [ "$file_count" -eq 0 ]; then
        echo "NONE" > "/tmp/bootstrap_result_${host}_${target}"
    elif [ "$different" -eq 0 ] && [ "$file_count" -gt 0 ]; then
        echo "PASS:${identical}/${total}" > "/tmp/bootstrap_result_${host}_${target}"
    else
        echo "DIFF:${identical}/${total}(${different}diff)" > "/tmp/bootstrap_result_${host}_${target}"
    fi
}

# Run a single bootstrapping path
run_path() {
    local host=$1
    local target=$2
    local path_num=$3

    echo "----------------------------------------------------------------------"
    echo "  Path ${path_num}/${TOTAL_PATHS}: ${host}-to-${target}"
    echo "----------------------------------------------------------------------"
    echo ""

    local path_start
    path_start=$(date +%s)

    local exit_code=0
    case "$host" in
        haskell)
            "$SCRIPT_DIR/bootstrap-to-${target}.sh" $EXTRA_FLAGS 2>&1 || exit_code=$?
            ;;
        java)
            "$SCRIPT_DIR/java-bootstrap.sh" --target "$target" $EXTRA_FLAGS 2>&1 || exit_code=$?
            ;;
        python)
            "$SCRIPT_DIR/python-bootstrap.sh" --target "$target" $EXTRA_FLAGS 2>&1 || exit_code=$?
            ;;
    esac

    local path_end
    path_end=$(date +%s)
    local path_secs=$((path_end - path_start))

    echo ""
    if [ "$exit_code" -ne 0 ]; then
        echo "  Status: FAILED (exit code $exit_code)"
        echo "FAIL" > "/tmp/bootstrap_result_${host}_${target}"
    else
        echo "  Status: completed"
        compare_output "$host" "$target"
    fi
    echo "  Path time: $(format_time $path_secs)"
    echo "$path_secs" > "/tmp/bootstrap_time_${host}_${target}"
    echo ""
}

# Run all paths
PATH_NUM=0
for host in "${HOST_LIST[@]}"; do
    for target in "${TARGET_LIST[@]}"; do
        PATH_NUM=$((PATH_NUM + 1))
        run_path "$host" "$target" "$PATH_NUM"
    done
done

OVERALL_END=$(date +%s)
OVERALL_SECS=$((OVERALL_END - OVERALL_START))

# Summary table
echo "=========================================================================="
echo "  Summary"
echo "=========================================================================="
echo ""

# Header
printf "  %-12s" ""
for target in "${TARGET_LIST[@]}"; do
    printf "%-20s" "→ $target"
done
echo ""
printf "  %-12s" ""
for target in "${TARGET_LIST[@]}"; do
    printf "%-20s" "-------------------"
done
echo ""

# Rows
ANY_FAIL=0
for host in "${HOST_LIST[@]}"; do
    printf "  %-12s" "$host"
    for target in "${TARGET_LIST[@]}"; do
        result_file="/tmp/bootstrap_result_${host}_${target}"
        time_file="/tmp/bootstrap_time_${host}_${target}"
        if [ -f "$result_file" ]; then
            result=$(cat "$result_file")
            t=""
            if [ -f "$time_file" ]; then
                t=" ($(format_time $(cat "$time_file")))"
            fi
            printf "%-20s" "${result}${t}"
            case "$result" in
                FAIL*) ANY_FAIL=1 ;;
            esac
        else
            printf "%-20s" "(not run)"
        fi
    done
    echo ""
done

echo ""
echo "  Total time: $(format_time $OVERALL_SECS)"
echo "  Output:     $OUTPUT_BASE"
echo ""
echo "=========================================================================="

# Clean up temp files
for host in "${HOST_LIST[@]}"; do
    for target in "${TARGET_LIST[@]}"; do
        rm -f "/tmp/bootstrap_result_${host}_${target}"
        rm -f "/tmp/bootstrap_time_${host}_${target}"
    done
done

if [ "$ANY_FAIL" -ne 0 ]; then
    exit 1
fi

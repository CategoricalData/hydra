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

# Extract timing and file count data from a bootstrap log file.
# Writes to /tmp/bootstrap_{main,test}_{time,files}_{host}_{target}
parse_bootstrap_log() {
    local host=$1
    local target=$2
    local logfile=$3

    # Extract main generation time (line after "Generated N main ... files.")
    local main_time
    main_time=$(grep -A1 "Generated.*main.*files" "$logfile" | grep "Time:" | head -1 | sed 's/.*Time: *//')
    echo "${main_time:-?}" > "/tmp/bootstrap_main_time_${host}_${target}"

    # Extract test generation time
    local test_time
    test_time=$(grep -A1 "Generated.*test.*files" "$logfile" | grep "Time:" | head -1 | sed 's/.*Time: *//')
    echo "${test_time:-?}" > "/tmp/bootstrap_test_time_${host}_${target}"

    # Extract main file count
    local main_files
    main_files=$(grep "Generated.*main.*files" "$logfile" | head -1 | grep -o '[0-9]*' | head -1)
    echo "${main_files:-0}" > "/tmp/bootstrap_main_files_${host}_${target}"

    # Extract test file count
    local test_files
    test_files=$(grep "Generated.*test.*files" "$logfile" | head -1 | grep -o '[0-9]*' | head -1)
    echo "${test_files:-0}" > "/tmp/bootstrap_test_files_${host}_${target}"
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
    local demo_dir="$OUTPUT_BASE/${host}-to-${target}"
    local logfile="/tmp/bootstrap_log_${host}_${target}.txt"
    set +e
    case "$host" in
        haskell)
            "$SCRIPT_DIR/haskell-to-${target}.sh" $EXTRA_FLAGS 2>&1 | tee "$logfile"
            exit_code=${PIPESTATUS[0]}
            ;;
        java)
            "$SCRIPT_DIR/java-bootstrap.sh" --target "$target" --output "$OUTPUT_BASE" --include-tests --kernel-only $EXTRA_FLAGS 2>&1 | tee "$logfile"
            exit_code=${PIPESTATUS[0]}
            if [ $exit_code -eq 0 ]; then
                "$SCRIPT_DIR/setup-${target}-target.sh" "$demo_dir" 2>&1 || exit_code=$?
            fi
            ;;
        python)
            "$SCRIPT_DIR/python-bootstrap.sh" --target "$target" --output "$OUTPUT_BASE" --include-tests --kernel-only $EXTRA_FLAGS 2>&1 | tee "$logfile"
            exit_code=${PIPESTATUS[0]}
            if [ $exit_code -eq 0 ]; then
                "$SCRIPT_DIR/setup-${target}-target.sh" "$demo_dir" 2>&1 || exit_code=$?
            fi
            ;;
    esac
    set -e

    local path_end
    path_end=$(date +%s)
    local path_secs=$((path_end - path_start))

    echo ""
    if [ "$exit_code" -ne 0 ]; then
        echo "  Status: FAILED (exit code $exit_code)"
        echo "FAIL" > "/tmp/bootstrap_result_${host}_${target}"
    else
        echo "  Status: completed"
        parse_bootstrap_log "$host" "$target" "$logfile"
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

# Summary matrix with box-drawing characters
#
# Each cell shows:
#   Line 1: main gen time (test gen time)
#   Line 2: main files (test files)

capitalize() {
    echo "$1" | sed 's/^./\U&/'
}

# Pre-compute all cell contents into arrays.
# Row 0 is the header; rows 1..N are hosts (two lines each: timing, files).
NUM_COLS=$(( ${#TARGET_LIST[@]} + 1 ))  # +1 for the header column

# Header row
CELLS_0_0="Host \\ Target"
for ci in "${!TARGET_LIST[@]}"; do
    col=$((ci + 1))
    eval "CELLS_0_${col}=\"$(capitalize "${TARGET_LIST[$ci]}")\""
done

# Data rows (two lines per host)
ANY_FAIL=0
ri=0
for host in "${HOST_LIST[@]}"; do
    r1=$((ri * 2 + 1))
    r2=$((ri * 2 + 2))

    eval "CELLS_${r1}_0=\"$(capitalize "$host")\""
    eval "CELLS_${r2}_0=\"\""

    for ci in "${!TARGET_LIST[@]}"; do
        target="${TARGET_LIST[$ci]}"
        col=$((ci + 1))

        result_file="/tmp/bootstrap_result_${host}_${target}"
        if [ -f "$result_file" ]; then
            result=$(cat "$result_file")
            case "$result" in
                FAIL*)
                    eval "CELLS_${r1}_${col}=\"FAILED\""
                    eval "CELLS_${r2}_${col}=\"\""
                    ANY_FAIL=1
                    ;;
                *)
                    mt=$(cat "/tmp/bootstrap_main_time_${host}_${target}" 2>/dev/null || echo "?")
                    tt=$(cat "/tmp/bootstrap_test_time_${host}_${target}" 2>/dev/null || echo "?")
                    mf=$(cat "/tmp/bootstrap_main_files_${host}_${target}" 2>/dev/null || echo "?")
                    tf=$(cat "/tmp/bootstrap_test_files_${host}_${target}" 2>/dev/null || echo "?")
                    eval "CELLS_${r1}_${col}=\"$mt ($tt)\""
                    eval "CELLS_${r2}_${col}=\"${mf} files (${tf} test)\""
                    ;;
            esac
        else
            eval "CELLS_${r1}_${col}=\"(not run)\""
            eval "CELLS_${r2}_${col}=\"\""
        fi
    done
    ri=$((ri + 1))
done

TOTAL_ROWS=$((${#HOST_LIST[@]} * 2 + 1))  # header + 2 lines per host

# Compute column widths: max content length + 2 for padding
COL_WIDTHS=()
for c in $(seq 0 $((NUM_COLS - 1))); do
    max_len=0
    for r in $(seq 0 $((TOTAL_ROWS - 1))); do
        val=$(eval "echo \"\${CELLS_${r}_${c}}\"")
        len=${#val}
        if [ "$len" -gt "$max_len" ]; then
            max_len=$len
        fi
    done
    COL_WIDTHS+=($((max_len + 2)))
done

# Draw a horizontal rule with given junction characters
draw_rule() {
    local left=$1 mid=$2 right=$3
    printf "  %s" "$left"
    for c in $(seq 0 $((NUM_COLS - 1))); do
        if [ "$c" -gt 0 ]; then
            printf "%s" "$mid"
        fi
        printf "─%.0s" $(seq 1 "${COL_WIDTHS[$c]}")
    done
    printf "%s\n" "$right"
}

# Draw a content row
draw_row() {
    local r=$1
    printf "  │"
    for c in $(seq 0 $((NUM_COLS - 1))); do
        val=$(eval "echo \"\${CELLS_${r}_${c}}\"")
        w=${COL_WIDTHS[$c]}
        printf " %-$((w - 1))s│" "$val"
    done
    printf "\n"
}

echo ""
echo "  Each cell: main gen time (test gen time) / main files (test files)"
echo ""

# Top border
draw_rule "┌" "┬" "┐"

# Header row
draw_row 0

# Header separator
draw_rule "├" "┼" "┤"

# Data rows
for hi in $(seq 0 $((${#HOST_LIST[@]} - 1))); do
    if [ "$hi" -gt 0 ]; then
        draw_rule "├" "┼" "┤"
    fi
    draw_row $((hi * 2 + 1))
    draw_row $((hi * 2 + 2))
done

# Bottom border
draw_rule "└" "┴" "┘"

echo ""
echo "  Total demo time: $(format_time $OVERALL_SECS)"
echo "  Output:          $OUTPUT_BASE"
echo ""

# Clean up temp files
for host in "${HOST_LIST[@]}"; do
    for target in "${TARGET_LIST[@]}"; do
        rm -f "/tmp/bootstrap_result_${host}_${target}"
        rm -f "/tmp/bootstrap_time_${host}_${target}"
        rm -f "/tmp/bootstrap_main_time_${host}_${target}"
        rm -f "/tmp/bootstrap_test_time_${host}_${target}"
        rm -f "/tmp/bootstrap_main_files_${host}_${target}"
        rm -f "/tmp/bootstrap_test_files_${host}_${target}"
        rm -f "/tmp/bootstrap_log_${host}_${target}.txt"
    done
done

if [ "$ANY_FAIL" -ne 0 ]; then
    exit 1
fi

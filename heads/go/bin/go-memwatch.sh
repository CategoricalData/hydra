#!/usr/bin/env bash
# Memory watchdog for Go compilation (issue #289).
#
# WHY THIS EXISTS, AND WHAT IT DOES NOT DO
#
# On 2026-07-20 a single arm64-native `compile` process reached 44.24 GB
# resident on a 64 GB host and caused a hardware watchdog reset
# (wdog,reset_in1). GOMEMLIMIT was set to 6GiB at the time and did not help:
# GOMEMLIMIT is a *soft* target for the Go garbage collector and does not bound
# the compiler's allocation arena.
#
# The obvious fix -- a hard rlimit -- IS NOT AVAILABLE ON macOS. Darwin does not
# implement RLIMIT_AS, RLIMIT_DATA, or RLIMIT_RSS; `ulimit -v/-d/-m` all fail
# with "Invalid argument" in bash, sh and zsh alike, and `launchctl limit` shows
# data/rss as unlimited with no settable ceiling. Any guard that calls `ulimit -v`
# on this platform silently does nothing while appearing to protect the machine.
#
# So this is an EXTERNAL SAMPLING WATCHDOG, and it is strictly weaker than an
# rlimit would be:
#   - It is REACTIVE, not preventive. It kills a process that has ALREADY
#     allocated past the threshold; it cannot make an allocation fail.
#   - It samples. Between two samples a process can grow without bound, so a
#     sufficiently fast allocation can still exhaust memory before we react.
#   - It therefore REDUCES BLAST RADIUS. It does not make Go builds safe.
#
# Treat a kill from this watchdog as a FINDING to investigate (a probable Go
# compiler pathology on large generated files), not as a transient failure to
# retry.
#
# Usage:  go_memwatch_run <command...>
# Env:
#   HYDRA_GO_MEMWATCH_LIMIT_GB   per-process RSS kill threshold (default 8)
#   HYDRA_GO_MEMWATCH_TOTAL_GB   aggregate toolchain RSS kill threshold (default 16)
#   HYDRA_GO_MEMWATCH_INTERVAL   sample interval in seconds (default 1)
#   HYDRA_GO_MEMWATCH_DISABLE    set to 1 to run unwatched (NOT recommended)

go_memwatch_run() {
    if [ $# -eq 0 ]; then
        echo "go-memwatch: FATAL: no command given." >&2
        return 1
    fi

    if [ "${HYDRA_GO_MEMWATCH_DISABLE:-0}" = "1" ]; then
        echo "go-memwatch: WARNING: watchdog disabled by HYDRA_GO_MEMWATCH_DISABLE=1." >&2
        echo "go-memwatch: an unbounded compile can hard-reset this machine. Proceeding anyway." >&2
        "$@"
        return $?
    fi

    local limit_gb="${HYDRA_GO_MEMWATCH_LIMIT_GB:-8}"
    local total_gb="${HYDRA_GO_MEMWATCH_TOTAL_GB:-16}"
    local interval="${HYDRA_GO_MEMWATCH_INTERVAL:-1}"

    # ps reports RSS in KiB.
    local limit_kb=$(( limit_gb * 1024 * 1024 ))
    local total_kb=$(( total_gb * 1024 * 1024 ))

    # Run the build in its own process group so we can signal the whole
    # go -> compile/link coalition. Killing only the child lets `go` respawn
    # the same work and blow up again.
    set -m
    "$@" &
    local build_pid=$!
    set +m

    local report_dir="${TMPDIR:-/tmp}"
    local report="$report_dir/go-memwatch-$build_pid.report"
    rm -f "$report"

    # Status goes to stderr, not stdout: callers routinely pipe a build through
    # `| tail -n`, which silently swallowed the arming line and made capped runs
    # look unwatched. Safety evidence must not be truncatable by a pipeline.
    echo "go-memwatch: watching pid $build_pid [per-proc ${limit_gb}GB, total ${total_gb}GB, every ${interval}s]" >&2

    (
        local peak_kb=0
        local peak_cmd="-"
        while kill -0 "$build_pid" 2>/dev/null; do
            # Collect the build's whole descendant tree. Go toolchain children
            # (compile, link, asm) are what actually blow up, not `go` itself.
            local snapshot
            snapshot="$(_go_memwatch_tree_rss "$build_pid")"
            [ -z "$snapshot" ] && { sleep "$interval"; continue; }

            local sum_kb=0
            local worst_kb=0
            local worst_pid=""
            local worst_cmd=""

            while read -r p rss cmd; do
                [ -z "$p" ] && continue
                sum_kb=$(( sum_kb + rss ))
                if [ "$rss" -gt "$worst_kb" ]; then
                    worst_kb="$rss"; worst_pid="$p"; worst_cmd="$cmd"
                fi
            done <<< "$snapshot"

            if [ "$worst_kb" -gt "$peak_kb" ]; then
                peak_kb="$worst_kb"; peak_cmd="$worst_cmd"
                # Persist the running peak every time it rises. The watcher is
                # killed as soon as the build exits, so a report written only
                # at loop end would be lost on the (normal) success path -- and
                # the peak is exactly the number we want on a green build, to
                # see how close it came to the threshold.
                { echo "OK"; echo "peak_process_gb=$(_go_memwatch_gb "$peak_kb")"; echo "peak_process_cmd=$peak_cmd"; } > "$report"
            fi

            local why=""
            if [ "$worst_kb" -ge "$limit_kb" ]; then
                why="process $worst_pid ($worst_cmd) at $(_go_memwatch_gb "$worst_kb") GB >= ${limit_gb} GB"
            elif [ "$sum_kb" -ge "$total_kb" ]; then
                why="toolchain total at $(_go_memwatch_gb "$sum_kb") GB >= ${total_gb} GB"
            fi

            if [ -n "$why" ]; then
                {
                    echo "KILLED"
                    echo "reason=$why"
                    echo "peak_process_gb=$(_go_memwatch_gb "$peak_kb")"
                    echo "peak_process_cmd=$peak_cmd"
                } > "$report"

                echo "" >&2
                echo "go-memwatch: *** KILLING BUILD ***" >&2
                echo "go-memwatch: $why" >&2
                echo "go-memwatch: this is a FINDING, not a flake -- do not simply retry." >&2
                echo "go-memwatch: offending tree:" >&2
                echo "$snapshot" | awk '{printf "go-memwatch:   pid %s  %.2f GB  %s\n",$1,$2/1048576,$3}' >&2

                # Whole process group first, then stragglers, then SIGKILL.
                kill -TERM -"$build_pid" 2>/dev/null || kill -TERM "$build_pid" 2>/dev/null
                echo "$snapshot" | while read -r p _ _; do
                    [ -n "$p" ] && kill -TERM "$p" 2>/dev/null
                done
                sleep 2
                kill -KILL -"$build_pid" 2>/dev/null || kill -KILL "$build_pid" 2>/dev/null
                echo "$snapshot" | while read -r p _ _; do
                    [ -n "$p" ] && kill -KILL "$p" 2>/dev/null
                done
                exit 0
            fi

            sleep "$interval"
        done

        { echo "OK"; echo "peak_process_gb=$(_go_memwatch_gb "$peak_kb")"; echo "peak_process_cmd=$peak_cmd"; } > "$report"
    ) &
    local watch_pid=$!

    local rc=0
    wait "$build_pid" 2>/dev/null || rc=$?
    kill "$watch_pid" 2>/dev/null
    wait "$watch_pid" 2>/dev/null || true

    if [ -f "$report" ]; then
        local peak
        peak="$(grep '^peak_process_gb=' "$report" 2>/dev/null | cut -d= -f2)"
        local peakcmd
        peakcmd="$(grep '^peak_process_cmd=' "$report" 2>/dev/null | cut -d= -f2)"
        if grep -q '^KILLED' "$report" 2>/dev/null; then
            echo "go-memwatch: build was killed by the watchdog (peak ${peak} GB in ${peakcmd})." >&2
            rm -f "$report"
            return 137
        fi
        # A build can finish inside the first sample interval, leaving no peak
        # recorded. Say so plainly rather than printing "  GB ()", which reads
        # like a measurement failure.
        if [ -n "$peak" ] && [ "$peak" != "0.00" ]; then
            echo "go-memwatch: peak toolchain process ${peak} GB (${peakcmd})." >&2
        else
            echo "go-memwatch: build finished within one sample; no peak recorded (cache hit or trivial compile)." >&2
        fi
        rm -f "$report"
    fi

    return $rc
}

# Echo "<pid> <rss_kb> <comm>" for pid and all its descendants.
_go_memwatch_tree_rss() {
    local root="$1"
    local all
    all="$(ps -eo pid=,ppid=,rss=,comm= 2>/dev/null)" || return 0

    local frontier="$root"
    local collected=""
    while [ -n "$frontier" ]; do
        local next=""
        local p
        for p in $frontier; do
            local line
            line="$(echo "$all" | awk -v t="$p" '$1==t {c=$4; for(i=5;i<=NF;i++) c=c" "$i; print $1, $3, c}')"
            [ -n "$line" ] && collected="${collected}${line}"$'\n'
            local kids
            kids="$(echo "$all" | awk -v t="$p" '$2==t {print $1}' | tr '\n' ' ')"
            next="$next $kids"
        done
        frontier="$(echo "$next" | tr -s ' ' | sed 's/^ //;s/ $//')"
    done

    echo "$collected" | sed '/^$/d'
}

_go_memwatch_gb() {
    awk -v k="$1" 'BEGIN { printf "%.2f", k/1048576 }'
}

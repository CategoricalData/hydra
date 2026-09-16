#!/usr/bin/env bash
# Safety guard for ALL Go compilation in Hydra (issue #289).
#
# History: on 2026-03-20 an x86_64 Go toolchain running under Rosetta 2 on an
# Apple Silicon host caused three hardware watchdog resets (wdog,reset_in1) via
# extreme disk I/O and CPU/thermal load. Go must never be compiled through
# Rosetta translation, and Go builds must not starve a machine that is also
# running heavy Haskell builds.
#
# On 2026-07-20 an arm64-NATIVE toolchain caused a second watchdog reset by a
# different mechanism: a single `compile` reached 44.24 GB resident on a 64 GB
# host. GOMEMLIMIT was set to 6GiB and did not prevent it -- GOMEMLIMIT is a
# soft GC target, not a bound on the compiler's arena. Rosetta was NOT involved
# the second time; the arch guard below had already done its job.
#
# A hard rlimit would be the right fix but DOES NOT EXIST on macOS: Darwin
# rejects RLIMIT_AS/DATA/RSS, so `ulimit -v/-d/-m` are no-ops here. The cap is
# therefore an external sampling watchdog (go-memwatch.sh), which reduces blast
# radius but cannot make allocation fail. See that file's header.
#
# Source this file and call go_guard_assert before any go build/test/vet, then
# run the build through go_guard_go (or go_memwatch_run). It (1) refuses a
# non-native Go toolchain, (2) exports conservative parallelism/memory caps,
# and (3) installs a `go` wrapper that REFUSES to run a build/test/vet outside
# the watchdog, so a future script cannot silently bypass the cap.
# Override the caps only knowingly:
#   HYDRA_GO_BUILD_P   parallel package compiles (default 2)
#   HYDRA_GO_MEMLIMIT  GOMEMLIMIT for toolchain processes (default 6GiB)
#   HYDRA_GO_GOGC      GOGC for toolchain processes (default 50)

go_guard_assert() {
    if ! command -v go >/dev/null 2>&1; then
        echo "go-guard: FATAL: no Go toolchain on PATH." >&2
        return 1
    fi

    # A Rosetta-translated shell reports x86_64 from `uname -m`, so ask the
    # hardware directly: hw.optional.arm64 is 1 on Apple Silicon regardless of
    # the calling process's translation state.
    local on_apple_silicon
    on_apple_silicon="$(sysctl -n hw.optional.arm64 2>/dev/null || echo 0)"

    local go_host_arch
    go_host_arch="$(go env GOHOSTARCH)"

    if [ "$on_apple_silicon" = "1" ] && [ "$go_host_arch" != "arm64" ]; then
        echo "go-guard: FATAL: Go toolchain is $go_host_arch on an Apple Silicon host." >&2
        echo "go-guard: Compiling through Rosetta caused hardware watchdog resets (2026-03-20)." >&2
        echo "go-guard: Install/point PATH at an arm64-native Go (e.g. /usr/local/go/bin/go) and retry." >&2
        echo "go-guard: Refusing to build. (which go = $(command -v go))" >&2
        return 1
    fi

    export GOFLAGS="-p=${HYDRA_GO_BUILD_P:-2}${GOFLAGS:+ $GOFLAGS}"
    export GOMEMLIMIT="${HYDRA_GO_MEMLIMIT:-6GiB}"
    export GOGC="${HYDRA_GO_GOGC:-50}"

    # The memory watchdog is the only real cap available on this platform;
    # without it GOMEMLIMIT alone is what we had on 2026-07-20, which failed.
    local guard_dir
    guard_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    if [ ! -f "$guard_dir/go-memwatch.sh" ]; then
        echo "go-guard: FATAL: go-memwatch.sh not found next to go-guard.sh." >&2
        echo "go-guard: refusing to build without the memory watchdog." >&2
        return 1
    fi
    # shellcheck source=./go-memwatch.sh
    source "$guard_dir/go-memwatch.sh"

    HYDRA_GO_GUARD_ACTIVE=1
    export HYDRA_GO_GUARD_ACTIVE

    echo "go-guard: $(go version) [GOHOSTARCH=$go_host_arch, GOFLAGS=$GOFLAGS, GOMEMLIMIT=$GOMEMLIMIT, GOGC=$GOGC]"
    echo "go-guard: memory watchdog armed (GOMEMLIMIT is soft; the watchdog is the actual cap)."
}

# Run a go command under the watchdog. Use this instead of a bare `go ...` for
# any subcommand that compiles: build, test, vet, run, install.
go_guard_go() {
    if [ "${HYDRA_GO_GUARD_ACTIVE:-0}" != "1" ]; then
        echo "go-guard: FATAL: go_guard_go called before go_guard_assert." >&2
        return 1
    fi
    go_memwatch_run command go "$@"
}

# Shadow `go` itself, so a bare `go build` in this shell (or in any script that
# sources this file) cannot escape the watchdog. Non-compiling subcommands
# (env, version, list, mod, fmt) pass straight through -- they don't allocate
# arenas and wrapping them would just add noise.
go() {
    case "${1:-}" in
        build|test|vet|run|install)
            if [ "${HYDRA_GO_GUARD_ACTIVE:-0}" != "1" ]; then
                echo "go-guard: FATAL: 'go $1' attempted without the guard active." >&2
                echo "go-guard: call go_guard_assert first. An uncapped compile hard-reset this machine on 2026-07-20." >&2
                return 1
            fi
            go_memwatch_run command go "$@"
            ;;
        *)
            command go "$@"
            ;;
    esac
}

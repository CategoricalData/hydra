#!/usr/bin/env bash
# Safety guard for ALL Go compilation in Hydra (issue #289).
#
# History: on 2026-03-20 an x86_64 Go toolchain running under Rosetta 2 on an
# Apple Silicon host caused three hardware watchdog resets (wdog,reset_in1) via
# extreme disk I/O and CPU/thermal load. Go must never be compiled through
# Rosetta translation, and Go builds must not starve a machine that is also
# running heavy Haskell builds.
#
# Source this file and call go_guard_assert before any go build/test/vet.
# It (1) refuses a non-native Go toolchain, and (2) exports conservative
# parallelism/memory caps for the compiler. Override the caps only knowingly:
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

    echo "go-guard: $(go version) [GOHOSTARCH=$go_host_arch, GOFLAGS=$GOFLAGS, GOMEMLIMIT=$GOMEMLIMIT, GOGC=$GOGC]"
}

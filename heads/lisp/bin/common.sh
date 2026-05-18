#!/usr/bin/env bash
# Shared Layer 2 assembler logic for the four Lisp dialects.
#
# Each dialect's heads/lisp/<dialect>/bin/assemble-distribution.sh is a
# thin wrapper that declares dialect identity, sources this file, and
# calls lisp_assemble_main "$@". The dialect-specific bits are passed
# in as variables before the call:
#
#   LISP_DIALECT      e.g. "clojure", "common-lisp", "emacs-lisp", "scheme"
#   LISP_PRETTY_NAME  e.g. "Clojure", "Common Lisp", "Emacs Lisp", "Scheme"
#   LISP_HEAD_DIR     absolute path to heads/lisp/<dialect>/
#   LISP_TEST_ENV     basename of the hand-written test_env file
#                     (e.g. "testEnv.clj"); empty string to skip the copy.
#
# Scheme additionally needs to copy runtime libs and write empty
# define-library stubs after the main + test generation; that's done
# in the Scheme wrapper after lisp_assemble_main returns, by calling
# scheme_post_kernel_extras "$OUT_DIR".
#
# This file is sourced via:
#   source "$(dirname "${BASH_SOURCE[0]}")/../../bin/common.sh"
# from each dialect wrapper.

# Source the shared per-source-set digest helpers and the project-level
# common helpers (banner, die, sed_inplace, etc.).
_LISP_COMMON_SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$_LISP_COMMON_SCRIPT_DIR/../../.." && pwd )"
source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Run Steps 1 (main) + 2 (test) + 3 (testEnv copy for hydra-kernel).
# Caller must have set LISP_DIALECT, LISP_PRETTY_NAME, LISP_HEAD_DIR,
# LISP_TEST_ENV before calling, and must pass through "$@" so the
# package + --dist-root args are seen.
#
# Sets PACKAGE, OUT_DIR, DIST_ROOT in the caller's scope (via standard
# bash function semantics — variables are global by default) so that
# any caller-supplied post-step hooks (e.g. Scheme's stub-write) can
# reuse them without re-parsing args.
lisp_assemble_main() {
    if [ $# -lt 1 ]; then
        echo "Usage: $0 <package> [--dist-root <dir>]" >&2
        exit 1
    fi

    # PACKAGE, DIST_ROOT, OUT_DIR are intentionally NOT local: callers
    # (e.g. the Scheme wrapper) read them after lisp_assemble_main
    # returns to drive post-kernel hooks. Don't add `local` here.
    PACKAGE="$1"
    shift

    DIST_ROOT="$HYDRA_ROOT_DIR/dist/$LISP_DIALECT"

    while [ $# -gt 0 ]; do
        case "$1" in
            --dist-root) DIST_ROOT="$2"; shift 2 ;;
            *) shift ;;
        esac
    done

    OUT_DIR="$DIST_ROOT/$PACKAGE"
    local out_main="$OUT_DIR/src/main/$LISP_DIALECT"
    local out_test="$OUT_DIR/src/test/$LISP_DIALECT"
    local dist_json_root="$HYDRA_ROOT_DIR/dist/json"
    local input_digest_main="$dist_json_root/$PACKAGE/src/main/digest.json"
    local input_digest_test="$dist_json_root/$PACKAGE/src/test/digest.json"
    local output_digest_main="$OUT_DIR/src/main/digest.json"
    local output_digest_test="$OUT_DIR/src/test/digest.json"
    local test_json_dir="$dist_json_root/$PACKAGE/src/test/json"

    echo "=== Assembling $LISP_PRETTY_NAME distribution: $PACKAGE ==="
    echo "  Output: $OUT_DIR"
    echo ""

    local haskell_bin="$HYDRA_ROOT_DIR/heads/haskell/bin"

    # #357: optional --keep-paths-from forwarded from the dialect wrapper.
    # The Scheme wrapper builds a manifest (via scheme_keep_paths) listing
    # the runtime libs + stubs that scheme_post_kernel_extras drops in
    # after Step 3; without --keep-paths-from, bootstrap-from-json's prune
    # would delete them.
    local keep_paths_flag=""
    if [ -n "${LISP_KEEP_MANIFEST:-}" ] && [ -f "${LISP_KEEP_MANIFEST}" ]; then
        keep_paths_flag="--keep-paths-from ${LISP_KEEP_MANIFEST}"
    fi

    # Step 1: Main modules.
    if assemble_check_fresh "$input_digest_main" "$out_main" "$output_digest_main"; then
        echo "Step 1: Main modules unchanged; skipping main regeneration."
    else
        rm -f "$output_digest_main"
        echo "Step 1: Generating main $LISP_PRETTY_NAME modules..."
        "$haskell_bin/transform-json-to-lisp.sh" "$PACKAGE" "$LISP_DIALECT" main \
            --output "$DIST_ROOT" \
            --prune-stale $keep_paths_flag
        assemble_refresh_digest "$input_digest_main" "$out_main" "$output_digest_main"
    fi

    # Step 2: Test modules. Any package can have a test source set (just
    # `dist/json/<pkg>/src/test/json/`); the per-source-set digest mechanism
    # is uniform — adding a test dir for any package automatically wires
    # it into the build.
    echo ""
    if [ ! -d "$test_json_dir" ]; then
        echo "Step 2: No test sources for $PACKAGE; skipping."
    else
        if assemble_check_fresh "$input_digest_test" "$out_test" "$output_digest_test"; then
            echo "Step 2: Test modules unchanged; skipping test regeneration."
        else
            rm -f "$output_digest_test"
            echo "Step 2: Generating test $LISP_PRETTY_NAME modules..."
            "$haskell_bin/transform-json-to-lisp.sh" "$PACKAGE" "$LISP_DIALECT" test \
                --output "$DIST_ROOT" \
                --prune-stale $keep_paths_flag
            assemble_refresh_digest "$input_digest_test" "$out_test" "$output_digest_test"
        fi
    fi

    # Step 3: For hydra-kernel, copy the hand-written test_env file into
    # the dist tree so the generated test_graph.<ext>'s import resolves.
    # The kernel filters hydra.test.testEnv from emitted output via
    # testSkipEmitNamespaces. Common Lisp doesn't need the copy here —
    # its run-tests.lisp loads test_env.lisp from heads/ directly before
    # test_graph.lisp.
    if [ "$PACKAGE" = "hydra-kernel" ] && [ -n "$LISP_TEST_ENV" ]; then
        local test_env_src="$LISP_HEAD_DIR/src/test/$LISP_DIALECT/hydra/test/$LISP_TEST_ENV"
        local test_env_dst="$OUT_DIR/src/test/$LISP_DIALECT/hydra/test/$LISP_TEST_ENV"
        if [ -f "$test_env_src" ]; then
            echo ""
            echo "Step 3: Copying $LISP_TEST_ENV from heads/lisp/$LISP_DIALECT..."
            mkdir -p "$(dirname "$test_env_dst")"
            cp "$test_env_src" "$test_env_dst"
        fi
    fi
}

# #357: enumerate the paths scheme_post_kernel_extras WILL drop, into a manifest
# for bootstrap-from-json --keep-paths-from. Called by the Scheme wrapper
# BEFORE lisp_assemble_main so the manifest exists when bootstrap-from-json
# prunes. Args: <manifest-file> <PACKAGE> [--dist-root <dir>]
#
# The paths emitted here MUST match what scheme_post_kernel_extras actually
# copies/writes — keep the two in sync.
scheme_keep_paths() {
    local manifest_file="$1"
    local pkg="$2"
    shift 2
    [ "$pkg" = "hydra-kernel" ] || return 0
    local dist_root="$HYDRA_ROOT_DIR/dist/scheme"
    while [ $# -gt 0 ]; do
        case "$1" in
            --dist-root) dist_root="$2"; shift 2 ;;
            *) shift ;;
        esac
    done
    local out_dir="$dist_root/$pkg"
    local scheme_main_dir="$out_dir/src/main/scheme"
    local scheme_lib_src="$LISP_HEAD_DIR/src/main/scheme/hydra/lib"
    # Runtime libs: every *.scm under heads/lisp/scheme/src/main/scheme/hydra/lib/
    # gets copied to <out>/src/main/scheme/hydra/lib/<name>.scm.
    if [ -d "$scheme_lib_src" ]; then
        for lib_file in "$scheme_lib_src"/*.scm; do
            [ -e "$lib_file" ] || continue
            printf "%s\thydra/lib/%s\n" "$scheme_main_dir" "$(basename "$lib_file")" >> "$manifest_file"
        done
    fi
    # Stub modules: must match the list in scheme_post_kernel_extras Step 3b.
    for stub in decode/graph decode/compute encode/graph encode/compute; do
        printf "%s\thydra/%s.scm\n" "$scheme_main_dir" "$stub" >> "$manifest_file"
    done
}

# Scheme-specific extras for hydra-kernel: copy runtime libs and write
# empty define-library stubs. Called by the Scheme wrapper after
# lisp_assemble_main returns (only when PACKAGE = hydra-kernel). Paired
# with scheme_keep_paths above, which emits a #357 keep-paths manifest
# enumerating the same files BEFORE generation.
#
# Historical note: maps.scm and sets.scm were previously skipped here
# because two implementations existed — an alist-backed portable version
# checked into dist/scheme/, and a vhash-backed Guile-specific version
# in heads/. The dist version was casualty of the dist/scheme/ untrack
# (commit 0a00d9166, "Stop tracking generated dist/ targets"), and CI
# only targets Guile, so we now copy the heads/ vhash version
# unconditionally. If portable Scheme support is needed later, restore
# the alist version from git history (0a00d9166^) and reintroduce the
# skip.
scheme_post_kernel_extras() {
    local out_dir="$1"
    local scheme_lib_src="$LISP_HEAD_DIR/src/main/scheme/hydra/lib"
    local scheme_lib_dst="$out_dir/src/main/scheme/hydra/lib"
    echo ""
    echo "Step 3a: Copying Scheme runtime libraries..."
    mkdir -p "$scheme_lib_dst"
    for lib_file in "$scheme_lib_src"/*.scm; do
        [ -e "$lib_file" ] || continue
        cp "$lib_file" "$scheme_lib_dst/$(basename "$lib_file")"
    done

    # Empty R7RS libraries as placeholders for modules that haven't been
    # generated.
    local scheme_main="$out_dir/src/main/scheme"
    echo "Step 3b: Writing Scheme stub modules..."
    local stub mod_name path
    for stub in decode/graph decode/compute encode/graph encode/compute; do
        path="$scheme_main/hydra/$stub.scm"
        mod_name=$(echo "hydra $stub" | tr '/' ' ')
        mkdir -p "$(dirname "$path")"
        cat > "$path" <<EOF
(define-library ($mod_name)
(import (scheme base))
(export)
(begin))
EOF
    done
}

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
#
# Step 3 (kernel-only) copies the hand-written runtime + test bridge from
# overlay/<dialect>/hydra-kernel/ into dist via lisp_copy_overlay (#434) — the
# single reader of overlay/. The head's test runner then loads the runtime from
# dist/, never from heads/ or overlay/.
#
# Scheme additionally writes empty define-library stub modules after the main +
# test generation; that's done in the Scheme wrapper after lisp_assemble_main
# returns, by calling scheme_post_kernel_extras "$OUT_DIR".
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

# Run Steps 1 (main) + 2 (test) + 3 (overlay copy for hydra-kernel).
# Caller must have set LISP_DIALECT, LISP_PRETTY_NAME, LISP_HEAD_DIR before
# calling, and must pass through "$@" so the package + --dist-root args are seen.
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
    local input_digest_main="$dist_json_root/$PACKAGE/build/main/digest.json"
    local input_digest_test="$dist_json_root/$PACKAGE/build/test/digest.json"
    local output_digest_main="$OUT_DIR/build/main/digest.json"
    local output_digest_test="$OUT_DIR/build/test/digest.json"
    local test_json_dir="$dist_json_root/$PACKAGE/src/test/json"

    echo "=== Assembling $LISP_PRETTY_NAME distribution: $PACKAGE ==="
    echo "  Output: $OUT_DIR"
    echo ""

    local haskell_bin="$HYDRA_ROOT_DIR/heads/haskell/bin"

    # Per-target generator stamp; see bin/lib/assemble-common.sh and #347.
    # All four Lisp dialects share dist/json/hydra-lisp/ (the dialect is a
    # runtime parameter, not a separate coder package), so the stamp scope
    # is "lisp" regardless of LISP_DIALECT.
    export HYDRA_GENERATOR_STAMP=$(compute_generator_stamp lisp)

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

    # Step 3: For hydra-kernel, copy the hand-written runtime + test bridge
    # from the overlay tree into the dist tree (#434). The canonical home is
    # overlay/<dialect>/hydra-kernel/src/{main,test}/<dialect>/ — this copy is
    # the ONLY reader of overlay/; the head's test runner then loads the runtime
    # from dist/, never from heads/ or overlay/. The generated test_graph.<ext>
    # imports hydra.test.testEnv (filtered from emitted output via
    # testSkipEmitModuleNames), which the copied test bridge satisfies.
    if [ "$PACKAGE" = "hydra-kernel" ]; then
        lisp_copy_overlay "$OUT_DIR"
    fi
}

# Copy the hand-written runtime + test bridge for $LISP_DIALECT from the overlay
# tree into the dist package $1 (#434). Mirrors the Java/Python/TypeScript
# copy-kernel-runtime.sh: a dumb full-tree merge of overlay/<dialect>/hydra-kernel/
# onto dist/<dialect>/hydra-kernel/, leaving generated siblings untouched.
# Scheme additionally writes R7RS stubs via scheme_post_kernel_extras (those are
# generated placeholders, not overlay material).
lisp_copy_overlay() {
    local out_dir="$1"
    local overlay_root="$HYDRA_ROOT_DIR/overlay/$LISP_DIALECT/hydra-kernel/src"
    local overlay_main="$overlay_root/main/$LISP_DIALECT"
    local overlay_test="$overlay_root/test/$LISP_DIALECT"

    if [ ! -d "$overlay_main" ] && [ ! -d "$overlay_test" ]; then
        echo "error: missing overlay tree $overlay_root (main or test)" >&2
        exit 1
    fi

    if [ -d "$overlay_main" ]; then
        echo ""
        echo "Step 3: Copying hand-written $LISP_PRETTY_NAME runtime from overlay/$LISP_DIALECT into dist..."
        mkdir -p "$out_dir/src/main/$LISP_DIALECT"
        cp -R "$overlay_main/." "$out_dir/src/main/$LISP_DIALECT/"
    fi
    if [ -d "$overlay_test" ]; then
        echo "Step 3: Copying hand-written $LISP_PRETTY_NAME test runtime from overlay/$LISP_DIALECT into dist..."
        mkdir -p "$out_dir/src/test/$LISP_DIALECT"
        cp -R "$overlay_test/." "$out_dir/src/test/$LISP_DIALECT/"
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
    # #434: the runtime libs now come from overlay/scheme/ via lisp_copy_overlay
    # (Step 3, post-prune), so they no longer need keep-paths protection. Only the
    # generated stub modules (written by scheme_post_kernel_extras, also post-prune)
    # are enumerated here for completeness / parity with the prune pass.
    # Stub modules: must match the list in scheme_post_kernel_extras.
    for stub in decode/graph decode/compute encode/graph encode/compute; do
        printf "%s\thydra/%s.scm\n" "$scheme_main_dir" "$stub" >> "$manifest_file"
    done
}

# Scheme-specific extras for hydra-kernel: write empty define-library stubs for
# modules the coder doesn't emit. Called by the Scheme wrapper after
# lisp_assemble_main returns (only when PACKAGE = hydra-kernel). Post-#434 the
# runtime-lib copy is gone — those libs are part of overlay/scheme/ and land in
# dist via lisp_copy_overlay (Step 3). The Guile-specific vhash maps/sets
# implementations are likewise just overlay files now (the portable alist
# versions remain available in git history at 0a00d9166^ if needed).
scheme_post_kernel_extras() {
    local out_dir="$1"
    # #434: the Scheme runtime libraries (hydra/lib/*, hydra/scheme/lib/*, plus
    # the bytevector/srfi externals) now live in overlay/scheme/ and are copied
    # into dist by lisp_copy_overlay (Step 3). This hook is left with only the
    # generated R7RS stub modules below — empty define-library placeholders for
    # modules the coder doesn't emit. (The stubs are generated, not overlay
    # material, so they stay here.)
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

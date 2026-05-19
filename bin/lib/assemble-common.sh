#!/usr/bin/env bash
# Shared per-source-set digest helpers for Layer 2 assemblers.
#
# Each assembler script sources this file and uses the helpers below to
# implement per-source-set freshness checks. The digest layout is uniform
# across every target language:
#
#   dist/json/<pkg>/src/<set>/digest.json     — input digest (v1)
#   dist/<lang>/<pkg>/src/<set>/digest.json   — output digest (v2)
#
# where <set> ∈ {main, test}.
#
# Usage:
#   source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"
#   assemble_check_fresh <input_digest> <output_dir> <output_digest>
#   assemble_refresh_digest <input_digest> <output_dir> <output_digest>
#   compute_generator_stamp <target_lang>
#   batch_emit_packages
#
# Digest helpers shell out to `digest-check` (Haskell exec). Caller
# must have $HYDRA_ROOT_DIR pointing at the worktree root.

# Compute a per-target generator-stamp hash, suitable for export as
# HYDRA_GENERATOR_STAMP before any assemble_check_fresh/refresh_digest
# call. The stamp is written into the per-target output digest by
# 'digest-check refresh' and re-read by 'digest-check fresh'; a mismatch
# triggers a cache miss even when the per-package input digest itself
# is byte-identical.
#
# Inputs hashed (per target lang L):
#   * dist/json/hydra-L/src/main/digest.json — the coder's own input
#     digest, which after Fix #1 transitively covers L's coder DSL
#     source files (packages/hydra-L/src/main/haskell/Hydra/Sources/L/**)
#     and L's emitted DSL wrappers.
#   * heads/L/bin/assemble-distribution.sh — the assembler script itself.
#   * For L == "haskell": additionally heads/haskell/src/main/haskell/**.hs,
#     since Haskell-emit pulls in the hand-written orchestrators
#     (Hydra.Generation, Hydra.Haskell.Generation, Hydra.PackageRouting)
#     that aren't reachable through dist/json/hydra-haskell/. Same scope
#     as bin/lib/check-phase1-fresh.py and sync-haskell.sh's BFJ cache,
#     so the three caches stay consistent.
#
# Output: a 16-char hex prefix of the SHA-256, short enough to live in a
# JSON string without bloating the digest file. The full hash is overkill
# for collision purposes given the small input space.
#
# See #347 for the broader transform-fingerprinting story.
compute_generator_stamp() {
    local lang="$1"
    local coder_digest="$HYDRA_ROOT_DIR/dist/json/hydra-$lang/src/main/digest.json"
    {
        [ -f "$coder_digest" ] && cat "$coder_digest"
        # Hash the per-target assembler script(s). Most heads use a single
        # heads/<lang>/bin/assemble-distribution.sh; Lisp dialects share
        # logic in heads/lisp/bin/common.sh and have per-dialect
        # assemble-distribution.sh wrappers.
        if [ "$lang" = "lisp" ]; then
            [ -f "$HYDRA_ROOT_DIR/heads/lisp/bin/common.sh" ] && cat "$HYDRA_ROOT_DIR/heads/lisp/bin/common.sh"
            find "$HYDRA_ROOT_DIR/heads/lisp" -name 'assemble-distribution.sh' -type f 2>/dev/null \
                | LC_ALL=C sort | xargs cat 2>/dev/null
        else
            local assembler="$HYDRA_ROOT_DIR/heads/$lang/bin/assemble-distribution.sh"
            [ -f "$assembler" ] && cat "$assembler"
            if [ "$lang" = "haskell" ]; then
                find "$HYDRA_ROOT_DIR/heads/haskell/src/main/haskell" -type f -name '*.hs' 2>/dev/null \
                    | LC_ALL=C sort | xargs cat 2>/dev/null
            fi
        fi
    } | shasum -a 256 | awk '{print substr($1,1,16)}'
}

assemble_check_fresh() {
    local input_digest="$1" output_dir="$2" output_digest="$3"
    [ -f "$input_digest" ] && [ -f "$output_digest" ] && \
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- fresh \
            --inputs "$input_digest" \
            --output-dir "$output_dir" \
            --output-digest "$output_digest" 2>/dev/null)
}

assemble_refresh_digest() {
    local input_digest="$1" output_dir="$2" output_digest="$3"
    [ -f "$input_digest" ] && \
        (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
         stack exec digest-check -- refresh \
            --inputs "$input_digest" \
            --output-dir "$output_dir" \
            --output-digest "$output_digest")
}

# Print the package list emitted by a Layer 2 batch assembler — i.e.
# the set of packages that `bootstrap-from-json --all-packages
# --include-coders` produces output for, regardless of target language.
# This is the union of:
#   - the two baseline packages (hydra-kernel, hydra-haskell), always loaded
#   - the four coder packages (hydra-java, hydra-python, hydra-scala,
#     hydra-lisp), loaded by --include-coders
#
# It does NOT include the ext / ext-demo packages (hydra-pg, hydra-rdf,
# hydra-coq, hydra-javascript, hydra-wasm, hydra-ext) — those are
# auto-loaded only when explicitly named via --package <pkg>, so they
# go through the per-package assemble-distribution.sh path instead.
# See heads/haskell/src/exec/bootstrap-from-json/Main.hs around
# "--all-packages alone does NOT auto-load these" for the executable
# side of the same contract.
#
# Used by Layer 2 batch assemblers to drive per-package post-processing
# and digest refresh from the registry rather than by walking dist/.
#
# Usage: batch_emit_packages
batch_emit_packages() {
    echo "hydra-kernel hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp"
}

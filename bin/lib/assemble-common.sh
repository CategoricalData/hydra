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
#   batch_emit_packages
#
# Digest helpers shell out to `digest-check` (Haskell exec). Caller
# must have $HYDRA_ROOT_DIR pointing at the worktree root.

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

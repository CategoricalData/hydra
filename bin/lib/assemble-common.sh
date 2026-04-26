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
#
# Both helpers shell out to `digest-check`, which lives in the Haskell
# stack-work tree. Caller must have $HYDRA_ROOT_DIR pointing at the
# worktree root.

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

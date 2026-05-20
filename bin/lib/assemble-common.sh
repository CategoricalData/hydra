#!/usr/bin/env bash
# Shared per-source-set digest helpers for Layer 2 assemblers.
#
# Each assembler script sources this file and uses the helpers below to
# implement per-source-set freshness checks. The digest layout is uniform
# across every target language:
#
#   dist/json/<pkg>/build/<set>/digest.json     — input digest (v1)
#   dist/<lang>/<pkg>/build/<set>/digest.json   — output digest (v2)
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
# Compositional structure: a host transform is a small dependency graph
# of three components, each of which has its own identity:
#
#   stamp(L) = hash(
#       kernel-id  = component_identity hydra-kernel,
#       coder-id   = component_identity hydra-L (or hydra-lisp for any L
#                    in {clojure, scheme, common-lisp, emacs-lisp}),
#       runtime-id = runtime_identity L
#   )
#
# This shape isolates invalidation: a kernel change invalidates every
# target's stamp; a Java-coder change invalidates only Java; a Python
# runtime edit invalidates only Python. The flat-blob predecessor
# invalidated every target on any heads/haskell/src/main/haskell edit,
# which was over-eager.
#
# component_identity is the swappable layer: today it always returns a
# local content hash (the package isn't published yet), but post-#370
# it will branch on published-vs-local and return a version string for
# published hosts. The composition above is unchanged either way.
#
# Output: a 16-char hex prefix of the SHA-256, short enough to live in a
# JSON string without bloating the digest file.
#
# See #347 (Merkle cache invalidation) and #370 (external versioned hosts).
compute_generator_stamp() {
    local lang="$1"
    local coder_pkg
    case "$lang" in
        clojure|scheme|common-lisp|emacs-lisp|lisp) coder_pkg="hydra-lisp" ;;
        *)                                          coder_pkg="hydra-$lang" ;;
    esac
    {
        printf 'kernel:%s\n'  "$(component_identity hydra-kernel)"
        printf 'coder:%s\n'   "$(component_identity "$coder_pkg")"
        printf 'runtime:%s\n' "$(runtime_identity "$lang")"
    } | shasum -a 256 | awk '{print substr($1,1,16)}'
}

# Identity of a Hydra package. Today: hash of every Haskell source under
# packages/<pkg>/src/main/haskell/ — this transitively covers the package's
# DSL sources, manifests, and (for the kernel) all the json/encode/decode
# modules previously fingerprinted by Hydra.Digest.encoderId (now retired).
#
# Post-#370 swap point: when the package is available as a published
# artifact pinned in this build's manifest, return the version string
# instead. Single-function change; callers don't need updating.
component_identity() {
    local pkg="$1"
    local src_dir="$HYDRA_ROOT_DIR/packages/$pkg/src/main/haskell"
    if [ -d "$src_dir" ]; then
        find "$src_dir" -type f -name '*.hs' 2>/dev/null \
            | LC_ALL=C sort | xargs cat 2>/dev/null \
            | shasum -a 256 | awk '{print $1}'
    else
        # Package directory missing — emit a sentinel rather than empty
        # so the composition still produces a stable distinct value.
        echo "missing:$pkg"
    fi
}

# Identity of a host's hand-written runtime support — the code under
# heads/<lang>/src/main/ that the generator depends on but isn't part of
# any packages/<pkg>/. For Haskell, this is the orchestrator code
# (Hydra.Generation, Hydra.PackageRouting, etc.) that the bootstrap-from-json
# binary links against. For other hosts, it's the runtime classes / modules
# the generated code depends on at compile time (the digest pipeline doesn't
# read these to make decisions, but a change to them invalidates the
# downstream output because consumers' compiled artifacts would differ).
#
# Lisp special case: dialects share heads/lisp/bin/common.sh assembler
# logic plus per-dialect heads/lisp/<dialect>/src/main/ runtime trees.
runtime_identity() {
    local lang="$1"
    {
        if [ "$lang" = "lisp" ] || [ "$lang" = "clojure" ] || [ "$lang" = "scheme" ] \
                || [ "$lang" = "common-lisp" ] || [ "$lang" = "emacs-lisp" ]; then
            [ -f "$HYDRA_ROOT_DIR/heads/lisp/bin/common.sh" ] && cat "$HYDRA_ROOT_DIR/heads/lisp/bin/common.sh"
            find "$HYDRA_ROOT_DIR/heads/lisp" -path '*/src/main/*' -type f 2>/dev/null \
                | LC_ALL=C sort | xargs cat 2>/dev/null
            find "$HYDRA_ROOT_DIR/heads/lisp" -name 'assemble-distribution.sh' -type f 2>/dev/null \
                | LC_ALL=C sort | xargs cat 2>/dev/null
        else
            local main_dir="$HYDRA_ROOT_DIR/heads/$lang/src/main"
            [ -d "$main_dir" ] && \
                find "$main_dir" -type f 2>/dev/null \
                    | LC_ALL=C sort | xargs cat 2>/dev/null
            local assembler="$HYDRA_ROOT_DIR/heads/$lang/bin/assemble-distribution.sh"
            [ -f "$assembler" ] && cat "$assembler"
        fi
    } | shasum -a 256 | awk '{print $1}'
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
# hydra-coq, hydra-typescript, hydra-wasm, hydra-ext) — those are
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

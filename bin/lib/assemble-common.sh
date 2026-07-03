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
        printf 'driver:%s\n'  "$(driver_identity)"
    } | shasum -a 256 | awk '{print substr($1,1,16)}'
}

# Export the full generation-provenance environment (#413 / #523) for a
# target language, to be read by 'digest-check refresh' via
# Hydra.Digest.generationRecord. This is a SUPERSET of the old
# `export HYDRA_GENERATOR_STAMP=$(compute_generator_stamp <lang>)` line and
# REPLACES it at each assembler call site.
#
# Exports:
#   HYDRA_GENERATOR_STAMP          — the gating id (compute_generator_stamp)
#   HYDRA_GENERATION_MODE          — published | shim
#   HYDRA_GENERATION_HOST          — the target host language
#   HYDRA_GENERATION_HYDRA_VERSION — release version (published only; else empty)
#   HYDRA_GENERATION_REVISION      — <short-sha>[-dirty] (required for shim)
#   HYDRA_GENERATION_TIMESTAMP     — ISO-8601 UTC build time
#
# mode derivation: the gating stamp already branches published-vs-local inside
# component_identity (host:<pkg>:<ver> for a published host, a content hash
# otherwise). We reuse THAT SAME resolution for the kernel so mode agrees with
# what actually gates: a `host:` pin ⇒ published; anything else ⇒ shim. This
# keeps the informational gatherer honest without a second source of truth for
# the published/local seam (#413: "compute generatorId from a separate path
# than the informational gatherer, so they can't cross-contaminate" — the
# gating VALUE stays compute_generator_stamp; only the mode DISCRIMINATOR is
# read off component_identity here).
#
# Invariant enforced downstream in Haskell (generationRecord): shim ⇒ revision
# present. We always compute a revision, so a shim build is never left without
# one; publishing simply also records it (harmless, optional for published).
export_generation_env() {
    local lang="$1"
    export HYDRA_GENERATOR_STAMP
    HYDRA_GENERATOR_STAMP="$(compute_generator_stamp "$lang")"

    # Resolve the kernel's identity the same way the stamp does. A published
    # host yields `host:hydra-kernel:<ver>`; anything else is a local content
    # hash ⇒ shim.
    local kernel_id
    kernel_id="$(component_identity hydra-kernel)"
    local mode host_version
    case "$kernel_id" in
        host:hydra-kernel:*)
            mode="published"
            host_version="${kernel_id#host:hydra-kernel:}"
            ;;
        *)
            mode="shim"
            host_version=""
            ;;
    esac

    # Working-tree revision: <short-sha>, plus -dirty when there are uncommitted
    # changes to TRACKED files. This is the shim's only precise identity (#523).
    # Computed even for published mode (harmless, optional there).
    #
    # Dirtiness is scoped with `git diff --quiet HEAD` (tracked modifications,
    # staged + unstaged) rather than `git status --porcelain` (which also counts
    # UNTRACKED files). That distinction is load-bearing: every real assembly
    # writes untracked dist/** artifacts, so a whole-tree porcelain check would
    # stamp EVERY build — including a clean release build — as "-dirty", which is
    # exactly the provenance lie #523 exists to prevent. Only a genuine edit to a
    # committed source file should downgrade a build to dirty.
    local revision=""
    local short_sha
    short_sha="$(git -C "$HYDRA_ROOT_DIR" rev-parse --short HEAD 2>/dev/null || true)"
    if [ -n "$short_sha" ]; then
        revision="$short_sha"
        if ! git -C "$HYDRA_ROOT_DIR" diff --quiet HEAD 2>/dev/null; then
            revision="$revision-dirty"
        fi
    fi

    export HYDRA_GENERATION_MODE="$mode"
    export HYDRA_GENERATION_HOST="$lang"
    export HYDRA_GENERATION_HYDRA_VERSION="$host_version"
    export HYDRA_GENERATION_REVISION="$revision"
    export HYDRA_GENERATION_TIMESTAMP="$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}

# Identity of the Haskell generation DRIVER — the bootstrap-from-json exec plus the
# generation orchestrator modules it links against (Hydra.Generation, Hydra.ExtGeneration,
# Hydra.PackageRouting). This single Haskell binary emits EVERY target language, so a change
# to its emission logic (e.g. #473 Step 0: the hydra.lib.* lib pass, the hydra.<lang>.lib.*
# redirect transform, lowerPrimitiveDefinitions) changes generated output for the SAME input
# JSON. The per-package input digest is keyed only on the input JSON hash, so without this
# component such a change would NOT invalidate any output digest, and warm builds (surviving
# gitignored digests, e.g. after a pull) would cache-skip regeneration and ship the
# pre-change layout — silently breaking self-host. Included for ALL targets, since the driver
# is target-independent. (#473)
driver_identity() {
    {
        find "$HYDRA_ROOT_DIR/heads/haskell/src/exec/bootstrap-from-json" -type f -name '*.hs' 2>/dev/null \
            | LC_ALL=C sort | xargs cat 2>/dev/null
        for m in Generation ExtGeneration PackageRouting; do
            cat "$HYDRA_ROOT_DIR/heads/haskell/src/main/haskell/Hydra/$m.hs" 2>/dev/null
        done
    } | shasum -a 256 | awk '{print $1}'
}

# Identity of a Hydra package, used as a component of the per-target generator
# stamp. Two modes:
#
#   1. Published-host mode (#347/#370): when <pkg> resolves to a published-host
#      version via hydra.json (hostVersion, or a per-host hostOverrides
#      entry), the identity is the version string `host:<pkg>:<ver>`. In this
#      mode the build is conceptually depending on the published artifact, so
#      local source edits to <pkg> do NOT change its identity — the cache stays
#      warm until the pinned version bumps. That is the #370 speed property
#      ("don't rebuild on every comment tweak"). The per-package resolution
#      lives in bin/lib/hydra-packages.py (`host-version`), keyed off the
#      PUBLISHED_HOSTS allowlist there.
#
#   2. Local-source mode (the migration shim): when <pkg> is NOT a consumed
#      published host, fall back to a hash of every Haskell source under
#      packages/<pkg>/src/main/haskell/. This transitively covers the package's
#      DSL sources, manifests, and (for the kernel) all the json/encode/decode
#      modules previously fingerprinted by Hydra.Digest.encoderId (now retired).
#      A source edit invalidates the stamp, exactly as before publishing.
#
# This is the swap point #347 wired and #370 builds on. Single function;
# callers (compute_generator_stamp) don't need updating.
component_identity() {
    local pkg="$1"
    local ver
    if ver=$("$HYDRA_ROOT_DIR/bin/lib/hydra-packages.py" host-version "$pkg" 2>/dev/null) \
            && [ -n "$ver" ]; then
        printf 'host:%s:%s' "$pkg" "$ver"
        return
    fi
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

# Predicate: is the per-source-set output up to date with respect to its
# input digest? Returns 0 (fresh — skip the rebuild) or non-zero (not
# fresh — do the rebuild). Always called as an `if` condition, never bare,
# so returning non-zero here does not trip the caller's `set -e`.
#
# `digest-check fresh` already handles a missing input or output digest
# itself, printing a "cache miss" line and exiting 1, so the shell-side
# `[ -f ]` pre-checks are an optimization (skip the stack invocation when
# there is provably nothing to compare), not the source of truth. The old
# implementation appended `2>/dev/null` to the stack call; that silenced
# digest-check's own diagnostic ("input digest absent", "generator stamp
# mismatch", etc.), which is exactly the kind of cause-naming output #414
# wants preserved. Drop the suppression so a miss explains itself.
#
# Note: digest-check fresh emits only 0 (hit) or 1 (miss); a crash in the
# Haskell runtime also exits non-zero, so a fault is conservatively treated
# as a miss (rebuild) rather than swallowed as a hit — the safe direction.
assemble_check_fresh() {
    local input_digest="$1" output_dir="$2" output_digest="$3" keep_manifest="${4:-}"
    # First-build / missing-artifact fast path: provably not fresh.
    if [ ! -f "$input_digest" ] || [ ! -f "$output_digest" ]; then
        return 1
    fi
    # Pass the keep-paths manifest (hand-written overlay files copied into the
    # output dir by copy-overlay.sh) so the #393 orphan reconcile inside `fresh`
    # does not delete them — they are not in the recorded output digest. (#511)
    local keep_args=()
    if [ -n "$keep_manifest" ] && [ -f "$keep_manifest" ]; then
        keep_args=(--keep-paths-from "$keep_manifest")
    fi
    (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
     stack exec digest-check -- fresh \
        --inputs "$input_digest" \
        --output-dir "$output_dir" \
        --output-digest "$output_digest" \
        "${keep_args[@]}")
}

# Write the per-source-set output digest after a (re)generation. Called in
# statement position immediately after the modules were generated, so the
# input digest MUST exist here — generation just consumed it. A missing
# input digest at this point is a pipeline inconsistency, not a routine
# condition: under `set -e` the old `[ -f X ] && (...)` guard turned that
# into a silent script death (the "Phase 2 silent exit" pitfall), aborting
# the assembler with no error and skipping every remaining package. Make
# it a loud, named error instead.
assemble_refresh_digest() {
    local input_digest="$1" output_dir="$2" output_digest="$3"
    if [ ! -f "$input_digest" ]; then
        echo "ERROR: assemble_refresh_digest: input digest not found: $input_digest" >&2
        echo "       Cannot write output digest $output_digest without it." >&2
        echo "       This indicates a missing or stale build artifact upstream;" >&2
        echo "       regenerate the per-package JSON / input digest before assembling." >&2
        return 1
    fi
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
#   - the five coder packages (hydra-java, hydra-python, hydra-scala,
#     hydra-lisp, hydra-typescript), loaded by --include-coders
#
# It does NOT include the ext / ext-demo packages (hydra-pg, hydra-rdf,
# hydra-coq, hydra-wasm, hydra-ext) — those are auto-loaded only when
# explicitly named via --package <pkg>, so they go through the per-package
# assemble-distribution.sh path instead. (hydra-go is loaded as a coder for
# the universe but not emitted here — it has no publishable JVM artifact.)
# See heads/haskell/src/exec/bootstrap-from-json/Main.hs around
# "--all-packages alone does NOT auto-load these" for the executable
# side of the same contract.
#
# Used by Layer 2 batch assemblers to drive per-package post-processing
# and digest refresh from the registry rather than by walking dist/.
#
# Usage: batch_emit_packages
batch_emit_packages() {
    echo "hydra-kernel hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp hydra-typescript"
}

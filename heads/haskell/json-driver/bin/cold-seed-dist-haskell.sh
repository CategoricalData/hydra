#!/usr/bin/env bash
# #376 Stage 1 — cold-clone seeder for dist/haskell.
#
# On a fresh checkout where dist/haskell/ is ABSENT, this seeds all 16 package
# source trees from dist/json (tracked) using the standalone cold-only seeder
# (json-driver/app/ColdSeedMain.hs) built against the PUBLISHED Hackage packages,
# then emits each package's package.yaml manifest so every dist/haskell/<pkg>/ is
# a self-contained, buildable Haskell package (Default A, #376).
#
# It does NOT compile the 5 unpublished packages (hydra-{go,coq,wasm,ext,bench});
# their SOURCE is seeded, their manifest is emitted, and a user can build them on
# demand with `stack build` inside dist/haskell/<pkg>/.
#
# This is the cold-start driver the untrack (Stage 3) depends on. It is NOT yet
# wired into sync.sh (Stage 2/3). Run standalone to seed a cold tree.
#
# Usage:
#   heads/haskell/json-driver/bin/cold-seed-dist-haskell.sh [--repo-root DIR]
#
# Env:
#   HYDRA_ROOT_DIR   worktree root (default: derived from this script's path)
set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DRIVER_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

REPO_ROOT="${HYDRA_ROOT_DIR:-}"
while [ $# -gt 0 ]; do
    case "$1" in
        --repo-root) REPO_ROOT="$2"; shift 2 ;;
        --repo-root=*) REPO_ROOT="${1#--repo-root=}"; shift ;;
        *) echo "Unknown argument: $1" >&2; exit 2 ;;
    esac
done
if [ -z "$REPO_ROOT" ]; then
    REPO_ROOT="$( cd "$DRIVER_DIR/../../.." && pwd )"
fi
export HYDRA_ROOT_DIR="$REPO_ROOT"

# Portable in-place sed (BSD/macOS vs GNU). The shim patches below use it so a
# macOS reseed of dist/haskell works — bare `sed -i -e` is GNU-only and dies on
# BSD sed (it reads `-e` as the backup suffix). Reported by feature_289_go.
source "$REPO_ROOT/bin/lib/common.sh"

echo "=== #376 cold-seed dist/haskell ==="
echo "  repo root:  $REPO_ROOT"
echo "  driver:     $DRIVER_DIR"
echo ""

# 0. Populate headmods/ with fresh copies of the exactly-four head generation
#    modules the cold seeder needs (Generation, PackageRouting, TargetFilePaths,
#    Digest). We copy (not symlink, not whole-tree source-dir) so that:
#      * the copies survive `git archive` / `cp` into a cold checkout (symlinks do
#        not reliably), and
#      * hpack discovers ONLY these four under the headmods source-dir — the whole
#        heads/haskell/src/main/haskell tree also holds Hydra.Coq.GenerateDriver /
#        Hydra.ExtGeneration / Hydra.Tools.* which need the unpublished ext/coq/wasm
#        coders and would break the published-only build.
#    They are regenerated from the canonical head source on every run (a generated
#    artifact, gitignored) — never hand-edited, so there is no content drift.
HEAD_SRC="$REPO_ROOT/heads/haskell/src/main/haskell/Hydra"
HEADMODS="$DRIVER_DIR/headmods/Hydra"
echo "[0/4] Refreshing headmods/ (4 head gen modules) from canonical head source..."
rm -rf "$DRIVER_DIR/headmods"
mkdir -p "$HEADMODS"
for m in Generation PackageRouting TargetFilePaths Digest; do
    cp "$HEAD_SRC/$m.hs" "$HEADMODS/$m.hs"
done
# #497 published-host shim: this copy compiles against the PUBLISHED hydra-kernel
# (stack.yaml extra-deps, pinned below 0.17.2), which still exports Hydra.Show.* —
# the canonical head source already uses the post-#497 Hydra.Print.* names (correct
# for its own, local-kernel compile context). Patch ONLY this ephemeral copy back to
# the published name; drop once hydra-kernel republishes with the #497 rename.
sed_inplace 's/Hydra\.Print\.Errors/Hydra.Show.Errors/g' \
    "$HEADMODS/Generation.hs"

# #622: writePerPackageManifestsJson (the #607 shim's target) has been moved
# structurally out of Generation.hs into Hydra.ManifestGeneration, a module used
# only by the update-json-manifest driver (which always runs against a local,
# non-cold-seed build with the full hydra-build available). Generation.hs no
# longer imports Hydra.Build.ManifestWriter at all, so the #607 sed patches are
# gone — not stubbed, structurally absent. ManifestGeneration.hs is deliberately
# NOT among the headmods copied below: ColdSeedMain never calls
# writePerPackageManifestsJson, so the cold seeder has no need of it, and copying
# it in would reintroduce the unpublished-module coupling this split removes.

# No-new-Build.*-imports invariant check (#622): the four cold-seeder headmods may
# only import the Hydra.Build.* modules already known to compile against the
# PUBLISHED hydra-build version this script pins (stack.yaml extra-deps, currently
# hydra-build-0.17.1) — today, only Hydra.Build.Routing (via PackageRouting.hs,
# #560's precedent: consumed from the published package, no shim needed). Any OTHER
# Hydra.Build.* import is exactly the dependency that breaks a cold-clone when a new
# hydra-build module lands in source before it is published (the #560/#607 revert
# class). This is a fast local tripwire so a future change (e.g. a Digest.hs
# repoint onto Hydra.Build.Format) fails here instead of surfacing only via a
# cold-clone CI run. Update ALLOWED_BUILD_IMPORTS when a new Build.* module is
# confirmed present in the pinned published hydra-build version.
ALLOWED_BUILD_IMPORTS='Hydra\.Build\.Routing'
for m in Generation PackageRouting TargetFilePaths Digest; do
    disallowed=$(grep -E '^import qualified Hydra\.Build\.' "$HEADMODS/$m.hs" | grep -vE "$ALLOWED_BUILD_IMPORTS" || true)
    if [ -n "$disallowed" ]; then
        echo "ERROR: $HEADMODS/$m.hs imports a Hydra.Build.* module not in the allow-list:" >&2
        echo "$disallowed" >&2
        echo "  Cold-seeder headmods must not depend on hydra-build modules beyond what is" >&2
        echo "  confirmed published at the pinned version (#560/#607 revert class). If this" >&2
        echo "  module IS published at the pinned hydra-build version, add it to" >&2
        echo "  ALLOWED_BUILD_IMPORTS above. Otherwise, extract the coupled logic into a" >&2
        echo "  module used only outside the cold-seed path (see Hydra.ManifestGeneration, #622)." >&2
        exit 1
    fi
done

# 0b. (#608) Refresh typesmods/ with a build-time copy of the Terms-FREE DSL Types
#     subtree the cold seeder compiles as its JSON decode-universe context:
#     Hydra/Sources/Kernel/Types/** plus the single Hydra/Sources/Json/Model.hs it
#     imports (the exact transitive closure of Hydra.Sources.Kernel.Types.All).
#     Copied — not whole-tree source-dir'd from packages/hydra-kernel — so hpack
#     never discovers the Terms modules (incl. Print.Error.Core, phantom-annotated
#     with HEAD-only kernel types) under the source-dir. This is the oil-and-water
#     fix (#608): compiling only Terms-free Type DEFINITIONS against the published
#     kernel keeps a single type identity, so published coders link with no
#     local-vs-published mismatch. Generated artifact, gitignored, never hand-edited.
KERNEL_SRC="$REPO_ROOT/packages/hydra-kernel/src/main/haskell"
TYPESMODS="$DRIVER_DIR/typesmods"
echo "[1/4] Refreshing typesmods/ (Terms-free DSL Types subtree) from packages/hydra-kernel..."
rm -rf "$TYPESMODS"
mkdir -p "$TYPESMODS/Hydra/Sources/Kernel/Types" "$TYPESMODS/Hydra/Sources/Json"
cp -R "$KERNEL_SRC/Hydra/Sources/Kernel/Types/." "$TYPESMODS/Hydra/Sources/Kernel/Types/"
cp "$KERNEL_SRC/Hydra/Sources/Json/Model.hs" "$TYPESMODS/Hydra/Sources/Json/Model.hs"

# 2. Build the cold-only seeder against the published Hackage packages.
echo "[2/4] Building cold-only seeder (published 0.17.1 deps)..."
( cd "$DRIVER_DIR" && stack build )

# 3. Seed all 16 package SOURCE trees from dist/json into dist/haskell.
#    The seeder is invoked PER-PACKAGE with --package <pkg> (mirroring
#    heads/haskell/bin/transform-json-to-target.sh): --all-packages mode only
#    emits the bootstrapping core (kernel/haskell/build), whereas the full tree
#    (coders + data-domain + ext) is seeded per-package with the right load flags.
#
# --include-dsls is scoped to the packages whose DSL wrapper modules are
# ACTUALLY part of today's tracked dist/haskell baseline (#376 investigation,
# 2026-07-17): hydra-{jvm,wasm,ext,build,bench} are excluded here because no
# current sync codepath (bin/sync.sh's LANG_UNION-driven Phase 2, or
# batch_emit_packages in bin/lib/assemble-common.sh) ever invokes
# assemble-distribution.sh --include-dsls for them, so their DSL wrappers
# were never generated into the committed tree, even though bootstrap-from-json
# WOULD produce them if asked (verified empirically — real binary, --package
# hydra-jvm --include-dsls, emits Hydra/Dsl/Jvm/Serde.hs; tracked dist lacks
# it). The seeder mirrors the CURRENT tracked reality rather than a corrected
# one, so the untrack's cold-clone output matches today's committed dist
# exactly. The sync gap itself (these 5 packages' DSL wrappers never being
# generated by any wired path) is a pre-existing issue orthogonal to #376 —
# left as a follow-up, not fixed on this branch. See bug_376_hackage_bootstrap-
# plan.md "RESOLVED: jvm/wasm/ext +7 DELTA" for the full investigation.
SEEDER_BIN="$( cd "$DRIVER_DIR" && stack path --local-install-root )/bin/cold-seed-from-json"
echo ""
echo "[3/4] Seeding dist/haskell source trees from dist/json (per-package)..."
# Two passes per package, mirroring heads/haskell/bin/assemble-distribution.sh
# EXACTLY (Steps 1 and 2 there): a MAIN pass (--include-dsls, no --include-tests)
# and a separate TEST pass (--include-tests, no --include-dsls). This is not a
# stylistic choice — bootstrap-from-json/Main.hs's `sourceSetForFilter` /
# `testOnlyInvocation` logic (around line 596/853-858) treats --include-tests as
# switching to a DIFFERENT digest-filter/prune mode ("test" vs "main"), and a
# combined single call with both flags set does not reproduce either pass
# faithfully (found in the #376 cold-clone proof: kernel came out with a
# main+test-mixed module count instead of the tracked main-only baseline).
for pkg in $(python3 "$REPO_ROOT/bin/lib/hydra-packages.py" list); do
    # Load flags mirror transform-json-to-target.sh: baseline needs nothing;
    # everything else needs --include-coders (coder packages themselves, and
    # ext/data-domain packages whose modules reference coder-package types).
    case "$pkg" in
        hydra-kernel|hydra-haskell) LOAD_FLAGS="" ;;
        *)                          LOAD_FLAGS="--include-coders" ;;
    esac
    # DSL_FLAG: only for packages with DSL wrappers in the current tracked
    # baseline (see comment above). Keep in sync with that baseline if it
    # ever changes.
    case "$pkg" in
        hydra-jvm|hydra-wasm|hydra-ext|hydra-build|hydra-bench) DSL_FLAG="" ;;
        *)                                                       DSL_FLAG="--include-dsls" ;;
    esac
    echo "  seeding $pkg (main) ..."
    "$SEEDER_BIN" \
        --target haskell \
        $LOAD_FLAGS \
        --package "$pkg" \
        --output "$REPO_ROOT/dist/haskell" \
        --dist-json-root "$REPO_ROOT/dist/json" \
        $DSL_FLAG \
        --prune-stale
    # Test pass: only if the package has test JSON at all (mirrors
    # assemble-distribution.sh's TEST_JSON_DIR existence check).
    TEST_JSON_DIR="$REPO_ROOT/dist/json/$pkg/src/test/json"
    if [ -d "$TEST_JSON_DIR" ]; then
        echo "  seeding $pkg (test) ..."
        "$SEEDER_BIN" \
            --target haskell \
            $LOAD_FLAGS \
            --package "$pkg" \
            --output "$REPO_ROOT/dist/haskell" \
            --dist-json-root "$REPO_ROOT/dist/json" \
            --include-tests \
            --prune-stale
    fi
done

# 3b. X3 published-host-prefix-helper-rename shim (0.17.2 release).
#
# Binary literals store a VALUE, not a primitive name; the Haskell CODER injects
# the helper wrapper name at emission time (Sources/Haskell/Coder.hs). The 0.17.2
# kernel renames that wrapper: Literals.stringToBinary -> Literals.base64ToBinary
# and Literals.binaryToString -> Literals.binaryToBase64 (X3). But this seeder is
# built against the PUBLISHED 0.17.1 coder, which hardcodes the OLD wrapper names,
# so the seeded test tree emits `Literals.stringToBinary`/`binaryToString` — names
# the 0.17.2 overlay Literals module no longer exports. That breaks the very next
# `stack build` of exe:bootstrap-from-json (its source-dirs include the kernel test
# tree, #546), which is the compile that PRODUCES the source-built, new-name coder —
# a bootstrap circularity: the coder that would regenerate the tree with correct
# names cannot be built until the tree compiles.
#
# The rename is pure (both wrappers base64-decode/-encode identically — the seeded
# literal values are already base64, e.g. "QUI="), so rewriting the name in the
# seeded output is semantics-preserving. Patch the seeded test tree back to the
# new names so exe:bootstrap-from-json compiles; sync-haskell.sh Step 4 then
# regenerates the whole tree with the source-built coder, overwriting these files
# with byte-identical output. This is a bootstrap patch (overwritten by the next
# regeneration), same shape as the #497/#607 shims above — NOT a persistent
# post-generation patch. Drop it once the 0.17.2 kernel is the published host.
echo ""
echo "[3b/4] X3 shim: rewriting stale binary-literal wrapper names in seeded test tree..."
grep -rl 'Literals\.stringToBinary\|Literals\.binaryToString' \
    "$REPO_ROOT/dist/haskell"/*/src/test/haskell 2>/dev/null \
    | while IFS= read -r f; do
        sed_inplace 's/Literals\.stringToBinary/Literals.base64ToBinary/g; s/Literals\.binaryToString/Literals.binaryToBase64/g' "$f"
        echo "  shimmed: ${f#$REPO_ROOT/}"
    done

# 4. Emit each package's package.yaml manifest so every dist/haskell/<pkg>/ is a
#    self-contained buildable package (Default A). Covers all 16, including the
#    5 unpublished ones — the generator already supports them.
#
# Also copy LICENSE/NOTICE/CHANGELOG.md into each package dir. `stack build`'s
# copy/register step fails without them (`generate-haskell-package-build.py`
# always declares them in extra-source-files, mirroring the publish-path
# assembler at heads/haskell/bin/assemble-haskell-distribution.sh:104-110,
# which stages the same 3 files into its sdist temp dir before generating
# build files). The cold seed has no staging step, so it must copy them
# directly into dist/haskell/<pkg>/ — otherwise the manifest declares files
# that don't exist and the on-demand `stack build` "buildable on demand"
# story (Default A, deliverable 4) fails at the final copy/register step even
# though compilation itself succeeds (found running the hydra-ext build demo).
echo ""
echo "[4/4] Emitting per-package manifests (all 16)..."
PACKAGES="$(python3 "$REPO_ROOT/bin/lib/hydra-packages.py" list)"
for pkg in $PACKAGES; do
    python3 "$REPO_ROOT/bin/lib/generate-haskell-package-build.py" "$pkg" \
        --repo-root "$REPO_ROOT"
    cp "$REPO_ROOT/heads/haskell/LICENSE" "$REPO_ROOT/dist/haskell/$pkg/LICENSE"
    cp "$REPO_ROOT/CHANGELOG.md" "$REPO_ROOT/dist/haskell/$pkg/CHANGELOG.md"
    cp "$REPO_ROOT/NOTICE" "$REPO_ROOT/dist/haskell/$pkg/NOTICE"
done

echo ""
echo "=== cold-seed complete: dist/haskell seeded + manifests emitted ==="

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
echo "[0/4] Refreshing headmods/ (5 head gen modules) from canonical head source..."
rm -rf "$DRIVER_DIR/headmods"
mkdir -p "$HEADMODS"
# DigestFormat (#512) carries the typed digest codec and is the ONE headmod
# permitted to import the published Hydra.Build.Format family (see the
# allow-list check below); Digest.hs itself stays Build-free.
for m in Generation PackageRouting TargetFilePaths Digest DigestFormat; do
    cp "$HEAD_SRC/$m.hs" "$HEADMODS/$m.hs"
done
# #497 published-host shim removed: stack.yaml now pins hydra-kernel-0.17.3, which
# publishes the post-#497 Hydra.Print.Errors name natively (verified present at
# 0.17.2, confirmed still true at 0.17.3), so the canonical head source (already
# using Hydra.Print.Errors) needs no patching for this ephemeral copy.

# #624 cold-seed shim REMOVED: it stripped the `False` compactMaps argument from
# DigestFormat.hs's 5-arg hydra.json.{encode.toJson,decode.fromJson} calls, because
# published hydra-kernel-0.17.3 still exposed the pre-#624 4-arg signature. The pin
# above is now 0.17.4, which publishes the 5-arg form, so the canonical head source
# compiles against it unmodified — and stripping the argument now BREAKS the build
# ("Couldn't match expected type `Bool' with actual type `Core.Name'"). Removing the
# shim on the pin bump is exactly what its own comment instructed.

# #622: writePerPackageManifestsJson (the #607 shim's target) has been moved
# structurally out of Generation.hs into Hydra.ManifestGeneration, a module used
# only by the update-json-manifest driver (which always runs against a local,
# non-cold-seed build with the full hydra-build available). Generation.hs no
# longer imports Hydra.Build.ManifestWriter at all, so the #607 sed patches are
# gone — not stubbed, structurally absent. ManifestGeneration.hs is deliberately
# NOT among the headmods copied below: ColdSeedMain never calls
# writePerPackageManifestsJson, so the cold seeder has no need of it, and copying
# it in would reintroduce the unpublished-module coupling this split removes.

# No-new-Build.*-imports invariant check (#622): the cold-seeder headmods may
# only import the hydra-build modules already known to compile against the
# PUBLISHED hydra-build version this script pins (stack.yaml extra-deps, currently
# hydra-build-0.17.3): Hydra.Build.Routing (via PackageRouting.hs, #560's
# precedent) and the Hydra.Build.Format codec family (via DigestFormat.hs, #512 —
# Format + Encode/Decode.Build.Format all verified exposed by the published
# 0.17.3 cabal). Any OTHER import from the hydra-build package is exactly the
# dependency that breaks a cold-clone when a new hydra-build module lands in
# source before it is published (the #560/#607 revert class). This is a fast
# local tripwire so such a change fails here instead of surfacing only via a
# cold-clone CI run. Update ALLOWED_BUILD_IMPORTS when a new module is confirmed
# present in the pinned published hydra-build version.
# #512 widened the grep: the original pattern matched only Hydra.Build.*, letting
# Hydra.{Encode,Decode,Dsl}.Build.* imports (equally hydra-build-owned) sail past
# unchecked.
ALLOWED_BUILD_IMPORTS='Hydra\.Build\.Routing|Hydra\.Build\.Walk|Hydra\.Build\.Format|Hydra\.Encode\.Build\.Format|Hydra\.Decode\.Build\.Format'
for m in Generation PackageRouting TargetFilePaths Digest DigestFormat; do
    disallowed=$(grep -E '^import qualified Hydra\.(Build|Encode\.Build|Decode\.Build|Dsl\.Build)\.' "$HEADMODS/$m.hs" | grep -vE "$ALLOWED_BUILD_IMPORTS" || true)
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
# #512: the typed digest codec (DigestFormat headmod) builds its schema context
# from the hydra.build.format DSL source; copy it (and only it) from
# packages/hydra-build — a type-level module, Terms-free like Kernel/Types.
BUILD_SRC="$REPO_ROOT/packages/hydra-build/src/main/haskell"
mkdir -p "$TYPESMODS/Hydra/Sources/Build"
cp "$BUILD_SRC/Hydra/Sources/Build/Format.hs" "$TYPESMODS/Hydra/Sources/Build/Format.hs"

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

# X3 published-host-prefix-helper-rename shim removed: it patched the seeded test
# tree's binary-literal wrapper names (Literals.stringToBinary/binaryToString ->
# base64ToBinary/binaryToBase64) because the seeder's coder, pinned at 0.17.1, still
# hardcoded the old names. Its own comment said "drop it once the 0.17.2 kernel is
# the published host" — stack.yaml now pins 0.17.3, so the seeder's coder emits the
# new names natively and the shim's grep finds zero matches (which, piped into
# `while read` under `set -euo pipefail`, killed the script — the shim itself broke
# on its own now-unneeded condition).

# (R22/#417 Formatting Prelude-hiding shim removed 0.17.5: stack.yaml now pins the
# R22-aware published coder (>= 0.17.5), which emits `import Prelude hiding (... lines
# ... unlines)` natively, so the seeded Formatting.hs needs no injection. The shim's
# own self-breaking retirement guard fired once pins hit 0.17.5 — retired here per its
# instruction, matching the #497/#624/X3 add-then-retire-on-publish precedent.)

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

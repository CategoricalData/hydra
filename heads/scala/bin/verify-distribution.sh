#!/usr/bin/env bash
# Verify the per-package Hydra Scala distributions are self-contained and
# self-consistent as PUBLISHED Maven artifacts.
#
# Scala analog of heads/java/bin/verify-distribution.sh and
# heads/python/bin/verify-distribution.sh. Same class of bug it guards against:
# a published package whose code references something that lives only in the
# worktree (heads/scala/ or another package's source set), not in the packaged
# dist/scala/<pkg>/ artifact. Locally everything compiles because each
# dist/scala/<pkg>/ is built via the monolithic packages/hydra-scala/ dev
# project during regular sync/test; the break would only appear ACROSS the
# packaging boundary, when a consumer resolves the published jar + POM from
# Maven. No existing check exercises that boundary for Scala — this does.
# (See #537; cf. #472/#519 for the Python/Java analogs of the same bug class.)
#
# How it works (mirrors the Java verifier's temp-repo isolation, adapted to
# sbt/Ivy):
#   1. Publish the Scala publish-set packages (mirror of publish-sbt.sh
#      PUBLISH_SET, leaves-first) from each dist/scala/<pkg>/ build into a
#      FRESH, TEMPORARY local Ivy repo (`sbt --ivy <tmp>`), NOT into ~/.ivy2
#      (so a stale or published artifact can't mask a gap). `sbt --ivy <path>`
#      redirects BOTH where publishLocal writes and where dependency
#      resolution reads, so the temp repo is a complete substitute for
#      ~/.ivy2/local for the rest of this script.
#   2. Build a tiny throwaway consumer sbt project that depends on
#      net.fortytwo.hydra.scala:<pkg>_3:<version> for every publish-set
#      coordinate. The consumer is built with the SAME --ivy temp dir, so
#      Hydra coordinates resolve ONLY from what step 1 published; third-party
#      transitive deps resolve normally (Maven Central + the sbt plugin
#      resolvers). A dependent whose POM references a sibling missing from the
#      publish set fails loudly instead of silently pulling a published
#      (possibly-broken) version from Central.
#   3. Compile a trivial source file that imports a public kernel type, proving
#      the published jar actually carries the classes its API surface needs.
#
# This is verification only — it does NOT upload anything (see publish-sbt.sh).
#
# Requirements: sbt on PATH; JDK 11+ (matches docs/release-workflow.md's stated
# Maven Central / Scala-sbt requirement).
#
# Usage:
#   verify-distribution.sh [--keep]
#     --keep  leave the temp Ivy repo + work dir in place for inspection.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_SCALA_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT="$( cd "$HYDRA_SCALA_DIR/../.." && pwd )"

KEEP=false
[ "${1:-}" = "--keep" ] && KEEP=true

# Scala publish set, leaves-first (mirror of publish-sbt.sh PUBLISH_SET).
# hydra-pg is included: the Scala pg coder's type-argument specialization on the
# multi-param hydra.pg.mapping.Schema[S,T,V,E] is fixed (#589) so hydra-pg
# compiles standalone. Experimental targets (go/coq/wasm), benchmarks
# (hydra-bench), and hydra-ext (coder limitation) are not published for Scala.
PUBLISH_SET=(
    hydra-kernel
    hydra-build
    hydra-haskell
    hydra-jvm
    hydra-java
    hydra-python
    hydra-scala
    hydra-lisp
    hydra-typescript
    hydra-rdf
    hydra-pg
)
# #519: Scala artifacts publish under the per-language group
# net.fortytwo.hydra.scala (must match GROUP_ID in
# bin/lib/generate-scala-package-build.py), with the Scala 3 cross-version
# suffix (_3) on every artifact id.
GROUP="net.fortytwo.hydra.scala"
VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"

# --- Guard: sbt on PATH -------------------------------------------------------
if ! command -v sbt >/dev/null 2>&1; then
    echo "ERROR: sbt not found on PATH; Scala distribution verification needs sbt." >&2
    exit 1
fi
echo "  sbt OK: $(sbt --version 2>&1 | head -1)"

WORK="$(mktemp -d -t hydra-verify-scala-XXXXXX)"
IVY_REPO="$WORK/ivy2"   # temp local Ivy repo (NOT ~/.ivy2)
CONSUMER="$WORK/consumer"
cleanup() { [ "$KEEP" = true ] || rm -rf "$WORK"; }
trap cleanup EXIT

echo "=== Verifying Hydra Scala per-package distributions (published-artifact isolation) ==="
echo "  version:   $VERSION"
echo "  publish:   ${PUBLISH_SET[*]}"
echo "  temp ivy:  $IVY_REPO"
echo "  work dir:  $WORK"
echo ""

# --- 1. Publish each publish-set package into the temp Ivy repo, leaves-first. --
for pkg in "${PUBLISH_SET[@]}"; do
    pkgdir="$HYDRA_ROOT/dist/scala/$pkg"
    [ -d "$pkgdir" ] || { echo "ERROR: missing dist package: $pkgdir" >&2; exit 1; }
    echo "--- publishLocal $pkg to temp Ivy repo ---"
    ( cd "$pkgdir" && sbt --ivy "$IVY_REPO" --batch publishLocal ) || {
        echo "ERROR: publishLocal failed for $pkg" >&2
        KEEP=true
        exit 1
    }
done
echo ""

# --- 2. Throwaway consumer: Hydra from the temp Ivy repo, third-party normal. --
echo "=== Building an isolated consumer against the published coordinates ==="
mkdir -p "$CONSUMER/src/main/scala/probe" "$CONSUMER/project"

cat > "$CONSUMER/project/build.properties" <<PROPS
sbt.version=1.10.7
PROPS

# build.sbt: depend on every publish-set coordinate (with the _3 cross-version
# suffix) so each POM + jar must resolve from the temp Ivy repo. `compile`
# proves the classes are present.
{
    echo "ThisBuild / scalaVersion := \"3.3.7\""
    echo "lazy val root = (project in file(\".\"))"
    echo "  .settings("
    echo "    name := \"hydra-verify-consumer\","
    echo "    libraryDependencies ++= Seq("
    for pkg in "${PUBLISH_SET[@]}"; do
        echo "      \"${GROUP}\" % \"${pkg}_3\" % \"${VERSION}\","
    done
    echo "    )"
    echo "  )"
} > "$CONSUMER/build.sbt"

# A trivial probe that touches a public kernel type, forcing the kernel jar's
# classes onto the compile classpath (a jar missing them fails here).
cat > "$CONSUMER/src/main/scala/probe/Probe.scala" <<'SCALA'
package probe

// Imports a public kernel type. If the published hydra-kernel jar does not
// carry this class (a self-broken artifact), compilation fails, which is
// exactly the packaging-boundary break this verifier exists to catch.
object Probe {
  def main(args: Array[String]): Unit = {
    val name = classOf[hydra.core.Name]
    println(s"hydra-kernel class on classpath: ${name.getName}")
  }
}
SCALA

if ( cd "$CONSUMER" && sbt --ivy "$IVY_REPO" --batch compile ); then
    echo ""
    echo "=== OK: published Scala artifacts resolve + compile in isolation ==="
else
    echo ""
    echo "=== FAIL: consumer did not resolve/compile against the published jars ===" >&2
    echo "    A publish-set package is missing classes its API needs, or a POM" >&2
    echo "    references a sibling not in the publish set. See $WORK." >&2
    KEEP=true
    exit 1
fi

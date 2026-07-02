#!/usr/bin/env bash
# Install the Hydra Java host packages into the local Maven repo (~/.m2) so
# that the published-host Java path (heads/java/json-driver/) can resolve them
# without waiting for a Maven Central upload.
#
# This is the "fast-path shim" described in docs/recipes/migration-shims.md:
# build hydra-kernel + hydra-java from dist/java/ and publishToMavenLocal so
# heads/java/json-driver/build.gradle finds them via its mavenLocal() resolver.
#
# After running this script, the java hostOverride ("local") can be dropped from
# hydra.json so the published-host sync path is active (bin/update-java-json.sh
# without --local-host, which is the default).
#
# Prerequisites:
#   - dist/java/ must already exist (run bin/sync-java.sh or bin/sync.sh first).
#   - JDK 11+ (the artifacts target Java 11; publishToMavenLocal does not need
#     JDK 17+ because it does not invoke the nmcp Central Portal plugin).
#
# Usage:
#   bin/install-java-local-m2.sh            # install hydra-kernel + hydra-java
#   bin/install-java-local-m2.sh --all      # install full publish set (all packages)
#   bin/install-java-local-m2.sh --package <pkg>   # install one package only
#   bin/install-java-local-m2.sh --refresh  # pass --refresh-dependencies to gradle
#
# Note: signing is NOT required for publishToMavenLocal (only Central Portal
# uploads need GPG credentials).

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

# Minimal set needed by the json-driver (net.fortytwo.hydra.java:hydra-java
# transitively declares hydra-kernel, so both are needed in ~/.m2).
MINIMAL_SET=(
    hydra-kernel
    hydra-java
)

# Full publish set (mirrors publish-maven.sh PUBLISH_SET, leaves first).
FULL_SET=(
    hydra-kernel
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

INSTALL_SET=("${MINIMAL_SET[@]}")
REFRESH_FLAG=""
ONLY_PKG=""

while [ $# -gt 0 ]; do
    case "$1" in
        --all) INSTALL_SET=("${FULL_SET[@]}"); shift ;;
        --package) ONLY_PKG="$2"; INSTALL_SET=("$2"); shift 2 ;;
        --refresh) REFRESH_FLAG="--refresh-dependencies"; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *) echo "Unknown argument: $1" >&2; exit 1 ;;
    esac
done

VERSION="$("$HYDRA_ROOT/bin/lib/hydra-packages.py" current-version)"
echo "Installing net.fortytwo.hydra.java @ $VERSION into ~/.m2 ..."

for pkg in "${INSTALL_SET[@]}"; do
    pkgdir="$HYDRA_ROOT/dist/java/$pkg"
    if [ ! -d "$pkgdir" ]; then
        echo ""
        echo "ERROR: $pkgdir not found." >&2
        echo "       Run bin/sync-java.sh (or bin/sync.sh) to generate dist/java/ first." >&2
        exit 1
    fi
    echo "--- publishToMavenLocal: $pkg @ $VERSION ---"
    ( cd "$pkgdir" && gradle publishToMavenLocal -x test $REFRESH_FLAG )
done

echo ""
echo "Done. net.fortytwo.hydra.java:{$(IFS=,; echo "${INSTALL_SET[*]}")}:${VERSION} installed in ~/.m2."
echo ""
echo "The published-host java path is now active:"
echo "  bin/update-java-json.sh   (no --local-host needed)"

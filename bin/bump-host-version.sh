#!/usr/bin/env bash
# Set hydra.json:hostVersion — the version of the PUBLISHED HOSTS the build/sync
# depends on. This is the Merkle basis for the per-target generator stamp
# (bin/lib/assemble-common.sh:component_identity): bumping it invalidates every
# published host's stamp (and, via the kernel, every target's), so the next sync
# regenerates against the newly-preferred published host version.
#
# hostVersion is usually the newest published release, but not always — if a
# release is buggy for a given host you depend on an earlier good one. The global
# value is set here; per-host exceptions live in hydra.json:hostOverrides
# and are hand-edited (uncommon — a single bad host in an otherwise good release).
#
# This is DISTINCT from bin/bump-version.sh, which bumps the package *release*
# version (currentVersion) and fans it out to every host's build files. This
# script touches ONLY hostVersion. (Once #370 wires the build to actually consume
# published hosts, this script also gains the host-dependency-coordinate rewrites —
# Stack extra-deps, Gradle/pip host deps — which do not exist yet.)
#
# Usage:
#   bin/bump-host-version.sh          # Print the current hostVersion
#   bin/bump-host-version.sh 0.16.0   # Set hostVersion to 0.16.0
#   bin/bump-host-version.sh --help

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
REPO_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"

source "$REPO_ROOT/bin/lib/common.sh"

PACKAGES_PY="$REPO_ROOT/bin/lib/hydra-packages.py"

case "${1:-}" in
    --help|-h)
        sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
        exit 0
        ;;
esac

if [ $# -eq 0 ]; then
    # No argument: report the current hostVersion (via the kernel, always a host).
    echo "hostVersion: $("$PACKAGES_PY" host-version hydra-kernel)"
    exit 0
fi

if ! echo "$1" | grep -qE '^[0-9]+\.[0-9]+\.[0-9]+$'; then
    die "'$1' does not look like a version (expected X.Y.Z)"
fi

"$PACKAGES_PY" set-host-version "$1"
echo "Set hostVersion to $1"
echo "(Per-host overrides in hydra.json:hostOverrides are unchanged.)"

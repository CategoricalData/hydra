#!/usr/bin/env bash
# #370: prepare (idempotently) a venv containing the PUBLISHED Python host
# net.fortytwo / hydra-python==<hostVersion> from PyPI, and print its
# site-packages directory on stdout. Sourced/called by
# bin/generate-hydra-python-from-python.sh's --published-host path so the Python
# DSL→JSON driver imports hydra.codegen / hydra.generation-runtime / hydra.dsl.python
# from the published wheels instead of a local dist/python build.
#
# The published hydra-python wheel Requires-Dist hydra-kernel==<same>, so a single
# install brings the whole runtime classpath. The version is resolved from
# hydra.json (hostOverrides["python"] -> hostVersion) via
# bin/lib/hydra-packages.py, the single source of truth shared with the Java path.
#
# Usage:
#   SITE=$(bin/lib/python-published-host.sh)            # ensure + print site-packages
#   bin/lib/python-published-host.sh --version          # print resolved host version
#   bin/lib/python-published-host.sh --force            # recreate the venv
#
# The venv lives at heads/python/.venv-published-host/ (matches the .venv-* gitignore
# rule). It is keyed by version: a .host-version stamp records the installed version;
# a mismatch (hydra.json bumped) triggers a reinstall.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../.." && pwd )"

VENV_DIR="$HYDRA_ROOT/heads/python/.venv-published-host"
STAMP="$VENV_DIR/.host-version"

FORCE=0
PRINT_VERSION_ONLY=0
while [ $# -gt 0 ]; do
    case "$1" in
        --force) FORCE=1; shift ;;
        --version) PRINT_VERSION_ONLY=1; shift ;;
        *) echo "python-published-host.sh: unknown arg $1" >&2; exit 2 ;;
    esac
done

HOST_VERSION="$(python3 "$HYDRA_ROOT/bin/lib/hydra-packages.py" host-version hydra-python)"
if [ -z "$HOST_VERSION" ]; then
    echo "python-published-host.sh: could not resolve hydra-python host version" >&2
    exit 1
fi

if [ "$PRINT_VERSION_ONLY" = "1" ]; then
    echo "$HOST_VERSION"
    exit 0
fi

# Reinstall when forced, venv missing, or the stamped version differs from the
# version hydra.json now requests.
NEED_INSTALL=0
if [ "$FORCE" = "1" ] || [ ! -d "$VENV_DIR" ] || [ ! -f "$STAMP" ]; then
    NEED_INSTALL=1
elif [ "$(cat "$STAMP" 2>/dev/null)" != "$HOST_VERSION" ]; then
    NEED_INSTALL=1
fi

if [ "$NEED_INSTALL" = "1" ]; then
    echo "=== Preparing published Python host venv (hydra-python==$HOST_VERSION) ===" >&2
    rm -rf "$VENV_DIR"
    uv venv --quiet "$VENV_DIR" >&2
    # mavenLocal equivalent: a locally-published interim host (the bootstrap shim)
    # in the default index is picked up automatically by version pin.
    VIRTUAL_ENV="$VENV_DIR" uv pip install --quiet \
        "hydra-python==$HOST_VERSION" >&2
    echo "$HOST_VERSION" > "$STAMP"
fi

# Print the site-packages directory for PYTHONPATH assembly.
"$VENV_DIR/bin/python" -c "import site; print(site.getsitepackages()[0])"

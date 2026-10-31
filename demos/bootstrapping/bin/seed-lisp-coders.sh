#!/usr/bin/env bash
# Seed the target-coder packages into dist/<dialect>/hydra-kernel for a Lisp-host bootstrap cell.
#
# Usage: seed-lisp-coders.sh <clojure|common-lisp|scheme|emacs-lisp>
#
# The Lisp host drivers load the coders they run (hydra.python.coder etc.) from
# dist/<dialect>/hydra-kernel/src/main/<dialect>/hydra/<lang>/, but a normal sync does not assemble
# coder packages for Lisp targets. This generates them via the Java host's per-package transform
# driver (heads/java/bin/transform-json-to-target.sh) -- Haskell-free, so a Lisp-host cell no longer
# needs a cold 930-module GHC build (and the shared stack slot) just to obtain its coders. The coders
# are translingual, so the Java driver's output is the same code the Haskell bootstrap-from-json
# --include-coders path produced. (#769; same class as #773/#787.)
set -euo pipefail

DIALECT="${1:?usage: $0 <clojure|common-lisp|scheme|emacs-lisp>}"
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../.." && pwd )"
DEST="$HYDRA_ROOT/dist/$DIALECT/hydra-kernel/src/main/$DIALECT/hydra"

# hydra-haskell supplies the ->haskell coder; the rest mirror bootstrap-from-json's coderPackages
# (minus hydra-go: the Lisp drivers have no go target).
PACKAGES="hydra-haskell hydra-jvm hydra-java hydra-python hydra-scala hydra-lisp hydra-typescript"

WORK="$(mktemp -d "${TMPDIR:-/tmp}/seed-lisp-coders.XXXXXX")"
trap 'rm -rf "$WORK"' EXIT

mkdir -p "$DEST"
for pkg in $PACKAGES; do
    # Prefer the coder package a sync already assembled (dist/<dialect>/<pkg>/), so the host runs the
    # same generated coder the rest of dist/ came from; generate it only when absent.
    assembled="$HYDRA_ROOT/dist/$DIALECT/$pkg/src/main/$DIALECT/hydra"
    if [ -d "$assembled" ]; then
        echo "  Seeding $pkg -> $DIALECT (from assembled dist/$DIALECT/$pkg)..."
        cp -R "$assembled/." "$DEST/"
        continue
    fi
    echo "  Seeding $pkg -> $DIALECT (Java host)..."
    "$HYDRA_ROOT/heads/java/bin/transform-json-to-target.sh" "$DIALECT" "$pkg" main \
        --output "$WORK" --dist-json-root "$HYDRA_ROOT/dist/json"
    src="$WORK/$pkg/src/main/$DIALECT/hydra"
    [ -d "$src" ] || { echo "ERROR: no $DIALECT output for $pkg under $src" >&2; exit 1; }
    cp -R "$src/." "$DEST/"
done
echo "  Coder modules seeded into $DEST"

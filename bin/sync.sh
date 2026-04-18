#!/usr/bin/env bash
# Layer 3 orchestrator: bring dist/ into sync with packages/ and heads/.
#
# Usage:
#   bin/sync.sh                              # all packages, all targets
#   bin/sync.sh <pkg>                        # one package, all targets
#   bin/sync.sh <pkg> --target <lang>        # one package, one target
#   bin/sync.sh --target <lang>              # all packages, one target
#   bin/sync.sh --from <pkg>                 # <pkg> + reverse-dep closure
#   bin/sync.sh --list                       # print packages and exit
#   bin/sync.sh --no-test                    # skip Layer 2.5 testers
#   bin/sync.sh --help
#
# Targets: haskell, java, python, scala, clojure, scheme, common-lisp,
#          emacs-lisp. Default: all.
#
# This orchestrator replaces sync-all.sh and the per-target sync-*.sh
# scripts. Work is dispatched through the three-layer stack:
#
#   Layer 1 (transforms) — bin wrappers in heads/haskell/bin/transform-*.sh
#   Layer 2 (assemblers)  — bin wrappers in heads/<lang>/bin/assemble-*.sh
#   Layer 2.5 (testers)   — bin wrappers in heads/<lang>/bin/test-*.sh
#
# See feature_290_packaging-plan.md, "Sync system redesign", for the full
# layered design rationale.

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"

# Source the common.sh helpers if available (for banner, step, etc.).
if [ -f "$HYDRA_ROOT_DIR/bin/lib/common.sh" ]; then
    source "$HYDRA_ROOT_DIR/bin/lib/common.sh"
fi

# -----------------------------------------------------------------------
# Package and target configuration
# -----------------------------------------------------------------------

# Packages that go through the JSON pipeline (i.e., have Layer 1 transforms).
# All packages are now handled uniformly; no package-specific carveouts.
ALL_PACKAGES="hydra-kernel hydra-haskell hydra-java hydra-python hydra-scala hydra-lisp hydra-pg hydra-rdf hydra-coq hydra-javascript hydra-ext"

# Valid target languages.
ALL_TARGETS="haskell java python scala clojure scheme common-lisp emacs-lisp"

# -----------------------------------------------------------------------
# Argument parsing
# -----------------------------------------------------------------------

PACKAGES=""          # explicit package arg (positional); empty means "all"
FROM_PACKAGE=""      # --from <pkg>
TARGETS=""           # --target <lang>; empty means "all"
NO_TEST=false
LIST_ONLY=false

while [ $# -gt 0 ]; do
    case "$1" in
        --target) TARGETS="${TARGETS}${TARGETS:+ }$2"; shift 2 ;;
        --target=*) TARGETS="${TARGETS}${TARGETS:+ }${1#--target=}"; shift ;;
        --from) FROM_PACKAGE="$2"; shift 2 ;;
        --from=*) FROM_PACKAGE="${1#--from=}"; shift ;;
        --no-test) NO_TEST=true; shift ;;
        --list) LIST_ONLY=true; shift ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# //;s/^#$//'
            exit 0
            ;;
        --*)
            echo "Error: unknown flag '$1'" >&2
            echo "Try: $0 --help" >&2
            exit 1
            ;;
        *)
            PACKAGES="${PACKAGES}${PACKAGES:+ }$1"
            shift
            ;;
    esac
done

# -----------------------------------------------------------------------
# Dependency resolution
# -----------------------------------------------------------------------

# Read a package's dependencies from its package.json.
# Returns space-separated list, or empty if none.
pkg_deps() {
    local pkg="$1"
    local pkg_json="$HYDRA_ROOT_DIR/packages/$pkg/package.json"
    if [ ! -f "$pkg_json" ]; then
        echo ""
        return
    fi
    # Extract the dependencies array; one dep per line, stripped of quotes.
    python3 -c "
import json, sys
try:
    with open('$pkg_json') as f:
        m = json.load(f)
    deps = m.get('dependencies', [])
    print(' '.join(deps))
except Exception as e:
    print('', file=sys.stderr)
" 2>/dev/null
}

# Topologically sort the given list of packages by dependency order.
# Uses a simple DFS-based topo sort.
topo_sort() {
    python3 <<EOF
import json, os, sys

packages = "$1".split()
root = "$HYDRA_ROOT_DIR"

def load_deps(pkg):
    path = os.path.join(root, "packages", pkg, "package.json")
    if not os.path.exists(path):
        return []
    with open(path) as f:
        return json.load(f).get("dependencies", [])

deps = {pkg: load_deps(pkg) for pkg in packages}

# DFS topo sort
visited = set()
order = []
def visit(pkg):
    if pkg in visited:
        return
    visited.add(pkg)
    for d in deps.get(pkg, []):
        if d in deps:
            visit(d)
    order.append(pkg)

for pkg in packages:
    visit(pkg)

print(" ".join(order))
EOF
}

# Compute reverse-dependency closure: all packages that transitively depend on <root_pkg>.
reverse_dep_closure() {
    local root_pkg="$1"
    python3 <<EOF
import json, os

root_pkg = "$root_pkg"
root = "$HYDRA_ROOT_DIR"
all_packages = "$ALL_PACKAGES".split()

def load_deps(pkg):
    path = os.path.join(root, "packages", pkg, "package.json")
    if not os.path.exists(path):
        return []
    with open(path) as f:
        return json.load(f).get("dependencies", [])

deps = {pkg: load_deps(pkg) for pkg in all_packages}

# Build reverse-dep map: for each package, who depends on it?
rdeps = {p: [] for p in all_packages}
for p, ds in deps.items():
    for d in ds:
        if d in rdeps:
            rdeps[d].append(p)

# BFS from root_pkg
closure = {root_pkg}
frontier = [root_pkg]
while frontier:
    next_frontier = []
    for p in frontier:
        for dependent in rdeps.get(p, []):
            if dependent not in closure:
                closure.add(dependent)
                next_frontier.append(dependent)
    frontier = next_frontier

print(" ".join(sorted(closure)))
EOF
}

# -----------------------------------------------------------------------
# Environment validation
# -----------------------------------------------------------------------

validate_environment() {
    local targets="$1"
    local errors=""

    for tgt in $targets; do
        case "$tgt" in
            haskell)
                if ! command -v stack >/dev/null 2>&1; then
                    errors="${errors}  - stack not found (required for Haskell target)\n"
                fi
                ;;
            java)
                # Prefer heads/haskell/bin/common.sh's check_native_jdk if available;
                # otherwise, do a minimal check.
                if ! command -v java >/dev/null 2>&1; then
                    errors="${errors}  - java not found (required for Java target)\n"
                fi
                ;;
            python)
                if ! command -v uv >/dev/null 2>&1 && ! command -v python3 >/dev/null 2>&1; then
                    errors="${errors}  - uv or python3 not found (required for Python target)\n"
                fi
                ;;
            scala)
                if ! command -v sbt >/dev/null 2>&1; then
                    errors="${errors}  - sbt not found (required for Scala target)\n"
                fi
                ;;
            clojure)
                if ! command -v clojure >/dev/null 2>&1; then
                    errors="${errors}  - clojure not found (required for Clojure target)\n"
                fi
                ;;
            scheme)
                if ! command -v guile >/dev/null 2>&1; then
                    errors="${errors}  - guile not found (required for Scheme target)\n"
                fi
                ;;
            common-lisp)
                if ! command -v sbcl >/dev/null 2>&1; then
                    errors="${errors}  - sbcl not found (required for Common Lisp target)\n"
                fi
                ;;
            emacs-lisp)
                if ! command -v emacs >/dev/null 2>&1; then
                    errors="${errors}  - emacs not found (required for Emacs Lisp target)\n"
                fi
                ;;
        esac
    done

    if [ -n "$errors" ]; then
        echo "Environment validation failed:" >&2
        printf "%b" "$errors" >&2
        return 1
    fi
    return 0
}

# -----------------------------------------------------------------------
# Pipeline dispatch
# -----------------------------------------------------------------------

# Map target language to its head directory.
head_dir_for_target() {
    case "$1" in
        haskell) echo "$HYDRA_ROOT_DIR/heads/haskell" ;;
        java) echo "$HYDRA_ROOT_DIR/heads/java" ;;
        python) echo "$HYDRA_ROOT_DIR/heads/python" ;;
        scala) echo "$HYDRA_ROOT_DIR/heads/scala" ;;
        clojure|scheme|common-lisp|emacs-lisp) echo "$HYDRA_ROOT_DIR/heads/lisp" ;;
        *) echo "" ;;
    esac
}

# Invoke Layer 2 assembler. For Lisp, pass the dialect as a second arg.
invoke_assembler() {
    local pkg="$1"
    local target="$2"
    local head_dir
    head_dir=$(head_dir_for_target "$target")
    local script="$head_dir/bin/assemble-distribution.sh"
    if [ ! -x "$script" ]; then
        echo "  [skip] $target assembler not available at $script" >&2
        return 0
    fi
    case "$target" in
        clojure|scheme|common-lisp|emacs-lisp)
            "$script" "$pkg" "$target"
            ;;
        *)
            "$script" "$pkg"
            ;;
    esac
}

# Invoke Layer 2.5 tester.
invoke_tester() {
    local pkg="$1"
    local target="$2"
    local head_dir
    head_dir=$(head_dir_for_target "$target")
    local script="$head_dir/bin/test-distribution.sh"
    if [ ! -x "$script" ]; then
        echo "  [skip] $target tester not available at $script" >&2
        return 0
    fi
    case "$target" in
        clojure|scheme|common-lisp|emacs-lisp)
            "$script" "$pkg" "$target"
            ;;
        *)
            "$script" "$pkg"
            ;;
    esac
}

# -----------------------------------------------------------------------
# Main
# -----------------------------------------------------------------------

# Resolve the effective package list.
if [ -n "$FROM_PACKAGE" ]; then
    if [ -n "$PACKAGES" ]; then
        echo "Error: --from and a positional package argument are mutually exclusive" >&2
        exit 1
    fi
    EFFECTIVE_PACKAGES=$(reverse_dep_closure "$FROM_PACKAGE")
elif [ -n "$PACKAGES" ]; then
    EFFECTIVE_PACKAGES="$PACKAGES"
else
    EFFECTIVE_PACKAGES="$ALL_PACKAGES"
fi

# Sort in dependency order (deps first).
EFFECTIVE_PACKAGES=$(topo_sort "$EFFECTIVE_PACKAGES")

# Resolve the effective target list.
if [ -n "$TARGETS" ]; then
    # Validate each target is in ALL_TARGETS.
    for t in $TARGETS; do
        if ! echo " $ALL_TARGETS " | grep -q " $t "; then
            echo "Error: unknown target '$t'" >&2
            echo "Valid targets: $ALL_TARGETS" >&2
            exit 1
        fi
    done
    EFFECTIVE_TARGETS="$TARGETS"
else
    EFFECTIVE_TARGETS="$ALL_TARGETS"
fi

if [ "$LIST_ONLY" = true ]; then
    echo "Packages (in dependency order):"
    for pkg in $EFFECTIVE_PACKAGES; do
        deps=$(pkg_deps "$pkg")
        if [ -n "$deps" ]; then
            echo "  $pkg -> $deps"
        else
            echo "  $pkg"
        fi
    done
    echo ""
    echo "Targets: $EFFECTIVE_TARGETS"
    exit 0
fi

# Environment validation — fail fast before any long-running work.
if ! validate_environment "$EFFECTIVE_TARGETS"; then
    exit 1
fi

# Banner.
echo "=========================================="
echo "Hydra sync"
echo "=========================================="
echo "  Packages: $EFFECTIVE_PACKAGES"
echo "  Targets:  $EFFECTIVE_TARGETS"
echo "  Tests:    $([ "$NO_TEST" = true ] && echo "skipped (--no-test)" || echo "enabled")"
echo "=========================================="
echo ""

# Phase 1: JSON sources. Run transform-haskell-dsl-to-json for each package,
# in dependency order. This is shared by every target language.
HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "[Phase 1] Regenerating JSON sources for each package..."
for pkg in $EFFECTIVE_PACKAGES; do
    "$HASKELL_BIN/transform-haskell-dsl-to-json.sh" "$pkg" main
    "$HASKELL_BIN/transform-haskell-dsl-to-json.sh" "$pkg" test
done
# Also regenerate per-package manifests so readers see the updated module list.
cd "$HYDRA_ROOT_DIR/heads/haskell"
stack build hydra:exe:update-json-manifest >/dev/null 2>&1
stack exec update-json-manifest >/dev/null
cd "$HYDRA_ROOT_DIR"
echo ""

# Phase 2: Assemblers. For each (package, target), produce the distribution.
echo "[Phase 2] Assembling distributions..."
for target in $EFFECTIVE_TARGETS; do
    for pkg in $EFFECTIVE_PACKAGES; do
        echo ""
        echo "--- $pkg -> $target ---"
        invoke_assembler "$pkg" "$target" || {
            echo "ERROR: assembly failed for $pkg / $target" >&2
            exit 1
        }
    done
done
echo ""

# Phase 3: Testers (unless --no-test).
if [ "$NO_TEST" = true ]; then
    echo "[Phase 3] Skipped (--no-test)."
else
    echo "[Phase 3] Running tests..."
    for target in $EFFECTIVE_TARGETS; do
        # Today the testers are monolithic (they run every package's tests as a
        # single target-language test suite), so we only need to invoke the
        # tester once per target, not once per (package, target). We use the
        # last package in dep order as the nominal "test subject" since that
        # minimizes re-running equivalent work.
        last_pkg=$(echo "$EFFECTIVE_PACKAGES" | awk '{print $NF}')
        echo ""
        echo "--- tests for $target (via $last_pkg) ---"
        invoke_tester "$last_pkg" "$target" || {
            echo "WARNING: tests had errors for $target (see log above)" >&2
        }
    done
fi

echo ""
echo "=========================================="
echo "Hydra sync: DONE"
echo "=========================================="

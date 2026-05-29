#!/usr/bin/env bash
# Top-level synchronization script for Hydra.
#
# Computes the (package, target) sync matrix needed to bootstrap from a
# given set of hosts into a given set of targets, then regenerates each
# entry. A sync run with flags `--hosts X --targets Y` produces the
# dist/ state that `run-bootstrapping-demo.sh --hosts X --targets Y`
# consumes; the flag names match so the two commands compose. Note
# that the flag semantics are inverted between the two scripts:
# sync.sh's --hosts names languages whose host capability is to be
# *built* (Phase 4 emits each target's coder into host's language),
# while bootstrap's --hosts names existing hosts to *use* as
# generators. Likewise, sync.sh's --targets names languages to emit
# into (an output), while bootstrap's --targets names targets to
# generate into during the demo (an input expectation about dist/).
#
# Usage:
#   bin/sync.sh                                  # all × all (every language)
#   bin/sync.sh --hosts H1,H2 --targets T1,T2    # Cartesian subset
#   bin/sync.sh --hosts H1,H2                    # targets mirror hosts
#   bin/sync.sh --targets T1,T2                  # hosts mirror targets
#   bin/sync.sh --no-tests                       # skip target-lang tests
#   bin/sync.sh --help
#
# For the common "bootstrapping triad" default (haskell, java, python),
# use bin/sync-default.sh, which is a thin wrapper around this script.
#
# Languages for --hosts / --targets:
#   haskell, java, python, scala, go, clojure, scheme, common-lisp, emacs-lisp.
#   Aliases: 'all' expands to all nine; 'lisp' expands to the four Lisp
#   dialects (clojure,common-lisp,emacs-lisp,scheme). Aliases can mix
#   with explicit names, e.g. 'java,lisp'.
#
# Go is a "head bud" today: it generates as a target (kernel only) but
# cannot yet host generation of any coder package in Go (Phase 4 host=go
# rows are skipped). See issue #289.
#
# Sync matrix (derived from hosts + targets):
#
#   Every language L in (hosts ∪ targets) gets:
#     (hydra-kernel, L)                -- kernel for that language
#
#   Every language L in (hosts ∪ targets) gets:
#     (hydra-L, haskell)               -- coder for L, in Haskell
#                                         (because the Haskell head drives
#                                         every downstream generation)
#
#   Every (host, target) pair with host ≠ haskell gets:
#     (hydra-target, host)             -- target's coder, in host's language
#                                         (so the host can emit target code)
#
# Packages NOT in the matrix: hydra-coq, hydra-wasm, hydra-ext, hydra-pg,
# hydra-rdf. These are extensions, not bootstrapping dependencies.
# Generate them on demand via bin/sync-packages.sh:
#
#   bin/sync-packages.sh hydra-pg                       # pg into every target
#   bin/sync-packages.sh hydra-pg --targets haskell     # pg into haskell only

set -euo pipefail

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_HASKELL_DIR="$HYDRA_ROOT/heads/haskell"

source "$HYDRA_ROOT/bin/lib/common.sh"

ALL_LANGS="haskell java python scala go typescript clojure scheme common-lisp emacs-lisp"

NO_TESTS=false
HOSTS_ARG=""
TARGETS_ARG=""

while [ $# -gt 0 ]; do
    case "$1" in
        --no-tests)
            NO_TESTS=true
            ;;
        --hosts)
            HOSTS_ARG="$2"
            shift
            ;;
        --hosts=*)
            HOSTS_ARG="${1#--hosts=}"
            ;;
        --targets)
            TARGETS_ARG="$2"
            shift
            ;;
        --targets=*)
            TARGETS_ARG="${1#--targets=}"
            ;;
        --help|-h)
            sed -n '2,/^$/p' "$0" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            die "Unknown argument: $1 (try --help)"
            ;;
    esac
    shift
done

# Expand the comma-separated lang list, with two aliases:
#   'all'  -> every supported language
#   'lisp' -> all four Lisp dialects (clojure,common-lisp,emacs-lisp,scheme)
# Aliases can mix with explicit names: 'java,lisp' expands to
# 'java clojure common-lisp emacs-lisp scheme'. Duplicates are tolerated;
# the caller dedups via 'sort -u' as needed.
expand_langs() {
    local input="$1"
    if [ -z "$input" ]; then
        echo "$ALL_LANGS"
        return
    fi
    local out=""
    local item
    for item in $(echo "$input" | tr ',' ' '); do
        case "$item" in
            all)  out="$out $ALL_LANGS" ;;
            lisp) out="$out clojure common-lisp emacs-lisp scheme" ;;
            *)    out="$out $item" ;;
        esac
    done
    echo "$out" | xargs
}

# Default: if neither --hosts nor --targets given, both = all (symmetric).
# If only --hosts given, --targets mirrors it.
# If only --targets given, --hosts mirrors it.
if [ -z "$HOSTS_ARG" ] && [ -z "$TARGETS_ARG" ]; then
    HOSTS_ARG="all"
    TARGETS_ARG="all"
elif [ -z "$TARGETS_ARG" ]; then
    TARGETS_ARG="$HOSTS_ARG"
elif [ -z "$HOSTS_ARG" ]; then
    HOSTS_ARG="$TARGETS_ARG"
fi

HOSTS=$(expand_langs "$HOSTS_ARG")
TARGETS=$(expand_langs "$TARGETS_ARG")

validate_lang() {
    local l="$1"
    for valid in $ALL_LANGS; do
        if [ "$l" = "$valid" ]; then return 0; fi
    done
    return 1
}

for l in $HOSTS $TARGETS; do
    if ! validate_lang "$l"; then
        die "Unknown language '$l'. Valid: $ALL_LANGS, all"
    fi
done

# (hosts ∪ targets) preserving the order in which languages first appear.
# We deliberately don't `sort -u` here — Java and Python are higher-value
# than Lisp, so when --targets all expands, we want them assembled (and
# tested) before the Lisp dialects so failures surface earlier.
LANG_UNION=$(printf '%s\n' $HOSTS $TARGETS | awk '!seen[$0]++' | xargs)

# Ensure JAVA_HOME is set to a native arm64 JDK 19 if any host or target is java.
need_java=false
for l in $LANG_UNION; do
    if [ "$l" = "java" ]; then need_java=true; break; fi
done
if [ "$need_java" = "true" ]; then
    if [ -z "${JAVA_HOME:-}" ]; then
        if command -v /usr/libexec/java_home &>/dev/null; then
            export JAVA_HOME="$(/usr/libexec/java_home 2>/dev/null || true)"
        fi
    fi
    if [ -z "${JAVA_HOME:-}" ]; then
        echo "ERROR: JAVA_HOME is not set. Java compilation requires a JDK 19 install." >&2
        echo "Set JAVA_HOME to a native arm64 JDK 19 and retry." >&2
        exit 1
    fi
    echo "Using JAVA_HOME=$JAVA_HOME"
    check_native_jdk
fi

START_TIME=$SECONDS

print_elapsed() {
    ELAPSED=$((SECONDS - START_TIME))
    MINUTES=$((ELAPSED / 60))
    SECS=$((ELAPSED % 60))
    echo ""
    echo "Total elapsed time: ${MINUTES}m ${SECS}s"
}
trap print_elapsed EXIT

NO_TESTS_FLAG=""
if [ "$NO_TESTS" = true ]; then
    NO_TESTS_FLAG="--no-tests"
fi

banner1 "Hydra sync-all"
echo "  Hosts:   $HOSTS"
echo "  Targets: $TARGETS"
echo "  Union:   $LANG_UNION"
echo ""

# ────────────────────────────────────────────────────────────────────
# Phase 0: Ensure essential Haskell executables are built.
# ────────────────────────────────────────────────────────────────────
# Every downstream phase invokes these via `stack exec`. The phase1-fresh
# cache below can skip sync-haskell.sh entirely on warm runs, so we
# cannot rely on sync-haskell.sh's own Step 1 build. On a fresh CI
# checkout with .stack-work/ empty, skipping sync-haskell.sh means no
# executables — and every subsequent `stack exec` fails.
#
# This stack build is a no-op on warm local trees (seconds to confirm)
# and one-time cost on cold CI (amortized by actions/cache on ~/.stack).
# Unconditional ensures correctness.

(cd "$HYDRA_HASKELL_DIR" && stack build \
    hydra:exe:update-json-main \
    hydra:exe:update-json-test \
    hydra:exe:update-json-manifest \
    hydra:exe:update-json-kernel \
    hydra:exe:verify-json-kernel \
    hydra:exe:bootstrap-from-json \
    hydra:exe:digest-check) || exit 1
echo ""

# ────────────────────────────────────────────────────────────────────
# Phase 1: DSL → JSON + Haskell kernel / hydra-haskell regen + lexicon.
# ────────────────────────────────────────────────────────────────────
# This refreshes dist/json/** (the source of truth consumed by every
# downstream phase) and dist/haskell/{hydra-kernel,hydra-haskell}/.
# It also runs stack test (unless --no-tests) and regenerates the lexicon.
#
# Shell-level shortcut: if every DSL source under packages/ matches
# its recorded hash in dist/json/build/digest.json, skip Phase 1
# entirely. This is sub-second and avoids ~30+ seconds of Haskell
# startup when nothing has changed.
#
# When --no-tests is NOT set, we still run Phase 1 unconditionally so
# 'stack test' executes — the JSON-side cache is independent of
# whether the test suite has been run since last invocation.

PHASE1_FRESH_CHECK="$HYDRA_ROOT/bin/lib/check-phase1-fresh.py"

# #344: native generators own dist/json/hydra-{java,python}/ in normal
# operation, so Phase 1 (Haskell DSL → JSON) skips those namespaces by
# default. On a cold tree where the JSON is missing, do a one-time
# Haskell-DSL bootstrap so Phases 3/4 have something to read; native
# generators then take over from Phase 5 onward.
JAVA_JSON_SENTINEL="$HYDRA_ROOT/dist/json/hydra-java/src/main/json/hydra/java/coder.json"
PYTHON_JSON_SENTINEL="$HYDRA_ROOT/dist/json/hydra-python/src/main/json/hydra/python/coder.json"
if [ ! -f "$JAVA_JSON_SENTINEL" ] || [ ! -f "$PYTHON_JSON_SENTINEL" ]; then
    export HYDRA_INCLUDE_JAVA_PYTHON=1
    echo ""
    echo "Cold-start detected (missing hydra-java/hydra-python JSON);"
    echo "Phase 1 will run with --include-java-python to seed the bootstrap."
    echo ""
fi

# Single-tier short-circuit. The phase1-fresh check covers every input
# that affects Phase 1's outputs (DSL sources + heads/haskell runtime,
# test infra, exec/, stack config). The recorded hash is stamped only
# after a fully-green sync-haskell.sh run, so a hit is safe regardless
# of --no-tests; a miss (including the no-cache fresh-checkout case)
# falls through to a real Phase 1 run.
if [ -x "$PHASE1_FRESH_CHECK" ] \
   && "$PHASE1_FRESH_CHECK" "$HYDRA_ROOT" >/dev/null 2>&1; then
    banner1 "Phase 1: DSL → JSON + Haskell kernel (skipped — every input clean)"
    echo ""
else
    banner1 "Phase 1: DSL → JSON + Haskell kernel"
    echo ""
    "$HYDRA_HASKELL_DIR/bin/sync-haskell.sh" $NO_TESTS_FLAG
fi

# ────────────────────────────────────────────────────────────────────
# Phase 2: Each coder (hydra-<L> for L ∈ union) regenerated in Haskell.
# ────────────────────────────────────────────────────────────────────
# Every host in any future phase drives target-language code generation
# through the Haskell head, so each needed coder must exist under
# dist/haskell/hydra-<L>/. hydra-haskell was done in Phase 1.

banner1 "Phase 2: Coder Haskell dists for (hosts ∪ targets)"
echo ""
for L in $LANG_UNION; do
    # hydra-haskell is already regenerated by Phase 1.
    if [ "$L" = "haskell" ]; then continue; fi
    # Map language name to package name.
    case "$L" in
        java|python|scala|go|typescript)  pkg="hydra-$L" ;;
        clojure|scheme|common-lisp|emacs-lisp)  pkg="hydra-lisp" ;;
        *)                  die "Internal: no coder package for $L" ;;
    esac
    echo ""
    echo "--- $pkg (Haskell) ---"
    "$HYDRA_HASKELL_DIR/bin/assemble-distribution.sh" "$pkg"
done

# ────────────────────────────────────────────────────────────────────
# Phase 3: hydra-kernel + hydra-pg + hydra-rdf into every language.
# ────────────────────────────────────────────────────────────────────
# Haskell kernel is already done by Phase 1. We dispatch through each
# target's assemble-distribution.sh (Layer 2), which calls Layer 1 for
# both main and test source sets and applies any per-target post-
# processing (e.g. Java/Python TestGraph patches, Lisp Coder.java
# PartialVisitor fix). Calling Layer 1 directly here would skip those
# patches and break tests for the affected combinations.
#
# hydra-pg and hydra-rdf are independent packages in 0.15 (they were
# bundled into hydra-java/hydra-python via --include-ext in 0.14).
# Hand-written host runtime under heads/java/src/main/java/hydra/rdf/
# imports hydra.rdf.syntax.*, so dist/java/hydra-rdf/ must be populated
# before the Java host compiles.

banner1 "Phase 3: hydra-kernel + hydra-pg + hydra-rdf into each language"
echo ""
for L in $LANG_UNION; do
    if [ "$L" = "haskell" ]; then continue; fi
    # The Go and TypeScript heads are "head buds": only hydra-kernel is
    # meaningful for them today. Skip hydra-pg / hydra-rdf into them
    # until corresponding runtime bindings exist (issue #289 for Go,
    # #126 for TypeScript).
    if [ "$L" = "go" ] || [ "$L" = "typescript" ]; then
        for pkg in hydra-kernel; do
            echo ""
            echo "--- $pkg -> $L ---"
            "$HYDRA_ROOT/heads/$L/bin/assemble-distribution.sh" "$pkg"
        done
        continue
    fi
    for pkg in hydra-kernel hydra-pg hydra-rdf; do
        echo ""
        echo "--- $pkg -> $L ---"
        case "$L" in
            java|python|scala)
                "$HYDRA_ROOT/heads/$L/bin/assemble-distribution.sh" "$pkg"
                ;;
            clojure|scheme|common-lisp|emacs-lisp)
                "$HYDRA_ROOT/heads/lisp/bin/assemble-distribution.sh" "$pkg" "$L"
                ;;
            *)
                die "Internal: no assembler for $L"
                ;;
        esac
    done
done

# ────────────────────────────────────────────────────────────────────
# Phase 4: Each (host, target) pair with host ≠ haskell.
# ────────────────────────────────────────────────────────────────────
# (host, target) ≠ haskell means host needs target's coder in host's
# own language. E.g., Python host bootstrapping to Scheme needs
# dist/python/hydra-lisp/ — the Lisp coder in Python.

banner1 "Phase 4: Cross-host coders (hydra-<target> in host's language)"
echo ""
for H in $HOSTS; do
    if [ "$H" = "haskell" ]; then continue; fi
    # Go and TypeScript heads are "head buds": they cannot yet host
    # generation of any coder package in their own language (no DSL
    # infrastructure). Skip the host=go and host=typescript rows of
    # Phase 4 until promotion is complete.
    if [ "$H" = "go" ] || [ "$H" = "typescript" ]; then continue; fi
    for T in $TARGETS; do
        # Map target language to its coder package.
        case "$T" in
            haskell)             pkg="hydra-haskell" ;;
            java|python|scala|go|typescript)   pkg="hydra-$T" ;;
            clojure|scheme|common-lisp|emacs-lisp)  pkg="hydra-lisp" ;;
            *)                   die "Internal: no coder package for $T" ;;
        esac
        echo ""
        echo "--- $pkg -> $H ---"
        # Dispatch to host H's assembler so post-processing patches apply.
        case "$H" in
            java|python|scala)
                "$HYDRA_ROOT/heads/$H/bin/assemble-distribution.sh" "$pkg"
                ;;
            clojure|scheme|common-lisp|emacs-lisp)
                "$HYDRA_ROOT/heads/lisp/bin/assemble-distribution.sh" "$pkg" "$H"
                ;;
            *)
                die "Internal: no assembler for $H"
                ;;
        esac
    done
done

# ────────────────────────────────────────────────────────────────────
# Phase 5: Native DSL→JSON for hydra-java and hydra-python (#344).
# ────────────────────────────────────────────────────────────────────
# Re-generate dist/json/hydra-java/ from the Java DSL sources, and
# dist/json/hydra-python/ from the Python DSL sources. The native
# generators are the authoritative DSL→JSON path for these two
# packages; the Haskell-DSL copies in packages/hydra-{java,python}/
# src/main/haskell/ remain a legacy bootstrap fallback (to be removed
# before 0.16). Phase 1 already wrote a Haskell-DSL-emitted version of
# these JSON trees; this phase overwrites them with the native output.
#
# Diff diagnostic: we snapshot the Haskell-DSL output, run the native
# generator, then report the number of files that differ (without
# failing). Byte-equality is no longer required — the two DSL paths
# can drift as long as their generated target code passes tests.
#
# Skip when the relevant host is not in HOSTS (the native generator
# needs the host's coder package compiled, which is only built when
# that language is a host).

# Sentinels indicating each native host is fully built (i.e. has the host's
# hydra-<lang> dist with the DSL meta-modules a self-host run needs).
JAVA_HOST_SENTINEL="$HYDRA_ROOT/dist/java/hydra-java/src/main/java/hydra/dsl/java/Syntax.java"
PYTHON_HOST_SENTINEL="$HYDRA_ROOT/dist/python/hydra-python/src/main/python/hydra/dsl/python/syntax.py"

native_generate_and_report() {
    local lang="$1"        # java | python
    local script="$2"      # bin/generate-hydra-<lang>-from-<lang>.sh
    local sentinel="$3"    # host-built sentinel file
    shift 3
    local extra=("$@")     # extra args (e.g. --pypy)
    local json_dir="$HYDRA_ROOT/dist/json/hydra-$lang/src/main/json"

    if [ ! -f "$sentinel" ]; then
        echo "  skipping: native $lang host not built (missing $sentinel)."
        echo "  hydra-$lang JSON remains as written by Phase 1 (Haskell DSL bootstrap)."
        return 0
    fi
    if [ ! -d "$json_dir" ]; then
        echo "  skipping: $json_dir not present"
        return 0
    fi

    # Phase 5 freshness gate: skip the demo entirely when every input is
    # byte-identical to the last successful run. The demo's outputs are a
    # pure function of the hashed inputs, so a hash match guarantees the
    # current dist/json/hydra-<lang>/ is correct. Set HYDRA_FORCE_PHASE5=1
    # to bypass (e.g. to validate the cached output really is reproducible).
    if [ "${HYDRA_FORCE_PHASE5:-0}" != "1" ]; then
        if python3 "$HYDRA_ROOT/bin/lib/check-phase5-fresh.py" "$HYDRA_ROOT" "$lang"; then
            return 0
        fi
    fi

    local snap_dir; snap_dir=$(mktemp -d)
    cp -R "$json_dir"/. "$snap_dir/"

    "$script" "${extra[@]+"${extra[@]}"}"

    local mismatched=0
    local total=0
    while IFS= read -r f; do
        total=$((total + 1))
        local rel="${f#$json_dir/}"
        if [ -f "$snap_dir/$rel" ] && ! cmp -s "$f" "$snap_dir/$rel"; then
            mismatched=$((mismatched + 1))
        fi
    done < <(find "$json_dir" -type f -name '*.json')

    rm -rf "$snap_dir"

    if [ "$mismatched" -gt 0 ]; then
        echo "  hydra-$lang: native output differs from snapshot on $mismatched/$total JSON files (native is authoritative)."
        # Native overwrote dist/json/hydra-<lang>/, so the downstream
        # dist/<lang>/hydra-<lang>/ is now derived from a stale JSON
        # version. Regenerate it from the native JSON so all downstream
        # state is consistent.
        echo "  hydra-$lang: regenerating dist/$lang/hydra-$lang from native JSON..."
        "$HYDRA_ROOT/heads/$lang/bin/assemble-distribution.sh" "hydra-$lang"
    else
        echo "  hydra-$lang: native output matches snapshot on all $total JSON files."
    fi

    # Record the input hash so the next sync can skip this demo when
    # nothing relevant has changed.
    python3 "$HYDRA_ROOT/bin/lib/check-phase5-fresh.py" "$HYDRA_ROOT" "$lang" --record
}

# Phase 5 is always run for hydra-java and hydra-python (#344): the native
# generators are the authoritative DSL→JSON path. Phase 1 only writes their
# JSON on a cold-start bootstrap (when no JSON exists). In any warm state,
# native is the only writer — drift from the legacy Haskell DSL is silently
# accepted because the Haskell DSL never runs.
banner1 "Phase 5: Native DSL→JSON for self-hosted coders (#344)"
echo ""
# HYDRA_IN_SYNC tells the generator scripts not to re-invoke sync.sh
# (which would recurse). Standalone invocations of those scripts run
# sync.sh themselves to ensure the cross-language dist trees their
# gradle compile imports from are populated.
export HYDRA_IN_SYNC=1
# Skip a language's native self-host pass entirely when that language
# is not in HOSTS — e.g. a TypeScript-only sync (--hosts typescript)
# has no reason to spin up Java's full kernel build or Python's pypy
# self-host demo, and on a tree where both heads were built previously
# the sentinel-based skip alone won't catch them.
if printf '%s\n' $HOSTS | grep -qx java; then
    echo "--- hydra-java (native Java DSL → JSON) ---"
    native_generate_and_report java \
        "$HYDRA_ROOT/bin/generate-hydra-java-from-java.sh" \
        "$JAVA_HOST_SENTINEL"
else
    echo "--- hydra-java (skipped: java not in HOSTS) ---"
fi
if printf '%s\n' $HOSTS | grep -qx python; then
    echo "--- hydra-python (native Python DSL → JSON) ---"
    # Use PyPy when available — ~4x faster than CPython.
    if command -v pypy3 >/dev/null 2>&1; then
        native_generate_and_report python \
            "$HYDRA_ROOT/bin/generate-hydra-python-from-python.sh" \
            "$PYTHON_HOST_SENTINEL" \
            --pypy
    else
        native_generate_and_report python \
            "$HYDRA_ROOT/bin/generate-hydra-python-from-python.sh" \
            "$PYTHON_HOST_SENTINEL"
    fi
else
    echo "--- hydra-python (skipped: python not in HOSTS) ---"
fi
unset HYDRA_IN_SYNC

banner1_done "Sync complete!"

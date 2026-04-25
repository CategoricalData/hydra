#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Clojure distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/clojure/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_CLOJURE_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_CLOJURE_HEAD/../../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/clojure"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/clojure"
OUT_TEST="$OUT_DIR/src/test/clojure"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Clojure distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

# Per-source-set freshness check via digest-check. See
# heads/java/bin/assemble-distribution.sh for the pattern; same shape
# across every target language.
source "$HYDRA_ROOT_DIR/bin/lib/assemble-common.sh"

# Step 1: Main modules.
if assemble_check_fresh "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"; then
    echo "Step 1: Main modules unchanged; skipping main regeneration."
else
    rm -f "$OUTPUT_DIGEST_MAIN"
    echo "Step 1: Generating main Clojure modules..."
    "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" clojure main \
        --output "$DIST_ROOT"
    assemble_refresh_digest "$INPUT_DIGEST_MAIN" "$OUT_MAIN" "$OUTPUT_DIGEST_MAIN"
fi

# Step 2: Test modules. Any package can have a test source set (just
# `dist/json/<pkg>/src/test/json/`); the per-source-set digest mechanism
# is uniform — adding a test dir for any package automatically wires it
# into the build.
echo ""
if [ ! -d "$TEST_JSON_DIR" ]; then
    echo "Step 2: No test sources for $PACKAGE; skipping."
else
    if assemble_check_fresh "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"; then
        echo "Step 2: Test modules unchanged; skipping test regeneration."
    else
        rm -f "$OUTPUT_DIGEST_TEST"
        echo "Step 2: Generating test Clojure modules..."
        "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" clojure test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Copy hand-written hydra/lib/ from heads/lisp/clojure into dist/.
        # heads is the source of truth for these helpers; dist mirrors it.
        for d in lib; do
            LIB_SRC="$HYDRA_CLOJURE_HEAD/src/main/clojure/hydra/$d"
            LIB_DST="$OUT_MAIN/hydra/$d"
            if [ -d "$LIB_SRC" ]; then
                echo "Step 3a: Copying hand-written hydra/$d/ from heads/lisp/clojure/..."
                mkdir -p "$LIB_DST"
                cp -R "$LIB_SRC/." "$LIB_DST/"
            fi
        done

        # Patch testGraph.clj: replace empty graph/context with a full test
        # graph (primitives + schema types + annotation bindings). The graph
        # must be defined AFTER test_terms/test_types to avoid forward
        # reference; we delete the empty defs and append full defs at the end.
        CLJ_TESTGRAPH="$OUT_DIR/src/test/clojure/hydra/test/testGraph.clj"
        if [ -f "$CLJ_TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching testGraph.clj..."
            # Drop the generator-emitted (hydra.test.testEnv :refer :all) entry
            # — no testEnv.clj is emitted for Clojure; the inline rebuild below
            # replaces its role. Then add imports needed by the inline rebuild.
            sed -i.bak 's| \[hydra.test.testEnv :refer :all\]||' "$CLJ_TESTGRAPH"
            sed -i.bak 's|\[hydra.packaging :refer :all\]|[hydra.packaging :refer :all] [hydra.lib.libraries :refer :all] [hydra.rewriting :refer :all] [hydra.scoping :refer :all] [hydra.json.bootstrap :refer :all] [hydra.graph :refer :all] [hydra.context :refer :all] [hydra.annotation-bindings :refer [annotation-bindings]]|' "$CLJ_TESTGRAPH"
            # Delete the generator's test_env-based defs; they'll be replaced
            # by the inline rebuild appended below.
            sed -i.bak '/^(def hydra_test_test_graph_test_context hydra_test_test_env_test_context)/d' "$CLJ_TESTGRAPH"
            sed -i.bak '/^(def hydra_test_test_graph_test_graph (hydra_test_test_env_test_graph hydra_test_test_graph_test_types))/d' "$CLJ_TESTGRAPH"
            # Also drop test_env symbols from the (declare ...) form.
            sed -i.bak 's| hydra_test_test_env_test_context||g; s| hydra_test_test_env_test_graph||g' "$CLJ_TESTGRAPH"
            rm -f "$CLJ_TESTGRAPH.bak"
            cat >> "$CLJ_TESTGRAPH" << 'CLJEOF'

(def hydra_test_test_graph_test_context {:functions () :annotations () :variable_types {}})

(def hydra_test_test_graph_test_graph
  (let [std-prims (standard-library)
        type-to-ts hydra_scoping_f_type_to_type_scheme
        boot-types-raw hydra_json_bootstrap_types_by_name
        kernel-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) boot-types-raw))
        test-types-list (seq hydra_test_test_graph_test_types)
        test-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) test-types-list))
        schema-types (merge kernel-schemas test-schemas)
        bound-terms (merge
          ;; Primitives are resolved via graphPrimitives, not boundTerms.
          (into {} (annotation-bindings))
          (into {} (seq hydra_test_test_graph_test_terms)))]
    {:bound_terms bound-terms
     :bound_types {}
     :class_constraints {}
     :lambda_variables #{}
     :metadata {}
     :primitives std-prims
     :schema_types schema-types
     :type_variables #{}}))
CLJEOF
        fi
        ;;
    *)
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="

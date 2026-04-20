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
INPUT_DIGEST="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/digest.json"
OUTPUT_DIGEST="$OUT_DIR/digest.json"

# Freshness check: skip the slow path when nothing has changed.
if [ -f "$INPUT_DIGEST" ] && [ -f "$OUTPUT_DIGEST" ]; then
    if (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
        stack exec digest-check -- fresh \
            --inputs "$INPUT_DIGEST" \
            --output-dir "$OUT_DIR" \
            --output-digest "$OUTPUT_DIGEST" 2>/dev/null); then
        echo "  Cache hit; skipping work."
        echo "=== Done. $PACKAGE (cache hit) ==="
        exit 0
    fi
fi

# Cache miss: invalidate the per-target digest so Stage 7 can't trust stale records.
rm -f "$OUTPUT_DIGEST"


echo "=== Assembling Clojure distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "Step 1: Generating main Clojure modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" clojure main \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test Clojure modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" clojure test \
    --output "$DIST_ROOT"

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Patch testGraph.clj: replace empty graph/context with a full test
        # graph (primitives + schema types + annotation bindings). The graph
        # must be defined AFTER test_terms/test_types to avoid forward
        # reference; we delete the empty defs and append full defs at the end.
        CLJ_TESTGRAPH="$OUT_DIR/src/test/clojure/hydra/test/testGraph.clj"
        if [ -f "$CLJ_TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching testGraph.clj..."
            # Add required imports
            sed -i.bak 's|\[hydra.lexical :refer :all\]|[hydra.lexical :refer :all] [hydra.lib.libraries :refer :all] [hydra.rewriting :refer :all] [hydra.scoping :refer :all] [hydra.json.bootstrap :refer :all] [hydra.graph :refer :all] [hydra.context :refer :all] [hydra.annotation-bindings :refer [annotation-bindings]]|' "$CLJ_TESTGRAPH"
            # Delete empty def lines; they'll be re-added at EOF.
            sed -i.bak '/^(def hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$CLJ_TESTGRAPH"
            sed -i.bak '/^(def hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$CLJ_TESTGRAPH"
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
# Refresh the per-target digest so future fresh-checks short-circuit.
if [ -f "$INPUT_DIGEST" ]; then
    (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
     stack exec digest-check -- refresh \
        --inputs "$INPUT_DIGEST" \
        --output-dir "$OUT_DIR" \
        --output-digest "$OUTPUT_DIGEST")
fi

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="

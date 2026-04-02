#!/bin/bash
# Build and test a Clojure bootstrap target directory.
#
# Usage: ./test-clojure-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# Patch testGraph.clj to build a full graph with primitives and schema types
# instead of using the empty graph. Same patching that sync-lisp.sh applies.
echo "Patching testGraph.clj..."
CLJ_TESTGRAPH="$OUTPUT_DIR/src/gen-test/clojure/hydra/test/testGraph.clj"
if [ -f "$CLJ_TESTGRAPH" ]; then
    # Add required imports
    sed -i '' 's|\[hydra.lexical :refer :all\]|[hydra.lexical :refer :all] [hydra.lib.libraries :refer :all] [hydra.rewriting :refer :all] [hydra.json.bootstrap :refer :all] [hydra.graph :refer :all] [hydra.context :refer :all] [hydra.annotation-bindings :refer [annotation-bindings]]|' "$CLJ_TESTGRAPH"
    # Delete the empty context and empty graph defs (they'll be re-added at the end)
    sed -i '' '/^(def hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$CLJ_TESTGRAPH"
    sed -i '' '/^(def hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$CLJ_TESTGRAPH"
    # Append full graph and context defs at end of file
    cat >> "$CLJ_TESTGRAPH" << 'CLJEOF'

(def hydra_test_test_graph_test_context {:functions () :annotations () :variable_types {}})

(def hydra_test_test_graph_test_graph
  (let [std-prims (standard-library)
        type-to-ts hydra_rewriting_f_type_to_type_scheme
        boot-types-raw hydra_json_bootstrap_types_by_name
        kernel-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) boot-types-raw))
        test-types-list (seq hydra_test_test_graph_test_types)
        test-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) test-types-list))
        schema-types (merge kernel-schemas test-schemas)
        bound-terms (merge
          (into {} (map (fn [[k _]] [k (list :function (list :primitive k))]) std-prims))
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

echo "Running Clojure tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" clojure -M -m run-tests 2>&1

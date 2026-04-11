#!/bin/bash
# Build and test a Scheme bootstrap target directory.
#
# Usage: ./test-scheme-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_ROOT="$( cd "$SCRIPT_DIR/../../../../.." && pwd )"

# Patch test_graph.scm to build a full graph with primitives and schema types
# instead of using the empty graph. Same patching that sync-lisp.sh applies.
echo "Patching test_graph.scm..."
SCHEME_TESTGRAPH="$OUTPUT_DIR/src/gen-test/scheme/hydra/test/test_graph.scm"
if [ -f "$SCHEME_TESTGRAPH" ]; then
    # Copy annotation bindings alongside the generated test graph
    cp "$OUTPUT_DIR/src/test/scheme/hydra/annotation_bindings.scm" \
       "$OUTPUT_DIR/src/gen-test/scheme/hydra/test/annotation_bindings.scm" 2>/dev/null || true

    # Add required imports for building graph with primitives
    sed -i '' 's|(import (scheme base) (hydra core) (hydra lexical) (hydra lib maps) (hydra module) (hydra test test_terms) (hydra test test_types))|(import (scheme base) (hydra core) (hydra context) (hydra graph) (hydra lexical) (hydra lib libraries) (hydra lib maps) (hydra module) (hydra rewriting) (hydra json bootstrap) (hydra test test_terms) (hydra test test_types))|' "$SCHEME_TESTGRAPH"
    # Delete the empty context and graph defs
    sed -i '' '/^(define hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$SCHEME_TESTGRAPH"
    sed -i '' '/^(define hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$SCHEME_TESTGRAPH"
    # Remove the final )) that closes begin and define-library, then append defs + closing
    sed -i '' '$ s/))$//' "$SCHEME_TESTGRAPH"
    cat >> "$SCHEME_TESTGRAPH" << 'SCMEOF'
;; Include annotation term-level bindings (shared with test runner).
SCMEOF
    # Insert include with absolute path
    ANN_BINDINGS_PATH="$OUTPUT_DIR/src/test/scheme/hydra/annotation_bindings.scm"
    echo "(include \"$ANN_BINDINGS_PATH\")" >> "$SCHEME_TESTGRAPH"
    cat >> "$SCHEME_TESTGRAPH" << 'SCMEOF'

(define hydra_test_test_graph_test_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))
(define hydra_test_test_graph_test_graph
  (let* ((all-prims (standard-library))
         (type-to-ts hydra_rewriting_f_type_to_type_scheme)
         (kernel-schemas (map (lambda (entry) (list (car entry) (type-to-ts (cdr entry)))) hydra_json_bootstrap_types_by_name))
         (test-schemas (map (lambda (entry) (list (car entry) (type-to-ts (cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
         (schema-types (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
         (test-terms (map (lambda (entry) (list (car entry) (cdr entry))) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))
         (bound-terms (append
           (map (lambda (pair) (list (car pair) (list (quote function) (list (quote primitive) (car pair))))) all-prims)
           (annotation-bindings)
           (list (list "hydra.monads.emptyContext" (list (quote unit) (list)))
                 (list "hydra.lexical.emptyGraph" (list (quote unit) (list))))
           test-terms)))
    (make-hydra_graph_graph
      (hydra_lib_maps_from_list bound-terms)
      hydra_lib_maps_empty (list) (list) hydra_lib_maps_empty
      (hydra_lib_maps_from_list (map (lambda (p) (list (car p) (cdr p))) all-prims))
      schema-types (list))))
))
SCMEOF
fi

echo "Running Scheme tests..."
cd "$OUTPUT_DIR"

# Detect Scheme implementation
if command -v guile > /dev/null 2>&1; then
    SCHEME_CMD="guile -L src/gen-main/scheme -L src/gen-test/scheme -L src/main/scheme -s run-tests.scm"
elif command -v chibi-scheme > /dev/null 2>&1; then
    SCHEME_CMD="chibi-scheme -I src/gen-main/scheme -I src/gen-test/scheme -I src/main/scheme run-tests.scm"
else
    echo "Error: No Scheme implementation found. Install guile or chibi-scheme."
    exit 1
fi

HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" $SCHEME_CMD 2>&1

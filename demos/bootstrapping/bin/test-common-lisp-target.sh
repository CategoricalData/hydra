#!/bin/bash
# Build and test a Common Lisp bootstrap target directory.
#
# Usage: ./test-common-lisp-target.sh <output-dir>

set -e

OUTPUT_DIR="$1"
if [ -z "$OUTPUT_DIR" ]; then
    echo "Usage: $0 <output-dir>"
    exit 1
fi

# Patch test_graph.lisp to build a full graph with primitives and schema types
# instead of using the empty graph. Same patching that the Common Lisp assembler applies.
echo "Patching test_graph.lisp..."
CL_TESTGRAPH="$OUTPUT_DIR/src/test/common-lisp/hydra/test/test_graph.lisp"
if [ -f "$CL_TESTGRAPH" ]; then
    sed -i '' '/^(cl:defvar hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$CL_TESTGRAPH"
    sed -i '' '/^(cl:defvar hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$CL_TESTGRAPH"
    cat >> "$CL_TESTGRAPH" << 'CLEOF'

(cl:defvar hydra_test_test_graph_test_context (cl:list (cl:cons :functions cl:nil) (cl:cons :annotations cl:nil) (cl:cons :variable_types cl:nil)))

(cl:defvar hydra_test_test_graph_test_graph
  (cl:let* ((std-prims (standard-library))
            (type-to-ts hydra_rewriting_f_type_to_type_scheme)
            (boot-types-raw hydra_json_bootstrap_types_by_name)
            (kernel-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cdr entry)))) (hydra_lib_maps_to_list boot-types-raw)))
            (test-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
            (schema-types (hydra_lib_maps_from_list (cl:append kernel-schemas test-schemas)))
            (prim-map (hydra_lib_maps_from_list (cl:mapcar (cl:lambda (p) (cl:list (cl:car p) (cl:cdr p))) std-prims)))
            (bound-terms (hydra_lib_maps_from_list (cl:append (cl:mapcar (cl:lambda (p) (cl:list (cl:car p) (cl:list :function (cl:list :primitive (cl:car p))))) std-prims) (annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (cl:list (cl:cons :bound_terms bound-terms) (cl:cons :bound_types cl:nil) (cl:cons :class_constraints cl:nil) (cl:cons :lambda_variables cl:nil) (cl:cons :metadata cl:nil) (cl:cons :primitives prim-map) (cl:cons :schema_types schema-types) (cl:cons :type_variables cl:nil))))
CLEOF
fi

echo "Running Common Lisp tests..."
cd "$OUTPUT_DIR"
HYDRA_BENCHMARK_OUTPUT="${HYDRA_BENCHMARK_OUTPUT:-}" sbcl --noinform --non-interactive --no-userinit --load src/test/common-lisp/run-tests.lisp 2>&1

#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Common Lisp distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/common-lisp/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_CL_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_CL_HEAD/../../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/common-lisp"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"

echo "=== Assembling Common Lisp distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "Step 1: Generating main Common Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp main \
    --output "$OUT_DIR/src/main"

echo ""
echo "Step 2: Generating test Common Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp test \
    --output "$OUT_DIR/src/test"

case "$PACKAGE" in
    hydra-kernel)
        # Patch test_graph.lisp — same pattern as Clojure and Scheme: delete
        # empty defs, append full graph/context defs at end of file.
        CL_TESTGRAPH="$OUT_DIR/src/test/common-lisp/hydra/test/test_graph.lisp"
        if [ -f "$CL_TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching test_graph.lisp..."
            sed -i.bak '/^(cl:defvar hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$CL_TESTGRAPH"
            sed -i.bak '/^(cl:defvar hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$CL_TESTGRAPH"
            rm -f "$CL_TESTGRAPH.bak"
            cat >> "$CL_TESTGRAPH" << 'CLEOF'

(cl:defvar hydra_test_test_graph_test_context (make-hydra_context_context (cl:list) (cl:list) hydra_lib_maps_empty))

(cl:defvar hydra_test_test_graph_test_graph
  (cl:let* ((std-prims (standard-library))
            (type-to-ts hydra_scoping_f_type_to_type_scheme)
            (boot-types-raw hydra_json_bootstrap_types_by_name)
            (kernel-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cdr entry)))) (hydra_lib_maps_to_list boot-types-raw)))
            (test-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
            (schema-types (hydra_lib_maps_from_list (cl:append kernel-schemas test-schemas)))
            (prim-map (hydra_lib_maps_from_list (cl:mapcar (cl:lambda (p) (cl:list (cl:car p) (cl:cdr p))) std-prims)))
            (bound-terms (hydra_lib_maps_from_list (cl:append (annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (make-hydra_graph_graph
      bound-terms
      hydra_lib_maps_empty
      hydra_lib_maps_empty
      hydra_lib_sets_empty
      hydra_lib_maps_empty
      prim-map
      schema-types
      hydra_lib_sets_empty)))
CLEOF
        fi
        ;;
    *)
        ;;
esac

echo ""
echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="

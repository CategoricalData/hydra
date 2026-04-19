#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Emacs Lisp distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/emacs-lisp/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EL_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EL_HEAD/../../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/emacs-lisp"

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
            --output-digest "$OUTPUT_DIGEST" 2>/dev/null); then
        echo "  Cache hit; skipping work."
        echo "=== Done. $PACKAGE (cache hit) ==="
        exit 0
    fi
fi


echo "=== Assembling Emacs Lisp distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "Step 1: Generating main Emacs Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" emacs-lisp main \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test Emacs Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" emacs-lisp test \
    --output "$DIST_ROOT"

case "$PACKAGE" in
    hydra-kernel)
        EL_TESTGRAPH="$OUT_DIR/src/test/emacs-lisp/hydra/test/test_graph.el"
        if [ -f "$EL_TESTGRAPH" ]; then
            echo ""
            echo "Step 3: Patching test_graph.el..."
            sed -i.bak '/^(setq hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$EL_TESTGRAPH"
            sed -i.bak '/^(setq hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$EL_TESTGRAPH"
            rm -f "$EL_TESTGRAPH.bak"
            cat >> "$EL_TESTGRAPH" << 'ELEOF'

(setq hydra_test_test_graph_test_context (list (cons :functions nil) (cons :annotations nil) (cons :variable_types nil)))

(setq hydra_test_test_graph_test_graph
  (let* ((std-prims (standard-library))
         (type-to-ts (lambda (t) (funcall hydra_scoping_f_type_to_type_scheme t)))
         (boot-types-raw hydra_json_bootstrap_types_by_name)
         (kernel-schemas (mapcar (lambda (entry) (list (car entry) (funcall type-to-ts (cdr entry)))) (hydra_lib_maps_to_list boot-types-raw)))
         (test-schemas (mapcar (lambda (entry) (list (car entry) (funcall type-to-ts (cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
         (schema-types (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
         (prim-map (hydra_lib_maps_from_list (mapcar (lambda (p) (list (car p) (cdr p))) std-prims)))
         (bound-terms (hydra_lib_maps_from_list (append (hydra-annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (list (cons :bound_terms bound-terms) (cons :bound_types nil) (cons :class_constraints nil) (cons :lambda_variables nil) (cons :metadata nil) (cons :primitives prim-map) (cons :schema_types schema-types) (cons :type_variables nil))))
ELEOF
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

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
INPUT_DIGEST="$HYDRA_ROOT_DIR/dist/json/$PACKAGE/digest.json"
OUTPUT_DIGEST="$OUT_DIR/digest.json"

# Cheap Python pre-check: compare input digest hashes to recorded
# output digest inputs. Avoids stack-exec startup for warm runs.
if [ -f "$INPUT_DIGEST" ] && [ -f "$OUTPUT_DIGEST" ]; then
    if python3 -c "
import json, sys
try:
    out = json.load(open('$OUTPUT_DIGEST'))
    inp = json.load(open('$INPUT_DIGEST'))
    recorded = {k: (v.get('hash') if isinstance(v, dict) else v)
                for k, v in out.get('inputs', {}).items()}
    current = inp.get('hashes', inp)
    sys.exit(0 if recorded == current else 1)
except Exception:
    sys.exit(1)
" 2>/dev/null; then
        echo "  Cache hit; skipping work."
        echo "=== Done. $PACKAGE (cache hit) ==="
        exit 0
    fi
fi

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


echo "=== Assembling Common Lisp distribution: $PACKAGE ==="
echo "  Output: $OUT_DIR"
echo ""

HASKELL_BIN="$HYDRA_ROOT_DIR/heads/haskell/bin"

echo "Step 1: Generating main Common Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp main \
    --output "$DIST_ROOT"

echo ""
echo "Step 2: Generating test Common Lisp modules..."
"$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp test \
    --output "$DIST_ROOT"

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
# Refresh the per-target digest so future fresh-checks short-circuit.
if [ -f "$INPUT_DIGEST" ]; then
    (cd "$HYDRA_ROOT_DIR/heads/haskell" && \
     stack exec digest-check -- refresh \
        --inputs "$INPUT_DIGEST" \
        --output-dir "$OUT_DIR" \
        --output-digest "$OUTPUT_DIGEST")
fi

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="

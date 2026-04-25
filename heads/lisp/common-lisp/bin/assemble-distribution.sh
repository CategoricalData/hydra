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
OUT_MAIN="$OUT_DIR/src/main/common-lisp"
OUT_TEST="$OUT_DIR/src/test/common-lisp"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Common Lisp distribution: $PACKAGE ==="
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
    echo "Step 1: Generating main Common Lisp modules..."
    "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp main \
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
        echo "Step 2: Generating test Common Lisp modules..."
        "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" common-lisp test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

case "$PACKAGE" in
    hydra-kernel)
        # Copy hand-written hydra/lib/ from heads/lisp/common-lisp into dist/.
        # heads is the source of truth for these helpers; dist mirrors it.
        for d in lib; do
            LIB_SRC="$HYDRA_CL_HEAD/src/main/common-lisp/hydra/$d"
            LIB_DST="$OUT_MAIN/hydra/$d"
            if [ -d "$LIB_SRC" ]; then
                echo "Step 3a: Copying hand-written hydra/$d/ from heads/lisp/common-lisp/..."
                mkdir -p "$LIB_DST"
                cp -R "$LIB_SRC/." "$LIB_DST/"
            fi
        done

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

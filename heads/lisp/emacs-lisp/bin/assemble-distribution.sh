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
OUT_MAIN="$OUT_DIR/src/main/emacs-lisp"
OUT_TEST="$OUT_DIR/src/test/emacs-lisp"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Emacs Lisp distribution: $PACKAGE ==="
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
    echo "Step 1: Generating main Emacs Lisp modules..."
    "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" emacs-lisp main \
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
        echo "Step 2: Generating test Emacs Lisp modules..."
        "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" emacs-lisp test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

case "$PACKAGE" in
    hydra-kernel)
        # (Emacs Lisp exception: do NOT copy heads/lisp/emacs-lisp/.../lib/
        # into dist/emacs-lisp/hydra-kernel/. The Emacs Lisp loader treats
        # both directories as load-path entries, so a copy could shadow or
        # double-load symbols.)

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

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="

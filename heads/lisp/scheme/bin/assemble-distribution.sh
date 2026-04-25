#!/usr/bin/env bash
# Layer 2 assembler: produce a complete Scheme distribution for one package.
#
# Usage:
#   assemble-distribution.sh <pkg> [--dist-root <dir>]
#
# Produces <dist-root>/<pkg>/ (default: ../../../dist/scheme/<pkg>/).

set -euo pipefail

if [ $# -lt 1 ]; then
    echo "Usage: $0 <package> [--dist-root <dir>]" >&2
    exit 1
fi

PACKAGE="$1"
shift

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_SCHEME_HEAD="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_SCHEME_HEAD/../../.." && pwd )"

DIST_ROOT="$HYDRA_ROOT_DIR/dist/scheme"

while [ $# -gt 0 ]; do
    case "$1" in
        --dist-root) DIST_ROOT="$2"; shift 2 ;;
        *) shift ;;
    esac
done

OUT_DIR="$DIST_ROOT/$PACKAGE"
OUT_MAIN="$OUT_DIR/src/main/scheme"
OUT_TEST="$OUT_DIR/src/test/scheme"
DIST_JSON_ROOT="$HYDRA_ROOT_DIR/dist/json"
INPUT_DIGEST_MAIN="$DIST_JSON_ROOT/$PACKAGE/src/main/digest.json"
INPUT_DIGEST_TEST="$DIST_JSON_ROOT/$PACKAGE/src/test/digest.json"
OUTPUT_DIGEST_MAIN="$OUT_DIR/src/main/digest.json"
OUTPUT_DIGEST_TEST="$OUT_DIR/src/test/digest.json"
TEST_JSON_DIR="$DIST_JSON_ROOT/$PACKAGE/src/test/json"

echo "=== Assembling Scheme distribution: $PACKAGE ==="
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
    echo "Step 1: Generating main Scheme modules..."
    "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" scheme main \
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
        echo "Step 2: Generating test Scheme modules..."
        "$HASKELL_BIN/transform-json-to-lisp.sh" "$PACKAGE" scheme test \
            --output "$DIST_ROOT"
        assemble_refresh_digest "$INPUT_DIGEST_TEST" "$OUT_TEST" "$OUTPUT_DIGEST_TEST"
    fi
fi

# Step 3: Package-specific post-processing.
case "$PACKAGE" in
    hydra-kernel)
        # Step 3a: Copy hand-written Scheme runtime library files
        # (chars, eithers, lists, maps, sets, ...) from heads/lisp/scheme
        # into dist/scheme. These implement the Scheme runtime for generated code.
        #
        # Historical note: maps.scm and sets.scm were previously skipped here
        # because two implementations existed — an alist-backed portable version
        # checked into dist/scheme/, and a vhash-backed Guile-specific version
        # in heads/. The dist version was casualty of the dist/scheme/ untrack
        # (commit 0a00d9166, "Stop tracking generated dist/ targets"), and CI
        # only targets Guile, so we now copy the heads/ vhash version
        # unconditionally. If portable Scheme support is needed later, restore
        # the alist version from git history (0a00d9166^) and reintroduce the
        # skip.
        SCHEME_LIB_SRC="$HYDRA_SCHEME_HEAD/src/main/scheme/hydra/lib"
        SCHEME_LIB_DST="$OUT_DIR/src/main/scheme/hydra/lib"
        echo ""
        echo "Step 3a: Copying Scheme runtime libraries..."
        mkdir -p "$SCHEME_LIB_DST"
        for lib_file in "$SCHEME_LIB_SRC"/*.scm; do
            [ -e "$lib_file" ] || continue
            base=$(basename "$lib_file")
            cp "$lib_file" "$SCHEME_LIB_DST/$base"
        done

        # Step 3b: Write Scheme stub modules for decode/encode graph/compute.
        # Empty R7RS libraries as placeholders for modules that haven't been
        # generated.
        SCHEME_MAIN="$OUT_DIR/src/main/scheme"
        echo "Step 3b: Writing Scheme stub modules..."
        for stub in decode/graph decode/compute encode/graph encode/compute; do
            path="$SCHEME_MAIN/hydra/$stub.scm"
            mod_name=$(echo "hydra $stub" | tr '/' ' ')
            mkdir -p "$(dirname "$path")"
            cat > "$path" <<EOF
(define-library ($mod_name)
(import (scheme base))
(export)
(begin))
EOF
        done

        # Step 3c: Copy annotation_bindings.scm next to test_graph.scm so the
        # test graph's (include ...) directive resolves.
        cp "$HYDRA_SCHEME_HEAD/src/test/scheme/hydra/annotation_bindings.scm" \
           "$OUT_DIR/src/test/scheme/hydra/test/annotation_bindings.scm" 2>/dev/null || true

        # Step 3d: Patch test_graph.scm — replace the generated test_env
        # references with a full graph built inline from primitives + schema
        # types + annotation bindings. R7RS Scheme's lack of dynamic loading
        # makes a clean handoff to a separate test_env.scm awkward (the inline
        # build relies on annotation_bindings.scm, hydra_test_test_graph_terms,
        # etc. — all in test_graph's scope). So Scheme keeps an inline patch
        # while Haskell/Java/Python use the cleaner DSL→runtime handoff.
        SCHEME_TESTGRAPH="$OUT_DIR/src/test/scheme/hydra/test/test_graph.scm"
        if [ -f "$SCHEME_TESTGRAPH" ]; then
            echo "Step 3d: Patching test_graph.scm..."
            # Replace the import block: drop the (hydra test test_env) entry
            # the DSL now emits, and add the imports we need for the inline build.
            sed -i.bak 's|(import (scheme base) (hydra core) (hydra lib maps) (hydra packaging) (hydra test test_env) (hydra test test_terms) (hydra test test_types))|(import (scheme base) (hydra core) (hydra context) (hydra graph) (hydra lexical) (hydra lib libraries) (hydra lib maps) (hydra packaging) (hydra rewriting) (hydra scoping) (hydra json bootstrap) (hydra test test_terms) (hydra test test_types))|' "$SCHEME_TESTGRAPH"
            # Drop the generator's test_env-based defines; they'll be replaced.
            sed -i.bak '/^(define hydra_test_test_graph_test_context hydra_test_test_env_test_context)/d' "$SCHEME_TESTGRAPH"
            sed -i.bak '/^(define hydra_test_test_graph_test_graph (hydra_test_test_env_test_graph hydra_test_test_graph_test_types))/d' "$SCHEME_TESTGRAPH"
            # Remove trailing )) that closes begin and define-library; we'll re-add after appending.
            sed -i.bak '$ s/))$//' "$SCHEME_TESTGRAPH"
            rm -f "$SCHEME_TESTGRAPH.bak"
            cat >> "$SCHEME_TESTGRAPH" << 'SCMEOF'
;; Include annotation term-level bindings (shared with test runner).
SCMEOF
            echo '(include "annotation_bindings.scm")' >> "$SCHEME_TESTGRAPH"
            cat >> "$SCHEME_TESTGRAPH" << 'SCMEOF'

(define hydra_test_test_graph_test_context (make-hydra_context_context (list) (list) hydra_lib_maps_empty))
(define hydra_test_test_graph_test_graph
  (let* ((all-prims (standard-library))
         (type-to-ts hydra_scoping_f_type_to_type_scheme)
         (kernel-schemas (map (lambda (entry) (list (car entry) (type-to-ts (cdr entry)))) hydra_json_bootstrap_types_by_name))
         (test-schemas (map (lambda (entry) (list (car entry) (type-to-ts (cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
         (schema-types (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
         (test-terms (map (lambda (entry) (list (car entry) (cdr entry))) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))
         (bound-terms (append
           ;; Primitives are resolved via graphPrimitives, not boundTerms.
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
        ;;
    *)
        ;;
esac

echo ""

echo "=== Done. $PACKAGE assembled under $OUT_DIR ==="

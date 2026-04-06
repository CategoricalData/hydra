#!/bin/bash
set -eo pipefail

# Script to regenerate Lisp code for all four dialects from Hydra sources.
#
# This generates Clojure, Common Lisp, Emacs Lisp, and Scheme code
# from the Hydra kernel and test modules using the Lisp coder.
#
# Prerequisites:
#   - Hydra-Ext must be consistent (run sync-ext.sh first)
#   - Run from the hydra-ext directory (or the script will cd there)
#
# Usage:
#   ./bin/sync-lisp.sh                                  # Generate all four dialects
#   ./bin/sync-lisp.sh --dialects clojure,scheme        # Generate specific dialects
#   ./bin/sync-lisp.sh --quick                          # Skip tests
#   ./bin/sync-lisp.sh --help                           # Show this help

QUICK_MODE=false
DIALECTS="clojure,common-lisp,emacs-lisp,scheme"

while [ $# -gt 0 ]; do
    case "$1" in
        --quick)
            QUICK_MODE=true
            ;;
        --dialects)
            DIALECTS="$2"
            shift
            ;;
        --help|-h)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Regenerate Lisp code from Hydra sources."
            echo ""
            echo "Options:"
            echo "  --dialects D,...  Generate only specified dialects (default: all four)"
            echo "                    Valid dialects: clojure, common-lisp, emacs-lisp, scheme"
            echo "  --quick           Skip running tests after generation"
            echo "  --help            Show this help message"
            echo ""
            echo "Steps performed:"
            echo "  1. Build generate-lisp executable"
            echo "  2. Generate Lisp code for selected dialects"
            echo "  3. Run tests for each dialect (unless --quick)"
            exit 0
            ;;
    esac
    shift
done

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
HYDRA_EXT_DIR="$( cd "$SCRIPT_DIR/.." && pwd )"
HYDRA_ROOT_DIR="$( cd "$HYDRA_EXT_DIR/.." && pwd )"

cd "$HYDRA_EXT_DIR"

# RTS flags to avoid stack overflow during generation
RTS_FLAGS="+RTS -K256M -A32M -RTS"

# Parse dialect list
IFS=',' read -ra DIALECT_LIST <<< "$DIALECTS"

# Expand 'lisp' and 'all' to all four dialects
expanded=()
for d in "${DIALECT_LIST[@]}"; do
    case "$d" in
        lisp|all) expanded+=(clojure common-lisp emacs-lisp scheme) ;;
        *)        expanded+=("$d") ;;
    esac
done
DIALECT_LIST=("${expanded[@]}")

# Validate dialects
for d in "${DIALECT_LIST[@]}"; do
    case "$d" in
        clojure|common-lisp|emacs-lisp|scheme) ;;
        *)
            echo "Error: Unknown dialect '$d'. Valid dialects: clojure, common-lisp, emacs-lisp, scheme"
            exit 1
            ;;
    esac
done

echo "=========================================="
echo "Synchronizing Lisp (${DIALECT_LIST[*]})"
echo "=========================================="
echo ""

echo "Step 1: Building generate-lisp executable..."
echo ""
stack build hydra-ext:exe:generate-lisp

echo ""
echo "Step 2: Generating Lisp code..."
echo ""
# generate-lisp generates all four dialects; we run it once
# and let the user's --dialects flag control which tests to run
stack exec generate-lisp -- $RTS_FLAGS

# Restore Scheme gen-main native libs to their committed (portable alist) versions.
# The generate-lisp tool overwrites these with the src/main vhash versions, but
# gen-main must use portable alists since standalone targets lack (ice-9 vlist).
SCHEME_GEN_LIB="$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme/src/gen-main/scheme/hydra/lib"
if [ -d "$SCHEME_GEN_LIB" ]; then
    echo "  Restoring portable gen-main lib files for Scheme..."
    git -C "$HYDRA_ROOT_DIR" checkout -- \
        hydra-lisp/hydra-scheme/src/gen-main/scheme/hydra/lib/maps.scm \
        hydra-lisp/hydra-scheme/src/gen-main/scheme/hydra/lib/sets.scm \
        2>/dev/null || true
fi

dialect_dir() {
    case "$1" in
        clojure)     echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-clojure" ;;
        common-lisp) echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-common-lisp" ;;
        emacs-lisp)  echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-emacs-lisp" ;;
        scheme)      echo "$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme" ;;
    esac
}

# Patch Scheme test_graph.scm to build a full graph with primitives and schema types.
# The graph must be defined AFTER test_terms and test_types (forward reference issue).
# Copy annotation bindings alongside the generated test graph (for include)
cp "$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme/src/test/scheme/hydra/annotation_bindings.scm" \
   "$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme/src/gen-test/scheme/hydra/test/annotation_bindings.scm" 2>/dev/null

echo "Patching Scheme test_graph.scm..."
SCHEME_TESTGRAPH="$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme/src/gen-test/scheme/hydra/test/test_graph.scm"
if [ -f "$SCHEME_TESTGRAPH" ]; then
    # Add required imports for building graph with primitives
    sed -i '' 's|(import (scheme base) (hydra core) (hydra lexical) (hydra lib maps) (hydra packaging) (hydra test test_terms) (hydra test test_types))|(import (scheme base) (hydra core) (hydra context) (hydra graph) (hydra lexical) (hydra lib libraries) (hydra lib maps) (hydra packaging) (hydra rewriting) (hydra scoping) (hydra json bootstrap) (hydra test test_terms) (hydra test test_types))|' "$SCHEME_TESTGRAPH"
    # Delete the empty context and graph defs
    sed -i '' '/^(define hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$SCHEME_TESTGRAPH"
    sed -i '' '/^(define hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$SCHEME_TESTGRAPH"
    # Remove the final )) that closes begin and define-library, then append defs + closing
    sed -i '' '$ s/))$//' "$SCHEME_TESTGRAPH"
    cat >> "$SCHEME_TESTGRAPH" << 'SCMEOF'
;; Include annotation term-level bindings (shared with test runner).
SCMEOF
    # Insert include with absolute path (Guile's include in define-library doesn't search load path)
    ANN_BINDINGS_PATH="$HYDRA_ROOT_DIR/hydra-lisp/hydra-scheme/src/test/scheme/hydra/annotation_bindings.scm"
    echo "(include \"$ANN_BINDINGS_PATH\")" >> "$SCHEME_TESTGRAPH"
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

# Patch Clojure testGraph.clj to build a full graph with primitives and schema types.
# The graph must be defined AFTER test_terms and test_types (forward reference issue).
echo "Patching Clojure testGraph.clj..."
CLJ_TESTGRAPH="$HYDRA_ROOT_DIR/hydra-lisp/hydra-clojure/src/gen-test/clojure/hydra/test/testGraph.clj"
if [ -f "$CLJ_TESTGRAPH" ]; then
    # Add required imports
    sed -i '' 's|\[hydra.lexical :refer :all\]|[hydra.lexical :refer :all] [hydra.lib.libraries :refer :all] [hydra.rewriting :refer :all] [hydra.scoping :refer :all] [hydra.json.bootstrap :refer :all] [hydra.graph :refer :all] [hydra.context :refer :all] [hydra.annotation-bindings :refer [annotation-bindings]]|' "$CLJ_TESTGRAPH"
    # Delete the empty context and empty graph defs (they'll be re-added at the end)
    sed -i '' '/^(def hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$CLJ_TESTGRAPH"
    sed -i '' '/^(def hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$CLJ_TESTGRAPH"
    # Append full graph and context defs at end of file (after test_types is defined)
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

# Patch Common Lisp test_graph.lisp — same approach as Clojure (append at end)
echo "Patching Common Lisp test_graph.lisp..."
CL_TESTGRAPH="$HYDRA_ROOT_DIR/hydra-lisp/hydra-common-lisp/src/gen-test/common-lisp/hydra/test/test_graph.lisp"
if [ -f "$CL_TESTGRAPH" ]; then
    sed -i '' '/^(cl:defvar hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$CL_TESTGRAPH"
    sed -i '' '/^(cl:defvar hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$CL_TESTGRAPH"
    cat >> "$CL_TESTGRAPH" << 'CLEOF'

(cl:defvar hydra_test_test_graph_test_context (cl:list (cl:cons :functions cl:nil) (cl:cons :annotations cl:nil) (cl:cons :variable_types cl:nil)))

(cl:defvar hydra_test_test_graph_test_graph
  (cl:let* ((std-prims (standard-library))
            (type-to-ts hydra_scoping_f_type_to_type_scheme)
            (boot-types-raw hydra_json_bootstrap_types_by_name)
            (kernel-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cdr entry)))) (hydra_lib_maps_to_list boot-types-raw)))
            (test-schemas (cl:mapcar (cl:lambda (entry) (cl:list (cl:car entry) (cl:funcall type-to-ts (cl:cadr entry)))) (hydra_lib_maps_to_list hydra_test_test_graph_test_types)))
            (schema-types (hydra_lib_maps_from_list (cl:append kernel-schemas test-schemas)))
            (prim-map (hydra_lib_maps_from_list (cl:mapcar (cl:lambda (p) (cl:list (cl:car p) (cl:cdr p))) std-prims)))
            (bound-terms (hydra_lib_maps_from_list (cl:append (cl:mapcar (cl:lambda (p) (cl:list (cl:car p) (cl:list :function (cl:list :primitive (cl:car p))))) std-prims) (annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (cl:list (cl:cons :bound_terms bound-terms) (cl:cons :bound_types cl:nil) (cl:cons :class_constraints cl:nil) (cl:cons :lambda_variables cl:nil) (cl:cons :metadata cl:nil) (cl:cons :primitives prim-map) (cl:cons :schema_types schema-types) (cl:cons :type_variables cl:nil))))
CLEOF
fi

# Patch Emacs Lisp test_graph.el — same approach (append at end)
echo "Patching Emacs Lisp test_graph.el..."
EL_TESTGRAPH="$HYDRA_ROOT_DIR/hydra-lisp/hydra-emacs-lisp/src/gen-test/emacs-lisp/hydra/test/test_graph.el"
if [ -f "$EL_TESTGRAPH" ]; then
    sed -i '' '/^(setq hydra_test_test_graph_test_context hydra_lexical_empty_context)/d' "$EL_TESTGRAPH"
    sed -i '' '/^(setq hydra_test_test_graph_test_graph hydra_lexical_empty_graph)/d' "$EL_TESTGRAPH"
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
         (bound-terms (hydra_lib_maps_from_list (append (mapcar (lambda (p) (list (car p) (list :function (list :primitive (car p))))) std-prims) (hydra-annotation-bindings) (hydra_lib_maps_to_list hydra_test_test_graph_test_terms)))))
    (list (cons :bound_terms bound-terms) (cons :bound_types nil) (cons :class_constraints nil) (cons :lambda_variables nil) (cons :metadata nil) (cons :primitives prim-map) (cons :schema_types schema-types) (cons :type_variables nil))))
ELEOF
fi

if [ "$QUICK_MODE" = false ]; then
    echo ""
    echo "Step 3: Running tests..."
    echo ""

    for dialect in "${DIALECT_LIST[@]}"; do
        DIR=$(dialect_dir "$dialect")
        echo "  Testing ${dialect}..."
        if [ -f "$DIR/run-tests.sh" ]; then
            cd "$DIR"
            bash run-tests.sh
            cd "$HYDRA_EXT_DIR"
        else
            echo "    Skipped (no run-tests.sh found in $DIR)"
        fi
        echo ""
    done
else
    echo ""
    echo "Step 3: Skipped (--quick mode)"
fi

# Report new files
for dialect in "${DIALECT_LIST[@]}"; do
    CHECK_DIR=$(dialect_dir "$dialect")
    LABEL=$(basename "$CHECK_DIR")
    if [ -d "$CHECK_DIR" ]; then
        NEW_FILES=$(cd "$CHECK_DIR" && git status --porcelain src/gen-main src/gen-test 2>/dev/null | grep "^??" | awk '{print $2}' || true)
        if [ -n "$NEW_FILES" ]; then
            NEW_COUNT=$(echo "$NEW_FILES" | wc -l | tr -d ' ')
            echo "New files in $LABEL ($NEW_COUNT). You may want to run:"
            echo "  cd $CHECK_DIR && git add src/gen-main src/gen-test"
            echo ""
        fi
    fi
done

echo ""
echo "=========================================="
echo "Lisp sync complete!"
echo "=========================================="

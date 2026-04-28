;; Hand-written test environment for Common Lisp.
;; Provides the runtime test graph and test context referenced by the
;; generated hydra.test.test_graph module.
;;
;; Mirrors the role of:
;;   - heads/python/.../hydra/test/test_env.py
;;   - heads/lisp/scheme/.../hydra/test/test_env.scm
;;
;; The kernel filters out hydra.test.testEnv on output (via the
;; testSkipEmit set in each host bootstrap), so the auto-generated
;; testGraph module references hydra_test_test_env_test_context and
;; hydra_test_test_env_test_graph — both must be supplied by hand
;; per host language. This file is the Common Lisp implementation.
;;
;; The actual graph build (primitives, annotation bindings, schema
;; types) is delegated to the CL test runner's build-test-graph helper
;; in heads/lisp/common-lisp/src/test/common-lisp/hydra/test_runner.lisp,
;; which already maintains the live test graph used by all CL tests.
;; This file provides the two symbols the generated test_graph.lisp
;; needs (a Context value and a (test-types -> Graph) function),
;; backed by that helper.

(in-package :cl-user)

;; Install the annotation-cache wrapper around hydra_strip_deannotate_term.
;; The kernel reducer strips annotations as a normal step of evaluation; the
;; CL-side annotation primitives (setTermAnnotation et al.) need access to
;; those stripped annotations to re-attach them after reduction. The wrapper
;; caches them so the primitives can look them up. Must be installed BEFORE
;; any test_graph evaluation runs, because the cache wrapper has to be in
;; place when the reducer first sees an annotated term. Defined in
;; test_runner.lisp, loaded earlier in run-tests.lisp.
(install-annotation-cache)

;; Defining the package allows the generated test_graph.lisp's
;; (defpackage :hydra.test.testGraph (:use ... :hydra.test.testEnv ...))
;; to find and use it. The exported symbols are looked up via
;; symbol-value at load time inside test_graph.lisp.
(cl:defpackage :hydra.test.testEnv
  (:use :cl))

;; Empty context: alist with the three hydra_context_context fields
;; (trace, messages, other). The generated kernel modules create
;; records as alists '((:key . val) ...).
(cl:defvar hydra_test_test_env_test_context
  (cl:list (cl:cons :trace cl:nil)
           (cl:cons :messages cl:nil)
           (cl:cons :other hydra_lib_maps_empty)))

;; Build a test graph parametrized by a map of test-types.
;; Delegates to the test runner's build-test-graph + schema-type
;; enrichment so we don't duplicate the annotation-bindings logic.
;;
;; The defvar holds a lambda; loader.lisp's hydra-set-function-bindings
;; will set the symbol-function cell so it can be called in function
;; position from the generated test_graph.lisp.
(cl:defvar hydra_test_test_env_test_graph
  (cl:lambda (test-types)
    (build-test-graph-with-types test-types)))

;; Helper for the lambda above. Builds the runtime graph with
;; primitives + annotation primitives, then enriches :schema_types
;; with kernel bootstrap schemas + the supplied test-types map.
(cl:defun build-test-graph-with-types (test-types)
  (cl:let* ((base (build-test-graph))
            (bootstrap-types (cl:if (cl:boundp 'hydra_json_bootstrap_types_by_name)
                                    hydra_json_bootstrap_types_by_name
                                    cl:nil))
            (type-to-ts (cl:when (cl:boundp 'hydra_scoping_f_type_to_type_scheme)
                          hydra_scoping_f_type_to_type_scheme))
            (kernel-entries
              (cl:when (cl:and bootstrap-types type-to-ts)
                (cl:mapcar (cl:lambda (entry)
                             (cl:list (cl:first entry)
                                      (cl:funcall type-to-ts (cl:second entry))))
                           (cl:funcall hydra_lib_maps_to_list bootstrap-types))))
            (test-entries
              (cl:when (cl:and test-types type-to-ts)
                (cl:mapcar (cl:lambda (entry)
                             (cl:list (cl:first entry)
                                      (cl:funcall type-to-ts (cl:second entry))))
                           (cl:funcall hydra_lib_maps_to_list test-types))))
            (all-entries (cl:append (cl:or kernel-entries cl:nil)
                                    (cl:or test-entries cl:nil)))
            (schema-types (cl:funcall hydra_lib_maps_from_list all-entries)))
    ;; Replace base graph's :schema_types alist cell with the enriched one.
    (cl:mapcar (cl:lambda (cell)
                 (cl:if (cl:eq (cl:car cell) :schema_types)
                        (cl:cons :schema_types schema-types)
                        cell))
               base)))

;; Import the bound vars into :hydra.test.testEnv so :use lookups in
;; consuming packages see them as exported symbols.
(cl:import '(hydra_test_test_env_test_context hydra_test_test_env_test_graph)
           :hydra.test.testEnv)
(cl:export '(hydra_test_test_env_test_context hydra_test_test_env_test_graph)
           :hydra.test.testEnv)

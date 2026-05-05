;; Hand-written test environment for Emacs Lisp.
;; Provides the runtime test graph and test context referenced by the
;; generated hydra.test.test_graph module.
;;
;; Mirrors the role of:
;;   - heads/python/.../test_env.py
;;   - heads/lisp/scheme/.../test_env.scm
;;   - heads/lisp/common-lisp/.../test_env.lisp
;;   - heads/lisp/clojure/.../testEnv.clj
;;
;; The kernel filters hydra.test.testEnv from emitted output (via the
;; testSkipEmit set in each host bootstrap), so the auto-generated
;; test_graph.el module references hydra_test_test_env_test_context and
;; hydra_test_test_env_test_graph — both must be supplied by hand per
;; host language. This file is the Emacs Lisp implementation.

(require 'cl-lib)
(require 'hydra.lib.libraries)
(require 'hydra.rewriting)
(require 'hydra.scoping)
(require 'hydra.json.bootstrap)
(require 'hydra.annotation-bindings)

;; Empty context: alist-style structure matching hydra_context_context.
(defvar hydra_test_test_env_test_context
  (list (cons :functions nil)
        (cons :annotations nil)
        (cons :variable_types nil)))

;; Test graph builder. Curried to match the Emacs Lisp coder's emission
;; for multi-arg DSL functions: testGraph :: Map Name Type -> Map Name
;; Term -> Graph becomes (funcall (f types) terms) at the call site.
;; Returns a Graph populated with primitives, schema types (kernel +
;; caller-supplied test types), annotation bindings, and caller-supplied
;; test terms.
(defun hydra_test_test_env_test_graph (test-types)
  (lambda (test-terms)
    (let* ((std-prims (standard-library))
           (type-to-ts (lambda (typ)
                         (funcall hydra_scoping_f_type_to_type_scheme typ)))
           (boot-types-raw hydra_json_bootstrap_types_by_name)
           (kernel-schemas
             (mapcar (lambda (entry)
                       (list (car entry) (funcall type-to-ts (cdr entry))))
                     (hydra_lib_maps_to_list boot-types-raw)))
           (test-schemas
             (mapcar (lambda (entry)
                       (list (car entry) (funcall type-to-ts (cadr entry))))
                     (hydra_lib_maps_to_list test-types)))
           (schema-types
             (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
           (prim-map
             (hydra_lib_maps_from_list
               (mapcar (lambda (p) (list (car p) (cdr p))) std-prims)))
           (bound-terms
             (hydra_lib_maps_from_list
               (append (hydra-annotation-bindings)
                       (hydra_lib_maps_to_list test-terms)))))
      (list (cons :bound_terms bound-terms)
            (cons :bound_types nil)
            (cons :class_constraints nil)
            (cons :lambda_variables nil)
            (cons :metadata nil)
            (cons :primitives prim-map)
            (cons :schema_types schema-types)
            (cons :type_variables nil)))))

(provide 'hydra.test.testEnv)

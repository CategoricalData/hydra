;; Hand-written test environment for Clojure.
;; Provides the runtime test graph and test context referenced by the
;; generated hydra.test.testGraph namespace.
;;
;; Mirrors the role of:
;;   - heads/python/.../test_env.py
;;   - heads/lisp/scheme/.../test_env.scm
;;   - heads/lisp/common-lisp/.../test_env.lisp
;;
;; The kernel filters hydra.test.testEnv from emitted output (via the
;; testSkipEmit set in each host bootstrap), so the auto-generated
;; testGraph namespace references hydra_test_test_env_test_context and
;; hydra_test_test_env_test_graph — both must be supplied by hand per
;; host language. This file is the Clojure implementation.

(ns hydra.test.testEnv
  (:require [hydra.lib.libraries :refer :all]
            [hydra.rewriting :refer :all]
            [hydra.scoping :refer :all]
            [hydra.json.bootstrap :refer :all]
            [hydra.annotation-bindings :refer [annotation-bindings]]))

;; Empty context: alist-style map matching hydra_context_context shape.
(def hydra_test_test_env_test_context
  {:functions () :annotations () :variable_types {}})

;; Test graph builder. Curried to match the Clojure coder's emission
;; for multi-arg DSL functions: testGraph :: Map Name Type -> Map Name
;; Term -> Graph becomes ((f types) terms) at the call site. Returns a
;; Graph populated with primitives, schema types (kernel + caller-supplied
;; test types), annotation bindings, and caller-supplied test terms.
(defn hydra_test_test_env_test_graph [test-types]
  (fn [test-terms]
    (let [std-prims (standard-library)
          type-to-ts hydra_scoping_f_type_to_type_scheme
          boot-types-raw hydra_json_bootstrap_types_by_name
          kernel-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) boot-types-raw))
          test-types-list (seq test-types)
          test-schemas (into {} (map (fn [[k v]] [k (type-to-ts v)]) test-types-list))
          schema-types (merge kernel-schemas test-schemas)
          bound-terms (merge
            ;; Primitives are resolved via graphPrimitives, not boundTerms.
            (into {} (annotation-bindings))
            (into {} (seq test-terms)))]
      {:bound_terms bound-terms
       :bound_types {}
       :class_constraints {}
       :lambda_variables #{}
       :metadata {}
       :primitives std-prims
       :schema_types schema-types
       :type_variables #{}})))

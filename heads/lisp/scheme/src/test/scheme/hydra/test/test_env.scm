;; Hand-written test environment for Scheme.
;; Provides the runtime test graph and test context referenced by the
;; generated hydra.test.test_graph module.
;;
;; Mirrors the role of heads/python/.../hydra/test/test_env.py: the DSL
;; declares hydra.test.testEnv as a stub with the right signatures, and
;; testGraph emits calls into hydra_test_test_env_test_graph /
;; hydra_test_test_env_test_context. The actual graph build (primitives,
;; annotation bindings, schema types) is host-language specific and
;; lives here.
;;
;; The annotation-bindings helper is inlined here (rather than included
;; from ../annotation_bindings.scm) because Guile's R7RS `include`
;; resolves relative to the current working directory, not the source
;; file's directory, which makes a path-based include fragile across
;; the (dist scheme tests cd into src/test/scheme; the bootstrap demo
;; runs guile from the demo root) and the assemble + setup scripts.
;; Keeping the helpers here makes the library self-contained.

(define-library (hydra test test_env)
  (import (scheme base)
          (hydra core)
          (hydra context)
          (hydra graph)
          (hydra lib libraries)
          (hydra lib maps)
          (hydra json bootstrap)
          (hydra scoping))
  (export hydra_test_test_env_test_context
          hydra_test_test_env_test_graph)
  (begin

    ;; -------------------------------------------------------------------
    ;; Term-building helpers (mirror annotation_bindings.scm)
    (define (t-lam param body)
      (list 'lambda (make-hydra_core_lambda param '() body)))
    (define (t-var name)
      (list 'variable name))
    (define (t-app fun arg)
      (list 'application (make-hydra_core_application fun arg)))
    (define (t-prim name)
      (list 'variable name))
    (define (t-let name val body)
      (list 'let (make-hydra_core_let
                   (list (make-hydra_core_binding name val '()))
                   body)))
    (define (t-inject type-name field-name term)
      (list 'inject (make-hydra_core_injection type-name
                     (make-hydra_core_field field-name term))))
    (define (t-record type-name fields)
      (list 'record (make-hydra_core_record type-name fields)))
    (define (t-field name term)
      (make-hydra_core_field name term))
    (define (t-project type-name field-name)
      (list 'project (make-hydra_core_projection type-name field-name)))
    (define (t-match type-name default . case-fields)
      (list 'cases (make-hydra_core_case_statement type-name default case-fields)))
    (define (t-right v) (list 'either (list 'right v)))
    (define (t-left v) (list 'either (list 'left v)))
    (define (t-just v) (list 'maybe (t-inject "hydra.core.Term" "literal"
                         (t-inject "hydra.core.Literal" "string" v))))
    (define (t-nothing) (list 'maybe (list 'nothing '())))

    ;; Annotation term-level bindings (mirrors Java TestSuiteRunner.addAnnotationsBindings)
    (define (annotation-bindings)
      (list
        (list "hydra.constants.key_classes"
              (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                            (list 'literal (list 'string "classes")))))
        (list "hydra.constants.key_description"
              (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                            (list 'literal (list 'string "description")))))
        (list "hydra.constants.key_type"
              (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                            (list 'literal (list 'string "type")))))
        (list "hydra.constants.key_debugId"
              (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                            (list 'literal (list 'string "debugId")))))
        (list "hydra.constants.key_firstClassType"
              (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                            (list 'literal (list 'string "firstClassType")))))

        (list "hydra.rewriting.deannotateTerm"
              (t-lam "t"
                (t-app
                  (t-match "hydra.core.Term" (list 'just (t-var "t"))
                    (t-field "annotated"
                      (t-lam "at"
                        (t-app (t-var "hydra.rewriting.deannotateTerm")
                          (t-app (t-project "hydra.core.AnnotatedTerm" "body")
                            (t-var "at"))))))
                  (t-var "t"))))

        (list "hydra.annotations.termAnnotationInternal"
              (t-lam "term"
                (t-let "toPairs"
                  (t-lam "rest"
                    (t-lam "t"
                      (t-app
                        (t-match "hydra.core.Term" (list 'just (t-var "rest"))
                          (t-field "annotated"
                            (t-lam "at"
                              (t-app
                                (t-app (t-var "toPairs")
                                  (t-app (t-app (t-prim "hydra.lib.lists.cons")
                                    (t-app (t-prim "hydra.lib.maps.toList")
                                      (t-app (t-project "hydra.core.AnnotatedTerm" "annotation")
                                        (t-var "at"))))
                                    (t-var "rest")))
                                (t-app (t-project "hydra.core.AnnotatedTerm" "body")
                                  (t-var "at"))))))
                        (t-var "t"))))
                  (t-app (t-prim "hydra.lib.maps.fromList")
                    (t-app (t-prim "hydra.lib.lists.concat")
                      (t-app (t-app (t-var "toPairs") (list 'list '()))
                        (t-var "term")))))))

        (list "hydra.annotations.setAnnotation"
              (t-lam "key"
                (t-lam "val"
                  (t-lam "m"
                    (t-app (t-app (t-app (t-prim "hydra.lib.maybes.maybe")
                      (t-app (t-app (t-prim "hydra.lib.maps.delete") (t-var "key")) (t-var "m")))
                      (t-lam "v"
                        (t-app (t-app (t-app (t-prim "hydra.lib.maps.insert")
                          (t-var "key")) (t-var "v")) (t-var "m"))))
                      (t-var "val"))))))

        (list "hydra.annotations.setTermAnnotation"
              (t-lam "key"
                (t-lam "val"
                  (t-lam "term"
                    (t-let "stripped"
                      (t-app (t-var "hydra.rewriting.deannotateTerm") (t-var "term"))
                      (t-let "anns"
                        (t-app (t-app (t-app (t-var "hydra.annotations.setAnnotation")
                          (t-var "key")) (t-var "val"))
                          (t-app (t-var "hydra.annotations.termAnnotationInternal") (t-var "term")))
                        (t-app (t-app (t-app (t-prim "hydra.lib.logic.ifElse")
                          (t-app (t-prim "hydra.lib.maps.null") (t-var "anns")))
                          (t-var "stripped"))
                          (t-inject "hydra.core.Term" "annotated"
                            (t-record "hydra.core.AnnotatedTerm"
                              (list (t-field "body" (t-var "stripped"))
                                    (t-field "annotation" (t-var "anns"))))))))))))

        (list "hydra.annotations.setTermDescription"
              (t-lam "d"
                (t-app (t-app (t-var "hydra.annotations.setTermAnnotation")
                  (t-var "hydra.constants.key_description"))
                  (t-app (t-app (t-prim "hydra.lib.maybes.map")
                    (t-lam "s"
                      (t-inject "hydra.core.Term" "literal"
                        (t-inject "hydra.core.Literal" "string" (t-var "s")))))
                    (t-var "d")))))

        (list "hydra.annotations.getTermAnnotation"
              (t-lam "key"
                (t-lam "term"
                  (t-app (t-app (t-prim "hydra.lib.maps.lookup") (t-var "key"))
                    (t-app (t-var "hydra.annotations.termAnnotationInternal")
                      (t-var "term"))))))

        (list "hydra.annotations.getDescription"
              (t-lam "cx"
                (t-lam "g"
                  (t-lam "anns"
                    (t-app (t-app (t-app (t-prim "hydra.lib.maybes.maybe")
                      (t-right (list 'maybe (list 'nothing '()))))
                      (t-lam "descTerm"
                        (t-app
                          (t-match "hydra.core.Term"
                            (list 'just (t-right (list 'maybe (list 'nothing '()))))
                            (t-field "literal"
                              (t-lam "lit"
                                (t-app
                                  (t-match "hydra.core.Literal"
                                    (list 'just (t-right (list 'maybe (list 'nothing '()))))
                                    (t-field "string"
                                      (t-lam "s"
                                        (t-right (list 'maybe (t-var "s"))))))
                                  (t-var "lit")))))
                          (t-var "descTerm"))))
                      (t-app (t-app (t-prim "hydra.lib.maps.lookup")
                        (t-var "hydra.constants.key_description"))
                        (t-var "anns")))))))

        (list "hydra.annotations.getTermDescription"
              (t-lam "cx"
                (t-lam "g"
                  (t-lam "term"
                    (t-let "peel"
                      (t-lam "t"
                        (t-app
                          (t-match "hydra.core.Term" (list 'just (t-var "t"))
                            (t-field "typeLambda"
                              (t-lam "tl"
                                (t-app (t-var "peel")
                                  (t-app (t-project "hydra.core.TypeLambda" "body")
                                    (t-var "tl")))))
                            (t-field "typeApplication"
                              (t-lam "ta"
                                (t-app (t-var "peel")
                                  (t-app (t-project "hydra.core.TypeApplicationTerm" "body")
                                    (t-var "ta"))))))
                          (t-var "t")))
                      (t-app (t-app (t-app (t-var "hydra.annotations.getDescription")
                        (t-var "cx")) (t-var "g"))
                        (t-app (t-var "hydra.annotations.termAnnotationInternal")
                          (t-app (t-var "peel") (t-var "term"))))))))))
      )

    ;; -------------------------------------------------------------------
    ;; Public API: testEnv exports

    (define hydra_test_test_env_test_context
      (make-hydra_context_context (list) (list) hydra_lib_maps_empty))

    (define (hydra_test_test_env_test_graph test-types)
      (let* ((all-prims (standard-library))
             (type-to-ts hydra_scoping_f_type_to_type_scheme)
             (kernel-schemas
               (map (lambda (entry)
                      (list (car entry) (type-to-ts (cdr entry))))
                    hydra_json_bootstrap_types_by_name))
             (test-schemas
               (map (lambda (entry)
                      (list (car entry) (type-to-ts (cadr entry))))
                    (hydra_lib_maps_to_list test-types)))
             (schema-types
               (hydra_lib_maps_from_list
                 (append kernel-schemas test-schemas)))
             (bound-terms
               (append
                 (annotation-bindings)
                 (list (list "hydra.monads.emptyContext" (list 'unit '()))
                       (list "hydra.lexical.emptyGraph" (list 'unit '()))))))
        (make-hydra_graph_graph
          (hydra_lib_maps_from_list bound-terms)
          hydra_lib_maps_empty
          (list)
          (list)
          hydra_lib_maps_empty
          (hydra_lib_maps_from_list
            (map (lambda (p) (list (car p) (cdr p))) all-prims))
          schema-types
          (list))))))

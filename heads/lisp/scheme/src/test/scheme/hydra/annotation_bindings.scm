;; ==========================================================================

;; Term-building helpers for annotation bindings
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
    ;; hydra.constants
    (list "hydra.constants.keyClasses"
          (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                        (list 'literal (list 'string "classes")))))
    (list "hydra.constants.keyDescription"
          (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                        (list 'literal (list 'string "description")))))
    (list "hydra.constants.keyType"
          (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                        (list 'literal (list 'string "type")))))
    (list "hydra.constants.keyDebugId"
          (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                        (list 'literal (list 'string "debugId")))))
    (list "hydra.constants.keyFirstClassType"
          (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                        (list 'literal (list 'string "firstClassType")))))

    ;; hydra.rewriting.deannotateTerm = \t -> case t of
    ;;   annotated(at) -> deannotateTerm(at.body)
    ;;   _ -> t
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

    ;; hydra.annotations.termAnnotationInternal = \term ->
    ;;   let toPairs = \rest -> \t -> case t of
    ;;     annotated(at) -> toPairs(cons(toList(at.annotation), rest), at.body)
    ;;     _ -> rest
    ;;   in fromList(concat(toPairs([], term)))
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

    ;; hydra.annotations.setAnnotation = \key -> \val -> \m ->
    ;;   maybe(delete(key, m), \v -> insert(key, v, m), val)
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

    ;; hydra.annotations.setTermAnnotation = \key -> \val -> \term ->
    ;;   let stripped = deannotateTerm(term)
    ;;       anns = setAnnotation(key, val, termAnnotationInternal(term))
    ;;   in if null(anns) then stripped
    ;;      else inject(Term){annotated=record(AnnotatedTerm){body=stripped, annotation=anns}}
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

    ;; hydra.annotations.setTermDescription = \d ->
    ;;   setTermAnnotation(keyDescription, maybes.map(\s -> inject(Term, literal, inject(Literal, string, s)), d))
    (list "hydra.annotations.setTermDescription"
          (t-lam "d"
            (t-app (t-app (t-var "hydra.annotations.setTermAnnotation")
              (t-var "hydra.constants.keyDescription"))
              (t-app (t-app (t-prim "hydra.lib.maybes.map")
                (t-lam "s"
                  (t-inject "hydra.core.Term" "literal"
                    (t-inject "hydra.core.Literal" "string" (t-var "s")))))
                (t-var "d")))))

    ;; hydra.annotations.getTermAnnotation = \key -> \term ->
    ;;   maps.lookup(key, termAnnotationInternal(term))
    (list "hydra.annotations.getTermAnnotation"
          (t-lam "key"
            (t-lam "term"
              (t-app (t-app (t-prim "hydra.lib.maps.lookup") (t-var "key"))
                (t-app (t-var "hydra.annotations.termAnnotationInternal")
                  (t-var "term"))))))

    ;; hydra.annotations.getDescription = \cx -> \g -> \anns ->
    ;;   maybe(right(nothing),
    ;;         \descTerm -> match Term { literal(\lit -> match Literal { string(\s -> right(just(s))) }) },
    ;;         maps.lookup(keyDescription, anns))
    (list "hydra.annotations.getDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "anns"
                (t-app (t-app (t-app (t-prim "hydra.lib.maybes.maybe")
                  ;; default: right(nothing)
                  (t-right (list 'maybe (list 'nothing '()))))
                  ;; \descTerm -> case match to extract string
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
                  ;; maps.lookup(keyDescription, anns)
                  (t-app (t-app (t-prim "hydra.lib.maps.lookup")
                    (t-var "hydra.constants.keyDescription"))
                    (t-var "anns")))))))

    ;; hydra.annotations.getTermDescription = \cx -> \g -> \term ->
    ;;   let peel = \t -> case t of
    ;;     typeLambda(tl) -> peel(tl.body)
    ;;     typeApplication(ta) -> peel(ta.body)
    ;;     _ -> t
    ;;   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
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

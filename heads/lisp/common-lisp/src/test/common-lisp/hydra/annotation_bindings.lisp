;; ==========================================================================

;; Term-building helpers for annotation bindings
(cl:defun t-lam (param body)
  (cl:list :lambda (make-hydra_core_model_lambda :parameter param :domain cl:nil :body body)))
(cl:defun t-var (name)
  (cl:list :variable name))
;; Variadic for left-associative curried application (#443).
(cl:defun t-app (fun cl:&rest args)
  (cl:reduce (cl:lambda (acc x)
               (cl:list :application
                 (make-hydra_core_model_application :function acc :argument x)))
             args :initial-value fun))
(cl:defun t-prim (name)
  (cl:list :variable name))
(cl:defun t-let (name val body)
  (cl:list :let (make-hydra_core_model_let
                  :bindings (cl:list (make-hydra_core_model_binding :name name :term val :type_scheme cl:nil))
                  :body body)))
(cl:defun t-inject (type-name field-name term)
  (cl:list :inject (make-hydra_core_model_injection
                    :type_name type-name
                    :field (make-hydra_core_model_field :name field-name :term term))))
(cl:defun t-record (type-name fields)
  (cl:list :record (make-hydra_core_model_record :type_name type-name :fields fields)))
(cl:defun t-field (name term)
  (make-hydra_core_model_field :name name :term term))
(cl:defun t-project (type-name field-name)
  (cl:list :project (make-hydra_core_model_projection :type_name type-name :field_name field-name)))
(cl:defun t-match (type-name default &rest case-fields)
  ;; CaseStatement.cases is [CaseAlternative{name,handler}] (#369); callers build
  ;; cases via t-field (Field{name,term}), so convert each to a CaseAlternative.
  (cl:list :cases (make-hydra_core_model_case_statement
                    :type_name type-name :default default
                    :cases (cl:mapcar
                             (cl:lambda (f)
                               (make-hydra_core_model_case_alternative
                                 :name (hydra_core_model_field-name f)
                                 :handler (hydra_core_model_field-term f)))
                             case-fields))))
(cl:defun t-right (v) (cl:list :either (cl:list :right v)))
(cl:defun t-left (v) (cl:list :either (cl:list :left v)))
(cl:defun t-just (v) (cl:list :optional (t-inject "hydra.core.model.Term" "literal"
                       (t-inject "hydra.core.model.Literal" "string" v))))
(cl:defun t-nothing () (cl:list :optional (cl:list :none cl:nil)))
;; Build a Term.pair value at Term-AST level: a 2-element pair payload.
;; Mirrors Java's hydra.core.dsl.Terms.pair helper (#443).
(cl:defun t-pair (a b) (cl:list :pair (cl:list a b)))

;; Annotation term-level bindings (mirrors Java TestSuiteRunner.addAnnotationsBindings)
(cl:defun annotation-bindings ()
  (cl:list
    ;; hydra.core.constants
    (cl:list "hydra.core.constants.keyClasses"
          (cl:list :wrap (make-hydra_core_model_wrapped_term
                           :type_name "hydra.core.model.Name"
                           :body (cl:list :literal (cl:list :string "classes")))))
    (cl:list "hydra.core.constants.keyDescription"
          (cl:list :wrap (make-hydra_core_model_wrapped_term
                           :type_name "hydra.core.model.Name"
                           :body (cl:list :literal (cl:list :string "description")))))
    (cl:list "hydra.core.constants.keyType"
          (cl:list :wrap (make-hydra_core_model_wrapped_term
                           :type_name "hydra.core.model.Name"
                           :body (cl:list :literal (cl:list :string "type")))))
    (cl:list "hydra.core.constants.keyDebugId"
          (cl:list :wrap (make-hydra_core_model_wrapped_term
                           :type_name "hydra.core.model.Name"
                           :body (cl:list :literal (cl:list :string "debugId")))))
    (cl:list "hydra.core.constants.keyFirstClassType"
          (cl:list :wrap (make-hydra_core_model_wrapped_term
                           :type_name "hydra.core.model.Name"
                           :body (cl:list :literal (cl:list :string "firstClassType")))))

    ;; hydra.core.rewriting.deannotateTerm = \t -> case t of
    ;;   annotated(at) -> deannotateTerm(at.body)
    ;;   _ -> t
    (cl:list "hydra.core.rewriting.deannotateTerm"
          (t-lam "t"
            (t-app
              (t-match "hydra.core.model.Term" (cl:list :given (t-var "t"))
                (t-field "annotated"
                  (t-lam "at"
                    (t-app (t-var "hydra.core.rewriting.deannotateTerm")
                      (t-app (t-project "hydra.core.model.AnnotatedTerm" "body")
                        (t-var "at"))))))
              (t-var "t"))))

    ;; hydra.core.annotations.getAnnotationMap (#386):
    ;;   getAnnotationMap :: Term -> Map<Name, Term>
    ;;   Project (Name, value) entries from a TermMap with TermVariable keys;
    ;;   return Maps.empty for any other Term shape.
    (cl:list "hydra.core.annotations.getAnnotationMap"
          (t-lam "t"
            (t-app
              (t-match "hydra.core.model.Term" (cl:list :given (t-app (t-prim "hydra.core.lib.maps.empty") (t-var "t")))
                (t-field "map"
                  (t-lam "m"
                    (t-app (t-prim "hydra.core.lib.maps.fromList")
                      (t-app (t-app (t-prim "hydra.core.lib.lists.foldl")
                        (t-lam "acc"
                          (t-lam "pair"
                            (t-app
                              (t-match "hydra.core.model.Term"
                                (cl:list :given (t-var "acc"))
                                (t-field "variable"
                                  (t-lam "n"
                                    (t-app (t-app (t-prim "hydra.core.lib.lists.cons")
                                      (t-pair
                                        (t-var "n")
                                        (t-app (t-prim "hydra.core.lib.pairs.second") (t-var "pair"))))
                                      (t-var "acc")))))
                              (t-app (t-prim "hydra.core.lib.pairs.first") (t-var "pair")))))
                        (cl:list :list cl:nil))
                        (t-app (t-prim "hydra.core.lib.maps.toList") (t-var "m")))))))
              (t-var "t"))))

    ;; hydra.core.annotations.wrapAnnotationMap (#386):
    ;;   wrapAnnotationMap :: Map<Name, Term> -> Term
    ;;   Encode each Name key as a TermVariable, then wrap as a TermMap.
    (cl:list "hydra.core.annotations.wrapAnnotationMap"
          (t-lam "m"
            (t-inject "hydra.core.model.Term" "map"
              (t-app (t-prim "hydra.core.lib.maps.fromList")
                (t-app (t-app (t-prim "hydra.core.lib.lists.map")
                  (t-lam "pair"
                    (t-pair
                      (t-inject "hydra.core.model.Term" "variable"
                        (t-app (t-prim "hydra.core.lib.pairs.first") (t-var "pair")))
                      (t-app (t-prim "hydra.core.lib.pairs.second") (t-var "pair")))))
                  (t-app (t-prim "hydra.core.lib.maps.toList") (t-var "m")))))))

    ;; hydra.core.annotations.termAnnotationInternal = \term ->
    ;;   let toPairs = \rest -> \t -> case t of
    ;;     annotated(at) -> toPairs(cons(toList(getAnnotationMap(at.annotation)), rest), at.body)
    ;;     _ -> rest
    ;;   in fromList(concat(toPairs([], term)))
    ;; After #386: at.annotation is a Term; project the map payload via
    ;; hydra.core.annotations.getAnnotationMap before calling maps.toList.
    (cl:list "hydra.core.annotations.termAnnotationInternal"
          (t-lam "term"
            (t-let "toPairs"
              (t-lam "rest"
                (t-lam "t"
                  (t-app
                    (t-match "hydra.core.model.Term" (cl:list :given (t-var "rest"))
                      (t-field "annotated"
                        (t-lam "at"
                          (t-app
                            (t-app (t-var "toPairs")
                              (t-app (t-app (t-prim "hydra.core.lib.lists.cons")
                                (t-app (t-prim "hydra.core.lib.maps.toList")
                                  (t-app (t-var "hydra.core.annotations.getAnnotationMap")
                                    (t-app (t-project "hydra.core.model.AnnotatedTerm" "annotation")
                                      (t-var "at")))))
                                (t-var "rest")))
                            (t-app (t-project "hydra.core.model.AnnotatedTerm" "body")
                              (t-var "at"))))))
                    (t-var "t"))))
              (t-app (t-prim "hydra.core.lib.maps.fromList")
                (t-app (t-prim "hydra.core.lib.lists.concat")
                  (t-app (t-app (t-var "toPairs") (cl:list :list cl:nil))
                    (t-var "term")))))))

    ;; hydra.core.annotations.setAnnotation = \key -> \val -> \m ->
    ;;   maybe(delete(key, m), \v -> insert(key, v, m), val)
    (cl:list "hydra.core.annotations.setAnnotation"
          (t-lam "key"
            (t-lam "val"
              (t-lam "m"
                (t-app (t-app (t-app (t-prim "hydra.core.lib.optionals.match")
                  (t-var "val"))
                  (t-app (t-app (t-prim "hydra.core.lib.maps.delete") (t-var "key")) (t-var "m")))
                  (t-lam "v"
                    (t-app (t-app (t-app (t-prim "hydra.core.lib.maps.insert")
                      (t-var "key")) (t-var "v")) (t-var "m"))))))))

    ;; hydra.core.annotations.setTermAnnotation = \key -> \val -> \term ->
    ;;   let stripped = deannotateTerm(term)
    ;;       anns = setAnnotation(key, val, termAnnotationInternal(term))
    ;;   in if null(anns) then stripped
    ;;      else inject(Term){annotated=record(AnnotatedTerm){body=stripped,
    ;;                                              annotation=wrapAnnotationMap(anns)}}
    ;; After #386: wrap the resulting Map<Name, Term> via wrapAnnotationMap
    ;; before storing it in AnnotatedTerm.annotation (which is now a Term).
    (cl:list "hydra.core.annotations.setTermAnnotation"
          (t-lam "key"
            (t-lam "val"
              (t-lam "term"
                (t-let "stripped"
                  (t-app (t-var "hydra.core.rewriting.deannotateTerm") (t-var "term"))
                  (t-let "anns"
                    (t-app (t-app (t-app (t-var "hydra.core.annotations.setAnnotation")
                      (t-var "key")) (t-var "val"))
                      (t-app (t-var "hydra.core.annotations.termAnnotationInternal") (t-var "term")))
                    (t-app (t-app (t-app (t-prim "hydra.core.lib.logic.ifElse")
                      (t-app (t-prim "hydra.core.lib.maps.isEmpty") (t-var "anns")))
                      (t-var "stripped"))
                      (t-inject "hydra.core.model.Term" "annotated"
                        (t-record "hydra.core.model.AnnotatedTerm"
                          (cl:list (t-field "body" (t-var "stripped"))
                                (t-field "annotation"
                                  (t-app (t-var "hydra.core.annotations.wrapAnnotationMap")
                                    (t-var "anns")))))))))))))

    ;; hydra.core.annotations.setTermDescription = \d ->
    ;;   setTermAnnotation(keyDescription, optionals.map(\s -> inject(Term, literal, inject(Literal, string, s)), d))
    (cl:list "hydra.core.annotations.setTermDescription"
          (t-lam "d"
            (t-app (t-app (t-var "hydra.core.annotations.setTermAnnotation")
              (t-var "hydra.core.constants.keyDescription"))
              (t-app (t-app (t-prim "hydra.core.lib.optionals.map")
                (t-lam "s"
                  (t-inject "hydra.core.model.Term" "literal"
                    (t-inject "hydra.core.model.Literal" "string" (t-var "s")))))
                (t-var "d")))))

    ;; hydra.core.annotations.getTermAnnotation = \key -> \term ->
    ;;   maps.lookup(key, termAnnotationInternal(term))
    (cl:list "hydra.core.annotations.getTermAnnotation"
          (t-lam "key"
            (t-lam "term"
              (t-app (t-app (t-prim "hydra.core.lib.maps.lookup") (t-var "key"))
                (t-app (t-var "hydra.core.annotations.termAnnotationInternal")
                  (t-var "term"))))))

    ;; hydra.core.annotations.getDescription = \cx -> \g -> \anns ->
    ;;   maybe(right(nothing),
    ;;         \descTerm -> match Term { literal(\lit -> match Literal { string(\s -> right(just(s))) }) },
    ;;         maps.lookup(keyDescription, anns))
    (cl:list "hydra.core.annotations.getDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "anns"
                (t-app (t-app (t-app (t-prim "hydra.core.lib.optionals.match")
                  ;; scrutinee: maps.lookup(keyDescription, anns)
                  (t-app (t-app (t-prim "hydra.core.lib.maps.lookup")
                    (t-var "hydra.core.constants.keyDescription"))
                    (t-var "anns")))
                  ;; default: right(nothing)
                  (t-right (cl:list :optional (cl:list :none cl:nil))))
                  ;; \descTerm -> case match to extract string
                  (t-lam "descTerm"
                    (t-app
                      (t-match "hydra.core.model.Term"
                        (cl:list :given (t-right (cl:list :optional (cl:list :none cl:nil))))
                        (t-field "literal"
                          (t-lam "lit"
                            (t-app
                              (t-match "hydra.core.model.Literal"
                                (cl:list :given (t-right (cl:list :optional (cl:list :none cl:nil))))
                                (t-field "string"
                                  (t-lam "s"
                                    (t-right (cl:list :optional (t-var "s"))))))
                              (t-var "lit")))))
                      (t-var "descTerm"))))))))

    ;; hydra.core.annotations.getTermDescription = \cx -> \g -> \term ->
    ;;   let peel = \t -> case t of
    ;;     typeLambda(tl) -> peel(tl.body)
    ;;     typeApplication(ta) -> peel(ta.body)
    ;;     _ -> t
    ;;   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
    (cl:list "hydra.core.annotations.getTermDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "term"
                (t-let "peel"
                  (t-lam "t"
                    (t-app
                      (t-match "hydra.core.model.Term" (cl:list :given (t-var "t"))
                        (t-field "typeLambda"
                          (t-lam "tl"
                            (t-app (t-var "peel")
                              (t-app (t-project "hydra.core.model.TypeLambda" "body")
                                (t-var "tl")))))
                        (t-field "typeApplication"
                          (t-lam "ta"
                            (t-app (t-var "peel")
                              (t-app (t-project "hydra.core.model.TypeApplicationTerm" "body")
                                (t-var "ta"))))))
                      (t-var "t")))
                  (t-app (t-app (t-app (t-var "hydra.core.annotations.getDescription")
                    (t-var "cx")) (t-var "g"))
                    (t-app (t-var "hydra.core.annotations.termAnnotationInternal")
                      (t-app (t-var "peel") (t-var "term"))))))))))
)

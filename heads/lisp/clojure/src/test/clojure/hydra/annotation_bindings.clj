(ns hydra.annotation-bindings
  (:require [hydra.core.model :refer :all]))

;; ==========================================================================

;; Term-building helpers for annotation bindings
(defn- t-lam [param body]
  (list :lambda (->hydra_core_model_lambda param nil body)))
(defn- t-var [name]
  (list :variable name))
;; Variadic for left-associative curried application (mirrors Java's
;; apply(func, args...) overload; #443).
(defn- t-app [fun & args]
  (reduce (fn [acc x] (list :application (->hydra_core_model_application acc x)))
          fun args))
(defn- t-prim [name]
  (list :variable name))
(defn- t-let [name val body]
  (list :let (->hydra_core_model_let
               (list (->hydra_core_model_binding name val nil))
               body)))
(defn- t-inject [type-name field-name term]
  (list :inject (->hydra_core_model_injection type-name
                 (->hydra_core_model_field field-name term))))
(defn- t-record [type-name fields]
  (list :record (->hydra_core_model_record type-name fields)))
(defn- t-field [name term]
  (->hydra_core_model_field name term))
(defn- t-project [type-name field-name]
  (list :project (->hydra_core_model_projection type-name field-name)))
(defn- t-match [type-name default & case-fields]
  ;; CaseStatement.cases is [CaseAlternative{name,handler}] (#369); callers build
  ;; cases via t-field (Field{name,term}), so convert each to a CaseAlternative.
  (list :cases (->hydra_core_model_case_statement type-name default
                 (map (fn [f] (->hydra_core_model_case_alternative (:name f) (:term f))) case-fields))))
(defn- t-right [v] (list :either (list :right v)))
(defn- t-left [v] (list :either (list :left v)))
(defn- t-just [v] (list :optional (t-inject "hydra.core.model.Term" "literal"
                     (t-inject "hydra.core.model.Literal" "string" v))))
(defn- t-nothing [] (list :optional (list :none nil)))
;; Build a Term.pair value at Term-AST level (mirrors Java Terms.pair, #443).
(defn- t-pair [a b] (list :pair (list a b)))

;; Annotation term-level bindings (mirrors Java TestSuiteRunner.addAnnotationsBindings)
(defn annotation-bindings []
  (list
    ;; hydra.core.constants
    (vector "hydra.core.constants.keyClasses"
          (list :wrap (->hydra_core_model_wrapped_term "hydra.core.model.Name"
                        (list :literal (list :string "classes")))))
    (vector "hydra.core.constants.keyDescription"
          (list :wrap (->hydra_core_model_wrapped_term "hydra.core.model.Name"
                        (list :literal (list :string "description")))))
    (vector "hydra.core.constants.keyType"
          (list :wrap (->hydra_core_model_wrapped_term "hydra.core.model.Name"
                        (list :literal (list :string "type")))))
    (vector "hydra.core.constants.keyDebugId"
          (list :wrap (->hydra_core_model_wrapped_term "hydra.core.model.Name"
                        (list :literal (list :string "debugId")))))
    (vector "hydra.core.constants.keyFirstClassType"
          (list :wrap (->hydra_core_model_wrapped_term "hydra.core.model.Name"
                        (list :literal (list :string "firstClassType")))))

    ;; hydra.core.rewriting.deannotateTerm = \t -> case t of
    ;;   annotated(at) -> deannotateTerm(at.body)
    ;;   _ -> t
    (vector "hydra.core.rewriting.deannotateTerm"
          (t-lam "t"
            (t-app
              (t-match "hydra.core.model.Term" (list :given (t-var "t"))
                (t-field "annotated"
                  (t-lam "at"
                    (t-app (t-var "hydra.core.rewriting.deannotateTerm")
                      (t-app (t-project "hydra.core.model.AnnotatedTerm" "body")
                        (t-var "at"))))))
              (t-var "t"))))

    ;; hydra.core.annotations.getAnnotationMap (#386):
    ;;   getAnnotationMap :: Term -> Map<Name, Term>
    (vector "hydra.core.annotations.getAnnotationMap"
          (t-lam "t"
            (t-app
              (t-match "hydra.core.model.Term" (list :given (t-app (t-prim "hydra.core.lib.maps.empty") (t-var "t")))
                (t-field "map"
                  (t-lam "m"
                    (t-app (t-prim "hydra.core.lib.maps.fromList")
                      (t-app (t-app (t-prim "hydra.core.lib.lists.foldl")
                        (t-lam "acc"
                          (t-lam "pair"
                            (t-app
                              (t-match "hydra.core.model.Term"
                                (list :given (t-var "acc"))
                                (t-field "variable"
                                  (t-lam "n"
                                    (t-app (t-app (t-prim "hydra.core.lib.lists.cons")
                                      (t-pair
                                        (t-var "n")
                                        (t-app (t-prim "hydra.core.lib.pairs.second") (t-var "pair"))))
                                      (t-var "acc")))))
                              (t-app (t-prim "hydra.core.lib.pairs.first") (t-var "pair")))))
                        (list :list '()))
                        (t-app (t-prim "hydra.core.lib.maps.toList") (t-var "m")))))))
              (t-var "t"))))

    ;; hydra.core.annotations.wrapAnnotationMap (#386):
    ;;   wrapAnnotationMap :: Map<Name, Term> -> Term
    (vector "hydra.core.annotations.wrapAnnotationMap"
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
    ;; After #386: at.annotation is a Term; project via getAnnotationMap first.
    (vector "hydra.core.annotations.termAnnotationInternal"
          (t-lam "term"
            (t-let "toPairs"
              (t-lam "rest"
                (t-lam "t"
                  (t-app
                    (t-match "hydra.core.model.Term" (list :given (t-var "rest"))
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
                  (t-app (t-app (t-var "toPairs") (list :list '()))
                    (t-var "term")))))))

    ;; hydra.core.annotations.setAnnotation = \key -> \val -> \m ->
    ;;   maybe(delete(key, m), \v -> insert(key, v, m), val)
    (vector "hydra.core.annotations.setAnnotation"
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
    (vector "hydra.core.annotations.setTermAnnotation"
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
                          (list (t-field "body" (t-var "stripped"))
                                (t-field "annotation"
                                  (t-app (t-var "hydra.core.annotations.wrapAnnotationMap")
                                    (t-var "anns")))))))))))))

    ;; hydra.core.annotations.setTermDescription = \d ->
    ;;   setTermAnnotation(keyDescription, optionals.map(\s -> inject(Term, literal, inject(Literal, string, s)), d))
    (vector "hydra.core.annotations.setTermDescription"
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
    (vector "hydra.core.annotations.getTermAnnotation"
          (t-lam "key"
            (t-lam "term"
              (t-app (t-app (t-prim "hydra.core.lib.maps.lookup") (t-var "key"))
                (t-app (t-var "hydra.core.annotations.termAnnotationInternal")
                  (t-var "term"))))))

    ;; hydra.core.annotations.getDescription = \cx -> \g -> \anns ->
    ;;   maybe(right(nothing),
    ;;         \descTerm -> match Term { literal(\lit -> match Literal { string(\s -> right(just(s))) }) },
    ;;         maps.lookup(keyDescription, anns))
    (vector "hydra.core.annotations.getDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "anns"
                (t-app (t-app (t-app (t-prim "hydra.core.lib.optionals.match")
                  ;; scrutinee: maps.lookup(keyDescription, anns)
                  (t-app (t-app (t-prim "hydra.core.lib.maps.lookup")
                    (t-var "hydra.core.constants.keyDescription"))
                    (t-var "anns")))
                  ;; default: right(nothing)
                  (t-right (list :optional (list :none nil))))
                  ;; \descTerm -> case match to extract string
                  (t-lam "descTerm"
                    (t-app
                      (t-match "hydra.core.model.Term"
                        (list :given (t-right (list :optional (list :none nil))))
                        (t-field "literal"
                          (t-lam "lit"
                            (t-app
                              (t-match "hydra.core.model.Literal"
                                (list :given (t-right (list :optional (list :none nil))))
                                (t-field "string"
                                  (t-lam "s"
                                    (t-right (list :optional (t-var "s"))))))
                              (t-var "lit")))))
                      (t-var "descTerm"))))))))

    ;; hydra.core.annotations.getTermDescription = \cx -> \g -> \term ->
    ;;   let peel = \t -> case t of
    ;;     typeLambda(tl) -> peel(tl.body)
    ;;     typeApplication(ta) -> peel(ta.body)
    ;;     _ -> t
    ;;   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
    (vector "hydra.core.annotations.getTermDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "term"
                (t-let "peel"
                  (t-lam "t"
                    (t-app
                      (t-match "hydra.core.model.Term" (list :given (t-var "t"))
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

;; ==========================================================================

;; Term-building helpers for annotation bindings
(cl:defun t-lam (param body)
  (cl:list :function (cl:list :lambda (make-hydra_core_lambda :parameter param :domain cl:nil :body body))))
(cl:defun t-var (name)
  (cl:list :variable name))
(cl:defun t-app (fun arg)
  (cl:list :application (make-hydra_core_application :function fun :argument arg)))
(cl:defun t-prim (name)
  (cl:list :function (cl:list :primitive name)))
(cl:defun t-let (name val body)
  (cl:list :let (make-hydra_core_let
                  :bindings (cl:list (make-hydra_core_binding :name name :term val :type cl:nil))
                  :body body)))
(cl:defun t-inject (type-name field-name term)
  (cl:list :union (make-hydra_core_injection
                    :type_name type-name
                    :field (make-hydra_core_field :name field-name :term term))))
(cl:defun t-record (type-name fields)
  (cl:list :record (make-hydra_core_record :type_name type-name :fields fields)))
(cl:defun t-field (name term)
  (make-hydra_core_field :name name :term term))
(cl:defun t-project (type-name field-name)
  (cl:list :function (cl:list :elimination
    (cl:list :record (make-hydra_core_projection :type_name type-name :field field-name)))))
(cl:defun t-match (type-name default &rest case-fields)
  (cl:list :function (cl:list :elimination
    (cl:list :union (make-hydra_core_case_statement
                      :type_name type-name :default default :cases case-fields)))))
(cl:defun t-right (v) (cl:list :either (cl:list :right v)))
(cl:defun t-left (v) (cl:list :either (cl:list :left v)))
(cl:defun t-just (v) (cl:list :maybe (t-inject "hydra.core.Term" "literal"
                       (t-inject "hydra.core.Literal" "string" v))))
(cl:defun t-nothing () (cl:list :maybe (cl:list :nothing cl:nil)))

;; Annotation term-level bindings (mirrors Java TestSuiteRunner.addAnnotationsBindings)
(cl:defun annotation-bindings ()
  (cl:list
    ;; hydra.constants
    (cl:list "hydra.constants.key_classes"
          (cl:list :wrap (make-hydra_core_wrapped_term
                           :type_name "hydra.core.Name"
                           :body (cl:list :literal (cl:list :string "classes")))))
    (cl:list "hydra.constants.key_description"
          (cl:list :wrap (make-hydra_core_wrapped_term
                           :type_name "hydra.core.Name"
                           :body (cl:list :literal (cl:list :string "description")))))
    (cl:list "hydra.constants.key_type"
          (cl:list :wrap (make-hydra_core_wrapped_term
                           :type_name "hydra.core.Name"
                           :body (cl:list :literal (cl:list :string "type")))))
    (cl:list "hydra.constants.key_debugId"
          (cl:list :wrap (make-hydra_core_wrapped_term
                           :type_name "hydra.core.Name"
                           :body (cl:list :literal (cl:list :string "debugId")))))
    (cl:list "hydra.constants.key_firstClassType"
          (cl:list :wrap (make-hydra_core_wrapped_term
                           :type_name "hydra.core.Name"
                           :body (cl:list :literal (cl:list :string "firstClassType")))))

    ;; hydra.rewriting.deannotateTerm = \t -> case t of
    ;;   annotated(at) -> deannotateTerm(at.body)
    ;;   _ -> t
    (cl:list "hydra.rewriting.deannotateTerm"
          (t-lam "t"
            (t-app
              (t-match "hydra.core.Term" (cl:list :just (t-var "t"))
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
    (cl:list "hydra.annotations.termAnnotationInternal"
          (t-lam "term"
            (t-let "toPairs"
              (t-lam "rest"
                (t-lam "t"
                  (t-app
                    (t-match "hydra.core.Term" (cl:list :just (t-var "rest"))
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
                  (t-app (t-app (t-var "toPairs") (cl:list :list cl:nil))
                    (t-var "term")))))))

    ;; hydra.annotations.setAnnotation = \key -> \val -> \m ->
    ;;   maybe(delete(key, m), \v -> insert(key, v, m), val)
    (cl:list "hydra.annotations.setAnnotation"
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
    (cl:list "hydra.annotations.setTermAnnotation"
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
                          (cl:list (t-field "body" (t-var "stripped"))
                                (t-field "annotation" (t-var "anns"))))))))))))

    ;; hydra.annotations.setTermDescription = \d ->
    ;;   setTermAnnotation(key_description, maybes.map(\s -> inject(Term, literal, inject(Literal, string, s)), d))
    (cl:list "hydra.annotations.setTermDescription"
          (t-lam "d"
            (t-app (t-app (t-var "hydra.annotations.setTermAnnotation")
              (t-var "hydra.constants.key_description"))
              (t-app (t-app (t-prim "hydra.lib.maybes.map")
                (t-lam "s"
                  (t-inject "hydra.core.Term" "literal"
                    (t-inject "hydra.core.Literal" "string" (t-var "s")))))
                (t-var "d")))))

    ;; hydra.annotations.getTermAnnotation = \key -> \term ->
    ;;   maps.lookup(key, termAnnotationInternal(term))
    (cl:list "hydra.annotations.getTermAnnotation"
          (t-lam "key"
            (t-lam "term"
              (t-app (t-app (t-prim "hydra.lib.maps.lookup") (t-var "key"))
                (t-app (t-var "hydra.annotations.termAnnotationInternal")
                  (t-var "term"))))))

    ;; hydra.annotations.getDescription = \cx -> \g -> \anns ->
    ;;   maybe(right(nothing),
    ;;         \descTerm -> match Term { literal(\lit -> match Literal { string(\s -> right(just(s))) }) },
    ;;         maps.lookup(key_description, anns))
    (cl:list "hydra.annotations.getDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "anns"
                (t-app (t-app (t-app (t-prim "hydra.lib.maybes.maybe")
                  ;; default: right(nothing)
                  (t-right (cl:list :maybe (cl:list :nothing cl:nil))))
                  ;; \descTerm -> case match to extract string
                  (t-lam "descTerm"
                    (t-app
                      (t-match "hydra.core.Term"
                        (cl:list :just (t-right (cl:list :maybe (cl:list :nothing cl:nil))))
                        (t-field "literal"
                          (t-lam "lit"
                            (t-app
                              (t-match "hydra.core.Literal"
                                (cl:list :just (t-right (cl:list :maybe (cl:list :nothing cl:nil))))
                                (t-field "string"
                                  (t-lam "s"
                                    (t-right (cl:list :maybe (t-var "s"))))))
                              (t-var "lit")))))
                      (t-var "descTerm"))))
                  ;; maps.lookup(key_description, anns)
                  (t-app (t-app (t-prim "hydra.lib.maps.lookup")
                    (t-var "hydra.constants.key_description"))
                    (t-var "anns")))))))

    ;; hydra.annotations.getTermDescription = \cx -> \g -> \term ->
    ;;   let peel = \t -> case t of
    ;;     typeLambda(tl) -> peel(tl.body)
    ;;     typeApplication(ta) -> peel(ta.body)
    ;;     _ -> t
    ;;   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
    (cl:list "hydra.annotations.getTermDescription"
          (t-lam "cx"
            (t-lam "g"
              (t-lam "term"
                (t-let "peel"
                  (t-lam "t"
                    (t-app
                      (t-match "hydra.core.Term" (cl:list :just (t-var "t"))
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

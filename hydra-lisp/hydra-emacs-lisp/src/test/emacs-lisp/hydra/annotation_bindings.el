;;; annotation-bindings.el --- -*- lexical-binding: t -*-

(require 'cl-lib)

;; ==========================================================================

;; Term-building helpers for annotation bindings
(defun hydra--t-lam (param body)
  (list :function (list :lambda (make-hydra_core_lambda :parameter param :domain nil :body body))))
(defun hydra--t-var (name)
  (list :variable name))
(defun hydra--t-app (fun arg)
  (list :application (make-hydra_core_application :function fun :argument arg)))
(defun hydra--t-prim (name)
  (list :variable name))
(defun hydra--t-let (name val body)
  (list :let (make-hydra_core_let
               :bindings (list (make-hydra_core_binding :name name :term val :type nil))
               :body body)))
(defun hydra--t-inject (type-name field-name term)
  (list :union (make-hydra_core_injection
                 :type_name type-name
                 :field (make-hydra_core_field :name field-name :term term))))
(defun hydra--t-record (type-name fields)
  (list :record (make-hydra_core_record :type_name type-name :fields fields)))
(defun hydra--t-field (name term)
  (make-hydra_core_field :name name :term term))
(defun hydra--t-project (type-name field-name)
  (list :function (list :elimination
    (list :record (make-hydra_core_projection :type_name type-name :field field-name)))))
(defun hydra--t-match (type-name default &rest case-fields)
  (list :function (list :elimination
    (list :union (make-hydra_core_case_statement :type_name type-name :default default :cases case-fields)))))
(defun hydra--t-right (v) (list :either (list :right v)))
(defun hydra--t-left (v) (list :either (list :left v)))
(defun hydra--t-just (v) (list :maybe (hydra--t-inject "hydra.core.Term" "literal"
                     (hydra--t-inject "hydra.core.Literal" "string" v))))
(defun hydra--t-nothing () (list :maybe (list :nothing nil)))

;; Annotation term-level bindings (mirrors Java TestSuiteRunner.addAnnotationsBindings)
(defun hydra-annotation-bindings ()
  (list
    ;; hydra.constants
    (list "hydra.constants.key_classes"
          (list :wrap (make-hydra_core_wrapped_term :type_name "hydra.core.Name" :body
                        (list :literal (list :string "classes")))))
    (list "hydra.constants.key_description"
          (list :wrap (make-hydra_core_wrapped_term :type_name "hydra.core.Name" :body
                        (list :literal (list :string "description")))))
    (list "hydra.constants.key_type"
          (list :wrap (make-hydra_core_wrapped_term :type_name "hydra.core.Name" :body
                        (list :literal (list :string "type")))))
    (list "hydra.constants.key_debugId"
          (list :wrap (make-hydra_core_wrapped_term :type_name "hydra.core.Name" :body
                        (list :literal (list :string "debugId")))))
    (list "hydra.constants.key_firstClassType"
          (list :wrap (make-hydra_core_wrapped_term :type_name "hydra.core.Name" :body
                        (list :literal (list :string "firstClassType")))))

    ;; hydra.rewriting.deannotateTerm = \t -> case t of
    ;;   annotated(at) -> deannotateTerm(at.body)
    ;;   _ -> t
    (list "hydra.rewriting.deannotateTerm"
          (hydra--t-lam "t"
            (hydra--t-app
              (hydra--t-match "hydra.core.Term" (list :just (hydra--t-var "t"))
                (hydra--t-field "annotated"
                  (hydra--t-lam "at"
                    (hydra--t-app (hydra--t-var "hydra.rewriting.deannotateTerm")
                      (hydra--t-app (hydra--t-project "hydra.core.AnnotatedTerm" "body")
                        (hydra--t-var "at"))))))
              (hydra--t-var "t"))))

    ;; hydra.annotations.termAnnotationInternal = \term ->
    ;;   let toPairs = \rest -> \t -> case t of
    ;;     annotated(at) -> toPairs(cons(toList(at.annotation), rest), at.body)
    ;;     _ -> rest
    ;;   in fromList(concat(toPairs([], term)))
    (list "hydra.annotations.termAnnotationInternal"
          (hydra--t-lam "term"
            (hydra--t-let "toPairs"
              (hydra--t-lam "rest"
                (hydra--t-lam "t"
                  (hydra--t-app
                    (hydra--t-match "hydra.core.Term" (list :just (hydra--t-var "rest"))
                      (hydra--t-field "annotated"
                        (hydra--t-lam "at"
                          (hydra--t-app
                            (hydra--t-app (hydra--t-var "toPairs")
                              (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.lists.cons")
                                (hydra--t-app (hydra--t-prim "hydra.lib.maps.toList")
                                  (hydra--t-app (hydra--t-project "hydra.core.AnnotatedTerm" "annotation")
                                    (hydra--t-var "at"))))
                                (hydra--t-var "rest")))
                            (hydra--t-app (hydra--t-project "hydra.core.AnnotatedTerm" "body")
                              (hydra--t-var "at"))))))
                    (hydra--t-var "t"))))
              (hydra--t-app (hydra--t-prim "hydra.lib.maps.fromList")
                (hydra--t-app (hydra--t-prim "hydra.lib.lists.concat")
                  (hydra--t-app (hydra--t-app (hydra--t-var "toPairs") (list :list nil))
                    (hydra--t-var "term")))))))

    ;; hydra.annotations.setAnnotation = \key -> \val -> \m ->
    ;;   maybe(delete(key, m), \v -> insert(key, v, m), val)
    (list "hydra.annotations.setAnnotation"
          (hydra--t-lam "key"
            (hydra--t-lam "val"
              (hydra--t-lam "m"
                (hydra--t-app (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maybes.maybe")
                  (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maps.delete") (hydra--t-var "key")) (hydra--t-var "m")))
                  (hydra--t-lam "v"
                    (hydra--t-app (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maps.insert")
                      (hydra--t-var "key")) (hydra--t-var "v")) (hydra--t-var "m"))))
                  (hydra--t-var "val"))))))

    ;; hydra.annotations.setTermAnnotation = \key -> \val -> \term ->
    ;;   let stripped = deannotateTerm(term)
    ;;       anns = setAnnotation(key, val, termAnnotationInternal(term))
    ;;   in if null(anns) then stripped
    ;;      else inject(Term){annotated=record(AnnotatedTerm){body=stripped, annotation=anns}}
    (list "hydra.annotations.setTermAnnotation"
          (hydra--t-lam "key"
            (hydra--t-lam "val"
              (hydra--t-lam "term"
                (hydra--t-let "stripped"
                  (hydra--t-app (hydra--t-var "hydra.rewriting.deannotateTerm") (hydra--t-var "term"))
                  (hydra--t-let "anns"
                    (hydra--t-app (hydra--t-app (hydra--t-app (hydra--t-var "hydra.annotations.setAnnotation")
                      (hydra--t-var "key")) (hydra--t-var "val"))
                      (hydra--t-app (hydra--t-var "hydra.annotations.termAnnotationInternal") (hydra--t-var "term")))
                    (hydra--t-app (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.logic.ifElse")
                      (hydra--t-app (hydra--t-prim "hydra.lib.maps.null") (hydra--t-var "anns")))
                      (hydra--t-var "stripped"))
                      (hydra--t-inject "hydra.core.Term" "annotated"
                        (hydra--t-record "hydra.core.AnnotatedTerm"
                          (list (hydra--t-field "body" (hydra--t-var "stripped"))
                                (hydra--t-field "annotation" (hydra--t-var "anns"))))))))))))

    ;; hydra.annotations.setTermDescription = \d ->
    ;;   setTermAnnotation(key_description, maybes.map(\s -> inject(Term, literal, inject(Literal, string, s)), d))
    (list "hydra.annotations.setTermDescription"
          (hydra--t-lam "d"
            (hydra--t-app (hydra--t-app (hydra--t-var "hydra.annotations.setTermAnnotation")
              (hydra--t-var "hydra.constants.key_description"))
              (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maybes.map")
                (hydra--t-lam "s"
                  (hydra--t-inject "hydra.core.Term" "literal"
                    (hydra--t-inject "hydra.core.Literal" "string" (hydra--t-var "s")))))
                (hydra--t-var "d")))))

    ;; hydra.annotations.getTermAnnotation = \key -> \term ->
    ;;   maps.lookup(key, termAnnotationInternal(term))
    (list "hydra.annotations.getTermAnnotation"
          (hydra--t-lam "key"
            (hydra--t-lam "term"
              (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maps.lookup") (hydra--t-var "key"))
                (hydra--t-app (hydra--t-var "hydra.annotations.termAnnotationInternal")
                  (hydra--t-var "term"))))))

    ;; hydra.annotations.getDescription = \cx -> \g -> \anns ->
    ;;   maybe(right(nothing),
    ;;         \descTerm -> match Term { literal(\lit -> match Literal { string(\s -> right(just(s))) }) },
    ;;         maps.lookup(key_description, anns))
    (list "hydra.annotations.getDescription"
          (hydra--t-lam "cx"
            (hydra--t-lam "g"
              (hydra--t-lam "anns"
                (hydra--t-app (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maybes.maybe")
                  ;; default: right(nothing)
                  (hydra--t-right (list :maybe (list :nothing nil))))
                  ;; \descTerm -> case match to extract string
                  (hydra--t-lam "descTerm"
                    (hydra--t-app
                      (hydra--t-match "hydra.core.Term"
                        (list :just (hydra--t-right (list :maybe (list :nothing nil))))
                        (hydra--t-field "literal"
                          (hydra--t-lam "lit"
                            (hydra--t-app
                              (hydra--t-match "hydra.core.Literal"
                                (list :just (hydra--t-right (list :maybe (list :nothing nil))))
                                (hydra--t-field "string"
                                  (hydra--t-lam "s"
                                    (hydra--t-right (list :maybe (hydra--t-var "s"))))))
                              (hydra--t-var "lit")))))
                      (hydra--t-var "descTerm"))))
                  ;; maps.lookup(key_description, anns)
                  (hydra--t-app (hydra--t-app (hydra--t-prim "hydra.lib.maps.lookup")
                    (hydra--t-var "hydra.constants.key_description"))
                    (hydra--t-var "anns")))))))

    ;; hydra.annotations.getTermDescription = \cx -> \g -> \term ->
    ;;   let peel = \t -> case t of
    ;;     typeLambda(tl) -> peel(tl.body)
    ;;     typeApplication(ta) -> peel(ta.body)
    ;;     _ -> t
    ;;   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
    (list "hydra.annotations.getTermDescription"
          (hydra--t-lam "cx"
            (hydra--t-lam "g"
              (hydra--t-lam "term"
                (hydra--t-let "peel"
                  (hydra--t-lam "t"
                    (hydra--t-app
                      (hydra--t-match "hydra.core.Term" (list :just (hydra--t-var "t"))
                        (hydra--t-field "typeLambda"
                          (hydra--t-lam "tl"
                            (hydra--t-app (hydra--t-var "peel")
                              (hydra--t-app (hydra--t-project "hydra.core.TypeLambda" "body")
                                (hydra--t-var "tl")))))
                        (hydra--t-field "typeApplication"
                          (hydra--t-lam "ta"
                            (hydra--t-app (hydra--t-var "peel")
                              (hydra--t-app (hydra--t-project "hydra.core.TypeApplicationTerm" "body")
                                (hydra--t-var "ta"))))))
                      (hydra--t-var "t")))
                  (hydra--t-app (hydra--t-app (hydra--t-app (hydra--t-var "hydra.annotations.getDescription")
                    (hydra--t-var "cx")) (hydra--t-var "g"))
                    (hydra--t-app (hydra--t-var "hydra.annotations.termAnnotationInternal")
                      (hydra--t-app (hydra--t-var "peel") (hydra--t-var "term")))))))))))

(provide 'hydra.annotation-bindings)

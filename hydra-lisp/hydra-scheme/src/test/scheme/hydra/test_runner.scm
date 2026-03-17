(define-library (hydra test_runner)
  (import (scheme base) (scheme write) (scheme cxr) (scheme char)
          (hydra core)
          (hydra context)
          (hydra graph)
          (hydra prims)
          (hydra reduction)
          (hydra rewriting)
          (hydra formatting)
          (hydra sorting)
          (hydra serialization)
          (hydra inference)
          (hydra checking)
          (hydra hoisting)
          (hydra unification)
          (hydra substitution)
          (hydra show core)
          (hydra testing)
          (hydra json bootstrap)
          (hydra json parser)
          (hydra json writer)
          (hydra json encode)
          (hydra json decode)
          (hydra test test_graph)
          (hydra lib libraries)
          (hydra lib equality)
          (hydra lib maps)
          (hydra lib maybes)
          (hydra lib pairs)
          (hydra lib sets))
  (export run-test-group)
  (begin

    ;; Simple insertion sort (no external sort library needed)
    (define (my-list-sort less? lst)
      (define (insert x sorted)
        (cond ((null? sorted) (list x))
              ((less? x (car sorted)) (cons x sorted))
              (else (cons (car sorted) (insert x (cdr sorted))))))
      (let loop ((rest lst) (acc '()))
        (if (null? rest) acc
            (loop (cdr rest) (insert (car rest) acc)))))

    ;; ==========================================================================
    ;; Graph construction
    ;; ==========================================================================

    ;; Term-building helpers for annotation bindings
    (define (t-lam param body)
      (list 'function (list 'lambda (make-hydra_core_lambda param '() body))))
    (define (t-var name)
      (list 'variable name))
    (define (t-app fun arg)
      (list 'application (make-hydra_core_application fun arg)))
    (define (t-prim name)
      (list 'function (list 'primitive name)))
    (define (t-let name val body)
      (list 'let (make-hydra_core_let
                   (list (make-hydra_core_binding name val '()))
                   body)))
    (define (t-inject type-name field-name term)
      (list 'union (make-hydra_core_injection type-name
                     (make-hydra_core_field field-name term))))
    (define (t-record type-name fields)
      (list 'record (make-hydra_core_record type-name fields)))
    (define (t-field name term)
      (make-hydra_core_field name term))
    (define (t-project type-name field-name)
      (list 'function (list 'elimination
        (list 'record (make-hydra_core_projection type-name field-name)))))
    (define (t-match type-name default . case-fields)
      (list 'function (list 'elimination
        (list 'union (make-hydra_core_case_statement type-name default case-fields)))))
    (define (t-right v) (list 'either (list 'right v)))
    (define (t-left v) (list 'either (list 'left v)))
    (define (t-just v) (list 'maybe (t-inject "hydra.core.Term" "literal"
                         (t-inject "hydra.core.Literal" "string" v))))
    (define (t-nothing) (list 'maybe (list 'nothing '())))

    ;; Annotation term-level bindings (mirrors Java TestSuiteRunner.addAnnotationsBindings)
    (define (annotation-bindings)
      (list
        ;; hydra.constants
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
        ;;   setTermAnnotation(key_description, maybes.map(\s -> inject(Term, literal, inject(Literal, string, s)), d))
        (list "hydra.annotations.setTermDescription"
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
        (list "hydra.annotations.getTermAnnotation"
              (t-lam "key"
                (t-lam "term"
                  (t-app (t-app (t-prim "hydra.lib.maps.lookup") (t-var "key"))
                    (t-app (t-var "hydra.annotations.termAnnotationInternal")
                      (t-var "term"))))))

        ;; hydra.annotations.getDescription = \cx -> \g -> \anns ->
        ;;   maybe(right(nothing),
        ;;         \descTerm -> match Term { literal(\lit -> match Literal { string(\s -> right(just(s))) }) },
        ;;         maps.lookup(key_description, anns))
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

    (define (build-test-graph)
      (let* ((std-prims (standard-library))
             (all-prims std-prims)
             ;; Build schema types from bootstrap + test types
             (bootstrap-types hydra_json_bootstrap_types_by_name)
             (test-types hydra_test_test_graph_test_types)
             ;; Convert types to TypeSchemes using f_type_to_type_scheme
             (type-to-ts hydra_rewriting_f_type_to_type_scheme)
             (kernel-schemas (map (lambda (entry)
                                    (list (car entry) (type-to-ts (cdr entry))))
                                  bootstrap-types))
             (test-schemas (map (lambda (entry)
                                  (list (car entry) (type-to-ts (cadr entry))))
                                (hydra_lib_maps_to_list test-types)))
             (schema-types (hydra_lib_maps_from_list (append kernel-schemas test-schemas)))
             ;; Test terms
             (test-terms-alist (hydra_lib_maps_to_list hydra_test_test_graph_test_terms))
             (test-terms (map (lambda (entry) (list (car entry) (cdr entry))) test-terms-alist))
             (bound-terms
               (append
                 ;; Primitive bridges
                 (map (lambda (pair)
                        (list (car pair) (list 'function (list 'primitive (car pair)))))
                      all-prims)
                 ;; Annotation term-level bindings (override primitive bridges for annotation names)
                 (annotation-bindings)
                 ;; Other constants
                 (list (list "hydra.monads.emptyContext" (list 'unit '()))
                       (list "hydra.lexical.emptyGraph" (list 'unit '())))
                 ;; Test terms
                 test-terms)))
        (make-hydra_graph_graph
          (hydra_lib_maps_from_list bound-terms)
          hydra_lib_maps_empty
          '()
          '()
          hydra_lib_maps_empty
          (hydra_lib_maps_from_list
            (map (lambda (p) (list (car p) (cdr p))) all-prims))
          schema-types
          '())))

    (define *test-graph* #f)

    (define (get-test-graph)
      (unless *test-graph* (set! *test-graph* (build-test-graph)))
      *test-graph*)

    (define (empty-context)
      (make-hydra_context_context '() '() hydra_lib_maps_empty))

    ;; Build an empty graph with standard primitives (for hoisting tests)
    (define (empty-graph)
      (let ((std-prims (standard-library)))
        (make-hydra_graph_graph
          hydra_lib_maps_empty
          hydra_lib_maps_empty
          '()
          '()
          hydra_lib_maps_empty
          (hydra_lib_maps_from_list
            (map (lambda (p) (list (car p) (cdr p))) std-prims))
          hydra_lib_maps_empty
          '())))

    ;; ==========================================================================
    ;; Term comparison
    ;; ==========================================================================

    (define (obj->string x)
      (let ((p (open-output-string)))
        (write x p)
        (get-output-string p)))

    (define (show-term t)
      (guard (exn (#t #f))
        (if (and (pair? t) (eq? (car t) 'annotated))
            (let* ((at (cadr t))
                   (body (hydra_core_annotated_term-body at))
                   (ann (hydra_core_annotated_term-annotation at))
                   (body-str (hydra_show_core_term body))
                   (ann-str (if (and (pair? ann) (eq? (car ann) 'map))
                                (hydra_show_core_term ann)
                                "{}")))
              (string-append "inject(hydra.core.Term){annotated=record(hydra.core.AnnotatedTerm){body="
                             (show-term body) ", annotation=" ann-str "}}"))
            (hydra_show_core_term t))))

    (define (show-type t)
      (guard (exn (#t #f))
        (hydra_show_core_type t)))

    (define (show-type-scheme ts)
      (guard (exn (#t #f))
        (hydra_show_core_type_scheme ts)))

    (define (show-let l)
      (guard (exn (#t #f))
        (hydra_show_core_let l)))

    (define (normalize-show s)
      (if (not s) s
          (let loop ((str s) (start 0))
            (let ((open-pos (string-index-of str #\{ start)))
              (if (not open-pos) str
                  (let ((close-pos (string-index-of str #\} (+ open-pos 1))))
                    (if (not close-pos) str
                        (let* ((contents (substring str (+ open-pos 1) close-pos))
                               (elems (map string-trim-both
                                           (string-split-on "," contents)))
                               (sorted (my-list-sort string<? elems))
                               (joined (string-join sorted ", "))
                               (new-block (string-append "{" joined "}")))
                          (loop (string-append
                                  (substring str 0 open-pos)
                                  new-block
                                  (substring str (+ close-pos 1) (string-length str)))
                                (+ open-pos (string-length new-block)))))))))))

    ;; String utility helpers
    (define (string-contains haystack needle)
      (let ((hlen (string-length haystack))
            (nlen (string-length needle)))
        (and (>= hlen nlen)
             (let loop ((i 0))
               (cond
                 ((> (+ i nlen) hlen) #f)
                 ((string=? (substring haystack i (+ i nlen)) needle) #t)
                 (else (loop (+ i 1))))))))

    (define (string-index-of str ch start)
      (let ((len (string-length str)))
        (let loop ((i start))
          (cond
            ((>= i len) #f)
            ((char=? (string-ref str i) ch) i)
            (else (loop (+ i 1)))))))

    (define (string-trim-both s)
      (let* ((len (string-length s))
             (start (let loop ((i 0))
                      (if (and (< i len) (char-whitespace? (string-ref s i)))
                          (loop (+ i 1)) i)))
             (end (let loop ((i (- len 1)))
                    (if (and (> i start) (char-whitespace? (string-ref s i)))
                        (loop (- i 1)) (+ i 1)))))
        (substring s start end)))

    (define (string-split-on sep str)
      (let ((sep-len (string-length sep))
            (str-len (string-length str)))
        (if (= sep-len 0) (list str)
            (let ((sep-ch (string-ref sep 0)))
              (let loop ((start 0) (acc '()))
                (let ((pos (string-index-of str sep-ch start)))
                  (if (not pos)
                      (reverse (cons (substring str start str-len) acc))
                      (loop (+ pos 1)
                            (cons (substring str start pos) acc)))))))))

    (define (string-join strs sep)
      (if (null? strs) ""
          (let loop ((rest (cdr strs)) (acc (car strs)))
            (if (null? rest) acc
                (loop (cdr rest) (string-append acc sep (car rest)))))))

    (define (float-close? actual expected)
      (let ((suffix ":float64"))
        (and (> (string-length actual) (string-length suffix))
             (string=? suffix (substring actual (- (string-length actual) (string-length suffix))
                                         (string-length actual)))
             (> (string-length expected) (string-length suffix))
             (string=? suffix (substring expected (- (string-length expected) (string-length suffix))
                                          (string-length expected)))
             (guard (exn (#t #f))
               (let ((av (string->number (substring actual 0 (- (string-length actual) (string-length suffix)))))
                     (ev (string->number (substring expected 0 (- (string-length expected) (string-length suffix))))))
                 (and av ev
                      (or (= av ev)
                          (< (abs (- av ev)) 1e-10))))))))

    ;; Convert struct-compat term to meta-encoded (union Injection ...) form
    (define (term-to-meta term)
      (if (not (pair? term)) term
          (let ((tag (car term)))
            (cond
              ((eq? tag 'annotated)
               (let* ((at (cadr term))
                      (body (hydra_core_annotated_term-body at))
                      (ann (hydra_core_annotated_term-annotation at))
                      (ann-term (if (and (pair? ann) (eq? (car ann) 'map)) ann (list 'map ann))))
                 (list 'union (make-hydra_core_injection "hydra.core.Term"
                   (make-hydra_core_field "annotated"
                     (list 'record (make-hydra_core_record "hydra.core.AnnotatedTerm"
                       (list (make-hydra_core_field "body" (term-to-meta body))
                             (make-hydra_core_field "annotation" (term-to-meta-map ann-term))))))))))
              ((eq? tag 'literal)
               (list 'union (make-hydra_core_injection "hydra.core.Term"
                 (make-hydra_core_field "literal" (literal-to-meta (cadr term))))))
              ((eq? tag 'application)
               (let ((app (cadr term)))
                 (list 'union (make-hydra_core_injection "hydra.core.Term"
                   (make-hydra_core_field "application"
                     (list 'record (make-hydra_core_record "hydra.core.Application"
                       (list (make-hydra_core_field "function" (term-to-meta (hydra_core_application-function app)))
                             (make-hydra_core_field "argument" (term-to-meta (hydra_core_application-argument app)))))))))))
              ((eq? tag 'variable)
               (let ((name (cadr term)))
                 (list 'union (make-hydra_core_injection "hydra.core.Term"
                   (make-hydra_core_field "variable"
                     (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                       (list 'literal (list 'string name)))))))))
              ((eq? tag 'wrap)
               (let ((wt (cadr term)))
                 (list 'union (make-hydra_core_injection "hydra.core.Term"
                   (make-hydra_core_field "wrap"
                     (list 'record (make-hydra_core_record "hydra.core.WrappedTerm"
                       (list (make-hydra_core_field "typeName"
                               (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name"
                                 (list 'literal (list 'string (hydra_core_wrapped_term-type_name wt))))))
                             (make-hydra_core_field "body" (term-to-meta (hydra_core_wrapped_term-body wt)))))))))))
              ((eq? tag 'maybe)
               (list 'union (make-hydra_core_injection "hydra.core.Term"
                 (make-hydra_core_field "maybe"
                   (if (cadr term)
                       (list 'maybe (term-to-meta (cadr term)))
                       (list 'maybe '()))))))
              ((eq? tag 'list)
               (list 'union (make-hydra_core_injection "hydra.core.Term"
                 (make-hydra_core_field "list"
                   (list 'list (map term-to-meta (cadr term)))))))
              ((eq? tag 'map)
               (list 'union (make-hydra_core_injection "hydra.core.Term"
                 (make-hydra_core_field "map" (term-to-meta-map term)))))
              (else term)))))

    (define (literal-to-meta lit)
      (if (not (pair? lit)) lit
          (let ((tag (car lit)))
            (cond
              ((eq? tag 'string)
               (list 'union (make-hydra_core_injection "hydra.core.Literal"
                 (make-hydra_core_field "string" (list 'literal (list 'string (cadr lit)))))))
              ((eq? tag 'boolean)
               (list 'union (make-hydra_core_injection "hydra.core.Literal"
                 (make-hydra_core_field "boolean" (list 'literal (list 'boolean (cadr lit)))))))
              ((eq? tag 'integer) (integer-to-meta (cadr lit)))
              ((eq? tag 'float) (float-to-meta (cadr lit)))
              (else lit)))))

    (define (integer-to-meta ival)
      (if (not (pair? ival)) ival
          (let ((tag (car ival)))
            (list 'union (make-hydra_core_injection "hydra.core.Literal"
              (make-hydra_core_field "integer"
                (list 'union (make-hydra_core_injection "hydra.core.IntegerValue"
                  (make-hydra_core_field (symbol->string tag)
                    (list 'literal (list 'integer ival)))))))))))

    (define (float-to-meta fval)
      (if (not (pair? fval)) fval
          (let ((tag (car fval)))
            (list 'union (make-hydra_core_injection "hydra.core.Literal"
              (make-hydra_core_field "float"
                (list 'union (make-hydra_core_injection "hydra.core.FloatValue"
                  (make-hydra_core_field (symbol->string tag)
                    (list 'literal (list 'float fval)))))))))))

    (define (ensure-name-key k)
      (if (string? k)
          (list 'wrap (make-hydra_core_wrapped_term "hydra.core.Name" (list 'literal (list 'string k))))
          k))

    (define (term-to-meta-map m)
      (if (and (pair? m) (eq? (car m) 'map))
          (list 'map (map (lambda (entry)
                            (cons (ensure-name-key (car entry))
                                  (term-to-meta (cdr entry))))
                          (cadr m)))
          m))

    (define (terms-match? actual expected)
      (or (equal? actual expected)
          (guard (exn (#t #f))
            (let ((a-str (show-term actual))
                  (e-str (show-term expected)))
              (or (equal? a-str e-str)
                  (equal? (normalize-show a-str) (normalize-show e-str))
                  (and a-str e-str (float-close? a-str e-str)))))
          ;; Try converting struct-compat annotated terms to meta-encoding
          (and (pair? actual) (eq? (car actual) 'annotated)
               (guard (exn (#t #f))
                 (let ((meta-actual (term-to-meta actual)))
                   (or (equal? meta-actual expected)
                       (guard (exn (#t #f))
                         (let ((a-str (show-term meta-actual))
                               (e-str (show-term expected)))
                           (or (equal? a-str e-str)
                               (equal? (normalize-show a-str) (normalize-show e-str)))))))))
          ;; Try converting Maybe-wrapped struct-compat terms
          (and (pair? actual) (eq? (car actual) 'maybe)
               (pair? (cadr actual)) (eq? (caadr actual) 'annotated)
               (guard (exn (#t #f))
                 (let ((meta-actual (list 'maybe (term-to-meta (cadr actual)))))
                   (or (equal? meta-actual expected)
                       (guard (exn (#t #f))
                         (let ((a-str (show-term meta-actual))
                               (e-str (show-term expected)))
                           (or (equal? a-str e-str)
                               (equal? (normalize-show a-str) (normalize-show e-str)))))))))))

    ;; ==========================================================================
    ;; Alpha-equivalence for type variables (for type_checking tests)
    ;; ==========================================================================

    (define (string-starts-with? s prefix)
      (and (>= (string-length s) (string-length prefix))
           (string=? (substring s 0 (string-length prefix)) prefix)))

    ;; Find the index of a character in a string, starting from a position
    (define (string-index-of-char str ch start)
      (let ((len (string-length str)))
        (let loop ((i start))
          (cond
            ((>= i len) -1)
            ((char=? (string-ref str i) ch) i)
            (else (loop (+ i 1)))))))

    ;; Extract binder chain from type-scheme string: "Λt0.Λt1.body" -> ((t0 t1) . "body")
    (define (extract-binders s)
      (let loop ((remaining s) (binders '()))
        (if (and (> (string-length remaining) 0)
                 (string-starts-with? remaining "\x039B;"))  ;; capital lambda
            (let ((dot-idx (string-index-of-char remaining #\. 0)))
              (if (< dot-idx 0)
                  (cons (reverse binders) remaining)
                  (let ((binder (substring remaining 1 dot-idx)))
                    (loop (substring remaining (+ dot-idx 1) (string-length remaining))
                          (cons binder binders)))))
            (cons (reverse binders) remaining))))

    ;; Check if a character is alphanumeric or underscore
    (define (word-char? ch)
      (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))

    ;; Find all occurrences of pattern matching "t\d+" at word boundaries in string
    (define (find-type-vars body)
      (let ((len (string-length body)))
        (let loop ((i 0) (result '()))
          (if (>= i len) (reverse result)
              (if (and (char=? (string-ref body i) #\t)
                       (or (= i 0) (not (word-char? (string-ref body (- i 1)))))
                       (< (+ i 1) len)
                       (char-numeric? (string-ref body (+ i 1))))
                  ;; Found potential type var starting with 't'
                  (let digit-loop ((j (+ i 1)))
                    (if (and (< j len) (char-numeric? (string-ref body j)))
                        (digit-loop (+ j 1))
                        (if (and (or (= j len) (not (word-char? (string-ref body j)))))
                            (let ((var-name (substring body i j)))
                              (loop j (if (member var-name result) result
                                          (cons var-name result))))
                            (loop (+ i 1) result))))
                  (loop (+ i 1) result))))))

    ;; Replace all occurrences of a word in a string
    (define (replace-word str from to)
      (let ((slen (string-length str))
            (flen (string-length from)))
        (let loop ((i 0) (acc '()))
          (if (> (+ i flen) slen)
              (apply string-append (reverse (cons (substring str i slen) acc)))
              (if (and (string=? (substring str i (+ i flen)) from)
                       (or (= i 0) (not (word-char? (string-ref str (- i 1)))))
                       (or (= (+ i flen) slen) (not (word-char? (string-ref str (+ i flen))))))
                  (loop (+ i flen) (cons to acc))
                  (loop (+ i 1) (cons (string (string-ref str i)) acc)))))))

    (define (normalize-type-var-names s)
      (if (not s) s
          (let* ((result (extract-binders s))
                 (binders (car result))
                 (body (cdr result)))
            (if (null? binders) s
                (let* ((binder-set binders)
                       ;; Find vars by first occurrence in body
                       (found-vars (find-type-vars body))
                       ;; Filter to only binder vars
                       (ordered (let loop ((vs found-vars) (acc '()))
                                  (if (null? vs) (reverse acc)
                                      (if (member (car vs) binder-set)
                                          (loop (cdr vs) (cons (car vs) acc))
                                          (loop (cdr vs) acc)))))
                       ;; Add binders not found in body
                       (all-ordered (append ordered
                                     (let loop ((bs binders) (acc '()))
                                       (if (null? bs) (reverse acc)
                                           (if (member (car bs) ordered)
                                               (loop (cdr bs) acc)
                                               (loop (cdr bs) (cons (car bs) acc)))))))
                       ;; First pass: rename to temp names
                       (temp-body (let loop ((i 0) (b body) (vars all-ordered))
                                    (if (null? vars) b
                                        (loop (+ i 1)
                                              (replace-word b (car vars) (string-append "tv" (number->string i)))
                                              (cdr vars)))))
                       ;; Second pass: rename to final names
                       (final-body (let loop ((i 0) (b temp-body))
                                     (if (>= i (length all-ordered)) b
                                         (loop (+ i 1)
                                               (replace-word b
                                                 (string-append "tv" (number->string i))
                                                 (string-append "t" (number->string i)))))))
                       ;; Reconstruct binder chain
                       (prefix (apply string-append
                                 (let loop ((i 0) (vs all-ordered) (acc '()))
                                   (if (null? vs) (reverse acc)
                                       (loop (+ i 1) (cdr vs)
                                             (cons (string-append "\x039B;t" (number->string i) ".") acc)))))))
                  (string-append prefix final-body))))))

    (define (normalize-set-order s)
      ;; Normalize set element ordering in show output: {b, a} -> {a, b}
      (if (not s) s
          (let loop ((str s) (start 0))
            (let ((open-pos (string-index-of str #\{ start)))
              (if (not open-pos) str
                  (let ((close-pos (string-index-of str #\} (+ open-pos 1))))
                    (if (not close-pos) str
                        (let* ((contents (substring str (+ open-pos 1) close-pos))
                               (elems (string-split-on "," contents))
                               (trimmed (map string-trim-both elems))
                               (sorted (my-list-sort string<? trimmed))
                               (joined (string-join sorted ", "))
                               (new-block (string-append "{" joined "}")))
                          (loop (string-append
                                  (substring str 0 open-pos)
                                  new-block
                                  (substring str (+ close-pos 1) (string-length str)))
                                (+ open-pos (string-length new-block)))))))))))

    (define (alpha-equivalent-terms? expected actual)
      (let ((e-str (normalize-set-order (normalize-type-var-names (show-term expected))))
            (a-str (normalize-set-order (normalize-type-var-names (show-term actual)))))
        (equal? e-str a-str)))

    (define (alpha-equivalent-types? expected actual)
      (let ((e-str (normalize-set-order (normalize-type-var-names (show-type expected))))
            (a-str (normalize-set-order (normalize-type-var-names (show-type actual)))))
        (equal? e-str a-str)))

    ;; ==========================================================================
    ;; Test execution helpers
    ;; ==========================================================================

    (define (extract-error-msg err)
      (cond
        ((hydra_context_in_context? err)
         (let ((obj (hydra_context_in_context-object err)))
           (cond
             ((and (pair? obj) (eq? (car obj) 'other))
              (extract-error-msg (cadr obj)))
             (else (extract-error-msg obj)))))
        ((string? err) err)
        (else (obj->string err))))

    (define (run-simple-test path expected actual-fn)
      "Run a test that compares expected to the result of actual-fn."
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let ((actual (actual-fn)))
          (if (terms-match? actual expected)
              (list 1 0 0)
              (begin
                (display (string-append "FAIL: " path "\n"))
                (guard (exn (#t
                             (display "  Expected (raw): ") (write expected) (newline)
                             (display "  Actual (raw):   ") (write actual) (newline)))
                  (display (string-append "  Expected: " (show-term expected) "\n"))
                  (display (string-append "  Actual:   " (show-term actual) "\n")))
                (list 0 1 0))))))

    (define (run-either-test path expected either-result)
      "Run a test where the result is (list 'right value) or (list 'left error)."
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (if (eq? (car either-result) 'left)
            (begin
              (display (string-append "FAIL: " path "\n"))
              (display (string-append "  ERROR: " (obj->string (cadr either-result)) "\n"))
              (list 0 1 0))
            (let ((actual (cadr either-result)))
              (if (terms-match? actual expected)
                  (list 1 0 0)
                  (begin
                    (display (string-append "FAIL: " path "\n"))
                    (guard (exn (#t
                                 (display "  Expected (raw): ") (write expected) (newline)
                                 (display "  Actual (raw):   ") (write actual) (newline)))
                      (display (string-append "  Expected: " (show-term expected) "\n"))
                      (display (string-append "  Actual:   " (show-term actual) "\n")))
                    (list 0 1 0)))))))

    (define (run-string-comparison-test path expected-str actual-str)
      "Run a test comparing two strings (e.g. show outputs)."
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (if (equal? expected-str actual-str)
            (list 1 0 0)
            (let ((e-norm (normalize-type-var-names expected-str))
                  (a-norm (normalize-type-var-names actual-str)))
              (if (equal? e-norm a-norm)
                  (list 1 0 0)
                  (begin
                    (display (string-append "FAIL: " path "\n"))
                    (display (string-append "  Expected: " (if expected-str expected-str "#f") "\n"))
                    (display (string-append "  Actual:   " (if actual-str actual-str "#f") "\n"))
                    (list 0 1 0)))))))

    ;; ==========================================================================
    ;; Individual test runners
    ;; ==========================================================================

    (define (run-evaluation-test path tc)
      (let* ((input (hydra_testing_evaluation_test_case-input tc))
             (expected (hydra_testing_evaluation_test_case-output tc))
             (style (hydra_testing_evaluation_test_case-evaluation_style tc))
             (graph (get-test-graph))
             (cx (empty-context))
             (eager (equal? (car style) 'eager)))
        (guard (exn (#t
                     (display (string-append "FAIL: " path "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0)))
          (let ((result ((((hydra_reduction_reduce_term cx) graph) eager) input)))
            (if (eq? (car result) 'left)
                (begin
                  (display (string-append "FAIL: " path "\n"))
                  (display (string-append "  ERROR: " (extract-error-msg (cadr result)) "\n"))
                  (list 0 1 0))
                (let ((actual (cadr result)))
                  (if (terms-match? actual expected)
                      (list 1 0 0)
                      (begin
                        (display (string-append "FAIL: " path "\n"))
                        (guard (exn (#t
                                     (display "  Expected (raw): ") (write expected) (newline)
                                     (display "  Actual (raw):   ") (write actual) (newline)))
                          (display (string-append "  Expected: " (show-term expected) "\n"))
                          (display (string-append "  Actual:   " (show-term actual) "\n")))
                        (list 0 1 0)))))))))

    ;; ---- Simple test runners (no graph/context needed) ----

    (define (run-alpha-conversion-test path tc)
      (run-simple-test path
        (hydra_testing_alpha_conversion_test_case-result tc)
        (lambda ()
          (((hydra_reduction_alpha_convert
              (hydra_testing_alpha_conversion_test_case-old_variable tc))
            (hydra_testing_alpha_conversion_test_case-new_variable tc))
           (hydra_testing_alpha_conversion_test_case-term tc)))))

    (define (run-case-conversion-test path tc)
      (run-simple-test path
        (hydra_testing_case_conversion_test_case-to_string tc)
        (lambda ()
          (((hydra_formatting_convert_case
              (hydra_testing_case_conversion_test_case-from_convention tc))
            (hydra_testing_case_conversion_test_case-to_convention tc))
           (hydra_testing_case_conversion_test_case-from_string tc)))))

    (define (run-deannotate-term-test path tc)
      (run-simple-test path
        (hydra_testing_deannotate_term_test_case-output tc)
        (lambda ()
          (hydra_rewriting_deannotate_term
            (hydra_testing_deannotate_term_test_case-input tc)))))

    (define (run-deannotate-type-test path tc)
      (run-simple-test path
        (hydra_testing_deannotate_type_test_case-output tc)
        (lambda ()
          (hydra_rewriting_deannotate_type
            (hydra_testing_deannotate_type_test_case-input tc)))))

    (define (run-flatten-let-terms-test path tc)
      (run-simple-test path
        (hydra_testing_flatten_let_terms_test_case-output tc)
        (lambda ()
          (hydra_rewriting_flatten_let_terms
            (hydra_testing_flatten_let_terms_test_case-input tc)))))

    (define (run-free-variables-test path tc)
      (run-simple-test path
        (hydra_testing_free_variables_test_case-output tc)
        (lambda ()
          (hydra_rewriting_free_variables_in_term
            (hydra_testing_free_variables_test_case-input tc)))))

    (define (run-lift-lambda-test path tc)
      (run-simple-test path
        (hydra_testing_lift_lambda_above_let_test_case-output tc)
        (lambda ()
          (hydra_rewriting_lift_lambda_above_let
            (hydra_testing_lift_lambda_above_let_test_case-input tc)))))

    (define (run-simplify-term-test path tc)
      (run-simple-test path
        (hydra_testing_simplify_term_test_case-output tc)
        (lambda ()
          (hydra_rewriting_simplify_term
            (hydra_testing_simplify_term_test_case-input tc)))))

    (define (run-normalize-type-vars-test path tc)
      (run-simple-test path
        (hydra_testing_normalize_type_variables_test_case-output tc)
        (lambda ()
          (hydra_rewriting_normalize_type_variables_in_term
            (hydra_testing_normalize_type_variables_test_case-input tc)))))

    (define (run-topological-sort-test path tc)
      (run-simple-test path
        (hydra_testing_topological_sort_test_case-expected tc)
        (lambda ()
          (hydra_sorting_topological_sort
            (hydra_testing_topological_sort_test_case-adjacency_list tc)))))

    (define (run-topological-sort-scc-test path tc)
      (run-simple-test path
        (hydra_testing_topological_sort_s_c_c_test_case-expected tc)
        (lambda ()
          (hydra_sorting_topological_sort_components
            (hydra_testing_topological_sort_s_c_c_test_case-adjacency_list tc)))))

    (define (run-serialization-test path tc)
      (run-simple-test path
        (hydra_testing_serialization_test_case-output tc)
        (lambda ()
          (hydra_serialization_print_expr
            (hydra_serialization_parenthesize
              (hydra_testing_serialization_test_case-input tc))))))

    (define (run-unshadow-variables-test path tc)
      (run-simple-test path
        (hydra_testing_unshadow_variables_test_case-output tc)
        (lambda ()
          (hydra_rewriting_unshadow_variables
            (hydra_testing_unshadow_variables_test_case-input tc)))))

    ;; ---- Medium test runners (need graph/context, Either results) ----

    (define (run-type-reduction-test path tc)
      (let ((cx (empty-context))
            (graph (get-test-graph)))
        (run-either-test path
          (hydra_testing_type_reduction_test_case-output tc)
          (((hydra_reduction_beta_reduce_type cx) graph)
           (hydra_testing_type_reduction_test_case-input tc)))))

    (define (run-eta-expansion-test path tc)
      (let ((cx (empty-context))
            (graph (get-test-graph)))
        (run-either-test path
          (hydra_testing_eta_expansion_test_case-output tc)
          (((hydra_reduction_eta_expand_typed_term cx) graph)
           (hydra_testing_eta_expansion_test_case-input tc)))))

    ;; ---- Inference tests ----

    (define (run-inference-test path tc)
      (let ((cx (empty-context))
            (graph (get-test-graph)))
        (guard (exn (#t
                     (display (string-append "FAIL: " path "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0)))
          (let ((result (((hydra_inference_infer_type_of cx) graph)
                          (hydra_testing_inference_test_case-input tc))))
            (if (eq? (car result) 'left)
                (begin
                  (display (string-append "FAIL: " path "\n"))
                  (display (string-append "  Inference ERROR: " (extract-error-msg (cadr result)) "\n"))
                  (list 0 1 0))
                ;; result is (right (pair (pair inferred-term type-scheme) context))
                (let* ((pair-val (cadr result))
                       (inner-pair (car pair-val))
                       (result-scheme (cadr inner-pair))
                       (expected-ts (hydra_testing_inference_test_case-output tc))
                       (expected-str (show-type-scheme expected-ts))
                       (actual-str (show-type-scheme result-scheme)))
                  (run-string-comparison-test path expected-str actual-str)))))))

    (define (run-inference-failure-test path tc)
      (let ((cx (empty-context))
            (graph (get-test-graph)))
        (guard (exn (#t
                     ;; Exception counts as failure (expected)
                     (list 1 0 0)))
          (let ((result (((hydra_inference_infer_type_of cx) graph)
                          (hydra_testing_inference_failure_test_case-input tc))))
            (if (eq? (car result) 'left)
                (list 1 0 0)  ;; Expected failure
                (begin
                  (display (string-append "FAIL: " path "\n"))
                  (display "  Expected inference failure but got success\n")
                  (list 0 1 0)))))))

    ;; ---- Type checking tests ----

    (define (type-scheme-to-type ts)
      "Convert a TypeScheme back to a Type by wrapping forall binders around the body."
      (let ((vars (hydra_core_type_scheme-variables ts))
            (body (hydra_core_type_scheme-type ts)))
        (let loop ((vs (reverse vars)) (t body))
          (if (null? vs) t
              (loop (cdr vs) (list 'forall (make-hydra_core_forall_type (car vs) t)))))))

    (define (run-type-checking-test path tc)
      (let ((cx (empty-context))
            (graph (get-test-graph)))
        (guard (exn (#t
                     (display (string-append "FAIL: " path "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0)))
          ;; Step 1: Infer type
          (let ((infer-result (((hydra_inference_infer_type_of cx) graph)
                                (hydra_testing_type_checking_test_case-input tc))))
            (if (eq? (car infer-result) 'left)
                (begin
                  (display (string-append "FAIL: " path "\n"))
                  (display (string-append "  Inference failed: " (extract-error-msg (cadr infer-result)) "\n"))
                  (list 0 1 0))
                (let* ((pair-val (cadr infer-result))
                       (inner-pair (car pair-val))
                       (inferred-term (car inner-pair))
                       (result-scheme (cadr inner-pair))
                       (infer-cx (cadr pair-val))
                       (inferred-type (type-scheme-to-type result-scheme))
                       ;; Step 2: Reconstruct type using typeOf
                       (type-of-result ((((hydra_checking_type_of infer-cx) graph) '()) inferred-term)))
                  (if (eq? (car type-of-result) 'left)
                      (begin
                        (display (string-append "FAIL: " path "\n"))
                        (display (string-append "  Type reconstruction failed: " (extract-error-msg (cadr type-of-result)) "\n"))
                        (list 0 1 0))
                      (let* ((reconstructed-type (car (cadr type-of-result)))
                             ;; Compare using alpha-equivalence
                             (expected-term (hydra_testing_type_checking_test_case-output_term tc))
                             (expected-type (hydra_testing_type_checking_test_case-output_type tc))
                             (term-ok? (alpha-equivalent-terms? expected-term inferred-term))
                             (type-ok? (alpha-equivalent-types? expected-type inferred-type))
                             (recon-ok? (alpha-equivalent-types? expected-type reconstructed-type)))
                        (if (and term-ok? type-ok? recon-ok?)
                            (list 1 0 0)
                            (begin
                              (display (string-append "FAIL: " path "\n"))
                              (when (not term-ok?)
                                (display "  Inferred term mismatch\n")
                                (display (string-append "    Expected: " (normalize-type-var-names (show-term expected-term)) "\n"))
                                (display (string-append "    Actual:   " (normalize-type-var-names (show-term inferred-term)) "\n")))
                              (when (not type-ok?)
                                (display "  Inferred type mismatch\n")
                                (display (string-append "    Expected: " (normalize-type-var-names (show-type expected-type)) "\n"))
                                (display (string-append "    Actual:   " (normalize-type-var-names (show-type inferred-type)) "\n")))
                              (when (not recon-ok?)
                                (display "  Reconstructed type mismatch\n")
                                (display (string-append "    Expected: " (normalize-type-var-names (show-type expected-type)) "\n"))
                                (display (string-append "    Actual:   " (normalize-type-var-names (show-type reconstructed-type)) "\n")))
                              (list 0 1 0)))))))))))

    (define (run-type-checking-failure-test path tc)
      ;; TODO: implement when test data is available
      (list 0 0 1))

    ;; ---- Variable occurs in type ----

    (define (run-variable-occurs-in-type-test path tc)
      (run-simple-test path
        (hydra_testing_variable_occurs_in_type_test_case-expected tc)
        (lambda ()
          ((hydra_unification_variable_occurs_in_type
             (hydra_testing_variable_occurs_in_type_test_case-variable tc))
           (hydra_testing_variable_occurs_in_type_test_case-type tc)))))

    ;; ---- Subst in type ----

    (define (run-subst-in-type-test path tc)
      (let* (;; Build TypeSubst from list of (name, type) pairs
             ;; Note: TypeSubst is transparent (bare alist map, not a record)
             (subst-alist (hydra_lib_maps_from_list
                            (hydra_testing_subst_in_type_test_case-substitution tc))))
        (run-simple-test path
          (hydra_testing_subst_in_type_test_case-output tc)
          (lambda ()
            ((hydra_substitution_subst_in_type subst-alist)
             (hydra_testing_subst_in_type_test_case-input tc))))))

    ;; ---- Unify types ----

    (define (run-unify-types-test path tc)
      (let* ((cx (empty-context))
             ;; Build schema types as Hydra alist map from the list of names
             (schema-entries (map (lambda (n) (list n (make-hydra_core_type_scheme '() (list 'variable n) '())))
                                  (hydra_testing_unify_types_test_case-schema_types tc)))
             (schema-types (hydra_lib_maps_from_list schema-entries))
             (result (((((hydra_unification_unify_types cx) schema-types)
                         (hydra_testing_unify_types_test_case-left tc))
                        (hydra_testing_unify_types_test_case-right tc))
                       "test"))
             (expected (hydra_testing_unify_types_test_case-expected tc)))
        (guard (exn (#t
                     (display (string-append "FAIL: " path "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0)))
          (cond
            ((eq? (car expected) 'left)
             ;; Expected failure
             (if (eq? (car result) 'left)
                 (list 1 0 0)
                 (begin
                   (display (string-append "FAIL: " path "\n"))
                   (display "  Expected unification failure but got success\n")
                   (list 0 1 0))))

            ((eq? (car expected) 'right)
             ;; Expected success
             (if (eq? (car result) 'left)
                 (begin
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  Expected unification success but got failure: "
                                           (obj->string (cadr result)) "\n"))
                   (list 0 1 0))
                 (let* ((actual-subst (cadr result))
                        (expected-subst (cadr expected))
                        ;; Normalize TypeSubst maps for comparison
                        (normalize-subst (lambda (ts)
                                           (my-list-sort
                                             (lambda (a b) (string<? (car a) (car b)))
                                             (hydra_lib_maps_to_list ts)))))
                   (if (equal? (normalize-subst expected-subst) (normalize-subst actual-subst))
                       (list 1 0 0)
                       (begin
                         (display (string-append "FAIL: " path "\n"))
                         (display "  Expected subst: ") (write (normalize-subst expected-subst)) (newline)
                         (display "  Actual subst:   ") (write (normalize-subst actual-subst)) (newline)
                         (list 0 1 0))))))

            (else
             (display (string-append "FAIL: " path " - unexpected expected format\n"))
             (list 0 1 0))))))

    ;; ---- Join types ----

    (define (run-join-types-test path tc)
      (let* ((cx (empty-context))
             (result ((((hydra_unification_join_types cx)
                         (hydra_testing_join_types_test_case-left tc))
                        (hydra_testing_join_types_test_case-right tc))
                       "test"))
             (expected (hydra_testing_join_types_test_case-expected tc)))
        (guard (exn (#t
                     (display (string-append "FAIL: " path "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0)))
          (cond
            ((eq? (car expected) 'left)
             ;; Expected failure
             (if (eq? (car result) 'left)
                 (list 1 0 0)
                 (begin
                   (display (string-append "FAIL: " path "\n"))
                   (display "  Expected join failure but got success\n")
                   (list 0 1 0))))

            ((eq? (car expected) 'right)
             ;; Expected success
             (if (eq? (car result) 'left)
                 (begin
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  Expected join success but got failure: "
                                           (obj->string (cadr result)) "\n"))
                   (list 0 1 0))
                 (let ((actual (cadr result))
                       (expected-val (cadr expected)))
                   (if (equal? expected-val actual)
                       (list 1 0 0)
                       (begin
                         (display (string-append "FAIL: " path "\n"))
                         (display "  Expected: ") (write expected-val) (newline)
                         (display "  Actual:   ") (write actual) (newline)
                         (list 0 1 0))))))

            (else
             (display (string-append "FAIL: " path " - unexpected expected format\n"))
             (list 0 1 0))))))

    ;; ---- Topological sort bindings ----

    (define (run-topological-sort-bindings-test path tc)
      (let* ((binding-map (hydra_lib_maps_from_list
                            (hydra_testing_topological_sort_bindings_test_case-bindings tc)))
             (result (hydra_rewriting_topological_sort_binding_map binding-map))
             ;; Compare as sets of sets (order within SCCs doesn't matter)
             (result-sets (map (lambda (scc) (my-list-sort string<? scc)) result))
             (expected-sets (map (lambda (scc) (my-list-sort string<? scc))
                                 (hydra_testing_topological_sort_bindings_test_case-expected tc)))
             (result-sorted (my-list-sort (lambda (a b)
                                            (string<? (if (null? a) "" (car a))
                                                      (if (null? b) "" (car b))))
                                          result-sets))
             (expected-sorted (my-list-sort (lambda (a b)
                                              (string<? (if (null? a) "" (car a))
                                                        (if (null? b) "" (car b))))
                                            expected-sets)))
        (if (equal? expected-sorted result-sorted)
            (list 1 0 0)
            (begin
              (display (string-append "FAIL: " path "\n"))
              (display "  Expected: ") (write expected-sorted) (newline)
              (display "  Actual:   ") (write result-sorted) (newline)
              (list 0 1 0)))))

    ;; ---- Hoist case statements ----

    (define (run-hoist-case-statements-test path tc)
      (let ((eg (empty-graph)))
        (run-simple-test path
          (hydra_testing_hoist_case_statements_test_case-output tc)
          (lambda ()
            ((hydra_hoisting_hoist_case_statements eg)
             (hydra_testing_hoist_case_statements_test_case-input tc))))))

    ;; ---- Hoist subterms ----

    (define (predicate-fn pred)
      "Convert a HoistPredicate union to a Scheme predicate function.
       Each predicate takes a (pair path term) and returns boolean."
      (let ((pred-type (car pred)))
        (cond
          ((eq? pred-type 'nothing)
           (lambda (pair) #f))
          ((eq? pred-type 'lists)
           (lambda (pair)
             (let ((term (cadr pair)))
               (and (pair? term) (eq? (car term) 'list)))))
          ((eq? pred-type 'applications)
           (lambda (pair)
             (let ((term (cadr pair)))
               (and (pair? term) (eq? (car term) 'application)))))
          ((eq? pred-type 'case_statements)
           (lambda (pair)
             (let ((term (cadr pair)))
               (and (pair? term) (eq? (car term) 'function)
                    (let ((f (cadr term)))
                      (and (pair? f) (eq? (car f) 'elimination)))))))
          ;; default: never hoist
          (else (lambda (pair) #f)))))

    (define (run-hoist-subterms-test path tc)
      (let ((eg (empty-graph))
            (pred (predicate-fn (hydra_testing_hoist_subterms_test_case-predicate tc))))
        (run-simple-test path
          (hydra_testing_hoist_subterms_test_case-output tc)
          (lambda ()
            (((hydra_hoisting_hoist_subterms pred) eg)
             (hydra_testing_hoist_subterms_test_case-input tc))))))

    ;; ---- Hoist let bindings ----

    (define (run-hoist-let-bindings-test path tc)
      ;; Compare via show (like Java)
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let* ((result (hydra_hoisting_hoist_all_let_bindings
                         (hydra_testing_hoist_let_bindings_test_case-input tc)))
               (expected-str (show-let (hydra_testing_hoist_let_bindings_test_case-output tc)))
               (actual-str (show-let result)))
          (run-string-comparison-test path expected-str actual-str))))

    ;; ---- Hoist polymorphic let bindings ----

    (define (run-hoist-polymorphic-let-bindings-test path tc)
      ;; Use (lambda (_) #t) predicate like Java: b -> true
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let* ((result ((hydra_hoisting_hoist_polymorphic_let_bindings (lambda (_) #t))
                         (hydra_testing_hoist_polymorphic_let_bindings_test_case-input tc)))
               (expected-str (show-let (hydra_testing_hoist_polymorphic_let_bindings_test_case-output tc)))
               (actual-str (show-let result)))
          (run-string-comparison-test path expected-str actual-str))))

    ;; ---- Rewrite term ----

    (define (run-rewrite-term-test path tc)
      (let* ((rewriter (hydra_testing_rewrite_term_test_case-rewriter tc))
             (rewriter-type (car rewriter))
             (rewrite-impl
               (cond
                 ((eq? rewriter-type 'replace_foo_with_bar)
                  (lambda (recurse)
                    (lambda (term)
                      (if (and (pair? term) (eq? (car term) 'literal)
                               (let ((lit (cadr term)))
                                 (and (pair? lit) (eq? (car lit) 'string)
                                      (equal? (cadr lit) "foo"))))
                          (list 'literal (list 'string "bar"))
                          (recurse term)))))
                 ((eq? rewriter-type 'replace_int32_with_int64)
                  (lambda (recurse)
                    (lambda (term)
                      (if (and (pair? term) (eq? (car term) 'literal)
                               (let ((lit (cadr term)))
                                 (and (pair? lit) (eq? (car lit) 'integer)
                                      (let ((iv (cadr lit)))
                                        (and (pair? iv) (eq? (car iv) 'int32))))))
                          (let ((val (cadr (cadr (cadr term)))))
                            (list 'literal (list 'integer (list 'int64 val))))
                          (recurse term)))))
                 ;; Default: identity rewrite
                 (else (lambda (recurse) (lambda (term) (recurse term)))))))
        (run-simple-test path
          (hydra_testing_rewrite_term_test_case-output tc)
          (lambda ()
            ((hydra_rewriting_rewrite_term rewrite-impl)
             (hydra_testing_rewrite_term_test_case-input tc))))))

    ;; ---- Rewrite type ----

    (define (run-rewrite-type-test path tc)
      (let* ((rewriter (hydra_testing_rewrite_type_test_case-rewriter tc))
             (rewriter-type (car rewriter))
             (rewrite-impl
               (cond
                 ((eq? rewriter-type 'replace_string_with_int32)
                  (lambda (recurse)
                    (lambda (typ)
                      (if (and (pair? typ) (eq? (car typ) 'literal)
                               (let ((lt (cadr typ)))
                                 (and (pair? lt) (eq? (car lt) 'string))))
                          (list 'literal (list 'integer (list 'int32 '())))
                          (recurse typ)))))
                 ;; Default: identity rewrite
                 (else (lambda (recurse) (lambda (typ) (recurse typ)))))))
        (run-simple-test path
          (hydra_testing_rewrite_type_test_case-output tc)
          (lambda ()
            ((hydra_rewriting_rewrite_type rewrite-impl)
             (hydra_testing_rewrite_type_test_case-input tc))))))

    ;; ---- Fold over term ----

    (define (get-int32 t)
      "Extract an int32 value from a term, or return 0."
      (if (and (pair? t) (eq? (car t) 'literal)
               (let ((lit (cadr t)))
                 (and (pair? lit) (eq? (car lit) 'integer)
                      (let ((iv (cadr lit)))
                        (and (pair? iv) (eq? (car iv) 'int32))))))
          (cadr (cadr (cadr t)))
          0))

    (define (get-list-length t)
      "If term is a list, return a list containing its length; otherwise empty list."
      (if (and (pair? t) (eq? (car t) 'list))
          (list (length (cadr t)))
          '()))

    (define (get-label t)
      "If term is a pair whose first element is a string literal, return a list with that literal."
      (if (and (pair? t) (eq? (car t) 'pair))
          (let* ((pair-val (cadr t))
                 (fst (car pair-val)))
            (if (and (pair? fst) (eq? (car fst) 'literal)
                     (let ((lit (cadr fst)))
                       (and (pair? lit) (eq? (car lit) 'string))))
                (list (cadr fst))
                '()))
          '()))

    (define (run-fold-over-term-test path tc)
      (let ((order (hydra_testing_fold_over_term_test_case-traversal_order tc))
            (operation (hydra_testing_fold_over_term_test_case-operation tc))
            (input (hydra_testing_fold_over_term_test_case-input tc)))
        (guard (exn (#t
                     (display (string-append "FAIL: " path "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0)))
          (let ((op-type (car operation)))
            (let ((result
                    (cond
                      ((eq? op-type 'sum_int32_literals)
                       (let ((sum ((((hydra_rewriting_fold_over_term order)
                                      (lambda (acc) (lambda (t) (+ acc (get-int32 t))))) 0) input)))
                         (list 'literal (list 'integer (list 'int32 sum)))))

                      ((eq? op-type 'collect_list_lengths)
                       (let ((lengths ((((hydra_rewriting_fold_over_term order)
                                          (lambda (acc) (lambda (t) (append acc (get-list-length t))))) '()) input)))
                         (list 'list (map (lambda (len) (list 'literal (list 'integer (list 'int32 len)))) lengths))))

                      ((eq? op-type 'collect_labels)
                       (let ((labels ((((hydra_rewriting_fold_over_term order)
                                         (lambda (acc) (lambda (t) (append acc (get-label t))))) '()) input)))
                         (list 'list (map (lambda (label) (list 'literal label)) labels))))

                      (else (list 'unit '())))))
              (if (terms-match? result (hydra_testing_fold_over_term_test_case-output tc))
                  (list 1 0 0)
                  (begin
                    (display (string-append "FAIL: " path "\n"))
                    (guard (exn (#t
                                 (display "  Expected (raw): ") (write (hydra_testing_fold_over_term_test_case-output tc)) (newline)
                                 (display "  Actual (raw):   ") (write result) (newline)))
                      (display (string-append "  Expected: " (show-term (hydra_testing_fold_over_term_test_case-output tc)) "\n"))
                      (display (string-append "  Actual:   " (show-term result) "\n")))
                    (list 0 1 0))))))))

    ;; ---- JSON tests ----

    (define (run-json-parser-test path tc)
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let ((result (hydra_json_parser_parse_json
                        (hydra_testing_parser_test_case-input tc))))
          (if (terms-match? result (hydra_testing_parser_test_case-output tc))
              (list 1 0 0)
              (begin
                (display (string-append "FAIL: " path "\n"))
                (display "  Expected (raw): ") (write (hydra_testing_parser_test_case-output tc)) (newline)
                (display "  Actual (raw):   ") (write result) (newline)
                (list 0 1 0))))))

    (define (run-json-writer-test path tc)
      (run-simple-test path
        (hydra_testing_writer_test_case-output tc)
        (lambda ()
          (hydra_json_writer_print_json
            (hydra_testing_writer_test_case-input tc)))))

    (define (run-json-coder-test path tc)
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let* ((empty-types hydra_lib_maps_empty)
               (tc-type (hydra_testing_json_coder_test_case-type tc))
               (tc-term (hydra_testing_json_coder_test_case-term tc))
               (tc-json (hydra_testing_json_coder_test_case-json tc))
               (encode-result (hydra_json_encode_to_json tc-term)))
          (if (eq? (car encode-result) 'left)
              (begin
                (display (string-append "FAIL: " path "\n"))
                (display (string-append "  JSON encode failed: " (obj->string (cadr encode-result)) "\n"))
                (list 0 1 0))
              (let ((encoded (cadr encode-result)))
                (if (not (terms-match? tc-json encoded))
                    (begin
                      (display (string-append "FAIL: " path "\n"))
                      (display "  JSON encode mismatch\n")
                      (display "  Expected (raw): ") (write tc-json) (newline)
                      (display "  Actual (raw):   ") (write encoded) (newline)
                      (list 0 1 0))
                    (let ((decode-result ((((hydra_json_decode_from_json empty-types)
                                            (make-hydra_core_name "test"))
                                           tc-type)
                                          encoded)))
                      (if (eq? (car decode-result) 'left)
                          (begin
                            (display (string-append "FAIL: " path "\n"))
                            (display (string-append "  JSON decode failed: " (obj->string (cadr decode-result)) "\n"))
                            (list 0 1 0))
                          (let ((decoded (cadr decode-result)))
                            (if (terms-match? tc-term decoded)
                                (list 1 0 0)
                                (begin
                                  (display (string-append "FAIL: " path "\n"))
                                  (display "  JSON roundtrip term mismatch\n")
                                  (display (string-append "  Expected: " (or (show-term tc-term) "(?)") "\n"))
                                  (display (string-append "  Actual:   " (or (show-term decoded) "(?)") "\n"))
                                  (list 0 1 0))))))))))))

    (define (run-json-roundtrip-test path tc)
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let* ((empty-types hydra_lib_maps_empty)
               (tc-type (hydra_testing_json_roundtrip_test_case-type tc))
               (tc-term (hydra_testing_json_roundtrip_test_case-term tc))
               (encode-result (hydra_json_encode_to_json tc-term)))
          (if (eq? (car encode-result) 'left)
              (begin
                (display (string-append "FAIL: " path "\n"))
                (display (string-append "  JSON encode failed: " (obj->string (cadr encode-result)) "\n"))
                (list 0 1 0))
              (let* ((encoded (cadr encode-result))
                     (decode-result ((((hydra_json_decode_from_json empty-types)
                                       (make-hydra_core_name "test"))
                                      tc-type)
                                     encoded)))
                (if (eq? (car decode-result) 'left)
                    (begin
                      (display (string-append "FAIL: " path "\n"))
                      (display (string-append "  JSON decode failed: " (obj->string (cadr decode-result)) "\n"))
                      (list 0 1 0))
                    (let ((decoded (cadr decode-result)))
                      (if (terms-match? tc-term decoded)
                          (list 1 0 0)
                          (begin
                            (display (string-append "FAIL: " path "\n"))
                            (display "  JSON roundtrip mismatch\n")
                            (display (string-append "  Expected: " (or (show-term tc-term) "(?)") "\n"))
                            (display (string-append "  Actual:   " (or (show-term decoded) "(?)") "\n"))
                            (list 0 1 0))))))))))

    (define (run-json-decode-test path tc)
      (guard (exn (#t
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                   (list 0 1 0)))
        (let* ((empty-types hydra_lib_maps_empty)
               (tc-type (hydra_testing_json_decode_test_case-type tc))
               (tc-json (hydra_testing_json_decode_test_case-json tc))
               (expected (hydra_testing_json_decode_test_case-expected tc))
               (decode-result ((((hydra_json_decode_from_json empty-types)
                                  (make-hydra_core_name "test"))
                                 tc-type)
                                tc-json)))
          (cond
            ((eq? (car expected) 'left)
             ;; Expected failure
             (if (eq? (car decode-result) 'left)
                 (list 1 0 0)
                 (begin
                   (display (string-append "FAIL: " path "\n"))
                   (display "  Expected decode failure but got success\n")
                   (list 0 1 0))))
            ((eq? (car expected) 'right)
             ;; Expected success
             (if (eq? (car decode-result) 'left)
                 (begin
                   (display (string-append "FAIL: " path "\n"))
                   (display (string-append "  JSON decode failed: " (obj->string (cadr decode-result)) "\n"))
                   (list 0 1 0))
                 (let ((actual (cadr decode-result))
                       (expected-term (cadr expected)))
                   (if (terms-match? expected-term actual)
                       (list 1 0 0)
                       (begin
                         (display (string-append "FAIL: " path "\n"))
                         (display "  JSON decode mismatch\n")
                         (display (string-append "  Expected: " (or (show-term expected-term) "(?)") "\n"))
                         (display (string-append "  Actual:   " (or (show-term actual) "(?)") "\n"))
                         (list 0 1 0))))))
            (else (list 0 1 0))))))

    (define (run-json-encode-test path tc)
      ;; JsonEncodeTestCase has :term and :expected but no :type, so we cannot create a coder.
      ;; Java also skips this.
      (list 0 0 1))

    ;; ==========================================================================
    ;; Test case dispatcher
    ;; ==========================================================================

    (define (run-test-case path tcase)
      (guard (exn (#t
                   (let ((full (string-append path " > "
                                 (hydra_testing_test_case_with_metadata-name tcase))))
                     (display (string-append "FAIL: " full "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0))))
        (let* ((tname (hydra_testing_test_case_with_metadata-name tcase))
               (full (string-append path " > " tname))
               (tags (hydra_testing_test_case_with_metadata-tags tcase))
               (disabled? (member "disabled" tags))
               (tc (hydra_testing_test_case_with_metadata-case tcase)))
          (if disabled?
              (list 0 0 1)
              (let ((case-type (car tc))
                    (case-data (cadr tc)))
                (cond
                  ((eq? case-type 'evaluation)              (run-evaluation-test full case-data))
                  ((eq? case-type 'alpha_conversion)        (run-alpha-conversion-test full case-data))
                  ((eq? case-type 'case_conversion)         (run-case-conversion-test full case-data))
                  ((eq? case-type 'deannotate_term)         (run-deannotate-term-test full case-data))
                  ((eq? case-type 'deannotate_type)         (run-deannotate-type-test full case-data))
                  ((eq? case-type 'flatten_let_terms)       (run-flatten-let-terms-test full case-data))
                  ((eq? case-type 'free_variables)          (run-free-variables-test full case-data))
                  ((eq? case-type 'lift_lambda_above_let)   (run-lift-lambda-test full case-data))
                  ((eq? case-type 'simplify_term)           (run-simplify-term-test full case-data))
                  ((eq? case-type 'normalize_type_variables) (run-normalize-type-vars-test full case-data))
                  ((eq? case-type 'topological_sort)        (run-topological-sort-test full case-data))
                  ((eq? case-type 'topological_sort_s_c_c)  (run-topological-sort-scc-test full case-data))
                  ((eq? case-type 'serialization)           (run-serialization-test full case-data))
                  ((eq? case-type 'type_reduction)          (run-type-reduction-test full case-data))
                  ((eq? case-type 'unshadow_variables)      (run-unshadow-variables-test full case-data))
                  ((eq? case-type 'eta_expansion)           (run-eta-expansion-test full case-data))
                  ((eq? case-type 'inference)               (run-inference-test full case-data))
                  ((eq? case-type 'inference_failure)       (run-inference-failure-test full case-data))
                  ((eq? case-type 'type_checking)           (run-type-checking-test full case-data))
                  ((eq? case-type 'type_checking_failure)   (run-type-checking-failure-test full case-data))
                  ((eq? case-type 'variable_occurs_in_type) (run-variable-occurs-in-type-test full case-data))
                  ((eq? case-type 'subst_in_type)           (run-subst-in-type-test full case-data))
                  ((eq? case-type 'unify_types)             (run-unify-types-test full case-data))
                  ((eq? case-type 'join_types)              (run-join-types-test full case-data))
                  ((eq? case-type 'topological_sort_bindings) (run-topological-sort-bindings-test full case-data))
                  ((eq? case-type 'hoist_case_statements)   (run-hoist-case-statements-test full case-data))
                  ((eq? case-type 'hoist_subterms)          (run-hoist-subterms-test full case-data))
                  ((eq? case-type 'hoist_let_bindings)      (run-hoist-let-bindings-test full case-data))
                  ((eq? case-type 'hoist_polymorphic_let_bindings) (run-hoist-polymorphic-let-bindings-test full case-data))
                  ((eq? case-type 'rewrite_term)            (run-rewrite-term-test full case-data))
                  ((eq? case-type 'rewrite_type)            (run-rewrite-type-test full case-data))
                  ((eq? case-type 'fold_over_term)          (run-fold-over-term-test full case-data))
                  ((eq? case-type 'json_parser)             (run-json-parser-test full case-data))
                  ((eq? case-type 'json_writer)             (run-json-writer-test full case-data))
                  ((eq? case-type 'json_coder)              (run-json-coder-test full case-data))
                  ((eq? case-type 'json_roundtrip)          (run-json-roundtrip-test full case-data))
                  ((eq? case-type 'json_decode)             (run-json-decode-test full case-data))
                  ((eq? case-type 'json_encode)             (run-json-encode-test full case-data))
                  ;; Skip remaining unimplemented test types
                  ((eq? case-type 'delegated_evaluation)    (list 0 0 1))
                  (else                                     (list 0 0 1))))))))

    (define (run-test-group path group)
      (guard (exn (#t
                   (let* ((gname (hydra_testing_test_group-name group))
                          (full (if (string=? path "") gname
                                    (string-append path " > " gname))))
                     (display (string-append "GROUP FAIL: " full "\n"))
                     (display (string-append "  EXCEPTION: " (obj->string exn) "\n"))
                     (list 0 1 0))))
        (let* ((gname (hydra_testing_test_group-name group))
               (full (if (string=? path "") gname (string-append path " > " gname)))
               (sub-results (map (lambda (sg) (run-test-group full sg))
                                 (hydra_testing_test_group-subgroups group)))
               (case-results (map (lambda (tc) (run-test-case full tc))
                                  (hydra_testing_test_group-cases group)))
               (all (append sub-results case-results)))
          (list (apply + (map car all))
                (apply + (map cadr all))
                (apply + (map caddr all))))))

)))  ;; close begin and define-library

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
        (body (hydra_core_type_scheme-body ts)))
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
         (result (hydra_dependencies_topological_sort_binding_map binding-map))
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
                              (list 0 1 0)))))))))))))

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
         (if (eq? (car decode-result) 'left) (list 1 0 0)
             (begin
               (display (string-append "FAIL: " path "\n"))
               (display "  Expected decode failure but got success\n")
               (list 0 1 0))))
        ((eq? (car expected) 'right)
         (if (eq? (car decode-result) 'left)
             (begin
               (display (string-append "FAIL: " path "\n"))
               (display (string-append "  JSON decode failed: " (obj->string (cadr decode-result)) "\n"))
               (list 0 1 0))
             (let ((actual (cadr decode-result))
                   (expected-term (cadr expected)))
               (if (terms-match? expected-term actual) (list 1 0 0)
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

)

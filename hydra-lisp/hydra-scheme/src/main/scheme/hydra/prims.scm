(define-library (hydra prims)
  (import (scheme base) (scheme cxr) (scheme inexact)
          (hydra core)
          (hydra context)
          (hydra graph)
          (hydra extract core)
          (hydra lib maps)
          (hydra lib pairs)
          (hydra lib sets))
  (export collect-type-vars-ordered build-type-scheme
          wrap-other-error
          tc-bigfloat tc-bigint tc-boolean
          tc-float32 tc-float64
          tc-int8 tc-int16 tc-int32 tc-int64
          tc-uint8 tc-uint16 tc-uint32 tc-uint64
          tc-string tc-binary
          tc-list tc-set tc-map tc-optional tc-either tc-pair
          tc-variable tc-term tc-comparison
          tc-function tc-function-with-reduce
          prim0 prim1 prim2 prim3)
  (begin

    ;; ============================================================================
    ;; Type scheme helpers
    ;; ============================================================================

    (define (collect-type-vars-ordered typ)
      "Collect type variable names from a Hydra type in order of first appearance."
      (let ((seen '())
            (result '()))
        (let visit ((t typ))
          (when (and (pair? t) (not (null? t)))
            (case (car t)
              ((variable)
               (let ((v (cadr t)))
                 (when (not (member v seen))
                   (set! seen (cons v seen))
                   (set! result (cons v result)))))
              ((function)
               (let ((ft (cadr t)))
                 (visit (hydra_core_function_type-domain ft))
                 (visit (hydra_core_function_type-codomain ft))))
              ((list set maybe)
               (visit (cadr t)))
              ((map)
               (let ((mt (cadr t)))
                 (visit (hydra_core_map_type-keys mt))
                 (visit (hydra_core_map_type-values mt))))
              ((pair)
               (let ((pt (cadr t)))
                 (visit (hydra_core_pair_type-first pt))
                 (visit (hydra_core_pair_type-second pt))))
              ((either)
               (let ((et (cadr t)))
                 (visit (hydra_core_either_type-left et))
                 (visit (hydra_core_either_type-right et))))
              (else #f))))
        (reverse result)))

    (define (build-type-scheme variables inputs output . rest)
      "Build a TypeScheme from TermCoder types. Uses the actual types from
       the TermCoders to construct proper function types for inference.
       Auto-detects type variables from the types.
       Optional constraints: alist of (varname . TypeVariableMetadata)."
      (let* ((constraints (if (and (pair? rest) (car rest)) (car rest) '()))
             (out-type (let ((t (hydra_graph_term_coder-type output)))
                         (if (or (not t) (null? t)) (list 'unit '()) t)))
             (fun-type (let loop ((ins (reverse inputs)) (acc out-type))
                         (if (null? ins)
                             acc
                             (let* ((in-tc (car ins))
                                    (in-type (let ((t (hydra_graph_term_coder-type in-tc)))
                                               (if (or (not t) (null? t)) (list 'unit '()) t))))
                               (loop (cdr ins)
                                     (list 'function (make-hydra_core_function_type in-type acc)))))))
             (all-vars (collect-type-vars-ordered fun-type))
             ;; Exclude qualified names (containing dots) — those are nominal type references
             (detected-vars (let remove-qualified ((vs all-vars) (acc '()))
                             (if (null? vs) (reverse acc)
                                 (let* ((v (car vs))
                                        (has-dot (let loop ((i 0))
                                                   (and (< i (string-length v))
                                                        (or (char=? (string-ref v i) #\.)
                                                            (loop (+ i 1)))))))
                                   (remove-qualified (cdr vs) (if has-dot acc (cons v acc)))))))
             (vars (if (and (pair? variables) (not (null? variables)))
                       variables
                       detected-vars)))
        (make-hydra_core_type_scheme vars fun-type constraints)))

    ;; ============================================================================
    ;; Error helpers
    ;; ============================================================================

    (define (wrap-other-error cx result)
      (if (eq? (car result) 'right)
          result
          (let* ((ic (cadr result))
                 (obj (if (hydra_context_in_context? ic) (hydra_context_in_context-object ic) ic))
                 (ctx (if (hydra_context_in_context? ic) (hydra_context_in_context-context ic) cx)))
            (list 'left (make-hydra_context_in_context (list 'other (make-hydra_context_in_context obj ctx)) ctx)))))

    ;; ============================================================================
    ;; TermCoder constructors
    ;; ============================================================================

    ;; Scalar TermCoders

    (define (tc-bigfloat)
      (make-hydra_graph_term_coder
       (list 'literal (list 'float (list 'bigfloat '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_bigfloat cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'float (list 'bigfloat (inexact v)))))))))

    (define (tc-bigint)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'bigint '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_bigint cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'bigint v))))))))

    (define (tc-boolean)
      (make-hydra_graph_term_coder
       (list 'literal (list 'boolean '()))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_boolean cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'boolean v)))))))

    (define (tc-float32)
      (make-hydra_graph_term_coder
       (list 'literal (list 'float (list 'float32 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_float32 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'float (list 'float32 (inexact v)))))))))

    (define (tc-float64)
      (make-hydra_graph_term_coder
       (list 'literal (list 'float (list 'float64 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_float64 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'float (list 'float64 (inexact v)))))))))

    (define (tc-int8)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'int8 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_int8 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'int8 v))))))))

    (define (tc-int16)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'int16 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_int16 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'int16 v))))))))

    (define (tc-int32)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'int32 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_int32 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'int32 v))))))))

    (define (tc-int64)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'int64 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_int64 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'int64 v))))))))

    (define (tc-uint8)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'uint8 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_uint8 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'uint8 v))))))))

    (define (tc-uint16)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'uint16 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_uint16 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'uint16 v))))))))

    (define (tc-uint32)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'uint32 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_uint32 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'uint32 v))))))))

    (define (tc-uint64)
      (make-hydra_graph_term_coder
       (list 'literal (list 'integer (list 'uint64 '())))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_uint64 cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'integer (list 'uint64 v))))))))

    (define (tc-string)
      (make-hydra_graph_term_coder
       (list 'literal (list 'string '()))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_string cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'string v)))))))

    (define (tc-binary)
      (make-hydra_graph_term_coder
       (list 'literal (list 'binary '()))
        (lambda (cx) (lambda (g) (lambda (t) (((hydra_extract_core_binary cx) g) t))))
        (lambda (cx) (lambda (v) (list 'right (list 'literal (list 'binary v)))))))

    ;; Container TermCoders

    (define (tc-list el-coder)
      (make-hydra_graph_term_coder
       (list 'list (hydra_graph_term_coder-type el-coder))
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              ((((hydra_extract_core_list_of cx)
                 (lambda (term) ((((hydra_graph_term_coder-encode el-coder) cx) g) term)))
                g) t))))
        (lambda (cx)
          (lambda (lst)
            (let loop ((items lst) (acc '()))
              (if (null? items)
                  (list 'right (list 'list (reverse acc)))
                  (let ((r (((hydra_graph_term_coder-decode el-coder) cx) (car items))))
                    (if (eq? (car r) 'left)
                        r
                        (loop (cdr items) (cons (cadr r) acc))))))))))

    (define (tc-set el-coder)
      (make-hydra_graph_term_coder
       (list 'set (hydra_graph_term_coder-type el-coder))
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              ((((hydra_extract_core_set_of cx)
                 (lambda (term) ((((hydra_graph_term_coder-encode el-coder) cx) g) term)))
                g) t))))
        (lambda (cx)
          (lambda (s)
            (let loop ((items (hydra_lib_sets_to_list s)) (acc '()))
              (if (null? items)
                  (list 'right (list 'set (hydra_lib_sets_from_list (reverse acc))))
                  (let ((r (((hydra_graph_term_coder-decode el-coder) cx) (car items))))
                    (if (eq? (car r) 'left)
                        r
                        (loop (cdr items) (cons (cadr r) acc))))))))))

    (define (tc-map key-coder val-coder)
      (make-hydra_graph_term_coder
       (list 'map (make-hydra_core_map_type (hydra_graph_term_coder-type key-coder) (hydra_graph_term_coder-type val-coder)))
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              (((((hydra_extract_core_map cx)
                  (lambda (term) ((((hydra_graph_term_coder-encode key-coder) cx) g) term)))
                 (lambda (term) ((((hydra_graph_term_coder-encode val-coder) cx) g) term)))
                g) t))))
        (lambda (cx)
          (lambda (m)
            (let loop ((pairs (hydra_lib_maps_to_list m)) (acc '()))
              (if (null? pairs)
                  (list 'right (list 'map (hydra_lib_maps_from_list (reverse acc))))
                  (let* ((p (car pairs))
                         (kr (((hydra_graph_term_coder-decode key-coder) cx) (hydra_lib_pairs_first p))))
                    (if (eq? (car kr) 'left)
                        kr
                        (let ((vr (((hydra_graph_term_coder-decode val-coder) cx) (hydra_lib_pairs_second p))))
                          (if (eq? (car vr) 'left)
                              vr
                              (loop (cdr pairs) (cons (list (cadr kr) (cadr vr)) acc))))))))))))

    (define (tc-optional el-coder)
      (make-hydra_graph_term_coder
       (list 'maybe (hydra_graph_term_coder-type el-coder))
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              ((((hydra_extract_core_maybe_term cx)
                 (lambda (term) ((((hydra_graph_term_coder-encode el-coder) cx) g) term)))
                g) t))))
        (lambda (cx)
          (lambda (mv)
            (cond
              ((null? mv) (list 'right (list 'maybe (list 'nothing))))
              ((and (pair? mv) (eq? (car mv) 'nothing)) (list 'right (list 'maybe (list 'nothing))))
              ((and (pair? mv) (eq? (car mv) 'just))
               (let ((r (((hydra_graph_term_coder-decode el-coder) cx) (cadr mv))))
                 (if (eq? (car r) 'left) r (list 'right (list 'maybe (cadr r))))))
              (else
               (let ((r (((hydra_graph_term_coder-decode el-coder) cx) mv)))
                 (if (eq? (car r) 'left) r (list 'right (list 'maybe (cadr r)))))))))))

    (define (tc-either left-coder right-coder)
      (make-hydra_graph_term_coder
       (list 'either (make-hydra_core_either_type (hydra_graph_term_coder-type left-coder) (hydra_graph_term_coder-type right-coder)))
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              (((((hydra_extract_core_either_term cx)
                  (lambda (term) ((((hydra_graph_term_coder-encode left-coder) cx) g) term)))
                 (lambda (term) ((((hydra_graph_term_coder-encode right-coder) cx) g) term)))
                g) t))))
        (lambda (cx)
          (lambda (ev)
            (if (eq? (car ev) 'left)
                (let ((r (((hydra_graph_term_coder-decode left-coder) cx) (cadr ev))))
                  (if (eq? (car r) 'left) r (list 'right (list 'either (list 'left (cadr r))))))
                (let ((r (((hydra_graph_term_coder-decode right-coder) cx) (cadr ev))))
                  (if (eq? (car r) 'left) r (list 'right (list 'either (list 'right (cadr r)))))))))))

    (define (tc-pair first-coder second-coder)
      (make-hydra_graph_term_coder
       (list 'pair (make-hydra_core_pair_type (hydra_graph_term_coder-type first-coder) (hydra_graph_term_coder-type second-coder)))
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              (((((hydra_extract_core_pair cx)
                  (lambda (term) ((((hydra_graph_term_coder-encode first-coder) cx) g) term)))
                 (lambda (term) ((((hydra_graph_term_coder-encode second-coder) cx) g) term)))
                g) t))))
        (lambda (cx)
          (lambda (p)
            (let ((fr (((hydra_graph_term_coder-decode first-coder) cx) (hydra_lib_pairs_first p))))
              (if (eq? (car fr) 'left) fr
                  (let ((sr (((hydra_graph_term_coder-decode second-coder) cx) (hydra_lib_pairs_second p))))
                    (if (eq? (car sr) 'left) sr
                        (list 'right (list 'pair (list (cadr fr) (cadr sr))))))))))))

    ;; Term/variable passthrough coders

    (define (tc-variable name)
      (make-hydra_graph_term_coder
       (list 'variable name)
        (lambda (cx) (lambda (g) (lambda (t) (list 'right t))))
        (lambda (cx) (lambda (t) (list 'right t)))))

    (define (tc-term)
      (make-hydra_graph_term_coder
       (list 'variable "hydra.core.Term")
        (lambda (cx) (lambda (g) (lambda (t) (list 'right t))))
        (lambda (cx) (lambda (t) (list 'right t)))))

    ;; Comparison coder

    (define (tc-comparison)
      (make-hydra_graph_term_coder
       (list 'variable "hydra.util.Comparison")
        (lambda (cx)
          (lambda (g)
            (lambda (t)
              (let ((r ((((hydra_extract_core_unit_variant cx) "hydra.util.Comparison") g) t)))
                (if (eq? (car r) 'left) r
                    (let ((variant-name (cadr r)))
                      (cond
                        ((equal? variant-name "lessThan")    (list 'right (list 'less_than '())))
                        ((equal? variant-name "equalTo")     (list 'right (list 'equal_to '())))
                        ((equal? variant-name "greaterThan") (list 'right (list 'greater_than '())))
                        (else (list 'left (make-hydra_context_in_context
                                            (string-append "unknown comparison: " variant-name) cx))))))))))
        (lambda (cx)
          (lambda (c)
            (let ((variant-name (cond
                                  ((and (pair? c) (eq? (car c) 'less_than)) "lessThan")
                                  ((and (pair? c) (eq? (car c) 'equal_to)) "equalTo")
                                  ((and (pair? c) (eq? (car c) 'greater_than)) "greaterThan")
                                  ;; Legacy: symbol or integer
                                  ((eq? c 'lt) "lessThan")
                                  ((and (number? c) (< c 0)) "lessThan")
                                  ((eq? c 'eq) "equalTo")
                                  ((and (number? c) (zero? c)) "equalTo")
                                  ((eq? c 'gt) "greaterThan")
                                  ((and (number? c) (> c 0)) "greaterThan")
                                  (else (error "not a comparison" c)))))
              (list 'right (list 'union (make-hydra_core_injection "hydra.util.Comparison"
                                          (make-hydra_core_field variant-name (list 'unit '()))))))))))

    ;; Function coders

    (define (tc-function dom cod)
      (make-hydra_graph_term_coder
       (list 'function (make-hydra_core_function_type (hydra_graph_term_coder-type dom) (hydra_graph_term_coder-type cod)))
        (lambda (cx) (lambda (g) (lambda (t) (list 'left (make-hydra_context_in_context "cannot encode term to a function" cx)))))
        (lambda (cx) (lambda (v) (list 'left (make-hydra_context_in_context "cannot decode functions to terms" cx))))))

    (define (tc-function-with-reduce reduce-fn dom cod)
      (make-hydra_graph_term_coder
       (list 'function (make-hydra_core_function_type (hydra_graph_term_coder-type dom) (hydra_graph_term_coder-type cod)))
        (lambda (cx)
          (lambda (g)
            (lambda (fun-term)
              (list 'right
                    (lambda (x)
                      (let ((arg-result (((hydra_graph_term_coder-decode dom) cx) x)))
                        (when (eq? (car arg-result) 'left)
                          (error "function_with_reduce: failed to encode argument"))
                        (let* ((arg-term (cadr arg-result))
                               (app-term (list 'application (make-hydra_core_application fun-term arg-term)))
                               (reduce-result (reduce-fn cx g app-term)))
                          (when (eq? (car reduce-result) 'left)
                            (error "function_with_reduce: failed to reduce application"))
                          (let* ((result-term (cadr reduce-result))
                                 (decode-result ((((hydra_graph_term_coder-encode cod) cx) g) result-term)))
                            (when (eq? (car decode-result) 'left)
                              (error "function_with_reduce: failed to decode result"))
                            (cadr decode-result)))))))))
        (lambda (cx) (lambda (v) (list 'left (make-hydra_context_in_context "cannot decode functions to terms" cx))))))

    ;; ============================================================================
    ;; Primitive constructors
    ;; ============================================================================

    (define (prim0 pname value-fn variables output . rest)
      (let ((constraints (if (pair? rest) (car rest) #f)))
        (make-hydra_graph_primitive pname (build-type-scheme variables '() output constraints)
          (lambda (cx)
            (lambda (g)
              (lambda (args)
                (let ((result (((hydra_graph_term_coder-decode output) cx) (value-fn))))
                  (wrap-other-error cx result))))))))

    (define (prim1 pname compute variables input1 output . rest)
      (let ((constraints (if (pair? rest) (car rest) #f)))
      (make-hydra_graph_primitive pname (build-type-scheme variables (list input1) output constraints)
        (lambda (cx)
          (lambda (g)
            (lambda (args)
              (let ((check ((((hydra_extract_core_n_args cx) pname) 1) args)))
                (if (eq? (car check) 'left) check
                    (let ((r1 ((((hydra_graph_term_coder-encode input1) cx) g) (car args))))
                      (if (eq? (car r1) 'left) (wrap-other-error cx r1)
                          (let ((result (((hydra_graph_term_coder-decode output) cx) (compute (cadr r1)))))
                            (wrap-other-error cx result))))))))))))

    (define (prim2 pname compute variables input1 input2 output . rest)
      (let ((constraints (if (pair? rest) (car rest) #f)))
      (make-hydra_graph_primitive pname (build-type-scheme variables (list input1 input2) output constraints)
        (lambda (cx)
          (lambda (g)
            (lambda (args)
              (let ((check ((((hydra_extract_core_n_args cx) pname) 2) args)))
                (if (eq? (car check) 'left) check
                    (let ((r1 ((((hydra_graph_term_coder-encode input1) cx) g) (car args))))
                      (if (eq? (car r1) 'left) (wrap-other-error cx r1)
                          (let ((r2 ((((hydra_graph_term_coder-encode input2) cx) g) (cadr args))))
                            (if (eq? (car r2) 'left) (wrap-other-error cx r2)
                                (let ((result (((hydra_graph_term_coder-decode output) cx) ((compute (cadr r1)) (cadr r2)))))
                                  (wrap-other-error cx result))))))))))))))

    (define (prim3 pname compute variables input1 input2 input3 output . rest)
      (let ((constraints (if (pair? rest) (car rest) #f)))
      (make-hydra_graph_primitive pname (build-type-scheme variables (list input1 input2 input3) output constraints)
        (lambda (cx)
          (lambda (g)
            (lambda (args)
              (let ((check ((((hydra_extract_core_n_args cx) pname) 3) args)))
                (if (eq? (car check) 'left) check
                    (let ((r1 ((((hydra_graph_term_coder-encode input1) cx) g) (car args))))
                      (if (eq? (car r1) 'left) (wrap-other-error cx r1)
                          (let ((r2 ((((hydra_graph_term_coder-encode input2) cx) g) (cadr args))))
                            (if (eq? (car r2) 'left) (wrap-other-error cx r2)
                                (let ((r3 ((((hydra_graph_term_coder-encode input3) cx) g) (caddr args))))
                                  (if (eq? (car r3) 'left) (wrap-other-error cx r3)
                                      (let ((result (((hydra_graph_term_coder-decode output) cx)
                                                      (((compute (cadr r1)) (cadr r2)) (cadr r3)))))
                                        (wrap-other-error cx result))))))))))))))))

))

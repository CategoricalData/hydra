;;; hydra-prims.el --- Hydra Emacs Lisp primitive infrastructure -*- lexical-binding: t -*-
;;; Provides TermCoder constructors and primitive builders (prim0-prim3).
;;; Ported from Common Lisp prims.lisp.

(require 'cl-lib)

;; ============================================================================
;; Type scheme helpers
;; ============================================================================

(defun make-arity-type (n)
  "Build a dummy function type with the given arity."
  (if (<= n 0) (list :unit)
    (list :function (make-hydra_core_function_type (list :unit) (make-arity-type (1- n))))))

(defun make-prim-type-scheme (arity)
  (make-hydra_core_type_scheme nil (make-arity-type arity) nil))

(defun collect-type-vars-ordered (typ)
  "Collect type variable names from a Hydra type in order of first appearance."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (cl-labels ((visit (t_)
                  (when (consp t_)
                    (let ((tag (car t_)))
                      (cond
                       ((eq tag :variable)
                        (let ((v (cadr t_)))
                          (unless (gethash v seen)
                            (puthash v t seen)
                            (push v result))))
                       ((eq tag :function)
                        (let ((ft (cadr t_)))
                          (visit (hydra_core_function_type-domain ft))
                          (visit (hydra_core_function_type-codomain ft))))
                       ((eq tag :list) (visit (cadr t_)))
                       ((eq tag :set) (visit (cadr t_)))
                       ((eq tag :maybe) (visit (cadr t_)))
                       ((eq tag :map)
                        (let ((mt (cadr t_)))
                          (visit (hydra_core_map_type-keys mt))
                          (visit (hydra_core_map_type-values mt))))
                       ((eq tag :pair)
                        (let ((pt (cadr t_)))
                          (visit (hydra_core_pair_type-first pt))
                          (visit (hydra_core_pair_type-second pt))))
                       ((eq tag :either)
                        (let ((et (cadr t_)))
                          (visit (hydra_core_either_type-left et))
                          (visit (hydra_core_either_type-right et)))))))))
      (visit typ)
      (nreverse result))))

(defun build-type-scheme (variables inputs output &optional constraints)
  "Build a TypeScheme from TermCoder types."
  (let* ((out-type (or (hydra_graph_term_coder-type output) (list :unit)))
         (fun-type (cl-reduce (lambda (result-type input-tc)
                                (let ((in-type (or (hydra_graph_term_coder-type input-tc) (list :unit))))
                                  (list :function (make-hydra_core_function_type in-type result-type))))
                              (reverse inputs) :initial-value out-type))
         (detected-vars (collect-type-vars-ordered fun-type))
         (vars (if variables variables detected-vars))
         (constraint-map
          (when constraints
            (funcall hydra_lib_maps_from_list
                     (mapcar (lambda (entry)
                               (list (car entry)
                                     (make-hydra_core_type_variable_metadata
                                      (funcall hydra_lib_sets_from_list (cdr entry)))))
                             constraints)))))
    (make-hydra_core_type_scheme vars fun-type constraint-map)))

;; ============================================================================
;; Error helpers
;; ============================================================================

(defun wrap-other-error (cx result)
  (if (eq (car result) :right) result
    (let* ((ic (cadr result))
           (obj (if (consp ic) (cdr (assq :object ic)) ic))
           (ctx (if (consp ic) (cdr (assq :context ic)) cx)))
      (list :left (make-hydra_context_in_context (list :other (make-hydra_context_in_context obj ctx)) ctx)))))

;; ============================================================================
;; TermCoder constructors — each carries its actual Hydra type
;; ============================================================================

(defun tc-bigfloat ()
  (make-hydra_graph_term_coder (list :literal (list :float (list :bigfloat nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_bigfloat cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :float (list :bigfloat (float v)))))))))

(defun tc-bigint ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :bigint nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_bigint cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :bigint v))))))))

(defun tc-boolean ()
  (make-hydra_graph_term_coder (list :literal (list :boolean nil))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_boolean cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :boolean v)))))))

(defun tc-float32 ()
  (make-hydra_graph_term_coder (list :literal (list :float (list :float32 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_float32 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :float (list :float32 (float v)))))))))

(defun tc-float64 ()
  (make-hydra_graph_term_coder (list :literal (list :float (list :float64 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_float64 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :float (list :float64 (float v)))))))))

(defun tc-int8 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :int8 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int8 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :int8 v))))))))

(defun tc-int16 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :int16 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int16 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :int16 v))))))))

(defun tc-int32 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :int32 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int32 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :int32 v))))))))

(defun tc-int64 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :int64 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int64 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :int64 v))))))))

(defun tc-uint8 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :uint8 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint8 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :uint8 v))))))))

(defun tc-uint16 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :uint16 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint16 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :uint16 v))))))))

(defun tc-uint32 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :uint32 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint32 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :uint32 v))))))))

(defun tc-uint64 ()
  (make-hydra_graph_term_coder (list :literal (list :integer (list :uint64 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint64 cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :integer (list :uint64 v))))))))

(defun tc-string ()
  (make-hydra_graph_term_coder (list :literal (list :string nil))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_string cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :string v)))))))

(defun tc-binary ()
  (make-hydra_graph_term_coder (list :literal (list :binary nil))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_binary cx) g) t_))))
    (lambda (cx) (ignore cx) (lambda (v) (list :right (list :literal (list :binary v)))))))

;; Container TermCoders

(defun tc-list (el-coder)
  (make-hydra_graph_term_coder (list :list (hydra_graph_term_coder-type el-coder))
    (lambda (cx) (lambda (g) (lambda (t_)
      (funcall (funcall (funcall (funcall hydra_extract_core_list_of cx)
                                 (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode el-coder) cx) g) term)))
                        g) t_))))
    (lambda (cx) (lambda (lst)
      (let ((result nil))
        (catch 'fail
          (dolist (item lst)
            (let ((r (funcall (funcall (hydra_graph_term_coder-decode el-coder) cx) item)))
              (if (eq (car r) :left) (throw 'fail r)
                (push (cadr r) result))))
          (list :right (list :list (nreverse result)))))))))

(defun tc-set (el-coder)
  (make-hydra_graph_term_coder (list :set (hydra_graph_term_coder-type el-coder))
    (lambda (cx) (lambda (g) (lambda (t_)
      (funcall (funcall (funcall (funcall hydra_extract_core_set_of cx)
                                 (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode el-coder) cx) g) term)))
                        g) t_))))
    (lambda (cx) (lambda (s)
      (let ((result nil))
        (catch 'fail
          (dolist (item (funcall hydra_lib_sets_to_list s))
            (let ((r (funcall (funcall (hydra_graph_term_coder-decode el-coder) cx) item)))
              (if (eq (car r) :left) (throw 'fail r)
                (push (cadr r) result))))
          (list :right (list :set (funcall hydra_lib_sets_from_list (nreverse result))))))))))

(defun tc-map (key-coder val-coder)
  (make-hydra_graph_term_coder (list :map (make-hydra_core_map_type (hydra_graph_term_coder-type key-coder) (hydra_graph_term_coder-type val-coder)))
    (lambda (cx) (lambda (g) (lambda (t_)
      (funcall (funcall (funcall (funcall (funcall hydra_extract_core_map cx)
                                          (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode key-coder) cx) g) term)))
                                 (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode val-coder) cx) g) term)))
                        g) t_))))
    (lambda (cx) (lambda (m)
      (let ((result nil))
        (catch 'fail
          (dolist (pair (funcall hydra_lib_maps_to_list m))
            (let ((kr (funcall (funcall (hydra_graph_term_coder-decode key-coder) cx) (funcall hydra_lib_pairs_first pair))))
              (if (eq (car kr) :left) (throw 'fail kr)
                (let ((vr (funcall (funcall (hydra_graph_term_coder-decode val-coder) cx) (funcall hydra_lib_pairs_second pair))))
                  (if (eq (car vr) :left) (throw 'fail vr)
                    (push (list (cadr kr) (cadr vr)) result))))))
          (list :right (list :map (funcall hydra_lib_maps_from_list (nreverse result))))))))))

(defun tc-optional (el-coder)
  (make-hydra_graph_term_coder (list :maybe (hydra_graph_term_coder-type el-coder))
    (lambda (cx) (lambda (g) (lambda (t_)
      (funcall (funcall (funcall (funcall hydra_extract_core_maybe_term cx)
                                 (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode el-coder) cx) g) term)))
                        g) t_))))
    (lambda (cx) (lambda (mv)
      (cond
       ((null mv) (list :right (list :maybe nil)))
       ((and (consp mv) (eq (car mv) :nothing)) (list :right (list :maybe nil)))
       ((and (consp mv) (eq (car mv) :just))
        (let ((r (funcall (funcall (hydra_graph_term_coder-decode el-coder) cx) (cadr mv))))
          (if (eq (car r) :left) r (list :right (list :maybe (cadr r))))))
       (t (let ((r (funcall (funcall (hydra_graph_term_coder-decode el-coder) cx) mv)))
            (if (eq (car r) :left) r (list :right (list :maybe (cadr r)))))))))))

(defun tc-either (left-coder right-coder)
  (make-hydra_graph_term_coder (list :either (make-hydra_core_either_type (hydra_graph_term_coder-type left-coder) (hydra_graph_term_coder-type right-coder)))
    (lambda (cx) (lambda (g) (lambda (t_)
      (funcall (funcall (funcall (funcall (funcall hydra_extract_core_either_term cx)
                                          (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode left-coder) cx) g) term)))
                                 (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode right-coder) cx) g) term)))
                        g) t_))))
    (lambda (cx) (lambda (ev)
      (if (eq (car ev) :left)
          (let ((r (funcall (funcall (hydra_graph_term_coder-decode left-coder) cx) (cadr ev))))
            (if (eq (car r) :left) r (list :right (list :either (list :left (cadr r))))))
        (let ((r (funcall (funcall (hydra_graph_term_coder-decode right-coder) cx) (cadr ev))))
          (if (eq (car r) :left) r (list :right (list :either (list :right (cadr r)))))))))))

(defun tc-pair (first-coder second-coder)
  (make-hydra_graph_term_coder (list :pair (make-hydra_core_pair_type (hydra_graph_term_coder-type first-coder) (hydra_graph_term_coder-type second-coder)))
    (lambda (cx) (lambda (g) (lambda (t_)
      (funcall (funcall (funcall (funcall (funcall hydra_extract_core_pair cx)
                                          (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode first-coder) cx) g) term)))
                                 (lambda (term) (funcall (funcall (funcall (hydra_graph_term_coder-encode second-coder) cx) g) term)))
                        g) t_))))
    (lambda (cx) (lambda (p)
      (let ((fr (funcall (funcall (hydra_graph_term_coder-decode first-coder) cx) (funcall hydra_lib_pairs_first p))))
        (if (eq (car fr) :left) fr
          (let ((sr (funcall (funcall (hydra_graph_term_coder-decode second-coder) cx) (funcall hydra_lib_pairs_second p))))
            (if (eq (car sr) :left) sr
              (list :right (list :pair (list (cadr fr) (cadr sr))))))))))))

;; Term/variable passthrough coders

(defun tc-variable (name)
  (make-hydra_graph_term_coder (list :variable name)
    (lambda (_cx) (lambda (_g) (lambda (t_) (list :right t_))))
    (lambda (_cx) (lambda (t_) (list :right t_)))))

(defun tc-term ()
  (make-hydra_graph_term_coder (list :variable "hydra.core.Term")
    (lambda (_cx) (lambda (_g) (lambda (t_) (list :right t_))))
    (lambda (_cx) (lambda (t_) (list :right t_)))))

;; Comparison coder

(defun tc-comparison ()
  (make-hydra_graph_term_coder (list :variable "hydra.util.Comparison")
    (lambda (cx) (lambda (g) (lambda (t_)
      (let ((r (funcall (funcall (funcall (funcall hydra_extract_core_unit_variant cx) "hydra.util.Comparison") g) t_)))
        (if (eq (car r) :left) r
          (let ((variant-name (cadr r)))
            (cond
             ((equal variant-name "lessThan")    (list :right :lt))
             ((equal variant-name "equalTo")     (list :right :eq))
             ((equal variant-name "greaterThan") (list :right :gt))
             (t (list :left (make-hydra_context_in_context (format "unknown comparison: %s" variant-name) cx))))))))))
    (lambda (_cx) (lambda (c)
      (let ((variant-name
             (cond
              ((and (consp c) (eq (car c) :less_than)) "lessThan")
              ((and (consp c) (eq (car c) :equal_to)) "equalTo")
              ((and (consp c) (eq (car c) :greater_than)) "greaterThan")
              ((or (eq c :lt) (and (numberp c) (< c 0))) "lessThan")
              ((or (eq c :eq) (and (numberp c) (zerop c))) "equalTo")
              ((or (eq c :gt) (and (numberp c) (> c 0))) "greaterThan")
              (t (error "not a comparison: %S" c)))))
        (list :right (list :union (make-hydra_core_injection "hydra.util.Comparison"
                                    (make-hydra_core_field variant-name (list :unit))))))))))

;; Function coders

(defun tc-function (dom cod)
  (make-hydra_graph_term_coder (list :function (make-hydra_core_function_type (hydra_graph_term_coder-type dom) (hydra_graph_term_coder-type cod)))
    (lambda (cx) (lambda (_g) (lambda (_t) (list :left (make-hydra_context_in_context "cannot encode term to a function" cx)))))
    (lambda (cx) (lambda (_v) (list :left (make-hydra_context_in_context "cannot decode functions to terms" cx))))))

(defun tc-function-with-reduce (reduce-fn dom cod)
  "TermCoder for function types, using a reducer to bridge term-level to native."
  (make-hydra_graph_term_coder (list :function (make-hydra_core_function_type (hydra_graph_term_coder-type dom) (hydra_graph_term_coder-type cod)))
    (lambda (cx) (lambda (g) (lambda (fun-term)
      (list :right
            (lambda (x)
              (let ((arg-result (funcall (funcall (hydra_graph_term_coder-decode dom) cx) x)))
                (when (eq (car arg-result) :left)
                  (error "function_with_reduce: failed to encode argument"))
                (let* ((arg-term (cadr arg-result))
                       (app-term (list :application (make-hydra_core_application fun-term arg-term)))
                       (reduce-result (funcall reduce-fn cx g app-term)))
                  (when (eq (car reduce-result) :left)
                    (error "function_with_reduce: failed to reduce application"))
                  (let* ((result-term (cadr reduce-result))
                         (decode-result (funcall (funcall (funcall (hydra_graph_term_coder-encode cod) cx) g) result-term)))
                    (when (eq (car decode-result) :left)
                      (error "function_with_reduce: failed to decode result"))
                    (cadr decode-result)))))))))
    (lambda (cx) (lambda (_v) (list :left (make-hydra_context_in_context "cannot decode functions to terms" cx))))))

;; ============================================================================
;; Primitive constructors
;; ============================================================================

(defun prim0 (pname value-fn variables output &optional constraints)
  (make-hydra_graph_primitive pname (build-type-scheme variables nil output constraints)
    (lambda (cx) (lambda (_g) (lambda (_args)
      (let ((result (funcall (funcall (hydra_graph_term_coder-decode output) cx) (funcall value-fn))))
        (wrap-other-error cx result)))))))

(defun prim1 (pname compute variables input1 output &optional constraints)
  (make-hydra_graph_primitive pname (build-type-scheme variables (list input1) output constraints)
    (lambda (cx) (lambda (g) (lambda (args)
      (let ((check (funcall (funcall (funcall (funcall hydra_extract_core_n_args cx) pname) 1) args)))
        (if (eq (car check) :left) check
          (let ((r1 (funcall (funcall (funcall (hydra_graph_term_coder-encode input1) cx) g) (car args))))
            (if (eq (car r1) :left) (wrap-other-error cx r1)
              (let ((result (funcall (funcall (hydra_graph_term_coder-decode output) cx) (funcall compute (cadr r1)))))
                (wrap-other-error cx result)))))))))))

(defun prim2 (pname compute variables input1 input2 output &optional constraints)
  (make-hydra_graph_primitive pname (build-type-scheme variables (list input1 input2) output constraints)
    (lambda (cx) (lambda (g) (lambda (args)
      (let ((check (funcall (funcall (funcall (funcall hydra_extract_core_n_args cx) pname) 2) args)))
        (if (eq (car check) :left) check
          (let ((r1 (funcall (funcall (funcall (hydra_graph_term_coder-encode input1) cx) g) (car args))))
            (if (eq (car r1) :left) (wrap-other-error cx r1)
              (let ((r2 (funcall (funcall (funcall (hydra_graph_term_coder-encode input2) cx) g) (cadr args))))
                (if (eq (car r2) :left) (wrap-other-error cx r2)
                  (let ((result (funcall (funcall (hydra_graph_term_coder-decode output) cx)
                                         (funcall (funcall compute (cadr r1)) (cadr r2)))))
                    (wrap-other-error cx result)))))))))))))

(defun prim3 (pname compute variables input1 input2 input3 output &optional constraints)
  (make-hydra_graph_primitive pname (build-type-scheme variables (list input1 input2 input3) output constraints)
    (lambda (cx) (lambda (g) (lambda (args)
      (let ((check (funcall (funcall (funcall (funcall hydra_extract_core_n_args cx) pname) 3) args)))
        (if (eq (car check) :left) check
          (let ((r1 (funcall (funcall (funcall (hydra_graph_term_coder-encode input1) cx) g) (car args))))
            (if (eq (car r1) :left) (wrap-other-error cx r1)
              (let ((r2 (funcall (funcall (funcall (hydra_graph_term_coder-encode input2) cx) g) (cadr args))))
                (if (eq (car r2) :left) (wrap-other-error cx r2)
                  (let ((r3 (funcall (funcall (funcall (hydra_graph_term_coder-encode input3) cx) g) (nth 2 args))))
                    (if (eq (car r3) :left) (wrap-other-error cx r3)
                      (let ((result (funcall (funcall (hydra_graph_term_coder-decode output) cx)
                                             (funcall (funcall (funcall compute (cadr r1)) (cadr r2)) (cadr r3)))))
                        (wrap-other-error cx result)))))))))))))))

(provide 'hydra-prims)

;;; Hydra Common Lisp primitive infrastructure
;;;
;;; Provides TermCoder constructors and primitive builders (prim0/prim1/prim2/prim3)
;;; used by libraries.lisp to register primitives for the generated reducer.
;;; Direct translation of Clojure hydra/dsl/prims.clj.

(in-package :cl-user)

;; ============================================================================
;; Type scheme helpers
;; ============================================================================

(defun make-arity-type (n)
  "Build a dummy function type with the given arity."
  (if (<= n 0)
      (list :unit)
      (list :function (make-function_type (list :unit) (make-arity-type (1- n))))))

(defun make-prim-type-scheme (arity)
  "Build a TypeScheme with the correct arity for primitive dispatch."
  (make-type_scheme nil (make-arity-type arity) nil))

(defun collect-type-vars-ordered (typ)
  "Collect type variable names from a Hydra type in order of first appearance."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    (labels ((visit (t_)
               (when (consp t_)
                 (let ((tag (first t_)))
                   (cond
                     ((eq tag :variable)
                      (let ((v (second t_)))
                        (unless (gethash v seen)
                          (setf (gethash v seen) t)
                          (push v result))))
                     ((eq tag :function)
                      (let ((ft (second t_)))
                        (visit (function_type-domain ft))
                        (visit (function_type-codomain ft))))
                     ((eq tag :list) (visit (second t_)))
                     ((eq tag :set) (visit (second t_)))
                     ((eq tag :maybe) (visit (second t_)))
                     ((eq tag :map)
                      (let ((mt (second t_)))
                        (visit (map_type-keys mt))
                        (visit (map_type-values mt))))
                     ((eq tag :pair)
                      (let ((pt (second t_)))
                        (visit (pair_type-first pt))
                        (visit (pair_type-second pt))))
                     ((eq tag :either)
                      (let ((et (second t_)))
                        (visit (either_type-left et))
                        (visit (either_type-right et)))))))))
      (visit typ)
      (nreverse result))))

(defun build-type-scheme (variables inputs output &optional constraints)
  "Build a TypeScheme from TermCoder types. Uses the actual types from
   the TermCoders to construct proper function types for inference.
   Auto-detects type variables from the types.
   Optional constraints: alist of (var-name . list-of-class-names)."
  (let* ((out-type (or (term_coder-type output) (list :unit)))
         (fun-type (reduce (lambda (result-type input-tc)
                             (let ((in-type (or (term_coder-type input-tc) (list :unit))))
                               (list :function (make-function_type in-type result-type))))
                           (reverse inputs) :initial-value out-type))
         ;; Auto-detect type variables in order of first appearance.
         ;; Exclude qualified names (containing dots) — those are nominal type references.
         (all-vars (collect-type-vars-ordered fun-type))
         (detected-vars (remove-if (lambda (v) (find #\. v)) all-vars))
         (vars (if variables variables detected-vars))
         ;; Build constraints map
         (constraint-map
          (when constraints
            (funcall hydra_lib_maps_from_list
                     (mapcar (lambda (entry)
                               (list (first entry)
                                     (make-type_variable_metadata
                                      (funcall hydra_lib_sets_from_list (cdr entry)))))
                             constraints)))))
    (make-type_scheme vars fun-type constraint-map)))

;; ============================================================================
;; Error helpers
;; ============================================================================

(defun wrap-other-error (cx result)
  "Wrap Either (InContext OtherError) into Either (InContext Error)."
  (if (eq (first result) :right)
      result
      (let* ((ic (second result))
             (obj (if (consp ic) (cdr (assoc :object ic :test #'eq)) ic))
             (ctx (if (consp ic) (cdr (assoc :context ic :test #'eq)) cx)))
        (list :left (make-in_context (list :other (make-in_context obj ctx)) ctx)))))

;; ============================================================================
;; TermCoder constructors
;; ============================================================================

;; Scalar TermCoders — each carries its actual Hydra type

(defun tc-bigfloat ()
  (make-term_coder (list :literal (list :float (list :bigfloat nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_bigfloat cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :float (list :bigfloat (float v 1.0d0)))))))))

(defun tc-bigint ()
  (make-term_coder (list :literal (list :integer (list :bigint nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_bigint cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :bigint v))))))))

(defun tc-boolean ()
  (make-term_coder (list :literal (list :boolean nil))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_boolean cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :boolean v)))))))

(defun tc-float32 ()
  (make-term_coder (list :literal (list :float (list :float32 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_float32 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :float (list :float32
      (float (float v 1.0f0) 1.0d0)))))))))

(defun tc-float64 ()
  (make-term_coder (list :literal (list :float (list :float64 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_float64 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :float (list :float64 (float v 1.0d0)))))))))

(defun tc-int8 ()
  (make-term_coder (list :literal (list :integer (list :int8 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int8 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :int8 v))))))))

(defun tc-int16 ()
  (make-term_coder (list :literal (list :integer (list :int16 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int16 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :int16 v))))))))

(defun tc-int32 ()
  (make-term_coder (list :literal (list :integer (list :int32 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int32 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :int32 v))))))))

(defun tc-int64 ()
  (make-term_coder (list :literal (list :integer (list :int64 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_int64 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :int64 v))))))))

(defun tc-uint8 ()
  (make-term_coder (list :literal (list :integer (list :uint8 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint8 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :uint8 v))))))))

(defun tc-uint16 ()
  (make-term_coder (list :literal (list :integer (list :uint16 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint16 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :uint16 v))))))))

(defun tc-uint32 ()
  (make-term_coder (list :literal (list :integer (list :uint32 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint32 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :uint32 v))))))))

(defun tc-uint64 ()
  (make-term_coder (list :literal (list :integer (list :uint64 nil)))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_uint64 cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :integer (list :uint64 v))))))))

(defun tc-string ()
  (make-term_coder (list :literal (list :string nil))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_string cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :string v)))))))

(defun tc-binary ()
  (make-term_coder (list :literal (list :binary nil))
    (lambda (cx) (lambda (g) (lambda (t_) (funcall (funcall (funcall hydra_extract_core_binary cx) g) t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (v) (list :right (list :literal (list :binary v)))))))

;; Container TermCoders

(defun tc-list (el-coder)
  (make-term_coder (list :list (term_coder-type el-coder))
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (funcall (funcall (funcall (funcall hydra_extract_core_list_of cx)
                                     (lambda (term) (funcall (funcall (funcall (term_coder-encode el-coder) cx) g) term)))
                            g) t_))))
    (lambda (cx)
      (lambda (lst)
        (block decode
          (let ((result nil))
            (dolist (item lst)
              (let ((r (funcall (funcall (term_coder-decode el-coder) cx) item)))
                (if (eq (first r) :left)
                    (return-from decode r)
                    (push (second r) result))))
            (list :right (list :list (nreverse result)))))))))

(defun tc-set (el-coder)
  (make-term_coder (list :set (term_coder-type el-coder))
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (funcall (funcall (funcall (funcall hydra_extract_core_set_of cx)
                                     (lambda (term) (funcall (funcall (funcall (term_coder-encode el-coder) cx) g) term)))
                            g) t_))))
    (lambda (cx)
      (lambda (s)
        (block decode
          (let ((result nil))
            (dolist (item (funcall hydra_lib_sets_to_list s))
              (let ((r (funcall (funcall (term_coder-decode el-coder) cx) item)))
                (if (eq (first r) :left)
                    (return-from decode r)
                    (push (second r) result))))
            (list :right (list :set (funcall hydra_lib_sets_from_list (nreverse result))))))))))

(defun tc-map (key-coder val-coder)
  (make-term_coder (list :map (make-map_type (term_coder-type key-coder) (term_coder-type val-coder)))
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (funcall (funcall (funcall (funcall (funcall hydra_extract_core_map cx)
                                              (lambda (term) (funcall (funcall (funcall (term_coder-encode key-coder) cx) g) term)))
                                     (lambda (term) (funcall (funcall (funcall (term_coder-encode val-coder) cx) g) term)))
                            g) t_))))
    (lambda (cx)
      (lambda (m)
        (block decode
          (let ((result nil))
            (dolist (pair (funcall hydra_lib_maps_to_list m))
              (let ((kr (funcall (funcall (term_coder-decode key-coder) cx) (funcall hydra_lib_pairs_first pair))))
                (if (eq (first kr) :left)
                    (return-from decode kr)
                    (let ((vr (funcall (funcall (term_coder-decode val-coder) cx) (funcall hydra_lib_pairs_second pair))))
                      (if (eq (first vr) :left)
                          (return-from decode vr)
                          (push (list (second kr) (second vr)) result))))))
            (list :right (list :map (funcall hydra_lib_maps_from_list (nreverse result))))))))))

(defun tc-optional (el-coder)
  (make-term_coder (list :maybe (term_coder-type el-coder))
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (funcall (funcall (funcall (funcall hydra_extract_core_maybe_term cx)
                                     (lambda (term) (funcall (funcall (funcall (term_coder-encode el-coder) cx) g) term)))
                            g) t_))))
    (lambda (cx)
      (lambda (mv)
        (cond
          ((null mv) (list :right (list :maybe nil)))
          ((and (consp mv) (eq (first mv) :nothing)) (list :right (list :maybe nil)))
          ((and (consp mv) (eq (first mv) :just))
           (let ((r (funcall (funcall (term_coder-decode el-coder) cx) (second mv))))
             (if (eq (first r) :left) r (list :right (list :maybe (second r))))))
          (t
           (let ((r (funcall (funcall (term_coder-decode el-coder) cx) mv)))
             (if (eq (first r) :left) r (list :right (list :maybe (second r)))))))))))

(defun tc-either (left-coder right-coder)
  (make-term_coder (list :either (make-either_type (term_coder-type left-coder) (term_coder-type right-coder)))
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (funcall (funcall (funcall (funcall (funcall hydra_extract_core_either_term cx)
                                              (lambda (term) (funcall (funcall (funcall (term_coder-encode left-coder) cx) g) term)))
                                     (lambda (term) (funcall (funcall (funcall (term_coder-encode right-coder) cx) g) term)))
                            g) t_))))
    (lambda (cx)
      (lambda (ev)
        (if (eq (first ev) :left)
            (let ((r (funcall (funcall (term_coder-decode left-coder) cx) (second ev))))
              (if (eq (first r) :left) r (list :right (list :either (list :left (second r))))))
            (let ((r (funcall (funcall (term_coder-decode right-coder) cx) (second ev))))
              (if (eq (first r) :left) r (list :right (list :either (list :right (second r)))))))))))

(defun tc-pair (first-coder second-coder)
  (make-term_coder (list :pair (make-pair_type (term_coder-type first-coder) (term_coder-type second-coder)))
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (funcall (funcall (funcall (funcall (funcall hydra_extract_core_pair cx)
                                              (lambda (term) (funcall (funcall (funcall (term_coder-encode first-coder) cx) g) term)))
                                     (lambda (term) (funcall (funcall (funcall (term_coder-encode second-coder) cx) g) term)))
                            g) t_))))
    (lambda (cx)
      (lambda (p)
        (let ((fr (funcall (funcall (term_coder-decode first-coder) cx) (funcall hydra_lib_pairs_first p))))
          (if (eq (first fr) :left) fr
              (let ((sr (funcall (funcall (term_coder-decode second-coder) cx) (funcall hydra_lib_pairs_second p))))
                (if (eq (first sr) :left) sr
                    (list :right (list :pair (list (second fr) (second sr))))))))))))

;; Term/variable passthrough coders

(defun tc-variable (name)
  (make-term_coder (list :variable name)
    (lambda (cx) (declare (ignore cx)) (lambda (g) (declare (ignore g)) (lambda (t_) (list :right t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (t_) (list :right t_)))))

(defun tc-term ()
  (make-term_coder (list :variable "hydra.core.Term")
    (lambda (cx) (declare (ignore cx)) (lambda (g) (declare (ignore g)) (lambda (t_) (list :right t_))))
    (lambda (cx) (declare (ignore cx)) (lambda (t_) (list :right t_)))))

;; Comparison coder

(defun tc-comparison ()
  (make-term_coder (list :variable "hydra.util.Comparison")
    (lambda (cx)
      (lambda (g)
        (lambda (t_)
          (let ((r (funcall (funcall (funcall (funcall hydra_extract_core_unit_variant cx) "hydra.util.Comparison") g) t_)))
            (if (eq (first r) :left) r
                (let ((variant-name (second r)))
                  (cond
                    ((equal variant-name "lessThan")    (list :right :lt))
                    ((equal variant-name "equalTo")     (list :right :eq))
                    ((equal variant-name "greaterThan") (list :right :gt))
                    (t (list :left (make-in_context (format nil "unknown comparison: ~A" variant-name) cx))))))))))
    (lambda (cx) (declare (ignore cx))
      (lambda (c)
        (let ((variant-name (cond
                              ;; Comparison union: (list :less_than nil), etc.
                              ((and (consp c) (eq (first c) :less_than)) "lessThan")
                              ((and (consp c) (eq (first c) :equal_to)) "equalTo")
                              ((and (consp c) (eq (first c) :greater_than)) "greaterThan")
                              ;; Legacy: keyword or integer
                              ((or (eq c :lt) (and (numberp c) (< c 0))) "lessThan")
                              ((or (eq c :eq) (and (numberp c) (zerop c))) "equalTo")
                              ((or (eq c :gt) (and (numberp c) (> c 0))) "greaterThan")
                              (t (error "not a comparison: ~A" c)))))
          (list :right (list :union (make-injection "hydra.util.Comparison"
                                      (make-field variant-name (list :unit))))))))))

;; Function coders

(defun tc-function (dom cod)
  (make-term_coder (list :function (make-function_type (term_coder-type dom) (term_coder-type cod)))
    (lambda (cx) (lambda (g) (declare (ignore g)) (lambda (t_) (declare (ignore t_)) (list :left (make-in_context "cannot encode term to a function" cx)))))
    (lambda (cx) (lambda (v) (declare (ignore v)) (list :left (make-in_context "cannot decode functions to terms" cx))))))

(defun tc-function-with-reduce (reduce-fn dom cod)
  "TermCoder for function types, using a reducer to bridge term-level to native."
  (make-term_coder (list :function (make-function_type (term_coder-type dom) (term_coder-type cod)))
    (lambda (cx)
      (lambda (g)
        (lambda (fun-term)
          (list :right
                (lambda (x)
                  (let ((arg-result (funcall (funcall (term_coder-decode dom) cx) x)))
                    (when (eq (first arg-result) :left)
                      (error "function_with_reduce: failed to encode argument"))
                    (let* ((arg-term (second arg-result))
                           (app-term (list :application (make-application fun-term arg-term)))
                           (reduce-result (funcall reduce-fn cx g app-term)))
                      (when (eq (first reduce-result) :left)
                        (error "function_with_reduce: failed to reduce application"))
                      (let* ((result-term (second reduce-result))
                             (decode-result (funcall (funcall (funcall (term_coder-encode cod) cx) g) result-term)))
                        (when (eq (first decode-result) :left)
                          (error "function_with_reduce: failed to decode result"))
                        (second decode-result)))))))))
    (lambda (cx) (lambda (v) (declare (ignore v)) (list :left (make-in_context "cannot decode functions to terms" cx))))))

;; ============================================================================
;; Primitive constructors
;; ============================================================================

(defun prim0 (pname value-fn variables output &optional constraints)
  "Create a 0-argument primitive function."
  (make-primitive pname (build-type-scheme variables nil output constraints)
    (lambda (cx)
      (lambda (g)
        (declare (ignore g))
        (lambda (args)
          (declare (ignore args))
          (let ((result (funcall (funcall (term_coder-decode output) cx) (funcall value-fn))))
            (wrap-other-error cx result)))))))

(defun prim1 (pname compute variables input1 output &optional constraints)
  "Create a 1-argument primitive function."
  (make-primitive pname (build-type-scheme variables (list input1) output constraints)
    (lambda (cx)
      (lambda (g)
        (lambda (args)
          (let ((check (funcall (funcall (funcall (funcall hydra_extract_core_n_args cx) pname) 1) args)))
            (if (eq (first check) :left) check
                (let ((r1 (funcall (funcall (funcall (term_coder-encode input1) cx) g) (first args))))
                  (if (eq (first r1) :left) (wrap-other-error cx r1)
                      (let ((result (funcall (funcall (term_coder-decode output) cx) (funcall compute (second r1)))))
                        (wrap-other-error cx result)))))))))))

(defun prim2 (pname compute variables input1 input2 output &optional constraints)
  "Create a 2-argument primitive function."
  (make-primitive pname (build-type-scheme variables (list input1 input2) output constraints)
    (lambda (cx)
      (lambda (g)
        (lambda (args)
          (let ((check (funcall (funcall (funcall (funcall hydra_extract_core_n_args cx) pname) 2) args)))
            (if (eq (first check) :left) check
                (let ((r1 (funcall (funcall (funcall (term_coder-encode input1) cx) g) (first args))))
                  (if (eq (first r1) :left) (wrap-other-error cx r1)
                      (let ((r2 (funcall (funcall (funcall (term_coder-encode input2) cx) g) (second args))))
                        (if (eq (first r2) :left) (wrap-other-error cx r2)
                            (let ((result (funcall (funcall (term_coder-decode output) cx) (funcall (funcall compute (second r1)) (second r2)))))
                              (wrap-other-error cx result)))))))))))))

(defun prim3 (pname compute variables input1 input2 input3 output &optional constraints)
  "Create a 3-argument primitive function."
  (make-primitive pname (build-type-scheme variables (list input1 input2 input3) output constraints)
    (lambda (cx)
      (lambda (g)
        (lambda (args)
          (let ((check (funcall (funcall (funcall (funcall hydra_extract_core_n_args cx) pname) 3) args)))
            (if (eq (first check) :left) check
                (let ((r1 (funcall (funcall (funcall (term_coder-encode input1) cx) g) (first args))))
                  (if (eq (first r1) :left) (wrap-other-error cx r1)
                      (let ((r2 (funcall (funcall (funcall (term_coder-encode input2) cx) g) (second args))))
                        (if (eq (first r2) :left) (wrap-other-error cx r2)
                            (let ((r3 (funcall (funcall (funcall (term_coder-encode input3) cx) g) (third args))))
                              (if (eq (first r3) :left) (wrap-other-error cx r3)
                                  (let ((result (funcall (funcall (term_coder-decode output) cx) (funcall (funcall (funcall compute (second r1)) (second r2)) (second r3)))))
                                    (wrap-other-error cx result)))))))))))))))

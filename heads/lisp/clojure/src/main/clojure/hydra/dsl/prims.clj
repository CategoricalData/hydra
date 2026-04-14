(ns hydra.dsl.prims
  (:require [hydra.core :refer :all]
            [hydra.graph :refer :all]
            [hydra.context :refer :all]
            [hydra.errors :refer :all])
  (:import [hydra.core hydra_core_function_type hydra_core_type_scheme
                       hydra_core_application hydra_core_injection hydra_core_field]
           [hydra.graph hydra_graph_primitive hydra_graph_term_coder]
           [hydra.errors hydra_errors_other_error]))

;; Type scheme helpers -- the reducer uses primitive arity (from TypeScheme)
;; to decide how many args to collect before calling the implementation.

(defn- make-arity-type
  "Build a dummy function type with the given arity.
   Arity 0 → :unit, arity 1 → :unit → :unit, arity 2 → :unit → :unit → :unit, etc."
  [n]
  (if (<= n 0)
    (list :unit)
    (list :function (->hydra_core_function_type (list :unit) (make-arity-type (dec n))))))

(defn- make-type-scheme
  "Build a TypeScheme with the correct arity for primitive dispatch."
  [arity]
  (->hydra_core_type_scheme [] (make-arity-type arity) nil))

(defn- collect-type-vars-ordered
  "Collect type variable names from a Hydra type in order of first appearance."
  [typ]
  (let [seen (atom #{})
        result (atom [])
        visit (fn visit [t]
                (when (and (sequential? t) (seq t))
                  (case (first t)
                    :variable (let [v (second t)]
                                (when-not (@seen v)
                                  (swap! seen conj v)
                                  (swap! result conj v)))
                    :function (let [ft (second t)]
                                (visit (:domain ft))
                                (visit (:codomain ft)))
                    :list (visit (second t))
                    :set (visit (second t))
                    :maybe (visit (second t))
                    :map (let [mt (second t)]
                           (visit (:keys mt))
                           (visit (:values mt)))
                    :pair (let [pt (second t)]
                            (visit (:first pt))
                            (visit (:second pt)))
                    :either (let [et (second t)]
                              (visit (:left et))
                              (visit (:right et)))
                    nil)))]
    (visit typ)
    @result))

(defn- build-type-scheme
  "Build a TypeScheme from TermCoder types. Uses the actual types from
   the TermCoders to construct proper function types for inference.
   Auto-detects type variables from the types.
   Optional constraints: map of var-name -> set of class names."
  ([variables inputs output] (build-type-scheme variables inputs output nil))
  ([variables inputs output constraints]
   (let [out-type (or (:type output) (list :unit))
         fun-type (reduce (fn [result-type input-tc]
                            (let [in-type (or (:type input-tc) (list :unit))]
                              (list :function (->hydra_core_function_type in-type result-type))))
                          out-type
                          (reverse inputs))
         ;; Auto-detect type variables in order of first appearance.
         ;; Exclude qualified names (containing dots) — those are nominal type references, not parameters.
         detected-vars (filterv #(not (.contains ^String % ".")) (collect-type-vars-ordered fun-type))
         vars (if (seq variables) (vec variables) (vec detected-vars))
         ;; Build constraints map: {name -> TypeVariableMetadata}
         constraint-map (when constraints
                          (into {} (map (fn [[k v]]
                                         [k (->hydra_core_type_variable_metadata (set v))])
                                       constraints)))]
     (->hydra_core_type_scheme vars fun-type constraint-map))))

;; Error helpers

(defn- other-err [msg]
  (list :other (->hydra_errors_other_error msg)))

(defn- wrap-other
  "Pass through Either Error results (no wrapping needed after InContext removal)."
  [result]
  result)

;; TermCoder constructors -- each returns a TermCoder record with :type, :encode, :decode

(defn tc-bigfloat []
  (->hydra_graph_term_coder
   (list :literal (list :float (list :bigfloat nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_bigfloat) g) t))
   (fn [cx v] (list :right (list :literal (list :float (list :bigfloat (double v))))))))

(defn tc-bigint []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :bigint nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_bigint) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :bigint v)))))))

(defn tc-boolean []
  (->hydra_graph_term_coder
   (list :literal (list :boolean nil))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_boolean) g) t))
   (fn [cx v] (list :right (list :literal (list :boolean v))))))

(defn tc-float32 []
  (->hydra_graph_term_coder
   (list :literal (list :float (list :float32 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_float32) g) t))
   (fn [cx v] (list :right (list :literal (list :float (list :float32 v)))))))

(defn tc-float64 []
  (->hydra_graph_term_coder
   (list :literal (list :float (list :float64 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_float64) g) t))
   (fn [cx v] (list :right (list :literal (list :float (list :float64 (double v))))))))

(defn tc-int8 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :int8 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_int8) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :int8 v)))))))

(defn tc-int16 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :int16 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_int16) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :int16 v)))))))

(defn tc-int32 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :int32 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_int32) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :int32 v)))))))

(defn tc-int64 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :int64 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_int64) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :int64 v)))))))

(defn tc-uint8 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :uint8 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_uint8) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :uint8 v)))))))

(defn tc-uint16 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :uint16 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_uint16) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :uint16 v)))))))

(defn tc-uint32 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :uint32 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_uint32) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :uint32 v)))))))

(defn tc-uint64 []
  (->hydra_graph_term_coder
   (list :literal (list :integer (list :uint64 nil)))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_uint64) g) t))
   (fn [cx v] (list :right (list :literal (list :integer (list :uint64 v)))))))

(defn tc-string []
  (->hydra_graph_term_coder
   (list :literal (list :string nil))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_string) g) t))
   (fn [cx v] (list :right (list :literal (list :string v))))))

(defn tc-binary []
  (->hydra_graph_term_coder
   (list :literal (list :binary nil))
   (fn [cx g t] ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_binary) g) t))
   (fn [cx v] (list :right (list :literal (list :binary v))))))

;; Container TermCoders

(defn tc-list [el-coder]
  (->hydra_graph_term_coder
   (list :list (:type el-coder))
   (fn [cx g t]
     (((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_list_of) (fn [term] ((.encode el-coder) cx g term))) g) t))
   (fn [cx lst]
     (loop [items lst result []]
       (if (empty? items)
         (list :right (list :list (apply list (map #(second %) result))))
         (let [r ((.decode el-coder) cx (first items))]
           (if (= (first r) :left)
             r
             (recur (rest items) (conj result r)))))))))

(defn tc-set [el-coder]
  (->hydra_graph_term_coder
   (list :set (:type el-coder))
   (fn [cx g t]
     (((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_set_of) (fn [term] ((.encode el-coder) cx g term))) g) t))
   (fn [cx s]
     (loop [items (seq s) result []]
       (if (empty? items)
         (list :right (list :set (into #{} (map #(second %) result))))
         (let [r ((.decode el-coder) cx (first items))]
           (if (= (first r) :left)
             r
             (recur (rest items) (conj result r)))))))))

(defn tc-map [key-coder val-coder]
  (->hydra_graph_term_coder
   (list :map (->hydra_core_map_type (:type key-coder) (:type val-coder)))
   (fn [cx g t]
     ((((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_map)
         (fn [term] ((.encode key-coder) cx g term)))
        (fn [term] ((.encode val-coder) cx g term)))
       g) t))
   (fn [cx m]
     (loop [pairs (seq m) result {}]
       (if (empty? pairs)
         (list :right (list :map result))
         (let [[k v] (first pairs)
               kr ((.decode key-coder) cx k)]
           (if (= (first kr) :left)
             kr
             (let [vr ((.decode val-coder) cx v)]
               (if (= (first vr) :left)
                 vr
                 (recur (rest pairs) (assoc result (second kr) (second vr))))))))))))

(defn tc-optional [el-coder]
  (->hydra_graph_term_coder
   (list :maybe (:type el-coder))
   (fn [cx g t]
     (((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_maybe_term)
        (fn [term] ((.encode el-coder) cx g term)))
       g) t))
   (fn [cx mv]
     (cond
       ;; nil or empty list → Nothing
       (or (nil? mv) (and (sequential? mv) (empty? mv))) (list :right (list :maybe nil))
       ;; (:nothing) → Nothing
       (and (sequential? mv) (= (first mv) :nothing)) (list :right (list :maybe nil))
       ;; (:just val) → Just val
       (and (sequential? mv) (= (first mv) :just))
       (let [r ((.decode el-coder) cx (second mv))]
         (if (= (first r) :left) r (list :right (list :maybe (second r)))))
       ;; bare value → Just val
       :else
       (let [r ((.decode el-coder) cx mv)]
         (if (= (first r) :left) r (list :right (list :maybe (second r)))))))))

(defn tc-either [left-coder right-coder]
  (->hydra_graph_term_coder
   (list :either (->hydra_core_either_type (:type left-coder) (:type right-coder)))
   (fn [cx g t]
     ((((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_either_term)
         (fn [term] ((.encode left-coder) cx g term)))
        (fn [term] ((.encode right-coder) cx g term)))
       g) t))
   (fn [cx ev]
     (if (= (first ev) :left)
       (let [r ((.decode left-coder) cx (second ev))]
         (if (= (first r) :left) r
             (list :right (list :either (list :left (second r))))))
       (let [r ((.decode right-coder) cx (second ev))]
         (if (= (first r) :left) r
             (list :right (list :either (list :right (second r))))))))))

(defn tc-pair [first-coder second-coder]
  (->hydra_graph_term_coder
   (list :pair (->hydra_core_pair_type (:type first-coder) (:type second-coder)))
   (fn [cx g t]
     ((((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_pair)
         (fn [term] ((.encode first-coder) cx g term)))
        (fn [term] ((.encode second-coder) cx g term)))
       g) t))
   (fn [cx p]
     (let [fr ((.decode first-coder) cx (first p))]
       (if (= (first fr) :left) fr
           (let [sr ((.decode second-coder) cx (second p))]
             (if (= (first sr) :left) sr
                 (list :right (list :pair (list (second fr) (second sr)))))))))))

;; Term/variable passthrough coders

(defn tc-variable
  "TermCoder for type variables -- just passes terms through unchanged."
  [_name]
  (->hydra_graph_term_coder
   (list :variable _name)
   (fn [cx g t] (list :right t))
   (fn [cx t] (list :right t))))

(defn tc-term []
  (->hydra_graph_term_coder
   (list :variable "hydra.core.Term")
   (fn [cx g t] (list :right t))
   (fn [cx t] (list :right t))))

;; Comparison coder
(defn tc-comparison []
  (->hydra_graph_term_coder
   (list :variable "hydra.util.Comparison")
   (fn [cx g t]
     ;; Extract a comparison union variant
     (let [r ((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_unit_variant) "hydra.util.Comparison") g t)]
       (if (= (first r) :left)
         r
         (let [variant-name (second r)]
           (cond
             (= variant-name "lessThan")    (list :right :lt)
             (= variant-name "equalTo")     (list :right :eq)
             (= variant-name "greaterThan") (list :right :gt)
             :else (list :left (other-err (str "unknown comparison: " variant-name))))))))
   (fn [cx c]
     (let [variant-name (cond
                          ;; Comparison union: (list :less_than nil), etc.
                          (and (sequential? c) (= (first c) :less_than)) "lessThan"
                          (and (sequential? c) (= (first c) :equal_to)) "equalTo"
                          (and (sequential? c) (= (first c) :greater_than)) "greaterThan"
                          ;; Legacy: keyword or integer
                          (or (= c :lt) (and (number? c) (neg? c))) "lessThan"
                          (or (= c :eq) (and (number? c) (zero? c))) "equalTo"
                          (or (= c :gt) (and (number? c) (pos? c))) "greaterThan"
                          :else (throw (IllegalArgumentException. (str "not a comparison: " c))))]
       (list :right (list :inject (->hydra_core_injection "hydra.util.Comparison"
                                   (->hydra_core_field variant-name (list :unit)))))))))

;; Function coders

(defn tc-function
  "TermCoder for function types -- not actually encodable/decodable."
  [_dom _cod]
  (->hydra_graph_term_coder
   (list :function (->hydra_core_function_type (:type _dom) (:type _cod)))
   (fn [cx g t] (list :left (other-err "cannot encode term to a function")))
   (fn [cx v] (list :left (other-err "cannot decode functions to terms")))))

(defn tc-function-with-reduce
  "TermCoder for function types, using a reducer to bridge term-level functions
   to native functions. The reduce parameter should be (fn [cx g term] -> Either)."
  [reduce-fn dom cod]
  (->hydra_graph_term_coder
   (list :function (->hydra_core_function_type (:type dom) (:type cod)))
   (fn [cx g fun-term]
     (list :right
           (fn [x]
             (let [arg-result ((.decode dom) cx x)]
               (when (= (first arg-result) :left)
                 (throw (RuntimeException. "function_with_reduce: failed to encode argument")))
               (let [arg-term (second arg-result)
                     app-term (list :application (->hydra_core_application fun-term arg-term))
                     reduce-result (reduce-fn cx g app-term)]
                 (when (= (first reduce-result) :left)
                   (throw (RuntimeException. "function_with_reduce: failed to reduce application")))
                 (let [result-term (second reduce-result)
                       decode-result ((.encode cod) cx g result-term)]
                   (when (= (first decode-result) :left)
                     (throw (RuntimeException. "function_with_reduce: failed to decode result")))
                   (second decode-result)))))))
   (fn [cx v] (list :left (other-err "cannot decode functions to terms")))))

;; Primitive constructors

(defn prim0
  "Create a 0-argument primitive function."
  ([pname value-fn _variables output]
   (prim0 pname value-fn _variables output nil))
  ([pname value-fn _variables output constraints]
   (->hydra_graph_primitive
    pname
    (build-type-scheme _variables [] output constraints)
    (fn [cx] (fn [g] (fn [args]
      (let [result ((.decode output) cx (value-fn))]
        (wrap-other result))))))))

(defn prim1
  "Create a 1-argument primitive function."
  ([pname compute _variables input1 output]
   (prim1 pname compute _variables input1 output nil))
  ([pname compute _variables input1 output constraints]
   (->hydra_graph_primitive
    pname
    (build-type-scheme _variables [input1] output constraints)
    (fn [cx] (fn [g] (fn [args]
      (let [check (((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_n_args) pname) 1) args)]
        (if (= (first check) :left)
          check
          (let [r1 ((.encode input1) cx g (first args))]
            (if (= (first r1) :left)
              (wrap-other r1)
              (let [result ((.decode output) cx (compute (second r1)))]
                (wrap-other result))))))))))))

(defn prim2
  "Create a 2-argument primitive function."
  ([pname compute _variables input1 input2 output]
   (prim2 pname compute _variables input1 input2 output nil))
  ([pname compute _variables input1 input2 output constraints]
  (->hydra_graph_primitive
   pname
   (build-type-scheme _variables [input1 input2] output constraints)
   (fn [cx] (fn [g] (fn [args]
     (let [check (((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_n_args) pname) 2) args)]
       (if (= (first check) :left)
         check
         (let [r1 ((.encode input1) cx g (first args))]
           (if (= (first r1) :left)
             (wrap-other r1)
             (let [r2 ((.encode input2) cx g (second args))]
               (if (= (first r2) :left)
                 (wrap-other r2)
                 (let [result ((.decode output) cx (compute (second r1) (second r2)))]
                   (wrap-other result))))))))))))))

(defn prim3
  "Create a 3-argument primitive function."
  ([pname compute _variables input1 input2 input3 output]
   (prim3 pname compute _variables input1 input2 input3 output nil))
  ([pname compute _variables input1 input2 input3 output constraints]
  (->hydra_graph_primitive
   pname
   (build-type-scheme _variables [input1 input2 input3] output constraints)
   (fn [cx] (fn [g] (fn [args]
     (let [check (((@(ns-resolve 'hydra.extract.core 'hydra_extract_core_n_args) pname) 3) args)]
       (if (= (first check) :left)
         check
         (let [r1 ((.encode input1) cx g (first args))]
           (if (= (first r1) :left)
             (wrap-other r1)
             (let [r2 ((.encode input2) cx g (second args))]
               (if (= (first r2) :left)
                 (wrap-other r2)
                 (let [r3 ((.encode input3) cx g (nth args 2))]
                   (if (= (first r3) :left)
                     (wrap-other r3)
                     (let [result ((.decode output) cx (compute (second r1) (second r2) (second r3)))]
                       (wrap-other result))))))))))))))))

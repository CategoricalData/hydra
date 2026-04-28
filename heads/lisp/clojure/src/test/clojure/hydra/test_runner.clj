(ns hydra.test-runner
  (:require [hydra.lib.libraries :as libraries]
            [hydra.lib.preload :as preload]
            [hydra.core :refer :all]
            [hydra.context :refer :all]
            [hydra.graph :refer :all]
            [hydra.typing :refer :all])
  (:import [hydra.context hydra_context_in_context]
           [hydra.core hydra_core_wrapped_term hydra_core_injection hydra_core_field]
           [hydra.graph hydra_graph_primitive hydra_graph_term_coder]))

;; ==========================================================================
;; Annotation primitives (native Clojure, operating on meta-encoded terms)
;; ==========================================================================

(defn- is-meta-annotated? [t]
  "Check if term is a meta-encoded annotated term:
   (:union {:type_name \"hydra.core.Term\" :field {:name \"annotated\" :term ...}})"
  (and (sequential? t) (= (first t) :inject)
       (let [m (second t)]
         (and (= (:type_name m) "hydra.core.Term")
              (= (:name (:field m)) "annotated")))))

(defn- meta-annotated-record [t]
  "Extract the AnnotatedTerm record from a meta-encoded annotated term.
   The record is inside: (:union {:field {:term RECORD}})"
  (:term (:field (second t))))

(defn- deannotate-term [t]
  (if (is-meta-annotated? t)
    (let [rec (meta-annotated-record t)]
      ;; rec is (:record {:type_name ... :fields [...]})
      ;; body is the first field's term
      (if (and (sequential? rec) (= (first rec) :record))
        (let [fields (:fields (second rec))
              body-field (first (filter #(= (:name %) "body") fields))]
          (recur (:term body-field)))
        t))
    t))

(defn- map-term-to-clj-map [m]
  "Convert the contents of a (:map ...) term to a Clojure map.
   The map contents may be a Clojure map already, or a list of (key val) pairs."
  (cond
    (map? m) m
    (sequential? m) (into {} (map (fn [pair]
                                    (if (sequential? pair)
                                      [(first pair) (second pair)]
                                      pair)) m))
    :else {}))

(defn- term-annotation-internal [term]
  "Extract annotation map from a meta-encoded annotated term.
   Returns the annotation map (map of key->value terms)."
  (loop [t term pairs ()]
    (if (is-meta-annotated? t)
      (let [rec (meta-annotated-record t)]
        (if (and (sequential? rec) (= (first rec) :record))
          (let [fields (:fields (second rec))
                body-field (first (filter #(= (:name %) "body") fields))
                ann-field (first (filter #(= (:name %) "annotation") fields))
                ann-term (:term ann-field)]
            ;; ann-term is (:map contents)
            (if (and (sequential? ann-term) (= (first ann-term) :map))
              (recur (:term body-field)
                     (cons (seq (map-term-to-clj-map (second ann-term))) pairs))
              (recur (:term body-field) pairs)))
          (into {} (apply concat pairs))))
      (into {} (apply concat pairs)))))

(defn- maybe-nothing? [val]
  (or (nil? val)
      (and (sequential? val) (empty? val))
      (and (sequential? val) (= (first val) :nothing))
      (and (sequential? val) (= (first val) :maybe)
           (or (< (count val) 2)
               (nil? (second val))
               (and (sequential? (second val)) (= (first (second val)) :nothing))))))

(defn- maybe-value [val]
  (cond
    (and (sequential? val) (= (first val) :just)) (second val)
    (and (sequential? val) (= (first val) :maybe))
      (let [body (second val)]
        (cond
          (and (sequential? body) (= (first body) :just)) (second body)
          :else body))
    :else val))

(defn- deep-equal?
  "Structural equality that works across defrecord classloader boundaries.
   Falls back to field-by-field comparison for records with matching keys."
  [a b]
  (cond
    (= a b) true
    (and (map? a) (map? b) (= (set (keys a)) (set (keys b))))
    (every? (fn [k] (deep-equal? (get a k) (get b k))) (keys a))
    (and (sequential? a) (sequential? b) (= (count a) (count b)))
    (every? identity (map deep-equal? a b))
    :else false))

(defn- set-annotation [key val m]
  ;; val is Maybe Term: (:maybe term) for Just, nil/(:nothing) for Nothing
  (if (maybe-nothing? val)
    ;; Remove key using structural comparison
    (into {} (remove (fn [[k _]] (deep-equal? k key)) m))
    ;; Add/replace key; first remove any structurally equal key
    (assoc (into {} (remove (fn [[k _]] (deep-equal? k key)) m))
           key (maybe-value val))))

(defn- make-meta-annotated [body anns-map]
  "Build a meta-encoded annotated term:
   (:union {:type_name \"hydra.core.Term\" :field {:name \"annotated\" :term
     (:record {:type_name \"hydra.core.AnnotatedTerm\" :fields [...]})}})"
  (list :union {:type_name "hydra.core.Term"
                :field {:name "annotated"
                        :term (list :record {:type_name "hydra.core.AnnotatedTerm"
                                             :fields [{:name "body" :term body}
                                                      {:name "annotation" :term (list :map anns-map)}]})}}))

(defn- make-type-scheme [arity]
  (let [make-type (fn make-type [n]
                    (if (<= n 0) (list :unit)
                        (list :function {:domain (list :unit) :codomain (make-type (dec n))})))]
    {:variables [] :type (make-type arity) :constraints []}))

(defn- make-annotation-primitive
  "Create a Primitive that takes raw term arguments and returns a term result.
   The implementation receives args as a list of terms and must return Either."
  [name arity impl-fn]
  (let [ic-err (fn [cx msg] (->hydra_context_in_context msg cx))]
    (->hydra_graph_primitive name (make-type-scheme arity)
      (fn [cx] (fn [g] (fn [args]
        (try
          (let [result (impl-fn cx g args)]
            (if (= (first result) :left)
              ;; Wrap error in proper InContext
              (let [err (second result)]
                (list :left (->hydra_context_in_context (list :other (->hydra_context_in_context err cx)) cx)))
              result))
          (catch Throwable e
            (list :left (->hydra_context_in_context (list :other (->hydra_context_in_context (.getMessage e) cx)) cx))))))))))

;; setTermAnnotation :: Name -> Maybe Term -> Term -> Term
(defn- prim-set-term-annotation [_cx _g args]
  (let [key (first args)     ;; Name (wrapped)
        val (second args)    ;; Maybe Term
        term (nth args 2)    ;; Term
        ;; The key is the whole wrap term — use it as the map key directly
        stripped (deannotate-term term)
        anns (set-annotation key val (term-annotation-internal term))]
    (list :right
      (if (empty? anns)
        stripped
        (make-meta-annotated stripped anns)))))

;; getTermAnnotation :: Name -> Term -> Maybe Term
(defn- prim-get-term-annotation [_cx _g args]
  (let [key (first args)    ;; Name (wrapped) — use as map key directly
        term (second args)  ;; Term (meta-encoded)
        anns (term-annotation-internal term)
        result (get anns key)]
    (list :right
      (if result
        (list :maybe result)
        (list :maybe nil)))))

;; setTermDescription :: Maybe String -> Term -> Term
;; d is Maybe String. The reducer extracts it to (:maybe (:literal (:string S))) for Just,
;; or (:maybe (:nothing)) for Nothing.
;; We need to convert the string to a meta-encoded literal string Term for the annotation value.
(defn- prim-set-term-description [_cx _g args]
  (let [d (first args)
        term (second args)
        term-val
        (when-not (maybe-nothing? d)
          (let [inner (maybe-value d)
                s (if (and (sequential? inner) (= (first inner) :literal)
                           (sequential? (second inner)) (= (first (second inner)) :string))
                    (second (second inner))
                    (str inner))]
            (list :inject (->hydra_core_injection "hydra.core.Term"
                           (->hydra_core_field "literal"
                             (list :inject (->hydra_core_injection "hydra.core.Literal"
                                            (->hydra_core_field "string"
                                              (list :literal (list :string s))))))))))
        desc-key (list :wrap (->hydra_core_wrapped_term "hydra.core.Name"
                               (list :literal (list :string "description"))))
        maybe-val (if term-val (list :maybe term-val) (list :maybe (list :nothing)))]
    (prim-set-term-annotation _cx _g [desc-key maybe-val term])))

;; getTermDescription :: Context -> Graph -> Term -> Either Error (Maybe String)
(defn- prim-get-term-description [_cx _g args]
  (let [;; cx = first, g = second, term = third
        term (nth args 2)
        ;; Peel type lambdas/applications (meta-encoded)
        peeled (loop [t term]
                 (cond
                   (and (sequential? t) (= (first t) :inject)
                        (= (:type_name (second t)) "hydra.core.Term")
                        (= (:name (:field (second t))) "typeLambda"))
                   (let [rec (:term (:field (second t)))]
                     (if (and (sequential? rec) (= (first rec) :record))
                       (let [fields (:fields (second rec))
                             body-field (first (filter #(= (:name %) "body") fields))]
                         (recur (:term body-field)))
                       t))
                   (and (sequential? t) (= (first t) :inject)
                        (= (:type_name (second t)) "hydra.core.Term")
                        (= (:name (:field (second t))) "typeApplication"))
                   (let [rec (:term (:field (second t)))]
                     (if (and (sequential? rec) (= (first rec) :record))
                       (let [fields (:fields (second rec))
                             body-field (first (filter #(= (:name %) "body") fields))]
                         (recur (:term body-field)))
                       t))
                   :else t))
        anns (term-annotation-internal peeled)
        ;; Look up the "description" key by structural comparison.
        ;; Keys are (:wrap WrappedTerm) where WrappedTerm may come from
        ;; different classloaders, so use field-level comparison.
        is-desc-key? (fn [k]
                       (and (sequential? k) (= (first k) :wrap)
                            (let [wt (second k)]
                              (and (= (:type_name wt) "hydra.core.Name")
                                   (let [b (:body wt)]
                                     (and (sequential? b) (= (first b) :literal)
                                          (sequential? (second b)) (= (first (second b)) :string)
                                          (= (second (second b)) "description")))))))
        desc-term (some (fn [[k v]] (when (is-desc-key? k) v)) anns)]
    (if desc-term
      ;; desc-term is a meta-encoded Term; extract string from it
      ;; It should be (:union {:type_name "hydra.core.Term" :field {:name "literal" :term
      ;;   (:union {:type_name "hydra.core.Literal" :field {:name "string" :term (:literal (:string S))}})}})
      (let [extract-str (fn [t]
                          (when (and (sequential? t) (= (first t) :inject)
                                     (= (:name (:field (second t))) "literal"))
                            (let [lit (:term (:field (second t)))]
                              (when (and (sequential? lit) (= (first lit) :union)
                                         (= (:name (:field (second lit))) "string"))
                                (let [inner (:term (:field (second lit)))]
                                  (when (and (sequential? inner) (= (first inner) :literal)
                                             (sequential? (second inner)) (= (first (second inner)) :string))
                                    (second (second inner))))))))]
        (if-let [s (extract-str desc-term)]
          (list :right (list :either (list :right (list :maybe (list :literal (list :string s))))))
          (list :right (list :either (list :right (list :maybe nil))))))
      (list :right (list :either (list :right (list :maybe nil)))))))

;; ==========================================================================
;; Graph construction
;; ==========================================================================

(defn build-test-graph []
  (let [std-prims (libraries/standard-library)

        ;; Create annotation primitives
        ann-prims
        {"hydra.annotations.setTermAnnotation"
         (make-annotation-primitive "hydra.annotations.setTermAnnotation" 3
           prim-set-term-annotation)
         "hydra.annotations.getTermAnnotation"
         (make-annotation-primitive "hydra.annotations.getTermAnnotation" 2
           prim-get-term-annotation)
         "hydra.annotations.setTermDescription"
         (make-annotation-primitive "hydra.annotations.setTermDescription" 2
           prim-set-term-description)
         "hydra.annotations.getTermDescription"
         (make-annotation-primitive "hydra.annotations.getTermDescription" 3
           prim-get-term-description)}

        all-prims (merge std-prims ann-prims)

        ;; Build schema types from test type definitions
        ;; TestGraph.testTypes() returns a map of Name -> Type
        ;; Schema types and test terms will be added post-construction
        schema-types {}
        test-terms {}

        bound-terms
        (merge
          ;; Primitives are resolved via graphPrimitives, not boundTerms.
          ;; Constants and monads
          {"hydra.monads.emptyContext" (list :unit)
           "hydra.lexical.emptyGraph"  (list :unit)}
          ;; Test terms
          test-terms)]
    {:bound_terms bound-terms
     :bound_types {}
     :class_constraints {}
     :lambda_variables #{}
     :metadata {}
     :primitives all-prims
     :schema_types schema-types
     :type_variables #{}}))

(def test-graph (atom nil))

(defn ensure-test-graph! []
  "Build and enhance the test graph with schema types (called after all namespaces loaded)."
  (when (nil? @test-graph)
    (let [base (build-test-graph)]
      ;; Try to enhance with schema types from test type definitions
      (try
        (require 'hydra.test.testGraph)
        (require 'hydra.json.bootstrap)
        (let [;; Load kernel types from bootstrap (like Python's _load_bootstrap_type_schemes)
              bootstrap-var (ns-resolve 'hydra.json.bootstrap 'hydra_json_bootstrap_types_by_name)
              bootstrap-types (if (and bootstrap-var (bound? bootstrap-var)) @bootstrap-var {})
              ;; Load test types
              test-types-var (ns-resolve 'hydra.test.testGraph 'hydra_test_test_graph_test_types)
              test-types (if (and test-types-var (bound? test-types-var)) @test-types-var ())
              ;; Convert types to TypeSchemes using f_type_to_type_scheme (handles foralls)
              type-to-ts-var (ns-resolve 'hydra.rewriting 'hydra_scoping_f_type_to_type_scheme)
              type-to-ts (if (and type-to-ts-var (bound? type-to-ts-var)) @type-to-ts-var nil)
              ;; Build schema types from bootstrap + test types
              ;; bootstrap-types is a Clojure map {string -> type}
              ;; test-types is an alist ((name type) ...)
              kernel-schemas (if type-to-ts
                               (into {} (map (fn [[name typ]] [name (type-to-ts typ)]) bootstrap-types))
                               {})
              test-schemas (if (and (seq test-types) type-to-ts)
                             (into {} (map (fn [entry] [(first entry) (type-to-ts (second entry))]) test-types))
                             {})
              schema-types (merge kernel-schemas test-schemas)
              ;; test-terms is also an alist
              test-terms-var (ns-resolve 'hydra.test.testGraph 'hydra_test_test_graph_test_terms)
              test-terms-alist (if (and test-terms-var (bound? test-terms-var)) @test-terms-var ())
              test-terms (into {} (map (fn [entry] [(first entry) (second entry)]) test-terms-alist))
              enhanced (-> base
                           (update :schema_types merge schema-types)
                           (update :bound_terms merge test-terms))]
          (reset! test-graph enhanced))
        (catch Throwable e
          (println "WARNING: Could not load test types:" (.getMessage e))
          (reset! test-graph base))))))

(defn- empty-context []
  {:functions () :annotations () :variable_types {}})

(defn- empty-graph []
  "Build an empty graph with standard primitives (for hoisting tests)."
  (let [std-prims (libraries/standard-library)]
    {:bound_terms {}
     :bound_types {}
     :class_constraints {}
     :lambda_variables #{}
     :metadata {}
     :primitives std-prims
     :schema_types {}
     :type_variables #{}}))

;; ==========================================================================
;; Term comparison (following Java TestSuiteRunner pattern)
;; ==========================================================================

(defn- show-term [t]
  (let [show-fn (or (resolve 'hydra_show_core_term)
                    (ns-resolve 'hydra.show.core 'hydra_show_core_term))]
    (when show-fn ((deref show-fn) t))))

(defn- show-type [t]
  (let [show-fn (or (resolve 'hydra_show_core_type)
                    (ns-resolve 'hydra.show.core 'hydra_show_core_type))]
    (when show-fn ((deref show-fn) t))))

(defn- show-type-scheme [ts]
  (let [show-fn (or (resolve 'hydra_show_core_type_scheme)
                    (ns-resolve 'hydra.show.core 'hydra_show_core_type_scheme))]
    (when show-fn ((deref show-fn) ts))))

(defn- show-let [l]
  (let [show-fn (or (resolve 'hydra_show_core_let)
                    (ns-resolve 'hydra.show.core 'hydra_show_core_let))]
    (when show-fn ((deref show-fn) l))))

(defn- normalize-show [s]
  ;; Normalize set element ordering in show output: {a, b, c} -> sorted
  (clojure.string/replace s #"\{([^{}]*)\}"
    (fn [[_ contents]]
      (let [elems (clojure.string/split contents #",\s*")]
        (str "{" (clojure.string/join ", " (sort elems)) "}")))))

(defn- float-close? [actual expected]
  ;; Check if two float64 show strings are close enough
  (let [a-match (re-matches #"(-?[\d.]+(?:E\d+)?):float64" actual)
        e-match (re-matches #"(-?[\d.]+(?:E\d+)?):float64" expected)]
    (when (and a-match e-match)
      (try
        (let [av (Double/parseDouble (second a-match))
              ev (Double/parseDouble (second e-match))]
          (or (== av ev)
              (< (Math/abs (- av ev)) (* 2 (Math/ulp ev)))))
        (catch Exception _ false)))))

(defn- terms-match? [actual expected]
  (or (= actual expected)
      ;; Fallback: compare via show with normalization (like Java TestSuiteRunner)
      (try
        (let [a-str (show-term actual)
              e-str (show-term expected)]
          (or (= a-str e-str)
              (= (normalize-show a-str) (normalize-show e-str))
              (float-close? a-str e-str)))
        (catch Throwable _ false))))

;; ==========================================================================
;; Alpha-equivalence for type variables (for type_checking tests)
;; ==========================================================================

(defn- normalize-type-var-names
  "Normalize type variable names for alpha-equivalence comparison.
   Extracts the lambda-binder chain and renames all type variables to canonical names
   based on their first occurrence order in the body."
  [s]
  (if (nil? s) s
    (let [;; Extract binder chain
          extract-binders (fn [s]
                            (loop [remaining s binders []]
                              (if (.startsWith remaining "\u039b") ;; capital lambda
                                (let [dot-idx (.indexOf remaining ".")]
                                  (if (< dot-idx 0)
                                    [binders remaining]
                                    (let [binder (.substring remaining 1 dot-idx)]
                                      (recur (.substring remaining (inc dot-idx))
                                             (conj binders binder)))))
                                [binders remaining])))
          [binders body] (extract-binders s)]
      (if (empty? binders)
        s
        (let [binder-set (set binders)
              ;; Find vars by first occurrence in body
              var-pattern (re-pattern "\\b(t\\d+)\\b")
              ordered (loop [matcher (re-matcher var-pattern body) result []]
                        (if (.find matcher)
                          (let [v (.group matcher 1)]
                            (if (and (binder-set v) (not (some #{v} result)))
                              (recur matcher (conj result v))
                              (recur matcher result)))
                          result))
              ;; Add binders not found in body
              all-ordered (into ordered (filter #(not (some #{%} ordered)) binders))
              ;; Build renaming: first use temp names, then final names
              temp-map (into {} (map-indexed (fn [i v] [v (str "tv" i)]) all-ordered))
              ;; Apply temp renaming to body
              temp-body (reduce (fn [s [from to]]
                                  (clojure.string/replace s (re-pattern (str "\\b" (java.util.regex.Pattern/quote from) "\\b")) to))
                                body temp-map)
              ;; Apply final renaming
              final-body (reduce (fn [s i]
                                   (clojure.string/replace s (re-pattern (str "\\btv" i "\\b")) (str "t" i)))
                                 temp-body (range (count all-ordered)))
              ;; Reconstruct binder chain
              prefix (apply str (map-indexed (fn [i _] (str "\u039bt" i ".")) all-ordered))]
          (str prefix final-body))))))

(defn- normalize-set-order
  "Normalize set element ordering in show output: {b, a} → {a, b}"
  [s]
  (clojure.string/replace s #"\{([^{}]+)\}"
    (fn [[_ contents]]
      (let [elements (clojure.string/split contents #", ")]
        (str "{" (clojure.string/join ", " (sort elements)) "}")))))

(defn- alpha-equivalent-terms? [expected actual]
  (let [e-str (normalize-set-order (normalize-type-var-names (show-term expected)))
        a-str (normalize-set-order (normalize-type-var-names (show-term actual)))]
    (= e-str a-str)))

(defn- alpha-equivalent-types? [expected actual]
  (let [e-str (normalize-set-order (normalize-type-var-names (show-type expected)))
        a-str (normalize-set-order (normalize-type-var-names (show-type actual)))]
    (= e-str a-str)))

;; ==========================================================================
;; Test execution
;; ==========================================================================

(defn- run-evaluation-test [path tc]
  (let [input (:input tc)
        expected (:output tc)
        graph @test-graph
        cx (empty-context)
        eager (= (first (:evaluation_style tc)) :eager)
        reduce-var (ns-resolve 'hydra.reduction 'hydra_reduction_reduce_term)
        _ (when (nil? reduce-var) (throw (RuntimeException. "reduce_term var not found")))
        reduce-fn (deref reduce-var)
        _ (when (nil? reduce-fn) (throw (RuntimeException. "reduce_term is nil")))]
    (try
      (let [step1 (reduce-fn cx)
            step2 (step1 graph)
            step3 (step2 eager)
            result (step3 input)]
        (if (= (first result) :left)
          (let [err (second result)
                msg (loop [e err]
                      (cond
                        (instance? hydra_context_in_context e) (recur (.object e))
                        (and (sequential? e) (= (first e) :other)) (recur (second e))
                        (map? e) (recur (:object e))
                        :else (str e)))]
            (println (str "FAIL: " path))
            (println (str "  ERROR: " msg))
            [0 1 0])
          (let [actual (second result)]
            (if (terms-match? actual expected)
              [1 0 0]
              (do (println (str "FAIL: " path))
                  (try
                    (println (str "  Expected: " (show-term expected)))
                    (println (str "  Actual:   " (show-term actual)))
                    (catch Throwable _se
                      (println (str "  Expected (raw): " (pr-str expected)))
                      (println (str "  Actual (raw):   " (pr-str actual)))))
                  [0 1 0])))))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        (when (= path "common > hydra.lib.chars primitives > isAlphaNum > letter")
          (.printStackTrace e))
        [0 1 0]))))

;; ---- Simple test runners (no graph/context needed) ----

(defn- resolve-fn [ns-sym fn-sym]
  (when-let [v (ns-resolve ns-sym fn-sym)] @v))

(defn- run-string-comparison-test [path expected-str actual-str]
  "Compare two show strings with alpha-equivalence normalization."
  (try
    (let [e (or expected-str "#f")
          a (or actual-str "#f")]
      (if (= e a)
        [1 0 0]
        (let [e-norm (normalize-set-order (normalize-type-var-names e))
              a-norm (normalize-set-order (normalize-type-var-names a))]
          (if (= e-norm a-norm)
            [1 0 0]
            (do (println (str "FAIL: " path))
                (println (str "  Expected: " e))
                (println (str "  Actual:   " a))
                [0 1 0])))))
    (catch Throwable e
      (println (str "FAIL: " path))
      (println (str "  EXCEPTION: " (.getMessage e)))
      [0 1 0])))

(defn- run-universal-test [path tc]
  "Run a universal test case: compare actual and expected strings."
  (let [actual (:actual tc)
        expected (:expected tc)]
    (if (= actual expected)
      [1 0 0]
      (do (println (str "FAIL: " path))
          (println (str "  Expected: " (pr-str expected)))
          (println (str "  Actual:   " (pr-str actual)))
          [0 1 0]))))

(defn- run-simple-test [path expected actual-fn]
  "Run a test that compares expected to the result of actual-fn."
  (try
    (let [actual (actual-fn)]
      (if (terms-match? actual expected)
        [1 0 0]
        (do (println (str "FAIL: " path))
            (try
              (println (str "  Expected: " (show-term expected)))
              (println (str "  Actual:   " (show-term actual)))
              (catch Throwable _
                (println (str "  Expected (raw): " (pr-str expected)))
                (println (str "  Actual (raw):   " (pr-str actual)))))
            [0 1 0])))
    (catch Throwable e
      (println (str "FAIL: " path))
      (println (str "  EXCEPTION: " (.getMessage e)))
      [0 1 0])))

(defn- run-either-test [path expected either-result]
  "Run a test where the result is (list :right value) or (list :left error)."
  (try
    (if (= (first either-result) :left)
      (do (println (str "FAIL: " path))
          (println (str "  ERROR: " (second either-result)))
          [0 1 0])
      (let [actual (second either-result)]
        (if (terms-match? actual expected)
          [1 0 0]
          (do (println (str "FAIL: " path))
              (try
                (println (str "  Expected: " (show-term expected)))
                (println (str "  Actual:   " (show-term actual)))
                (catch Throwable _
                  (println (str "  Expected (raw): " (pr-str expected)))
                  (println (str "  Actual (raw):   " (pr-str actual)))))
              [0 1 0]))))
    (catch Throwable e
      (println (str "FAIL: " path))
      (println (str "  EXCEPTION: " (.getMessage e)))
      [0 1 0])))

(defn- run-string-comparison-test
  "Run a test comparing two strings (e.g. show outputs)."
  [path expected-str actual-str]
  (try
    (if (= expected-str actual-str)
      [1 0 0]
      (let [e-norm (normalize-type-var-names expected-str)
            a-norm (normalize-type-var-names actual-str)]
        (if (= e-norm a-norm)
          [1 0 0]
          (do (println (str "FAIL: " path))
              (println (str "  Expected: " expected-str))
              (println (str "  Actual:   " actual-str))
              [0 1 0]))))
    (catch Throwable e
      (println (str "FAIL: " path))
      (println (str "  EXCEPTION: " (.getMessage e)))
      [0 1 0])))

(defn- run-alpha-conversion-test [path tc]
  (let [f (resolve-fn 'hydra.reduction 'hydra_reduction_alpha_convert)]
    (run-simple-test path (:result tc) #(((f (:old_variable tc)) (:new_variable tc)) (:term tc)))))

(defn- run-case-conversion-test [path tc]
  (let [f (resolve-fn 'hydra.formatting 'hydra_formatting_convert_case)]
    (run-simple-test path (:to_string tc) #(((f (:from_convention tc)) (:to_convention tc)) (:from_string tc)))))

(defn- run-deannotate-term-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_strip_deannotate_term)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-deannotate-type-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_deannotate_type)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-flatten-let-terms-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_flatten_let_terms)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-free-variables-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_free_variables_in_term)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-lift-lambda-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_lift_lambda_above_let)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-simplify-term-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_simplify_term)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-normalize-type-vars-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_normalize_type_variables_in_term)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-topological-sort-test [path tc]
  (let [f (resolve-fn 'hydra.sorting 'hydra_sorting_topological_sort)]
    (run-simple-test path (:expected tc) #(f (:adjacency_list tc)))))

(defn- run-topological-sort-scc-test [path tc]
  (let [f (resolve-fn 'hydra.sorting 'hydra_sorting_topological_sort_components)]
    (run-simple-test path (:expected tc) #(f (:adjacency_list tc)))))

(defn- run-serialization-test [path tc]
  (let [parenthesize (resolve-fn 'hydra.serialization 'hydra_serialization_parenthesize)
        print-expr (resolve-fn 'hydra.serialization 'hydra_serialization_print_expr)]
    (run-simple-test path (:output tc) #(print-expr (parenthesize (:input tc))))))

(defn- run-type-reduction-test [path tc]
  (let [f (resolve-fn 'hydra.reduction 'hydra_reduction_beta_reduce_type)
        cx (empty-context) graph @test-graph]
    (run-either-test path (:output tc) (((f cx) graph) (:input tc)))))

(defn- run-unshadow-variables-test [path tc]
  (let [f (resolve-fn 'hydra.rewriting 'hydra_rewriting_unshadow_variables)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

;; ---- Medium test runners (need graph/context, Either results) ----

(defn- run-eta-expansion-test [path tc]
  (let [f (resolve-fn 'hydra.reduction 'hydra_reduction_eta_expand_typed_term)
        cx (empty-context) graph @test-graph]
    (run-either-test path (:output tc) (((f cx) graph) (:input tc)))))

(defn- run-inference-test [path tc]
  (let [f (resolve-fn 'hydra.inference 'hydra_inference_infer_type_of)
        cx (empty-context) graph @test-graph]
    (try
      (let [_ (when (nil? f) (throw (RuntimeException. "infer_type_of function is nil")))
            step1 (f cx)
            _ (when (nil? step1) (throw (RuntimeException. "infer_type_of(cx) returned nil")))
            step2 (step1 graph)
            _ (when (nil? step2) (throw (RuntimeException. "infer_type_of(cx)(graph) returned nil")))
            result (step2 (:input tc))]
        (if (= (first result) :left)
          (do (println (str "FAIL: " path))
              (let [err (second result)
                    msg (loop [e err]
                          (cond
                            (instance? hydra_context_in_context e) (recur (.object e))
                            (and (sequential? e) (= (first e) :other)) (recur (second e))
                            (map? e) (recur (:object e))
                            :else (str e)))]
                (println (str "  Inference ERROR: " msg)))
              [0 1 0])
          ;; result is (:right (pair (pair inferred-term type-scheme) context))
          (let [pair-val (second result)
                inner-pair (first pair-val)
                result-scheme (second inner-pair)
                show-ts (resolve-fn 'hydra.show.core 'hydra_show_core_type_scheme)
                expected-str (when show-ts (show-ts (:output tc)))
                actual-str (when show-ts (show-ts result-scheme))]
            (run-string-comparison-test path expected-str actual-str))))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        (when (= path "common > inference > Fundamentals > Literals > #1")
          (.printStackTrace e))
        [0 1 0]))))

(defn- run-inference-failure-test [path tc]
  (let [f (resolve-fn 'hydra.inference 'hydra_inference_infer_type_of)
        cx (empty-context) graph @test-graph
        result (((f cx) graph) (:input tc))]
    (if (= (first result) :left)
      [1 0 0]  ;; Expected failure
      (do (println (str "FAIL: " path))
          (println "  Expected inference failure but got success")
          [0 1 0]))))

;; ---- Type checking tests ----

(defn- type-scheme-to-type
  "Convert a TypeScheme back to a Type by wrapping forall binders around the body."
  [ts]
  (let [vars (:variables ts)
        body (:type ts)]
    (reduce (fn [t v] (list :forall {:parameter v :body t}))
            body
            (reverse vars))))

(defn- run-type-checking-test [path tc]
  (let [infer-fn (resolve-fn 'hydra.inference 'hydra_inference_infer_type_of)
        type-of-fn (resolve-fn 'hydra.checking 'hydra_checking_type_of)
        remove-types-fn (resolve-fn 'hydra.rewriting 'hydra_rewriting_remove_types_from_term)
        cx (empty-context) graph @test-graph]
    (try
      ;; Step 1: Infer type
      (let [infer-result (((infer-fn cx) graph) (:input tc))]
        (if (= (first infer-result) :left)
          (do (println (str "FAIL: " path))
              (println (str "  Inference failed: " (second infer-result)))
              [0 1 0])
          (let [pair-val (second infer-result)
                inner-pair (first pair-val)
                inferred-term (first inner-pair)
                result-scheme (second inner-pair)
                infer-cx (second pair-val)
                inferred-type (type-scheme-to-type result-scheme)
                ;; Step 2: Reconstruct type using typeOf
                type-of-result ((((type-of-fn infer-cx) graph) (list)) inferred-term)]
            (if (= (first type-of-result) :left)
              (do (println (str "FAIL: " path))
                  (println (str "  Type reconstruction failed: " (second type-of-result)))
                  [0 1 0])
              (let [reconstructed-type (first (second type-of-result))
                    ;; Compare using alpha-equivalence
                    term-ok? (alpha-equivalent-terms? (:output_term tc) inferred-term)
                    type-ok? (alpha-equivalent-types? (:output_type tc) inferred-type)
                    recon-ok? (alpha-equivalent-types? (:output_type tc) reconstructed-type)]
                (if (and term-ok? type-ok? recon-ok?)
                  [1 0 0]
                  (do (println (str "FAIL: " path))
                      (when (not term-ok?)
                        (println (str "  Inferred term mismatch"))
                        (println (str "    Expected: " (normalize-type-var-names (show-term (:output_term tc)))))
                        (println (str "    Actual:   " (normalize-type-var-names (show-term inferred-term)))))
                      (when (not type-ok?)
                        (println (str "  Inferred type mismatch"))
                        (println (str "    Expected: " (normalize-type-var-names (show-type (:output_type tc)))))
                        (println (str "    Actual:   " (normalize-type-var-names (show-type inferred-type)))))
                      (when (not recon-ok?)
                        (println (str "  Reconstructed type mismatch"))
                        (println (str "    Expected: " (normalize-type-var-names (show-type (:output_type tc)))))
                        (println (str "    Actual:   " (normalize-type-var-names (show-type reconstructed-type)))))
                      [0 1 0])))))))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

(defn- run-type-checking-failure-test [path tc]
  ;; TODO: implement when test data is available
  [0 0 1])

;; ---- Variable occurs in type ----

(defn- run-variable-occurs-in-type-test [path tc]
  (let [f (resolve-fn 'hydra.unification 'hydra_unification_variable_occurs_in_type)]
    (run-simple-test path (:expected tc) #((f (:variable tc)) (:type tc)))))

;; ---- Subst in type ----

(defn- run-subst-in-type-test [path tc]
  (let [f (resolve-fn 'hydra.substitution 'hydra_substitution_subst_in_type)
        from-list (resolve-fn 'hydra.lib.maps 'hydra_lib_maps_from_list)
        ;; Build TypeSubst from list of (name, type) pairs as alist (Hydra map format)
        ;; Note: in generated code, TypeSubst is transparent (bare map, not a record)
        subst-alist (if from-list (from-list (:substitution tc)) ())]
    (run-simple-test path (:output tc) #((f subst-alist) (:input tc)))))

;; ---- Unify types ----

(defn- run-unify-types-test [path tc]
  (let [f (resolve-fn 'hydra.unification 'hydra_unification_unify_types)
        cx (empty-context)
        ;; Build schema types as Hydra alist map from the list of names
        from-list (resolve-fn 'hydra.lib.maps 'hydra_lib_maps_from_list)
        schema-entries (map (fn [n] (list n (->hydra_core_type_scheme () (list :variable n) nil)))
                            (:schema_types tc))
        schema-types (if from-list (from-list schema-entries) ())
        result (((((f cx) schema-types) (:left tc)) (:right tc)) "test")]
    (try
      (let [expected (:expected tc)]
        ;; expected is an Either: (:left ...) for expected failure, (:right ...) for expected success
        (cond
          (= (first expected) :left)
          ;; Expected failure
          (if (= (first result) :left)
            [1 0 0]
            (do (println (str "FAIL: " path))
                (println "  Expected unification failure but got success")
                [0 1 0]))

          (= (first expected) :right)
          ;; Expected success
          (if (= (first result) :left)
            (do (println (str "FAIL: " path))
                (println (str "  Expected unification success but got failure: " (second result)))
                [0 1 0])
            (let [actual-subst (second result)
                  expected-subst (second expected)
                  ;; Normalize TypeSubst maps for comparison: convert alist and Clojure maps to sorted pairs
                  normalize-subst (fn [ts]
                                    (let [v (:value ts)]
                                      (sort-by first
                                        (cond
                                          (map? v) (vec v)
                                          (sequential? v) (vec (map (fn [entry] [(first entry) (second entry)]) v))
                                          :else []))))]
              (if (= (normalize-subst expected-subst) (normalize-subst actual-subst))
                [1 0 0]
                (do (println (str "FAIL: " path))
                    (println (str "  Expected subst: " (pr-str (normalize-subst expected-subst))))
                    (println (str "  Actual subst:   " (pr-str (normalize-subst actual-subst))))
                    [0 1 0]))))

          :else
          (do (println (str "FAIL: " path " - unexpected expected format"))
              [0 1 0])))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

;; ---- Join types ----

(defn- run-join-types-test [path tc]
  (let [f (resolve-fn 'hydra.unification 'hydra_unification_join_types)
        cx (empty-context)
        result ((((f cx) (:left tc)) (:right tc)) "test")]
    (try
      (let [expected (:expected tc)]
        (cond
          (= (first expected) :left)
          ;; Expected failure
          (if (= (first result) :left)
            [1 0 0]
            (do (println (str "FAIL: " path))
                (println "  Expected join failure but got success")
                [0 1 0]))

          (= (first expected) :right)
          ;; Expected success
          (if (= (first result) :left)
            (do (println (str "FAIL: " path))
                (println (str "  Expected join success but got failure: " (second result)))
                [0 1 0])
            (let [actual (second result)
                  expected-val (second expected)]
              (if (= expected-val actual)
                [1 0 0]
                (do (println (str "FAIL: " path))
                    (println (str "  Expected: " (pr-str expected-val)))
                    (println (str "  Actual:   " (pr-str actual)))
                    [0 1 0]))))

          :else
          (do (println (str "FAIL: " path " - unexpected expected format"))
              [0 1 0])))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

;; ---- Topological sort bindings ----

(defn- run-topological-sort-bindings-test [path tc]
  (let [f (resolve-fn 'hydra.dependencies 'hydra_dependencies_topological_sort_binding_map)
        ;; tc.bindings is a list of (Name, Term) pairs -- build a map
        from-list-fn (resolve-fn 'hydra.lib.maps 'hydra_lib_maps_from_list)
        binding-map (if from-list-fn
                      (from-list-fn (:bindings tc))
                      (into {} (map (fn [p] [(first p) (second p)]) (:bindings tc))))
        result (f binding-map)
        ;; Compare as sets of sets (order within SCCs doesn't matter)
        result-set (set (map set result))
        expected-set (set (map set (:expected tc)))]
    (if (= expected-set result-set)
      [1 0 0]
      (do (println (str "FAIL: " path))
          (println (str "  Expected: " (pr-str (:expected tc))))
          (println (str "  Actual:   " (pr-str result)))
          [0 1 0]))))

;; ---- Hoist case statements ----

(defn- run-hoist-case-statements-test [path tc]
  (let [f (resolve-fn 'hydra.hoisting 'hydra_hoisting_hoist_case_statements)
        eg (empty-graph)]
    (run-simple-test path (:output tc) #((f eg) (:input tc)))))

;; ---- Hoist subterms ----

(defn- predicate-fn
  "Convert a HoistPredicate union to a Clojure predicate function.
   Each predicate takes a (pair path term) and returns boolean."
  [pred]
  (let [pred-type (first pred)]
    (case pred-type
      :nothing    (fn [pair] false)
      :lists      (fn [pair]
                    (let [term (second pair)]
                      (and (sequential? term) (= (first term) :list))))
      :applications (fn [pair]
                      (let [term (second pair)]
                        (and (sequential? term) (= (first term) :application))))
      :case_statements (fn [pair]
                         (let [term (second pair)]
                           (and (sequential? term) (= (first term) :function)
                                (let [f (second term)]
                                  (and (sequential? f) (= (first f) :elimination))))))
      ;; default: never hoist
      (fn [pair] false))))

(defn- run-hoist-subterms-test [path tc]
  (let [f (resolve-fn 'hydra.hoisting 'hydra_hoisting_hoist_subterms)
        eg (empty-graph)
        pred (predicate-fn (:predicate tc))]
    (run-simple-test path (:output tc) #(((f pred) eg) (:input tc)))))

;; ---- Hoist let bindings ----

(defn- run-hoist-let-bindings-test [path tc]
  (let [f (resolve-fn 'hydra.hoisting 'hydra_hoisting_hoist_all_let_bindings)]
    ;; Compare via show (like Java)
    (try
      (let [result (f (:input tc))
            expected-str (show-let (:output tc))
            actual-str (show-let result)]
        (run-string-comparison-test path expected-str actual-str))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

;; ---- Hoist polymorphic let bindings ----

(defn- run-hoist-polymorphic-let-bindings-test [path tc]
  (let [f (resolve-fn 'hydra.hoisting 'hydra_hoisting_hoist_polymorphic_let_bindings)]
    ;; Use (fn [_] true) predicate like Java: b -> true
    (try
      (let [result ((f (fn [_] true)) (:input tc))
            expected-str (show-let (:output tc))
            actual-str (show-let result)]
        (run-string-comparison-test path expected-str actual-str))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

;; ---- Rewrite term ----

(defn- run-rewrite-term-test [path tc]
  (let [rewrite-fn (resolve-fn 'hydra.rewriting 'hydra_rewriting_rewrite_term)
        rewriter (:rewriter tc)
        rewriter-type (first rewriter)
        rewrite-impl
        (case rewriter-type
          :replace_foo_with_bar
          (fn [recurse]
            (fn [term]
              (if (and (sequential? term) (= (first term) :literal)
                       (let [lit (second term)]
                         (and (sequential? lit) (= (first lit) :string)
                              (= (second lit) "foo"))))
                (list :literal (list :string "bar"))
                (recurse term))))
          :replace_int32_with_int64
          (fn [recurse]
            (fn [term]
              (if (and (sequential? term) (= (first term) :literal)
                       (let [lit (second term)]
                         (and (sequential? lit) (= (first lit) :integer)
                              (let [iv (second lit)]
                                (and (sequential? iv) (= (first iv) :int32))))))
                (let [val (second (second (second term)))]
                  (list :literal (list :integer (list :int64 (long val)))))
                (recurse term))))
          ;; Default: identity rewrite
          (fn [recurse] (fn [term] (recurse term))))]
    (run-simple-test path (:output tc) #((rewrite-fn rewrite-impl) (:input tc)))))

;; ---- Rewrite type ----

(defn- run-rewrite-type-test [path tc]
  (let [rewrite-fn (resolve-fn 'hydra.rewriting 'hydra_rewriting_rewrite_type)
        rewriter (:rewriter tc)
        rewriter-type (first rewriter)
        rewrite-impl
        (case rewriter-type
          :replace_string_with_int32
          (fn [recurse]
            (fn [typ]
              (if (and (sequential? typ) (= (first typ) :literal)
                       (let [lt (second typ)]
                         (and (sequential? lt) (= (first lt) :string))))
                (list :literal (list :integer (list :int32 nil)))
                (recurse typ))))
          ;; Default: identity rewrite
          (fn [recurse] (fn [typ] (recurse typ))))]
    (run-simple-test path (:output tc) #((rewrite-fn rewrite-impl) (:input tc)))))

;; ---- Fold over term ----

(defn- get-int32
  "Extract an int32 value from a term, or return 0."
  [t]
  (if (and (sequential? t) (= (first t) :literal)
           (let [lit (second t)]
             (and (sequential? lit) (= (first lit) :integer)
                  (let [iv (second lit)]
                    (and (sequential? iv) (= (first iv) :int32))))))
    (second (second (second t)))
    0))

(defn- get-list-length
  "If term is a list, return a list containing its length; otherwise empty list."
  [t]
  (if (and (sequential? t) (= (first t) :list))
    (list (count (second t)))
    (list)))

(defn- get-label
  "If term is a pair whose first element is a string literal, return a list with that literal."
  [t]
  (if (and (sequential? t) (= (first t) :pair))
    (let [pair-val (second t)
          fst (first pair-val)]
      (if (and (sequential? fst) (= (first fst) :literal)
               (let [lit (second fst)]
                 (and (sequential? lit) (= (first lit) :string))))
        (list (second fst))
        (list)))
    (list)))

(defn- run-fold-over-term-test [path tc]
  (let [fold-fn (resolve-fn 'hydra.rewriting 'hydra_rewriting_fold_over_term)
        order (:traversal_order tc)
        operation (:operation tc)
        op-type (first operation)
        input (:input tc)]
    (try
      (let [result
            (case op-type
              :sum_int32_literals
              (let [sum ((((fold-fn order) (fn [acc] (fn [t] (+ acc (get-int32 t))))) 0) input)]
                (list :literal (list :integer (list :int32 sum))))

              :collect_list_lengths
              (let [lengths ((((fold-fn order) (fn [acc] (fn [t] (concat acc (get-list-length t))))) (list)) input)]
                (list :list (mapv (fn [len] (list :literal (list :integer (list :int32 len)))) lengths)))

              :collect_labels
              (let [labels ((((fold-fn order) (fn [acc] (fn [t] (concat acc (get-label t))))) (list)) input)]
                (list :list (mapv (fn [label] (list :literal label)) labels))))]
        (if (terms-match? result (:output tc))
          [1 0 0]
          (do (println (str "FAIL: " path))
              (try
                (println (str "  Expected: " (show-term (:output tc))))
                (println (str "  Actual:   " (show-term result)))
                (catch Throwable _
                  (println (str "  Expected (raw): " (pr-str (:output tc))))
                  (println (str "  Actual (raw):   " (pr-str result)))))
              [0 1 0])))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

;; ---- JSON tests ----

(defn- run-json-parser-test [path tc]
  (let [f (resolve-fn 'hydra.json.parser 'hydra_json_parser_parse_json)]
    (try
      (let [result (f (:input tc))]
        (if (terms-match? result (:output tc))
          [1 0 0]
          (do (println (str "FAIL: " path))
              (println (str "  Expected (raw): " (pr-str (:output tc))))
              (println (str "  Actual (raw):   " (pr-str result)))
              [0 1 0])))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

(defn- run-json-writer-test [path tc]
  (let [f (resolve-fn 'hydra.json.writer 'hydra_json_writer_print_json)]
    (run-simple-test path (:output tc) #(f (:input tc)))))

(defn- run-json-coder-test [path tc]
  (let [encode-fn (resolve-fn 'hydra.json.encode 'hydra_json_encode_to_json)
        decode-fn (resolve-fn 'hydra.json.decode 'hydra_json_decode_from_json)
        empty-types ()]
    (try
      (let [encode-result (encode-fn (:term tc))]
        (if (= (first encode-result) :left)
          (do (println (str "FAIL: " path))
              (println (str "  JSON encode failed: " (second encode-result)))
              [0 1 0])
          (let [encoded (second encode-result)]
            (if (not (terms-match? (:json tc) encoded))
              (do (println (str "FAIL: " path))
                  (println (str "  JSON encode mismatch"))
                  (println (str "  Expected (raw): " (pr-str (:json tc))))
                  (println (str "  Actual (raw):   " (pr-str encoded)))
                  [0 1 0])
              ;; Roundtrip: decode back
              (let [decode-result ((((decode-fn empty-types) (->hydra_core_name "test")) (:type tc)) encoded)]
                (if (= (first decode-result) :left)
                  (do (println (str "FAIL: " path))
                      (println (str "  JSON decode failed: " (second decode-result)))
                      [0 1 0])
                  (let [decoded (second decode-result)]
                    (if (terms-match? (:term tc) decoded)
                      [1 0 0]
                      (do (println (str "FAIL: " path))
                          (println (str "  JSON roundtrip term mismatch"))
                          (try
                            (println (str "  Expected: " (show-term (:term tc))))
                            (println (str "  Actual:   " (show-term decoded)))
                            (catch Throwable _
                              (println (str "  Expected (raw): " (pr-str (:term tc))))
                              (println (str "  Actual (raw):   " (pr-str decoded)))))
                          [0 1 0])))))))))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

(defn- run-json-roundtrip-test [path tc]
  (let [encode-fn (resolve-fn 'hydra.json.encode 'hydra_json_encode_to_json)
        decode-fn (resolve-fn 'hydra.json.decode 'hydra_json_decode_from_json)
        empty-types ()]
    (try
      (let [encode-result (encode-fn (:term tc))]
        (if (= (first encode-result) :left)
          (do (println (str "FAIL: " path))
              (println (str "  JSON encode failed: " (second encode-result)))
              [0 1 0])
          (let [encoded (second encode-result)
                ;; Decode back
                decode-result ((((decode-fn empty-types) (->hydra_core_name "test")) (:type tc)) encoded)]
            (if (= (first decode-result) :left)
              (do (println (str "FAIL: " path))
                  (println (str "  JSON decode failed: " (second decode-result)))
                  [0 1 0])
              (let [decoded (second decode-result)]
                (if (terms-match? (:term tc) decoded)
                  [1 0 0]
                  (do (println (str "FAIL: " path))
                      (println (str "  JSON roundtrip mismatch"))
                      (try
                        (println (str "  Expected: " (show-term (:term tc))))
                        (println (str "  Actual:   " (show-term decoded)))
                        (catch Throwable _
                          (println (str "  Expected (raw): " (pr-str (:term tc))))
                          (println (str "  Actual (raw):   " (pr-str decoded)))))
                      [0 1 0])))))))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

(defn- run-json-decode-test [path tc]
  (let [decode-fn (resolve-fn 'hydra.json.decode 'hydra_json_decode_from_json)
        empty-types ()]
    (try
      (let [decode-result ((((decode-fn empty-types) (->hydra_core_name "test")) (:type tc)) (:json tc))
            expected (:expected tc)]
        ;; expected is an Either
        (cond
          (= (first expected) :left)
          ;; Expected failure
          (if (= (first decode-result) :left)
            [1 0 0]
            (do (println (str "FAIL: " path))
                (println "  Expected decode failure but got success")
                [0 1 0]))

          (= (first expected) :right)
          ;; Expected success
          (if (= (first decode-result) :left)
            (do (println (str "FAIL: " path))
                (println (str "  JSON decode failed: " (second decode-result)))
                [0 1 0])
            (let [actual (second decode-result)
                  expected-term (second expected)]
              (if (terms-match? expected-term actual)
                [1 0 0]
                (do (println (str "FAIL: " path))
                    (println (str "  JSON decode mismatch"))
                    (try
                      (println (str "  Expected: " (show-term expected-term)))
                      (println (str "  Actual:   " (show-term actual)))
                      (catch Throwable _
                        (println (str "  Expected (raw): " (pr-str expected-term)))
                        (println (str "  Actual (raw):   " (pr-str actual)))))
                    [0 1 0]))))

          :else
          (do (println (str "FAIL: " path " - unexpected expected format"))
              [0 1 0])))
      (catch Throwable e
        (println (str "FAIL: " path))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

(defn- run-json-encode-test [path tc]
  ;; JsonEncodeTestCase has :term and :expected but no :type, so we cannot create a coder.
  ;; Java also skips this: `return withTimeout(name, () -> {});`
  [0 0 1])

(defn- run-validate-core-term-test [path tc]
  (if (resolve 'hydra_validate_core_term)
    (run-simple-test path (:output tc)
      (fn [] ((resolve 'hydra_validate_core_term) (:typed tc) @test-graph (:input tc))))
    [0 0 1]))

;; ---- Test case dispatcher ----

(defn- run-test-case [path tcase]
  (try
    (let [tname (:name tcase) full (str path " > " tname)
          tags (:tags tcase) disabled? (some #(= % "disabled") tags)
          tc (:case tcase)]
      (if disabled? [0 0 1]
          (let [case-type (first tc) case-data (second tc)]
            (case case-type
              :evaluation              (run-evaluation-test full case-data)
              :alpha_conversion        (run-alpha-conversion-test full case-data)
              :case_conversion         (run-case-conversion-test full case-data)
              :deannotate_term         (run-deannotate-term-test full case-data)
              :deannotate_type         (run-deannotate-type-test full case-data)
              :flatten_let_terms       (run-flatten-let-terms-test full case-data)
              :free_variables          (run-free-variables-test full case-data)
              :lift_lambda_above_let   (run-lift-lambda-test full case-data)
              :simplify_term           (run-simplify-term-test full case-data)
              :normalize_type_variables (run-normalize-type-vars-test full case-data)
              :topological_sort        (run-topological-sort-test full case-data)
              :topological_sort_s_c_c  (run-topological-sort-scc-test full case-data)
              :serialization           (run-serialization-test full case-data)
              :type_reduction          (run-type-reduction-test full case-data)
              :unshadow_variables      (run-unshadow-variables-test full case-data)
              :eta_expansion           (run-eta-expansion-test full case-data)
              :inference               (run-inference-test full case-data)
              :inference_failure       (run-inference-failure-test full case-data)
              ;; New test types
              :type_checking           (run-type-checking-test full case-data)
              :type_checking_failure   (run-type-checking-failure-test full case-data)
              :variable_occurs_in_type (run-variable-occurs-in-type-test full case-data)
              :subst_in_type           (run-subst-in-type-test full case-data)
              :unify_types             (run-unify-types-test full case-data)
              :join_types              (run-join-types-test full case-data)
              :topological_sort_bindings (run-topological-sort-bindings-test full case-data)
              :hoist_case_statements   (run-hoist-case-statements-test full case-data)
              :hoist_subterms          (run-hoist-subterms-test full case-data)
              :hoist_let_bindings      (run-hoist-let-bindings-test full case-data)
              :hoist_polymorphic_let_bindings (run-hoist-polymorphic-let-bindings-test full case-data)
              :rewrite_term            (run-rewrite-term-test full case-data)
              :rewrite_type            (run-rewrite-type-test full case-data)
              :fold_over_term          (run-fold-over-term-test full case-data)
              :json_parser             (run-json-parser-test full case-data)
              :json_writer             (run-json-writer-test full case-data)
              :json_coder              (run-json-coder-test full case-data)
              :json_roundtrip          (run-json-roundtrip-test full case-data)
              :json_decode             (run-json-decode-test full case-data)
              :json_encode             (run-json-encode-test full case-data)
              :validate_core_term      (run-validate-core-term-test full case-data)
              ;; Skip remaining unimplemented test types
              :delegated_evaluation    [0 0 1]
              :universal               (run-universal-test full case-data)
              [0 0 1]))))
    (catch Throwable e
      (let [tname (:name tcase) full (str path " > " tname)]
        (println (str "FAIL: " full))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0]))))

(defn run-test-group
  "Run a test group. Returns [pass fail skip benchmark].
   benchmark is {:path :passed :failed :skipped :totalTimeMs :subgroups}."
  ([path group] (run-test-group path "" group))
  ([path bench-prefix group]
  (try
    (let [gname (:name group)
          full (if (= path "") gname (str path " > " gname))
          bench-path (if (= bench-prefix "") gname (str bench-prefix "/" gname))
          t0 (System/nanoTime)
          sub-results (mapv #(run-test-group full bench-path %) (:subgroups group))
          case-results (mapv #(run-test-case full %) (:cases group))
          elapsed-ms (/ (- (System/nanoTime) t0) 1e6)
          pass (+ (reduce + 0 (map first sub-results)) (reduce + 0 (map first case-results)))
          fail (+ (reduce + 0 (map second sub-results)) (reduce + 0 (map second case-results)))
          skip (+ (reduce + 0 (map #(nth % 2) sub-results)) (reduce + 0 (map #(nth % 2) case-results)))
          sub-benchmarks (mapv #(nth % 3) sub-results)
          benchmark {:path bench-path
                     :passed pass :failed fail :skipped skip
                     :totalTimeMs (Math/round elapsed-ms)
                     :subgroups (vec (filter some? sub-benchmarks))}]
      [pass fail skip benchmark])
    (catch Throwable e
      (let [gname (:name group) full (if (= path "") gname (str path " > " gname))]
        (println (str "GROUP FAIL: " full))
        (println (str "  EXCEPTION: " (.getMessage e)))
        [0 1 0 nil])))))

(defn- benchmark-to-json
  "Convert a benchmark map to a JSON string."
  [b]
  (let [subs (:subgroups b [])
        sub-json (when (seq subs)
                   (str ", \"subgroups\": ["
                        (clojure.string/join ", " (map benchmark-to-json subs))
                        "]"))]
    (str "{\"path\": \"" (:path b) "\""
         ", \"passed\": " (:passed b 0)
         ", \"failed\": " (:failed b 0)
         ", \"skipped\": " (:skipped b 0)
         ", \"totalTimeMs\": " (:totalTimeMs b 0)
         (or sub-json "")
         "}")))

(defn- write-benchmark-json
  "Write benchmark results to HYDRA_BENCHMARK_OUTPUT if set."
  [benchmark total-ms]
  (when-let [output-path (System/getenv "HYDRA_BENCHMARK_OUTPUT")]
    (let [top-groups (:subgroups benchmark)
          json-groups (mapv (fn [g] (str "    " (benchmark-to-json g))) top-groups)
          json (str "{\n  \"groups\": [\n"
                    (clojure.string/join ",\n" json-groups)
                    "\n  ],\n  \"summary\": {\n"
                    "    \"totalPassed\": " (:passed benchmark) ",\n"
                    "    \"totalFailed\": " (:failed benchmark) ",\n"
                    "    \"totalSkipped\": " (:skipped benchmark) ",\n"
                    "    \"totalTimeMs\": " (Math/round total-ms) "\n"
                    "  }\n}")]
      (spit output-path json)
      (println (str "Benchmark output: " output-path)))))

(defn -main [& _args]
  ;; Load hand-written libraries and globalize their vars so generated code can find them
  (preload/ensure-libs-loaded!)
  ;; Load generated modules used by test dispatchers
  ;; Load substitution first via preloader (has forward reference issue with native require)
  (preload/load-gen-main!)
  (require 'hydra.reduction)
  (require 'hydra.rewriting)
  (require 'hydra.formatting)
  (require 'hydra.sorting)
  (require 'hydra.serialization)
  (require 'hydra.inference)
  (require 'hydra.checking)
  (require 'hydra.hoisting)
  (require 'hydra.unification)
  (require 'hydra.substitution)
  (require 'hydra.show.core)
  ;; JSON/bootstrap modules
  (require 'hydra.json.bootstrap)
  (require 'hydra.json.parser)
  (require 'hydra.json.writer)
  (require 'hydra.json.encode)
  (require 'hydra.json.decode)
  ;; Load test suite
  (require 'hydra.test.testSuite)
  ;; Build the test graph now that all namespaces are loaded
  (ensure-test-graph!)
  ;; Run test suite
  (let [t0 (System/nanoTime)
        suite (deref (ns-resolve 'hydra.test.testSuite 'hydra_test_test_suite_all_tests))
        [pass fail skip benchmark] (run-test-group "" suite)
        total-ms (/ (- (System/nanoTime) t0) 1e6)]
    (println)
    (println "========================================")
    (println (str "Pass: " pass))
    (println (str "Fail: " fail))
    (println (str "Skip: " skip))
    (write-benchmark-json (assoc benchmark :passed pass :failed fail :skipped skip) total-ms)
    (System/exit (if (> fail 0) 1 0))))

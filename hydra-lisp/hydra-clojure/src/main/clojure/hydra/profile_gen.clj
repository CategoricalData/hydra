(ns hydra.profile-gen
  "Profile code generation to find bottlenecks.
   Usage: clojure -M -m hydra.profile-gen --target python --json-dir <path>"
  (:require [hydra.lib.preload :as preload]
            [hydra.lib.libraries :as libraries]
            [clojure.data.json :as json])
  (:gen-class))

(defn- r [sym]
  (or @(ns-resolve 'clojure.core sym)
      (throw (RuntimeException. (str "Cannot resolve: " sym)))))

(defn- rc [sym]
  (or (ns-resolve 'clojure.core sym)
      (throw (RuntimeException. (str "Cannot resolve: " sym)))))

(defn- format-time [millis]
  (cond
    (< millis 1000) (str millis "ms")
    (< millis 60000) (format "%.1fs" (/ millis 1000.0))
    :else (let [mins (quot millis 60000)
                secs (/ (mod millis 60000) 1000.0)]
            (format "%dm %.1fs" mins secs))))

(defn- timed [label f]
  (let [t0 (System/currentTimeMillis)
        result (f)
        t1 (System/currentTimeMillis)]
    (println (str "  " label ": " (format-time (- t1 t0))))
    (flush)
    result))

(defn- clojure-to-hydra-json [obj]
  (cond
    (nil? obj) (list :null nil)
    (boolean? obj) (list :boolean obj)
    (number? obj) (list :number (double obj))
    (string? obj) (list :string obj)
    (vector? obj) (list :array (mapv clojure-to-hydra-json obj))
    (sequential? obj) (list :array (vec (map clojure-to-hydra-json obj)))
    (map? obj) (list :object (into {} (map (fn [[k v]] [k (clojure-to-hydra-json v)]) obj)))
    :else (throw (IllegalArgumentException. (str "Unexpected JSON type: " (type obj))))))

(defn- bootstrap-graph []
  ((r '->hydra_graph_graph)
    {} {} {} #{} {} (libraries/standard-library) {} #{}))

(defn- bootstrap-schema-map []
  (into {}
    (map (fn [[name typ]]
           (let [ts ((r 'hydra_rewriting_f_type_to_type_scheme) typ)]
             [name ((r 'hydra_rewriting_deannotate_type_recursive) (:type ts))]))
         (r 'hydra_json_bootstrap_types_by_name))))

(defn- load-modules-from-json [base-path namespaces]
  (let [bs-graph (bootstrap-graph)
        schema-map (bootstrap-schema-map)]
    (mapv (fn [ns-str]
            (let [file-path (str base-path "/" ((r 'hydra_code_generation_namespace_to_path) ns-str) ".json")
                  obj (json/read-str (slurp file-path))
                  json-val (clojure-to-hydra-json obj)
                  mod-type (list :variable "hydra.module.Module")
                  json-result (((((r 'hydra_json_decode_from_json) schema-map) "hydra.module.Module") mod-type) json-val)
                  _ (when (= (first json-result) :left)
                      (throw (RuntimeException. (str "JSON decode error for " ns-str ": " (second json-result)))))
                  term (second json-result)
                  mod-result (((r 'hydra_decode_module_module) bs-graph) term)
                  _ (when (= (first mod-result) :left)
                      (throw (RuntimeException. (str "Module decode error for " ns-str ": " (second mod-result)))))]
              (second mod-result)))
          namespaces)))

(defn- read-manifest-field [base-path field-name]
  (let [manifest (json/read-str (slurp (str base-path "/manifest.json")))]
    (mapv identity (get manifest field-name))))

(defn- load-coder-modules! [ns-names]
  (doseq [ns-name ns-names]
    (preload/pre-declare-ns-symbols! ns-name)
    (let [ns-sym (symbol ns-name)
          the-ns (or (find-ns ns-sym) (create-ns ns-sym))]
      (preload/refer-clojure-into-ns! the-ns)))
  (doseq [ns-name ns-names]
    (preload/require-and-globalize! ns-name)))

(defn -main [& args]
  (let [args-map (apply hash-map args)
        target (get args-map "--target" "python")
        json-dir (get args-map "--json-dir")]

    (when (nil? json-dir)
      (println "Usage: clojure -M -m hydra.profile-gen --target <target> --json-dir <path>")
      (System/exit 1))

    (println "=== Profiling code generation ===")
    (println (str "Target: " target))
    (println)
    (flush)

    ;; Step 1: Load kernel
    (println "Step 1: Loading kernel...")
    (flush)
    (timed "preload" #(do (preload/ensure-libs-loaded!) (preload/load-gen-main!)))
    (println)

    ;; Step 2: Load coder modules
    (println "Step 2: Loading coder modules...")
    (flush)
    (timed "coder modules"
      #(do (case target
             "python" (load-coder-modules!
                        ["hydra.ext.python.syntax" "hydra.ext.python.language" "hydra.ext.python.names"
                         "hydra.ext.python.helpers" "hydra.ext.python.utils"
                         "hydra.ext.python.serde" "hydra.ext.python.coder"])
             "clojure" (load-coder-modules!
                         ["hydra.ext.lisp.syntax" "hydra.ext.lisp.language"
                          "hydra.ext.lisp.serde" "hydra.ext.lisp.coder"]))
           (preload/install-coder-performance-patches!)))
    (println)

    ;; Step 3: Load modules from JSON
    (println "Step 3: Loading modules from JSON...")
    (flush)
    (let [main-ns (read-manifest-field json-dir "mainModules")
          eval-ns (read-manifest-field json-dir "evalLibModules")
          all-kernel-ns (into (vec main-ns) eval-ns)
          all-mods (timed "load JSON" #(load-modules-from-json json-dir all-kernel-ns))]
      (println (str "  Loaded " (count all-mods) " modules"))
      (println)
      (flush)

      (let [bs-graph (bootstrap-graph)
            cx ((r '->hydra_context_in_context) nil (list))
            coder (case target
                    "python" @(rc 'hydra_ext_python_coder_module_to_python)
                    "clojure" (let [mtl @(rc 'hydra_ext_lisp_coder_module_to_lisp)
                                    pte @(rc 'hydra_ext_lisp_serde_program_to_expr)]
                                (fn [mod] (fn [defs] (fn [cx2] (fn [g]
                                  (let [result (((((mtl (list :clojure nil)) mod) defs) cx2) g)]
                                    (if (= (first result) :left)
                                      result
                                      (let [program (second result)
                                            code (@(rc 'hydra_serialization_print_expr)
                                                   (@(rc 'hydra_serialization_parenthesize)
                                                     (pte program)))
                                            ns-val (let [ns (:namespace mod)]
                                                     (if (string? ns) ns (:value ns)))
                                            fp (str (@(rc 'hydra_code_generation_namespace_to_path) ns-val) ".clj")]
                                        (list :right {fp code}))))))))))
            language (case target
                       "python" @(rc 'hydra_ext_python_language_python_language)
                       "clojure" @(rc 'hydra_ext_lisp_language_lisp_language))
            flags (case target
                    "python" [false true true false]
                    "clojure" [false false false false])
            do-infer (nth flags 0)
            do-expand (nth flags 1)
            do-hoist-case (nth flags 2)
            do-hoist-poly (nth flags 3)
            constraints (:constraints language)]

        ;; Step 4: Partition
        (println "Step 4: Partition modules...")
        (flush)
        (let [is-native-type (r 'hydra_annotations_is_native_type)
              type-mods (vec (filter (fn [mod]
                                       (some #(is-native-type %) (:elements mod)))
                                     all-mods))
              term-mods (vec (remove (fn [mod]
                                       (some #(is-native-type %) (:elements mod)))
                                     all-mods))]
          (println (str "  Type modules: " (count type-mods) ", Term modules: " (count term-mods)))
          (println)
          (flush)

          ;; Step 5: Build graphs
          (println "Step 5: Build graphs...")
          (flush)
          (let [namespace-map (into {} (map (fn [m] [(:namespace m) m]) all-mods))
                schema-mods (timed "schemaModDeps"
                              #(((r 'hydra_code_generation_module_type_deps_transitive) namespace-map) all-mods))
                schema-elements (vec (filter #(is-native-type %)
                                      (mapcat :elements (concat schema-mods type-mods))))
                data-mods (timed "dataModDeps"
                            #(((r 'hydra_code_generation_module_term_deps_transitive) namespace-map) all-mods))
                data-elements (vec (mapcat :elements data-mods))
                schema-graph (timed "schemaGraph"
                               #((((r 'hydra_lexical_elements_to_graph) bs-graph) {}) schema-elements))
                schema-types (let [result (((r 'hydra_schemas_schema_graph_to_typing_environment)
                                             (r 'hydra_lexical_empty_context)) schema-graph)]
                               (if (= (first result) :right) (second result) {}))
                data-graph (timed "dataGraph"
                             #((((r 'hydra_lexical_elements_to_graph) bs-graph) schema-types) data-elements))]
            (println)
            (flush)

            ;; Step 6: dataGraphToDefinitions
            (println "Step 6: dataGraphToDefinitions (adapt/expand)...")
            (println (str "  flags: infer=" do-infer " expand=" do-expand
                          " hoistCase=" do-hoist-case " hoistPoly=" do-hoist-poly))
            (flush)
            (let [namespaces (mapv #(:namespace %) term-mods)
                  raw-result (timed "dataGraphToDefinitions"
                               #((((((((((r 'hydra_adapt_data_graph_to_definitions)
                                           constraints) do-infer) do-expand) do-hoist-case) do-hoist-poly)
                                      data-elements) data-graph) namespaces) cx))]
              (if (= (first raw-result) :left)
                (do (println (str "  FAILED: " (second raw-result)))
                    (flush))

                (let [data-result (second raw-result)
                      g1 (first data-result)
                      def-lists (second data-result)]
                  (println (str "  Success: " (count def-lists) " definition lists"))
                  (println)
                  (flush)

                  ;; Step 7: Per-module coder timing
                  (println "Step 7: Per-module coder timing (ALL term modules)...")
                  (flush)
                  (let [all-bindings ((r 'hydra_lexical_graph_to_bindings) g1)
                        total-start (System/currentTimeMillis)]
                    (doseq [idx (range (count term-mods))]
                      (let [mod (nth term-mods idx)
                            defs (nth def-lists idx)
                            ;; Refresh module elements from inferred graph
                            refreshed-els ((r 'hydra_lib_maybes_cat)
                                            (mapv (fn [e]
                                                    (((r 'hydra_lib_lists_find)
                                                       (fn [b] (((r 'hydra_lib_equality_equal) (:name b)) (:name e))))
                                                     all-bindings))
                                                  (:elements mod)))
                            refreshed-mod (assoc mod :elements refreshed-els)
                            ns-str (let [ns (:namespace mod)]
                                     (if (string? ns) ns (:value ns)))
                            t0 (System/currentTimeMillis)
                            result ((((coder refreshed-mod)
                                       (mapv (fn [d] (list :term d)) defs)) cx) g1)
                            t1 (System/currentTimeMillis)]
                        (println (str "  [" idx "] " ns-str ": " (format-time (- t1 t0))
                                      (if (= (first result) :left)
                                        (str " FAILED: " (second result))
                                        (str " -> " (count (second result)) " files"))))
                        (flush)))
                    (let [total-end (System/currentTimeMillis)]
                      (println)
                      (println (str "  Total coder time: " (format-time (- total-end total-start))))
                      (println)
                      (println "=== Done ===")
                      (flush))))))))))))

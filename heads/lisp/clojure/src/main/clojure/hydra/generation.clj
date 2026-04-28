(ns hydra.generation
  "I/O wrapper for Hydra code generation in Clojure.
   Provides file I/O around the pure Either-based functions in hydra.codeGeneration.

   IMPORTANT: This module must be loaded AFTER preload/load-gen-main! has been called,
   so that all generated kernel symbols are globalized into clojure.core."
  (:require [hydra.lib.libraries :as libraries]
            [clojure.data.json :as json])
  (:import [java.io File]))

;; All hydra_* functions are globalized into clojure.core by preload at runtime,
;; but not available at compile time. Use (r 'sym) to resolve them lazily.
(defn- r [sym]
  (or @(ns-resolve 'clojure.core sym)
      (throw (RuntimeException. (str "Cannot resolve: " sym)))))

(defn bootstrap-schema-map
  "Build a schema map from the bootstrap type map."
  []
  (into {}
    (map (fn [[name typ]]
           (let [ts ((r 'hydra_scoping_f_type_to_type_scheme) typ)]
             [name ((r 'hydra_strip_deannotate_type_recursive) (:type ts))]))
         (r 'hydra_json_bootstrap_types_by_name))))

(defn bootstrap-graph
  "Create an empty graph with standard primitives (the bootstrap graph)."
  []
  ((r '->hydra_graph_graph)
    {}       ; bound_terms
    {}       ; bound_types
    {}       ; class_constraints
    #{}      ; lambda_variables
    {}       ; metadata
    (libraries/standard-library)  ; primitives
    {}       ; schema_types
    #{}))    ; type_variables

(defn empty-context
  "Create an empty Context."
  []
  ((r '->hydra_context_in_context) nil (list)))

(defn unwrap-either
  "Unwrap an Either value, throwing on Left."
  [result]
  (cond
    (= (first result) :right) (second result)
    (= (first result) :left)
    (let [err (second result)]
      (throw (RuntimeException.
               (str "Error: "
                    (if (and (record? err) (contains? (set (keys err)) :object))
                      (:object err)
                      err)))))
    :else (throw (RuntimeException. (str "Unexpected result type: " result)))))

(defn- clojure-to-hydra-json
  "Convert a Clojure value (from clojure.data.json) to a Hydra JSON value."
  [obj]
  (cond
    (nil? obj) (list :null nil)
    (boolean? obj) (list :boolean obj)
    (number? obj) (list :number (double obj))
    (string? obj) (list :string obj)
    (vector? obj) (list :array (mapv clojure-to-hydra-json obj))
    (sequential? obj) (list :array (vec (map clojure-to-hydra-json obj)))
    (map? obj) (list :object (into {} (map (fn [[k v]] [k (clojure-to-hydra-json v)]) obj)))
    :else (throw (IllegalArgumentException. (str "Unexpected JSON type: " (type obj))))))

(defn parse-json-file
  "Read a JSON file, parse to Hydra JSON value."
  [path]
  (let [obj (json/read-str (slurp path))]
    (clojure-to-hydra-json obj)))

(defn decode-module
  "Decode a single module from a JSON value."
  [bs-graph schema-map json-val]
  (let [mod-type (list :variable "hydra.packaging.Module")
        json-result (((((r 'hydra_json_decode_from_json) schema-map) "hydra.packaging.Module") mod-type) json-val)]
    (when (= (first json-result) :left)
      (throw (RuntimeException. (str "Module JSON decode error: " (second json-result)))))
    (let [term (second json-result)
          mod-result (((r 'hydra_decode_packaging_module) bs-graph) term)]
      (when (= (first mod-result) :left)
        (throw (RuntimeException. (str "Module decode error: " (second mod-result)))))
      (second mod-result))))

(defn load-modules-from-json
  "Load modules from JSON files using the bootstrap schema map."
  [base-path namespaces]
  (let [bs-graph (bootstrap-graph)
        schema-map (bootstrap-schema-map)]
    (mapv (fn [ns]
            (let [ns-str (if (string? ns) ns (:value ns))
                  file-path (str base-path "/" ((r 'hydra_codegen_namespace_to_path) ns-str) ".json")
                  json-val (parse-json-file file-path)
                  mod (decode-module bs-graph schema-map json-val)]
              (println (str "  Loaded: " ns-str))
              (flush)
              mod))
          namespaces)))

(defn read-manifest-field
  "Read a field from manifest.json as a list of Namespaces."
  [base-path field-name]
  (let [manifest-path (str base-path "/manifest.json")
        manifest (json/read-str (slurp manifest-path))]
    ;; Return plain strings — the JSON decoder produces string namespaces
    (mapv identity (get manifest field-name))))

(defn generate-sources
  "Generate source files and write them to disk."
  [coder language do-infer do-expand do-hoist-case do-hoist-poly
   base-path universe modules-to-generate]
  (let [bs-graph (bootstrap-graph)
        cx (empty-context)
        t0 (System/currentTimeMillis)
        _ (println (str "  [gen] Starting generate_source_files at " (java.time.Instant/now)))
        _ (flush)
        result (((((((((((r 'hydra_codegen_generate_source_files)
                          coder) language) do-infer) do-expand) do-hoist-case) do-hoist-poly)
                     bs-graph) universe) modules-to-generate) cx)
        files (unwrap-either result)
        t1 (System/currentTimeMillis)]
    (println (str "  Code generation took " (/ (- t1 t0) 1000.0) "s for " (count files) " files"))
    (flush)
    (doseq [[path content] files]
      (let [file-path (str base-path "/" path)
            content (if (.endsWith ^String content "\n") content (str content "\n"))
            parent (.getParentFile (File. ^String file-path))]
        (.mkdirs parent)
        (spit file-path content)))
    (count files)))

(defn- ns-str-of [m]
  (let [ns (:namespace m)]
    (if (string? ns) ns (:value ns))))

(defn filter-kernel-modules
  "Filter modules to only kernel modules (exclude hydra.* namespaces)."
  [modules]
  (filterv (fn [m]
             (let [ns-str (ns-str-of m)]
               (and (not (.startsWith ^String ns-str "hydra."))
                    (not (.startsWith ^String ns-str "hydra.json.yaml.")))))
           modules))

(defn filter-type-modules
  "Filter modules to only those containing type-defining bindings."
  [modules]
  (filterv (fn [m]
             (some (r 'hydra_annotations_is_native_type) (:definitions m)))
           modules))

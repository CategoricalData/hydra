(ns hydra.bootstrap
  "Bootstrapping entry point: loads Hydra modules from JSON and generates
   code for a target language. Demonstrates that Clojure can independently
   regenerate Hydra from a language-independent JSON representation.

   Usage:
     clojure -M -m hydra.bootstrap --target <target> --json-dir <path> [OPTIONS]

   Options:
     --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
     --include-tests        Also load and generate kernel test modules
     --kernel-only          Only generate kernel modules (exclude hydra.ext.*)
     --types-only           Only generate type-defining modules"
  (:require [hydra.lib.preload :as preload])
  (:gen-class))

(defn- format-time [millis]
  (cond
    (< millis 1000) (str millis "ms")
    (< millis 60000) (format "%.1fs" (/ millis 1000.0))
    :else (let [mins (quot millis 60000)
                secs (/ (mod millis 60000) 1000.0)]
            (format "%dm %.1fs" mins secs))))

(defn- parse-args [args]
  (loop [args (seq args)
         opts {:target nil
               :json-dir nil
               :output "/tmp/hydra-bootstrapping-demo"
               :include-tests false
               :types-only false
               :kernel-only false}]
    (if-not args
      opts
      (let [[arg & rest] args]
        (case arg
          "--target" (recur (next rest) (assoc opts :target (first rest)))
          "--json-dir" (recur (next rest) (assoc opts :json-dir (first rest)))
          "--output" (recur (next rest) (assoc opts :output (first rest)))
          "--include-tests" (recur rest (assoc opts :include-tests true))
          "--types-only" (recur rest (assoc opts :types-only true))
          "--kernel-only" (recur rest (assoc opts :kernel-only true))
          (recur rest opts))))))

(defn- rc
  "Resolve a symbol from clojure.core (where generated vars are globalized).
   Unlike resolve, this always finds vars interned after namespace creation."
  [sym]
  (or (ns-resolve 'clojure.core sym)
      (throw (RuntimeException. (str "Cannot resolve: " sym)))))

(defn- load-coder-modules!
  "Load a list of coder namespaces in dependency order.
   Pre-declares all symbols across all modules first, then loads each module."
  [ns-names]
  ;; Phase 1: Pre-declare all symbols and create all namespaces
  (doseq [ns-name ns-names]
    (preload/pre-declare-ns-symbols! ns-name)
    (let [ns-sym (symbol ns-name)
          the-ns (or (find-ns ns-sym) (create-ns ns-sym))]
      (preload/refer-clojure-into-ns! the-ns)))
  ;; Phase 2: Load and globalize each module in order
  (doseq [ns-name ns-names]
    (preload/require-and-globalize! ns-name)))

(defn- resolve-coder
  "Resolve the coder function and language for a given target.
   The coder modules must already be available on the classpath."
  [target]
  (case target
    "haskell"
    (do (load-coder-modules!
          ["hydra.ext.haskell.ast" "hydra.ext.haskell.language"
           "hydra.ext.haskell.operators" "hydra.ext.haskell.utils"
           "hydra.ext.haskell.serde" "hydra.ext.haskell.coder"])
        {:coder @(rc 'hydra_ext_haskell_coder_module_to_haskell)
         :language @(rc 'hydra_ext_haskell_language_haskell_language)
         :flags [false false false false]
         :subdir "haskell"})
    "java"
    (do (load-coder-modules!
          ["hydra.ext.java.syntax" "hydra.ext.java.language" "hydra.ext.java.names"
           "hydra.ext.java.environment" "hydra.ext.java.utils"
           "hydra.ext.java.serde" "hydra.ext.java.coder"])
        {:coder @(rc 'hydra_ext_java_coder_module_to_java)
         :language @(rc 'hydra_ext_java_language_java_language)
         :flags [false true false true]
         :subdir "java"})
    "python"
    (do (load-coder-modules!
          ["hydra.ext.python.syntax" "hydra.ext.python.language" "hydra.ext.python.names"
           "hydra.ext.python.environment" "hydra.ext.python.utils"
           "hydra.ext.python.serde" "hydra.ext.python.coder"])
        (preload/install-coder-performance-patches!)
        {:coder @(rc 'hydra_ext_python_coder_module_to_python)
         :language @(rc 'hydra_ext_python_language_python_language)
         :flags [false true true false]
         :subdir "python"})
    ("clojure" "scheme" "common-lisp" "emacs-lisp")
    (do (load-coder-modules!
          ["hydra.ext.lisp.syntax" "hydra.ext.lisp.language"
           "hydra.ext.lisp.serde" "hydra.ext.lisp.coder"])
        (let [module-to-lisp @(rc 'hydra_ext_lisp_coder_module_to_lisp)
              program-to-expr @(rc 'hydra_ext_lisp_serde_program_to_expr)
              lang @(rc 'hydra_ext_lisp_language_lisp_language)
              dialect (case target
                        "clojure"    (list :clojure nil)
                        "scheme"     (list :scheme nil)
                        "common-lisp" (list :common_lisp nil)
                        "emacs-lisp" (list :emacs_lisp nil))
              ext (case target
                    "clojure"    "clj"
                    "scheme"     "scm"
                    "common-lisp" "lisp"
                    "emacs-lisp" "el")
              subdir (case target
                       "clojure"    "clojure"
                       "scheme"     "scheme"
                       "common-lisp" "common-lisp"
                       "emacs-lisp" "emacs-lisp")
              coder (fn [mod] (fn [defs] (fn [cx] (fn [g]
                      (let [result (((((module-to-lisp dialect) mod) defs) cx) g)]
                        (if (= (first result) :left)
                          result
                          (let [program (second result)
                                code (@(rc 'hydra_serialization_print_expr)
                                       (@(rc 'hydra_serialization_parenthesize)
                                         (program-to-expr program)))
                                ns-val (let [ns (:namespace mod)]
                                         (if (string? ns) ns (:value ns)))
                                case-conv (if (= target "clojure")
                                           (list :camel nil)
                                           (list :lower_snake nil))
                                file-path (((@(rc 'hydra_names_namespace_to_file_path)
                                              case-conv) ext) ns-val)]
                            (list :right {file-path code}))))))))]
          {:coder coder
           :language lang
           ;; do_infer=false to avoid hydra.adapt mutual recursion issue in Clojure
           :flags [false false false false]
           :subdir subdir}))
    ;; Default: unsupported
    (do (println (str "Unsupported target for Clojure host: " target))
        (println "Supported targets: haskell, java, python, clojure, scheme, common-lisp, emacs-lisp")
        (System/exit 1))))

(defn -main [& args]
  ;; Load generated kernel modules (includes encode/decode and JSON modules)
  (preload/ensure-libs-loaded!)
  (preload/load-gen-main!)

  (let [total-start (System/currentTimeMillis)
        opts (parse-args args)
        target (:target opts)
        json-dir (:json-dir opts)]

    (when (or (nil? target) (nil? json-dir))
      (println "Usage: clojure -M -m hydra.bootstrap --target <target> --json-dir <path> [OPTIONS]")
      (println)
      (println "Options:")
      (println "  --output <dir>         Output base directory")
      (println "  --include-tests        Also load and generate kernel test modules")
      (println "  --kernel-only          Only generate kernel modules (exclude hydra.ext.*)")
      (println "  --types-only           Only generate type-defining modules")
      (System/exit 1))

    ;; Load generation module (after preload has globalized kernel symbols)
    (require 'hydra.generation)
    (let [gen-ns (find-ns 'hydra.generation)
          read-manifest @(ns-resolve gen-ns 'read-manifest-field)
          load-mods @(ns-resolve gen-ns 'load-modules-from-json)
          gen-sources @(ns-resolve gen-ns 'generate-sources)]

      (let [target-cap (str (.toUpperCase (subs target 0 1)) (subs target 1))
            out-dir (str (:output opts) "/clojure-to-" target)
            coder-info (resolve-coder target)
            [do-infer do-expand do-hoist-case do-hoist-poly] (:flags coder-info)]

        (println "==========================================")
        (println (str "Mapping JSON to " target-cap " (via Clojure host)"))
        (println "==========================================")
        (println (str "  Host language:   Clojure"))
        (println (str "  Target language: " target-cap))
        (println (str "  JSON directory:  " json-dir))
        (println (str "  Output:          " out-dir))
        (println (str "  Include tests:   " (:include-tests opts)))
        (when (:types-only opts) (println "  Filter:          types only"))
        (when (:kernel-only opts) (println "  Filter:          kernel only"))
        (println "==========================================")
        (println)
        (flush)

        ;; Step 1: Load main + eval lib modules
        (println "Step 1: Loading main modules from JSON...")
        (println (str "  Source: " json-dir))
        (flush)
        (let [step-start (System/currentTimeMillis)
              main-ns (read-manifest json-dir "mainModules")
              eval-ns (read-manifest json-dir "evalLibModules")
              all-kernel-ns (into (vec main-ns) eval-ns)
              main-mods (load-mods false json-dir all-kernel-ns)
              step-time (- (System/currentTimeMillis) step-start)
              total-bindings (reduce + 0 (map #(count (:definitions %)) main-mods))]
          (println (str "  Loaded " (count main-mods) " modules (" total-bindings " bindings)."))
          (println (str "  Time: " (format-time step-time)))
          (println)
          (flush)

          (let [;; Apply filters
                all-main-mods main-mods
                ;; Namespace may be a string or a record with :value
                ns-str-of (fn [m] (let [ns (:namespace m)]
                                    (if (string? ns) ns (:value ns))))
                mods-to-generate (cond->> all-main-mods
                                   (:kernel-only opts) (filterv (fn [m]
                                     (let [ns-str (ns-str-of m)]
                                       (and (not (.startsWith ^String ns-str "hydra.ext."))
                                            (not (.startsWith ^String ns-str "hydra.json.yaml."))))))
                                   (:types-only opts) (filterv (fn [m]
                                     (some (ns-resolve (find-ns 'hydra.annotations) 'hydra_annotations_is_native_type) (:elements m)))))]

            (when (:kernel-only opts)
              (println (str "Filtering to kernel modules: " (count mods-to-generate) " of " (count all-main-mods)))
              (println)
              (flush))

            ;; Generate main modules
            (let [out-main (str out-dir "/src/gen-main/" (:subdir coder-info))]
              (println (str "Mapping " (count mods-to-generate) " modules to " target-cap "..."))
              (println (str "  Universe: " (count all-main-mods) " modules"))
              (println (str "  Output: " out-main))
              (println)
              (flush)

              (let [step-start2 (System/currentTimeMillis)
                    main-file-count (gen-sources (:coder coder-info) (:language coder-info)
                                                 do-infer do-expand do-hoist-case do-hoist-poly
                                                 out-main all-main-mods mods-to-generate)
                    step-time2 (- (System/currentTimeMillis) step-start2)]
                (println (str "  Generated " main-file-count " files."))
                (println (str "  Time: " (format-time step-time2)))
                (println)
                (flush)

                ;; Optionally generate test modules
                (let [test-file-count
                      (if (:include-tests opts)
                        (let [test-json-dir (.replace ^String json-dir "gen-main/json" "gen-test/json")
                              _ (println "Loading test modules from JSON...")
                              _ (flush)
                              test-ns (read-manifest json-dir "testModules")
                              test-mods (load-mods false test-json-dir test-ns)
                              all-universe (into (vec main-mods) test-mods)
                              out-test (str out-dir "/src/gen-test/" (:subdir coder-info))]
                          (println (str "  Loaded " (count test-mods) " test modules."))
                          (println)
                          (println (str "Mapping test modules to " target-cap "..."))
                          (flush)
                          (let [step-start3 (System/currentTimeMillis)
                                count (gen-sources (:coder coder-info) (:language coder-info)
                                                    do-infer do-expand do-hoist-case do-hoist-poly
                                                    out-test all-universe test-mods)
                                step-time3 (- (System/currentTimeMillis) step-start3)]
                            (println (str "  Generated " count " test files."))
                            (println (str "  Time: " (format-time step-time3)))
                            (println)
                            (flush)
                            count))
                        0)
                      total-time (- (System/currentTimeMillis) total-start)]

                  (println "==========================================")
                  (println (str "Done: " main-file-count " main"
                                (when (:include-tests opts) (str " + " test-file-count " test"))
                                " files"))
                  (println (str "  Output: " out-dir))
                  (println (str "  Total time: " (format-time total-time)))
                  (println "==========================================")
                  (flush))))))))))

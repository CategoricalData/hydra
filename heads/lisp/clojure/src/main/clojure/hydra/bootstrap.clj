(ns hydra.bootstrap
  "Bootstrapping entry point: loads Hydra modules from JSON and generates
   code for a target language. Demonstrates that Clojure can independently
   regenerate Hydra from a language-independent JSON representation.

   Usage:
     clojure -M -m hydra.bootstrap --target <target> --json-dir <path> [OPTIONS]

   Options:
     --output <dir>         Output base directory (default: /tmp/hydra-bootstrapping-demo)
     --include-tests        Also load and generate kernel test modules
     --kernel-only          Only generate kernel modules (those listed in the kernel JSON manifest)
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

;; #473 Step 0 — lib pass + redirect (mirrors the Java/Python/Scala host drivers +
;; bootstrap-from-json/Main.hs). hydra.lib.* primitive IMPLEMENTATIONS were relocated to
;; hydra.<lang>.lib.*; hydra.lib.* is free for the generated PrimitiveDefinition def-modules. A host
;; generating a def-module-consuming target (everything except haskell) must emit the hydra.lib.*
;; def-modules (lib pass) and redirect generated consumer call-sites to hydra.<lang>.lib.* (redirect).
;; See project_473_self_host_lib_pass_gap.
(def ^:private lib-subs
  ["chars" "eithers" "equality" "lists" "literals" "logic" "maps"
   "math" "optionals" "pairs" "regex" "sets" "strings"])

(defn- lib-module? [m]
  (let [ns (:name m)] (.startsWith ^String (if (string? ns) ns (:value ns)) "hydra.lib.")))

(defn- run-lib-pass
  "Emit the hydra.lib.* def-modules from their LOWERED form using the same coder. Mirrors
   genForDirLib: lib modules lowered; universe lowers ONLY lib modules."
  [gen-sources coder-info do-infer out-main all-main-mods mods-to-generate]
  ;; The lowering fn is a generated kernel var globalized into clojure.core (same resolution as `rc`).
  (let [lower @(ns-resolve 'clojure.core 'hydra_codegen_lower_primitive_definitions)
        lib-mods (mapv lower (filterv lib-module? mods-to-generate))]
    (when (seq lib-mods)
      (let [lib-universe (mapv #(if (lib-module? %) (lower %) %) all-main-mods)]
        (println (str "Lib pass: emitting " (count lib-mods) " hydra.lib.* definition modules..."))
        (gen-sources (:coder coder-info) (:language coder-info) do-infer
                     out-main lib-universe lib-mods)))))

;; The Clojure coder emits consumer `(:require [hydra.lib.<sub> :refer :all])` and flat call sites
;; `hydra_lib_<sub>_<fn>` (resolved via that :refer). The relocation is driver-side: rewrite the require
;; namespace hydra.lib.<sub> -> hydra.overlay.clojure.lib.<sub> (the relocated impls). The flat call identifiers
;; stay (resolved via the relocated :refer :all). Def-modules at hydra/lib/ keep canonical hydra.lib.*.
(defn- all-files-under [^java.io.File dir]
  (if (.isDirectory dir)
    (mapcat (fn [^java.io.File f] (all-files-under f)) (.listFiles dir))
    [dir]))

(defn- lib-def-file?
  "Files under hydra/lib/ are the lib-pass def-modules (must keep canonical hydra.lib.*) or the
   hand-written registry; never redirect these."
  [^java.io.File f]
  (.contains (.replace (.getPath f) java.io.File/separatorChar \/) "/hydra/lib/"))

(defn- redirect-dotted
  "Rewrite consumer require namespaces hydra.lib.<sub> -> hydra.overlay.clojure.lib.<sub>, protecting quoted
   primitive-NAME strings (\"hydra.lib...\")."
  [^String s]
  (let [sentinel "@@HYDRA_LIB_NAME@@"
        protected (.replace s "\"hydra.lib." (str "\"" sentinel))
        rewritten (reduce (fn [^String acc sub]
                            (.replace acc (str "hydra.lib." sub) (str "hydra.overlay.clojure.lib." sub)))
                          protected
                          lib-subs)]
    (.replace rewritten sentinel "hydra.lib.")))

(defn- redirect-lib-calls
  "#473 redirect over a generated dir (Clojure self-host only)."
  [lang-dir]
  (let [dir (java.io.File. ^String lang-dir)]
    (when (.isDirectory dir)
      (doseq [^java.io.File f (all-files-under dir)]
        (when-not (lib-def-file? f)
          (let [s (slurp f)]
            (when (.contains s "hydra.lib.")
              (let [out (redirect-dotted s)]
                (when (not= out s) (spit f out))))))))))

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

(defn- dist-json-root-of
  "Strip a legacy <pkg>/src/main/json suffix to recover the dist/json root.
   Callers historically pass dist/json/hydra-kernel/src/main/json as --json-dir;
   we need the parent dist/json/ to find sibling packages."
  [json-dir]
  (let [trimmed (.replaceAll ^String json-dir "/+$" "")]
    (if (.endsWith ^String trimmed "/src/main/json")
      (let [without-suffix (subs trimmed 0 (- (count trimmed) (count "/src/main/json")))
            slash (.lastIndexOf ^String without-suffix "/")]
        (if (>= slash 0) (subs without-suffix 0 slash) "."))
      trimmed)))

(defn- load-package-main
  "Load mainModules ∪ defaultLibModules from <root>/<pkg>/src/main/json.
   Returns [] if the package's manifest.json does not exist."
  [root pkg read-manifest load-mods]
  (let [pkg-dir (str root "/" pkg "/src/main/json")
        manifest-path (str pkg-dir "/manifest.json")]
    (if-not (.exists (java.io.File. ^String manifest-path))
      []
      (let [main-ns (vec (read-manifest pkg-dir "mainModules"))
            default-ns (vec (read-manifest pkg-dir "defaultLibModules"))
            all-ns (into main-ns default-ns)]
        (when (seq all-ns)
          (println (str "  " pkg ": " (count all-ns) " modules from " pkg-dir))
          (flush))
        (load-mods pkg-dir all-ns)))))

(defn- load-coder-modules!
  "Load a coder root namespace plus all of its transitive :require deps in
   topological order. Pre-declares all symbols across all modules first, then
   loads each module. Already-loaded nses (e.g. anything from the main kernel
   preload pass) are skipped automatically by the topo walk."
  [root-ns-names]
  (let [ns-names (preload/coder-load-order root-ns-names)
        already-loaded? (fn [n] (some? (find-ns (symbol n))))
        ;; Skip nses already loaded by the main preload pass — re-running their
        ;; defs would re-evaluate and risk clobbering globalized state. The
        ;; coder-specific nses (e.g. hydra.haskell.coder) are loaded fresh.
        new-ns-names (vec (remove already-loaded? ns-names))]
    ;; Phase 1: Pre-declare all symbols and create all namespaces
    (doseq [ns-name new-ns-names]
      (preload/pre-declare-ns-symbols! ns-name)
      (let [ns-sym (symbol ns-name)
            the-ns (or (find-ns ns-sym) (create-ns ns-sym))]
        (preload/refer-clojure-into-ns! the-ns)))
    ;; Phase 2: Load and globalize each module in order
    (doseq [ns-name new-ns-names]
      (preload/require-and-globalize! ns-name))))

(defn- resolve-coder
  "Resolve the coder function and language for a given target.
   The coder modules must already be available on the classpath."
  [target]
  (case target
    "haskell"
    (do (load-coder-modules! ["hydra.haskell.coder"])
        {:coder @(rc 'hydra_haskell_coder_module_to_haskell)
         :language @(rc 'hydra_haskell_language_haskell_language)
         :flags [false false false false]
         :subdir "haskell"})
    "java"
    (do (load-coder-modules! ["hydra.java.coder"])
        {:coder @(rc 'hydra_java_coder_module_to_java)
         :language @(rc 'hydra_java_language_java_language)
         :flags [false true false true]
         :subdir "java"})
    "python"
    (do (load-coder-modules! ["hydra.python.coder"])
        (preload/install-coder-performance-patches!)
        {:coder @(rc 'hydra_python_coder_module_to_python)
         :language @(rc 'hydra_python_language_python_language)
         :flags [false true true false]
         :subdir "python"})
    "scala"
    (do (load-coder-modules! ["hydra.scala.coder"])
        {:coder @(rc 'hydra_scala_coder_module_to_scala)
         :language @(rc 'hydra_scala_language_scala_language)
         :flags [false false false false]
         :subdir "scala"})
    "typescript"
    ;; Namespace is hydra.typeScript (camelCase S); the lisp coder lowers
    ;; identifier segments to snake_case, so vars are hydra_type_script_*.
    ;; CLI target stays "typescript".
    (do (load-coder-modules! ["hydra.typeScript.coder"])
        {:coder @(rc 'hydra_type_script_coder_module_to_type_script)
         :language @(rc 'hydra_type_script_language_type_script_language)
         :flags [false false false false]
         :subdir "typescript"})
    ("clojure" "scheme" "common-lisp" "emacs-lisp")
    ;; hydra.lisp.serde is *not* reachable from hydra.lisp.coder's :require graph
    ;; (the coder builds a program; serde converts the program to printable expr).
    ;; Pass both as roots so the transitive walk pulls in serde's own deps too.
    (do (load-coder-modules! ["hydra.lisp.coder" "hydra.lisp.serde"])
        (let [module-to-lisp @(rc 'hydra_lisp_coder_module_to_lisp)
              program-to-expr @(rc 'hydra_lisp_serde_program_to_expr)
              lang @(rc 'hydra_lisp_language_lisp_language)
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
                                ns-val (let [ns (:name mod)]
                                         (if (string? ns) ns (:value ns)))
                                case-conv (if (= target "clojure")
                                           (list :camel nil)
                                           (list :lower_snake nil))
                                file-path (((@(rc 'hydra_names_module_name_to_file_path)
                                              case-conv) ext) ns-val)]
                            (list :right {file-path code}))))))))]
          {:coder coder
           :language lang
           ;; do_infer=false to avoid hydra.adapt mutual recursion issue in Clojure
           :flags [false false false false]
           :subdir subdir}))
    ;; Default: unsupported
    (do (println (str "Unsupported target for Clojure host: " target))
        (println "Supported targets: haskell, java, python, scala, typescript, clojure, scheme, common-lisp, emacs-lisp")
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
      (println "  --kernel-only          Only generate kernel modules (those listed in the kernel JSON manifest)")
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

        ;; Step 1: Load baseline packages (hydra-kernel + hydra-haskell), mirroring
        ;; the Scala, Python, and CL hosts. The hydra-haskell package supplies the
        ;; runtime AST modules (Hydra.Haskell.Syntax, .Environment, etc.) that the
        ;; generated DSL source modules import. Without it the Haskell rebuild step
        ;; fails on dangling imports. (#487; CL-host analog: #460.)
        (println "Step 1: Loading baseline main modules from JSON...")
        (flush)
        (let [step-start (System/currentTimeMillis)
              dist-json-root (dist-json-root-of json-dir)
              kernel-mods (load-package-main dist-json-root "hydra-kernel" read-manifest load-mods)
              haskell-mods (load-package-main dist-json-root "hydra-haskell" read-manifest load-mods)
              main-mods (vec (concat kernel-mods haskell-mods))
              ns-str-of-mod (fn [m] (let [n (:name m)] (if (string? n) n (:value n))))
              kernel-ns-set (set (map ns-str-of-mod main-mods))
              step-time (- (System/currentTimeMillis) step-start)
              total-bindings (reduce + 0 (map #(count (:definitions %)) main-mods))]
          (println (str "  Loaded " (count main-mods) " baseline modules (" total-bindings " bindings)."))
          (println (str "  Time: " (format-time step-time)))
          (println)
          (flush)

          (let [;; Apply filters
                all-main-mods main-mods
                ;; Module name may be a bare string or a ModuleName record with :value
                ns-str-of (fn [m] (let [ns (:name m)]
                                    (if (string? ns) ns (:value ns))))
                ;; kernel-only keeps modules whose namespace is in the kernel
                ;; manifest (mainModules ∪ defaultLibModules). When the source
                ;; dir is already hydra-kernel/, this is an identity filter; it
                ;; becomes useful when called against a mixed JSON source.
                mods-to-generate (cond->> all-main-mods
                                   (:kernel-only opts) (filterv (fn [m]
                                     (contains? kernel-ns-set (ns-str-of m))))
                                   (:types-only opts) (filterv (fn [m]
                                     (some (ns-resolve (find-ns 'hydra.annotations) 'hydra_annotations_is_native_type) (:definitions m)))))]

            (when (:kernel-only opts)
              (println (str "Filtering to kernel modules: " (count mods-to-generate) " of " (count all-main-mods)))
              (println)
              (flush))

            ;; Generate main modules
            (let [out-main (str out-dir "/src/main/" (:subdir coder-info))]
              (println (str "Mapping " (count mods-to-generate) " modules to " target-cap "..."))
              (println (str "  Universe: " (count all-main-mods) " modules"))
              (println (str "  Output: " out-main))
              (println)
              (flush)

              (let [step-start2 (System/currentTimeMillis)
                    main-file-count (gen-sources (:coder coder-info) (:language coder-info)
                                                 do-infer
                                                 out-main all-main-mods mods-to-generate)
                    step-time2 (- (System/currentTimeMillis) step-start2)]
                (println (str "  Generated " main-file-count " files."))
                (println (str "  Time: " (format-time step-time2)))
                (println)
                (flush)

                ;; #473 lib pass: emit the hydra.lib.* def-modules now (redirect runs LAST, below).
                (when (not= target "haskell")
                  (run-lib-pass gen-sources coder-info do-infer out-main all-main-mods mods-to-generate))

                ;; Optionally generate test modules
                (let [test-file-count
                      (if (:include-tests opts)
                        (let [test-json-dir (.replace ^String json-dir "src/main/json" "src/test/json")
                              _ (println "Loading test modules from JSON...")
                              _ (flush)
                              test-ns (read-manifest json-dir "testModules")
                              test-mods (load-mods test-json-dir test-ns)
                              all-universe (into (vec main-mods) test-mods)
                              ;; Filter skip-emit test namespaces (e.g.
                              ;; hydra.test.testEnv): these are type-only stubs
                              ;; whose hand-written per-language counterparts
                              ;; are the source of truth. Mirrors
                              ;; testSkipEmitModuleNames in
                              ;; Hydra.Sources.Test.All and the equivalent
                              ;; filter in heads/python/.../bootstrap.py.
                              ns-of (fn [m] (let [n (:name m)] (if (string? n) n (:value n))))
                              test-skip-emit #{"hydra.test.testEnv"}
                              test-mods-to-emit (filterv (fn [m] (not (contains? test-skip-emit (ns-of m)))) test-mods)
                              out-test (str out-dir "/src/test/" (:subdir coder-info))]
                          (println (str "  Loaded " (count test-mods) " test modules."))
                          (println)
                          (println (str "Mapping test modules to " target-cap "..."))
                          (flush)
                          (let [step-start3 (System/currentTimeMillis)
                                count (gen-sources (:coder coder-info) (:language coder-info)
                                                    do-infer
                                                    out-test all-universe test-mods-to-emit)
                                step-time3 (- (System/currentTimeMillis) step-start3)]
                            (println (str "  Generated " count " test files."))
                            (println (str "  Time: " (format-time step-time3)))
                            (println)
                            (flush)
                            count))
                        0)
                      ;; #473 redirect — run LAST over every generated dir (main + test) so consumer
                      ;; require namespaces hydra.lib.* are rewritten to hydra.overlay.clojure.lib.*.
                      _ (when (= target "clojure")
                          (redirect-lib-calls (str out-dir "/src/main/" (:subdir coder-info)))
                          (when (:include-tests opts)
                            (redirect-lib-calls (str out-dir "/src/test/" (:subdir coder-info)))))
                      total-time (- (System/currentTimeMillis) total-start)]

                  (println "==========================================")
                  (println (str "Done: " main-file-count " main"
                                (when (:include-tests opts) (str " + " test-file-count " test"))
                                " files"))
                  (println (str "  Output: " out-dir))
                  (println (str "  Total time: " (format-time total-time)))
                  (println "==========================================")
                  (flush))))))))))

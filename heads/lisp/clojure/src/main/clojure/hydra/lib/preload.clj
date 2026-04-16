(ns hydra.lib.preload
  "Pre-creates generated Clojure namespaces with clojure.core referred
   so that generated code compiles when loaded.
   Also globalizes lib symbols so cross-module references resolve."
  (:require [clojure.string :as str]))

(def ^:private lib-ns-names
  "Library namespaces whose public vars should be globalized."
  ["hydra.lib.chars" "hydra.lib.eithers" "hydra.lib.equality"
   "hydra.lib.lists" "hydra.lib.literals" "hydra.lib.logic"
   "hydra.lib.maps" "hydra.lib.math" "hydra.lib.maybes"
   "hydra.lib.pairs" "hydra.lib.sets" "hydra.lib.strings"])

(defn globalize-ns-vars!
  "Intern all public vars from the given namespace into clojure.core,
   making them available in all namespaces without explicit requires.
   Globalizes both hydra_ prefixed vars and -> constructors."
  [ns-sym]
  (when-let [the-ns (find-ns ns-sym)]
    (doseq [[sym var] (ns-publics the-ns)]
      (when (and (.isBound var)
                 (or (.startsWith (name sym) "hydra_")
                     (.startsWith (name sym) "->hydra_")
                     (.startsWith (name sym) "make-hydra_")))
        (intern 'clojure.core sym (deref var))))))

(defn globalize-all-lib-vars!
  "Load all lib namespaces and intern their hydra_ vars into clojure.core."
  []
  (doseq [ns-name lib-ns-names]
    (require (symbol ns-name))
    (globalize-ns-vars! (symbol ns-name)))
  ;; Also globalize equal? which is used in generated code
  (intern 'clojure.core 'equal? (fn [a b] (= a b))))

(defn globalize-gen-ns!
  "After loading a generated namespace, intern its hydra_ vars into clojure.core
   so subsequent modules can reference them."
  [ns-sym]
  (globalize-ns-vars! ns-sym))

(def ^:private gen-ns-names
  "All generated namespaces to pre-create."
  ["hydra.core" "hydra.error.checking" "hydra.error.core" "hydra.error.module"
   "hydra.error" "hydra.context"
   "hydra.graph" "hydra.module" "hydra.phantoms"
   "hydra.coders" "hydra.ast" "hydra.testing"
   "hydra.typing" "hydra.query" "hydra.relational" "hydra.tabular"
   "hydra.classes" "hydra.topology" "hydra.parsing"
   "hydra.variants" "hydra.paths" "hydra.util" "hydra.constants"
   "hydra.literals" "hydra.reflect" "hydra.formatting"
   "hydra.serialization" "hydra.names" "hydra.languages" "hydra.parsers"
   "hydra.sorting" "hydra.tarjan" "hydra.rewriting"
   "hydra.show.core" "hydra.show.graph" "hydra.show.typing"
   "hydra.show.meta" "hydra.show.paths" "hydra.show.util"
   "hydra.show.error.core" "hydra.show.errors"
   "hydra.lexical" "hydra.substitution"
   "hydra.extract.helpers" "hydra.extract.core" "hydra.extract.util"
   "hydra.extract.json"
   "hydra.unification" "hydra.validate.core"
   "hydra.encode.core" "hydra.encode.paths"
   "hydra.encode.context" "hydra.encode.ast" "hydra.encode.classes"
   "hydra.encode.coders"
   "hydra.encode.module" "hydra.encode.phantoms" "hydra.encode.query"
   "hydra.encode.relational"
   "hydra.encode.tabular" "hydra.encode.testing" "hydra.encode.typing"
   "hydra.encode.topology" "hydra.encode.util" "hydra.encode.variants"
   "hydra.encode.parsing" "hydra.encode.json.model"
   "hydra.encode.error.checking" "hydra.encode.error.core" "hydra.encode.errors"
   "hydra.decode.core" "hydra.decode.paths"
   "hydra.decode.context" "hydra.decode.ast" "hydra.decode.classes"
   "hydra.decode.coders"
   "hydra.decode.module" "hydra.decode.phantoms" "hydra.decode.query"
   "hydra.decode.relational"
   "hydra.decode.tabular" "hydra.decode.testing" "hydra.decode.typing"
   "hydra.decode.topology" "hydra.decode.util" "hydra.decode.variants"
   "hydra.decode.parsing" "hydra.decode.json.model"
   "hydra.decode.error.checking" "hydra.decode.error.core" "hydra.decode.errors"
   "hydra.annotations" "hydra.templates"
   "hydra.schemas" "hydra.json.model" "hydra.json.bootstrap"
   "hydra.checking" "hydra.decoding" "hydra.encoding" "hydra.hoisting"
   "hydra.arity" "hydra.reduction"
   "hydra.inference"
   "hydra.adapt"
   "hydra.json.decode" "hydra.json.encode" "hydra.json.parser"
   "hydra.json.writer" "hydra.json.yaml.decode" "hydra.json.yaml.encode"
   "hydra.json.decoding"
   "hydra.yaml.model"
   "hydra.haskell.operators"
   "hydra.codeGeneration" "hydra.coderUtils"
   "hydra.lib.names"
   "hydra.test.transform" "hydra.test.utils"
   ;; Test namespaces
   "hydra.test.test_graph" "hydra.test.test_terms" "hydra.test.test_types"
   "hydra.test.lib.chars" "hydra.test.lib.eithers" "hydra.test.lib.equality"
   "hydra.test.lib.lists" "hydra.test.lib.literals" "hydra.test.lib.logic"
   "hydra.test.lib.maps" "hydra.test.lib.math" "hydra.test.lib.maybes"
   "hydra.test.lib.pairs" "hydra.test.lib.regex" "hydra.test.lib.sets" "hydra.test.lib.strings"
   "hydra.test.annotations" "hydra.test.reduction" "hydra.test.rewriting"
   "hydra.test.formatting" "hydra.test.serialization" "hydra.test.sorting"
   "hydra.test.substitution" "hydra.test.eta_expansion"
   "hydra.test.checking.all" "hydra.test.hoisting.all"
   "hydra.test.inference.all" "hydra.test.unification"
   "hydra.test.json.coder" "hydra.test.json.parser"
   "hydra.test.json.roundtrip" "hydra.test.json.writer"
   "hydra.test.json.yaml"
   "hydra.test.validate.all" "hydra.test.validate.core"
   "hydra.test.testSuite" "hydra.test.test_suite"])

(def ^:private gen-main-load-order
  "Generated main namespaces in dependency order.
   Each module is loaded and its vars globalized before the next."
  [;; Core types (no cross-module term references)
   "hydra.core" "hydra.error.checking" "hydra.error.core" "hydra.error.module"
   "hydra.error" "hydra.context"
   "hydra.graph" "hydra.module" "hydra.phantoms" "hydra.coders"
   "hydra.ast" "hydra.testing" "hydra.typing"
   "hydra.query" "hydra.relational" "hydra.tabular"
   "hydra.classes" "hydra.topology" "hydra.parsing" "hydra.variants"
   "hydra.paths" "hydra.util" "hydra.constants" "hydra.json.model"
   ;; Core operations (topological order based on cross-module deps)
   "hydra.formatting" "hydra.reflect" "hydra.names" "hydra.lib.names"
   "hydra.sorting" "hydra.tarjan" "hydra.rewriting"
   "hydra.show.core" "hydra.show.error.core" "hydra.show.errors"
   "hydra.show.meta" "hydra.show.typing"
   "hydra.show.graph" "hydra.show.paths" "hydra.show.util"
   "hydra.lexical"
   "hydra.extract.core" "hydra.extract.helpers" "hydra.extract.util"
   "hydra.extract.json"
   "hydra.encode.core" "hydra.decode.core"
   "hydra.annotations" "hydra.substitution" "hydra.validate.core"
   "hydra.schemas" "hydra.literals"
   "hydra.arity"
   "hydra.hoisting"
   "hydra.checking" "hydra.unification" "hydra.inference"
   "hydra.serialization" "hydra.coderUtils"
   ;; Encode/decode (needed by codeGeneration)
   "hydra.encode.paths" "hydra.encode.context" "hydra.encode.ast"
   "hydra.encode.classes" "hydra.encode.coders"
   "hydra.encode.module" "hydra.encode.phantoms" "hydra.encode.query"
   "hydra.encode.relational" "hydra.encode.tabular" "hydra.encode.testing"
   "hydra.encode.typing" "hydra.encode.topology" "hydra.encode.util"
   "hydra.encode.variants" "hydra.encode.parsing" "hydra.encode.json.model"
   "hydra.encode.error.checking" "hydra.encode.error.core" "hydra.encode.errors"
   "hydra.decode.paths" "hydra.decode.context" "hydra.decode.ast"
   "hydra.decode.classes" "hydra.decode.coders"
   "hydra.decode.module" "hydra.decode.phantoms" "hydra.decode.query"
   "hydra.decode.relational" "hydra.decode.tabular" "hydra.decode.testing"
   "hydra.decode.typing" "hydra.decode.topology" "hydra.decode.util"
   "hydra.decode.variants" "hydra.decode.parsing" "hydra.decode.json.model"
   "hydra.decode.error.checking" "hydra.decode.error.core" "hydra.decode.errors"
   "hydra.decoding" "hydra.encoding"
   ;; Reduction
   "hydra.reduction"
   ;; JSON
   "hydra.json.bootstrap"
   "hydra.json.decode" "hydra.json.encode"
   "hydra.json.parser" "hydra.json.writer"
   "hydra.json.decoding"
   "hydra.adapt"
   "hydra.templates" "hydra.parsers"
   "hydra.json.yaml.decode" "hydra.json.yaml.encode"
   "hydra.yaml.model"
   "hydra.codeGeneration"])

(def ^:private gen-test-load-order
  "Generated test namespaces in dependency order."
  ["hydra.test.test_graph" "hydra.test.test_terms" "hydra.test.test_types"
   "hydra.test.lib.chars" "hydra.test.lib.eithers" "hydra.test.lib.equality"
   "hydra.test.lib.lists" "hydra.test.lib.literals" "hydra.test.lib.logic"
   "hydra.test.lib.maps" "hydra.test.lib.math" "hydra.test.lib.maybes"
   "hydra.test.lib.pairs" "hydra.test.lib.regex" "hydra.test.lib.sets" "hydra.test.lib.strings"
   "hydra.test.annotations" "hydra.test.reduction" "hydra.test.rewriting"
   "hydra.test.formatting" "hydra.test.serialization" "hydra.test.sorting"
   "hydra.test.substitution" "hydra.test.eta_expansion"
   "hydra.test.checking.all" "hydra.test.hoisting.all"
   "hydra.test.inference.all" "hydra.test.unification"
   "hydra.test.json.coder" "hydra.test.json.parser"
   "hydra.test.json.roundtrip" "hydra.test.json.writer"
   "hydra.test.json.yaml"
   "hydra.test.validate.all" "hydra.test.validate.core"
   "hydra.test.testSuite" "hydra.test.test_suite"])

(defn- extract-defs-from-file
  "Read a .clj file and return all top-level (def sym ...) symbol names."
  [file]
  (when (.exists file)
    (with-open [rdr (java.io.PushbackReader. (java.io.FileReader. file))]
      (binding [*read-eval* false]
        (loop [syms []]
          (let [form (try (read rdr false ::eof) (catch Exception _ ::eof))]
            (if (= form ::eof)
              syms
              (if (and (sequential? form) (= (first form) 'def) (symbol? (second form)))
                (recur (conj syms (second form)))
                (recur syms)))))))))

(defn- find-ns-file
  "Find the .clj file for a namespace on the classpath."
  [ns-name]
  (let [path (-> ns-name
                 (str/replace "." "/")
                 (str/replace "-" "_")
                 (str ".clj"))
        cp (System/getProperty "java.class.path")
        dirs (str/split cp (re-pattern (System/getProperty "path.separator")))]
    (some (fn [dir]
            (let [f (java.io.File. dir path)]
              (when (.exists f) f)))
          dirs)))

(defn pre-declare-ns-symbols!
  "Read the source file for a namespace and forward-declare all def symbols,
   both in the target namespace and in clojure.core."
  [ns-name]
  (when-let [file (find-ns-file ns-name)]
    (let [ns-sym (symbol ns-name)
          the-ns (or (find-ns ns-sym) (create-ns ns-sym))
          syms (extract-defs-from-file file)]
      (doseq [sym syms]
        ;; Create unbound var in the namespace
        (intern the-ns sym)
        ;; Also create in clojure.core for cross-module access
        (when (.startsWith (name sym) "hydra_")
          (intern 'clojure.core sym))))))

(declare ^:private contains-free-symbol?)

(defn- fix-forward-ref-let
  "Transform native (let [...] body) forms where a binding references a later binding.
   Detects forward references between function bindings and uses atoms for letrec semantics."
  [form]
  (cond
    (vector? form) (mapv fix-forward-ref-let form)
    (not (sequential? form)) form
    :else
    (let [form (apply list (map fix-forward-ref-let form))]
      ;; Check for (let [bindings...] body) with forward references
      (if (and (= (first form) 'let)
               (>= (count form) 3)
               (vector? (second form))
               (>= (count (second form)) 4))  ;; at least 2 bindings (4 elements)
        (let [binding-vec (second form)
              body (nth form 2)
              pairs (partition 2 binding-vec)
              syms (mapv first pairs)
              vals (mapv second pairs)
              ;; Check if any value references a later-defined symbol
              has-forward-ref (some (fn [i]
                                     (let [val (nth vals i)]
                                       (some (fn [j]
                                               (contains-free-symbol? (nth syms j) val))
                                             (range (inc i) (count syms)))))
                                   (range (count syms)))
              ;; Only transform if ALL values are functions
              all-fns (every? (fn [v] (and (sequential? v) (= (first v) 'fn))) vals)]
          (if (and has-forward-ref all-fns)
            ;; Use atoms with typed proxies for mutually-recursive functions.
            ;; Each proxy matches the exact param count of the fn for fast dispatch.
            (let [ref-syms (mapv #(symbol (str (name %) "__ref")) syms)
                  atom-decls (vec (mapcat (fn [rs] [rs (list 'atom nil)]) ref-syms))
                  proxy-decls (vec (mapcat (fn [sym val rs]
                                            ;; Handle both (fn [params] body) and (fn name [params] body)
                                            (let [params (if (vector? (second val))
                                                           (second val)
                                                           (nth val 2))]
                                              [sym (list 'fn params
                                                         (cons (list 'deref rs) params))]))
                                          syms vals ref-syms))
                  resets (map (fn [rs val] (list 'reset! rs val)) ref-syms vals)]
              (list 'let (vec (concat atom-decls proxy-decls))
                    (cons 'do (concat resets [body]))))
            form))
        form))))

(defn- contains-free-symbol?
  "Check if a form contains a free reference to the given symbol.
   Respects fn parameter shadowing: (fn [x] ...x...) does not
   count as a free reference to x."
  [sym form]
  (cond
    (= form sym) true
    (and (sequential? form) (= (first form) 'fn))
    ;; fn form: check if sym is shadowed by params
    (let [params (second form)
          param-set (if (vector? params) (set params) #{})]
      (if (contains? param-set sym)
        false  ;; sym is shadowed by a parameter
        (some #(contains-free-symbol? sym %) (drop 2 form))))
    (sequential? form) (some #(contains-free-symbol? sym %) form)
    (set? form) (some #(contains-free-symbol? sym %) form)
    (map? form) (some #(or (contains-free-symbol? sym (key %))
                           (contains-free-symbol? sym (val %)))
                      form)
    :else false))

(defn- desugar-if-else
  "Transform (((hydra_lib_logic_if_else COND) THEN) ELSE) into (if COND THEN ELSE).
   This is needed because Clojure is strict — ifElse evaluates both branches,
   which causes infinite recursion in recursive functions."
  [form]
  (cond
    (vector? form) (mapv desugar-if-else form)
    (not (sequential? form)) form
    :else
    (let [form (apply list (map desugar-if-else form))]
      ;; Pattern: ((X ELSE) where X = ((hydra_lib_logic_if_else COND) THEN)
      ;; Full: (((hydra_lib_logic_if_else COND) THEN) ELSE)
      (if (and (= (count form) 2)
               (sequential? (first form))
               (= (count (first form)) 2)
               (sequential? (first (first form)))
               (= (count (first (first form))) 2)
               (= (first (first (first form))) 'hydra_lib_logic_if_else))
        (let [cond-expr (second (first (first form)))
              then-expr (second (first form))
              else-expr (second form)]
          (list 'if cond-expr then-expr else-expr))
        form))))

(defn- fix-self-ref-let
  "Transform ((fn [x] BODY) VALUE) where VALUE has a free reference to x
   (self-reference) or to a binding defined in BODY (forward reference).
   For fn-valued self-refs: use named fn for recursion.
   For other self/forward refs: use atom indirection for the affected bindings."
  [form]
  (cond
    (vector? form) (mapv fix-self-ref-let form)
    (not (sequential? form)) form
    :else
    (let [items (map fix-self-ref-let form)
          ;; Restore vectors in fn forms: (fn [params] body) needs vector params
          items (if (and (= (first items) 'fn)
                        (>= (count items) 3)
                        (sequential? (second items))
                        (not (vector? (second items))))
                  (cons 'fn (cons (vec (second items)) (drop 2 items)))
                  items)
          form (apply list items)]
      ;; Pattern: ((fn [x] BODY) VALUE)
      (if (and (= (count form) 2)
               (sequential? (first form))
               (= (first (first form)) 'fn)
               (vector? (second (first form)))
               (= (count (second (first form))) 1)
               (symbol? (first (second (first form)))))
        (let [binding-sym (first (second (first form)))
              body (nth (first form) 2)
              value (second form)]
          (if (and (contains-free-symbol? binding-sym value)
                   ;; Exclude false positives where VALUE just uses an outer-scope
                   ;; variable that happens to have the same name:
                   ;; - bare symbol: ((fn [v] ...) v)
                   ;; - lambda application on binding: ((fn [v] ...) ((fn [x] ...) v))
                   (not (symbol? value))
                   (not (and (sequential? value)
                             (sequential? (first value))
                             (= (first (first value)) 'fn))))
            (if (and (sequential? value) (= (first value) 'fn)
                     (vector? (second value)))
              ;; Self-referencing fn: use named fn for recursion
              (let [fn-params (second value)
                    fn-body (drop 2 value)
                    named-fn (apply list 'fn binding-sym fn-params fn-body)]
                (list 'let [binding-sym named-fn] body))
              ;; Self-referencing non-fn: use atom indirection
              (let [ref-sym (symbol (str (name binding-sym) "__ref"))]
                (list 'let [ref-sym (list 'atom nil)
                            binding-sym (list 'fn ['& 'args__] (list 'apply (list 'deref ref-sym) 'args__))]
                      (list 'do
                            (list 'reset! ref-sym value)
                            body))))
            ;; Not a recursive self-reference; leave unchanged
            form))
        form))))

(defn load-ns-manually!
  "Load a generated namespace's .clj file by reading forms and evaluating them
   in the target namespace, skipping the (ns ...) declaration.
   Uses multi-pass retry for forward references (like the CL loader).
   Transforms self-referencing let bindings to use named fn for recursion."
  [ns-name]
  (when-let [file (find-ns-file ns-name)]
    (let [ns-sym (symbol ns-name)
          the-ns (or (find-ns ns-sym) (create-ns ns-sym))
          ;; Read all forms from file
          forms (with-open [rdr (java.io.PushbackReader. (java.io.FileReader. file))]
                  (binding [*read-eval* false]
                    (loop [acc []]
                      (let [form (try (read rdr false ::eof) (catch Exception _ ::eof))]
                        (if (= form ::eof) acc
                            (recur (conj acc form)))))))
          ;; Filter out ns declarations, desugar if_else, then fix self/forward-referencing let bindings
          eval-forms (->> forms
                         (filterv #(not (and (sequential? %) (= (first %) 'ns))))
                         (mapv desugar-if-else)
                         (mapv fix-forward-ref-let)
                         (mapv fix-self-ref-let))]
      ;; Multi-pass evaluation (retry failed forms up to 5 times)
      (binding [*ns* the-ns]
        (loop [remaining eval-forms pass 0]
          (when (and (seq remaining) (< pass 5))
            (let [still-failed (atom [])]
              (doseq [form remaining]
                (try
                  (eval form)
                  (catch Throwable _
                    (swap! still-failed conj form))))
              (let [failed @still-failed]
                ;; Only retry if we made progress
                (when (and (seq failed) (< (count failed) (count remaining)))
                  (recur failed (inc pass)))))))))))

(defn refer-clojure-into-ns!
  "Refer clojure.core into a namespace so fn, def, cond, etc. are available."
  [the-ns]
  (binding [*ns* the-ns]
    (eval '(clojure.core/refer-clojure))))

(defn require-and-globalize!
  "Load a generated namespace using multi-pass approach:
   1. Pre-declare all def symbols (for forward references within the file)
   2. Re-refer clojure.core (to pick up any newly globalized vars from previous modules)
   3. Load the file manually (skip ns form to preserve declarations)
   4. Globalize vars into clojure.core for subsequent modules"
  [ns-name]
  (let [ns-sym (symbol ns-name)]
    (try
      ;; Pre-declare all defs in target ns and clojure.core
      (pre-declare-ns-symbols! ns-name)
      ;; Re-refer clojure.core to pick up vars globalized by previous modules
      (let [the-ns (find-ns ns-sym)]
        (when the-ns (refer-clojure-into-ns! the-ns)))
      ;; Load manually, skipping (ns ...) to preserve pre-declared vars
      (load-ns-manually! ns-name)
      ;; Globalize actual values into clojure.core
      (globalize-ns-vars! ns-sym)
      (catch Exception e
        (binding [*out* *err*]
          (println (str "Warning: failed to load " ns-name ": " (.getMessage e))))))))

(defn ensure-libs-loaded!
  "Load lib modules, globalize their symbols into clojure.core,
   then pre-create all generated namespaces with clojure.core referred
   (so they pick up the globalized lib vars)."
  []
  ;; Step 1: Load libs and globalize their vars into clojure.core
  (globalize-all-lib-vars!)
  ;; Step 2: Pre-create all namespaces AFTER globalization,
  ;; so refer-clojure picks up the lib vars
  (doseq [ns-name gen-ns-names]
    (let [ns-sym (symbol ns-name)
          the-ns (or (find-ns ns-sym) (create-ns ns-sym))]
      (refer-clojure-into-ns! the-ns))))

(defn install-performance-patches!
  "Install memoized versions of expensive generated functions.
   Must be called after load-gen-main! has globalized the generated vars.
   Addresses eager evaluation overhead in functions like freeVariablesInTerm
   which are called repeatedly on the same terms during code generation."
  []
  ;; Placeholder for future performance patches.
  ;; Note: Python coder reorderDefs override must be installed after coder loading.
  ;; See install-coder-performance-patches! below.
  nil)

(defn install-coder-performance-patches!
  "Placeholder for future coder-level performance patches.
   Called after coder modules are loaded. Currently a no-op since laziness
   is handled by thunking in the Lisp coder and by wrapping dfltVars in
   freeVariablesInTerm as a lambda in the kernel DSL source."
  []
  nil)

(defn load-gen-main!
  "Load generated main modules in dependency order, globalizing after each."
  []
  (doseq [ns-name gen-main-load-order]
    (require-and-globalize! ns-name))
  (install-performance-patches!))

(defn load-gen-test!
  "Load generated test modules in dependency order, globalizing after each."
  []
  (doseq [ns-name gen-test-load-order]
    (require-and-globalize! ns-name)))

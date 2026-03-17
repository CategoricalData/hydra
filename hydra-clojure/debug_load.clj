(ns debug-load
  (:require [hydra.lib.preload :as preload]))
(preload/ensure-libs-loaded!)
(require 'user)

;; Load all gen-main modules EXCEPT reduction
(doseq [ns-name ["hydra.core" "hydra.error" "hydra.compute" "hydra.context"
                  "hydra.graph" "hydra.module" "hydra.phantoms" "hydra.coders"
                  "hydra.grammar" "hydra.ast" "hydra.testing" "hydra.typing"
                  "hydra.query" "hydra.relational" "hydra.tabular" "hydra.workflow"
                  "hydra.classes" "hydra.topology" "hydra.parsing" "hydra.variants"
                  "hydra.accessors" "hydra.util" "hydra.constants" "hydra.json.model"
                  "hydra.formatting" "hydra.rewriting" "hydra.sorting" "hydra.names"
                  "hydra.schemas" "hydra.arity" "hydra.lexical" "hydra.monads"
                  "hydra.hoisting" "hydra.show.core"
                  "hydra.encode.core" "hydra.decode.core"
                  "hydra.extract.core" "hydra.extract.util"
                  "hydra.substitution" "hydra.annotations" "hydra.unification"
                  "hydra.inference" "hydra.checking" "hydra.serialization"]]
  (preload/require-and-globalize! ns-name))

;; Now manually try to load reduction.clj form by form
(println "\nLoading reduction.clj forms one by one:")
(let [file (java.io.File. "src/gen-main/clojure/hydra/reduction.clj")
      the-ns (find-ns 'hydra.reduction)]
  (preload/pre-declare-ns-symbols! "hydra.reduction")
  (preload/refer-clojure-into-ns! the-ns)
  (with-open [rdr (java.io.PushbackReader. (java.io.FileReader. file))]
    (binding [*ns* the-ns *read-eval* false]
      (loop [i 0]
        (let [form (try (read rdr false ::eof) (catch Exception _ ::eof))]
          (when (not= form ::eof)
            (when-not (and (sequential? form) (= (first form) 'ns))
              (let [sym (when (and (sequential? form) (= (first form) 'def)) (second form))]
                (try
                  (eval form)
                  (println (str "  [" i "] " (or sym "?") ": OK"))
                  (catch Throwable e
                    (let [cause (or (.getCause e) e)]
                      (println (str "  [" i "] " (or sym "?") ": FAIL - " (.getMessage cause))))))))
            (recur (inc i))))))))

(System/exit 0)

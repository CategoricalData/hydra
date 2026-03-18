(ns run-generation-tests
  (:require [hydra.lib.preload :as preload]
            [clojure.test :as test]
            [clojure.java.io :as io]))

;; Load kernel modules
(require 'user) ;; registers make- constructors
(preload/ensure-libs-loaded!)
(preload/load-gen-main!)

;; Discover and load all generation test namespaces
(defn find-gen-test-namespaces []
  (let [base-dir (io/file "src/gen-test/clojure/generation")
        clj-files (filter #(.endsWith (.getName %) ".clj") (file-seq base-dir))]
    (map (fn [f]
           (let [path (.getPath f)
                 ;; Convert file path to namespace: src/gen-test/clojure/generation/hydra/test/lib/chars_test.clj
                 ;; -> generation.hydra.test.lib.chars-test
                 rel (subs path (count "src/gen-test/clojure/"))
                 ns-str (-> rel
                            (.replace ".clj" "")
                            (.replace "/" ".")
                            (.replace "_" "-"))]
             (symbol ns-str)))
         clj-files)))

(let [ns-syms (sort (find-gen-test-namespaces))]
  (println (str "Loading " (count ns-syms) " generation test namespaces..."))
  (doseq [ns-sym ns-syms]
    (require ns-sym))
  (println "Running generation tests...")
  (let [results (apply test/run-tests ns-syms)
        {:keys [test pass fail error]} results]
    (println)
    (println (str "Generation tests: " test " tests, " pass " passed, " fail " failed, " error " errors"))
    (System/exit (if (and (zero? fail) (zero? error)) 0 1))))

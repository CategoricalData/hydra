;; Hand-written test environment for Clojure.
;; Provides a real graph with primitives and schema types.
;; Delegates to test_runner/build-test-graph for the full graph setup.
(ns hydra.test.test-env
  (:require [hydra.context :refer :all]))

(def ^:private cached-graph (atom nil))
(def ^:private cached-context (atom nil))

(defn test-graph []
  (when (nil? @cached-graph)
    ;; Import at call time to avoid circular dependency
    (require 'hydra.test-runner)
    (let [build-fn (resolve 'hydra.test-runner/build-test-graph)]
      (reset! cached-graph (build-fn))))
  @cached-graph)

(defn test-context []
  (when (nil? @cached-context)
    (reset! cached-context (make-hydra_context_context (list) (list) {})))
  @cached-context)

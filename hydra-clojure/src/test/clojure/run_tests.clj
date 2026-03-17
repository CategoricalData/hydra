(ns run-tests
  (:require [hydra.testing :refer :all]
            [hydra.test.testSuite :refer :all]
            [hydra.test-runner :refer [run-test-group]]))

(let [[pass fail skip] (run-test-group "" hydra_test_test_suite_all_tests)]
  (println)
  (println "========================================")
  (println (str "Pass: " pass))
  (println (str "Fail: " fail))
  (println (str "Skip: " skip))
  (System/exit (if (> fail 0) 1 0)))

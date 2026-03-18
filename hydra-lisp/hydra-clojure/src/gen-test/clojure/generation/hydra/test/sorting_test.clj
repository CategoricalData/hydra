;; Note: this is an automatically generated file. Do not edit.
;; sorting

(ns generation.hydra.test.sorting-test
  (:require [clojure.test :refer :all]))

;; topological sort

(deftest test-topological-sort-negempty-set

  (is (= (list :right (list ))

         (hydra_sorting_topological_sort (list )))))

(deftest test-topological-sort-negsingleton-set

  (is (= (list :right (list 1))

         (hydra_sorting_topological_sort (list (list 1 (list )))))))

(deftest test-topological-sort-negdiscrete-set-with-multiple-elements

  (is (= (list :right (list 1 2 3))

         (hydra_sorting_topological_sort (list (list 3 (list )) (list 1 (list )) (list 2 (list )))))))

(deftest test-topological-sort-neglinked-list

  (is (= (list :right (list 1 3 2))

         (hydra_sorting_topological_sort (list (list 3 (list 1)) (list 2 (list 3)) (list 1 (list )))))))

(deftest test-topological-sort-negbinary-tree

  (is (= (list :right (list 5 1 2 6 4 3))

         (hydra_sorting_topological_sort (list (list 3 (list 1 4)) (list 4 (list 6 2)) (list 1 (list 5)) (list 2 (list )) (list 6 (list )) (list 5 (list )))))))

(deftest test-topological-sort-negtwo-trees

  (is (= (list :right (list 1 7 2 4 3 6 5))

         (hydra_sorting_topological_sort (list (list 3 (list 1 4)) (list 5 (list 6 2)) (list 2 (list 7)) (list 1 (list )) (list 4 (list )) (list 6 (list )) (list 7 (list )))))))

(deftest test-topological-sort-negdiamond-dag

  (is (= (list :right (list 5 2 3 4 1))

         (hydra_sorting_topological_sort (list (list 1 (list 3 4)) (list 3 (list 2)) (list 4 (list 2)) (list 2 (list 5)) (list 5 (list )))))))

(deftest test-topological-sort-negtwo-negnode-cycle

  (is (= (list :left (list (list 1 2)))

         (hydra_sorting_topological_sort (list (list 1 (list 2)) (list 2 (list 1)))))))

(deftest test-topological-sort-negcycle-with-incoming-and-outgoing-edges

  (is (= (list :left (list (list 2 3)))

         (hydra_sorting_topological_sort (list (list 1 (list 3)) (list 3 (list 2)) (list 2 (list 3 4)) (list 4 (list 5)) (list 5 (list )))))))

;; topological sort SCC

(deftest test-topological-sort-scc-negempty-set

  (is (= (list )

         (hydra_sorting_topological_sort_components (list )))))

(deftest test-topological-sort-scc-negsingleton-set

  (is (= (list (list 1))

         (hydra_sorting_topological_sort_components (list (list 1 (list )))))))

(deftest test-topological-sort-scc-negdiscrete-set-with-multiple-elements

  (is (= (list (list 1) (list 2) (list 3))

         (hydra_sorting_topological_sort_components (list (list 3 (list )) (list 1 (list )) (list 2 (list )))))))

(deftest test-topological-sort-scc-negsingle-two-negelement-component--num1

  (is (= (list (list 2) (list 1))

         (hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list )))))))

(deftest test-topological-sort-scc-negsingle-two-negelement-component--num2

  (is (= (list (list 1) (list 2))

         (hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 1 (list )))))))

(deftest test-topological-sort-scc-negmultiple-negelement-component

  (is (= (list (list 3) (list 1) (list 2))

         (hydra_sorting_topological_sort_components (list (list 2 (list 1 3)) (list 1 (list 3)) (list 3 (list )))))))

(deftest test-topological-sort-scc-negcycle-of-two-nodes--num1

  (is (= (list (list 1 2))

         (hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list 1)))))))

(deftest test-topological-sort-scc-negcycle-of-two-nodes--num2

  (is (= (list (list 1 2))

         (hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 1 (list 2)))))))

(deftest test-topological-sort-scc-negcycle-of-three-nodes--num1

  (is (= (list (list 1 2 3))

         (hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list 3)) (list 3 (list 1)))))))

(deftest test-topological-sort-scc-negcycle-of-three-nodes--num2

  (is (= (list (list 1 2 3))

         (hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 3 (list 2)) (list 1 (list 3)))))))

(deftest test-topological-sort-scc-negmultiple-disconnected-cycles

  (is (= (list (list 1 2 3) (list 10 20) (list 100) (list 200) (list 300))

         (hydra_sorting_topological_sort_components (list (list 200 (list )) (list 100 (list )) (list 300 (list )) (list 10 (list 20)) (list 20 (list 10)) (list 1 (list 2)) (list 2 (list 3)) (list 3 (list 1)))))))

(deftest test-topological-sort-scc-negcomplex-cycles

  (is (= (list (list 1 2 3))

         (hydra_sorting_topological_sort_components (list (list 1 (list 2 3)) (list 2 (list 3)) (list 3 (list 1)))))))

(deftest test-topological-sort-scc-negchain-of-three-sccs

  (is (= (list (list 100) (list 10 20) (list 1 2 3))

         (hydra_sorting_topological_sort_components (list (list 1 (list 2 10)) (list 2 (list 3)) (list 3 (list 1)) (list 10 (list 20)) (list 20 (list 100 10)) (list 100 (list )))))))

(deftest test-topological-sort-scc-negsccs-with-dependencies-to-divfrom-non-negscc-nodes

  (is (= (list (list 30) (list 20) (list 10) (list 1 2 3) (list 200) (list 100) (list 300) (list 1000) (list 2000))

         (hydra_sorting_topological_sort_components (list (list 1 (list 2 3 10)) (list 2 (list 3)) (list 3 (list 1)) (list 10 (list 20 30)) (list 20 (list 30)) (list 30 (list )) (list 100 (list 200 2)) (list 200 (list )) (list 300 (list 100)) (list 1000 (list )) (list 2000 (list )))))))

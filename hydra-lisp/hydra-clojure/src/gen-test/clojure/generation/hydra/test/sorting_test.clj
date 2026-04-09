;; Note: this is an automatically generated file. Do not edit.
;; sorting

(ns test-ns
  (:require [clojure.test :refer :all]))

;; topological sort

(deftest test-sorting-negtopological-sort-negempty-set

  (is (= right([])

         right([]))))

(deftest test-sorting-negtopological-sort-negsingleton-set

  (is (= right([1])

         right([1]))))

(deftest test-sorting-negtopological-sort-negdiscrete-set-with-multiple-elements

  (is (= right([1, 2, 3])

         right([1, 2, 3]))))

(deftest test-sorting-negtopological-sort-neglinked-list

  (is (= right([1, 3, 2])

         right([1, 3, 2]))))

(deftest test-sorting-negtopological-sort-negbinary-tree

  (is (= right([5, 1, 2, 6, 4, 3])

         right([5, 1, 2, 6, 4, 3]))))

(deftest test-sorting-negtopological-sort-negtwo-trees

  (is (= right([1, 7, 2, 4, 3, 6, 5])

         right([1, 7, 2, 4, 3, 6, 5]))))

(deftest test-sorting-negtopological-sort-negdiamond-dag

  (is (= right([5, 2, 3, 4, 1])

         right([5, 2, 3, 4, 1]))))

(deftest test-sorting-negtopological-sort-negtwo-negnode-cycle

  (is (= left([[1, 2]])

         left([[1, 2]]))))

(deftest test-sorting-negtopological-sort-negcycle-with-incoming-and-outgoing-edges

  (is (= left([[2, 3]])

         left([[2, 3]]))))

;; topological sort SCC

(deftest test-sorting-negtopological-sort-scc-negempty-set

  (is (= []

         [])))

(deftest test-sorting-negtopological-sort-scc-negsingleton-set

  (is (= [[1]]

         [[1]])))

(deftest test-sorting-negtopological-sort-scc-negdiscrete-set-with-multiple-elements

  (is (= [[1], [2], [3]]

         [[1], [2], [3]])))

(deftest test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num1

  (is (= [[2], [1]]

         [[2], [1]])))

(deftest test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num2

  (is (= [[1], [2]]

         [[1], [2]])))

(deftest test-sorting-negtopological-sort-scc-negmultiple-negelement-component

  (is (= [[3], [1], [2]]

         [[3], [1], [2]])))

(deftest test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num1

  (is (= [[1, 2]]

         [[1, 2]])))

(deftest test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num2

  (is (= [[1, 2]]

         [[1, 2]])))

(deftest test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num1

  (is (= [[1, 2, 3]]

         [[1, 2, 3]])))

(deftest test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num2

  (is (= [[1, 2, 3]]

         [[1, 2, 3]])))

(deftest test-sorting-negtopological-sort-scc-negmultiple-disconnected-cycles

  (is (= [[1, 2, 3], [10, 20], [100], [200], [300]]

         [[1, 2, 3], [10, 20], [100], [200], [300]])))

(deftest test-sorting-negtopological-sort-scc-negcomplex-cycles

  (is (= [[1, 2, 3]]

         [[1, 2, 3]])))

(deftest test-sorting-negtopological-sort-scc-negchain-of-three-sccs

  (is (= [[100], [10, 20], [1, 2, 3]]

         [[100], [10, 20], [1, 2, 3]])))

(deftest test-sorting-negtopological-sort-scc-negsccs-with-dependencies-to-divfrom-non-negscc-nodes

  (is (= [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]]

         [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]])))

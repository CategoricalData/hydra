;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; sorting

(require 'ert)

;; topological sort

(ert-deftest test-sorting-negtopological-sort-negempty-set ()

  (should (equal right([]) right([]))))

(ert-deftest test-sorting-negtopological-sort-negsingleton-set ()

  (should (equal right([1]) right([1]))))

(ert-deftest test-sorting-negtopological-sort-negdiscrete-set-with-multiple-elements ()

  (should (equal right([1, 2, 3]) right([1, 2, 3]))))

(ert-deftest test-sorting-negtopological-sort-neglinked-list ()

  (should (equal right([1, 3, 2]) right([1, 3, 2]))))

(ert-deftest test-sorting-negtopological-sort-negbinary-tree ()

  (should (equal right([5, 1, 2, 6, 4, 3]) right([5, 1, 2, 6, 4, 3]))))

(ert-deftest test-sorting-negtopological-sort-negtwo-trees ()

  (should (equal right([1, 7, 2, 4, 3, 6, 5]) right([1, 7, 2, 4, 3, 6, 5]))))

(ert-deftest test-sorting-negtopological-sort-negdiamond-dag ()

  (should (equal right([5, 2, 3, 4, 1]) right([5, 2, 3, 4, 1]))))

(ert-deftest test-sorting-negtopological-sort-negtwo-negnode-cycle ()

  (should (equal left([[1, 2]]) left([[1, 2]]))))

(ert-deftest test-sorting-negtopological-sort-negcycle-with-incoming-and-outgoing-edges ()

  (should (equal left([[2, 3]]) left([[2, 3]]))))

;; topological sort SCC

(ert-deftest test-sorting-negtopological-sort-scc-negempty-set ()

  (should (equal [] [])))

(ert-deftest test-sorting-negtopological-sort-scc-negsingleton-set ()

  (should (equal [[1]] [[1]])))

(ert-deftest test-sorting-negtopological-sort-scc-negdiscrete-set-with-multiple-elements ()

  (should (equal [[1], [2], [3]] [[1], [2], [3]])))

(ert-deftest test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num1 ()

  (should (equal [[2], [1]] [[2], [1]])))

(ert-deftest test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num2 ()

  (should (equal [[1], [2]] [[1], [2]])))

(ert-deftest test-sorting-negtopological-sort-scc-negmultiple-negelement-component ()

  (should (equal [[3], [1], [2]] [[3], [1], [2]])))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num1 ()

  (should (equal [[1, 2]] [[1, 2]])))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num2 ()

  (should (equal [[1, 2]] [[1, 2]])))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num1 ()

  (should (equal [[1, 2, 3]] [[1, 2, 3]])))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num2 ()

  (should (equal [[1, 2, 3]] [[1, 2, 3]])))

(ert-deftest test-sorting-negtopological-sort-scc-negmultiple-disconnected-cycles ()

  (should (equal [[1, 2, 3], [10, 20], [100], [200], [300]] [[1, 2, 3], [10, 20], [100], [200], [300]])))

(ert-deftest test-sorting-negtopological-sort-scc-negcomplex-cycles ()

  (should (equal [[1, 2, 3]] [[1, 2, 3]])))

(ert-deftest test-sorting-negtopological-sort-scc-negchain-of-three-sccs ()

  (should (equal [[100], [10, 20], [1, 2, 3]] [[100], [10, 20], [1, 2, 3]])))

(ert-deftest test-sorting-negtopological-sort-scc-negsccs-with-dependencies-to-divfrom-non-negscc-nodes ()

  (should (equal [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]] [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]])))

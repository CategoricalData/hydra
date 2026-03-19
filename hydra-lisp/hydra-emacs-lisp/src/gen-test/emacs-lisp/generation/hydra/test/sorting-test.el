;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; sorting

(require 'ert)

;; topological sort

(ert-deftest test-sorting-negtopological-sort-negempty-set ()

  (should (equal (list :right (list )) (funcall hydra_sorting_topological_sort (list )))))

(ert-deftest test-sorting-negtopological-sort-negsingleton-set ()

  (should (equal (list :right (list 1)) (funcall hydra_sorting_topological_sort (list (list 1 (list )))))))

(ert-deftest test-sorting-negtopological-sort-negdiscrete-set-with-multiple-elements ()

  (should (equal (list :right (list 1 2 3)) (funcall hydra_sorting_topological_sort (list (list 3 (list )) (list 1 (list )) (list 2 (list )))))))

(ert-deftest test-sorting-negtopological-sort-neglinked-list ()

  (should (equal (list :right (list 1 3 2)) (funcall hydra_sorting_topological_sort (list (list 3 (list 1)) (list 2 (list 3)) (list 1 (list )))))))

(ert-deftest test-sorting-negtopological-sort-negbinary-tree ()

  (should (equal (list :right (list 5 1 2 6 4 3)) (funcall hydra_sorting_topological_sort (list (list 3 (list 1 4)) (list 4 (list 6 2)) (list 1 (list 5)) (list 2 (list )) (list 6 (list )) (list 5 (list )))))))

(ert-deftest test-sorting-negtopological-sort-negtwo-trees ()

  (should (equal (list :right (list 1 7 2 4 3 6 5)) (funcall hydra_sorting_topological_sort (list (list 3 (list 1 4)) (list 5 (list 6 2)) (list 2 (list 7)) (list 1 (list )) (list 4 (list )) (list 6 (list )) (list 7 (list )))))))

(ert-deftest test-sorting-negtopological-sort-negdiamond-dag ()

  (should (equal (list :right (list 5 2 3 4 1)) (funcall hydra_sorting_topological_sort (list (list 1 (list 3 4)) (list 3 (list 2)) (list 4 (list 2)) (list 2 (list 5)) (list 5 (list )))))))

(ert-deftest test-sorting-negtopological-sort-negtwo-negnode-cycle ()

  (should (equal (list :left (list (list 1 2))) (funcall hydra_sorting_topological_sort (list (list 1 (list 2)) (list 2 (list 1)))))))

(ert-deftest test-sorting-negtopological-sort-negcycle-with-incoming-and-outgoing-edges ()

  (should (equal (list :left (list (list 2 3))) (funcall hydra_sorting_topological_sort (list (list 1 (list 3)) (list 3 (list 2)) (list 2 (list 3 4)) (list 4 (list 5)) (list 5 (list )))))))

;; topological sort SCC

(ert-deftest test-sorting-negtopological-sort-scc-negempty-set ()

  (should (equal (list ) (funcall hydra_sorting_topological_sort_components (list )))))

(ert-deftest test-sorting-negtopological-sort-scc-negsingleton-set ()

  (should (equal (list (list 1)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list )))))))

(ert-deftest test-sorting-negtopological-sort-scc-negdiscrete-set-with-multiple-elements ()

  (should (equal (list (list 1) (list 2) (list 3)) (funcall hydra_sorting_topological_sort_components (list (list 3 (list )) (list 1 (list )) (list 2 (list )))))))

(ert-deftest test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num1 ()

  (should (equal (list (list 2) (list 1)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list )))))))

(ert-deftest test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num2 ()

  (should (equal (list (list 1) (list 2)) (funcall hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 1 (list )))))))

(ert-deftest test-sorting-negtopological-sort-scc-negmultiple-negelement-component ()

  (should (equal (list (list 3) (list 1) (list 2)) (funcall hydra_sorting_topological_sort_components (list (list 2 (list 1 3)) (list 1 (list 3)) (list 3 (list )))))))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num1 ()

  (should (equal (list (list 1 2)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list 1)))))))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num2 ()

  (should (equal (list (list 1 2)) (funcall hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 1 (list 2)))))))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num1 ()

  (should (equal (list (list 1 2 3)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list 3)) (list 3 (list 1)))))))

(ert-deftest test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num2 ()

  (should (equal (list (list 1 2 3)) (funcall hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 3 (list 2)) (list 1 (list 3)))))))

(ert-deftest test-sorting-negtopological-sort-scc-negmultiple-disconnected-cycles ()

  (should (equal (list (list 1 2 3) (list 10 20) (list 100) (list 200) (list 300)) (funcall hydra_sorting_topological_sort_components (list (list 200 (list )) (list 100 (list )) (list 300 (list )) (list 10 (list 20)) (list 20 (list 10)) (list 1 (list 2)) (list 2 (list 3)) (list 3 (list 1)))))))

(ert-deftest test-sorting-negtopological-sort-scc-negcomplex-cycles ()

  (should (equal (list (list 1 2 3)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list 2 3)) (list 2 (list 3)) (list 3 (list 1)))))))

(ert-deftest test-sorting-negtopological-sort-scc-negchain-of-three-sccs ()

  (should (equal (list (list 100) (list 10 20) (list 1 2 3)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list 2 10)) (list 2 (list 3)) (list 3 (list 1)) (list 10 (list 20)) (list 20 (list 100 10)) (list 100 (list )))))))

(ert-deftest test-sorting-negtopological-sort-scc-negsccs-with-dependencies-to-divfrom-non-negscc-nodes ()

  (should (equal (list (list 30) (list 20) (list 10) (list 1 2 3) (list 200) (list 100) (list 300) (list 1000) (list 2000)) (funcall hydra_sorting_topological_sort_components (list (list 1 (list 2 3 10)) (list 2 (list 3)) (list 3 (list 1)) (list 10 (list 20 30)) (list 20 (list 30)) (list 30 (list )) (list 100 (list 200 2)) (list 200 (list )) (list 300 (list 100)) (list 1000 (list )) (list 2000 (list )))))))

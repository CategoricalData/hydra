;; Note: this is an automatically generated file. Do not edit.
;; sorting

(import (scheme base))

;; topological sort

(define (test-topological-sort-negempty-set)

  (assert (equal? (list :right (list )) (hydra_sorting_topological_sort (list )))))

(define (test-topological-sort-negsingleton-set)

  (assert (equal? (list :right (list 1)) (hydra_sorting_topological_sort (list (list 1 (list )))))))

(define (test-topological-sort-negdiscrete-set-with-multiple-elements)

  (assert (equal? (list :right (list 1 2 3)) (hydra_sorting_topological_sort (list (list 3 (list )) (list 1 (list )) (list 2 (list )))))))

(define (test-topological-sort-neglinked-list)

  (assert (equal? (list :right (list 1 3 2)) (hydra_sorting_topological_sort (list (list 3 (list 1)) (list 2 (list 3)) (list 1 (list )))))))

(define (test-topological-sort-negbinary-tree)

  (assert (equal? (list :right (list 5 1 2 6 4 3)) (hydra_sorting_topological_sort (list (list 3 (list 1 4)) (list 4 (list 6 2)) (list 1 (list 5)) (list 2 (list )) (list 6 (list )) (list 5 (list )))))))

(define (test-topological-sort-negtwo-trees)

  (assert (equal? (list :right (list 1 7 2 4 3 6 5)) (hydra_sorting_topological_sort (list (list 3 (list 1 4)) (list 5 (list 6 2)) (list 2 (list 7)) (list 1 (list )) (list 4 (list )) (list 6 (list )) (list 7 (list )))))))

(define (test-topological-sort-negdiamond-dag)

  (assert (equal? (list :right (list 5 2 3 4 1)) (hydra_sorting_topological_sort (list (list 1 (list 3 4)) (list 3 (list 2)) (list 4 (list 2)) (list 2 (list 5)) (list 5 (list )))))))

(define (test-topological-sort-negtwo-negnode-cycle)

  (assert (equal? (list :left (list (list 1 2))) (hydra_sorting_topological_sort (list (list 1 (list 2)) (list 2 (list 1)))))))

(define (test-topological-sort-negcycle-with-incoming-and-outgoing-edges)

  (assert (equal? (list :left (list (list 2 3))) (hydra_sorting_topological_sort (list (list 1 (list 3)) (list 3 (list 2)) (list 2 (list 3 4)) (list 4 (list 5)) (list 5 (list )))))))

;; topological sort SCC

(define (test-topological-sort-scc-negempty-set)

  (assert (equal? (list ) (hydra_sorting_topological_sort_components (list )))))

(define (test-topological-sort-scc-negsingleton-set)

  (assert (equal? (list (list 1)) (hydra_sorting_topological_sort_components (list (list 1 (list )))))))

(define (test-topological-sort-scc-negdiscrete-set-with-multiple-elements)

  (assert (equal? (list (list 1) (list 2) (list 3)) (hydra_sorting_topological_sort_components (list (list 3 (list )) (list 1 (list )) (list 2 (list )))))))

(define (test-topological-sort-scc-negsingle-two-negelement-component--num1)

  (assert (equal? (list (list 2) (list 1)) (hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list )))))))

(define (test-topological-sort-scc-negsingle-two-negelement-component--num2)

  (assert (equal? (list (list 1) (list 2)) (hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 1 (list )))))))

(define (test-topological-sort-scc-negmultiple-negelement-component)

  (assert (equal? (list (list 3) (list 1) (list 2)) (hydra_sorting_topological_sort_components (list (list 2 (list 1 3)) (list 1 (list 3)) (list 3 (list )))))))

(define (test-topological-sort-scc-negcycle-of-two-nodes--num1)

  (assert (equal? (list (list 1 2)) (hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list 1)))))))

(define (test-topological-sort-scc-negcycle-of-two-nodes--num2)

  (assert (equal? (list (list 1 2)) (hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 1 (list 2)))))))

(define (test-topological-sort-scc-negcycle-of-three-nodes--num1)

  (assert (equal? (list (list 1 2 3)) (hydra_sorting_topological_sort_components (list (list 1 (list 2)) (list 2 (list 3)) (list 3 (list 1)))))))

(define (test-topological-sort-scc-negcycle-of-three-nodes--num2)

  (assert (equal? (list (list 1 2 3)) (hydra_sorting_topological_sort_components (list (list 2 (list 1)) (list 3 (list 2)) (list 1 (list 3)))))))

(define (test-topological-sort-scc-negmultiple-disconnected-cycles)

  (assert (equal? (list (list 1 2 3) (list 10 20) (list 100) (list 200) (list 300)) (hydra_sorting_topological_sort_components (list (list 200 (list )) (list 100 (list )) (list 300 (list )) (list 10 (list 20)) (list 20 (list 10)) (list 1 (list 2)) (list 2 (list 3)) (list 3 (list 1)))))))

(define (test-topological-sort-scc-negcomplex-cycles)

  (assert (equal? (list (list 1 2 3)) (hydra_sorting_topological_sort_components (list (list 1 (list 2 3)) (list 2 (list 3)) (list 3 (list 1)))))))

(define (test-topological-sort-scc-negchain-of-three-sccs)

  (assert (equal? (list (list 100) (list 10 20) (list 1 2 3)) (hydra_sorting_topological_sort_components (list (list 1 (list 2 10)) (list 2 (list 3)) (list 3 (list 1)) (list 10 (list 20)) (list 20 (list 100 10)) (list 100 (list )))))))

(define (test-topological-sort-scc-negsccs-with-dependencies-to-divfrom-non-negscc-nodes)

  (assert (equal? (list (list 30) (list 20) (list 10) (list 1 2 3) (list 200) (list 100) (list 300) (list 1000) (list 2000)) (hydra_sorting_topological_sort_components (list (list 1 (list 2 3 10)) (list 2 (list 3)) (list 3 (list 1)) (list 10 (list 20 30)) (list 20 (list 30)) (list 30 (list )) (list 100 (list 200 2)) (list 200 (list )) (list 300 (list 100)) (list 1000 (list )) (list 2000 (list )))))))

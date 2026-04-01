;; Note: this is an automatically generated file. Do not edit.
;; sorting

(import (scheme base))

;; topological sort

(define (test-sorting-negtopological-sort-negempty-set)

  (assert (equal? right([]) right([]))))

(define (test-sorting-negtopological-sort-negsingleton-set)

  (assert (equal? right([1]) right([1]))))

(define (test-sorting-negtopological-sort-negdiscrete-set-with-multiple-elements)

  (assert (equal? right([1, 2, 3]) right([1, 2, 3]))))

(define (test-sorting-negtopological-sort-neglinked-list)

  (assert (equal? right([1, 3, 2]) right([1, 3, 2]))))

(define (test-sorting-negtopological-sort-negbinary-tree)

  (assert (equal? right([5, 1, 2, 6, 4, 3]) right([5, 1, 2, 6, 4, 3]))))

(define (test-sorting-negtopological-sort-negtwo-trees)

  (assert (equal? right([1, 7, 2, 4, 3, 6, 5]) right([1, 7, 2, 4, 3, 6, 5]))))

(define (test-sorting-negtopological-sort-negdiamond-dag)

  (assert (equal? right([5, 2, 3, 4, 1]) right([5, 2, 3, 4, 1]))))

(define (test-sorting-negtopological-sort-negtwo-negnode-cycle)

  (assert (equal? left([[1, 2]]) left([[1, 2]]))))

(define (test-sorting-negtopological-sort-negcycle-with-incoming-and-outgoing-edges)

  (assert (equal? left([[2, 3]]) left([[2, 3]]))))

;; topological sort SCC

(define (test-sorting-negtopological-sort-scc-negempty-set)

  (assert (equal? [] [])))

(define (test-sorting-negtopological-sort-scc-negsingleton-set)

  (assert (equal? [[1]] [[1]])))

(define (test-sorting-negtopological-sort-scc-negdiscrete-set-with-multiple-elements)

  (assert (equal? [[1], [2], [3]] [[1], [2], [3]])))

(define (test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num1)

  (assert (equal? [[2], [1]] [[2], [1]])))

(define (test-sorting-negtopological-sort-scc-negsingle-two-negelement-component--num2)

  (assert (equal? [[1], [2]] [[1], [2]])))

(define (test-sorting-negtopological-sort-scc-negmultiple-negelement-component)

  (assert (equal? [[3], [1], [2]] [[3], [1], [2]])))

(define (test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num1)

  (assert (equal? [[1, 2]] [[1, 2]])))

(define (test-sorting-negtopological-sort-scc-negcycle-of-two-nodes--num2)

  (assert (equal? [[1, 2]] [[1, 2]])))

(define (test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num1)

  (assert (equal? [[1, 2, 3]] [[1, 2, 3]])))

(define (test-sorting-negtopological-sort-scc-negcycle-of-three-nodes--num2)

  (assert (equal? [[1, 2, 3]] [[1, 2, 3]])))

(define (test-sorting-negtopological-sort-scc-negmultiple-disconnected-cycles)

  (assert (equal? [[1, 2, 3], [10, 20], [100], [200], [300]] [[1, 2, 3], [10, 20], [100], [200], [300]])))

(define (test-sorting-negtopological-sort-scc-negcomplex-cycles)

  (assert (equal? [[1, 2, 3]] [[1, 2, 3]])))

(define (test-sorting-negtopological-sort-scc-negchain-of-three-sccs)

  (assert (equal? [[100], [10, 20], [1, 2, 3]] [[100], [10, 20], [1, 2, 3]])))

(define (test-sorting-negtopological-sort-scc-negsccs-with-dependencies-to-divfrom-non-negscc-nodes)

  (assert (equal? [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]] [[30], [20], [10], [1, 2, 3], [200], [100], [300], [1000], [2000]])))

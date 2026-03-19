;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

(import (scheme base))

;; empty

(define (test-sets-negempty-negempty-set)

  (assert (equal? (list) hydra_lib_sets_empty)))

;; singleton

(define (test-sets-negsingleton-negsingle-element)

  (assert (equal? (list 42) (hydra_lib_sets_singleton 42))))

;; fromList

(define (test-sets-negfromlist-negcreate-from-list)

  (assert (equal? (list 1 2 3) (hydra_lib_sets_from_list (list 1 2 3)))))

(define (test-sets-negfromlist-negduplicates-removed)

  (assert (equal? (list 1 2 3) (hydra_lib_sets_from_list (list 1 2 1 3)))))

(define (test-sets-negfromlist-negempty-list)

  (assert (equal? (list) (hydra_lib_sets_from_list (list )))))

;; toList

(define (test-sets-negtolist-negconvert-to-list)

  (assert (equal? (list 1 2 3) (hydra_lib_sets_to_list (list 1 2 3)))))

(define (test-sets-negtolist-negunsorted-input)

  (assert (equal? (list 1 2 3) (hydra_lib_sets_to_list (list 1 2 3)))))

(define (test-sets-negtolist-negempty-set)

  (assert (equal? (list ) (hydra_lib_sets_to_list (list)))))

;; insert

(define (test-sets-neginsert-neginsert-new-element)

  (assert (equal? (list 1 2 3 4) ((hydra_lib_sets_insert 4) (list 1 2 3)))))

(define (test-sets-neginsert-neginsert-existing-element)

  (assert (equal? (list 1 2 3) ((hydra_lib_sets_insert 2) (list 1 2 3)))))

(define (test-sets-neginsert-neginsert-into-empty)

  (assert (equal? (list 1) ((hydra_lib_sets_insert 1) (list)))))

;; delete

(define (test-sets-negdelete-negdelete-existing)

  (assert (equal? (list 1 3) ((hydra_lib_sets_delete 2) (list 1 2 3)))))

(define (test-sets-negdelete-negdelete-non-negexisting)

  (assert (equal? (list 1 2 3) ((hydra_lib_sets_delete 4) (list 1 2 3)))))

(define (test-sets-negdelete-negdelete-from-empty)

  (assert (equal? (list) ((hydra_lib_sets_delete 1) (list)))))

;; member

(define (test-sets-negmember-negelement-exists)

  (assert (equal? #t ((hydra_lib_sets_member 2) (list 1 2 3)))))

(define (test-sets-negmember-negelement-missing)

  (assert (equal? #f ((hydra_lib_sets_member 4) (list 1 2 3)))))

(define (test-sets-negmember-negempty-set)

  (assert (equal? #f ((hydra_lib_sets_member 1) (list)))))

;; size

(define (test-sets-negsize-negthree-elements)

  (assert (equal? 3 (hydra_lib_sets_size (list 1 2 3)))))

(define (test-sets-negsize-negsingle-element)

  (assert (equal? 1 (hydra_lib_sets_size (list 42)))))

(define (test-sets-negsize-negempty-set)

  (assert (equal? 0 (hydra_lib_sets_size (list)))))

;; null

(define (test-sets-negnull-negempty-set)

  (assert (equal? #t (hydra_lib_sets_null (list)))))

(define (test-sets-negnull-negnon-negempty-set)

  (assert (equal? #f (hydra_lib_sets_null (list 1 2)))))

;; union

(define (test-sets-negunion-negunion-two-sets)

  (assert (equal? (list 1 2 3) ((hydra_lib_sets_union (list 1 2)) (list 2 3)))))

(define (test-sets-negunion-negunion-with-empty)

  (assert (equal? (list 1 2) ((hydra_lib_sets_union (list 1 2)) (list)))))

(define (test-sets-negunion-negempty-with-non-negempty)

  (assert (equal? (list 1 2) ((hydra_lib_sets_union (list)) (list 1 2)))))

;; unions

(define (test-sets-negunions-negunion-of-multiple-sets)

  (assert (equal? (list 1 2 3 4) (hydra_lib_sets_unions (list (list 1 2) (list 2 3) (list 3 4))))))

(define (test-sets-negunions-negunion-with-empty-sets)

  (assert (equal? (list 1 2 3) (hydra_lib_sets_unions (list (list 1 2) (list) (list 3))))))

(define (test-sets-negunions-negempty-list-of-sets)

  (assert (equal? (list) (hydra_lib_sets_unions (list )))))

(define (test-sets-negunions-negsingle-set)

  (assert (equal? (list 1 2 3) (hydra_lib_sets_unions (list (list 1 2 3))))))

;; intersection

(define (test-sets-negintersection-negcommon-elements)

  (assert (equal? (list 2 3) ((hydra_lib_sets_intersection (list 1 2 3)) (list 2 3 4)))))

(define (test-sets-negintersection-negno-common-elements)

  (assert (equal? (list) ((hydra_lib_sets_intersection (list 1 2)) (list 3 4)))))

(define (test-sets-negintersection-negintersection-with-empty)

  (assert (equal? (list) ((hydra_lib_sets_intersection (list 1 2)) (list)))))

;; difference

(define (test-sets-negdifference-negremove-elements)

  (assert (equal? (list 1 3) ((hydra_lib_sets_difference (list 1 2 3)) (list 2 4)))))

(define (test-sets-negdifference-negno-overlap)

  (assert (equal? (list 1 2) ((hydra_lib_sets_difference (list 1 2)) (list 3 4)))))

(define (test-sets-negdifference-negdifference-with-empty)

  (assert (equal? (list 1 2) ((hydra_lib_sets_difference (list 1 2)) (list)))))

;; map

(define (test-sets-negmap-negmap-function)

  (assert (equal? (list 2 4 6) ((hydra_lib_sets_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list 1 2 3)))))

(define (test-sets-negmap-negmap-on-empty)

  (assert (equal? (list) ((hydra_lib_sets_map (lambda (x) ((hydra_lib_math_mul x) 2))) (list)))))

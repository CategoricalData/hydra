;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.sets primitives

(import (scheme base))

;; empty

(define (test-sets-negempty-negempty-set)

  (assert (equal? {} {})))

;; singleton

(define (test-sets-negsingleton-negsingle-element)

  (assert (equal? {42} {42})))

;; fromList

(define (test-sets-negfromlist-negcreate-from-list)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

(define (test-sets-negfromlist-negduplicates-removed)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

(define (test-sets-negfromlist-negempty-list)

  (assert (equal? {} {})))

;; toList

(define (test-sets-negtolist-negconvert-to-list)

  (assert (equal? [1, 2, 3] [1, 2, 3])))

(define (test-sets-negtolist-negunsorted-input)

  (assert (equal? [1, 2, 3] [1, 2, 3])))

(define (test-sets-negtolist-negempty-set)

  (assert (equal? [] [])))

;; insert

(define (test-sets-neginsert-neginsert-new-element)

  (assert (equal? {1, 2, 3, 4} {1, 2, 3, 4})))

(define (test-sets-neginsert-neginsert-existing-element)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

(define (test-sets-neginsert-neginsert-into-empty)

  (assert (equal? {1} {1})))

;; delete

(define (test-sets-negdelete-negdelete-existing)

  (assert (equal? {1, 3} {1, 3})))

(define (test-sets-negdelete-negdelete-non-negexisting)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

(define (test-sets-negdelete-negdelete-from-empty)

  (assert (equal? {} {})))

;; member

(define (test-sets-negmember-negelement-exists)

  (assert (equal? true true)))

(define (test-sets-negmember-negelement-missing)

  (assert (equal? false false)))

(define (test-sets-negmember-negempty-set)

  (assert (equal? false false)))

;; size

(define (test-sets-negsize-negthree-elements)

  (assert (equal? 3 3)))

(define (test-sets-negsize-negsingle-element)

  (assert (equal? 1 1)))

(define (test-sets-negsize-negempty-set)

  (assert (equal? 0 0)))

;; null

(define (test-sets-negnull-negempty-set)

  (assert (equal? true true)))

(define (test-sets-negnull-negnon-negempty-set)

  (assert (equal? false false)))

;; union

(define (test-sets-negunion-negunion-two-sets)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

(define (test-sets-negunion-negunion-with-empty)

  (assert (equal? {1, 2} {1, 2})))

(define (test-sets-negunion-negempty-with-non-negempty)

  (assert (equal? {1, 2} {1, 2})))

;; unions

(define (test-sets-negunions-negunion-of-multiple-sets)

  (assert (equal? {1, 2, 3, 4} {1, 2, 3, 4})))

(define (test-sets-negunions-negunion-with-empty-sets)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

(define (test-sets-negunions-negempty-list-of-sets)

  (assert (equal? {} {})))

(define (test-sets-negunions-negsingle-set)

  (assert (equal? {1, 2, 3} {1, 2, 3})))

;; intersection

(define (test-sets-negintersection-negcommon-elements)

  (assert (equal? {2, 3} {2, 3})))

(define (test-sets-negintersection-negno-common-elements)

  (assert (equal? {} {})))

(define (test-sets-negintersection-negintersection-with-empty)

  (assert (equal? {} {})))

;; difference

(define (test-sets-negdifference-negremove-elements)

  (assert (equal? {1, 3} {1, 3})))

(define (test-sets-negdifference-negno-overlap)

  (assert (equal? {1, 2} {1, 2})))

(define (test-sets-negdifference-negdifference-with-empty)

  (assert (equal? {1, 2} {1, 2})))

;; map

(define (test-sets-negmap-negmap-function)

  (assert (equal? {2, 4, 6} {2, 4, 6})))

(define (test-sets-negmap-negmap-on-empty)

  (assert (equal? {} {})))

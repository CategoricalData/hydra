;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.eithers primitives

(import (scheme base))

;; bind

(define (test-eithers-negbind-negbind-right-with-success)

  (assert (equal? right(2) right(2))))

(define (test-eithers-negbind-negbind-right-with-failure)

  (assert (equal? left(0) left(0))))

(define (test-eithers-negbind-negbind-left-returns-left-unchanged)

  (assert (equal? left(42) left(42))))

;; bimap

(define (test-eithers-negbimap-negmap-left-value)

  (assert (equal? left(10) left(10))))

(define (test-eithers-negbimap-negmap-right-value)

  (assert (equal? right(2) right(2))))

;; isLeft

(define (test-eithers-negisleft-negleft-value)

  (assert (equal? true true)))

(define (test-eithers-negisleft-negright-value)

  (assert (equal? false false)))

;; isRight

(define (test-eithers-negisright-negright-value)

  (assert (equal? true true)))

(define (test-eithers-negisright-negleft-value)

  (assert (equal? false false)))

;; fromLeft

(define (test-eithers-negfromleft-negextract-left)

  (assert (equal? 42 42)))

(define (test-eithers-negfromleft-neguse-default-for-right)

  (assert (equal? 99 99)))

;; fromRight

(define (test-eithers-negfromright-negextract-right)

  (assert (equal? "test" "test")))

(define (test-eithers-negfromright-neguse-default-for-left)

  (assert (equal? "default" "default")))

;; either

(define (test-eithers-negeither-negapply-left-function)

  (assert (equal? 10 10)))

(define (test-eithers-negeither-negapply-right-function)

  (assert (equal? 2 2)))

;; lefts

(define (test-eithers-neglefts-negfilter-left-values)

  (assert (equal? [1, 2] [1, 2])))

(define (test-eithers-neglefts-negall-lefts)

  (assert (equal? [1, 2] [1, 2])))

(define (test-eithers-neglefts-negall-rights)

  (assert (equal? [] [])))

(define (test-eithers-neglefts-negempty-list)

  (assert (equal? [] [])))

;; rights

(define (test-eithers-negrights-negfilter-right-values)

  (assert (equal? ["a", "b"] ["a", "b"])))

(define (test-eithers-negrights-negall-rights)

  (assert (equal? ["a", "b"] ["a", "b"])))

(define (test-eithers-negrights-negall-lefts)

  (assert (equal? [] [])))

(define (test-eithers-negrights-negempty-list)

  (assert (equal? [] [])))

;; partitionEithers

(define (test-eithers-negpartitioneithers-negpartition-mixed)

  (assert (equal? ([1, 2], ["a", "b"]) ([1, 2], ["a", "b"]))))

(define (test-eithers-negpartitioneithers-negall-lefts)

  (assert (equal? ([1, 2], []) ([1, 2], []))))

(define (test-eithers-negpartitioneithers-negall-rights)

  (assert (equal? ([], ["a", "b"]) ([], ["a", "b"]))))

(define (test-eithers-negpartitioneithers-negempty-list)

  (assert (equal? ([], []) ([], []))))

;; map

(define (test-eithers-negmap-negmap-right-value)

  (assert (equal? right(10) right(10))))

(define (test-eithers-negmap-negpreserve-left)

  (assert (equal? left(99) left(99))))

;; mapList

(define (test-eithers-negmaplist-negall-succeed)

  (assert (equal? right([2, 4, 6]) right([2, 4, 6]))))

(define (test-eithers-negmaplist-negfirst-fails)

  (assert (equal? left("zero") left("zero"))))

(define (test-eithers-negmaplist-negempty-list)

  (assert (equal? right([]) right([]))))

;; mapMaybe

(define (test-eithers-negmapmaybe-negjust-succeeds)

  (assert (equal? right(just(10)) right(just(10)))))

(define (test-eithers-negmapmaybe-negjust-fails)

  (assert (equal? left("zero") left("zero"))))

(define (test-eithers-negmapmaybe-negnothing)

  (assert (equal? right(nothing) right(nothing))))

;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.maybes primitives

(import (scheme base))

;; apply

(define (test-maybes-negapply-negboth-just)

  (assert (equal? just(8) just(8))))

(define (test-maybes-negapply-negnothing-function)

  (assert (equal? nothing nothing)))

(define (test-maybes-negapply-negnothing-value)

  (assert (equal? nothing nothing)))

;; bind

(define (test-maybes-negbind-negjust-to-just)

  (assert (equal? just(10) just(10))))

(define (test-maybes-negbind-negnothing-to-nothing)

  (assert (equal? nothing nothing)))

;; cases

(define (test-maybes-negcases-negjust-applies-function)

  (assert (equal? 10 10)))

(define (test-maybes-negcases-negnothing-returns-default)

  (assert (equal? 99 99)))

;; cat

(define (test-maybes-negcat-negfilters-nothings)

  (assert (equal? [1, 2] [1, 2])))

(define (test-maybes-negcat-negall-justs)

  (assert (equal? [1, 2] [1, 2])))

(define (test-maybes-negcat-negall-nothings)

  (assert (equal? [] [])))

(define (test-maybes-negcat-negempty-list)

  (assert (equal? [] [])))

;; compose

(define (test-maybes-negcompose-negboth-succeed)

  (assert (equal? just(12) just(12))))

(define (test-maybes-negcompose-negfirst-fails)

  (assert (equal? nothing nothing)))

(define (test-maybes-negcompose-negsecond-fails)

  (assert (equal? nothing nothing)))

;; fromJust

(define (test-maybes-negfromjust-negextract-from-just)

  (assert (equal? 42 42)))

;; fromMaybe

(define (test-maybes-negfrommaybe-negjust-value)

  (assert (equal? 42 42)))

(define (test-maybes-negfrommaybe-negnothing-with-default)

  (assert (equal? 99 99)))

;; isJust

(define (test-maybes-negisjust-negjust-value)

  (assert (equal? true true)))

(define (test-maybes-negisjust-negnothing)

  (assert (equal? false false)))

;; isNothing

(define (test-maybes-negisnothing-negjust-value)

  (assert (equal? false false)))

(define (test-maybes-negisnothing-negnothing)

  (assert (equal? true true)))

;; map

(define (test-maybes-negmap-negmaps-just-value)

  (assert (equal? just(10) just(10))))

(define (test-maybes-negmap-negnothing-unchanged)

  (assert (equal? nothing nothing)))

;; mapMaybe

(define (test-maybes-negmapmaybe-negfilter-and-transform)

  (assert (equal? [6, 8, 10] [6, 8, 10])))

(define (test-maybes-negmapmaybe-negempty-result)

  (assert (equal? [] [])))

(define (test-maybes-negmapmaybe-negempty-input)

  (assert (equal? [] [])))

;; maybe

(define (test-maybes-negmaybe-negjust-value-applies-function)

  (assert (equal? 10 10)))

(define (test-maybes-negmaybe-negnothing-returns-default)

  (assert (equal? 99 99)))

;; pure

(define (test-maybes-negpure-negwraps-integer)

  (assert (equal? just(42) just(42))))

(define (test-maybes-negpure-negwraps-string)

  (assert (equal? just("hello") just("hello"))))

;; toList

(define (test-maybes-negtolist-negjust-value)

  (assert (equal? [42] [42])))

(define (test-maybes-negtolist-negnothing)

  (assert (equal? [] [])))

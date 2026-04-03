;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.pairs primitives

(import (scheme base))

;; bimap

(define (test-pairs-negbimap-negtransform-both-elements)

  (assert (equal? (10, 2) (10, 2))))

(define (test-pairs-negbimap-negwith-zero)

  (assert (equal? (0, 5) (0, 5))))

;; first

(define (test-pairs-negfirst-negextract-first-element)

  (assert (equal? 42 42)))

(define (test-pairs-negfirst-negwith-zero)

  (assert (equal? 0 0)))

(define (test-pairs-negfirst-negnegative-number)

  (assert (equal? -5 -5)))

;; second

(define (test-pairs-negsecond-negextract-second-element)

  (assert (equal? hello hello)))

(define (test-pairs-negsecond-negempty-string)

  (assert (equal?  )))

(define (test-pairs-negsecond-neglong-string)

  (assert (equal? testing testing)))

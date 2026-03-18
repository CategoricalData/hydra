;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

(import (scheme base))

;; compare

(define (test-compare-negless-than)

  (assert (equal? (list :less_than '()) ((hydra_lib_equality_compare 3) 5))))

(define (test-compare-negequal)

  (assert (equal? (list :equal_to '()) ((hydra_lib_equality_compare 5) 5))))

(define (test-compare-neggreater-than)

  (assert (equal? (list :greater_than '()) ((hydra_lib_equality_compare 5) 3))))

;; equal

(define (test-equal-negequal-integers)

  (assert (equal? #t ((hydra_lib_equality_equal 5) 5))))

(define (test-equal-negunequal-integers)

  (assert (equal? #f ((hydra_lib_equality_equal 5) 3))))

;; gt

(define (test-gt-neggreater)

  (assert (equal? #t ((hydra_lib_equality_gt 5) 3))))

(define (test-gt-negequal)

  (assert (equal? #f ((hydra_lib_equality_gt 5) 5))))

(define (test-gt-negless)

  (assert (equal? #f ((hydra_lib_equality_gt 3) 5))))

;; gte

(define (test-gte-neggreater)

  (assert (equal? #t ((hydra_lib_equality_gte 5) 3))))

(define (test-gte-negequal)

  (assert (equal? #t ((hydra_lib_equality_gte 5) 5))))

(define (test-gte-negless)

  (assert (equal? #f ((hydra_lib_equality_gte 3) 5))))

;; identity

(define (test-identity-neginteger)

  (assert (equal? 42 (hydra_lib_equality_identity 42))))

;; lt

(define (test-lt-negless)

  (assert (equal? #t ((hydra_lib_equality_lt 3) 5))))

(define (test-lt-negequal)

  (assert (equal? #f ((hydra_lib_equality_lt 5) 5))))

(define (test-lt-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lt 5) 3))))

;; lte

(define (test-lte-negless)

  (assert (equal? #t ((hydra_lib_equality_lte 3) 5))))

(define (test-lte-negequal)

  (assert (equal? #t ((hydra_lib_equality_lte 5) 5))))

(define (test-lte-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lte 5) 3))))

;; max

(define (test-max-negfirst-greater)

  (assert (equal? 5 ((hydra_lib_equality_max 5) 3))))

(define (test-max-negsecond-greater)

  (assert (equal? 5 ((hydra_lib_equality_max 3) 5))))

(define (test-max-negequal)

  (assert (equal? 5 ((hydra_lib_equality_max 5) 5))))

;; min

(define (test-min-negfirst-less)

  (assert (equal? 3 ((hydra_lib_equality_min 3) 5))))

(define (test-min-negsecond-less)

  (assert (equal? 3 ((hydra_lib_equality_min 5) 3))))

(define (test-min-negequal)

  (assert (equal? 5 ((hydra_lib_equality_min 5) 5))))

;; compare strings

(define (test-compare-strings-negless-than-lexicographic)

  (assert (equal? (list :less_than '()) ((hydra_lib_equality_compare "apple") "banana"))))

(define (test-compare-strings-negequal)

  (assert (equal? (list :equal_to '()) ((hydra_lib_equality_compare "hello") "hello"))))

(define (test-compare-strings-neggreater-than-lexicographic)

  (assert (equal? (list :greater_than '()) ((hydra_lib_equality_compare "zebra") "apple"))))

(define (test-compare-strings-negempty-vs-non-negempty)

  (assert (equal? (list :less_than '()) ((hydra_lib_equality_compare "") "a"))))

(define (test-compare-strings-negprefix-vs-longer)

  (assert (equal? (list :less_than '()) ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(define (test-lt-strings-negless-lexicographic)

  (assert (equal? #t ((hydra_lib_equality_lt "apple") "banana"))))

(define (test-lt-strings-negequal)

  (assert (equal? #f ((hydra_lib_equality_lt "hello") "hello"))))

(define (test-lt-strings-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(define (test-gt-strings-neggreater-lexicographic)

  (assert (equal? #t ((hydra_lib_equality_gt "zebra") "apple"))))

(define (test-gt-strings-negequal)

  (assert (equal? #f ((hydra_lib_equality_gt "hello") "hello"))))

(define (test-gt-strings-negless)

  (assert (equal? #f ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(define (test-max-strings-negfirst-greater)

  (assert (equal? "zebra" ((hydra_lib_equality_max "zebra") "apple"))))

(define (test-max-strings-negsecond-greater)

  (assert (equal? "zebra" ((hydra_lib_equality_max "apple") "zebra"))))

(define (test-max-strings-negequal)

  (assert (equal? "hello" ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(define (test-min-strings-negfirst-less)

  (assert (equal? "apple" ((hydra_lib_equality_min "apple") "zebra"))))

(define (test-min-strings-negsecond-less)

  (assert (equal? "apple" ((hydra_lib_equality_min "zebra") "apple"))))

(define (test-min-strings-negequal)

  (assert (equal? "hello" ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(define (test-compare-floats-negless-than)

  (assert (equal? (list :less_than '()) ((hydra_lib_equality_compare 1.5) 2.5))))

(define (test-compare-floats-negequal)

  (assert (equal? (list :equal_to '()) ((hydra_lib_equality_compare 3.14) 3.14))))

(define (test-compare-floats-neggreater-than)

  (assert (equal? (list :greater_than '()) ((hydra_lib_equality_compare 5.0) 3.0))))

(define (test-compare-floats-negnegative-vs-positive)

  (assert (equal? (list :less_than '()) ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(define (test-lt-floats-negless)

  (assert (equal? #t ((hydra_lib_equality_lt 1.5) 2.5))))

(define (test-lt-floats-negequal)

  (assert (equal? #f ((hydra_lib_equality_lt 3.14) 3.14))))

(define (test-lt-floats-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(define (test-gt-floats-neggreater)

  (assert (equal? #t ((hydra_lib_equality_gt 5.0) 3.0))))

(define (test-gt-floats-negequal)

  (assert (equal? #f ((hydra_lib_equality_gt 3.14) 3.14))))

(define (test-gt-floats-negless)

  (assert (equal? #f ((hydra_lib_equality_gt 1.5) 2.5))))

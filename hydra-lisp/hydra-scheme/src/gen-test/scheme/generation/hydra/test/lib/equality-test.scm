;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

(import (scheme base))

;; compare

(define (test-equality-negcompare-negless-than)

  (assert (equal? (list 'less_than '()) ((hydra_lib_equality_compare 3) 5))))

(define (test-equality-negcompare-negequal)

  (assert (equal? (list 'equal_to '()) ((hydra_lib_equality_compare 5) 5))))

(define (test-equality-negcompare-neggreater-than)

  (assert (equal? (list 'greater_than '()) ((hydra_lib_equality_compare 5) 3))))

;; equal

(define (test-equality-negequal-negequal-integers)

  (assert (equal? #t ((hydra_lib_equality_equal 5) 5))))

(define (test-equality-negequal-negunequal-integers)

  (assert (equal? #f ((hydra_lib_equality_equal 5) 3))))

;; gt

(define (test-equality-neggt-neggreater)

  (assert (equal? #t ((hydra_lib_equality_gt 5) 3))))

(define (test-equality-neggt-negequal)

  (assert (equal? #f ((hydra_lib_equality_gt 5) 5))))

(define (test-equality-neggt-negless)

  (assert (equal? #f ((hydra_lib_equality_gt 3) 5))))

;; gte

(define (test-equality-neggte-neggreater)

  (assert (equal? #t ((hydra_lib_equality_gte 5) 3))))

(define (test-equality-neggte-negequal)

  (assert (equal? #t ((hydra_lib_equality_gte 5) 5))))

(define (test-equality-neggte-negless)

  (assert (equal? #f ((hydra_lib_equality_gte 3) 5))))

;; identity

(define (test-equality-negidentity-neginteger)

  (assert (equal? 42 (hydra_lib_equality_identity 42))))

;; lt

(define (test-equality-neglt-negless)

  (assert (equal? #t ((hydra_lib_equality_lt 3) 5))))

(define (test-equality-neglt-negequal)

  (assert (equal? #f ((hydra_lib_equality_lt 5) 5))))

(define (test-equality-neglt-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lt 5) 3))))

;; lte

(define (test-equality-neglte-negless)

  (assert (equal? #t ((hydra_lib_equality_lte 3) 5))))

(define (test-equality-neglte-negequal)

  (assert (equal? #t ((hydra_lib_equality_lte 5) 5))))

(define (test-equality-neglte-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lte 5) 3))))

;; max

(define (test-equality-negmax-negfirst-greater)

  (assert (equal? 5 ((hydra_lib_equality_max 5) 3))))

(define (test-equality-negmax-negsecond-greater)

  (assert (equal? 5 ((hydra_lib_equality_max 3) 5))))

(define (test-equality-negmax-negequal)

  (assert (equal? 5 ((hydra_lib_equality_max 5) 5))))

;; min

(define (test-equality-negmin-negfirst-less)

  (assert (equal? 3 ((hydra_lib_equality_min 3) 5))))

(define (test-equality-negmin-negsecond-less)

  (assert (equal? 3 ((hydra_lib_equality_min 5) 3))))

(define (test-equality-negmin-negequal)

  (assert (equal? 5 ((hydra_lib_equality_min 5) 5))))

;; compare strings

(define (test-equality-negcompare-strings-negless-than-lexicographic)

  (assert (equal? (list 'less_than '()) ((hydra_lib_equality_compare "apple") "banana"))))

(define (test-equality-negcompare-strings-negequal)

  (assert (equal? (list 'equal_to '()) ((hydra_lib_equality_compare "hello") "hello"))))

(define (test-equality-negcompare-strings-neggreater-than-lexicographic)

  (assert (equal? (list 'greater_than '()) ((hydra_lib_equality_compare "zebra") "apple"))))

(define (test-equality-negcompare-strings-negempty-vs-non-negempty)

  (assert (equal? (list 'less_than '()) ((hydra_lib_equality_compare "") "a"))))

(define (test-equality-negcompare-strings-negprefix-vs-longer)

  (assert (equal? (list 'less_than '()) ((hydra_lib_equality_compare "ab") "abc"))))

;; lt strings

(define (test-equality-neglt-strings-negless-lexicographic)

  (assert (equal? #t ((hydra_lib_equality_lt "apple") "banana"))))

(define (test-equality-neglt-strings-negequal)

  (assert (equal? #f ((hydra_lib_equality_lt "hello") "hello"))))

(define (test-equality-neglt-strings-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lt "zebra") "apple"))))

;; gt strings

(define (test-equality-neggt-strings-neggreater-lexicographic)

  (assert (equal? #t ((hydra_lib_equality_gt "zebra") "apple"))))

(define (test-equality-neggt-strings-negequal)

  (assert (equal? #f ((hydra_lib_equality_gt "hello") "hello"))))

(define (test-equality-neggt-strings-negless)

  (assert (equal? #f ((hydra_lib_equality_gt "apple") "banana"))))

;; max strings

(define (test-equality-negmax-strings-negfirst-greater)

  (assert (equal? "zebra" ((hydra_lib_equality_max "zebra") "apple"))))

(define (test-equality-negmax-strings-negsecond-greater)

  (assert (equal? "zebra" ((hydra_lib_equality_max "apple") "zebra"))))

(define (test-equality-negmax-strings-negequal)

  (assert (equal? "hello" ((hydra_lib_equality_max "hello") "hello"))))

;; min strings

(define (test-equality-negmin-strings-negfirst-less)

  (assert (equal? "apple" ((hydra_lib_equality_min "apple") "zebra"))))

(define (test-equality-negmin-strings-negsecond-less)

  (assert (equal? "apple" ((hydra_lib_equality_min "zebra") "apple"))))

(define (test-equality-negmin-strings-negequal)

  (assert (equal? "hello" ((hydra_lib_equality_min "hello") "hello"))))

;; compare floats

(define (test-equality-negcompare-floats-negless-than)

  (assert (equal? (list 'less_than '()) ((hydra_lib_equality_compare 1.5) 2.5))))

(define (test-equality-negcompare-floats-negequal)

  (assert (equal? (list 'equal_to '()) ((hydra_lib_equality_compare 3.14) 3.14))))

(define (test-equality-negcompare-floats-neggreater-than)

  (assert (equal? (list 'greater_than '()) ((hydra_lib_equality_compare 5.0) 3.0))))

(define (test-equality-negcompare-floats-negnegative-vs-positive)

  (assert (equal? (list 'less_than '()) ((hydra_lib_equality_compare -1.0) 1.0))))

;; lt floats

(define (test-equality-neglt-floats-negless)

  (assert (equal? #t ((hydra_lib_equality_lt 1.5) 2.5))))

(define (test-equality-neglt-floats-negequal)

  (assert (equal? #f ((hydra_lib_equality_lt 3.14) 3.14))))

(define (test-equality-neglt-floats-neggreater)

  (assert (equal? #f ((hydra_lib_equality_lt 5.0) 3.0))))

;; gt floats

(define (test-equality-neggt-floats-neggreater)

  (assert (equal? #t ((hydra_lib_equality_gt 5.0) 3.0))))

(define (test-equality-neggt-floats-negequal)

  (assert (equal? #f ((hydra_lib_equality_gt 3.14) 3.14))))

(define (test-equality-neggt-floats-negless)

  (assert (equal? #f ((hydra_lib_equality_gt 1.5) 2.5))))

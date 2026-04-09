;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.equality primitives

(import (scheme base))

;; compare

(define (test-equality-negcompare-negless-than)

  (assert (equal? inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(define (test-equality-negcompare-negequal)

  (assert (equal? inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(define (test-equality-negcompare-neggreater-than)

  (assert (equal? inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

;; equal

(define (test-equality-negequal-negequal-integers)

  (assert (equal? true true)))

(define (test-equality-negequal-negunequal-integers)

  (assert (equal? false false)))

;; gt

(define (test-equality-neggt-neggreater)

  (assert (equal? true true)))

(define (test-equality-neggt-negequal)

  (assert (equal? false false)))

(define (test-equality-neggt-negless)

  (assert (equal? false false)))

;; gte

(define (test-equality-neggte-neggreater)

  (assert (equal? true true)))

(define (test-equality-neggte-negequal)

  (assert (equal? true true)))

(define (test-equality-neggte-negless)

  (assert (equal? false false)))

;; identity

(define (test-equality-negidentity-neginteger)

  (assert (equal? 42:int32 42:int32)))

;; lt

(define (test-equality-neglt-negless)

  (assert (equal? true true)))

(define (test-equality-neglt-negequal)

  (assert (equal? false false)))

(define (test-equality-neglt-neggreater)

  (assert (equal? false false)))

;; lte

(define (test-equality-neglte-negless)

  (assert (equal? true true)))

(define (test-equality-neglte-negequal)

  (assert (equal? true true)))

(define (test-equality-neglte-neggreater)

  (assert (equal? false false)))

;; max

(define (test-equality-negmax-negfirst-greater)

  (assert (equal? 5:int32 5:int32)))

(define (test-equality-negmax-negsecond-greater)

  (assert (equal? 5:int32 5:int32)))

(define (test-equality-negmax-negequal)

  (assert (equal? 5:int32 5:int32)))

;; min

(define (test-equality-negmin-negfirst-less)

  (assert (equal? 3:int32 3:int32)))

(define (test-equality-negmin-negsecond-less)

  (assert (equal? 3:int32 3:int32)))

(define (test-equality-negmin-negequal)

  (assert (equal? 5:int32 5:int32)))

;; compare strings

(define (test-equality-negcompare-strings-negless-than-lexicographic)

  (assert (equal? inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(define (test-equality-negcompare-strings-negequal)

  (assert (equal? inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(define (test-equality-negcompare-strings-neggreater-than-lexicographic)

  (assert (equal? inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

(define (test-equality-negcompare-strings-negempty-vs-non-negempty)

  (assert (equal? inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(define (test-equality-negcompare-strings-negprefix-vs-longer)

  (assert (equal? inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

;; lt strings

(define (test-equality-neglt-strings-negless-lexicographic)

  (assert (equal? true true)))

(define (test-equality-neglt-strings-negequal)

  (assert (equal? false false)))

(define (test-equality-neglt-strings-neggreater)

  (assert (equal? false false)))

;; gt strings

(define (test-equality-neggt-strings-neggreater-lexicographic)

  (assert (equal? true true)))

(define (test-equality-neggt-strings-negequal)

  (assert (equal? false false)))

(define (test-equality-neggt-strings-negless)

  (assert (equal? false false)))

;; max strings

(define (test-equality-negmax-strings-negfirst-greater)

  (assert (equal? "zebra" "zebra")))

(define (test-equality-negmax-strings-negsecond-greater)

  (assert (equal? "zebra" "zebra")))

(define (test-equality-negmax-strings-negequal)

  (assert (equal? "hello" "hello")))

;; min strings

(define (test-equality-negmin-strings-negfirst-less)

  (assert (equal? "apple" "apple")))

(define (test-equality-negmin-strings-negsecond-less)

  (assert (equal? "apple" "apple")))

(define (test-equality-negmin-strings-negequal)

  (assert (equal? "hello" "hello")))

;; compare floats

(define (test-equality-negcompare-floats-negless-than)

  (assert (equal? inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

(define (test-equality-negcompare-floats-negequal)

  (assert (equal? inject(hydra.util.Comparison){equalTo=unit} inject(hydra.util.Comparison){equalTo=unit})))

(define (test-equality-negcompare-floats-neggreater-than)

  (assert (equal? inject(hydra.util.Comparison){greaterThan=unit} inject(hydra.util.Comparison){greaterThan=unit})))

(define (test-equality-negcompare-floats-negnegative-vs-positive)

  (assert (equal? inject(hydra.util.Comparison){lessThan=unit} inject(hydra.util.Comparison){lessThan=unit})))

;; lt floats

(define (test-equality-neglt-floats-negless)

  (assert (equal? true true)))

(define (test-equality-neglt-floats-negequal)

  (assert (equal? false false)))

(define (test-equality-neglt-floats-neggreater)

  (assert (equal? false false)))

;; gt floats

(define (test-equality-neggt-floats-neggreater)

  (assert (equal? true true)))

(define (test-equality-neggt-floats-negequal)

  (assert (equal? false false)))

(define (test-equality-neggt-floats-negless)

  (assert (equal? false false)))

;; Note: this is an automatically generated file. Do not edit.
;; hydra.lib.chars primitives

(import (scheme base))

;; isAlphaNum

(define (test-chars-negisalphanum-negletter)

  (assert (equal? true true)))

(define (test-chars-negisalphanum-negdigit)

  (assert (equal? true true)))

(define (test-chars-negisalphanum-negspace)

  (assert (equal? false false)))

(define (test-chars-negisalphanum-negpunctuation)

  (assert (equal? false false)))

;; isLower

(define (test-chars-negislower-neglowercase)

  (assert (equal? true true)))

(define (test-chars-negislower-neguppercase)

  (assert (equal? false false)))

(define (test-chars-negislower-negdigit)

  (assert (equal? false false)))

;; isSpace

(define (test-chars-negisspace-negspace)

  (assert (equal? true true)))

(define (test-chars-negisspace-negtab)

  (assert (equal? true true)))

(define (test-chars-negisspace-negnewline)

  (assert (equal? true true)))

(define (test-chars-negisspace-negletter)

  (assert (equal? false false)))

;; isUpper

(define (test-chars-negisupper-neguppercase)

  (assert (equal? true true)))

(define (test-chars-negisupper-neglowercase)

  (assert (equal? false false)))

(define (test-chars-negisupper-negdigit)

  (assert (equal? false false)))

;; toLower

(define (test-chars-negtolower-neguppercase)

  (assert (equal? 97:int32 97:int32)))

(define (test-chars-negtolower-neglowercase)

  (assert (equal? 97:int32 97:int32)))

(define (test-chars-negtolower-negdigit)

  (assert (equal? 53:int32 53:int32)))

;; toUpper

(define (test-chars-negtoupper-neglowercase)

  (assert (equal? 65:int32 65:int32)))

(define (test-chars-negtoupper-neguppercase)

  (assert (equal? 65:int32 65:int32)))

(define (test-chars-negtoupper-negdigit)

  (assert (equal? 53:int32 53:int32)))

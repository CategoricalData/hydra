;; Note: this is an automatically generated file. Do not edit.
;; serialization

(import (scheme base))

;; associativity

(define (test-serialization-negassociativity-negright-negassociative-operator)

  (assert (equal? (a -> b) -> c -> d (a -> b) -> c -> d)))

;; case statements

(define (test-serialization-negcase-statements-negsimple-case-statement)

  (assert (equal? case x > 42 of
  False -> Big
  True -> Small case x > 42 of
  False -> Big
  True -> Small)))

(define (test-serialization-negcase-statements-negnested-case-statement)

  (assert (equal? case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small)))

;; lambdas

(define (test-serialization-neglambdas-negsimple-lambda)

  (assert (equal? \x y -> x + y \x y -> x + y)))

;; lists

(define (test-serialization-neglists-negempty-list)

  (assert (equal? [] [])))

(define (test-serialization-neglists-negsimple-non-negempty-list)

  (assert (equal? [1, 2, 3] [1, 2, 3])))

(define (test-serialization-neglists-negnested-list)

  (assert (equal? [[1, 3], 2] [[1, 3], 2])))

(define (test-serialization-neglists-neglist-with-parenthesized-expression-inside)

  (assert (equal? [[1, (2 + 3) * (1 + 10)], 2] [[1, (2 + 3) * (1 + 10)], 2])))

;; precedence

(define (test-serialization-negprecedence-negoperators-with-different-precedence--neg-no-parens-needed)

  (assert (equal? 2 * 3 + 1 * 10 2 * 3 + 1 * 10)))

(define (test-serialization-negprecedence-negoperators-with-different-precedence--neg-parens-needed)

  (assert (equal? (2 + 3) * (1 + 10) (2 + 3) * (1 + 10))))

(define (test-serialization-negprecedence-negassociative-operator-left-nesting)

  (assert (equal? x * y * z x * y * z)))

(define (test-serialization-negprecedence-negassociative-operator-right-nesting)

  (assert (equal? x * y * z x * y * z)))

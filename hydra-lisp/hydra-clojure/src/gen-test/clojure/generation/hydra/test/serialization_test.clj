;; Note: this is an automatically generated file. Do not edit.
;; serialization

(ns test-ns
  (:require [clojure.test :refer :all]))

;; associativity

(deftest test-serialization-negassociativity-negright-negassociative-operator

  (is (= (a -> b) -> c -> d

         (a -> b) -> c -> d)))

;; case statements

(deftest test-serialization-negcase-statements-negsimple-case-statement

  (is (= case x > 42 of
  False -> Big
  True -> Small

         case x > 42 of
  False -> Big
  True -> Small)))

(deftest test-serialization-negcase-statements-negnested-case-statement

  (is (= case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small

         case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small)))

;; lambdas

(deftest test-serialization-neglambdas-negsimple-lambda

  (is (= \x y -> x + y

         \x y -> x + y)))

;; lists

(deftest test-serialization-neglists-negempty-list

  (is (= []

         [])))

(deftest test-serialization-neglists-negsimple-non-negempty-list

  (is (= [1, 2, 3]

         [1, 2, 3])))

(deftest test-serialization-neglists-negnested-list

  (is (= [[1, 3], 2]

         [[1, 3], 2])))

(deftest test-serialization-neglists-neglist-with-parenthesized-expression-inside

  (is (= [[1, (2 + 3) * (1 + 10)], 2]

         [[1, (2 + 3) * (1 + 10)], 2])))

;; precedence

(deftest test-serialization-negprecedence-negoperators-with-different-precedence--neg-no-parens-needed

  (is (= 2 * 3 + 1 * 10

         2 * 3 + 1 * 10)))

(deftest test-serialization-negprecedence-negoperators-with-different-precedence--neg-parens-needed

  (is (= (2 + 3) * (1 + 10)

         (2 + 3) * (1 + 10))))

(deftest test-serialization-negprecedence-negassociative-operator-left-nesting

  (is (= x * y * z

         x * y * z)))

(deftest test-serialization-negprecedence-negassociative-operator-right-nesting

  (is (= x * y * z

         x * y * z)))

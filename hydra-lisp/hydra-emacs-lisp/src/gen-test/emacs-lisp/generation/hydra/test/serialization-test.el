;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; serialization

(require 'ert)

;; associativity

(ert-deftest test-serialization-negassociativity-negright-negassociative-operator ()

  (should (equal (a -> b) -> c -> d (a -> b) -> c -> d)))

;; case statements

(ert-deftest test-serialization-negcase-statements-negsimple-case-statement ()

  (should (equal case x > 42 of
  False -> Big
  True -> Small case x > 42 of
  False -> Big
  True -> Small)))

(ert-deftest test-serialization-negcase-statements-negnested-case-statement ()

  (should (equal case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small)))

;; lambdas

(ert-deftest test-serialization-neglambdas-negsimple-lambda ()

  (should (equal \x y -> x + y \x y -> x + y)))

;; lists

(ert-deftest test-serialization-neglists-negempty-list ()

  (should (equal [] [])))

(ert-deftest test-serialization-neglists-negsimple-non-negempty-list ()

  (should (equal [1, 2, 3] [1, 2, 3])))

(ert-deftest test-serialization-neglists-negnested-list ()

  (should (equal [[1, 3], 2] [[1, 3], 2])))

(ert-deftest test-serialization-neglists-neglist-with-parenthesized-expression-inside ()

  (should (equal [[1, (2 + 3) * (1 + 10)], 2] [[1, (2 + 3) * (1 + 10)], 2])))

;; precedence

(ert-deftest test-serialization-negprecedence-negoperators-with-different-precedence--neg-no-parens-needed ()

  (should (equal 2 * 3 + 1 * 10 2 * 3 + 1 * 10)))

(ert-deftest test-serialization-negprecedence-negoperators-with-different-precedence--neg-parens-needed ()

  (should (equal (2 + 3) * (1 + 10) (2 + 3) * (1 + 10))))

(ert-deftest test-serialization-negprecedence-negassociative-operator-left-nesting ()

  (should (equal x * y * z x * y * z)))

(ert-deftest test-serialization-negprecedence-negassociative-operator-right-nesting ()

  (should (equal x * y * z x * y * z)))

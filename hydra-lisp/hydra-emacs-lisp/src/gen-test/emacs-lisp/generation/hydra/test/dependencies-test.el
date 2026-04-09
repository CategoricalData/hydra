;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; dependencies

(require 'ert)

;; simplifyTerm

(ert-deftest test-dependencies-negsimplifyterm-negconst-application-with-literal ()

  (should (equal "foo" "foo")))

(ert-deftest test-dependencies-negsimplifyterm-negidentity-application ()

  (should (equal [y, y] [y, y])))

(ert-deftest test-dependencies-negsimplifyterm-negunused-parameter ()

  (should (equal "foo" "foo")))

(ert-deftest test-dependencies-negsimplifyterm-negnested-lambda-applications ()

  (should (equal ["foo", y] ["foo", y])))

;; flattenLetTerms

(ert-deftest test-dependencies-negflattenletterms-negnon-neglet-term-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-dependencies-negflattenletterms-neglist-term-unchanged ()

  (should (equal ["foo"] ["foo"])))

(ert-deftest test-dependencies-negflattenletterms-negsequential-lets-in-body-are-flattened ()

  (should (equal let x = 1:int32, y = 2:int32 in [x, y] let x = 1:int32, y = 2:int32 in [x, y])))

(ert-deftest test-dependencies-negflattenletterms-negnested-binding-in-let-value-is-flattened ()

  (should (equal let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b])))

(ert-deftest test-dependencies-negflattenletterms-negmultiple-levels-of-nesting-are-flattened ()

  (should (equal let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b])))

;; liftLambdaAboveLet

(ert-deftest test-dependencies-negliftlambdaabovelet-negsimple-let-with-lambda-in-body ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negbare-lambda-unchanged ()

  (should (equal λx.x λx.x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negbare-let-unchanged ()

  (should (equal let x = 42:int32 in x let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglambda-with-let-in-body-unchanged ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-with-two-nested-lambdas ()

  (should (equal λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglambda-inside-let-body-already-above-let ()

  (should (equal λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-without-lambda-in-body-unchanged ()

  (should (equal let x = 42:int32, y = "hello" in (x, y) let x = 42:int32, y = "hello" in (x, y))))

(ert-deftest test-dependencies-negliftlambdaabovelet-negmultiple-let-bindings-with-lambda ()

  (should (equal λz.let x = 42:int32, y = "hello" in x λz.let x = 42:int32, y = "hello" in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negnested-lets-with-lambda-at-innermost-level ()

  (should (equal λz.let x = 42:int32 in let y = "hello" in x λz.let x = 42:int32 in let y = "hello" in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglambda-between-two-lets ()

  (should (equal λy.let x = 42:int32 in let z = "hello" in x λy.let x = 42:int32 in let z = "hello" in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negmultiple-lambdas-between-nested-lets ()

  (should (equal λx.λy.let a = 1:int32 in let b = 2:int32 in a λx.λy.let a = 1:int32 in let b = 2:int32 in a)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negmultiple-lambdas-already-above-let ()

  (should (equal λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negannotation-above-let-containing-lambda ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negannotation-above-lambda-in-let-body ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negannotation-between-two-lambdas ()

  (should (equal λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negannotation-on-the-body-of-lambda-in-let ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-negannotation-on-lambda-already-above-let ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-a-list ()

  (should (equal [1:int32, λy.let x = 42:int32 in x, 2:int32] [1:int32, λy.let x = 42:int32 in x, 2:int32])))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-multiple-list-elements ()

  (should (equal [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z] [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z])))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-a-let-binding-value ()

  (should (equal let f = λy.let x = 42:int32 in x in f let f = λy.let x = 42:int32 in x in f)))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-a-pair ()

  (should (equal (λy.let x = 42:int32 in x, "test") (λy.let x = 42:int32 in x, "test"))))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-both-elements-of-a-pair ()

  (should (equal (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z) (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z))))

(ert-deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-lambda-body ()

  (should (equal λouter.λinner.let x = 42:int32 in x λouter.λinner.let x = 42:int32 in x)))

;; topologicalSortBindings

(ert-deftest test-dependencies-negtopologicalsortbindings-negisolated-bindings ()

  (should (equal [[(a, "foo")], [(b, "bar")]] [[(a, "foo")], [(b, "bar")]])))

(ert-deftest test-dependencies-negtopologicalsortbindings-negsingle-recursive-binding ()

  (should (equal [[(a, [a])]] [[(a, [a])]])))

(ert-deftest test-dependencies-negtopologicalsortbindings-negmutually-recursive-bindings ()

  (should (equal [[(a, [b]), (b, [a])]] [[(a, [b]), (b, [a])]])))

(ert-deftest test-dependencies-negtopologicalsortbindings-negmixed-bindings ()

  (should (equal [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]] [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]])))

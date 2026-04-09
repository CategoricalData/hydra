;; Note: this is an automatically generated file. Do not edit.
;; dependencies

(ns test-ns
  (:require [clojure.test :refer :all]))

;; simplifyTerm

(deftest test-dependencies-negsimplifyterm-negconst-application-with-literal

  (is (= "foo"

         "foo")))

(deftest test-dependencies-negsimplifyterm-negidentity-application

  (is (= [y, y]

         [y, y])))

(deftest test-dependencies-negsimplifyterm-negunused-parameter

  (is (= "foo"

         "foo")))

(deftest test-dependencies-negsimplifyterm-negnested-lambda-applications

  (is (= ["foo", y]

         ["foo", y])))

;; flattenLetTerms

(deftest test-dependencies-negflattenletterms-negnon-neglet-term-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-dependencies-negflattenletterms-neglist-term-unchanged

  (is (= ["foo"]

         ["foo"])))

(deftest test-dependencies-negflattenletterms-negsequential-lets-in-body-are-flattened

  (is (= let x = 1:int32, y = 2:int32 in [x, y]

         let x = 1:int32, y = 2:int32 in [x, y])))

(deftest test-dependencies-negflattenletterms-negnested-binding-in-let-value-is-flattened

  (is (= let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b]

         let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b])))

(deftest test-dependencies-negflattenletterms-negmultiple-levels-of-nesting-are-flattened

  (is (= let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b]

         let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b])))

;; liftLambdaAboveLet

(deftest test-dependencies-negliftlambdaabovelet-negsimple-let-with-lambda-in-body

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-negbare-lambda-unchanged

  (is (= λx.x

         λx.x)))

(deftest test-dependencies-negliftlambdaabovelet-negbare-let-unchanged

  (is (= let x = 42:int32 in x

         let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-neglambda-with-let-in-body-unchanged

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-neglet-with-two-nested-lambdas

  (is (= λy.λz.let x = 42:int32 in x

         λy.λz.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-neglambda-inside-let-body-already-above-let

  (is (= λx.λy.let z = 42:int32 in z

         λx.λy.let z = 42:int32 in z)))

(deftest test-dependencies-negliftlambdaabovelet-neglet-without-lambda-in-body-unchanged

  (is (= let x = 42:int32, y = "hello" in (x, y)

         let x = 42:int32, y = "hello" in (x, y))))

(deftest test-dependencies-negliftlambdaabovelet-negmultiple-let-bindings-with-lambda

  (is (= λz.let x = 42:int32, y = "hello" in x

         λz.let x = 42:int32, y = "hello" in x)))

(deftest test-dependencies-negliftlambdaabovelet-negnested-lets-with-lambda-at-innermost-level

  (is (= λz.let x = 42:int32 in let y = "hello" in x

         λz.let x = 42:int32 in let y = "hello" in x)))

(deftest test-dependencies-negliftlambdaabovelet-neglambda-between-two-lets

  (is (= λy.let x = 42:int32 in let z = "hello" in x

         λy.let x = 42:int32 in let z = "hello" in x)))

(deftest test-dependencies-negliftlambdaabovelet-negmultiple-lambdas-between-nested-lets

  (is (= λx.λy.let a = 1:int32 in let b = 2:int32 in a

         λx.λy.let a = 1:int32 in let b = 2:int32 in a)))

(deftest test-dependencies-negliftlambdaabovelet-negmultiple-lambdas-already-above-let

  (is (= λx.λy.let z = 42:int32 in z

         λx.λy.let z = 42:int32 in z)))

(deftest test-dependencies-negliftlambdaabovelet-negannotation-above-let-containing-lambda

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-negannotation-above-lambda-in-let-body

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-negannotation-between-two-lambdas

  (is (= λy.λz.let x = 42:int32 in x

         λy.λz.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-negannotation-on-the-body-of-lambda-in-let

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-negannotation-on-lambda-already-above-let

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-a-list

  (is (= [1:int32, λy.let x = 42:int32 in x, 2:int32]

         [1:int32, λy.let x = 42:int32 in x, 2:int32])))

(deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-multiple-list-elements

  (is (= [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z]

         [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z])))

(deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-a-let-binding-value

  (is (= let f = λy.let x = 42:int32 in x in f

         let f = λy.let x = 42:int32 in x in f)))

(deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-a-pair

  (is (= (λy.let x = 42:int32 in x, "test")

         (λy.let x = 42:int32 in x, "test"))))

(deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-both-elements-of-a-pair

  (is (= (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z)

         (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z))))

(deftest test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-lambda-body

  (is (= λouter.λinner.let x = 42:int32 in x

         λouter.λinner.let x = 42:int32 in x)))

;; topologicalSortBindings

(deftest test-dependencies-negtopologicalsortbindings-negisolated-bindings

  (is (= [[(a, "foo")], [(b, "bar")]]

         [[(a, "foo")], [(b, "bar")]])))

(deftest test-dependencies-negtopologicalsortbindings-negsingle-recursive-binding

  (is (= [[(a, [a])]]

         [[(a, [a])]])))

(deftest test-dependencies-negtopologicalsortbindings-negmutually-recursive-bindings

  (is (= [[(a, [b]), (b, [a])]]

         [[(a, [b]), (b, [a])]])))

(deftest test-dependencies-negtopologicalsortbindings-negmixed-bindings

  (is (= [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]]

         [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]])))

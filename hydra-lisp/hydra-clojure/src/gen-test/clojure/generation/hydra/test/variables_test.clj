;; Note: this is an automatically generated file. Do not edit.
;; variables

(ns test-ns
  (:require [clojure.test :refer :all]))

;; freeVariables

(deftest test-variables-negfreevariables-negstring-literal-has-no-free-variables

  (is (= {}

         {})))

(deftest test-variables-negfreevariables-negsingle-variable

  (is (= {x}

         {x})))

(deftest test-variables-negfreevariables-negbound-variable-is-not-free

  (is (= {}

         {})))

(deftest test-variables-negfreevariables-negunbound-variable-in-lambda-body

  (is (= {x}

         {x})))

(deftest test-variables-negfreevariables-negmixed-free-and-bound-variables

  (is (= {x}

         {x})))

(deftest test-variables-negfreevariables-negmultiple-free-variables

  (is (= {x, y}

         {x, y})))

;; normalizeTypeVariables

(deftest test-variables-negnormalizetypevariables-negliteral-without-type-variables-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-variables-negnormalizetypevariables-negsimple-let-without-type-annotations-unchanged

  (is (= let foo = "foo" in 42:int32

         let foo = "foo" in 42:int32)))

(deftest test-variables-negnormalizetypevariables-neglet-with-monomorphic-type-scheme-unchanged

  (is (= let foo:((string)) = "foo" in 42:int32

         let foo:((string)) = "foo" in 42:int32)))

(deftest test-variables-negnormalizetypevariables-neglet-with-monomorphic-binding-referencing-string

  (is (= let foo:((string)) = "foo" in 42:int32

         let foo:((string)) = "foo" in 42:int32)))

(deftest test-variables-negnormalizetypevariables-negpolymorphic-binding-with-free-type-variable-unchanged

  (is (= let foo:((a)) = bar in 42:int32

         let foo:((a)) = bar in 42:int32)))

(deftest test-variables-negnormalizetypevariables-negmonomorphic-binding-with-typed-lambda-unchanged

  (is (= let foo:((string)) = "foo" in λx:(a → int32).42:int32

         let foo:((string)) = "foo" in λx:(a → int32).42:int32)))

(deftest test-variables-negnormalizetypevariables-negpolymorphic-binding-with-typed-lambda-in-body-unchanged

  (is (= let foo:((a)) = bar in λx:(a → int32).42:int32

         let foo:((a)) = bar in λx:(a → int32).42:int32)))

(deftest test-variables-negnormalizetypevariables-negpolymorphic-identity-function-normalized

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(deftest test-variables-negnormalizetypevariables-negpolymorphic-const-function-normalized

  (is (= let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo")

         let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo"))))

(deftest test-variables-negnormalizetypevariables-negbinding-rewriting-does-not-affect-body-with-typed-lambda

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32

         let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32)))

(deftest test-variables-negnormalizetypevariables-negnested-polymorphic-lets-normalized

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32))

         let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)))))

(deftest test-variables-negnormalizetypevariables-negnested-same-substitution-in-bindings-and-environment

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(deftest test-variables-negnormalizetypevariables-negparent-type-variable-shadows-child-variable

  (is (= let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(deftest test-variables-negnormalizetypevariables-negno-shadowing-distinct-type-variables

  (is (= let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(deftest test-variables-negnormalizetypevariables-neglocally-free-type-variable-in-nested-binding

  (is (= let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32)

         let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32))))

;; unshadowVariables

(deftest test-variables-negunshadowvariables-negliteral-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-variables-negunshadowvariables-negvariable-unchanged

  (is (= x

         x)))

(deftest test-variables-negunshadowvariables-negsingle-lambda-unchanged

  (is (= λx.x

         λx.x)))

(deftest test-variables-negunshadowvariables-negdistinct-lambda-parameters-unchanged

  (is (= λx.λy.[x, y]

         λx.λy.[x, y])))

(deftest test-variables-negunshadowvariables-neglet-with-no-shadowing-unchanged

  (is (= let x = 1:int32 in x

         let x = 1:int32 in x)))

(deftest test-variables-negunshadowvariables-neglet-and-lambda-with-distinct-names-unchanged

  (is (= let x = 1:int32 in λy.[x, y]

         let x = 1:int32 in λy.[x, y])))

(deftest test-variables-negunshadowvariables-neginner-lambda-shadows-outer-lambda

  (is (= λx.λx2.x2

         λx.λx2.x2)))

(deftest test-variables-negunshadowvariables-neginner-lambda-shadows-outer--neg-body-references-both

  (is (= λx.[x, λx2.x2]

         λx.[x, λx2.x2])))

(deftest test-variables-negunshadowvariables-negtriple-nested-lambda-same-name

  (is (= λx.λx2.λx3.x3

         λx.λx2.λx3.x3)))

(deftest test-variables-negunshadowvariables-negtwo-parameters-shadow-sequentially

  (is (= λx.λy.λx2.λy2.[x2, y2]

         λx.λy.λx2.λy2.[x2, y2])))

(deftest test-variables-negunshadowvariables-neglambda-shadows-let-negbound-variable

  (is (= let x = 1:int32 in λx2.x2

         let x = 1:int32 in λx2.x2)))

(deftest test-variables-negunshadowvariables-neglambda-shadows-one-of-multiple-let-bindings

  (is (= let x = 1:int32, y = 2:int32 in λx2.[x2, y]

         let x = 1:int32, y = 2:int32 in λx2.[x2, y])))

(deftest test-variables-negunshadowvariables-neginner-let-body-with-lambda-shadowing-outer-let

  (is (= let x = 1:int32 in let y = 2:int32 in λx2.x2

         let x = 1:int32 in let y = 2:int32 in λx2.x2)))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-in-function-position-of-application

  (is (= λf.(λf2.f2 @ f)

         λf.(λf2.f2 @ f))))

(deftest test-variables-negunshadowvariables-negshadowed-lambdas-in-list-elements

  (is (= λx.[λx2.x2, λx2.x2]

         λx.[λx2.x2, λx2.x2])))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-in-record-field

  (is (= λx.record(Pair){fst=λx2.x2, snd=x}

         λx.record(Pair){fst=λx2.x2, snd=x})))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-in-case-branch

  (is (= λx.case(Maybe){nothing=0:int32, just=λx2.x2}

         λx.case(Maybe){nothing=0:int32, just=λx2.x2})))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-in-pair

  (is (= λx.(λx2.x2, x)

         λx.(λx2.x2, x))))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-optional

  (is (= λx.just(λx2.x2)

         λx.just(λx2.x2))))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-set-element

  (is (= λx.{λx2.x2}

         λx.{λx2.x2})))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-in-union-injection

  (is (= λx.inject(Result){ok=λx2.x2}

         λx.inject(Result){ok=λx2.x2})))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-wrapped-term

  (is (= λx.wrap(Age){λx2.x2}

         λx.wrap(Age){λx2.x2})))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-type-lambda

  (is (= λx.Λa.λx2.x2

         λx.Λa.λx2.x2)))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-type-application

  (is (= λx.λx2.x2⟨string⟩

         λx.λx2.x2⟨string⟩)))

(deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-annotated-term

  (is (= λx.λx2.x2

         λx.λx2.x2)))

(deftest test-variables-negunshadowvariables-negshadowing-at-multiple-depths

  (is (= λx.λy.λx2.λy2.[x2, y2]

         λx.λy.λx2.λy2.[x2, y2])))

(deftest test-variables-negunshadowvariables-neglet-then-lambda-then-lambda-all-same-name

  (is (= let x = 1:int32 in λx2.λx3.x3

         let x = 1:int32 in λx2.λx3.x3)))

(deftest test-variables-negunshadowvariables-neglambda-with-shadowing-in-let-binding-value

  (is (= λx.let y = λx2.x2 in (y @ x)

         λx.let y = λx2.x2 in (y @ x))))

(deftest test-variables-negunshadowvariables-negapplication-without-shadowing-unchanged

  (is (= (f @ 42:int32)

         (f @ 42:int32))))

(deftest test-variables-negunshadowvariables-neglist-of-literals-unchanged

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-variables-negunshadowvariables-negnested-record-unchanged

  (is (= record(Point){x=10:int32, y=20:int32}

         record(Point){x=10:int32, y=20:int32})))

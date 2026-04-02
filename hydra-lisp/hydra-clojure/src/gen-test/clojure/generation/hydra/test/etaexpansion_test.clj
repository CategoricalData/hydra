;; Note: this is an automatically generated file. Do not edit.
;; eta expansion

(ns test-ns
  (:require [clojure.test :refer :all]))

;; Partial application of primitives

;; Bare primitives are not expanded

(deftest test-etaexpansion-negpartial-application-of-primitives-negbare-primitives-are-not-expanded-negunary-primitive

  (is (= hydra.lib.strings.toLower!

         hydra.lib.strings.toLower!)))

(deftest test-etaexpansion-negpartial-application-of-primitives-negbare-primitives-are-not-expanded-negbinary-primitive

  (is (= hydra.lib.strings.splitOn!

         hydra.lib.strings.splitOn!)))

;; Partially applied primitives expand with lambdas

(deftest test-etaexpansion-negpartial-application-of-primitives-negpartially-applied-primitives-expand-with-lambdas-negbinary-primitive-with-one-argument

  (is (= λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1)

         λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1))))

(deftest test-etaexpansion-negpartial-application-of-primitives-negpartially-applied-primitives-expand-with-lambdas-negternary-primitive-with-one-argument

  (is (= λv1.λv2.(hydra.lib.lists.foldl! @ f @ v1 @ v2)

         λv1.λv2.(hydra.lib.lists.foldl! @ f @ v1 @ v2))))

;; Fully applied primitives are not expanded

(deftest test-etaexpansion-negpartial-application-of-primitives-negfully-applied-primitives-are-not-expanded-negunary-primitive

  (is (= (hydra.lib.strings.toLower! @ "FOO")

         (hydra.lib.strings.toLower! @ "FOO"))))

(deftest test-etaexpansion-negpartial-application-of-primitives-negfully-applied-primitives-are-not-expanded-negbinary-primitive

  (is (= (hydra.lib.strings.splitOn! @ "," @ "a,b,c")

         (hydra.lib.strings.splitOn! @ "," @ "a,b,c"))))

;; Record projections

;; Bare projections expand with a lambda

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negbare-projections-expand-with-a-lambda-negprojection-without-argument

  (is (= λv1.(project(Person){firstName} @ v1)

         λv1.(project(Person){firstName} @ v1))))

;; Applied projections are not expanded

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negapplied-projections-are-not-expanded-negprojection-with-argument

  (is (= (project(Person){firstName} @ person)

         (project(Person){firstName} @ person))))

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negapplied-projections-are-not-expanded-negprojection-applied-to-a-record

  (is (= (project(Person){firstName} @ record(Person){firstName="John", lastName="Doe"})

         (project(Person){firstName} @ record(Person){firstName="John", lastName="Doe"}))))

;; Projections nested in other structures

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negprojections-nested-in-other-structures-negprojection-in-a-list

  (is (= [λv1.(project(Person){firstName} @ v1), hydra.lib.strings.toLower!]

         [λv1.(project(Person){firstName} @ v1), hydra.lib.strings.toLower!])))

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negprojections-nested-in-other-structures-negprojection-in-a-tuple

  (is (= (λv1.(project(Person){firstName} @ v1), "default")

         (λv1.(project(Person){firstName} @ v1), "default"))))

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negprojections-nested-in-other-structures-negprojection-in-let-binding

  (is (= let getter = λv1.(project(Person){firstName} @ v1) in getter

         let getter = λv1.(project(Person){firstName} @ v1) in getter)))

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negprojections-nested-in-other-structures-negprojection-in-lambda-body

  (is (= λx.λv1.(project(Person){firstName} @ v1)

         λx.λv1.(project(Person){firstName} @ v1))))

;; Function-valued projections

(deftest test-etaexpansion-negpartial-application-of-primitives-negrecord-projections-negfunction-negvalued-projections-negprojection-of-function-negvalued-field-applied-to-arguments-should-not-be-expanded

  (is (= (project(Triple){first}⟨(string → string)⟩⟨string⟩⟨string⟩ @ record(Triple){first=hydra.lib.strings.toLower!, second="middle", third="last"}⟨(string → string)⟩⟨string⟩⟨string⟩ @ "DATA")

         (project(Triple){first}⟨(string → string)⟩⟨string⟩⟨string⟩ @ record(Triple){first=hydra.lib.strings.toLower!, second="middle", third="last"}⟨(string → string)⟩⟨string⟩⟨string⟩ @ "DATA"))))

;; Polymorphic terms (System F)

;; Type lambdas in let bindings

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-lambdas-in-let-bindings-negpolymorphic-identity-function

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in id

         let id:((forall a. (a → a))) = Λa.λx.x in id)))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-lambdas-in-let-bindings-negmonomorphic-partially-applied-primitive

  (is (= let partial:(((string → list<string>))) = λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1) in partial

         let partial:(((string → list<string>))) = λv1.(hydra.lib.strings.splitOn! @ "foo" @ v1) in partial)))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-lambdas-in-let-bindings-negmonomorphic-projection

  (is (= let getter:(((Person → string))) = λv1.(project(Person){firstName} @ v1) in getter

         let getter:(((Person → string))) = λv1.(project(Person){firstName} @ v1) in getter)))

;; Type applications of polymorphic bindings

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-applications-of-polymorphic-bindings-negpolymorphic-variable-with-type-application

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in id⟨string⟩

         let id:((forall a. (a → a))) = Λa.λx.x in id⟨string⟩)))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-applications-of-polymorphic-bindings-negtype-application-of-identity-applied-to-binary-function-with-no-arguments

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn!)

         let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn!))))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-applications-of-polymorphic-bindings-negtype-application-of-identity-applied-to-partially-applied-binary-function

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → list<string>)⟩ @ λv1.(hydra.lib.strings.splitOn! @ "," @ v1))

         let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → list<string>)⟩ @ λv1.(hydra.lib.strings.splitOn! @ "," @ v1)))))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-applications-of-polymorphic-bindings-negtype-application-of-identity-applied-to-fully-applied-binary-function

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in (id⟨list<string>⟩ @ (hydra.lib.strings.splitOn! @ "," @ "foo,bar"))

         let id:((forall a. (a → a))) = Λa.λx.x in (id⟨list<string>⟩ @ (hydra.lib.strings.splitOn! @ "," @ "foo,bar")))))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-applications-of-polymorphic-bindings-negtype-application-of-identity-applied-to-binary-function-then-applied-to-one-argument

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in λv1.(id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ v1)

         let id:((forall a. (a → a))) = Λa.λx.x in λv1.(id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ v1))))

(deftest test-etaexpansion-negpolymorphic-terms-system-f--negtype-applications-of-polymorphic-bindings-negtype-application-of-identity-applied-to-binary-function-then-fully-applied-to-two-arguments

  (is (= let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ "foo,bar")

         let id:((forall a. (a → a))) = Λa.λx.x in (id⟨(string → string → list<string>)⟩ @ hydra.lib.strings.splitOn! @ "," @ "foo,bar"))))

;; Higher-Order Functions

;; Functions that return functions

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-neglambda-returning-bare-binary-primitive

  (is (= λx.hydra.lib.strings.splitOn!

         λx.hydra.lib.strings.splitOn!)))

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-neglambda-returning-bare-unary-primitive

  (is (= λx.hydra.lib.strings.toLower!

         λx.hydra.lib.strings.toLower!)))

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-neglambda-returning-partially-applied-primitive

  (is (= λx.λv1.(hydra.lib.strings.splitOn! @ "," @ v1)

         λx.λv1.(hydra.lib.strings.splitOn! @ "," @ v1))))

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-neglambda-returning-fully-applied-primitive

  (is (= λx.(hydra.lib.strings.splitOn! @ "," @ x)

         λx.(hydra.lib.strings.splitOn! @ "," @ x))))

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-neglambda-returning-bare-projection

  (is (= λperson.λv1.(project(Person){firstName} @ v1)

         λperson.λv1.(project(Person){firstName} @ v1))))

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-negnested-lambdas-with-partial-application-in-body

  (is (= λx.λy.λv1.(hydra.lib.strings.splitOn! @ x @ v1)

         λx.λy.λv1.(hydra.lib.strings.splitOn! @ x @ v1))))

(deftest test-etaexpansion-neghigher-negorder-functions-negfunctions-that-return-functions-neglambda-returning-lambda-returning-partial-application

  (is (= λx.λy.λz.λv1.(hydra.lib.strings.splitOn! @ x @ v1)

         λx.λy.λz.λv1.(hydra.lib.strings.splitOn! @ x @ v1))))

;; Let terms

;; partial application of a let-bound function

(deftest test-etaexpansion-neglet-terms-negpartial-application-of-a-let-negbound-function-negsimple

  (is (= let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2)

         let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2))))

(deftest test-etaexpansion-neglet-terms-negpartial-application-of-a-let-negbound-function-negin-a-fold

  (is (= let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in (hydra.lib.lists.foldl!⟨string⟩⟨string⟩ @ λv1.λv2.(helper @ "foo" @ v1 @ v2) @ "" @ ["bar", "baz"])

         let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in (hydra.lib.lists.foldl!⟨string⟩⟨string⟩ @ λv1.λv2.(helper @ "foo" @ v1 @ v2) @ "" @ ["bar", "baz"]))))

(deftest test-etaexpansion-neglet-terms-negpartial-application-of-a-let-negbound-function-negwithin-another-let-binding

  (is (= let tryme:(((string → string → string))) = let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2) in unit

         let tryme:(((string → string → string))) = let helper:(((string → string → string → string))) = λarg1.λarg2.λarg3.(hydra.lib.strings.cat! @ [arg1, arg2, arg3]) in λv1.λv2.(helper @ "foo" @ v1 @ v2) in unit)))

;; Case statements

;; monomorphic at top level

(deftest test-etaexpansion-negcase-statements-negmonomorphic-at-top-level-negnon-negapplied-case-statement

  (is (= λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1)

         λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1))))

(deftest test-etaexpansion-negcase-statements-negmonomorphic-at-top-level-negapplied-case-statement

  (is (= (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"})

         (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"}))))

(deftest test-etaexpansion-negcase-statements-negmonomorphic-at-top-level-negapplied-case-statement-in-lambda

  (is (= λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x)

         λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x))))

;; monomorphic in let binding

(deftest test-etaexpansion-negcase-statements-negmonomorphic-in-let-binding-negnon-negapplied-case-statement

  (is (= let test:(((UnionMonomorphic → string))) = λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1) in "ignored"

         let test:(((UnionMonomorphic → string))) = λv1.(case(UnionMonomorphic){string=λs.s, [default]="other"} @ v1) in "ignored")))

(deftest test-etaexpansion-negcase-statements-negmonomorphic-in-let-binding-negapplied-case-statement

  (is (= let test:((string)) = (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"}) in "ignored"

         let test:((string)) = (case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ inject(UnionMonomorphic){string="foo"}) in "ignored")))

(deftest test-etaexpansion-negcase-statements-negmonomorphic-in-let-binding-negapplied-case-statement-in-lambda

  (is (= let test:(((string → string))) = λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x) in "ignored"

         let test:(((string → string))) = λx:UnionMonomorphic.(case(UnionMonomorphic){string=λs:string.s, [default]="other"} @ x) in "ignored")))

;; polymorphic in let binding

(deftest test-etaexpansion-negcase-statements-negpolymorphic-in-let-binding-negnon-negapplied-unionpolymorphicrecursive

  (is (= let test:((((UnionPolymorphicRecursive @ int32) → string))) = λv1.(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ v1) in test

         let test:((((UnionPolymorphicRecursive @ int32) → string))) = λv1.(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ v1) in test)))

(deftest test-etaexpansion-negcase-statements-negpolymorphic-in-let-binding-negapplied-unionpolymorphicrecursive-with-int32

  (is (= let test:((string)) = (case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ inject(UnionPolymorphicRecursive){value=42:int32}⟨int32⟩) in test

         let test:((string)) = (case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ inject(UnionPolymorphicRecursive){value=42:int32}⟨int32⟩) in test)))

(deftest test-etaexpansion-negcase-statements-negpolymorphic-in-let-binding-negapplied-unionpolymorphicrecursive-with-int32-in-lambda

  (is (= let test:((((UnionPolymorphicRecursive @ int32) → string))) = λx:(UnionPolymorphicRecursive @ int32).(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ x) in test

         let test:((((UnionPolymorphicRecursive @ int32) → string))) = λx:(UnionPolymorphicRecursive @ int32).(case(UnionPolymorphicRecursive){value=λi:int32.(hydra.lib.literals.showInt32! @ i), [default]="other"}⟨int32⟩ @ x) in test)))

(deftest test-etaexpansion-negcase-statements-negpolymorphic-in-let-binding-negapplied-generic-unionpolymorphicrecursive-in-lambda

  (is (= Λt0.let test:((forall t1. ((UnionPolymorphicRecursive @ t1) → string))) = Λt1.λx:(UnionPolymorphicRecursive @ t1).(case(UnionPolymorphicRecursive){value=λignored:t1."foo", [default]="other"}⟨t1⟩ @ x) in test⟨t0⟩

         Λt0.let test:((forall t1. ((UnionPolymorphicRecursive @ t1) → string))) = Λt1.λx:(UnionPolymorphicRecursive @ t1).(case(UnionPolymorphicRecursive){value=λignored:t1."foo", [default]="other"}⟨t1⟩ @ x) in test⟨t0⟩)))

;; Forced expansion in case statement branches

(deftest test-etaexpansion-negcase-statements-negforced-expansion-in-case-statement-branches-negvariable-reference-in-case-branch-is-expanded

  (is (= let handler:(((string → string))) = hydra.lib.strings.toLower! in λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(handler @ v1), unit=λignored."unit value"} @ v1)

         let handler:(((string → string))) = hydra.lib.strings.toLower! in λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(handler @ v1), unit=λignored."unit value"} @ v1))))

(deftest test-etaexpansion-negcase-statements-negforced-expansion-in-case-statement-branches-negbare-primitive-in-case-branch-is-expanded

  (is (= λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(hydra.lib.strings.toLower! @ v1), unit=λignored."unit value"} @ v1)

         λv1.(case(UnionMonomorphic){bool=λignored."boolean value", string=λv1.(hydra.lib.strings.toLower! @ v1), unit=λignored."unit value"} @ v1))))

(deftest test-etaexpansion-negcase-statements-negforced-expansion-in-case-statement-branches-negvariable-reference-outside-case-branch-is-not-expanded

  (is (= let handler = hydra.lib.strings.toLower! in handler

         let handler = hydra.lib.strings.toLower! in handler)))

(deftest test-etaexpansion-negcase-statements-negforced-expansion-in-case-statement-branches-negbare-primitive-outside-case-branch-is-not-expanded

  (is (= hydra.lib.strings.toLower!

         hydra.lib.strings.toLower!)))

;; Non-expansion of eliminations which produce functions

(deftest test-etaexpansion-negnon-negexpansion-of-eliminations-which-produce-functions-negapplied-case-statement

  (is (= Λt0.λdir:hydra.coders.CoderDirection.λcoder:(hydra.coders.Coder @ t0 @ t0).λcx:hydra.context.Context.λv1:t0.(case(hydra.coders.CoderDirection){encode=λ_:unit.(project(hydra.coders.Coder){encode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1), decode=λ_:unit.(project(hydra.coders.Coder){decode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1)} @ dir)

         Λt0.λdir:hydra.coders.CoderDirection.λcoder:(hydra.coders.Coder @ t0 @ t0).λcx:hydra.context.Context.λv1:t0.(case(hydra.coders.CoderDirection){encode=λ_:unit.(project(hydra.coders.Coder){encode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1), decode=λ_:unit.(project(hydra.coders.Coder){decode}⟨t0⟩⟨t0⟩ @ coder @ cx @ v1)} @ dir))))

(deftest test-etaexpansion-negnon-negexpansion-of-eliminations-which-produce-functions-negapplied-projection

  (is (= (project(Triple){third}⟨int32⟩⟨int32⟩⟨(string → string)⟩ @ record(Triple){first=42:int32, second=137:int32, third=λs.(hydra.lib.strings.toLower! @ s)})

         (project(Triple){third}⟨int32⟩⟨int32⟩⟨(string → string)⟩ @ record(Triple){first=42:int32, second=137:int32, third=λs.(hydra.lib.strings.toLower! @ s)}))))

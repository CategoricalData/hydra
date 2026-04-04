;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; variables

(require 'ert)

;; freeVariables

(ert-deftest test-variables-negfreevariables-negstring-literal-has-no-free-variables ()

  (should (equal {} {})))

(ert-deftest test-variables-negfreevariables-negsingle-variable ()

  (should (equal {x} {x})))

(ert-deftest test-variables-negfreevariables-negbound-variable-is-not-free ()

  (should (equal {} {})))

(ert-deftest test-variables-negfreevariables-negunbound-variable-in-lambda-body ()

  (should (equal {x} {x})))

(ert-deftest test-variables-negfreevariables-negmixed-free-and-bound-variables ()

  (should (equal {x} {x})))

(ert-deftest test-variables-negfreevariables-negmultiple-free-variables ()

  (should (equal {x, y} {x, y})))

;; normalizeTypeVariables

(ert-deftest test-variables-negnormalizetypevariables-negliteral-without-type-variables-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-negsimple-let-without-type-annotations-unchanged ()

  (should (equal let foo = "foo" in 42:int32 let foo = "foo" in 42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-neglet-with-monomorphic-type-scheme-unchanged ()

  (should (equal let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-neglet-with-monomorphic-binding-referencing-string ()

  (should (equal let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-negpolymorphic-binding-with-free-type-variable-unchanged ()

  (should (equal let foo:((a)) = bar in 42:int32 let foo:((a)) = bar in 42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-negmonomorphic-binding-with-typed-lambda-unchanged ()

  (should (equal let foo:((string)) = "foo" in λx:(a → int32).42:int32 let foo:((string)) = "foo" in λx:(a → int32).42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-negpolymorphic-binding-with-typed-lambda-in-body-unchanged ()

  (should (equal let foo:((a)) = bar in λx:(a → int32).42:int32 let foo:((a)) = bar in λx:(a → int32).42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-negpolymorphic-identity-function-normalized ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(ert-deftest test-variables-negnormalizetypevariables-negpolymorphic-const-function-normalized ()

  (should (equal let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo") let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo"))))

(ert-deftest test-variables-negnormalizetypevariables-negbinding-rewriting-does-not-affect-body-with-typed-lambda ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32 let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32)))

(ert-deftest test-variables-negnormalizetypevariables-negnested-polymorphic-lets-normalized ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)))))

(ert-deftest test-variables-negnormalizetypevariables-negnested-same-substitution-in-bindings-and-environment ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(ert-deftest test-variables-negnormalizetypevariables-negparent-type-variable-shadows-child-variable ()

  (should (equal let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(ert-deftest test-variables-negnormalizetypevariables-negno-shadowing-distinct-type-variables ()

  (should (equal let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(ert-deftest test-variables-negnormalizetypevariables-neglocally-free-type-variable-in-nested-binding ()

  (should (equal let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32) let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32))))

;; unshadowVariables

(ert-deftest test-variables-negunshadowvariables-negliteral-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-variables-negunshadowvariables-negvariable-unchanged ()

  (should (equal x x)))

(ert-deftest test-variables-negunshadowvariables-negsingle-lambda-unchanged ()

  (should (equal λx.x λx.x)))

(ert-deftest test-variables-negunshadowvariables-negdistinct-lambda-parameters-unchanged ()

  (should (equal λx.λy.[x, y] λx.λy.[x, y])))

(ert-deftest test-variables-negunshadowvariables-neglet-with-no-shadowing-unchanged ()

  (should (equal let x = 1:int32 in x let x = 1:int32 in x)))

(ert-deftest test-variables-negunshadowvariables-neglet-and-lambda-with-distinct-names-unchanged ()

  (should (equal let x = 1:int32 in λy.[x, y] let x = 1:int32 in λy.[x, y])))

(ert-deftest test-variables-negunshadowvariables-neginner-lambda-shadows-outer-lambda ()

  (should (equal λx.λx2.x2 λx.λx2.x2)))

(ert-deftest test-variables-negunshadowvariables-neginner-lambda-shadows-outer--neg-body-references-both ()

  (should (equal λx.[x, λx2.x2] λx.[x, λx2.x2])))

(ert-deftest test-variables-negunshadowvariables-negtriple-nested-lambda-same-name ()

  (should (equal λx.λx2.λx3.x3 λx.λx2.λx3.x3)))

(ert-deftest test-variables-negunshadowvariables-negtwo-parameters-shadow-sequentially ()

  (should (equal λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(ert-deftest test-variables-negunshadowvariables-neglambda-shadows-let-negbound-variable ()

  (should (equal let x = 1:int32 in λx2.x2 let x = 1:int32 in λx2.x2)))

(ert-deftest test-variables-negunshadowvariables-neglambda-shadows-one-of-multiple-let-bindings ()

  (should (equal let x = 1:int32, y = 2:int32 in λx2.[x2, y] let x = 1:int32, y = 2:int32 in λx2.[x2, y])))

(ert-deftest test-variables-negunshadowvariables-neginner-let-body-with-lambda-shadowing-outer-let ()

  (should (equal let x = 1:int32 in let y = 2:int32 in λx2.x2 let x = 1:int32 in let y = 2:int32 in λx2.x2)))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-in-function-position-of-application ()

  (should (equal λf.(λf2.f2 @ f) λf.(λf2.f2 @ f))))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambdas-in-list-elements ()

  (should (equal λx.[λx2.x2, λx2.x2] λx.[λx2.x2, λx2.x2])))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-in-record-field ()

  (should (equal λx.record(Pair){fst=λx2.x2, snd=x} λx.record(Pair){fst=λx2.x2, snd=x})))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-in-case-branch ()

  (should (equal λx.case(Maybe){nothing=0:int32, just=λx2.x2} λx.case(Maybe){nothing=0:int32, just=λx2.x2})))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-in-pair ()

  (should (equal λx.(λx2.x2, x) λx.(λx2.x2, x))))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-optional ()

  (should (equal λx.just(λx2.x2) λx.just(λx2.x2))))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-set-element ()

  (should (equal λx.{λx2.x2} λx.{λx2.x2})))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-in-union-injection ()

  (should (equal λx.inject(Result){ok=λx2.x2} λx.inject(Result){ok=λx2.x2})))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-wrapped-term ()

  (should (equal λx.wrap(Age){λx2.x2} λx.wrap(Age){λx2.x2})))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-type-lambda ()

  (should (equal λx.Λa.λx2.x2 λx.Λa.λx2.x2)))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-type-application ()

  (should (equal λx.λx2.x2⟨string⟩ λx.λx2.x2⟨string⟩)))

(ert-deftest test-variables-negunshadowvariables-negshadowed-lambda-inside-annotated-term ()

  (should (equal λx.λx2.x2 λx.λx2.x2)))

(ert-deftest test-variables-negunshadowvariables-negshadowing-at-multiple-depths ()

  (should (equal λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(ert-deftest test-variables-negunshadowvariables-neglet-then-lambda-then-lambda-all-same-name ()

  (should (equal let x = 1:int32 in λx2.λx3.x3 let x = 1:int32 in λx2.λx3.x3)))

(ert-deftest test-variables-negunshadowvariables-neglambda-with-shadowing-in-let-binding-value ()

  (should (equal λx.let y = λx2.x2 in (y @ x) λx.let y = λx2.x2 in (y @ x))))

(ert-deftest test-variables-negunshadowvariables-negapplication-without-shadowing-unchanged ()

  (should (equal (f @ 42:int32) (f @ 42:int32))))

(ert-deftest test-variables-negunshadowvariables-neglist-of-literals-unchanged ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-variables-negunshadowvariables-negnested-record-unchanged ()

  (should (equal record(Point){x=10:int32, y=20:int32} record(Point){x=10:int32, y=20:int32})))

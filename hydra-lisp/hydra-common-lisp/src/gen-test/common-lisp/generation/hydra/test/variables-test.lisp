;; Note: this is an automatically generated file. Do not edit.
;; variables

;; freeVariables

(defun test-variables-negfreevariables-negstring-literal-has-no-free-variables ()

  (assert (equal {} {})))

(defun test-variables-negfreevariables-negsingle-variable ()

  (assert (equal {x} {x})))

(defun test-variables-negfreevariables-negbound-variable-is-not-free ()

  (assert (equal {} {})))

(defun test-variables-negfreevariables-negunbound-variable-in-lambda-body ()

  (assert (equal {x} {x})))

(defun test-variables-negfreevariables-negmixed-free-and-bound-variables ()

  (assert (equal {x} {x})))

(defun test-variables-negfreevariables-negmultiple-free-variables ()

  (assert (equal {x, y} {x, y})))

;; normalizeTypeVariables

(defun test-variables-negnormalizetypevariables-negliteral-without-type-variables-unchanged ()

  (assert (equal 42:int32 42:int32)))

(defun test-variables-negnormalizetypevariables-negsimple-let-without-type-annotations-unchanged ()

  (assert (equal let foo = "foo" in 42:int32 let foo = "foo" in 42:int32)))

(defun test-variables-negnormalizetypevariables-neglet-with-monomorphic-type-scheme-unchanged ()

  (assert (equal let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(defun test-variables-negnormalizetypevariables-neglet-with-monomorphic-binding-referencing-string ()

  (assert (equal let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(defun test-variables-negnormalizetypevariables-negpolymorphic-binding-with-free-type-variable-unchanged ()

  (assert (equal let foo:((a)) = bar in 42:int32 let foo:((a)) = bar in 42:int32)))

(defun test-variables-negnormalizetypevariables-negmonomorphic-binding-with-typed-lambda-unchanged ()

  (assert (equal let foo:((string)) = "foo" in λx:(a → int32).42:int32 let foo:((string)) = "foo" in λx:(a → int32).42:int32)))

(defun test-variables-negnormalizetypevariables-negpolymorphic-binding-with-typed-lambda-in-body-unchanged ()

  (assert (equal let foo:((a)) = bar in λx:(a → int32).42:int32 let foo:((a)) = bar in λx:(a → int32).42:int32)))

(defun test-variables-negnormalizetypevariables-negpolymorphic-identity-function-normalized ()

  (assert (equal let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(defun test-variables-negnormalizetypevariables-negpolymorphic-const-function-normalized ()

  (assert (equal let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo") let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo"))))

(defun test-variables-negnormalizetypevariables-negbinding-rewriting-does-not-affect-body-with-typed-lambda ()

  (assert (equal let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32 let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32)))

(defun test-variables-negnormalizetypevariables-negnested-polymorphic-lets-normalized ()

  (assert (equal let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)))))

(defun test-variables-negnormalizetypevariables-negnested-same-substitution-in-bindings-and-environment ()

  (assert (equal let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(defun test-variables-negnormalizetypevariables-negparent-type-variable-shadows-child-variable ()

  (assert (equal let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(defun test-variables-negnormalizetypevariables-negno-shadowing-distinct-type-variables ()

  (assert (equal let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(defun test-variables-negnormalizetypevariables-neglocally-free-type-variable-in-nested-binding ()

  (assert (equal let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32) let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32))))

;; unshadowVariables

(defun test-variables-negunshadowvariables-negliteral-unchanged ()

  (assert (equal 42:int32 42:int32)))

(defun test-variables-negunshadowvariables-negvariable-unchanged ()

  (assert (equal x x)))

(defun test-variables-negunshadowvariables-negsingle-lambda-unchanged ()

  (assert (equal λx.x λx.x)))

(defun test-variables-negunshadowvariables-negdistinct-lambda-parameters-unchanged ()

  (assert (equal λx.λy.[x, y] λx.λy.[x, y])))

(defun test-variables-negunshadowvariables-neglet-with-no-shadowing-unchanged ()

  (assert (equal let x = 1:int32 in x let x = 1:int32 in x)))

(defun test-variables-negunshadowvariables-neglet-and-lambda-with-distinct-names-unchanged ()

  (assert (equal let x = 1:int32 in λy.[x, y] let x = 1:int32 in λy.[x, y])))

(defun test-variables-negunshadowvariables-neginner-lambda-shadows-outer-lambda ()

  (assert (equal λx.λx2.x2 λx.λx2.x2)))

(defun test-variables-negunshadowvariables-neginner-lambda-shadows-outer--neg-body-references-both ()

  (assert (equal λx.[x, λx2.x2] λx.[x, λx2.x2])))

(defun test-variables-negunshadowvariables-negtriple-nested-lambda-same-name ()

  (assert (equal λx.λx2.λx3.x3 λx.λx2.λx3.x3)))

(defun test-variables-negunshadowvariables-negtwo-parameters-shadow-sequentially ()

  (assert (equal λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(defun test-variables-negunshadowvariables-neglambda-shadows-let-negbound-variable ()

  (assert (equal let x = 1:int32 in λx2.x2 let x = 1:int32 in λx2.x2)))

(defun test-variables-negunshadowvariables-neglambda-shadows-one-of-multiple-let-bindings ()

  (assert (equal let x = 1:int32, y = 2:int32 in λx2.[x2, y] let x = 1:int32, y = 2:int32 in λx2.[x2, y])))

(defun test-variables-negunshadowvariables-neginner-let-body-with-lambda-shadowing-outer-let ()

  (assert (equal let x = 1:int32 in let y = 2:int32 in λx2.x2 let x = 1:int32 in let y = 2:int32 in λx2.x2)))

(defun test-variables-negunshadowvariables-negshadowed-lambda-in-function-position-of-application ()

  (assert (equal λf.(λf2.f2 @ f) λf.(λf2.f2 @ f))))

(defun test-variables-negunshadowvariables-negshadowed-lambdas-in-list-elements ()

  (assert (equal λx.[λx2.x2, λx2.x2] λx.[λx2.x2, λx2.x2])))

(defun test-variables-negunshadowvariables-negshadowed-lambda-in-record-field ()

  (assert (equal λx.record(Pair){fst=λx2.x2, snd=x} λx.record(Pair){fst=λx2.x2, snd=x})))

(defun test-variables-negunshadowvariables-negshadowed-lambda-in-case-branch ()

  (assert (equal λx.case(Maybe){nothing=0:int32, just=λx2.x2} λx.case(Maybe){nothing=0:int32, just=λx2.x2})))

(defun test-variables-negunshadowvariables-negshadowed-lambda-in-pair ()

  (assert (equal λx.(λx2.x2, x) λx.(λx2.x2, x))))

(defun test-variables-negunshadowvariables-negshadowed-lambda-inside-optional ()

  (assert (equal λx.just(λx2.x2) λx.just(λx2.x2))))

(defun test-variables-negunshadowvariables-negshadowed-lambda-inside-set-element ()

  (assert (equal λx.{λx2.x2} λx.{λx2.x2})))

(defun test-variables-negunshadowvariables-negshadowed-lambda-in-union-injection ()

  (assert (equal λx.inject(Result){ok=λx2.x2} λx.inject(Result){ok=λx2.x2})))

(defun test-variables-negunshadowvariables-negshadowed-lambda-inside-wrapped-term ()

  (assert (equal λx.wrap(Age){λx2.x2} λx.wrap(Age){λx2.x2})))

(defun test-variables-negunshadowvariables-negshadowed-lambda-inside-type-lambda ()

  (assert (equal λx.Λa.λx2.x2 λx.Λa.λx2.x2)))

(defun test-variables-negunshadowvariables-negshadowed-lambda-inside-type-application ()

  (assert (equal λx.λx2.x2⟨string⟩ λx.λx2.x2⟨string⟩)))

(defun test-variables-negunshadowvariables-negshadowed-lambda-inside-annotated-term ()

  (assert (equal λx.λx2.x2 λx.λx2.x2)))

(defun test-variables-negunshadowvariables-negshadowing-at-multiple-depths ()

  (assert (equal λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(defun test-variables-negunshadowvariables-neglet-then-lambda-then-lambda-all-same-name ()

  (assert (equal let x = 1:int32 in λx2.λx3.x3 let x = 1:int32 in λx2.λx3.x3)))

(defun test-variables-negunshadowvariables-neglambda-with-shadowing-in-let-binding-value ()

  (assert (equal λx.let y = λx2.x2 in (y @ x) λx.let y = λx2.x2 in (y @ x))))

(defun test-variables-negunshadowvariables-negapplication-without-shadowing-unchanged ()

  (assert (equal (f @ 42:int32) (f @ 42:int32))))

(defun test-variables-negunshadowvariables-neglist-of-literals-unchanged ()

  (assert (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(defun test-variables-negunshadowvariables-negnested-record-unchanged ()

  (assert (equal record(Point){x=10:int32, y=20:int32} record(Point){x=10:int32, y=20:int32})))

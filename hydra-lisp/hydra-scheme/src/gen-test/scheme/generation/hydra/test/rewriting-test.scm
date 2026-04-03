;; Note: this is an automatically generated file. Do not edit.
;; rewriting

(import (scheme base))

;; freeVariables

(define (test-rewriting-negfreevariables-negstring-literal-has-no-free-variables)

  (assert (equal? {} {})))

(define (test-rewriting-negfreevariables-negsingle-variable)

  (assert (equal? {x} {x})))

(define (test-rewriting-negfreevariables-negbound-variable-is-not-free)

  (assert (equal? {} {})))

(define (test-rewriting-negfreevariables-negunbound-variable-in-lambda-body)

  (assert (equal? {x} {x})))

(define (test-rewriting-negfreevariables-negmixed-free-and-bound-variables)

  (assert (equal? {x} {x})))

(define (test-rewriting-negfreevariables-negmultiple-free-variables)

  (assert (equal? {x, y} {x, y})))

;; simplifyTerm

(define (test-rewriting-negsimplifyterm-negconst-application-with-literal)

  (assert (equal? "foo" "foo")))

(define (test-rewriting-negsimplifyterm-negidentity-application)

  (assert (equal? [y, y] [y, y])))

(define (test-rewriting-negsimplifyterm-negunused-parameter)

  (assert (equal? "foo" "foo")))

(define (test-rewriting-negsimplifyterm-negnested-lambda-applications)

  (assert (equal? ["foo", y] ["foo", y])))

;; flattenLetTerms

(define (test-rewriting-negflattenletterms-negnon-neglet-term-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negflattenletterms-neglist-term-unchanged)

  (assert (equal? ["foo"] ["foo"])))

(define (test-rewriting-negflattenletterms-negsequential-lets-in-body-are-flattened)

  (assert (equal? let x = 1:int32, y = 2:int32 in [x, y] let x = 1:int32, y = 2:int32 in [x, y])))

(define (test-rewriting-negflattenletterms-negnested-binding-in-let-value-is-flattened)

  (assert (equal? let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b])))

(define (test-rewriting-negflattenletterms-negmultiple-levels-of-nesting-are-flattened)

  (assert (equal? let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b])))

;; liftLambdaAboveLet

(define (test-rewriting-negliftlambdaabovelet-negsimple-let-with-lambda-in-body)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-negbare-lambda-unchanged)

  (assert (equal? λx.x λx.x)))

(define (test-rewriting-negliftlambdaabovelet-negbare-let-unchanged)

  (assert (equal? let x = 42:int32 in x let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-neglambda-with-let-in-body-unchanged)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-neglet-with-two-nested-lambdas)

  (assert (equal? λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-neglambda-inside-let-body-already-above-let)

  (assert (equal? λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(define (test-rewriting-negliftlambdaabovelet-neglet-without-lambda-in-body-unchanged)

  (assert (equal? let x = 42:int32, y = "hello" in (x, y) let x = 42:int32, y = "hello" in (x, y))))

(define (test-rewriting-negliftlambdaabovelet-negmultiple-let-bindings-with-lambda)

  (assert (equal? λz.let x = 42:int32, y = "hello" in x λz.let x = 42:int32, y = "hello" in x)))

(define (test-rewriting-negliftlambdaabovelet-negnested-lets-with-lambda-at-innermost-level)

  (assert (equal? λz.let x = 42:int32 in let y = "hello" in x λz.let x = 42:int32 in let y = "hello" in x)))

(define (test-rewriting-negliftlambdaabovelet-neglambda-between-two-lets)

  (assert (equal? λy.let x = 42:int32 in let z = "hello" in x λy.let x = 42:int32 in let z = "hello" in x)))

(define (test-rewriting-negliftlambdaabovelet-negmultiple-lambdas-between-nested-lets)

  (assert (equal? λx.λy.let a = 1:int32 in let b = 2:int32 in a λx.λy.let a = 1:int32 in let b = 2:int32 in a)))

(define (test-rewriting-negliftlambdaabovelet-negmultiple-lambdas-already-above-let)

  (assert (equal? λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(define (test-rewriting-negliftlambdaabovelet-negannotation-above-let-containing-lambda)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-negannotation-above-lambda-in-let-body)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-negannotation-between-two-lambdas)

  (assert (equal? λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-negannotation-on-the-body-of-lambda-in-let)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-negannotation-on-lambda-already-above-let)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-a-list)

  (assert (equal? [1:int32, λy.let x = 42:int32 in x, 2:int32] [1:int32, λy.let x = 42:int32 in x, 2:int32])))

(define (test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-multiple-list-elements)

  (assert (equal? [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z] [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z])))

(define (test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-a-let-binding-value)

  (assert (equal? let f = λy.let x = 42:int32 in x in f let f = λy.let x = 42:int32 in x in f)))

(define (test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-a-pair)

  (assert (equal? (λy.let x = 42:int32 in x, "test") (λy.let x = 42:int32 in x, "test"))))

(define (test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-both-elements-of-a-pair)

  (assert (equal? (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z) (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z))))

(define (test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-lambda-body)

  (assert (equal? λouter.λinner.let x = 42:int32 in x λouter.λinner.let x = 42:int32 in x)))

;; deannotateTerm

(define (test-rewriting-negdeannotateterm-negunannotated-literal-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negdeannotateterm-negunannotated-variable-unchanged)

  (assert (equal? x x)))

(define (test-rewriting-negdeannotateterm-negunannotated-lambda-unchanged)

  (assert (equal? λx.x λx.x)))

(define (test-rewriting-negdeannotateterm-negsingle-annotation-stripped)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negdeannotateterm-negnested-annotations-stripped)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negdeannotateterm-negannotated-lambda-stripped)

  (assert (equal? λx.x λx.x)))

(define (test-rewriting-negdeannotateterm-negannotated-application-stripped)

  (assert (equal? (f @ x) (f @ x))))

;; deannotateType

(define (test-rewriting-negdeannotatetype-negunannotated-primitive-type-unchanged)

  (assert (equal? int32 int32)))

(define (test-rewriting-negdeannotatetype-negunannotated-string-type-unchanged)

  (assert (equal? string string)))

(define (test-rewriting-negdeannotatetype-negunannotated-function-type-unchanged)

  (assert (equal? (int32 → string) (int32 → string))))

(define (test-rewriting-negdeannotatetype-negsingle-annotation-stripped)

  (assert (equal? int32 int32)))

(define (test-rewriting-negdeannotatetype-negnested-annotations-stripped)

  (assert (equal? string string)))

(define (test-rewriting-negdeannotatetype-negannotated-list-type-stripped)

  (assert (equal? list<int32> list<int32>)))

(define (test-rewriting-negdeannotatetype-negannotated-function-type-stripped)

  (assert (equal? (int32 → string) (int32 → string))))

;; topologicalSortBindings

(define (test-rewriting-negtopologicalsortbindings-negisolated-bindings)

  (assert (equal? [[(a, "foo")], [(b, "bar")]] [[(a, "foo")], [(b, "bar")]])))

(define (test-rewriting-negtopologicalsortbindings-negsingle-recursive-binding)

  (assert (equal? [[(a, [a])]] [[(a, [a])]])))

(define (test-rewriting-negtopologicalsortbindings-negmutually-recursive-bindings)

  (assert (equal? [[(a, [b]), (b, [a])]] [[(a, [b]), (b, [a])]])))

(define (test-rewriting-negtopologicalsortbindings-negmixed-bindings)

  (assert (equal? [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]] [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]])))

;; normalizeTypeVariables

(define (test-rewriting-negnormalizetypevariables-negliteral-without-type-variables-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negnormalizetypevariables-negsimple-let-without-type-annotations-unchanged)

  (assert (equal? let foo = "foo" in 42:int32 let foo = "foo" in 42:int32)))

(define (test-rewriting-negnormalizetypevariables-neglet-with-monomorphic-type-scheme-unchanged)

  (assert (equal? let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(define (test-rewriting-negnormalizetypevariables-neglet-with-monomorphic-binding-referencing-string)

  (assert (equal? let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(define (test-rewriting-negnormalizetypevariables-negpolymorphic-binding-with-free-type-variable-unchanged)

  (assert (equal? let foo:((a)) = bar in 42:int32 let foo:((a)) = bar in 42:int32)))

(define (test-rewriting-negnormalizetypevariables-negmonomorphic-binding-with-typed-lambda-unchanged)

  (assert (equal? let foo:((string)) = "foo" in λx:(a → int32).42:int32 let foo:((string)) = "foo" in λx:(a → int32).42:int32)))

(define (test-rewriting-negnormalizetypevariables-negpolymorphic-binding-with-typed-lambda-in-body-unchanged)

  (assert (equal? let foo:((a)) = bar in λx:(a → int32).42:int32 let foo:((a)) = bar in λx:(a → int32).42:int32)))

(define (test-rewriting-negnormalizetypevariables-negpolymorphic-identity-function-normalized)

  (assert (equal? let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(define (test-rewriting-negnormalizetypevariables-negpolymorphic-const-function-normalized)

  (assert (equal? let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo") let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo"))))

(define (test-rewriting-negnormalizetypevariables-negbinding-rewriting-does-not-affect-body-with-typed-lambda)

  (assert (equal? let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32 let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32)))

(define (test-rewriting-negnormalizetypevariables-negnested-polymorphic-lets-normalized)

  (assert (equal? let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)))))

(define (test-rewriting-negnormalizetypevariables-negnested-same-substitution-in-bindings-and-environment)

  (assert (equal? let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(define (test-rewriting-negnormalizetypevariables-negparent-type-variable-shadows-child-variable)

  (assert (equal? let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(define (test-rewriting-negnormalizetypevariables-negno-shadowing-distinct-type-variables)

  (assert (equal? let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(define (test-rewriting-negnormalizetypevariables-neglocally-free-type-variable-in-nested-binding)

  (assert (equal? let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32) let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32))))

;; etaExpandTerm

(define (test-rewriting-negetaexpandterm-neginteger-literal-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negetaexpandterm-negstring-list-unchanged)

  (assert (equal? ["foo", "bar"] ["foo", "bar"])))

(define (test-rewriting-negetaexpandterm-negfully-applied-binary-function-unchanged)

  (assert (equal? (hydra.lib.strings.splitOn! @ "foo" @ "bar") (hydra.lib.strings.splitOn! @ "foo" @ "bar"))))

(define (test-rewriting-negetaexpandterm-neglambda-with-fully-applied-primitive-unchanged)

  (assert (equal? λx.(hydra.lib.strings.splitOn! @ "," @ x) λx.(hydra.lib.strings.splitOn! @ "," @ x))))

(define (test-rewriting-negetaexpandterm-neglambda-returning-constant-unchanged)

  (assert (equal? λx.42:int32 λx.42:int32)))

(define (test-rewriting-negetaexpandterm-negbare-unary-primitive-unchanged)

  (assert (equal? hydra.lib.strings.toLower! hydra.lib.strings.toLower!)))

(define (test-rewriting-negetaexpandterm-negbare-binary-primitive-unchanged)

  (assert (equal? hydra.lib.strings.splitOn! hydra.lib.strings.splitOn!)))

(define (test-rewriting-negetaexpandterm-negpartially-applied-binary-primitive-expands-to-one-lambda)

  (assert (equal? λv1.(hydra.lib.strings.splitOn! @ foo @ v1) λv1.(hydra.lib.strings.splitOn! @ foo @ v1))))

(define (test-rewriting-negetaexpandterm-negprojection-expands-to-lambda)

  (assert (equal? λv1.(project(Person){firstName} @ v1) λv1.(project(Person){firstName} @ v1))))

(define (test-rewriting-negetaexpandterm-negpartial-application-inside-lambda-expands)

  (assert (equal? λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1) λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1))))

(define (test-rewriting-negetaexpandterm-neglet-with-constant-body-unchanged)

  (assert (equal? let foo = 137:int32 in 42:int32 let foo = 137:int32 in 42:int32)))

(define (test-rewriting-negetaexpandterm-neglet-with-bare-primitive-value-unchanged)

  (assert (equal? let foo = hydra.lib.strings.splitOn! in foo let foo = hydra.lib.strings.splitOn! in foo)))

(define (test-rewriting-negetaexpandterm-negfully-applied-unary-unchanged)

  (assert (equal? (hydra.lib.strings.toLower! @ "FOO") (hydra.lib.strings.toLower! @ "FOO"))))

(define (test-rewriting-negetaexpandterm-negpartial-application-in-list-expands)

  (assert (equal? [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)] [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)])))

;; foldOverTerm

(define (test-rewriting-negfoldoverterm-negcollect-labels-from-single-node--neg-pre-negorder)

  (assert (equal? ["a"] ["a"])))

(define (test-rewriting-negfoldoverterm-negcollect-labels-from-tree--neg-pre-negorder)

  (assert (equal? ["a", "b", "c", "d"] ["a", "b", "c", "d"])))

(define (test-rewriting-negfoldoverterm-negcollect-labels-from-single-node--neg-post-negorder)

  (assert (equal? ["a"] ["a"])))

(define (test-rewriting-negfoldoverterm-negcollect-labels-from-tree--neg-post-negorder)

  (assert (equal? ["b", "d", "c", "a"] ["b", "d", "c", "a"])))

(define (test-rewriting-negfoldoverterm-negsum-int32-literals)

  (assert (equal? 52:int32 52:int32)))

(define (test-rewriting-negfoldoverterm-negcollect-list-lengths--neg-pre-negorder)

  (assert (equal? [2:int32, 2:int32, 1:int32] [2:int32, 2:int32, 1:int32])))

(define (test-rewriting-negfoldoverterm-negcollect-list-lengths--neg-post-negorder)

  (assert (equal? [2:int32, 1:int32, 2:int32] [2:int32, 1:int32, 2:int32])))

;; rewriteType

(define (test-rewriting-negrewritetype-negstring-type-in-left-side-of-either-is-replaced)

  (assert (equal? either<int32, int32> either<int32, int32>)))

(define (test-rewriting-negrewritetype-negstring-type-in-right-side-of-either-is-replaced)

  (assert (equal? either<int32, int32> either<int32, int32>)))

(define (test-rewriting-negrewritetype-negstring-types-in-both-sides-of-either-are-replaced)

  (assert (equal? either<int32, int32> either<int32, int32>)))

(define (test-rewriting-negrewritetype-negstring-type-in-nested-either-left-of-left-is-replaced)

  (assert (equal? either<either<int32, int32>, int64> either<either<int32, int32>, int64>)))

(define (test-rewriting-negrewritetype-negstring-type-in-nested-either-right-of-right-is-replaced)

  (assert (equal? either<int64, either<int32, int32>> either<int64, either<int32, int32>>)))

(define (test-rewriting-negrewritetype-negstring-types-in-complex-nested-either-are-all-replaced)

  (assert (equal? either<either<int32, int32>, either<int32, int64>> either<either<int32, int32>, either<int32, int64>>)))

(define (test-rewriting-negrewritetype-negstring-in-list-type-is-replaced)

  (assert (equal? list<int32> list<int32>)))

(define (test-rewriting-negrewritetype-negstring-in-function-domain-is-replaced)

  (assert (equal? (int32 → int64) (int32 → int64))))

(define (test-rewriting-negrewritetype-negstring-in-function-codomain-is-replaced)

  (assert (equal? (int64 → int32) (int64 → int32))))

(define (test-rewriting-negrewritetype-negstring-in-optional-type-is-replaced)

  (assert (equal? maybe<int32> maybe<int32>)))

;; rewriteTerm

(define (test-rewriting-negrewriteterm-negstring-literal-foo-replaced-with-bar)

  (assert (equal? "bar" "bar")))

(define (test-rewriting-negrewriteterm-negstring-in-variable-not-changed)

  (assert (equal? x x)))

(define (test-rewriting-negrewriteterm-negstring-in-list)

  (assert (equal? ["bar", "baz"] ["bar", "baz"])))

(define (test-rewriting-negrewriteterm-negmultiple-strings-in-list)

  (assert (equal? ["bar", "bar", "baz"] ["bar", "bar", "baz"])))

(define (test-rewriting-negrewriteterm-negstring-in-optional-just)

  (assert (equal? just("bar") just("bar"))))

(define (test-rewriting-negrewriteterm-negstring-in-function-application)

  (assert (equal? (print @ "bar") (print @ "bar"))))

(define (test-rewriting-negrewriteterm-negstring-in-lambda-body)

  (assert (equal? λx."bar" λx."bar")))

(define (test-rewriting-negrewriteterm-negstring-in-nested-applications)

  (assert (equal? (f @ (g @ "bar")) (f @ (g @ "bar")))))

(define (test-rewriting-negrewriteterm-negstring-in-record-field)

  (assert (equal? record(Person){name="bar"} record(Person){name="bar"})))

(define (test-rewriting-negrewriteterm-negstrings-in-multiple-record-fields)

  (assert (equal? record(Data){a="bar", b="baz", c="bar"} record(Data){a="bar", b="baz", c="bar"})))

(define (test-rewriting-negrewriteterm-negstring-in-pair)

  (assert (equal? ("bar", 42:int32) ("bar", 42:int32))))

(define (test-rewriting-negrewriteterm-negstring-in-let-binding-value)

  (assert (equal? let x = "bar" in x let x = "bar" in x)))

(define (test-rewriting-negrewriteterm-negstring-in-let-body)

  (assert (equal? let x = 1:int32 in "bar" let x = 1:int32 in "bar")))

(define (test-rewriting-negrewriteterm-negstring-in-first-case-branch)

  (assert (equal? case(Result){success="bar", error="baz"} case(Result){success="bar", error="baz"})))

(define (test-rewriting-negrewriteterm-negstring-in-second-case-branch)

  (assert (equal? case(Result){success="baz", error="bar"} case(Result){success="baz", error="bar"})))

(define (test-rewriting-negrewriteterm-negstring-in-default-branch)

  (assert (equal? case(Result){success="baz", error="baz", [default]="bar"} case(Result){success="baz", error="baz", [default]="bar"})))

(define (test-rewriting-negrewriteterm-negstring-deeply-nested-in-record-in-list-in-application)

  (assert (equal? (process @ [record(Item){value="bar"}]) (process @ [record(Item){value="bar"}]))))

(define (test-rewriting-negrewriteterm-negstring-in-union-inject-value)

  (assert (equal? inject(Result){success="bar"} inject(Result){success="bar"})))

(define (test-rewriting-negrewriteterm-negstring-in-wrapped-term)

  (assert (equal? wrap(Email){"bar"} wrap(Email){"bar"})))

(define (test-rewriting-negrewriteterm-negstring-in-annotated-term-body)

  (assert (equal? "bar" "bar")))

(define (test-rewriting-negrewriteterm-negstring-in-first-of-multiple-let-bindings)

  (assert (equal? let x = "bar", y = "baz" in x let x = "bar", y = "baz" in x)))

(define (test-rewriting-negrewriteterm-negstring-in-second-of-multiple-let-bindings)

  (assert (equal? let x = "baz", y = "bar" in y let x = "baz", y = "bar" in y)))

(define (test-rewriting-negrewriteterm-negstring-in-all-let-bindings-and-body)

  (assert (equal? let x = "bar", y = "bar" in "bar" let x = "bar", y = "bar" in "bar")))

(define (test-rewriting-negrewriteterm-negstring-in-set)

  (assert (equal? {"bar", "baz"} {"bar", "baz"})))

(define (test-rewriting-negrewriteterm-negstring-in-type-lambda-body)

  (assert (equal? Λa."bar" Λa."bar")))

(define (test-rewriting-negrewriteterm-negstring-in-type-application-body)

  (assert (equal? "bar"⟨string⟩ "bar"⟨string⟩)))

(define (test-rewriting-negrewriteterm-negstring-in-nested-type-lambdas)

  (assert (equal? Λa.Λb."bar" Λa.Λb."bar")))

(define (test-rewriting-negrewriteterm-negstring-in-case-branch-within-let-binding)

  (assert (equal? let handler = case(Result){ok="bar", err="baz"} in handler let handler = case(Result){ok="bar", err="baz"} in handler)))

(define (test-rewriting-negrewriteterm-negstring-in-annotated-wrapped-record-field)

  (assert (equal? wrap(User){record(UserData){name="bar"}} wrap(User){record(UserData){name="bar"}})))

;; rewriteAndFoldTermWithPath

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-application--neg-sum-literals)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-nested-applications)

  (assert (equal? 3:int32 3:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-let-bindings)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-record-fields)

  (assert (equal? 30:int32 30:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-case-branches)

  (assert (equal? 3:int32 3:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-pair)

  (assert (equal? 12:int32 12:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-optional)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-wrapped-term)

  (assert (equal? 25:int32 25:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-type-lambda)

  (assert (equal? 100:int32 100:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-type-application)

  (assert (equal? 50:int32 50:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-set-elements)

  (assert (equal? 6:int32 6:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negdeep-nesting--neg-application-in-lambda-in-let)

  (assert (equal? 15:int32 15:int32)))

(define (test-rewriting-negrewriteandfoldtermwithpath-negcollect-list-lengths-in-nested-structure)

  (assert (equal? [2:int32, 2:int32, 1:int32] [2:int32, 2:int32, 1:int32])))

(define (test-rewriting-negrewriteandfoldtermwithpath-negcollect-list-lengths-in-let-body)

  (assert (equal? [2:int32, 1:int32] [2:int32, 1:int32])))

;; unshadowVariables

(define (test-rewriting-negunshadowvariables-negliteral-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-rewriting-negunshadowvariables-negvariable-unchanged)

  (assert (equal? x x)))

(define (test-rewriting-negunshadowvariables-negsingle-lambda-unchanged)

  (assert (equal? λx.x λx.x)))

(define (test-rewriting-negunshadowvariables-negdistinct-lambda-parameters-unchanged)

  (assert (equal? λx.λy.[x, y] λx.λy.[x, y])))

(define (test-rewriting-negunshadowvariables-neglet-with-no-shadowing-unchanged)

  (assert (equal? let x = 1:int32 in x let x = 1:int32 in x)))

(define (test-rewriting-negunshadowvariables-neglet-and-lambda-with-distinct-names-unchanged)

  (assert (equal? let x = 1:int32 in λy.[x, y] let x = 1:int32 in λy.[x, y])))

(define (test-rewriting-negunshadowvariables-neginner-lambda-shadows-outer-lambda)

  (assert (equal? λx.λx2.x2 λx.λx2.x2)))

(define (test-rewriting-negunshadowvariables-neginner-lambda-shadows-outer--neg-body-references-both)

  (assert (equal? λx.[x, λx2.x2] λx.[x, λx2.x2])))

(define (test-rewriting-negunshadowvariables-negtriple-nested-lambda-same-name)

  (assert (equal? λx.λx2.λx3.x3 λx.λx2.λx3.x3)))

(define (test-rewriting-negunshadowvariables-negtwo-parameters-shadow-sequentially)

  (assert (equal? λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(define (test-rewriting-negunshadowvariables-neglambda-shadows-let-negbound-variable)

  (assert (equal? let x = 1:int32 in λx2.x2 let x = 1:int32 in λx2.x2)))

(define (test-rewriting-negunshadowvariables-neglambda-shadows-one-of-multiple-let-bindings)

  (assert (equal? let x = 1:int32, y = 2:int32 in λx2.[x2, y] let x = 1:int32, y = 2:int32 in λx2.[x2, y])))

(define (test-rewriting-negunshadowvariables-neginner-let-body-with-lambda-shadowing-outer-let)

  (assert (equal? let x = 1:int32 in let y = 2:int32 in λx2.x2 let x = 1:int32 in let y = 2:int32 in λx2.x2)))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-in-function-position-of-application)

  (assert (equal? λf.(λf2.f2 @ f) λf.(λf2.f2 @ f))))

(define (test-rewriting-negunshadowvariables-negshadowed-lambdas-in-list-elements)

  (assert (equal? λx.[λx2.x2, λx2.x2] λx.[λx2.x2, λx2.x2])))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-in-record-field)

  (assert (equal? λx.record(Pair){fst=λx2.x2, snd=x} λx.record(Pair){fst=λx2.x2, snd=x})))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-in-case-branch)

  (assert (equal? λx.case(Maybe){nothing=0:int32, just=λx2.x2} λx.case(Maybe){nothing=0:int32, just=λx2.x2})))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-in-pair)

  (assert (equal? λx.(λx2.x2, x) λx.(λx2.x2, x))))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-inside-optional)

  (assert (equal? λx.just(λx2.x2) λx.just(λx2.x2))))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-inside-set-element)

  (assert (equal? λx.{λx2.x2} λx.{λx2.x2})))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-in-union-injection)

  (assert (equal? λx.inject(Result){ok=λx2.x2} λx.inject(Result){ok=λx2.x2})))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-inside-wrapped-term)

  (assert (equal? λx.wrap(Age){λx2.x2} λx.wrap(Age){λx2.x2})))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-inside-type-lambda)

  (assert (equal? λx.Λa.λx2.x2 λx.Λa.λx2.x2)))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-inside-type-application)

  (assert (equal? λx.λx2.x2⟨string⟩ λx.λx2.x2⟨string⟩)))

(define (test-rewriting-negunshadowvariables-negshadowed-lambda-inside-annotated-term)

  (assert (equal? λx.λx2.x2 λx.λx2.x2)))

(define (test-rewriting-negunshadowvariables-negshadowing-at-multiple-depths)

  (assert (equal? λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(define (test-rewriting-negunshadowvariables-neglet-then-lambda-then-lambda-all-same-name)

  (assert (equal? let x = 1:int32 in λx2.λx3.x3 let x = 1:int32 in λx2.λx3.x3)))

(define (test-rewriting-negunshadowvariables-neglambda-with-shadowing-in-let-binding-value)

  (assert (equal? λx.let y = λx2.x2 in (y @ x) λx.let y = λx2.x2 in (y @ x))))

(define (test-rewriting-negunshadowvariables-negapplication-without-shadowing-unchanged)

  (assert (equal? (f @ 42:int32) (f @ 42:int32))))

(define (test-rewriting-negunshadowvariables-neglist-of-literals-unchanged)

  (assert (equal? [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(define (test-rewriting-negunshadowvariables-negnested-record-unchanged)

  (assert (equal? record(Point){x=10:int32, y=20:int32} record(Point){x=10:int32, y=20:int32})))

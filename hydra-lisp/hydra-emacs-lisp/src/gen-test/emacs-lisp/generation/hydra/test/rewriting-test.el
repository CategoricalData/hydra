;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; rewriting

(require 'ert)

;; freeVariables

(ert-deftest test-rewriting-negfreevariables-negstring-literal-has-no-free-variables ()

  (should (equal {} {})))

(ert-deftest test-rewriting-negfreevariables-negsingle-variable ()

  (should (equal {x} {x})))

(ert-deftest test-rewriting-negfreevariables-negbound-variable-is-not-free ()

  (should (equal {} {})))

(ert-deftest test-rewriting-negfreevariables-negunbound-variable-in-lambda-body ()

  (should (equal {x} {x})))

(ert-deftest test-rewriting-negfreevariables-negmixed-free-and-bound-variables ()

  (should (equal {x} {x})))

(ert-deftest test-rewriting-negfreevariables-negmultiple-free-variables ()

  (should (equal {x, y} {x, y})))

;; simplifyTerm

(ert-deftest test-rewriting-negsimplifyterm-negconst-application-with-literal ()

  (should (equal "foo" "foo")))

(ert-deftest test-rewriting-negsimplifyterm-negidentity-application ()

  (should (equal [y, y] [y, y])))

(ert-deftest test-rewriting-negsimplifyterm-negunused-parameter ()

  (should (equal "foo" "foo")))

(ert-deftest test-rewriting-negsimplifyterm-negnested-lambda-applications ()

  (should (equal ["foo", y] ["foo", y])))

;; flattenLetTerms

(ert-deftest test-rewriting-negflattenletterms-negnon-neglet-term-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negflattenletterms-neglist-term-unchanged ()

  (should (equal ["foo"] ["foo"])))

(ert-deftest test-rewriting-negflattenletterms-negsequential-lets-in-body-are-flattened ()

  (should (equal let x = 1:int32, y = 2:int32 in [x, y] let x = 1:int32, y = 2:int32 in [x, y])))

(ert-deftest test-rewriting-negflattenletterms-negnested-binding-in-let-value-is-flattened ()

  (should (equal let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b])))

(ert-deftest test-rewriting-negflattenletterms-negmultiple-levels-of-nesting-are-flattened ()

  (should (equal let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b])))

;; liftLambdaAboveLet

(ert-deftest test-rewriting-negliftlambdaabovelet-negsimple-let-with-lambda-in-body ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negbare-lambda-unchanged ()

  (should (equal λx.x λx.x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negbare-let-unchanged ()

  (should (equal let x = 42:int32 in x let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglambda-with-let-in-body-unchanged ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-with-two-nested-lambdas ()

  (should (equal λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglambda-inside-let-body-already-above-let ()

  (should (equal λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-without-lambda-in-body-unchanged ()

  (should (equal let x = 42:int32, y = "hello" in (x, y) let x = 42:int32, y = "hello" in (x, y))))

(ert-deftest test-rewriting-negliftlambdaabovelet-negmultiple-let-bindings-with-lambda ()

  (should (equal λz.let x = 42:int32, y = "hello" in x λz.let x = 42:int32, y = "hello" in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negnested-lets-with-lambda-at-innermost-level ()

  (should (equal λz.let x = 42:int32 in let y = "hello" in x λz.let x = 42:int32 in let y = "hello" in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglambda-between-two-lets ()

  (should (equal λy.let x = 42:int32 in let z = "hello" in x λy.let x = 42:int32 in let z = "hello" in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negmultiple-lambdas-between-nested-lets ()

  (should (equal λx.λy.let a = 1:int32 in let b = 2:int32 in a λx.λy.let a = 1:int32 in let b = 2:int32 in a)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negmultiple-lambdas-already-above-let ()

  (should (equal λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negannotation-above-let-containing-lambda ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negannotation-above-lambda-in-let-body ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negannotation-between-two-lambdas ()

  (should (equal λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negannotation-on-the-body-of-lambda-in-let ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-negannotation-on-lambda-already-above-let ()

  (should (equal λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-a-list ()

  (should (equal [1:int32, λy.let x = 42:int32 in x, 2:int32] [1:int32, λy.let x = 42:int32 in x, 2:int32])))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-multiple-list-elements ()

  (should (equal [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z] [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z])))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-a-let-binding-value ()

  (should (equal let f = λy.let x = 42:int32 in x in f let f = λy.let x = 42:int32 in x in f)))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-a-pair ()

  (should (equal (λy.let x = 42:int32 in x, "test") (λy.let x = 42:int32 in x, "test"))))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-both-elements-of-a-pair ()

  (should (equal (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z) (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z))))

(ert-deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-lambda-body ()

  (should (equal λouter.λinner.let x = 42:int32 in x λouter.λinner.let x = 42:int32 in x)))

;; deannotateTerm

(ert-deftest test-rewriting-negdeannotateterm-negunannotated-literal-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negdeannotateterm-negunannotated-variable-unchanged ()

  (should (equal x x)))

(ert-deftest test-rewriting-negdeannotateterm-negunannotated-lambda-unchanged ()

  (should (equal λx.x λx.x)))

(ert-deftest test-rewriting-negdeannotateterm-negsingle-annotation-stripped ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negdeannotateterm-negnested-annotations-stripped ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negdeannotateterm-negannotated-lambda-stripped ()

  (should (equal λx.x λx.x)))

(ert-deftest test-rewriting-negdeannotateterm-negannotated-application-stripped ()

  (should (equal (f @ x) (f @ x))))

;; deannotateType

(ert-deftest test-rewriting-negdeannotatetype-negunannotated-primitive-type-unchanged ()

  (should (equal int32 int32)))

(ert-deftest test-rewriting-negdeannotatetype-negunannotated-string-type-unchanged ()

  (should (equal string string)))

(ert-deftest test-rewriting-negdeannotatetype-negunannotated-function-type-unchanged ()

  (should (equal (int32 → string) (int32 → string))))

(ert-deftest test-rewriting-negdeannotatetype-negsingle-annotation-stripped ()

  (should (equal int32 int32)))

(ert-deftest test-rewriting-negdeannotatetype-negnested-annotations-stripped ()

  (should (equal string string)))

(ert-deftest test-rewriting-negdeannotatetype-negannotated-list-type-stripped ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-rewriting-negdeannotatetype-negannotated-function-type-stripped ()

  (should (equal (int32 → string) (int32 → string))))

;; topologicalSortBindings

(ert-deftest test-rewriting-negtopologicalsortbindings-negisolated-bindings ()

  (should (equal [[(a, "foo")], [(b, "bar")]] [[(a, "foo")], [(b, "bar")]])))

(ert-deftest test-rewriting-negtopologicalsortbindings-negsingle-recursive-binding ()

  (should (equal [[(a, [a])]] [[(a, [a])]])))

(ert-deftest test-rewriting-negtopologicalsortbindings-negmutually-recursive-bindings ()

  (should (equal [[(a, [b]), (b, [a])]] [[(a, [b]), (b, [a])]])))

(ert-deftest test-rewriting-negtopologicalsortbindings-negmixed-bindings ()

  (should (equal [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]] [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]])))

;; normalizeTypeVariables

(ert-deftest test-rewriting-negnormalizetypevariables-negliteral-without-type-variables-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-negsimple-let-without-type-annotations-unchanged ()

  (should (equal let foo = "foo" in 42:int32 let foo = "foo" in 42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-neglet-with-monomorphic-type-scheme-unchanged ()

  (should (equal let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-neglet-with-monomorphic-binding-referencing-string ()

  (should (equal let foo:((string)) = "foo" in 42:int32 let foo:((string)) = "foo" in 42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-negpolymorphic-binding-with-free-type-variable-unchanged ()

  (should (equal let foo:((a)) = bar in 42:int32 let foo:((a)) = bar in 42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-negmonomorphic-binding-with-typed-lambda-unchanged ()

  (should (equal let foo:((string)) = "foo" in λx:(a → int32).42:int32 let foo:((string)) = "foo" in λx:(a → int32).42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-negpolymorphic-binding-with-typed-lambda-in-body-unchanged ()

  (should (equal let foo:((a)) = bar in λx:(a → int32).42:int32 let foo:((a)) = bar in λx:(a → int32).42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-negpolymorphic-identity-function-normalized ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(ert-deftest test-rewriting-negnormalizetypevariables-negpolymorphic-const-function-normalized ()

  (should (equal let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo") let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo"))))

(ert-deftest test-rewriting-negnormalizetypevariables-negbinding-rewriting-does-not-affect-body-with-typed-lambda ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32 let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32)))

(ert-deftest test-rewriting-negnormalizetypevariables-negnested-polymorphic-lets-normalized ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)))))

(ert-deftest test-rewriting-negnormalizetypevariables-negnested-same-substitution-in-bindings-and-environment ()

  (should (equal let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(ert-deftest test-rewriting-negnormalizetypevariables-negparent-type-variable-shadows-child-variable ()

  (should (equal let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(ert-deftest test-rewriting-negnormalizetypevariables-negno-shadowing-distinct-type-variables ()

  (should (equal let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32) let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(ert-deftest test-rewriting-negnormalizetypevariables-neglocally-free-type-variable-in-nested-binding ()

  (should (equal let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32) let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32))))

;; etaExpandTerm

(ert-deftest test-rewriting-negetaexpandterm-neginteger-literal-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negetaexpandterm-negstring-list-unchanged ()

  (should (equal ["foo", "bar"] ["foo", "bar"])))

(ert-deftest test-rewriting-negetaexpandterm-negfully-applied-binary-function-unchanged ()

  (should (equal (hydra.lib.strings.splitOn! @ "foo" @ "bar") (hydra.lib.strings.splitOn! @ "foo" @ "bar"))))

(ert-deftest test-rewriting-negetaexpandterm-neglambda-with-fully-applied-primitive-unchanged ()

  (should (equal λx.(hydra.lib.strings.splitOn! @ "," @ x) λx.(hydra.lib.strings.splitOn! @ "," @ x))))

(ert-deftest test-rewriting-negetaexpandterm-neglambda-returning-constant-unchanged ()

  (should (equal λx.42:int32 λx.42:int32)))

(ert-deftest test-rewriting-negetaexpandterm-negbare-unary-primitive-unchanged ()

  (should (equal hydra.lib.strings.toLower! hydra.lib.strings.toLower!)))

(ert-deftest test-rewriting-negetaexpandterm-negbare-binary-primitive-unchanged ()

  (should (equal hydra.lib.strings.splitOn! hydra.lib.strings.splitOn!)))

(ert-deftest test-rewriting-negetaexpandterm-negpartially-applied-binary-primitive-expands-to-one-lambda ()

  (should (equal λv1.(hydra.lib.strings.splitOn! @ foo @ v1) λv1.(hydra.lib.strings.splitOn! @ foo @ v1))))

(ert-deftest test-rewriting-negetaexpandterm-negprojection-expands-to-lambda ()

  (should (equal λv1.(project(Person){firstName} @ v1) λv1.(project(Person){firstName} @ v1))))

(ert-deftest test-rewriting-negetaexpandterm-negpartial-application-inside-lambda-expands ()

  (should (equal λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1) λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1))))

(ert-deftest test-rewriting-negetaexpandterm-neglet-with-constant-body-unchanged ()

  (should (equal let foo = 137:int32 in 42:int32 let foo = 137:int32 in 42:int32)))

(ert-deftest test-rewriting-negetaexpandterm-neglet-with-bare-primitive-value-unchanged ()

  (should (equal let foo = hydra.lib.strings.splitOn! in foo let foo = hydra.lib.strings.splitOn! in foo)))

(ert-deftest test-rewriting-negetaexpandterm-negfully-applied-unary-unchanged ()

  (should (equal (hydra.lib.strings.toLower! @ "FOO") (hydra.lib.strings.toLower! @ "FOO"))))

(ert-deftest test-rewriting-negetaexpandterm-negpartial-application-in-list-expands ()

  (should (equal [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)] [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)])))

;; foldOverTerm

(ert-deftest test-rewriting-negfoldoverterm-negcollect-labels-from-single-node--neg-pre-negorder ()

  (should (equal ["a"] ["a"])))

(ert-deftest test-rewriting-negfoldoverterm-negcollect-labels-from-tree--neg-pre-negorder ()

  (should (equal ["a", "b", "c", "d"] ["a", "b", "c", "d"])))

(ert-deftest test-rewriting-negfoldoverterm-negcollect-labels-from-single-node--neg-post-negorder ()

  (should (equal ["a"] ["a"])))

(ert-deftest test-rewriting-negfoldoverterm-negcollect-labels-from-tree--neg-post-negorder ()

  (should (equal ["b", "d", "c", "a"] ["b", "d", "c", "a"])))

(ert-deftest test-rewriting-negfoldoverterm-negsum-int32-literals ()

  (should (equal 52:int32 52:int32)))

(ert-deftest test-rewriting-negfoldoverterm-negcollect-list-lengths--neg-pre-negorder ()

  (should (equal [2:int32, 2:int32, 1:int32] [2:int32, 2:int32, 1:int32])))

(ert-deftest test-rewriting-negfoldoverterm-negcollect-list-lengths--neg-post-negorder ()

  (should (equal [2:int32, 1:int32, 2:int32] [2:int32, 1:int32, 2:int32])))

;; rewriteType

(ert-deftest test-rewriting-negrewritetype-negstring-type-in-left-side-of-either-is-replaced ()

  (should (equal either<int32, int32> either<int32, int32>)))

(ert-deftest test-rewriting-negrewritetype-negstring-type-in-right-side-of-either-is-replaced ()

  (should (equal either<int32, int32> either<int32, int32>)))

(ert-deftest test-rewriting-negrewritetype-negstring-types-in-both-sides-of-either-are-replaced ()

  (should (equal either<int32, int32> either<int32, int32>)))

(ert-deftest test-rewriting-negrewritetype-negstring-type-in-nested-either-left-of-left-is-replaced ()

  (should (equal either<either<int32, int32>, int64> either<either<int32, int32>, int64>)))

(ert-deftest test-rewriting-negrewritetype-negstring-type-in-nested-either-right-of-right-is-replaced ()

  (should (equal either<int64, either<int32, int32>> either<int64, either<int32, int32>>)))

(ert-deftest test-rewriting-negrewritetype-negstring-types-in-complex-nested-either-are-all-replaced ()

  (should (equal either<either<int32, int32>, either<int32, int64>> either<either<int32, int32>, either<int32, int64>>)))

(ert-deftest test-rewriting-negrewritetype-negstring-in-list-type-is-replaced ()

  (should (equal list<int32> list<int32>)))

(ert-deftest test-rewriting-negrewritetype-negstring-in-function-domain-is-replaced ()

  (should (equal (int32 → int64) (int32 → int64))))

(ert-deftest test-rewriting-negrewritetype-negstring-in-function-codomain-is-replaced ()

  (should (equal (int64 → int32) (int64 → int32))))

(ert-deftest test-rewriting-negrewritetype-negstring-in-optional-type-is-replaced ()

  (should (equal maybe<int32> maybe<int32>)))

;; rewriteTerm

(ert-deftest test-rewriting-negrewriteterm-negstring-literal-foo-replaced-with-bar ()

  (should (equal "bar" "bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-variable-not-changed ()

  (should (equal x x)))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-list ()

  (should (equal ["bar", "baz"] ["bar", "baz"])))

(ert-deftest test-rewriting-negrewriteterm-negmultiple-strings-in-list ()

  (should (equal ["bar", "bar", "baz"] ["bar", "bar", "baz"])))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-optional-just ()

  (should (equal just("bar") just("bar"))))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-function-application ()

  (should (equal (print @ "bar") (print @ "bar"))))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-lambda-body ()

  (should (equal λx."bar" λx."bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-nested-applications ()

  (should (equal (f @ (g @ "bar")) (f @ (g @ "bar")))))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-record-field ()

  (should (equal record(Person){name="bar"} record(Person){name="bar"})))

(ert-deftest test-rewriting-negrewriteterm-negstrings-in-multiple-record-fields ()

  (should (equal record(Data){a="bar", b="baz", c="bar"} record(Data){a="bar", b="baz", c="bar"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-pair ()

  (should (equal ("bar", 42:int32) ("bar", 42:int32))))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-let-binding-value ()

  (should (equal let x = "bar" in x let x = "bar" in x)))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-let-body ()

  (should (equal let x = 1:int32 in "bar" let x = 1:int32 in "bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-first-case-branch ()

  (should (equal case(Result){success="bar", error="baz"} case(Result){success="bar", error="baz"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-second-case-branch ()

  (should (equal case(Result){success="baz", error="bar"} case(Result){success="baz", error="bar"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-default-branch ()

  (should (equal case(Result){success="baz", error="baz", [default]="bar"} case(Result){success="baz", error="baz", [default]="bar"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-deeply-nested-in-record-in-list-in-application ()

  (should (equal (process @ [record(Item){value="bar"}]) (process @ [record(Item){value="bar"}]))))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-union-inject-value ()

  (should (equal inject(Result){success="bar"} inject(Result){success="bar"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-wrapped-term ()

  (should (equal wrap(Email){"bar"} wrap(Email){"bar"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-annotated-term-body ()

  (should (equal "bar" "bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-first-of-multiple-let-bindings ()

  (should (equal let x = "bar", y = "baz" in x let x = "bar", y = "baz" in x)))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-second-of-multiple-let-bindings ()

  (should (equal let x = "baz", y = "bar" in y let x = "baz", y = "bar" in y)))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-all-let-bindings-and-body ()

  (should (equal let x = "bar", y = "bar" in "bar" let x = "bar", y = "bar" in "bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-set ()

  (should (equal {"bar", "baz"} {"bar", "baz"})))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-type-lambda-body ()

  (should (equal Λa."bar" Λa."bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-type-application-body ()

  (should (equal "bar"⟨string⟩ "bar"⟨string⟩)))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-nested-type-lambdas ()

  (should (equal Λa.Λb."bar" Λa.Λb."bar")))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-case-branch-within-let-binding ()

  (should (equal let handler = case(Result){ok="bar", err="baz"} in handler let handler = case(Result){ok="bar", err="baz"} in handler)))

(ert-deftest test-rewriting-negrewriteterm-negstring-in-annotated-wrapped-record-field ()

  (should (equal wrap(User){record(UserData){name="bar"}} wrap(User){record(UserData){name="bar"}})))

;; rewriteAndFoldTermWithPath

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-application--neg-sum-literals ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-nested-applications ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-let-bindings ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-record-fields ()

  (should (equal 30:int32 30:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-case-branches ()

  (should (equal 3:int32 3:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-pair ()

  (should (equal 12:int32 12:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-optional ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-wrapped-term ()

  (should (equal 25:int32 25:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-type-lambda ()

  (should (equal 100:int32 100:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-type-application ()

  (should (equal 50:int32 50:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-set-elements ()

  (should (equal 6:int32 6:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negdeep-nesting--neg-application-in-lambda-in-let ()

  (should (equal 15:int32 15:int32)))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negcollect-list-lengths-in-nested-structure ()

  (should (equal [2:int32, 2:int32, 1:int32] [2:int32, 2:int32, 1:int32])))

(ert-deftest test-rewriting-negrewriteandfoldtermwithpath-negcollect-list-lengths-in-let-body ()

  (should (equal [2:int32, 1:int32] [2:int32, 1:int32])))

;; unshadowVariables

(ert-deftest test-rewriting-negunshadowvariables-negliteral-unchanged ()

  (should (equal 42:int32 42:int32)))

(ert-deftest test-rewriting-negunshadowvariables-negvariable-unchanged ()

  (should (equal x x)))

(ert-deftest test-rewriting-negunshadowvariables-negsingle-lambda-unchanged ()

  (should (equal λx.x λx.x)))

(ert-deftest test-rewriting-negunshadowvariables-negdistinct-lambda-parameters-unchanged ()

  (should (equal λx.λy.[x, y] λx.λy.[x, y])))

(ert-deftest test-rewriting-negunshadowvariables-neglet-with-no-shadowing-unchanged ()

  (should (equal let x = 1:int32 in x let x = 1:int32 in x)))

(ert-deftest test-rewriting-negunshadowvariables-neglet-and-lambda-with-distinct-names-unchanged ()

  (should (equal let x = 1:int32 in λy.[x, y] let x = 1:int32 in λy.[x, y])))

(ert-deftest test-rewriting-negunshadowvariables-neginner-lambda-shadows-outer-lambda ()

  (should (equal λx.λx2.x2 λx.λx2.x2)))

(ert-deftest test-rewriting-negunshadowvariables-neginner-lambda-shadows-outer--neg-body-references-both ()

  (should (equal λx.[x, λx2.x2] λx.[x, λx2.x2])))

(ert-deftest test-rewriting-negunshadowvariables-negtriple-nested-lambda-same-name ()

  (should (equal λx.λx2.λx3.x3 λx.λx2.λx3.x3)))

(ert-deftest test-rewriting-negunshadowvariables-negtwo-parameters-shadow-sequentially ()

  (should (equal λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(ert-deftest test-rewriting-negunshadowvariables-neglambda-shadows-let-negbound-variable ()

  (should (equal let x = 1:int32 in λx2.x2 let x = 1:int32 in λx2.x2)))

(ert-deftest test-rewriting-negunshadowvariables-neglambda-shadows-one-of-multiple-let-bindings ()

  (should (equal let x = 1:int32, y = 2:int32 in λx2.[x2, y] let x = 1:int32, y = 2:int32 in λx2.[x2, y])))

(ert-deftest test-rewriting-negunshadowvariables-neginner-let-body-with-lambda-shadowing-outer-let ()

  (should (equal let x = 1:int32 in let y = 2:int32 in λx2.x2 let x = 1:int32 in let y = 2:int32 in λx2.x2)))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-function-position-of-application ()

  (should (equal λf.(λf2.f2 @ f) λf.(λf2.f2 @ f))))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambdas-in-list-elements ()

  (should (equal λx.[λx2.x2, λx2.x2] λx.[λx2.x2, λx2.x2])))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-record-field ()

  (should (equal λx.record(Pair){fst=λx2.x2, snd=x} λx.record(Pair){fst=λx2.x2, snd=x})))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-case-branch ()

  (should (equal λx.case(Maybe){nothing=0:int32, just=λx2.x2} λx.case(Maybe){nothing=0:int32, just=λx2.x2})))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-pair ()

  (should (equal λx.(λx2.x2, x) λx.(λx2.x2, x))))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-optional ()

  (should (equal λx.just(λx2.x2) λx.just(λx2.x2))))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-set-element ()

  (should (equal λx.{λx2.x2} λx.{λx2.x2})))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-union-injection ()

  (should (equal λx.inject(Result){ok=λx2.x2} λx.inject(Result){ok=λx2.x2})))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-wrapped-term ()

  (should (equal λx.wrap(Age){λx2.x2} λx.wrap(Age){λx2.x2})))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-type-lambda ()

  (should (equal λx.Λa.λx2.x2 λx.Λa.λx2.x2)))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-type-application ()

  (should (equal λx.λx2.x2⟨string⟩ λx.λx2.x2⟨string⟩)))

(ert-deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-annotated-term ()

  (should (equal λx.λx2.x2 λx.λx2.x2)))

(ert-deftest test-rewriting-negunshadowvariables-negshadowing-at-multiple-depths ()

  (should (equal λx.λy.λx2.λy2.[x2, y2] λx.λy.λx2.λy2.[x2, y2])))

(ert-deftest test-rewriting-negunshadowvariables-neglet-then-lambda-then-lambda-all-same-name ()

  (should (equal let x = 1:int32 in λx2.λx3.x3 let x = 1:int32 in λx2.λx3.x3)))

(ert-deftest test-rewriting-negunshadowvariables-neglambda-with-shadowing-in-let-binding-value ()

  (should (equal λx.let y = λx2.x2 in (y @ x) λx.let y = λx2.x2 in (y @ x))))

(ert-deftest test-rewriting-negunshadowvariables-negapplication-without-shadowing-unchanged ()

  (should (equal (f @ 42:int32) (f @ 42:int32))))

(ert-deftest test-rewriting-negunshadowvariables-neglist-of-literals-unchanged ()

  (should (equal [1:int32, 2:int32, 3:int32] [1:int32, 2:int32, 3:int32])))

(ert-deftest test-rewriting-negunshadowvariables-negnested-record-unchanged ()

  (should (equal record(Point){x=10:int32, y=20:int32} record(Point){x=10:int32, y=20:int32})))

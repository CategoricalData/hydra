;; Note: this is an automatically generated file. Do not edit.
;; rewriting

(ns test-ns
  (:require [clojure.test :refer :all]))

;; freeVariables

(deftest test-rewriting-negfreevariables-negstring-literal-has-no-free-variables

  (is (= {}

         {})))

(deftest test-rewriting-negfreevariables-negsingle-variable

  (is (= {x}

         {x})))

(deftest test-rewriting-negfreevariables-negbound-variable-is-not-free

  (is (= {}

         {})))

(deftest test-rewriting-negfreevariables-negunbound-variable-in-lambda-body

  (is (= {x}

         {x})))

(deftest test-rewriting-negfreevariables-negmixed-free-and-bound-variables

  (is (= {x}

         {x})))

(deftest test-rewriting-negfreevariables-negmultiple-free-variables

  (is (= {x, y}

         {x, y})))

;; simplifyTerm

(deftest test-rewriting-negsimplifyterm-negconst-application-with-literal

  (is (= "foo"

         "foo")))

(deftest test-rewriting-negsimplifyterm-negidentity-application

  (is (= [y, y]

         [y, y])))

(deftest test-rewriting-negsimplifyterm-negunused-parameter

  (is (= "foo"

         "foo")))

(deftest test-rewriting-negsimplifyterm-negnested-lambda-applications

  (is (= ["foo", y]

         ["foo", y])))

;; flattenLetTerms

(deftest test-rewriting-negflattenletterms-negnon-neglet-term-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negflattenletterms-neglist-term-unchanged

  (is (= ["foo"]

         ["foo"])))

(deftest test-rewriting-negflattenletterms-negsequential-lets-in-body-are-flattened

  (is (= let x = 1:int32, y = 2:int32 in [x, y]

         let x = 1:int32, y = 2:int32 in [x, y])))

(deftest test-rewriting-negflattenletterms-negnested-binding-in-let-value-is-flattened

  (is (= let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b]

         let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b])))

(deftest test-rewriting-negflattenletterms-negmultiple-levels-of-nesting-are-flattened

  (is (= let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b]

         let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b])))

;; liftLambdaAboveLet

(deftest test-rewriting-negliftlambdaabovelet-negsimple-let-with-lambda-in-body

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-negbare-lambda-unchanged

  (is (= λx.x

         λx.x)))

(deftest test-rewriting-negliftlambdaabovelet-negbare-let-unchanged

  (is (= let x = 42:int32 in x

         let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-neglambda-with-let-in-body-unchanged

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-neglet-with-two-nested-lambdas

  (is (= λy.λz.let x = 42:int32 in x

         λy.λz.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-neglambda-inside-let-body-already-above-let

  (is (= λx.λy.let z = 42:int32 in z

         λx.λy.let z = 42:int32 in z)))

(deftest test-rewriting-negliftlambdaabovelet-neglet-without-lambda-in-body-unchanged

  (is (= let x = 42:int32, y = "hello" in (x, y)

         let x = 42:int32, y = "hello" in (x, y))))

(deftest test-rewriting-negliftlambdaabovelet-negmultiple-let-bindings-with-lambda

  (is (= λz.let x = 42:int32, y = "hello" in x

         λz.let x = 42:int32, y = "hello" in x)))

(deftest test-rewriting-negliftlambdaabovelet-negnested-lets-with-lambda-at-innermost-level

  (is (= λz.let x = 42:int32 in let y = "hello" in x

         λz.let x = 42:int32 in let y = "hello" in x)))

(deftest test-rewriting-negliftlambdaabovelet-neglambda-between-two-lets

  (is (= λy.let x = 42:int32 in let z = "hello" in x

         λy.let x = 42:int32 in let z = "hello" in x)))

(deftest test-rewriting-negliftlambdaabovelet-negmultiple-lambdas-between-nested-lets

  (is (= λx.λy.let a = 1:int32 in let b = 2:int32 in a

         λx.λy.let a = 1:int32 in let b = 2:int32 in a)))

(deftest test-rewriting-negliftlambdaabovelet-negmultiple-lambdas-already-above-let

  (is (= λx.λy.let z = 42:int32 in z

         λx.λy.let z = 42:int32 in z)))

(deftest test-rewriting-negliftlambdaabovelet-negannotation-above-let-containing-lambda

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-negannotation-above-lambda-in-let-body

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-negannotation-between-two-lambdas

  (is (= λy.λz.let x = 42:int32 in x

         λy.λz.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-negannotation-on-the-body-of-lambda-in-let

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-negannotation-on-lambda-already-above-let

  (is (= λy.let x = 42:int32 in x

         λy.let x = 42:int32 in x)))

(deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-a-list

  (is (= [1:int32, λy.let x = 42:int32 in x, 2:int32]

         [1:int32, λy.let x = 42:int32 in x, 2:int32])))

(deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-multiple-list-elements

  (is (= [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z]

         [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z])))

(deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-a-let-binding-value

  (is (= let f = λy.let x = 42:int32 in x in f

         let f = λy.let x = 42:int32 in x in f)))

(deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-a-pair

  (is (= (λy.let x = 42:int32 in x, "test")

         (λy.let x = 42:int32 in x, "test"))))

(deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-in-both-elements-of-a-pair

  (is (= (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z)

         (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z))))

(deftest test-rewriting-negliftlambdaabovelet-neglet-neglambda-inside-lambda-body

  (is (= λouter.λinner.let x = 42:int32 in x

         λouter.λinner.let x = 42:int32 in x)))

;; deannotateTerm

(deftest test-rewriting-negdeannotateterm-negunannotated-literal-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negdeannotateterm-negunannotated-variable-unchanged

  (is (= x

         x)))

(deftest test-rewriting-negdeannotateterm-negunannotated-lambda-unchanged

  (is (= λx.x

         λx.x)))

(deftest test-rewriting-negdeannotateterm-negsingle-annotation-stripped

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negdeannotateterm-negnested-annotations-stripped

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negdeannotateterm-negannotated-lambda-stripped

  (is (= λx.x

         λx.x)))

(deftest test-rewriting-negdeannotateterm-negannotated-application-stripped

  (is (= (f @ x)

         (f @ x))))

;; deannotateType

(deftest test-rewriting-negdeannotatetype-negunannotated-primitive-type-unchanged

  (is (= int32

         int32)))

(deftest test-rewriting-negdeannotatetype-negunannotated-string-type-unchanged

  (is (= string

         string)))

(deftest test-rewriting-negdeannotatetype-negunannotated-function-type-unchanged

  (is (= (int32 → string)

         (int32 → string))))

(deftest test-rewriting-negdeannotatetype-negsingle-annotation-stripped

  (is (= int32

         int32)))

(deftest test-rewriting-negdeannotatetype-negnested-annotations-stripped

  (is (= string

         string)))

(deftest test-rewriting-negdeannotatetype-negannotated-list-type-stripped

  (is (= list<int32>

         list<int32>)))

(deftest test-rewriting-negdeannotatetype-negannotated-function-type-stripped

  (is (= (int32 → string)

         (int32 → string))))

;; topologicalSortBindings

(deftest test-rewriting-negtopologicalsortbindings-negisolated-bindings

  (is (= [[(a, "foo")], [(b, "bar")]]

         [[(a, "foo")], [(b, "bar")]])))

(deftest test-rewriting-negtopologicalsortbindings-negsingle-recursive-binding

  (is (= [[(a, [a])]]

         [[(a, [a])]])))

(deftest test-rewriting-negtopologicalsortbindings-negmutually-recursive-bindings

  (is (= [[(a, [b]), (b, [a])]]

         [[(a, [b]), (b, [a])]])))

(deftest test-rewriting-negtopologicalsortbindings-negmixed-bindings

  (is (= [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]]

         [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]])))

;; normalizeTypeVariables

(deftest test-rewriting-negnormalizetypevariables-negliteral-without-type-variables-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negnormalizetypevariables-negsimple-let-without-type-annotations-unchanged

  (is (= let foo = "foo" in 42:int32

         let foo = "foo" in 42:int32)))

(deftest test-rewriting-negnormalizetypevariables-neglet-with-monomorphic-type-scheme-unchanged

  (is (= let foo:((string)) = "foo" in 42:int32

         let foo:((string)) = "foo" in 42:int32)))

(deftest test-rewriting-negnormalizetypevariables-neglet-with-monomorphic-binding-referencing-string

  (is (= let foo:((string)) = "foo" in 42:int32

         let foo:((string)) = "foo" in 42:int32)))

(deftest test-rewriting-negnormalizetypevariables-negpolymorphic-binding-with-free-type-variable-unchanged

  (is (= let foo:((a)) = bar in 42:int32

         let foo:((a)) = bar in 42:int32)))

(deftest test-rewriting-negnormalizetypevariables-negmonomorphic-binding-with-typed-lambda-unchanged

  (is (= let foo:((string)) = "foo" in λx:(a → int32).42:int32

         let foo:((string)) = "foo" in λx:(a → int32).42:int32)))

(deftest test-rewriting-negnormalizetypevariables-negpolymorphic-binding-with-typed-lambda-in-body-unchanged

  (is (= let foo:((a)) = bar in λx:(a → int32).42:int32

         let foo:((a)) = bar in λx:(a → int32).42:int32)))

(deftest test-rewriting-negnormalizetypevariables-negpolymorphic-identity-function-normalized

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(deftest test-rewriting-negnormalizetypevariables-negpolymorphic-const-function-normalized

  (is (= let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo")

         let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo"))))

(deftest test-rewriting-negnormalizetypevariables-negbinding-rewriting-does-not-affect-body-with-typed-lambda

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32

         let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32)))

(deftest test-rewriting-negnormalizetypevariables-negnested-polymorphic-lets-normalized

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32))

         let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32)))))

(deftest test-rewriting-negnormalizetypevariables-negnested-same-substitution-in-bindings-and-environment

  (is (= let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32))))

(deftest test-rewriting-negnormalizetypevariables-negparent-type-variable-shadows-child-variable

  (is (= let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(deftest test-rewriting-negnormalizetypevariables-negno-shadowing-distinct-type-variables

  (is (= let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)

         let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32))))

(deftest test-rewriting-negnormalizetypevariables-neglocally-free-type-variable-in-nested-binding

  (is (= let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32)

         let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32))))

;; etaExpandTerm

(deftest test-rewriting-negetaexpandterm-neginteger-literal-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negetaexpandterm-negstring-list-unchanged

  (is (= ["foo", "bar"]

         ["foo", "bar"])))

(deftest test-rewriting-negetaexpandterm-negfully-applied-binary-function-unchanged

  (is (= (hydra.lib.strings.splitOn! @ "foo" @ "bar")

         (hydra.lib.strings.splitOn! @ "foo" @ "bar"))))

(deftest test-rewriting-negetaexpandterm-neglambda-with-fully-applied-primitive-unchanged

  (is (= λx.(hydra.lib.strings.splitOn! @ "," @ x)

         λx.(hydra.lib.strings.splitOn! @ "," @ x))))

(deftest test-rewriting-negetaexpandterm-neglambda-returning-constant-unchanged

  (is (= λx.42:int32

         λx.42:int32)))

(deftest test-rewriting-negetaexpandterm-negbare-unary-primitive-unchanged

  (is (= hydra.lib.strings.toLower!

         hydra.lib.strings.toLower!)))

(deftest test-rewriting-negetaexpandterm-negbare-binary-primitive-unchanged

  (is (= hydra.lib.strings.splitOn!

         hydra.lib.strings.splitOn!)))

(deftest test-rewriting-negetaexpandterm-negpartially-applied-binary-primitive-expands-to-one-lambda

  (is (= λv1.(hydra.lib.strings.splitOn! @ foo @ v1)

         λv1.(hydra.lib.strings.splitOn! @ foo @ v1))))

(deftest test-rewriting-negetaexpandterm-negprojection-expands-to-lambda

  (is (= λv1.(project(Person){firstName} @ v1)

         λv1.(project(Person){firstName} @ v1))))

(deftest test-rewriting-negetaexpandterm-negpartial-application-inside-lambda-expands

  (is (= λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1)

         λx.λv1.(hydra.lib.strings.splitOn! @ x @ v1))))

(deftest test-rewriting-negetaexpandterm-neglet-with-constant-body-unchanged

  (is (= let foo = 137:int32 in 42:int32

         let foo = 137:int32 in 42:int32)))

(deftest test-rewriting-negetaexpandterm-neglet-with-bare-primitive-value-unchanged

  (is (= let foo = hydra.lib.strings.splitOn! in foo

         let foo = hydra.lib.strings.splitOn! in foo)))

(deftest test-rewriting-negetaexpandterm-negfully-applied-unary-unchanged

  (is (= (hydra.lib.strings.toLower! @ "FOO")

         (hydra.lib.strings.toLower! @ "FOO"))))

(deftest test-rewriting-negetaexpandterm-negpartial-application-in-list-expands

  (is (= [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)]

         [λx.["foo"], λv1.(hydra.lib.strings.splitOn! @ "bar" @ v1)])))

;; foldOverTerm

(deftest test-rewriting-negfoldoverterm-negcollect-labels-from-single-node--neg-pre-negorder

  (is (= ["a"]

         ["a"])))

(deftest test-rewriting-negfoldoverterm-negcollect-labels-from-tree--neg-pre-negorder

  (is (= ["a", "b", "c", "d"]

         ["a", "b", "c", "d"])))

(deftest test-rewriting-negfoldoverterm-negcollect-labels-from-single-node--neg-post-negorder

  (is (= ["a"]

         ["a"])))

(deftest test-rewriting-negfoldoverterm-negcollect-labels-from-tree--neg-post-negorder

  (is (= ["b", "d", "c", "a"]

         ["b", "d", "c", "a"])))

(deftest test-rewriting-negfoldoverterm-negsum-int32-literals

  (is (= 52:int32

         52:int32)))

(deftest test-rewriting-negfoldoverterm-negcollect-list-lengths--neg-pre-negorder

  (is (= [2:int32, 2:int32, 1:int32]

         [2:int32, 2:int32, 1:int32])))

(deftest test-rewriting-negfoldoverterm-negcollect-list-lengths--neg-post-negorder

  (is (= [2:int32, 1:int32, 2:int32]

         [2:int32, 1:int32, 2:int32])))

;; rewriteType

(deftest test-rewriting-negrewritetype-negstring-type-in-left-side-of-either-is-replaced

  (is (= either<int32, int32>

         either<int32, int32>)))

(deftest test-rewriting-negrewritetype-negstring-type-in-right-side-of-either-is-replaced

  (is (= either<int32, int32>

         either<int32, int32>)))

(deftest test-rewriting-negrewritetype-negstring-types-in-both-sides-of-either-are-replaced

  (is (= either<int32, int32>

         either<int32, int32>)))

(deftest test-rewriting-negrewritetype-negstring-type-in-nested-either-left-of-left-is-replaced

  (is (= either<either<int32, int32>, int64>

         either<either<int32, int32>, int64>)))

(deftest test-rewriting-negrewritetype-negstring-type-in-nested-either-right-of-right-is-replaced

  (is (= either<int64, either<int32, int32>>

         either<int64, either<int32, int32>>)))

(deftest test-rewriting-negrewritetype-negstring-types-in-complex-nested-either-are-all-replaced

  (is (= either<either<int32, int32>, either<int32, int64>>

         either<either<int32, int32>, either<int32, int64>>)))

(deftest test-rewriting-negrewritetype-negstring-in-list-type-is-replaced

  (is (= list<int32>

         list<int32>)))

(deftest test-rewriting-negrewritetype-negstring-in-function-domain-is-replaced

  (is (= (int32 → int64)

         (int32 → int64))))

(deftest test-rewriting-negrewritetype-negstring-in-function-codomain-is-replaced

  (is (= (int64 → int32)

         (int64 → int32))))

(deftest test-rewriting-negrewritetype-negstring-in-optional-type-is-replaced

  (is (= maybe<int32>

         maybe<int32>)))

;; rewriteTerm

(deftest test-rewriting-negrewriteterm-negstring-literal-foo-replaced-with-bar

  (is (= "bar"

         "bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-variable-not-changed

  (is (= x

         x)))

(deftest test-rewriting-negrewriteterm-negstring-in-list

  (is (= ["bar", "baz"]

         ["bar", "baz"])))

(deftest test-rewriting-negrewriteterm-negmultiple-strings-in-list

  (is (= ["bar", "bar", "baz"]

         ["bar", "bar", "baz"])))

(deftest test-rewriting-negrewriteterm-negstring-in-optional-just

  (is (= just("bar")

         just("bar"))))

(deftest test-rewriting-negrewriteterm-negstring-in-function-application

  (is (= (print @ "bar")

         (print @ "bar"))))

(deftest test-rewriting-negrewriteterm-negstring-in-lambda-body

  (is (= λx."bar"

         λx."bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-nested-applications

  (is (= (f @ (g @ "bar"))

         (f @ (g @ "bar")))))

(deftest test-rewriting-negrewriteterm-negstring-in-record-field

  (is (= record(Person){name="bar"}

         record(Person){name="bar"})))

(deftest test-rewriting-negrewriteterm-negstrings-in-multiple-record-fields

  (is (= record(Data){a="bar", b="baz", c="bar"}

         record(Data){a="bar", b="baz", c="bar"})))

(deftest test-rewriting-negrewriteterm-negstring-in-pair

  (is (= ("bar", 42:int32)

         ("bar", 42:int32))))

(deftest test-rewriting-negrewriteterm-negstring-in-let-binding-value

  (is (= let x = "bar" in x

         let x = "bar" in x)))

(deftest test-rewriting-negrewriteterm-negstring-in-let-body

  (is (= let x = 1:int32 in "bar"

         let x = 1:int32 in "bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-first-case-branch

  (is (= case(Result){success="bar", error="baz"}

         case(Result){success="bar", error="baz"})))

(deftest test-rewriting-negrewriteterm-negstring-in-second-case-branch

  (is (= case(Result){success="baz", error="bar"}

         case(Result){success="baz", error="bar"})))

(deftest test-rewriting-negrewriteterm-negstring-in-default-branch

  (is (= case(Result){success="baz", error="baz", [default]="bar"}

         case(Result){success="baz", error="baz", [default]="bar"})))

(deftest test-rewriting-negrewriteterm-negstring-deeply-nested-in-record-in-list-in-application

  (is (= (process @ [record(Item){value="bar"}])

         (process @ [record(Item){value="bar"}]))))

(deftest test-rewriting-negrewriteterm-negstring-in-union-inject-value

  (is (= inject(Result){success="bar"}

         inject(Result){success="bar"})))

(deftest test-rewriting-negrewriteterm-negstring-in-wrapped-term

  (is (= wrap(Email){"bar"}

         wrap(Email){"bar"})))

(deftest test-rewriting-negrewriteterm-negstring-in-annotated-term-body

  (is (= "bar"

         "bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-first-of-multiple-let-bindings

  (is (= let x = "bar", y = "baz" in x

         let x = "bar", y = "baz" in x)))

(deftest test-rewriting-negrewriteterm-negstring-in-second-of-multiple-let-bindings

  (is (= let x = "baz", y = "bar" in y

         let x = "baz", y = "bar" in y)))

(deftest test-rewriting-negrewriteterm-negstring-in-all-let-bindings-and-body

  (is (= let x = "bar", y = "bar" in "bar"

         let x = "bar", y = "bar" in "bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-set

  (is (= {"bar", "baz"}

         {"bar", "baz"})))

(deftest test-rewriting-negrewriteterm-negstring-in-type-lambda-body

  (is (= Λa."bar"

         Λa."bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-type-application-body

  (is (= "bar"⟨string⟩

         "bar"⟨string⟩)))

(deftest test-rewriting-negrewriteterm-negstring-in-nested-type-lambdas

  (is (= Λa.Λb."bar"

         Λa.Λb."bar")))

(deftest test-rewriting-negrewriteterm-negstring-in-case-branch-within-let-binding

  (is (= let handler = case(Result){ok="bar", err="baz"} in handler

         let handler = case(Result){ok="bar", err="baz"} in handler)))

(deftest test-rewriting-negrewriteterm-negstring-in-annotated-wrapped-record-field

  (is (= wrap(User){record(UserData){name="bar"}}

         wrap(User){record(UserData){name="bar"}})))

;; rewriteAndFoldTermWithPath

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-application--neg-sum-literals

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-nested-applications

  (is (= 3:int32

         3:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-let-bindings

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-record-fields

  (is (= 30:int32

         30:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-case-branches

  (is (= 3:int32

         3:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-pair

  (is (= 12:int32

         12:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-optional

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-wrapped-term

  (is (= 25:int32

         25:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-type-lambda

  (is (= 100:int32

         100:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-type-application

  (is (= 50:int32

         50:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negpath-tracking-through-set-elements

  (is (= 6:int32

         6:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negdeep-nesting--neg-application-in-lambda-in-let

  (is (= 15:int32

         15:int32)))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negcollect-list-lengths-in-nested-structure

  (is (= [2:int32, 2:int32, 1:int32]

         [2:int32, 2:int32, 1:int32])))

(deftest test-rewriting-negrewriteandfoldtermwithpath-negcollect-list-lengths-in-let-body

  (is (= [2:int32, 1:int32]

         [2:int32, 1:int32])))

;; unshadowVariables

(deftest test-rewriting-negunshadowvariables-negliteral-unchanged

  (is (= 42:int32

         42:int32)))

(deftest test-rewriting-negunshadowvariables-negvariable-unchanged

  (is (= x

         x)))

(deftest test-rewriting-negunshadowvariables-negsingle-lambda-unchanged

  (is (= λx.x

         λx.x)))

(deftest test-rewriting-negunshadowvariables-negdistinct-lambda-parameters-unchanged

  (is (= λx.λy.[x, y]

         λx.λy.[x, y])))

(deftest test-rewriting-negunshadowvariables-neglet-with-no-shadowing-unchanged

  (is (= let x = 1:int32 in x

         let x = 1:int32 in x)))

(deftest test-rewriting-negunshadowvariables-neglet-and-lambda-with-distinct-names-unchanged

  (is (= let x = 1:int32 in λy.[x, y]

         let x = 1:int32 in λy.[x, y])))

(deftest test-rewriting-negunshadowvariables-neginner-lambda-shadows-outer-lambda

  (is (= λx.λx2.x2

         λx.λx2.x2)))

(deftest test-rewriting-negunshadowvariables-neginner-lambda-shadows-outer--neg-body-references-both

  (is (= λx.[x, λx2.x2]

         λx.[x, λx2.x2])))

(deftest test-rewriting-negunshadowvariables-negtriple-nested-lambda-same-name

  (is (= λx.λx2.λx3.x3

         λx.λx2.λx3.x3)))

(deftest test-rewriting-negunshadowvariables-negtwo-parameters-shadow-sequentially

  (is (= λx.λy.λx2.λy2.[x2, y2]

         λx.λy.λx2.λy2.[x2, y2])))

(deftest test-rewriting-negunshadowvariables-neglambda-shadows-let-negbound-variable

  (is (= let x = 1:int32 in λx2.x2

         let x = 1:int32 in λx2.x2)))

(deftest test-rewriting-negunshadowvariables-neglambda-shadows-one-of-multiple-let-bindings

  (is (= let x = 1:int32, y = 2:int32 in λx2.[x2, y]

         let x = 1:int32, y = 2:int32 in λx2.[x2, y])))

(deftest test-rewriting-negunshadowvariables-neginner-let-body-with-lambda-shadowing-outer-let

  (is (= let x = 1:int32 in let y = 2:int32 in λx2.x2

         let x = 1:int32 in let y = 2:int32 in λx2.x2)))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-function-position-of-application

  (is (= λf.(λf2.f2 @ f)

         λf.(λf2.f2 @ f))))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambdas-in-list-elements

  (is (= λx.[λx2.x2, λx2.x2]

         λx.[λx2.x2, λx2.x2])))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-record-field

  (is (= λx.record(Pair){fst=λx2.x2, snd=x}

         λx.record(Pair){fst=λx2.x2, snd=x})))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-case-branch

  (is (= λx.case(Maybe){nothing=0:int32, just=λx2.x2}

         λx.case(Maybe){nothing=0:int32, just=λx2.x2})))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-pair

  (is (= λx.(λx2.x2, x)

         λx.(λx2.x2, x))))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-optional

  (is (= λx.just(λx2.x2)

         λx.just(λx2.x2))))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-set-element

  (is (= λx.{λx2.x2}

         λx.{λx2.x2})))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-in-union-injection

  (is (= λx.inject(Result){ok=λx2.x2}

         λx.inject(Result){ok=λx2.x2})))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-wrapped-term

  (is (= λx.wrap(Age){λx2.x2}

         λx.wrap(Age){λx2.x2})))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-type-lambda

  (is (= λx.Λa.λx2.x2

         λx.Λa.λx2.x2)))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-type-application

  (is (= λx.λx2.x2⟨string⟩

         λx.λx2.x2⟨string⟩)))

(deftest test-rewriting-negunshadowvariables-negshadowed-lambda-inside-annotated-term

  (is (= λx.λx2.x2

         λx.λx2.x2)))

(deftest test-rewriting-negunshadowvariables-negshadowing-at-multiple-depths

  (is (= λx.λy.λx2.λy2.[x2, y2]

         λx.λy.λx2.λy2.[x2, y2])))

(deftest test-rewriting-negunshadowvariables-neglet-then-lambda-then-lambda-all-same-name

  (is (= let x = 1:int32 in λx2.λx3.x3

         let x = 1:int32 in λx2.λx3.x3)))

(deftest test-rewriting-negunshadowvariables-neglambda-with-shadowing-in-let-binding-value

  (is (= λx.let y = λx2.x2 in (y @ x)

         λx.let y = λx2.x2 in (y @ x))))

(deftest test-rewriting-negunshadowvariables-negapplication-without-shadowing-unchanged

  (is (= (f @ 42:int32)

         (f @ 42:int32))))

(deftest test-rewriting-negunshadowvariables-neglist-of-literals-unchanged

  (is (= [1:int32, 2:int32, 3:int32]

         [1:int32, 2:int32, 3:int32])))

(deftest test-rewriting-negunshadowvariables-negnested-record-unchanged

  (is (= record(Point){x=10:int32, y=20:int32}

         record(Point){x=10:int32, y=20:int32})))

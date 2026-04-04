;; Note: this is an automatically generated file. Do not edit.
;; rewriting

(ns test-ns
  (:require [clojure.test :refer :all]))

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

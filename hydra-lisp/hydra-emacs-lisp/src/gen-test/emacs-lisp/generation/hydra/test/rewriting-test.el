;;; Note: this is an automatically generated file. Do not edit. -*- lexical-binding: t; coding: utf-8 -*-
;;; rewriting

(require 'ert)

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

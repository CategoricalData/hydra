;; Note: this is an automatically generated file. Do not edit.
;; rewriting

(import (scheme base))

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

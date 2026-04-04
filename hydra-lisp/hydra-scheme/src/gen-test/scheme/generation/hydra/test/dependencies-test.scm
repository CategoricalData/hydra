;; Note: this is an automatically generated file. Do not edit.
;; dependencies

(import (scheme base))

;; simplifyTerm

(define (test-dependencies-negsimplifyterm-negconst-application-with-literal)

  (assert (equal? "foo" "foo")))

(define (test-dependencies-negsimplifyterm-negidentity-application)

  (assert (equal? [y, y] [y, y])))

(define (test-dependencies-negsimplifyterm-negunused-parameter)

  (assert (equal? "foo" "foo")))

(define (test-dependencies-negsimplifyterm-negnested-lambda-applications)

  (assert (equal? ["foo", y] ["foo", y])))

;; flattenLetTerms

(define (test-dependencies-negflattenletterms-negnon-neglet-term-unchanged)

  (assert (equal? 42:int32 42:int32)))

(define (test-dependencies-negflattenletterms-neglist-term-unchanged)

  (assert (equal? ["foo"] ["foo"])))

(define (test-dependencies-negflattenletterms-negsequential-lets-in-body-are-flattened)

  (assert (equal? let x = 1:int32, y = 2:int32 in [x, y] let x = 1:int32, y = 2:int32 in [x, y])))

(define (test-dependencies-negflattenletterms-negnested-binding-in-let-value-is-flattened)

  (assert (equal? let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b])))

(define (test-dependencies-negflattenletterms-negmultiple-levels-of-nesting-are-flattened)

  (assert (equal? let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b] let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b])))

;; liftLambdaAboveLet

(define (test-dependencies-negliftlambdaabovelet-negsimple-let-with-lambda-in-body)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-negbare-lambda-unchanged)

  (assert (equal? λx.x λx.x)))

(define (test-dependencies-negliftlambdaabovelet-negbare-let-unchanged)

  (assert (equal? let x = 42:int32 in x let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-neglambda-with-let-in-body-unchanged)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-neglet-with-two-nested-lambdas)

  (assert (equal? λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-neglambda-inside-let-body-already-above-let)

  (assert (equal? λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(define (test-dependencies-negliftlambdaabovelet-neglet-without-lambda-in-body-unchanged)

  (assert (equal? let x = 42:int32, y = "hello" in (x, y) let x = 42:int32, y = "hello" in (x, y))))

(define (test-dependencies-negliftlambdaabovelet-negmultiple-let-bindings-with-lambda)

  (assert (equal? λz.let x = 42:int32, y = "hello" in x λz.let x = 42:int32, y = "hello" in x)))

(define (test-dependencies-negliftlambdaabovelet-negnested-lets-with-lambda-at-innermost-level)

  (assert (equal? λz.let x = 42:int32 in let y = "hello" in x λz.let x = 42:int32 in let y = "hello" in x)))

(define (test-dependencies-negliftlambdaabovelet-neglambda-between-two-lets)

  (assert (equal? λy.let x = 42:int32 in let z = "hello" in x λy.let x = 42:int32 in let z = "hello" in x)))

(define (test-dependencies-negliftlambdaabovelet-negmultiple-lambdas-between-nested-lets)

  (assert (equal? λx.λy.let a = 1:int32 in let b = 2:int32 in a λx.λy.let a = 1:int32 in let b = 2:int32 in a)))

(define (test-dependencies-negliftlambdaabovelet-negmultiple-lambdas-already-above-let)

  (assert (equal? λx.λy.let z = 42:int32 in z λx.λy.let z = 42:int32 in z)))

(define (test-dependencies-negliftlambdaabovelet-negannotation-above-let-containing-lambda)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-negannotation-above-lambda-in-let-body)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-negannotation-between-two-lambdas)

  (assert (equal? λy.λz.let x = 42:int32 in x λy.λz.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-negannotation-on-the-body-of-lambda-in-let)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-negannotation-on-lambda-already-above-let)

  (assert (equal? λy.let x = 42:int32 in x λy.let x = 42:int32 in x)))

(define (test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-a-list)

  (assert (equal? [1:int32, λy.let x = 42:int32 in x, 2:int32] [1:int32, λy.let x = 42:int32 in x, 2:int32])))

(define (test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-multiple-list-elements)

  (assert (equal? [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z] [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z])))

(define (test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-a-let-binding-value)

  (assert (equal? let f = λy.let x = 42:int32 in x in f let f = λy.let x = 42:int32 in x in f)))

(define (test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-a-pair)

  (assert (equal? (λy.let x = 42:int32 in x, "test") (λy.let x = 42:int32 in x, "test"))))

(define (test-dependencies-negliftlambdaabovelet-neglet-neglambda-in-both-elements-of-a-pair)

  (assert (equal? (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z) (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z))))

(define (test-dependencies-negliftlambdaabovelet-neglet-neglambda-inside-lambda-body)

  (assert (equal? λouter.λinner.let x = 42:int32 in x λouter.λinner.let x = 42:int32 in x)))

;; topologicalSortBindings

(define (test-dependencies-negtopologicalsortbindings-negisolated-bindings)

  (assert (equal? [[(a, "foo")], [(b, "bar")]] [[(a, "foo")], [(b, "bar")]])))

(define (test-dependencies-negtopologicalsortbindings-negsingle-recursive-binding)

  (assert (equal? [[(a, [a])]] [[(a, [a])]])))

(define (test-dependencies-negtopologicalsortbindings-negmutually-recursive-bindings)

  (assert (equal? [[(a, [b]), (b, [a])]] [[(a, [b]), (b, [a])]])))

(define (test-dependencies-negtopologicalsortbindings-negmixed-bindings)

  (assert (equal? [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]] [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]])))

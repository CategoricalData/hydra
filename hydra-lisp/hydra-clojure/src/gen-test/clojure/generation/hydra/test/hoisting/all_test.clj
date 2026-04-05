;; Note: this is an automatically generated file. Do not edit.
;; hoisting

(ns test-ns
  (:require [clojure.test :refer :all]))

;; hoistCases

;; hoistSubterms

(deftest test-all-neghoistcases-neghoistsubterms-neghoistnothing-simple-let-unchanged

  (is (= let x = 42:int32 in x

         let x = 42:int32 in x)))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistnothing-let-with-list-in-body-unchanged

  (is (= let x = 1:int32 in [x, 2:int32, 3:int32]

         let x = 1:int32 in [x, 2:int32, 3:int32])))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistnothing-let-with-application-in-body-unchanged

  (is (= let f = g in (f @ (h @ 42:int32))

         let f = g in (f @ (h @ 42:int32)))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-list-in-body-is-hoisted-into-local-let

  (is (= let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32, 3:int32] in (f @ _hoist_x_body_1)

         let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32, 3:int32] in (f @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-multiple-lists-in-body-are-hoisted-together

  (is (= let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [3:int32, 4:int32] in (pair @ _hoist_x_body_1 @ _hoist_x_body_2)

         let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [3:int32, 4:int32] in (pair @ _hoist_x_body_1 @ _hoist_x_body_2))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-list-in-binding-value-is-hoisted-into-local-let

  (is (= let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in x

         let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in x)))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-nested-lists-hoisted-from-inside-out

  (is (= let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [_hoist_x_body_1, 3:int32] in (f @ _hoist_x_body_2)

         let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [_hoist_x_body_1, 3:int32] in (f @ _hoist_x_body_2))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistapplications-application-in-list-element-is-hoisted

  (is (= let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in [_hoist_x_body_1, y]

         let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in [_hoist_x_body_1, y])))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistapplications-application-in-record-field-is-hoisted

  (is (= let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in record(Data){value=_hoist_x_body_1}

         let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in record(Data){value=_hoist_x_body_1})))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistapplications-nested-applications-hoisted-from-inside-out

  (is (= let x = 1:int32 in let _hoist_x_body_1 = (g @ x), _hoist_x_body_2 = (f @ _hoist_x_body_1) in [_hoist_x_body_2]

         let x = 1:int32 in let _hoist_x_body_1 = (g @ x), _hoist_x_body_2 = (f @ _hoist_x_body_1) in [_hoist_x_body_2])))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistcasestatements-case-in-application-argument-is-hoisted

  (is (= let x = just(42:int32) in let _hoist_x_body_1 = case(Optional){just=λy.y, nothing=0:int32, [default]=x} in (f @ _hoist_x_body_1)

         let x = just(42:int32) in let _hoist_x_body_1 = case(Optional){just=λy.y, nothing=0:int32, [default]=x} in (f @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistcasestatements-case-in-list-element-is-hoisted

  (is (= let x = 1:int32 in let _hoist_x_body_1 = case(Result){ok=x, err=0:int32, [default]=y} in [_hoist_x_body_1]

         let x = 1:int32 in let _hoist_x_body_1 = case(Result){ok=x, err=0:int32, [default]=y} in [_hoist_x_body_1])))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-nested-let--neg-inner-let-processed-independently

  (is (= let x = 1:int32 in let y = 2:int32 in let _hoist_y_body_1 = [x, y] in (f @ _hoist_y_body_1)

         let x = 1:int32 in let y = 2:int32 in let _hoist_y_body_1 = [x, y] in (f @ _hoist_y_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-non-neglet-term-is-unchanged

  (is (= (f @ [1:int32, 2:int32, 3:int32])

         (f @ [1:int32, 2:int32, 3:int32]))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistapplications-bare-application-unchanged

  (is (= (f @ (g @ x))

         (f @ (g @ x)))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-term-referring-to-let-negbound-variable-needs-no-capture

  (is (= let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in (f @ _hoist_x_body_1)

         let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in (f @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-term-referring-to-lambda-above-let-needs-no-capture

  (is (= λy.let x = 1:int32 in let _hoist_x_body_1 = [y, x] in (f @ _hoist_x_body_1)

         λy.let x = 1:int32 in let _hoist_x_body_1 = [y, x] in (f @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-lambda-negbound-var-not-free-in-hoisted-term-needs-no-capture

  (is (= let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in λy.(f @ _hoist_x_body_1)

         let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in λy.(f @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-lambda-negbound-var-free-in-hoisted-term-requires-capture

  (is (= let x = 1:int32 in let _hoist_x_body_1 = λy.[x, y] in λy.(f @ (_hoist_x_body_1 @ y))

         let x = 1:int32 in let _hoist_x_body_1 = λy.[x, y] in λy.(f @ (_hoist_x_body_1 @ y)))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-only-free-lambda-negbound-vars-are-captured

  (is (= let x = 1:int32 in let _hoist_x_body_1 = λb.[x, b] in λa.λb.(f @ (_hoist_x_body_1 @ b))

         let x = 1:int32 in let _hoist_x_body_1 = λb.[x, b] in λa.λb.(f @ (_hoist_x_body_1 @ b)))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-stable-naming-for-binding-and-body

  (is (= let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in let _hoist_x_body_1 = [3:int32, 4:int32] in (g @ _hoist_x_body_1)

         let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in let _hoist_x_body_1 = [3:int32, 4:int32] in (g @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-stable-naming-for-multiple-bindings

  (is (= let x = let _hoist_x_1 = [1:int32] in (f @ _hoist_x_1), y = let _hoist_y_1 = [2:int32] in (g @ _hoist_y_1) in x

         let x = let _hoist_x_1 = [1:int32] in (f @ _hoist_x_1), y = let _hoist_y_1 = [2:int32] in (g @ _hoist_y_1) in x)))

(deftest test-all-neghoistcases-neghoistsubterms-neghoistlists-polymorphic-binding-with-self-negreference-below-hoisted-term

  (is (= let f = let _hoist_f_1 = λx.[x, 1:int32] in λx.(pair @ (f @ x) @ (_hoist_f_1 @ x)) in (f @ 42:int32)

         let f = let _hoist_f_1 = λx.[x, 1:int32] in λx.(pair @ (f @ x) @ (_hoist_f_1 @ x)) in (f @ 42:int32))))

;; hoistCaseStatements

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-at-top-level-of-let-body-is-not-hoisted

  (is (= let x = just(42:int32) in case(Optional){just=λy.y, nothing=0:int32, [default]=x}

         let x = just(42:int32) in case(Optional){just=λy.y, nothing=0:int32, [default]=x})))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-let-binding-value-is-not-hoisted

  (is (= let x = case(Optional){just=λz.z, nothing=0:int32, [default]=y} in x

         let x = case(Optional){just=λz.z, nothing=0:int32, [default]=y} in x)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-inside-lambda-body-is-not-hoisted

  (is (= let f = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (f @ just(42:int32))

         let f = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (f @ just(42:int32)))))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-inside-nested-lambdas-is-not-hoisted

  (is (= let f = λa.λb.case(Result){ok=b, err=0:int32, [default]=a} in f

         let f = λa.λb.case(Result){ok=b, err=0:int32, [default]=a} in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-as-lhs-of-one-application-is-not-hoisted

  (is (= let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f

         let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-wrapped-in-annotation-is-not-hoisted

  (is (= let f = case(Optional){just=λy.y, nothing=0:int32} in f

         let f = case(Optional){just=λy.y, nothing=0:int32} in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-lambda-with-one-application-is-not-hoisted

  (is (= let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f

         let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-as-rhs-of-application-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_f_1) in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_f_1) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-nested-application-lhs-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λz.λw.z, nothing=λw.0:int32} in (_hoist_f_1 @ x @ y) in f

         let f = let _hoist_f_1 = case(Optional){just=λz.λw.z, nothing=λw.0:int32} in (_hoist_f_1 @ x @ y) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-inside-list-element-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [_hoist_f_1] in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [_hoist_f_1] in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-inside-lambda-inside-list-is-hoisted

  (is (= let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in [λa.(_hoist_f_1 @ a)] in f

         let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in [λa.(_hoist_f_1 @ a)] in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-neglist-inside-lambda-is-not-hoisted-only-case-statements

  (is (= let f = λa.[a, 1:int32, 2:int32] in (f @ 0:int32)

         let f = λa.[a, 1:int32, 2:int32] in (f @ 0:int32))))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-binding-is-not-hoisted-case-in-arg-position-is-hoisted

  (is (= let x = case(Optional){just=λz.z, nothing=0:int32, [default]=a} in let _hoist_x_body_1 = case(Optional){just=λw.w, nothing=0:int32, [default]=b} in (f @ _hoist_x_body_1)

         let x = case(Optional){just=λz.z, nothing=0:int32, [default]=a} in let _hoist_x_body_1 = case(Optional){just=λw.w, nothing=0:int32, [default]=b} in (f @ _hoist_x_body_1))))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-nested-let-body-is-not-hoisted

  (is (= let x = 1:int32 in let y = 2:int32 in case(Optional){just=λw.w, nothing=0:int32, [default]=z}

         let x = 1:int32 in let y = 2:int32 in case(Optional){just=λw.w, nothing=0:int32, [default]=z})))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-let-inside-lambda-is-not-hoisted

  (is (= let f = λa.let x = 1:int32 in case(Optional){just=λy.y, nothing=0:int32, [default]=a} in f

         let f = λa.let x = 1:int32 in case(Optional){just=λy.y, nothing=0:int32, [default]=a} in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-lambda-inside-let-body-is-not-hoisted

  (is (= let x = 1:int32 in λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a}

         let x = 1:int32 in λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a})))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-with-let-pluslambda-plusapp-is-not-hoisted

  (is (= let f = λa.let x = 1:int32 in (case(Optional){just=λy.y, nothing=0:int32} @ x) in f

         let f = λa.let x = 1:int32 in (case(Optional){just=λy.y, nothing=0:int32} @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-triple-application-lhs-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λa.λb.λc.a, nothing=λb.λc.0:int32} in (_hoist_f_1 @ x @ y @ z) in f

         let f = let _hoist_f_1 = case(Optional){just=λa.λb.λc.a, nothing=λb.λc.0:int32} in (_hoist_f_1 @ x @ y @ z) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-as-second-argument-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ x @ _hoist_f_1) in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ x @ _hoist_f_1) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-both-arguments--neg-both-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_f_1 @ _hoist_f_2) in f

         let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_f_1 @ _hoist_f_2) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-second-list-element-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [1:int32, _hoist_f_1] in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [1:int32, _hoist_f_1] in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negmultiple-cases-in-list--neg-all-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in [_hoist_f_1, _hoist_f_2] in f

         let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in [_hoist_f_1, _hoist_f_2] in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-pair-first-element-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (_hoist_f_1, 1:int32) in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (_hoist_f_1, 1:int32) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-pair-second-element-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (1:int32, _hoist_f_1) in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (1:int32, _hoist_f_1) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-child-let-binding-hoisted-into-child

  (is (= let outer = let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_1) in inner in outer

         let outer = let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_1) in inner in outer)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-child-let-body-hoisted-into-child

  (is (= let outer = let inner = 1:int32 in let _hoist_inner_body_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_body_1) in outer

         let outer = let inner = 1:int32 in let _hoist_inner_body_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_body_1) in outer)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-at-top-level-of-child-let-not-hoisted

  (is (= let outer = let inner = case(Optional){just=λy.y, nothing=0:int32} in inner in outer

         let outer = let inner = case(Optional){just=λy.y, nothing=0:int32} in inner in outer)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcases-in-both-outer-and-child--neg-each-hoisted-locally

  (is (= let outer = let _hoist_outer_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a} in (f @ _hoist_outer_1 @ let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_inner_1) in inner) in outer

         let outer = let _hoist_outer_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a} in (f @ _hoist_outer_1 @ let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_inner_1) in inner) in outer)))

(deftest test-all-neghoistcases-neghoistcasestatements-neglambda-after-app-lhs-takes-us-out-of-top-level

  (is (= let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (λa.(_hoist_f_1 @ a) @ x) in f

         let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (λa.(_hoist_f_1 @ a) @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-inside-case-branch-is-not-hoisted

  (is (= let f = case(Optional){just=λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a}, nothing=0:int32, [default]=x} in f

         let f = case(Optional){just=λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a}, nothing=0:int32, [default]=x} in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-inside-case-default-branch-is-not-hoisted

  (is (= let f = case(Optional){just=λa.a, nothing=case(Optional){just=λb.b, nothing=0:int32, [default]=y}, [default]=x} in f

         let f = case(Optional){just=λa.a, nothing=case(Optional){just=λb.b, nothing=0:int32, [default]=y}, [default]=x} in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-arg-position-inside-case-branch-is-hoisted

  (is (= let f = let _hoist_f_1 = λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a} in case(Optional){just=λa.(g @ (_hoist_f_1 @ a)), nothing=0:int32, [default]=x} in f

         let f = let _hoist_f_1 = λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a} in case(Optional){just=λa.(g @ (_hoist_f_1 @ a)), nothing=0:int32, [default]=x} in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-let-body-inside-applied-case-default-is-hoisted

  (is (= let f = (case(Optional){just=λa.a, nothing=let b = (g @ x) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b)} @ x) in f

         let f = (case(Optional){just=λa.a, nothing=let b = (g @ x) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b)} @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-in-let-body-inside-applied-case-branch-is-hoisted

  (is (= let f = (case(Optional){just=λa.let b = (h @ a) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b), nothing=0:int32} @ x) in f

         let f = (case(Optional){just=λa.let b = (h @ a) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b), nothing=0:int32} @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-application-at-top-level-of-binding-is-not-hoisted

  (is (= let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f

         let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-application-in-arg-position-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ (_hoist_f_1 @ x)) in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ (_hoist_f_1 @ x)) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-application-inside-immediately-negapplied-lambda-is-hoisted

  (is (= let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (λa.(_hoist_f_1 @ a) @ x) in f

         let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (λa.(_hoist_f_1 @ a) @ x) in f)))

(deftest test-all-neghoistcases-neghoistcasestatements-negcase-application-in-lambda-body-is-not-hoisted

  (is (= let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f

         let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f)))

;; hoistLet

;; hoistLetBindings

(deftest test-all-neghoistlet-neghoistletbindings-negnested-let-inside-lambda-binding-hoisted-with-lambda-capture

  (is (= let f = λa.(hydra.lib.math.mul @ (f_g @ a) @ 2:int32), f_g = λa.(hydra.lib.math.add @ a @ 1:int32) in (f @ 10:int32)

         let f = λa.(hydra.lib.math.mul @ (f_g @ a) @ 2:int32), f_g = λa.(hydra.lib.math.add @ a @ 1:int32) in (f @ 10:int32))))

(deftest test-all-neghoistlet-neghoistletbindings-negtype-application-nested-let-outside-lambda-can-be-hoisted

  (is (= let f = λx.(hydra.lib.math.add @ x @ f_y)⟨int32⟩, f_y = 1:int32 in (f @ 10:int32)

         let f = λx.(hydra.lib.math.add @ x @ f_y)⟨int32⟩, f_y = 1:int32 in (f @ 10:int32))))

;; hoistPolymorphicLetBindings

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negno-polymorphic-bindings-simple-let-unchanged

  (is (= let x:((int32)) = 42:int32 in x

         let x:((int32)) = 42:int32 in x)))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negno-polymorphic-bindings-multiple-monomorphic-bindings

  (is (= let x:((int32)) = 1:int32, y:((string)) = "hi" in (pair @ x @ y)

         let x:((int32)) = 1:int32, y:((string)) = "hi" in (pair @ x @ y))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negsingle-polymorphic-binding-already-at-top-level

  (is (= let id:((forall a. (a → a))) = λx.x in (id @ 42:int32)

         let id:((forall a. (a → a))) = λx.x in (id @ 42:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-inside-lambda-no-capture

  (is (= let f:(((int32 → int32))) = λa.(f_id @ a), f_id:((forall b. (b → b))) = Λb.λx.x in (f @ 42:int32)

         let f:(((int32 → int32))) = λa.(f_id @ a), f_id:((forall b. (b → b))) = Λb.λx.x in (f @ 42:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-captures-lambda-variable-wrapped-in-lambda

  (is (= let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall b. (string → b → (string, b)))) = Λb.λa:string.λx.(pair @ a @ x) in (f @ "hello")

         let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall b. (string → b → (string, b)))) = Λb.λa:string.λx.(pair @ a @ x) in (f @ "hello"))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-captures-multiple-lambda-variables

  (is (= let f:(((int32 → int32 → int32))) = λa:int32.λb:int32.(f_g @ a @ b @ 42:int32), f_g:((forall c. (int32 → int32 → c → c))) = Λc.λa:int32.λb:int32.λx.(triple @ a @ b @ x) in (f @ 1:int32 @ 2:int32)

         let f:(((int32 → int32 → int32))) = λa:int32.λb:int32.(f_g @ a @ b @ 42:int32), f_g:((forall c. (int32 → int32 → c → c))) = Λc.λa:int32.λb:int32.λx.(triple @ a @ b @ x) in (f @ 1:int32 @ 2:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-captures-some-but-not-all-lambda-variables

  (is (= let f:(((int32 → int32 → (int32, int32)))) = λa:int32.λb:int32.(f_g @ a @ b), f_g:((forall c. (int32 → c → (int32, c)))) = Λc.λa:int32.λx.(pair @ a @ x) in (f @ 1:int32 @ 2:int32)

         let f:(((int32 → int32 → (int32, int32)))) = λa:int32.λb:int32.(f_g @ a @ b), f_g:((forall c. (int32 → c → (int32, c)))) = Λc.λa:int32.λx.(pair @ a @ x) in (f @ 1:int32 @ 2:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-captures-both-lambda-negbound-and-let-negbound-variables

  (is (= let f:(((int32 → int32))) = λa:int32.let x:((int32)) = 1:int32 in (f_g @ a @ x @ 42:int32), f_g:((forall b. (int32 → int32 → b → b))) = Λb.λa:int32.λx:int32.λy.(hydra.lib.math.add @ (hydra.lib.math.add @ a @ x) @ y) in (f @ 10:int32)

         let f:(((int32 → int32))) = λa:int32.let x:((int32)) = 1:int32 in (f_g @ a @ x @ 42:int32), f_g:((forall b. (int32 → int32 → b → b))) = Λb.λa:int32.λx:int32.λy.(hydra.lib.math.add @ (hydra.lib.math.add @ a @ x) @ y) in (f @ 10:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negsibling-polymorphic-bindings-inside-lambda-one-calls-the-other

  (is (= let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 42:int32), wrapper_g:((forall a. (int32 → a → a))) = Λa.λouter:int32.λy.(hydra.lib.math.add @ outer @ y), wrapper_h:((forall b. (int32 → b → b))) = Λb.λouter:int32.λz.(wrapper_g @ outer @ z) in (wrapper @ 10:int32)

         let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 42:int32), wrapper_g:((forall a. (int32 → a → a))) = Λa.λouter:int32.λy.(hydra.lib.math.add @ outer @ y), wrapper_h:((forall b. (int32 → b → b))) = Λb.λouter:int32.λz.(wrapper_g @ outer @ z) in (wrapper @ 10:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negsibling-polymorphic-bindings-inside-lambda-h-passes-its-own-args-to-g

  (is (= let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 1:int32 @ 2:int32), wrapper_g:((forall a. (int32 → a → a → a))) = Λa.λouter:int32.λv.λt.(hydra.lib.math.add @ outer @ (hydra.lib.math.add @ v @ t)), wrapper_h:((forall b. (int32 → b → b → b))) = Λb.λouter:int32.λv.λt.(wrapper_g @ outer @ v @ t) in (wrapper @ 10:int32)

         let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 1:int32 @ 2:int32), wrapper_g:((forall a. (int32 → a → a → a))) = Λa.λouter:int32.λv.λt.(hydra.lib.math.add @ outer @ (hydra.lib.math.add @ v @ t)), wrapper_h:((forall b. (int32 → b → b → b))) = Λb.λouter:int32.λv.λt.(wrapper_g @ outer @ v @ t) in (wrapper @ 10:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-neguntyped-binding-not-hoisted

  (is (= let x = 1:int32 in let y = 2:int32 in (hydra.lib.math.add @ x @ y)

         let x = 1:int32 in let y = 2:int32 in (hydra.lib.math.add @ x @ y))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negno-name-collision-distinct-names-after-unshadowing

  (is (= let id:(((int32 → int32))) = λx.x, f:(((int32 → int32))) = λa.(f_id2 @ (id @ a)), f_id2:((forall b. (b → b))) = Λb.λy.y in (f @ 42:int32)

         let id:(((int32 → int32))) = λx.x, f:(((int32 → int32))) = λa.(f_id2 @ (id @ a)), f_id2:((forall b. (b → b))) = Λb.λy.y in (f @ 42:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negnested-polymorphic-binding-calls-enclosing-polymorphic-binding

  (is (= let wrapper:(((int32 → int32 → int32))) = λouter:int32.λinner:int32.(wrapper_h @ 42:int32), wrapper_g:((forall a. (a → a))) = Λa.λy.y, wrapper_h:((forall b. (b → b))) = Λb.λz.(wrapper_g @ z) in (wrapper @ 10:int32 @ 20:int32)

         let wrapper:(((int32 → int32 → int32))) = λouter:int32.λinner:int32.(wrapper_h @ 42:int32), wrapper_g:((forall a. (a → a))) = Λa.λy.y, wrapper_h:((forall b. (b → b))) = Λb.λz.(wrapper_g @ z) in (wrapper @ 10:int32 @ 20:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-captures-monomorphic-sibling-in-same-let

  (is (= let wrapper:(((int32 → int32 → int32))) = λleft:int32.λright:int32.let sleft:((int32)) = (f @ left), sright:((int32)) = (f @ right) in (wrapper_cannotUnify @ sleft @ sright @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → int32 → a → a))) = Λa.λsleft:int32.λsright:int32.λx.(hydra.lib.math.add @ sleft @ (hydra.lib.math.add @ sright @ x)) in (wrapper @ 1:int32 @ 2:int32)

         let wrapper:(((int32 → int32 → int32))) = λleft:int32.λright:int32.let sleft:((int32)) = (f @ left), sright:((int32)) = (f @ right) in (wrapper_cannotUnify @ sleft @ sright @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → int32 → a → a))) = Λa.λsleft:int32.λsright:int32.λx.(hydra.lib.math.add @ sleft @ (hydra.lib.math.add @ sright @ x)) in (wrapper @ 1:int32 @ 2:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negnested-lets-poly-binding-references-poly-sibling-from-outer-let

  (is (= let wrapper:(((int32 → int32))) = λleft:int32.let sleft:((int32)) = left in (wrapper_joinList @ sleft @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → a → a))) = Λa.λsleft:int32.λx.(hydra.lib.math.add @ sleft @ x), wrapper_joinList:((forall b. (int32 → b → b))) = Λb.λsleft:int32.λy.(wrapper_cannotUnify @ sleft @ y) in (wrapper @ 1:int32)

         let wrapper:(((int32 → int32))) = λleft:int32.let sleft:((int32)) = left in (wrapper_joinList @ sleft @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → a → a))) = Λa.λsleft:int32.λx.(hydra.lib.math.add @ sleft @ x), wrapper_joinList:((forall b. (int32 → b → b))) = Λb.λsleft:int32.λy.(wrapper_cannotUnify @ sleft @ y) in (wrapper @ 1:int32))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negpolymorphic-binding-with-pair-type-applications-preserved

  (is (= let f:(((wrap(string) → (list<t0>, set<wrap(string)>)))) = λb:wrap(string).(f_init @ b), f_init:((forall t0. (wrap(string) → (list<t0>, set<wrap(string)>)))) = Λt0.λb:wrap(string).([]⟨t0⟩, (singleton @ b))⟨list<t0>⟩⟨set<wrap(string)>⟩ in (f @ name_x)

         let f:(((wrap(string) → (list<t0>, set<wrap(string)>)))) = λb:wrap(string).(f_init @ b), f_init:((forall t0. (wrap(string) → (list<t0>, set<wrap(string)>)))) = Λt0.λb:wrap(string).([]⟨t0⟩, (singleton @ b))⟨list<t0>⟩⟨set<wrap(string)>⟩ in (f @ name_x))))

(deftest test-all-neghoistlet-neghoistpolymorphicletbindings-negmonomorphic-binding-captures-type-vars-replacement-includes-type-applications

  (is (= let f:((forall a,b. (a → b))) = Λa.Λb.λx:a.(f_q⟨a⟩⟨b⟩ @ x), f_q:((forall a,b. (a → b))) = Λa.Λb.λy:a.(g @ y) in f

         let f:((forall a,b. (a → b))) = Λa.Λb.λx:a.(f_q⟨a⟩⟨b⟩ @ x), f_q:((forall a,b. (a → b))) = Λa.Λb.λy:a.(g @ y) in f)))

;; hoistPolymorphicTypeParameters

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negnested-function-types-all-type-variables-must-be-declared

  (is (= let f:((((string → int32) → (boolean → int32) → string → int32))) = f_choose, f_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in f

         let f:((((string → int32) → (boolean → int32) → string → int32))) = f_choose, f_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in f)))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negtype-variable-in-return-position-only

  (is (= let f:(((unit → int32))) = f_returnT, f_returnT:((forall t. (unit → t))) = Λt.λunit.undefined in f

         let f:(((unit → int32))) = f_returnT, f_returnT:((forall t. (unit → t))) = Λt.λunit.undefined in f)))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negtype-variables-in-deeply-nested-generics

  (is (= let f:(((((string, int32), boolean) → string))) = f_nested, f_nested:((forall t0,t1,t2. (((t0, t1), t2) → t0))) = Λt0.Λt1.Λt2.λx.undefined in f

         let f:(((((string, int32), boolean) → string))) = f_nested, f_nested:((forall t0,t1,t2. (((t0, t1), t2) → t0))) = Λt0.Λt1.Λt2.λx.undefined in f)))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negmultiple-bindings-with-overlapping-type-variable-names

  (is (= let outer:((((int32 → int32), (string → string)))) = (pair @ outer_id1 @ outer_id2), outer_id1:((forall t. (t → t))) = Λt.λx.x, outer_id2:((forall t. (t → t))) = Λt.λy.y in outer

         let outer:((((int32 → int32), (string → string)))) = (pair @ outer_id1 @ outer_id2), outer_id1:((forall t. (t → t))) = Λt.λx.x, outer_id2:((forall t. (t → t))) = Λt.λy.y in outer)))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negcaptured-variable-with-type-parameters

  (is (= let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall t. (string → t → (string, t)))) = Λt.λa:string.λx.(pair @ a @ x) in (f @ "hello")

         let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall t. (string → t → (string, t)))) = Λt.λa:string.λx.(pair @ a @ x) in (f @ "hello"))))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negshort-type-variable-names-are-treated-as-type-parameters

  (is (= let f:(((int32 → string → boolean))) = f_g, f_g:((forall s,t,v. (s → t → v))) = Λs.Λt.Λv.λs.λt.undefined in f

         let f:(((int32 → string → boolean))) = f_g, f_g:((forall s,t,v. (s → t → v))) = Λs.Λt.Λv.λs.λt.undefined in f)))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negnumbered-type-variables-like-t0-t1-t2

  (is (= let f:(((int32 → string → boolean))) = f_g, f_g:((forall t0,t1,t2. (t0 → t1 → t2))) = Λt0.Λt1.Λt2.λx.λy.undefined in f

         let f:(((int32 → string → boolean))) = f_g, f_g:((forall t0,t1,t2. (t0 → t1 → t2))) = Λt0.Λt1.Λt2.λx.λy.undefined in f)))

(deftest test-all-neghoistlet-neghoistpolymorphictypeparameters-negchoose-pattern-from-mutatetrace

  (is (= let mutateTrace:(((int32 → int32 → int32 → int32))) = λmutate.λrestore.λf.(mutateTrace_choose @ forLeft @ forRight @ e), mutateTrace_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in mutateTrace

         let mutateTrace:(((int32 → int32 → int32 → int32))) = λmutate.λrestore.λf.(mutateTrace_choose @ forLeft @ forRight @ e), mutateTrace_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in mutateTrace)))

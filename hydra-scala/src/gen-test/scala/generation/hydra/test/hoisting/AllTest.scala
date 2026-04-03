// Note: this is an automatically generated file. Do not edit.
// hoisting

package generation.hydra.test.hoisting

import org.scalatest.funsuite.AnyFunSuite

class AllTest extends AnyFunSuite {

  // hoistCases

  // hoistSubterms

  test("hoistCases - hoistSubterms - hoistNothing: simple let unchanged") {

    assert((

      let x = 42:int32 in x) == (

      let x = 42:int32 in x))

  }

  test("hoistCases - hoistSubterms - hoistNothing: let with list in body unchanged") {

    assert((

      let x = 1:int32 in [x, 2:int32, 3:int32]) == (

      let x = 1:int32 in [x, 2:int32, 3:int32]))

  }

  test("hoistCases - hoistSubterms - hoistNothing: let with application in body unchanged") {

    assert((

      let f = g in (f @ (h @ 42:int32))) == (

      let f = g in (f @ (h @ 42:int32))))

  }

  test("hoistCases - hoistSubterms - hoistLists: list in body is hoisted into local let") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32, 3:int32] in (f @ _hoist_x_body_1)) == (

      let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32, 3:int32] in (f @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistLists: multiple lists in body are hoisted together") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [3:int32, 4:int32] in (pair @ _hoist_x_body_1 @ _hoist_x_body_2)) == (

      let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [3:int32, 4:int32] in (pair @ _hoist_x_body_1 @ _hoist_x_body_2)))

  }

  test("hoistCases - hoistSubterms - hoistLists: list in binding value is hoisted into local let") {

    assert((

      let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in x) == (

      let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in x))

  }

  test("hoistCases - hoistSubterms - hoistLists: nested lists hoisted from inside out") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [_hoist_x_body_1, 3:int32] in (f @ _hoist_x_body_2)) == (

      let x = 1:int32 in let _hoist_x_body_1 = [1:int32, 2:int32], _hoist_x_body_2 = [_hoist_x_body_1, 3:int32] in (f @ _hoist_x_body_2)))

  }

  test("hoistCases - hoistSubterms - hoistApplications: application in list element is hoisted") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in [_hoist_x_body_1, y]) == (

      let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in [_hoist_x_body_1, y]))

  }

  test("hoistCases - hoistSubterms - hoistApplications: application in record field is hoisted") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in record(Data){value=_hoist_x_body_1}) == (

      let x = 1:int32 in let _hoist_x_body_1 = (f @ x) in record(Data){value=_hoist_x_body_1}))

  }

  test("hoistCases - hoistSubterms - hoistApplications: nested applications hoisted from inside out") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = (g @ x), _hoist_x_body_2 = (f @ _hoist_x_body_1) in [_hoist_x_body_2]) == (

      let x = 1:int32 in let _hoist_x_body_1 = (g @ x), _hoist_x_body_2 = (f @ _hoist_x_body_1) in [_hoist_x_body_2]))

  }

  test("hoistCases - hoistSubterms - hoistCaseStatements: case in application argument is hoisted") {

    assert((

      let x = just(42:int32) in let _hoist_x_body_1 = case(Optional){just=λy.y, nothing=0:int32, [default]=x} in (f @ _hoist_x_body_1)) == (

      let x = just(42:int32) in let _hoist_x_body_1 = case(Optional){just=λy.y, nothing=0:int32, [default]=x} in (f @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistCaseStatements: case in list element is hoisted") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = case(Result){ok=x, err=0:int32, [default]=y} in [_hoist_x_body_1]) == (

      let x = 1:int32 in let _hoist_x_body_1 = case(Result){ok=x, err=0:int32, [default]=y} in [_hoist_x_body_1]))

  }

  test("hoistCases - hoistSubterms - hoistLists: nested let - inner let processed independently") {

    assert((

      let x = 1:int32 in let y = 2:int32 in let _hoist_y_body_1 = [x, y] in (f @ _hoist_y_body_1)) == (

      let x = 1:int32 in let y = 2:int32 in let _hoist_y_body_1 = [x, y] in (f @ _hoist_y_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistLists: non-let term is unchanged") {

    assert((

      (f @ [1:int32, 2:int32, 3:int32])) == (

      (f @ [1:int32, 2:int32, 3:int32])))

  }

  test("hoistCases - hoistSubterms - hoistApplications: bare application unchanged") {

    assert((

      (f @ (g @ x))) == (

      (f @ (g @ x))))

  }

  test("hoistCases - hoistSubterms - hoistLists: term referring to let-bound variable needs no capture") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in (f @ _hoist_x_body_1)) == (

      let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in (f @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistLists: term referring to lambda above let needs no capture") {

    assert((

      λy.let x = 1:int32 in let _hoist_x_body_1 = [y, x] in (f @ _hoist_x_body_1)) == (

      λy.let x = 1:int32 in let _hoist_x_body_1 = [y, x] in (f @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistLists: lambda-bound var not free in hoisted term needs no capture") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in λy.(f @ _hoist_x_body_1)) == (

      let x = 1:int32 in let _hoist_x_body_1 = [x, 2:int32] in λy.(f @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistLists: lambda-bound var free in hoisted term requires capture") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = λy.[x, y] in λy.(f @ (_hoist_x_body_1 @ y))) == (

      let x = 1:int32 in let _hoist_x_body_1 = λy.[x, y] in λy.(f @ (_hoist_x_body_1 @ y))))

  }

  test("hoistCases - hoistSubterms - hoistLists: only free lambda-bound vars are captured") {

    assert((

      let x = 1:int32 in let _hoist_x_body_1 = λb.[x, b] in λa.λb.(f @ (_hoist_x_body_1 @ b))) == (

      let x = 1:int32 in let _hoist_x_body_1 = λb.[x, b] in λa.λb.(f @ (_hoist_x_body_1 @ b))))

  }

  test("hoistCases - hoistSubterms - hoistLists: stable naming for binding and body") {

    assert((

      let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in let _hoist_x_body_1 = [3:int32, 4:int32] in (g @ _hoist_x_body_1)) == (

      let x = let _hoist_x_1 = [1:int32, 2:int32] in (f @ _hoist_x_1) in let _hoist_x_body_1 = [3:int32, 4:int32] in (g @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistSubterms - hoistLists: stable naming for multiple bindings") {

    assert((

      let x = let _hoist_x_1 = [1:int32] in (f @ _hoist_x_1), y = let _hoist_y_1 = [2:int32] in (g @ _hoist_y_1) in x) == (

      let x = let _hoist_x_1 = [1:int32] in (f @ _hoist_x_1), y = let _hoist_y_1 = [2:int32] in (g @ _hoist_y_1) in x))

  }

  test("hoistCases - hoistSubterms - hoistLists: polymorphic binding with self-reference below hoisted term") {

    assert((

      let f = let _hoist_f_1 = λx.[x, 1:int32] in λx.(pair @ (f @ x) @ (_hoist_f_1 @ x)) in (f @ 42:int32)) == (

      let f = let _hoist_f_1 = λx.[x, 1:int32] in λx.(pair @ (f @ x) @ (_hoist_f_1 @ x)) in (f @ 42:int32)))

  }

  // hoistCaseStatements

  test("hoistCases - hoistCaseStatements - case at top level of let body is NOT hoisted") {

    assert((

      let x = just(42:int32) in case(Optional){just=λy.y, nothing=0:int32, [default]=x}) == (

      let x = just(42:int32) in case(Optional){just=λy.y, nothing=0:int32, [default]=x}))

  }

  test("hoistCases - hoistCaseStatements - case in let binding value is NOT hoisted") {

    assert((

      let x = case(Optional){just=λz.z, nothing=0:int32, [default]=y} in x) == (

      let x = case(Optional){just=λz.z, nothing=0:int32, [default]=y} in x))

  }

  test("hoistCases - hoistCaseStatements - case inside lambda body is NOT hoisted") {

    assert((

      let f = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (f @ just(42:int32))) == (

      let f = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (f @ just(42:int32))))

  }

  test("hoistCases - hoistCaseStatements - case inside nested lambdas is NOT hoisted") {

    assert((

      let f = λa.λb.case(Result){ok=b, err=0:int32, [default]=a} in f) == (

      let f = λa.λb.case(Result){ok=b, err=0:int32, [default]=a} in f))

  }

  test("hoistCases - hoistCaseStatements - case as LHS of one application is NOT hoisted") {

    assert((

      let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f) == (

      let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case wrapped in annotation is NOT hoisted") {

    assert((

      let f = case(Optional){just=λy.y, nothing=0:int32} in f) == (

      let f = case(Optional){just=λy.y, nothing=0:int32} in f))

  }

  test("hoistCases - hoistCaseStatements - case in lambda with one application is NOT hoisted") {

    assert((

      let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f) == (

      let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f))

  }

  test("hoistCases - hoistCaseStatements - case as RHS of application IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_f_1) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_f_1) in f))

  }

  test("hoistCases - hoistCaseStatements - case in nested application LHS IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λz.λw.z, nothing=λw.0:int32} in (_hoist_f_1 @ x @ y) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λz.λw.z, nothing=λw.0:int32} in (_hoist_f_1 @ x @ y) in f))

  }

  test("hoistCases - hoistCaseStatements - case inside list element IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [_hoist_f_1] in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [_hoist_f_1] in f))

  }

  test("hoistCases - hoistCaseStatements - case inside lambda inside list IS hoisted") {

    assert((

      let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in [λa.(_hoist_f_1 @ a)] in f) == (

      let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in [λa.(_hoist_f_1 @ a)] in f))

  }

  test("hoistCases - hoistCaseStatements - list inside lambda is NOT hoisted (only case statements)") {

    assert((

      let f = λa.[a, 1:int32, 2:int32] in (f @ 0:int32)) == (

      let f = λa.[a, 1:int32, 2:int32] in (f @ 0:int32)))

  }

  test("hoistCases - hoistCaseStatements - case in binding is not hoisted, case in arg position is hoisted") {

    assert((

      let x = case(Optional){just=λz.z, nothing=0:int32, [default]=a} in let _hoist_x_body_1 = case(Optional){just=λw.w, nothing=0:int32, [default]=b} in (f @ _hoist_x_body_1)) == (

      let x = case(Optional){just=λz.z, nothing=0:int32, [default]=a} in let _hoist_x_body_1 = case(Optional){just=λw.w, nothing=0:int32, [default]=b} in (f @ _hoist_x_body_1)))

  }

  test("hoistCases - hoistCaseStatements - case in nested let body is NOT hoisted") {

    assert((

      let x = 1:int32 in let y = 2:int32 in case(Optional){just=λw.w, nothing=0:int32, [default]=z}) == (

      let x = 1:int32 in let y = 2:int32 in case(Optional){just=λw.w, nothing=0:int32, [default]=z}))

  }

  test("hoistCases - hoistCaseStatements - case in let inside lambda is NOT hoisted") {

    assert((

      let f = λa.let x = 1:int32 in case(Optional){just=λy.y, nothing=0:int32, [default]=a} in f) == (

      let f = λa.let x = 1:int32 in case(Optional){just=λy.y, nothing=0:int32, [default]=a} in f))

  }

  test("hoistCases - hoistCaseStatements - case in lambda inside let body is NOT hoisted") {

    assert((

      let x = 1:int32 in λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a}) == (

      let x = 1:int32 in λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a}))

  }

  test("hoistCases - hoistCaseStatements - case with let+lambda+app is NOT hoisted") {

    assert((

      let f = λa.let x = 1:int32 in (case(Optional){just=λy.y, nothing=0:int32} @ x) in f) == (

      let f = λa.let x = 1:int32 in (case(Optional){just=λy.y, nothing=0:int32} @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case in triple application LHS IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λa.λb.λc.a, nothing=λb.λc.0:int32} in (_hoist_f_1 @ x @ y @ z) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λa.λb.λc.a, nothing=λb.λc.0:int32} in (_hoist_f_1 @ x @ y @ z) in f))

  }

  test("hoistCases - hoistCaseStatements - case as second argument IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ x @ _hoist_f_1) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ x @ _hoist_f_1) in f))

  }

  test("hoistCases - hoistCaseStatements - case in both arguments - both hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_f_1 @ _hoist_f_2) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_f_1 @ _hoist_f_2) in f))

  }

  test("hoistCases - hoistCaseStatements - case in second list element IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [1:int32, _hoist_f_1] in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in [1:int32, _hoist_f_1] in f))

  }

  test("hoistCases - hoistCaseStatements - multiple cases in list - all hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in [_hoist_f_1, _hoist_f_2] in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a}, _hoist_f_2 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in [_hoist_f_1, _hoist_f_2] in f))

  }

  test("hoistCases - hoistCaseStatements - case in pair first element IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (_hoist_f_1, 1:int32) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (_hoist_f_1, 1:int32) in f))

  }

  test("hoistCases - hoistCaseStatements - case in pair second element IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (1:int32, _hoist_f_1) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (1:int32, _hoist_f_1) in f))

  }

  test("hoistCases - hoistCaseStatements - case in child let binding hoisted into child") {

    assert((

      let outer = let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_1) in inner in outer) == (

      let outer = let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_1) in inner in outer))

  }

  test("hoistCases - hoistCaseStatements - case in child let body hoisted into child") {

    assert((

      let outer = let inner = 1:int32 in let _hoist_inner_body_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_body_1) in outer) == (

      let outer = let inner = 1:int32 in let _hoist_inner_body_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ _hoist_inner_body_1) in outer))

  }

  test("hoistCases - hoistCaseStatements - case at top level of child let NOT hoisted") {

    assert((

      let outer = let inner = case(Optional){just=λy.y, nothing=0:int32} in inner in outer) == (

      let outer = let inner = case(Optional){just=λy.y, nothing=0:int32} in inner in outer))

  }

  test("hoistCases - hoistCaseStatements - cases in both outer and child - each hoisted locally") {

    assert((

      let outer = let _hoist_outer_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a} in (f @ _hoist_outer_1 @ let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_inner_1) in inner) in outer) == (

      let outer = let _hoist_outer_1 = case(Optional){just=λx.x, nothing=0:int32, [default]=a} in (f @ _hoist_outer_1 @ let inner = let _hoist_inner_1 = case(Optional){just=λy.y, nothing=1:int32, [default]=b} in (g @ _hoist_inner_1) in inner) in outer))

  }

  test("hoistCases - hoistCaseStatements - lambda after app LHS takes us out of top level") {

    assert((

      let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (λa.(_hoist_f_1 @ a) @ x) in f) == (

      let f = let _hoist_f_1 = λa.case(Optional){just=λy.y, nothing=0:int32, [default]=a} in (λa.(_hoist_f_1 @ a) @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case inside case branch is NOT hoisted") {

    assert((

      let f = case(Optional){just=λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a}, nothing=0:int32, [default]=x} in f) == (

      let f = case(Optional){just=λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a}, nothing=0:int32, [default]=x} in f))

  }

  test("hoistCases - hoistCaseStatements - case inside case default branch is NOT hoisted") {

    assert((

      let f = case(Optional){just=λa.a, nothing=case(Optional){just=λb.b, nothing=0:int32, [default]=y}, [default]=x} in f) == (

      let f = case(Optional){just=λa.a, nothing=case(Optional){just=λb.b, nothing=0:int32, [default]=y}, [default]=x} in f))

  }

  test("hoistCases - hoistCaseStatements - case in arg position inside case branch IS hoisted") {

    assert((

      let f = let _hoist_f_1 = λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a} in case(Optional){just=λa.(g @ (_hoist_f_1 @ a)), nothing=0:int32, [default]=x} in f) == (

      let f = let _hoist_f_1 = λa.case(Optional){just=λb.b, nothing=0:int32, [default]=a} in case(Optional){just=λa.(g @ (_hoist_f_1 @ a)), nothing=0:int32, [default]=x} in f))

  }

  test("hoistCases - hoistCaseStatements - case in let body inside applied case default IS hoisted") {

    assert((

      let f = (case(Optional){just=λa.a, nothing=let b = (g @ x) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b)} @ x) in f) == (

      let f = (case(Optional){just=λa.a, nothing=let b = (g @ x) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b)} @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case in let body inside applied case branch IS hoisted") {

    assert((

      let f = (case(Optional){just=λa.let b = (h @ a) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b), nothing=0:int32} @ x) in f) == (

      let f = (case(Optional){just=λa.let b = (h @ a) in let _hoist_b_body_1 = case(Result){ok=λy.y, err=0:int32} in (_hoist_b_body_1 @ b), nothing=0:int32} @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case application at top level of binding is NOT hoisted") {

    assert((

      let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f) == (

      let f = (case(Optional){just=λy.y, nothing=0:int32} @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case application in arg position IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ (_hoist_f_1 @ x)) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (g @ (_hoist_f_1 @ x)) in f))

  }

  test("hoistCases - hoistCaseStatements - case application inside immediately-applied lambda IS hoisted") {

    assert((

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (λa.(_hoist_f_1 @ a) @ x) in f) == (

      let f = let _hoist_f_1 = case(Optional){just=λy.y, nothing=0:int32} in (λa.(_hoist_f_1 @ a) @ x) in f))

  }

  test("hoistCases - hoistCaseStatements - case application in lambda body is NOT hoisted") {

    assert((

      let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f) == (

      let f = λa.(case(Optional){just=λy.y, nothing=0:int32} @ a) in f))

  }

  // hoistLet

  // hoistLetBindings

  test("hoistLet - hoistLetBindings - nested let inside lambda: binding hoisted with lambda capture") {

    assert((

      let f = λa.(hydra.lib.math.mul! @ (f_g @ a) @ 2:int32), f_g = λa.(hydra.lib.math.add! @ a @ 1:int32) in (f @ 10:int32)) == (

      let f = λa.(hydra.lib.math.mul! @ (f_g @ a) @ 2:int32), f_g = λa.(hydra.lib.math.add! @ a @ 1:int32) in (f @ 10:int32)))

  }

  test("hoistLet - hoistLetBindings - type application: nested let outside lambda CAN be hoisted") {

    assert((

      let f = λx.(hydra.lib.math.add! @ x @ f_y)⟨int32⟩, f_y = 1:int32 in (f @ 10:int32)) == (

      let f = λx.(hydra.lib.math.add! @ x @ f_y)⟨int32⟩, f_y = 1:int32 in (f @ 10:int32)))

  }

  // hoistPolymorphicLetBindings

  test("hoistLet - hoistPolymorphicLetBindings - no polymorphic bindings: simple let unchanged") {

    assert((

      let x:((int32)) = 42:int32 in x) == (

      let x:((int32)) = 42:int32 in x))

  }

  test("hoistLet - hoistPolymorphicLetBindings - no polymorphic bindings: multiple monomorphic bindings") {

    assert((

      let x:((int32)) = 1:int32, y:((string)) = "hi" in (pair @ x @ y)) == (

      let x:((int32)) = 1:int32, y:((string)) = "hi" in (pair @ x @ y)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - single polymorphic binding: already at top level") {

    assert((

      let id:((forall a. (a → a))) = λx.x in (id @ 42:int32)) == (

      let id:((forall a. (a → a))) = λx.x in (id @ 42:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding inside lambda: no capture") {

    assert((

      let f:(((int32 → int32))) = λa.(f_id @ a), f_id:((forall b. (b → b))) = Λb.λx.x in (f @ 42:int32)) == (

      let f:(((int32 → int32))) = λa.(f_id @ a), f_id:((forall b. (b → b))) = Λb.λx.x in (f @ 42:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding captures lambda variable: wrapped in lambda") {

    assert((

      let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall b. (string → b → (string, b)))) = Λb.λa:string.λx.(pair @ a @ x) in (f @ "hello")) == (

      let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall b. (string → b → (string, b)))) = Λb.λa:string.λx.(pair @ a @ x) in (f @ "hello")))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding captures multiple lambda variables") {

    assert((

      let f:(((int32 → int32 → int32))) = λa:int32.λb:int32.(f_g @ a @ b @ 42:int32), f_g:((forall c. (int32 → int32 → c → c))) = Λc.λa:int32.λb:int32.λx.(triple @ a @ b @ x) in (f @ 1:int32 @ 2:int32)) == (

      let f:(((int32 → int32 → int32))) = λa:int32.λb:int32.(f_g @ a @ b @ 42:int32), f_g:((forall c. (int32 → int32 → c → c))) = Λc.λa:int32.λb:int32.λx.(triple @ a @ b @ x) in (f @ 1:int32 @ 2:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding captures some but not all lambda variables") {

    assert((

      let f:(((int32 → int32 → (int32, int32)))) = λa:int32.λb:int32.(f_g @ a @ b), f_g:((forall c. (int32 → c → (int32, c)))) = Λc.λa:int32.λx.(pair @ a @ x) in (f @ 1:int32 @ 2:int32)) == (

      let f:(((int32 → int32 → (int32, int32)))) = λa:int32.λb:int32.(f_g @ a @ b), f_g:((forall c. (int32 → c → (int32, c)))) = Λc.λa:int32.λx.(pair @ a @ x) in (f @ 1:int32 @ 2:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding captures both lambda-bound and let-bound variables") {

    assert((

      let f:(((int32 → int32))) = λa:int32.let x:((int32)) = 1:int32 in (f_g @ a @ x @ 42:int32), f_g:((forall b. (int32 → int32 → b → b))) = Λb.λa:int32.λx:int32.λy.(hydra.lib.math.add! @ (hydra.lib.math.add! @ a @ x) @ y) in (f @ 10:int32)) == (

      let f:(((int32 → int32))) = λa:int32.let x:((int32)) = 1:int32 in (f_g @ a @ x @ 42:int32), f_g:((forall b. (int32 → int32 → b → b))) = Λb.λa:int32.λx:int32.λy.(hydra.lib.math.add! @ (hydra.lib.math.add! @ a @ x) @ y) in (f @ 10:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - sibling polymorphic bindings inside lambda: one calls the other") {

    assert((

      let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 42:int32), wrapper_g:((forall a. (int32 → a → a))) = Λa.λouter:int32.λy.(hydra.lib.math.add! @ outer @ y), wrapper_h:((forall b. (int32 → b → b))) = Λb.λouter:int32.λz.(wrapper_g @ outer @ z) in (wrapper @ 10:int32)) == (

      let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 42:int32), wrapper_g:((forall a. (int32 → a → a))) = Λa.λouter:int32.λy.(hydra.lib.math.add! @ outer @ y), wrapper_h:((forall b. (int32 → b → b))) = Λb.λouter:int32.λz.(wrapper_g @ outer @ z) in (wrapper @ 10:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - sibling polymorphic bindings inside lambda: h passes its own args to g") {

    assert((

      let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 1:int32 @ 2:int32), wrapper_g:((forall a. (int32 → a → a → a))) = Λa.λouter:int32.λv.λt.(hydra.lib.math.add! @ outer @ (hydra.lib.math.add! @ v @ t)), wrapper_h:((forall b. (int32 → b → b → b))) = Λb.λouter:int32.λv.λt.(wrapper_g @ outer @ v @ t) in (wrapper @ 10:int32)) == (

      let wrapper:(((int32 → int32))) = λouter:int32.(wrapper_h @ outer @ 1:int32 @ 2:int32), wrapper_g:((forall a. (int32 → a → a → a))) = Λa.λouter:int32.λv.λt.(hydra.lib.math.add! @ outer @ (hydra.lib.math.add! @ v @ t)), wrapper_h:((forall b. (int32 → b → b → b))) = Λb.λouter:int32.λv.λt.(wrapper_g @ outer @ v @ t) in (wrapper @ 10:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - untyped binding: not hoisted") {

    assert((

      let x = 1:int32 in let y = 2:int32 in (hydra.lib.math.add! @ x @ y)) == (

      let x = 1:int32 in let y = 2:int32 in (hydra.lib.math.add! @ x @ y)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - no name collision: distinct names after unshadowing") {

    assert((

      let id:(((int32 → int32))) = λx.x, f:(((int32 → int32))) = λa.(f_id2 @ (id @ a)), f_id2:((forall b. (b → b))) = Λb.λy.y in (f @ 42:int32)) == (

      let id:(((int32 → int32))) = λx.x, f:(((int32 → int32))) = λa.(f_id2 @ (id @ a)), f_id2:((forall b. (b → b))) = Λb.λy.y in (f @ 42:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - nested polymorphic binding calls enclosing polymorphic binding") {

    assert((

      let wrapper:(((int32 → int32 → int32))) = λouter:int32.λinner:int32.(wrapper_h @ 42:int32), wrapper_g:((forall a. (a → a))) = Λa.λy.y, wrapper_h:((forall b. (b → b))) = Λb.λz.(wrapper_g @ z) in (wrapper @ 10:int32 @ 20:int32)) == (

      let wrapper:(((int32 → int32 → int32))) = λouter:int32.λinner:int32.(wrapper_h @ 42:int32), wrapper_g:((forall a. (a → a))) = Λa.λy.y, wrapper_h:((forall b. (b → b))) = Λb.λz.(wrapper_g @ z) in (wrapper @ 10:int32 @ 20:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding captures monomorphic sibling in same let") {

    assert((

      let wrapper:(((int32 → int32 → int32))) = λleft:int32.λright:int32.let sleft:((int32)) = (f @ left), sright:((int32)) = (f @ right) in (wrapper_cannotUnify @ sleft @ sright @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → int32 → a → a))) = Λa.λsleft:int32.λsright:int32.λx.(hydra.lib.math.add! @ sleft @ (hydra.lib.math.add! @ sright @ x)) in (wrapper @ 1:int32 @ 2:int32)) == (

      let wrapper:(((int32 → int32 → int32))) = λleft:int32.λright:int32.let sleft:((int32)) = (f @ left), sright:((int32)) = (f @ right) in (wrapper_cannotUnify @ sleft @ sright @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → int32 → a → a))) = Λa.λsleft:int32.λsright:int32.λx.(hydra.lib.math.add! @ sleft @ (hydra.lib.math.add! @ sright @ x)) in (wrapper @ 1:int32 @ 2:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - nested lets: poly binding references poly sibling from outer let") {

    assert((

      let wrapper:(((int32 → int32))) = λleft:int32.let sleft:((int32)) = left in (wrapper_joinList @ sleft @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → a → a))) = Λa.λsleft:int32.λx.(hydra.lib.math.add! @ sleft @ x), wrapper_joinList:((forall b. (int32 → b → b))) = Λb.λsleft:int32.λy.(wrapper_cannotUnify @ sleft @ y) in (wrapper @ 1:int32)) == (

      let wrapper:(((int32 → int32))) = λleft:int32.let sleft:((int32)) = left in (wrapper_joinList @ sleft @ 42:int32), wrapper_cannotUnify:((forall a. (int32 → a → a))) = Λa.λsleft:int32.λx.(hydra.lib.math.add! @ sleft @ x), wrapper_joinList:((forall b. (int32 → b → b))) = Λb.λsleft:int32.λy.(wrapper_cannotUnify @ sleft @ y) in (wrapper @ 1:int32)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - polymorphic binding with pair: type applications preserved") {

    assert((

      let f:(((wrap(string) → (list<t0>, set<wrap(string)>)))) = λb:wrap(string).(f_init @ b), f_init:((forall t0. (wrap(string) → (list<t0>, set<wrap(string)>)))) = Λt0.λb:wrap(string).([]⟨t0⟩, (singleton @ b))⟨list<t0>⟩⟨set<wrap(string)>⟩ in (f @ name_x)) == (

      let f:(((wrap(string) → (list<t0>, set<wrap(string)>)))) = λb:wrap(string).(f_init @ b), f_init:((forall t0. (wrap(string) → (list<t0>, set<wrap(string)>)))) = Λt0.λb:wrap(string).([]⟨t0⟩, (singleton @ b))⟨list<t0>⟩⟨set<wrap(string)>⟩ in (f @ name_x)))

  }

  test("hoistLet - hoistPolymorphicLetBindings - monomorphic binding captures type vars: replacement includes type applications") {

    assert((

      let f:((forall a,b. (a → b))) = Λa.Λb.λx:a.(f_q⟨a⟩⟨b⟩ @ x), f_q:((forall a,b. (a → b))) = Λa.Λb.λy:a.(g @ y) in f) == (

      let f:((forall a,b. (a → b))) = Λa.Λb.λx:a.(f_q⟨a⟩⟨b⟩ @ x), f_q:((forall a,b. (a → b))) = Λa.Λb.λy:a.(g @ y) in f))

  }

  // hoistPolymorphicTypeParameters

  test("hoistLet - hoistPolymorphicTypeParameters - nested function types: all type variables must be declared") {

    assert((

      let f:((((string → int32) → (boolean → int32) → string → int32))) = f_choose, f_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in f) == (

      let f:((((string → int32) → (boolean → int32) → string → int32))) = f_choose, f_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in f))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - type variable in return position only") {

    assert((

      let f:(((unit → int32))) = f_returnT, f_returnT:((forall t. (unit → t))) = Λt.λunit.undefined in f) == (

      let f:(((unit → int32))) = f_returnT, f_returnT:((forall t. (unit → t))) = Λt.λunit.undefined in f))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - type variables in deeply nested generics") {

    assert((

      let f:(((((string, int32), boolean) → string))) = f_nested, f_nested:((forall t0,t1,t2. (((t0, t1), t2) → t0))) = Λt0.Λt1.Λt2.λx.undefined in f) == (

      let f:(((((string, int32), boolean) → string))) = f_nested, f_nested:((forall t0,t1,t2. (((t0, t1), t2) → t0))) = Λt0.Λt1.Λt2.λx.undefined in f))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - multiple bindings with overlapping type variable names") {

    assert((

      let outer:((((int32 → int32), (string → string)))) = (pair @ outer_id1 @ outer_id2), outer_id1:((forall t. (t → t))) = Λt.λx.x, outer_id2:((forall t. (t → t))) = Λt.λy.y in outer) == (

      let outer:((((int32 → int32), (string → string)))) = (pair @ outer_id1 @ outer_id2), outer_id1:((forall t. (t → t))) = Λt.λx.x, outer_id2:((forall t. (t → t))) = Λt.λy.y in outer))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - captured variable with type parameters") {

    assert((

      let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall t. (string → t → (string, t)))) = Λt.λa:string.λx.(pair @ a @ x) in (f @ "hello")) == (

      let f:(((string → (string, int32)))) = λa:string.(f_g @ a @ 42:int32), f_g:((forall t. (string → t → (string, t)))) = Λt.λa:string.λx.(pair @ a @ x) in (f @ "hello")))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - short type variable names are treated as type parameters") {

    assert((

      let f:(((int32 → string → boolean))) = f_g, f_g:((forall s,t,v. (s → t → v))) = Λs.Λt.Λv.λs.λt.undefined in f) == (

      let f:(((int32 → string → boolean))) = f_g, f_g:((forall s,t,v. (s → t → v))) = Λs.Λt.Λv.λs.λt.undefined in f))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - numbered type variables like t0 t1 t2") {

    assert((

      let f:(((int32 → string → boolean))) = f_g, f_g:((forall t0,t1,t2. (t0 → t1 → t2))) = Λt0.Λt1.Λt2.λx.λy.undefined in f) == (

      let f:(((int32 → string → boolean))) = f_g, f_g:((forall t0,t1,t2. (t0 → t1 → t2))) = Λt0.Λt1.Λt2.λx.λy.undefined in f))

  }

  test("hoistLet - hoistPolymorphicTypeParameters - choose pattern from mutateTrace") {

    assert((

      let mutateTrace:(((int32 → int32 → int32 → int32))) = λmutate.λrestore.λf.(mutateTrace_choose @ forLeft @ forRight @ e), mutateTrace_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in mutateTrace) == (

      let mutateTrace:(((int32 → int32 → int32 → int32))) = λmutate.λrestore.λf.(mutateTrace_choose @ forLeft @ forRight @ e), mutateTrace_choose:((forall t0,t1,t2. ((t0 → t1) → (t2 → t1) → t0 → t1))) = Λt0.Λt1.Λt2.λforLeft.λforRight.λe.(forLeft @ e) in mutateTrace))

  }
}

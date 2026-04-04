// Note: this is an automatically generated file. Do not edit.
// variables

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class VariablesTest extends AnyFunSuite {

  // freeVariables

  test("freeVariables - string literal has no free variables") {

    assert((

      {}) == (

      {}))

  }

  test("freeVariables - single variable") {

    assert((

      {x}) == (

      {x}))

  }

  test("freeVariables - bound variable is not free") {

    assert((

      {}) == (

      {}))

  }

  test("freeVariables - unbound variable in lambda body") {

    assert((

      {x}) == (

      {x}))

  }

  test("freeVariables - mixed free and bound variables") {

    assert((

      {x}) == (

      {x}))

  }

  test("freeVariables - multiple free variables") {

    assert((

      {x, y}) == (

      {x, y}))

  }

  // normalizeTypeVariables

  test("normalizeTypeVariables - literal without type variables unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("normalizeTypeVariables - simple let without type annotations unchanged") {

    assert((

      let foo = "foo" in 42:int32) == (

      let foo = "foo" in 42:int32))

  }

  test("normalizeTypeVariables - let with monomorphic type scheme unchanged") {

    assert((

      let foo:((string)) = "foo" in 42:int32) == (

      let foo:((string)) = "foo" in 42:int32))

  }

  test("normalizeTypeVariables - let with monomorphic binding referencing string") {

    assert((

      let foo:((string)) = "foo" in 42:int32) == (

      let foo:((string)) = "foo" in 42:int32))

  }

  test("normalizeTypeVariables - polymorphic binding with free type variable unchanged") {

    assert((

      let foo:((a)) = bar in 42:int32) == (

      let foo:((a)) = bar in 42:int32))

  }

  test("normalizeTypeVariables - monomorphic binding with typed lambda unchanged") {

    assert((

      let foo:((string)) = "foo" in λx:(a → int32).42:int32) == (

      let foo:((string)) = "foo" in λx:(a → int32).42:int32))

  }

  test("normalizeTypeVariables - polymorphic binding with typed lambda in body unchanged") {

    assert((

      let foo:((a)) = bar in λx:(a → int32).42:int32) == (

      let foo:((a)) = bar in λx:(a → int32).42:int32))

  }

  test("normalizeTypeVariables - polymorphic identity function normalized") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - polymorphic const function normalized") {

    assert((

      let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo")) == (

      let const:((forall t0,t1. (t0 → t1 → t0))) = λx.λy.x in (const @ 42:int32 @ "foo")))

  }

  test("normalizeTypeVariables - binding rewriting does not affect body with typed lambda") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32) == (

      let id:((forall t0. (t0 → t0))) = λx.x in λx:(a → int32).42:int32))

  }

  test("normalizeTypeVariables - nested polymorphic lets normalized") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32))) == (

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λy.y in (id @ (id2 @ 42:int32))))

  }

  test("normalizeTypeVariables - nested same substitution in bindings and environment") {

    assert((

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = λx.x in let id2:((forall t0. (t0 → t0))) = λx.x in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - parent type variable shadows child variable") {

    assert((

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - no shadowing distinct type variables") {

    assert((

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)) == (

      let id:((forall t0. (t0 → t0))) = let id2:((forall t1. (t1 → t1))) = λx:t1.x in λy:t0.(id2 @ y) in (id @ 42:int32)))

  }

  test("normalizeTypeVariables - locally free type variable in nested binding") {

    assert((

      let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32)) == (

      let fun1:((forall t0,t1. (t0 → t1 → (t0, t1)))) = λx:t0.λy:t1.let fun2:((forall t2. (t2 → (t2, t1)))) = λz:t2.(z, y) in (fun2 @ x) in (fun1 @ "foo" @ 42:int32)))

  }

  // unshadowVariables

  test("unshadowVariables - literal unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("unshadowVariables - variable unchanged") {

    assert((

      x) == (

      x))

  }

  test("unshadowVariables - single lambda unchanged") {

    assert((

      λx.x) == (

      λx.x))

  }

  test("unshadowVariables - distinct lambda parameters unchanged") {

    assert((

      λx.λy.[x, y]) == (

      λx.λy.[x, y]))

  }

  test("unshadowVariables - let with no shadowing unchanged") {

    assert((

      let x = 1:int32 in x) == (

      let x = 1:int32 in x))

  }

  test("unshadowVariables - let and lambda with distinct names unchanged") {

    assert((

      let x = 1:int32 in λy.[x, y]) == (

      let x = 1:int32 in λy.[x, y]))

  }

  test("unshadowVariables - inner lambda shadows outer lambda") {

    assert((

      λx.λx2.x2) == (

      λx.λx2.x2))

  }

  test("unshadowVariables - inner lambda shadows outer - body references both") {

    assert((

      λx.[x, λx2.x2]) == (

      λx.[x, λx2.x2]))

  }

  test("unshadowVariables - triple nested lambda same name") {

    assert((

      λx.λx2.λx3.x3) == (

      λx.λx2.λx3.x3))

  }

  test("unshadowVariables - two parameters shadow sequentially") {

    assert((

      λx.λy.λx2.λy2.[x2, y2]) == (

      λx.λy.λx2.λy2.[x2, y2]))

  }

  test("unshadowVariables - lambda shadows let-bound variable") {

    assert((

      let x = 1:int32 in λx2.x2) == (

      let x = 1:int32 in λx2.x2))

  }

  test("unshadowVariables - lambda shadows one of multiple let bindings") {

    assert((

      let x = 1:int32, y = 2:int32 in λx2.[x2, y]) == (

      let x = 1:int32, y = 2:int32 in λx2.[x2, y]))

  }

  test("unshadowVariables - inner let body with lambda shadowing outer let") {

    assert((

      let x = 1:int32 in let y = 2:int32 in λx2.x2) == (

      let x = 1:int32 in let y = 2:int32 in λx2.x2))

  }

  test("unshadowVariables - shadowed lambda in function position of application") {

    assert((

      λf.(λf2.f2 @ f)) == (

      λf.(λf2.f2 @ f)))

  }

  test("unshadowVariables - shadowed lambdas in list elements") {

    assert((

      λx.[λx2.x2, λx2.x2]) == (

      λx.[λx2.x2, λx2.x2]))

  }

  test("unshadowVariables - shadowed lambda in record field") {

    assert((

      λx.record(Pair){fst=λx2.x2, snd=x}) == (

      λx.record(Pair){fst=λx2.x2, snd=x}))

  }

  test("unshadowVariables - shadowed lambda in case branch") {

    assert((

      λx.case(Maybe){nothing=0:int32, just=λx2.x2}) == (

      λx.case(Maybe){nothing=0:int32, just=λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda in pair") {

    assert((

      λx.(λx2.x2, x)) == (

      λx.(λx2.x2, x)))

  }

  test("unshadowVariables - shadowed lambda inside optional") {

    assert((

      λx.just(λx2.x2)) == (

      λx.just(λx2.x2)))

  }

  test("unshadowVariables - shadowed lambda inside set element") {

    assert((

      λx.{λx2.x2}) == (

      λx.{λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda in union injection") {

    assert((

      λx.inject(Result){ok=λx2.x2}) == (

      λx.inject(Result){ok=λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda inside wrapped term") {

    assert((

      λx.wrap(Age){λx2.x2}) == (

      λx.wrap(Age){λx2.x2}))

  }

  test("unshadowVariables - shadowed lambda inside type lambda") {

    assert((

      λx.Λa.λx2.x2) == (

      λx.Λa.λx2.x2))

  }

  test("unshadowVariables - shadowed lambda inside type application") {

    assert((

      λx.λx2.x2⟨string⟩) == (

      λx.λx2.x2⟨string⟩))

  }

  test("unshadowVariables - shadowed lambda inside annotated term") {

    assert((

      λx.λx2.x2) == (

      λx.λx2.x2))

  }

  test("unshadowVariables - shadowing at multiple depths") {

    assert((

      λx.λy.λx2.λy2.[x2, y2]) == (

      λx.λy.λx2.λy2.[x2, y2]))

  }

  test("unshadowVariables - let then lambda then lambda all same name") {

    assert((

      let x = 1:int32 in λx2.λx3.x3) == (

      let x = 1:int32 in λx2.λx3.x3))

  }

  test("unshadowVariables - lambda with shadowing in let binding value") {

    assert((

      λx.let y = λx2.x2 in (y @ x)) == (

      λx.let y = λx2.x2 in (y @ x)))

  }

  test("unshadowVariables - application without shadowing unchanged") {

    assert((

      (f @ 42:int32)) == (

      (f @ 42:int32)))

  }

  test("unshadowVariables - list of literals unchanged") {

    assert((

      [1:int32, 2:int32, 3:int32]) == (

      [1:int32, 2:int32, 3:int32]))

  }

  test("unshadowVariables - nested record unchanged") {

    assert((

      record(Point){x=10:int32, y=20:int32}) == (

      record(Point){x=10:int32, y=20:int32}))

  }
}

// Note: this is an automatically generated file. Do not edit.
// dependencies

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class DependenciesTest extends AnyFunSuite {

  // simplifyTerm

  test("simplifyTerm - const application with literal") {

    assert((

      "foo") == (

      "foo"))

  }

  test("simplifyTerm - identity application") {

    assert((

      [y, y]) == (

      [y, y]))

  }

  test("simplifyTerm - unused parameter") {

    assert((

      "foo") == (

      "foo"))

  }

  test("simplifyTerm - nested lambda applications") {

    assert((

      ["foo", y]) == (

      ["foo", y]))

  }

  // flattenLetTerms

  test("flattenLetTerms - non-let term unchanged") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("flattenLetTerms - list term unchanged") {

    assert((

      ["foo"]) == (

      ["foo"]))

  }

  test("flattenLetTerms - sequential lets in body are flattened") {

    assert((

      let x = 1:int32, y = 2:int32 in [x, y]) == (

      let x = 1:int32, y = 2:int32 in [x, y]))

  }

  test("flattenLetTerms - nested binding in let value is flattened") {

    assert((

      let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b]) == (

      let a = 1:int32, b_x = 1:int32, b_y = 2:int32, b = [b_x, b_y] in [a, b]))

  }

  test("flattenLetTerms - multiple levels of nesting are flattened") {

    assert((

      let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b]) == (

      let a = 1:int32, b_x = 1:int32, b_y_p = 137:int32, b_y_q = [b_x, 5:int32], b_y = [a, b_y_q], b = [b_x, b_y] in [a, b]))

  }

  // liftLambdaAboveLet

  test("liftLambdaAboveLet - simple let with lambda in body") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - bare lambda unchanged") {

    assert((

      λx.x) == (

      λx.x))

  }

  test("liftLambdaAboveLet - bare let unchanged") {

    assert((

      let x = 42:int32 in x) == (

      let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - lambda with let in body unchanged") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - let with two nested lambdas") {

    assert((

      λy.λz.let x = 42:int32 in x) == (

      λy.λz.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - lambda inside let body already above let") {

    assert((

      λx.λy.let z = 42:int32 in z) == (

      λx.λy.let z = 42:int32 in z))

  }

  test("liftLambdaAboveLet - let without lambda in body unchanged") {

    assert((

      let x = 42:int32, y = "hello" in (x, y)) == (

      let x = 42:int32, y = "hello" in (x, y)))

  }

  test("liftLambdaAboveLet - multiple let bindings with lambda") {

    assert((

      λz.let x = 42:int32, y = "hello" in x) == (

      λz.let x = 42:int32, y = "hello" in x))

  }

  test("liftLambdaAboveLet - nested lets with lambda at innermost level") {

    assert((

      λz.let x = 42:int32 in let y = "hello" in x) == (

      λz.let x = 42:int32 in let y = "hello" in x))

  }

  test("liftLambdaAboveLet - lambda between two lets") {

    assert((

      λy.let x = 42:int32 in let z = "hello" in x) == (

      λy.let x = 42:int32 in let z = "hello" in x))

  }

  test("liftLambdaAboveLet - multiple lambdas between nested lets") {

    assert((

      λx.λy.let a = 1:int32 in let b = 2:int32 in a) == (

      λx.λy.let a = 1:int32 in let b = 2:int32 in a))

  }

  test("liftLambdaAboveLet - multiple lambdas already above let") {

    assert((

      λx.λy.let z = 42:int32 in z) == (

      λx.λy.let z = 42:int32 in z))

  }

  test("liftLambdaAboveLet - annotation above let containing lambda") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation above lambda in let body") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation between two lambdas") {

    assert((

      λy.λz.let x = 42:int32 in x) == (

      λy.λz.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation on the body of lambda in let") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - annotation on lambda already above let") {

    assert((

      λy.let x = 42:int32 in x) == (

      λy.let x = 42:int32 in x))

  }

  test("liftLambdaAboveLet - let-lambda inside a list") {

    assert((

      [1:int32, λy.let x = 42:int32 in x, 2:int32]) == (

      [1:int32, λy.let x = 42:int32 in x, 2:int32]))

  }

  test("liftLambdaAboveLet - let-lambda in multiple list elements") {

    assert((

      [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z]) == (

      [λy.let x = 1:int32 in x, λw.let z = 2:int32 in z]))

  }

  test("liftLambdaAboveLet - let-lambda in a let binding value") {

    assert((

      let f = λy.let x = 42:int32 in x in f) == (

      let f = λy.let x = 42:int32 in x in f))

  }

  test("liftLambdaAboveLet - let-lambda inside a pair") {

    assert((

      (λy.let x = 42:int32 in x, "test")) == (

      (λy.let x = 42:int32 in x, "test")))

  }

  test("liftLambdaAboveLet - let-lambda in both elements of a pair") {

    assert((

      (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z)) == (

      (λy.let x = 1:int32 in x, λw.let z = 2:int32 in z)))

  }

  test("liftLambdaAboveLet - let-lambda inside lambda body") {

    assert((

      λouter.λinner.let x = 42:int32 in x) == (

      λouter.λinner.let x = 42:int32 in x))

  }

  // topologicalSortBindings

  test("topologicalSortBindings - isolated bindings") {

    assert((

      [[(a, "foo")], [(b, "bar")]]) == (

      [[(a, "foo")], [(b, "bar")]]))

  }

  test("topologicalSortBindings - single recursive binding") {

    assert((

      [[(a, [a])]]) == (

      [[(a, [a])]]))

  }

  test("topologicalSortBindings - mutually recursive bindings") {

    assert((

      [[(a, [b]), (b, [a])]]) == (

      [[(a, [b]), (b, [a])]]))

  }

  test("topologicalSortBindings - mixed bindings") {

    assert((

      [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]]) == (

      [[(c, "foo")], [(a, b), (b, [a, c])], [(d, "bar")]]))

  }
}

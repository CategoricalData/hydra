// Note: this is an automatically generated file. Do not edit.
// serialization

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class SerializationTest extends AnyFunSuite {

  // associativity

  test("associativity - right-associative operator") {

    assert((

      (a -> b) -> c -> d) == (

      (a -> b) -> c -> d))

  }

  // case statements

  test("case statements - simple case statement") {

    assert((

      case x > 42 of
  False -> Big
  True -> Small) == (

      case x > 42 of
  False -> Big
  True -> Small))

  }

  test("case statements - nested case statement") {

    assert((

      case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small) == (

      case x > 42 of
  True -> case x > 100 of
    True -> ReallyBig
    False -> Big
  False -> Small))

  }

  // lambdas

  test("lambdas - simple lambda") {

    assert((

      \x y -> x + y) == (

      \x y -> x + y))

  }

  // lists

  test("lists - empty list") {

    assert((

      []) == (

      []))

  }

  test("lists - simple non-empty list") {

    assert((

      [1, 2, 3]) == (

      [1, 2, 3]))

  }

  test("lists - nested list") {

    assert((

      [[1, 3], 2]) == (

      [[1, 3], 2]))

  }

  test("lists - list with parenthesized expression inside") {

    assert((

      [[1, (2 + 3) * (1 + 10)], 2]) == (

      [[1, (2 + 3) * (1 + 10)], 2]))

  }

  // precedence

  test("precedence - operators with different precedence - no parens needed") {

    assert((

      2 * 3 + 1 * 10) == (

      2 * 3 + 1 * 10))

  }

  test("precedence - operators with different precedence - parens needed") {

    assert((

      (2 + 3) * (1 + 10)) == (

      (2 + 3) * (1 + 10)))

  }

  test("precedence - associative operator left nesting") {

    assert((

      x * y * z) == (

      x * y * z))

  }

  test("precedence - associative operator right nesting") {

    assert((

      x * y * z) == (

      x * y * z))

  }
}

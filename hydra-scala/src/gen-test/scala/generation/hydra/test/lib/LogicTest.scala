// Note: this is an automatically generated file. Do not edit.
// hydra.lib.logic primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class LogicTest extends AnyFunSuite {

  // and

  test("and - true and true") {

    assert((

      hydra.lib.logic.and(true)(true)) == (

      true))

  }

  test("and - true and false") {

    assert((

      hydra.lib.logic.and(true)(false)) == (

      false))

  }

  test("and - false and true") {

    assert((

      hydra.lib.logic.and(false)(true)) == (

      false))

  }

  test("and - false and false") {

    assert((

      hydra.lib.logic.and(false)(false)) == (

      false))

  }

  // ifElse

  // boolean values

  test("ifElse - boolean values - true condition returns then") {

    assert((

      hydra.lib.logic.ifElse[Boolean](true)(true)(false)) == (

      true))

  }

  test("ifElse - boolean values - false condition returns else") {

    assert((

      hydra.lib.logic.ifElse[Boolean](false)(true)(false)) == (

      false))

  }

  // integer values

  test("ifElse - integer values - true selects first int") {

    assert((

      hydra.lib.logic.ifElse[Int](true)(42)(0)) == (

      42))

  }

  test("ifElse - integer values - false selects second int") {

    assert((

      hydra.lib.logic.ifElse[Int](false)(42)(0)) == (

      0))

  }

  // string values

  test("ifElse - string values - true selects first string") {

    assert((

      hydra.lib.logic.ifElse[scala.Predef.String](true)("yes")("no")) == (

      "yes"))

  }

  test("ifElse - string values - false selects second string") {

    assert((

      hydra.lib.logic.ifElse[scala.Predef.String](false)("yes")("no")) == (

      "no"))

  }

  // not

  test("not - not true") {

    assert((

      hydra.lib.logic.not(true)) == (

      false))

  }

  test("not - not false") {

    assert((

      hydra.lib.logic.not(false)) == (

      true))

  }

  // or

  test("or - true or true") {

    assert((

      hydra.lib.logic.or(true)(true)) == (

      true))

  }

  test("or - true or false") {

    assert((

      hydra.lib.logic.or(true)(false)) == (

      true))

  }

  test("or - false or true") {

    assert((

      hydra.lib.logic.or(false)(true)) == (

      true))

  }

  test("or - false or false") {

    assert((

      hydra.lib.logic.or(false)(false)) == (

      false))

  }
}

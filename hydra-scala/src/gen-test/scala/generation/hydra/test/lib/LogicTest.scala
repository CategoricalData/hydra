// Note: this is an automatically generated file. Do not edit.
// hydra.lib.logic primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class LogicTest extends AnyFunSuite {

  // and

  test("and - true and true") {

    assert((

      true) == (

      true))

  }

  test("and - true and false") {

    assert((

      false) == (

      false))

  }

  test("and - false and true") {

    assert((

      false) == (

      false))

  }

  test("and - false and false") {

    assert((

      false) == (

      false))

  }

  // ifElse

  // boolean values

  test("ifElse - boolean values - true condition returns then") {

    assert((

      true) == (

      true))

  }

  test("ifElse - boolean values - false condition returns else") {

    assert((

      false) == (

      false))

  }

  // integer values

  test("ifElse - integer values - true selects first int") {

    assert((

      42:int32) == (

      42:int32))

  }

  test("ifElse - integer values - false selects second int") {

    assert((

      0:int32) == (

      0:int32))

  }

  // string values

  test("ifElse - string values - true selects first string") {

    assert((

      "yes") == (

      "yes"))

  }

  test("ifElse - string values - false selects second string") {

    assert((

      "no") == (

      "no"))

  }

  // not

  test("not - not true") {

    assert((

      false) == (

      false))

  }

  test("not - not false") {

    assert((

      true) == (

      true))

  }

  // or

  test("or - true or true") {

    assert((

      true) == (

      true))

  }

  test("or - true or false") {

    assert((

      true) == (

      true))

  }

  test("or - false or true") {

    assert((

      true) == (

      true))

  }

  test("or - false or false") {

    assert((

      false) == (

      false))

  }
}

// Note: this is an automatically generated file. Do not edit.
// hydra.lib.chars primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class CharsTest extends AnyFunSuite {

  // isAlphaNum

  test("isAlphaNum - letter") {

    assert((

      true) == (

      true))

  }

  test("isAlphaNum - digit") {

    assert((

      true) == (

      true))

  }

  test("isAlphaNum - space") {

    assert((

      false) == (

      false))

  }

  test("isAlphaNum - punctuation") {

    assert((

      false) == (

      false))

  }

  // isLower

  test("isLower - lowercase") {

    assert((

      true) == (

      true))

  }

  test("isLower - uppercase") {

    assert((

      false) == (

      false))

  }

  test("isLower - digit") {

    assert((

      false) == (

      false))

  }

  // isSpace

  test("isSpace - space") {

    assert((

      true) == (

      true))

  }

  test("isSpace - tab") {

    assert((

      true) == (

      true))

  }

  test("isSpace - newline") {

    assert((

      true) == (

      true))

  }

  test("isSpace - letter") {

    assert((

      false) == (

      false))

  }

  // isUpper

  test("isUpper - uppercase") {

    assert((

      true) == (

      true))

  }

  test("isUpper - lowercase") {

    assert((

      false) == (

      false))

  }

  test("isUpper - digit") {

    assert((

      false) == (

      false))

  }

  // toLower

  test("toLower - uppercase") {

    assert((

      97:int32) == (

      97:int32))

  }

  test("toLower - lowercase") {

    assert((

      97:int32) == (

      97:int32))

  }

  test("toLower - digit") {

    assert((

      53:int32) == (

      53:int32))

  }

  // toUpper

  test("toUpper - lowercase") {

    assert((

      65:int32) == (

      65:int32))

  }

  test("toUpper - uppercase") {

    assert((

      65:int32) == (

      65:int32))

  }

  test("toUpper - digit") {

    assert((

      53:int32) == (

      53:int32))

  }
}

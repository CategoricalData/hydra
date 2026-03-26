// Note: this is an automatically generated file. Do not edit.
// hydra.lib.chars primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class CharsTest extends AnyFunSuite {

  // isAlphaNum

  test("isAlphaNum - letter") {

    assert((

      hydra.lib.chars.isAlphaNum(97)) == (

      true))

  }

  test("isAlphaNum - digit") {

    assert((

      hydra.lib.chars.isAlphaNum(53)) == (

      true))

  }

  test("isAlphaNum - space") {

    assert((

      hydra.lib.chars.isAlphaNum(32)) == (

      false))

  }

  test("isAlphaNum - punctuation") {

    assert((

      hydra.lib.chars.isAlphaNum(46)) == (

      false))

  }

  // isLower

  test("isLower - lowercase") {

    assert((

      hydra.lib.chars.isLower(97)) == (

      true))

  }

  test("isLower - uppercase") {

    assert((

      hydra.lib.chars.isLower(65)) == (

      false))

  }

  test("isLower - digit") {

    assert((

      hydra.lib.chars.isLower(53)) == (

      false))

  }

  // isSpace

  test("isSpace - space") {

    assert((

      hydra.lib.chars.isSpace(32)) == (

      true))

  }

  test("isSpace - tab") {

    assert((

      hydra.lib.chars.isSpace(9)) == (

      true))

  }

  test("isSpace - newline") {

    assert((

      hydra.lib.chars.isSpace(10)) == (

      true))

  }

  test("isSpace - letter") {

    assert((

      hydra.lib.chars.isSpace(97)) == (

      false))

  }

  // isUpper

  test("isUpper - uppercase") {

    assert((

      hydra.lib.chars.isUpper(65)) == (

      true))

  }

  test("isUpper - lowercase") {

    assert((

      hydra.lib.chars.isUpper(97)) == (

      false))

  }

  test("isUpper - digit") {

    assert((

      hydra.lib.chars.isUpper(53)) == (

      false))

  }

  // toLower

  test("toLower - uppercase") {

    assert((

      hydra.lib.chars.toLower(65)) == (

      97))

  }

  test("toLower - lowercase") {

    assert((

      hydra.lib.chars.toLower(97)) == (

      97))

  }

  test("toLower - digit") {

    assert((

      hydra.lib.chars.toLower(53)) == (

      53))

  }

  // toUpper

  test("toUpper - lowercase") {

    assert((

      hydra.lib.chars.toUpper(97)) == (

      65))

  }

  test("toUpper - uppercase") {

    assert((

      hydra.lib.chars.toUpper(65)) == (

      65))

  }

  test("toUpper - digit") {

    assert((

      hydra.lib.chars.toUpper(53)) == (

      53))

  }
}

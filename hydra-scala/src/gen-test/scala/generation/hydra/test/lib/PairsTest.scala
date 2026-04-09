// Note: this is an automatically generated file. Do not edit.
// hydra.lib.pairs primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class PairsTest extends AnyFunSuite {

  // bimap

  test("bimap - transform both elements") {

    assert((

      (10, 2)) == (

      (10, 2)))

  }

  test("bimap - with zero") {

    assert((

      (0, 5)) == (

      (0, 5)))

  }

  // first

  test("first - extract first element") {

    assert((

      42) == (

      42))

  }

  test("first - with zero") {

    assert((

      0) == (

      0))

  }

  test("first - negative number") {

    assert((

      -5) == (

      -5))

  }

  // second

  test("second - extract second element") {

    assert((

      hello) == (

      hello))

  }

  test("second - empty string") {

    assert((

      ) == (

      ))

  }

  test("second - long string") {

    assert((

      testing) == (

      testing))

  }
}

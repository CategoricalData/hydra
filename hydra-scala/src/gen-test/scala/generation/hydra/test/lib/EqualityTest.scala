// Note: this is an automatically generated file. Do not edit.
// hydra.lib.equality primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class EqualityTest extends AnyFunSuite {

  // compare

  test("compare - less than") {

    assert((

      inject(hydra.util.Comparison){lessThan=unit}) == (

      inject(hydra.util.Comparison){lessThan=unit}))

  }

  test("compare - equal") {

    assert((

      inject(hydra.util.Comparison){equalTo=unit}) == (

      inject(hydra.util.Comparison){equalTo=unit}))

  }

  test("compare - greater than") {

    assert((

      inject(hydra.util.Comparison){greaterThan=unit}) == (

      inject(hydra.util.Comparison){greaterThan=unit}))

  }

  // equal

  test("equal - equal integers") {

    assert((

      true) == (

      true))

  }

  test("equal - unequal integers") {

    assert((

      false) == (

      false))

  }

  // gt

  test("gt - greater") {

    assert((

      true) == (

      true))

  }

  test("gt - equal") {

    assert((

      false) == (

      false))

  }

  test("gt - less") {

    assert((

      false) == (

      false))

  }

  // gte

  test("gte - greater") {

    assert((

      true) == (

      true))

  }

  test("gte - equal") {

    assert((

      true) == (

      true))

  }

  test("gte - less") {

    assert((

      false) == (

      false))

  }

  // identity

  test("identity - integer") {

    assert((

      42:int32) == (

      42:int32))

  }

  // lt

  test("lt - less") {

    assert((

      true) == (

      true))

  }

  test("lt - equal") {

    assert((

      false) == (

      false))

  }

  test("lt - greater") {

    assert((

      false) == (

      false))

  }

  // lte

  test("lte - less") {

    assert((

      true) == (

      true))

  }

  test("lte - equal") {

    assert((

      true) == (

      true))

  }

  test("lte - greater") {

    assert((

      false) == (

      false))

  }

  // max

  test("max - first greater") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("max - second greater") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("max - equal") {

    assert((

      5:int32) == (

      5:int32))

  }

  // min

  test("min - first less") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("min - second less") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("min - equal") {

    assert((

      5:int32) == (

      5:int32))

  }

  // compare strings

  test("compare strings - less than (lexicographic)") {

    assert((

      inject(hydra.util.Comparison){lessThan=unit}) == (

      inject(hydra.util.Comparison){lessThan=unit}))

  }

  test("compare strings - equal") {

    assert((

      inject(hydra.util.Comparison){equalTo=unit}) == (

      inject(hydra.util.Comparison){equalTo=unit}))

  }

  test("compare strings - greater than (lexicographic)") {

    assert((

      inject(hydra.util.Comparison){greaterThan=unit}) == (

      inject(hydra.util.Comparison){greaterThan=unit}))

  }

  test("compare strings - empty vs non-empty") {

    assert((

      inject(hydra.util.Comparison){lessThan=unit}) == (

      inject(hydra.util.Comparison){lessThan=unit}))

  }

  test("compare strings - prefix vs longer") {

    assert((

      inject(hydra.util.Comparison){lessThan=unit}) == (

      inject(hydra.util.Comparison){lessThan=unit}))

  }

  // lt strings

  test("lt strings - less (lexicographic)") {

    assert((

      true) == (

      true))

  }

  test("lt strings - equal") {

    assert((

      false) == (

      false))

  }

  test("lt strings - greater") {

    assert((

      false) == (

      false))

  }

  // gt strings

  test("gt strings - greater (lexicographic)") {

    assert((

      true) == (

      true))

  }

  test("gt strings - equal") {

    assert((

      false) == (

      false))

  }

  test("gt strings - less") {

    assert((

      false) == (

      false))

  }

  // max strings

  test("max strings - first greater") {

    assert((

      "zebra") == (

      "zebra"))

  }

  test("max strings - second greater") {

    assert((

      "zebra") == (

      "zebra"))

  }

  test("max strings - equal") {

    assert((

      "hello") == (

      "hello"))

  }

  // min strings

  test("min strings - first less") {

    assert((

      "apple") == (

      "apple"))

  }

  test("min strings - second less") {

    assert((

      "apple") == (

      "apple"))

  }

  test("min strings - equal") {

    assert((

      "hello") == (

      "hello"))

  }

  // compare floats

  test("compare floats - less than") {

    assert((

      inject(hydra.util.Comparison){lessThan=unit}) == (

      inject(hydra.util.Comparison){lessThan=unit}))

  }

  test("compare floats - equal") {

    assert((

      inject(hydra.util.Comparison){equalTo=unit}) == (

      inject(hydra.util.Comparison){equalTo=unit}))

  }

  test("compare floats - greater than") {

    assert((

      inject(hydra.util.Comparison){greaterThan=unit}) == (

      inject(hydra.util.Comparison){greaterThan=unit}))

  }

  test("compare floats - negative vs positive") {

    assert((

      inject(hydra.util.Comparison){lessThan=unit}) == (

      inject(hydra.util.Comparison){lessThan=unit}))

  }

  // lt floats

  test("lt floats - less") {

    assert((

      true) == (

      true))

  }

  test("lt floats - equal") {

    assert((

      false) == (

      false))

  }

  test("lt floats - greater") {

    assert((

      false) == (

      false))

  }

  // gt floats

  test("gt floats - greater") {

    assert((

      true) == (

      true))

  }

  test("gt floats - equal") {

    assert((

      false) == (

      false))

  }

  test("gt floats - less") {

    assert((

      false) == (

      false))

  }
}

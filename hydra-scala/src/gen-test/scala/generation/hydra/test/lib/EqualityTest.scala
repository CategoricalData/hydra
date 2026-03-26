// Note: this is an automatically generated file. Do not edit.
// hydra.lib.equality primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class EqualityTest extends AnyFunSuite {

  // compare

  test("compare - less than") {

    assert((

      hydra.lib.equality.compare[Int](3)(5)) == (

      hydra.util.Comparison.lessThan))

  }

  test("compare - equal") {

    assert((

      hydra.lib.equality.compare[Int](5)(5)) == (

      hydra.util.Comparison.equalTo))

  }

  test("compare - greater than") {

    assert((

      hydra.lib.equality.compare[Int](5)(3)) == (

      hydra.util.Comparison.greaterThan))

  }

  // equal

  test("equal - equal integers") {

    assert((

      hydra.lib.equality.equal[Int](5)(5)) == (

      true))

  }

  test("equal - unequal integers") {

    assert((

      hydra.lib.equality.equal[Int](5)(3)) == (

      false))

  }

  // gt

  test("gt - greater") {

    assert((

      hydra.lib.equality.gt[Int](5)(3)) == (

      true))

  }

  test("gt - equal") {

    assert((

      hydra.lib.equality.gt[Int](5)(5)) == (

      false))

  }

  test("gt - less") {

    assert((

      hydra.lib.equality.gt[Int](3)(5)) == (

      false))

  }

  // gte

  test("gte - greater") {

    assert((

      hydra.lib.equality.gte[Int](5)(3)) == (

      true))

  }

  test("gte - equal") {

    assert((

      hydra.lib.equality.gte[Int](5)(5)) == (

      true))

  }

  test("gte - less") {

    assert((

      hydra.lib.equality.gte[Int](3)(5)) == (

      false))

  }

  // identity

  test("identity - integer") {

    assert((

      hydra.lib.equality.identity[Int](42)) == (

      42))

  }

  // lt

  test("lt - less") {

    assert((

      hydra.lib.equality.lt[Int](3)(5)) == (

      true))

  }

  test("lt - equal") {

    assert((

      hydra.lib.equality.lt[Int](5)(5)) == (

      false))

  }

  test("lt - greater") {

    assert((

      hydra.lib.equality.lt[Int](5)(3)) == (

      false))

  }

  // lte

  test("lte - less") {

    assert((

      hydra.lib.equality.lte[Int](3)(5)) == (

      true))

  }

  test("lte - equal") {

    assert((

      hydra.lib.equality.lte[Int](5)(5)) == (

      true))

  }

  test("lte - greater") {

    assert((

      hydra.lib.equality.lte[Int](5)(3)) == (

      false))

  }

  // max

  test("max - first greater") {

    assert((

      hydra.lib.equality.max[Int](5)(3)) == (

      5))

  }

  test("max - second greater") {

    assert((

      hydra.lib.equality.max[Int](3)(5)) == (

      5))

  }

  test("max - equal") {

    assert((

      hydra.lib.equality.max[Int](5)(5)) == (

      5))

  }

  // min

  test("min - first less") {

    assert((

      hydra.lib.equality.min[Int](3)(5)) == (

      3))

  }

  test("min - second less") {

    assert((

      hydra.lib.equality.min[Int](5)(3)) == (

      3))

  }

  test("min - equal") {

    assert((

      hydra.lib.equality.min[Int](5)(5)) == (

      5))

  }

  // compare strings

  test("compare strings - less than (lexicographic)") {

    assert((

      hydra.lib.equality.compare[scala.Predef.String]("apple")("banana")) == (

      hydra.util.Comparison.lessThan))

  }

  test("compare strings - equal") {

    assert((

      hydra.lib.equality.compare[scala.Predef.String]("hello")("hello")) == (

      hydra.util.Comparison.equalTo))

  }

  test("compare strings - greater than (lexicographic)") {

    assert((

      hydra.lib.equality.compare[scala.Predef.String]("zebra")("apple")) == (

      hydra.util.Comparison.greaterThan))

  }

  test("compare strings - empty vs non-empty") {

    assert((

      hydra.lib.equality.compare[scala.Predef.String]("")("a")) == (

      hydra.util.Comparison.lessThan))

  }

  test("compare strings - prefix vs longer") {

    assert((

      hydra.lib.equality.compare[scala.Predef.String]("ab")("abc")) == (

      hydra.util.Comparison.lessThan))

  }

  // lt strings

  test("lt strings - less (lexicographic)") {

    assert((

      hydra.lib.equality.lt[scala.Predef.String]("apple")("banana")) == (

      true))

  }

  test("lt strings - equal") {

    assert((

      hydra.lib.equality.lt[scala.Predef.String]("hello")("hello")) == (

      false))

  }

  test("lt strings - greater") {

    assert((

      hydra.lib.equality.lt[scala.Predef.String]("zebra")("apple")) == (

      false))

  }

  // gt strings

  test("gt strings - greater (lexicographic)") {

    assert((

      hydra.lib.equality.gt[scala.Predef.String]("zebra")("apple")) == (

      true))

  }

  test("gt strings - equal") {

    assert((

      hydra.lib.equality.gt[scala.Predef.String]("hello")("hello")) == (

      false))

  }

  test("gt strings - less") {

    assert((

      hydra.lib.equality.gt[scala.Predef.String]("apple")("banana")) == (

      false))

  }

  // max strings

  test("max strings - first greater") {

    assert((

      hydra.lib.equality.max[scala.Predef.String]("zebra")("apple")) == (

      "zebra"))

  }

  test("max strings - second greater") {

    assert((

      hydra.lib.equality.max[scala.Predef.String]("apple")("zebra")) == (

      "zebra"))

  }

  test("max strings - equal") {

    assert((

      hydra.lib.equality.max[scala.Predef.String]("hello")("hello")) == (

      "hello"))

  }

  // min strings

  test("min strings - first less") {

    assert((

      hydra.lib.equality.min[scala.Predef.String]("apple")("zebra")) == (

      "apple"))

  }

  test("min strings - second less") {

    assert((

      hydra.lib.equality.min[scala.Predef.String]("zebra")("apple")) == (

      "apple"))

  }

  test("min strings - equal") {

    assert((

      hydra.lib.equality.min[scala.Predef.String]("hello")("hello")) == (

      "hello"))

  }

  // compare floats

  test("compare floats - less than") {

    assert((

      hydra.lib.equality.compare[Double](1.5)(2.5)) == (

      hydra.util.Comparison.lessThan))

  }

  test("compare floats - equal") {

    assert((

      hydra.lib.equality.compare[Double](3.14)(3.14)) == (

      hydra.util.Comparison.equalTo))

  }

  test("compare floats - greater than") {

    assert((

      hydra.lib.equality.compare[Double](5.0)(3.0)) == (

      hydra.util.Comparison.greaterThan))

  }

  test("compare floats - negative vs positive") {

    assert((

      hydra.lib.equality.compare[Double](-1.0)(1.0)) == (

      hydra.util.Comparison.lessThan))

  }

  // lt floats

  test("lt floats - less") {

    assert((

      hydra.lib.equality.lt[Double](1.5)(2.5)) == (

      true))

  }

  test("lt floats - equal") {

    assert((

      hydra.lib.equality.lt[Double](3.14)(3.14)) == (

      false))

  }

  test("lt floats - greater") {

    assert((

      hydra.lib.equality.lt[Double](5.0)(3.0)) == (

      false))

  }

  // gt floats

  test("gt floats - greater") {

    assert((

      hydra.lib.equality.gt[Double](5.0)(3.0)) == (

      true))

  }

  test("gt floats - equal") {

    assert((

      hydra.lib.equality.gt[Double](3.14)(3.14)) == (

      false))

  }

  test("gt floats - less") {

    assert((

      hydra.lib.equality.gt[Double](1.5)(2.5)) == (

      false))

  }
}

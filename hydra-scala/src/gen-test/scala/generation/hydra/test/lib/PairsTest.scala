// Note: this is an automatically generated file. Do not edit.
// hydra.lib.pairs primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class PairsTest extends AnyFunSuite {

  // bimap

  test("bimap - transform both elements") {

    assert((

      hydra.lib.pairs.bimap[Int, scala.Predef.String, Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))((s: scala.Predef.String) => hydra.lib.strings.length(s))(Tuple2(5, "ab"))) == (

      Tuple2(10, 2)))

  }

  test("bimap - with zero") {

    assert((

      hydra.lib.pairs.bimap[Int, scala.Predef.String, Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))((s: scala.Predef.String) => hydra.lib.strings.length(s))(Tuple2(0, "hello"))) == (

      Tuple2(0, 5)))

  }

  // first

  test("first - extract first element") {

    assert((

      hydra.lib.pairs.first[Int, scala.Predef.String](Tuple2(42, "hello"))) == (

      42))

  }

  test("first - with zero") {

    assert((

      hydra.lib.pairs.first[Int, scala.Predef.String](Tuple2(0, "world"))) == (

      0))

  }

  test("first - negative number") {

    assert((

      hydra.lib.pairs.first[Int, scala.Predef.String](Tuple2(-5, "test"))) == (

      -5))

  }

  // second

  test("second - extract second element") {

    assert((

      hydra.lib.pairs.second[Int, scala.Predef.String](Tuple2(42, "hello"))) == (

      "hello"))

  }

  test("second - empty string") {

    assert((

      hydra.lib.pairs.second[Int, scala.Predef.String](Tuple2(0, ""))) == (

      ""))

  }

  test("second - long string") {

    assert((

      hydra.lib.pairs.second[Int, scala.Predef.String](Tuple2(123, "testing"))) == (

      "testing"))

  }
}

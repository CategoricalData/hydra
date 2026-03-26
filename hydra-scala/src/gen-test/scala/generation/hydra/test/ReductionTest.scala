// Note: this is an automatically generated file. Do not edit.
// reduction

package generation.hydra.test

import org.scalatest.funsuite.AnyFunSuite

class ReductionTest extends AnyFunSuite {

  // beta reduction

  test("beta reduction - identity function applied to literal") {

    assert((

      ((x: Int) => x)(42)) == (

      42))

  }

  test("beta reduction - constant function") {

    assert((

      ((x: Int) => 1)(42)) == (

      1))

  }

  test("beta reduction - nested application") {

    assert((

      ((x: Int) => (y: Int) => x)(1)(2)) == (

      1))

  }

  // monomorphic primitives

  test("monomorphic primitives - toUpper on lowercase") {

    assert((

      hydra.lib.strings.toUpper("hello")) == (

      "HELLO"))

  }

  test("monomorphic primitives - toUpper on mixed case") {

    assert((

      hydra.lib.strings.toUpper("Hello World")) == (

      "HELLO WORLD"))

  }

  test("monomorphic primitives - toUpper on empty string") {

    assert((

      hydra.lib.strings.toUpper("")) == (

      ""))

  }

  test("monomorphic primitives - toLower on uppercase") {

    assert((

      hydra.lib.strings.toLower("HELLO")) == (

      "hello"))

  }

  test("monomorphic primitives - string length") {

    assert((

      hydra.lib.strings.length("hello")) == (

      5))

  }

  test("monomorphic primitives - string length of empty") {

    assert((

      hydra.lib.strings.length("")) == (

      0))

  }

  test("monomorphic primitives - add two positive integers") {

    assert((

      hydra.lib.math.add(3)(5)) == (

      8))

  }

  test("monomorphic primitives - add negative and positive") {

    assert((

      hydra.lib.math.add(-10)(3)) == (

      -7))

  }

  test("monomorphic primitives - add with zero") {

    assert((

      hydra.lib.math.add(0)(42)) == (

      42))

  }

  test("monomorphic primitives - subtract integers") {

    assert((

      hydra.lib.math.sub(10)(3)) == (

      7))

  }

  test("monomorphic primitives - multiply integers") {

    assert((

      hydra.lib.math.mul(6)(7)) == (

      42))

  }

  test("monomorphic primitives - multiply by zero") {

    assert((

      hydra.lib.math.mul(100)(0)) == (

      0))

  }

  test("monomorphic primitives - divide integers") {

    assert((

      hydra.lib.math.div(20)(4)) == (

      5))

  }

  test("monomorphic primitives - modulo") {

    assert((

      hydra.lib.math.mod(17)(5)) == (

      2))

  }

  test("monomorphic primitives - splitOn basic") {

    assert((

      hydra.lib.strings.splitOn(",")("a,b,c")) == (

      Seq("a", "b", "c")))

  }

  test("monomorphic primitives - cat2 strings") {

    assert((

      hydra.lib.strings.cat2("hello")("world")) == (

      "helloworld"))

  }

  // polymorphic primitives

  test("polymorphic primitives - length of integer list") {

    assert((

      hydra.lib.lists.length[Int](Seq(1, 2, 3))) == (

      3))

  }

  test("polymorphic primitives - length of string list") {

    assert((

      hydra.lib.lists.length[scala.Predef.String](Seq("a", "b"))) == (

      2))

  }

  test("polymorphic primitives - length of empty list") {

    assert((

      hydra.lib.lists.length(Seq())) == (

      0))

  }

  test("polymorphic primitives - length of single element list") {

    assert((

      hydra.lib.lists.length[Boolean](Seq(true))) == (

      1))

  }

  test("polymorphic primitives - head of integer list") {

    assert((

      hydra.lib.lists.head[Int](Seq(10, 20, 30))) == (

      10))

  }

  test("polymorphic primitives - head of string list") {

    assert((

      hydra.lib.lists.head[scala.Predef.String](Seq("first", "second"))) == (

      "first"))

  }

  test("polymorphic primitives - last of integer list") {

    assert((

      hydra.lib.lists.last[Int](Seq(10, 20, 30))) == (

      30))

  }

  test("polymorphic primitives - concat two integer lists") {

    assert((

      hydra.lib.lists.concat2[Int](Seq(1, 2))(Seq(3, 4))) == (

      Seq(1, 2, 3, 4)))

  }

  test("polymorphic primitives - concat with empty list") {

    assert((

      hydra.lib.lists.concat2[Int](Seq())(Seq(1, 2))) == (

      Seq(1, 2)))

  }

  test("polymorphic primitives - reverse integer list") {

    assert((

      hydra.lib.lists.reverse[Int](Seq(1, 2, 3))) == (

      Seq(3, 2, 1)))

  }

  test("polymorphic primitives - reverse empty list") {

    assert((

      hydra.lib.lists.reverse(Seq())) == (

      Seq()))

  }

  // nullary primitives

  test("nullary primitives - empty set has size zero") {

    assert((

      hydra.lib.sets.size(hydra.lib.sets.empty)) == (

      0))

  }

  // literals as values

  test("literals as values - integer literal is a value") {

    assert((

      42) == (

      42))

  }

  test("literals as values - negative integer literal") {

    assert((

      -17) == (

      -17))

  }

  test("literals as values - zero integer literal") {

    assert((

      0) == (

      0))

  }

  test("literals as values - string literal is a value") {

    assert((

      "hello") == (

      "hello"))

  }

  test("literals as values - empty string literal") {

    assert((

      "") == (

      ""))

  }

  test("literals as values - string with special characters") {

    assert((

      "hello\nworld\ttab") == (

      "hello\nworld\ttab"))

  }

  test("literals as values - boolean true is a value") {

    assert((

      true) == (

      true))

  }

  test("literals as values - boolean false is a value") {

    assert((

      false) == (

      false))

  }

  test("literals as values - float literal is a value") {

    assert(math.abs((3.14) - (3.14)) <= 1e-15)

  }

  test("literals as values - negative float literal") {

    assert(math.abs((-2.718) - (-2.718)) <= 1e-15)

  }

  test("literals as values - zero float literal") {

    assert(math.abs((0.0) - (0.0)) <= 1e-15)

  }

  // list reduction

  test("list reduction - empty list is a value") {

    assert((

      Seq()) == (

      Seq()))

  }

  test("list reduction - list of literals is a value") {

    assert((

      Seq(1, 2, 3)) == (

      Seq(1, 2, 3)))

  }

  test("list reduction - list with reducible element") {

    assert((

      Seq(((x: Int) => x)(42))) == (

      Seq(42)))

  }

  // optional reduction

  test("optional reduction - nothing is a value") {

    assert((

      None) == (

      None))

  }

  test("optional reduction - just literal is a value") {

    assert((

      Some(42)) == (

      Some(42)))

  }

  test("optional reduction - just with reducible content") {

    assert((

      Some(((x: Int) => x)(42))) == (

      Some(42)))

  }
}

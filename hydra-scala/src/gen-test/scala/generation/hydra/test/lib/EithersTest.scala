// Note: this is an automatically generated file. Do not edit.
// hydra.lib.eithers primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class EithersTest extends AnyFunSuite {

  // bind

  test("bind - bind Right with success") {

    assert((

      hydra.lib.eithers.bind[Int, scala.Predef.String, Int](Right("ab"))((s: scala.Predef.String) =>
  hydra.lib.logic.ifElse[Either[Int, Int]](hydra.lib.strings.`null`(s))(Left(0))(Right(hydra.lib.strings.length(s))))) == (

      Right(2)))

  }

  test("bind - bind Right with failure") {

    assert((

      hydra.lib.eithers.bind[Int, scala.Predef.String, Int](Right(""))((s: scala.Predef.String) =>
  hydra.lib.logic.ifElse[Either[Int, Int]](hydra.lib.strings.`null`(s))(Left(0))(Right(hydra.lib.strings.length(s))))) == (

      Left(0)))

  }

  test("bind - bind Left returns Left unchanged") {

    assert((

      hydra.lib.eithers.bind[Int, scala.Predef.String, Int](Left(42))((s: scala.Predef.String) =>
  hydra.lib.logic.ifElse[Either[Int, Int]](hydra.lib.strings.`null`(s))(Left(0))(Right(hydra.lib.strings.length(s))))) == (

      Left(42)))

  }

  // bimap

  test("bimap - map left value") {

    assert((

      hydra.lib.eithers.bimap[Int, scala.Predef.String, Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))((s: scala.Predef.String) => hydra.lib.strings.length(s))(Left(5))) == (

      Left(10)))

  }

  test("bimap - map right value") {

    assert((

      hydra.lib.eithers.bimap[Int, scala.Predef.String, Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))((s: scala.Predef.String) => hydra.lib.strings.length(s))(Right("ab"))) == (

      Right(2)))

  }

  // isLeft

  test("isLeft - left value") {

    assert((

      hydra.lib.eithers.isLeft(Left(42))) == (

      true))

  }

  test("isLeft - right value") {

    assert((

      hydra.lib.eithers.isLeft(Right("test"))) == (

      false))

  }

  // isRight

  test("isRight - right value") {

    assert((

      hydra.lib.eithers.isRight(Right("test"))) == (

      true))

  }

  test("isRight - left value") {

    assert((

      hydra.lib.eithers.isRight(Left(42))) == (

      false))

  }

  // fromLeft

  test("fromLeft - extract left") {

    assert((

      hydra.lib.eithers.fromLeft(99)(Left(42))) == (

      42))

  }

  test("fromLeft - use default for right") {

    assert((

      hydra.lib.eithers.fromLeft[Int, scala.Predef.String](99)(Right("test"))) == (

      99))

  }

  // fromRight

  test("fromRight - extract right") {

    assert((

      hydra.lib.eithers.fromRight("default")(Right("test"))) == (

      "test"))

  }

  test("fromRight - use default for left") {

    assert((

      hydra.lib.eithers.fromRight[Int, scala.Predef.String]("default")(Left(42))) == (

      "default"))

  }

  // either

  test("either - apply left function") {

    assert((

      hydra.lib.eithers.either[Int, scala.Predef.String, Int]((x: Int) => hydra.lib.math.mul(x)(2))((s: scala.Predef.String) => hydra.lib.strings.length(s))(Left(5))) == (

      10))

  }

  test("either - apply right function") {

    assert((

      hydra.lib.eithers.either[Int, scala.Predef.String, Int]((x: Int) => hydra.lib.math.mul(x)(2))((s: scala.Predef.String) => hydra.lib.strings.length(s))(Right("ab"))) == (

      2))

  }

  // lefts

  test("lefts - filter left values") {

    assert((

      hydra.lib.eithers.lefts[Int, scala.Predef.String](Seq(Left(1), Right("a"), Left(2), Right("b")))) == (

      Seq(1, 2)))

  }

  test("lefts - all lefts") {

    assert((

      hydra.lib.eithers.lefts(Seq(Left(1), Left(2)))) == (

      Seq(1, 2)))

  }

  test("lefts - all rights") {

    assert((

      hydra.lib.eithers.lefts(Seq(Right("a"), Right("b")))) == (

      Seq()))

  }

  test("lefts - empty list") {

    assert((

      hydra.lib.eithers.lefts(Seq())) == (

      Seq()))

  }

  // rights

  test("rights - filter right values") {

    assert((

      hydra.lib.eithers.rights[Int, scala.Predef.String](Seq(Left(1), Right("a"), Left(2), Right("b")))) == (

      Seq("a", "b")))

  }

  test("rights - all rights") {

    assert((

      hydra.lib.eithers.rights(Seq(Right("a"), Right("b")))) == (

      Seq("a", "b")))

  }

  test("rights - all lefts") {

    assert((

      hydra.lib.eithers.rights(Seq(Left(1), Left(2)))) == (

      Seq()))

  }

  test("rights - empty list") {

    assert((

      hydra.lib.eithers.rights(Seq())) == (

      Seq()))

  }

  // partitionEithers

  test("partitionEithers - partition mixed") {

    assert((

      hydra.lib.eithers.partitionEithers[Int, scala.Predef.String](Seq(Left(1), Right("a"), Left(2), Right("b")))) == (

      Tuple2(Seq(1, 2), Seq("a", "b"))))

  }

  test("partitionEithers - all lefts") {

    assert((

      hydra.lib.eithers.partitionEithers(Seq(Left(1), Left(2)))) == (

      Tuple2(Seq(1, 2), Seq())))

  }

  test("partitionEithers - all rights") {

    assert((

      hydra.lib.eithers.partitionEithers(Seq(Right("a"), Right("b")))) == (

      Tuple2(Seq(), Seq("a", "b"))))

  }

  test("partitionEithers - empty list") {

    assert((

      hydra.lib.eithers.partitionEithers(Seq())) == (

      Tuple2(Seq(), Seq())))

  }

  // map

  test("map - map right value") {

    assert((

      hydra.lib.eithers.map((x: Int) => hydra.lib.math.mul(x)(2))(Right(5))) == (

      Right(10)))

  }

  test("map - preserve left") {

    assert((

      hydra.lib.eithers.map[Int, Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))(Left(99))) == (

      Left(99)))

  }

  // mapList

  test("mapList - all succeed") {

    assert((

      hydra.lib.eithers.mapList[Int, Int, scala.Predef.String]((x: Int) =>
  hydra.lib.logic.ifElse[Either[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](x)(0))(Left("zero"))(Right(hydra.lib.math.mul(x)(2))))(Seq(1, 2, 3))) == (

      Right(Seq(2, 4, 6))))

  }

  test("mapList - first fails") {

    assert((

      hydra.lib.eithers.mapList[Int, Int, scala.Predef.String]((x: Int) =>
  hydra.lib.logic.ifElse[Either[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](x)(0))(Left("zero"))(Right(hydra.lib.math.mul(x)(2))))(Seq(1, 0, 3))) == (

      Left("zero")))

  }

  test("mapList - empty list") {

    assert((

      hydra.lib.eithers.mapList[Int, Int, scala.Predef.String]((x: Int) =>
  hydra.lib.logic.ifElse[Either[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](x)(0))(Left("zero"))(Right(hydra.lib.math.mul(x)(2))))(Seq())) == (

      Right(Seq())))

  }

  // mapMaybe

  test("mapMaybe - just succeeds") {

    assert((

      hydra.lib.eithers.mapMaybe[Int, Int, scala.Predef.String]((x: Int) =>
  hydra.lib.logic.ifElse[Either[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](x)(0))(Left("zero"))(Right(hydra.lib.math.mul(x)(2))))(Some(5))) == (

      Right(Some(10))))

  }

  test("mapMaybe - just fails") {

    assert((

      hydra.lib.eithers.mapMaybe[Int, Int, scala.Predef.String]((x: Int) =>
  hydra.lib.logic.ifElse[Either[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](x)(0))(Left("zero"))(Right(hydra.lib.math.mul(x)(2))))(Some(0))) == (

      Left("zero")))

  }

  test("mapMaybe - nothing") {

    assert((

      hydra.lib.eithers.mapMaybe[Int, Int, scala.Predef.String]((x: Int) =>
  hydra.lib.logic.ifElse[Either[scala.Predef.String, Int]](hydra.lib.equality.equal[Int](x)(0))(Left("zero"))(Right(hydra.lib.math.mul(x)(2))))(None)) == (

      Right(None)))

  }
}

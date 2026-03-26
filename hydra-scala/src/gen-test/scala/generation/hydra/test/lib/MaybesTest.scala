// Note: this is an automatically generated file. Do not edit.
// hydra.lib.maybes primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class MaybesTest extends AnyFunSuite {

  // apply

  test("apply - both just") {

    assert((

      hydra.lib.maybes.apply[Int, Int](Some(hydra.lib.math.add(3)))(Some(5))) == (

      Some(8)))

  }

  test("apply - nothing function") {

    assert((

      hydra.lib.maybes.apply(None)(Some(5))) == (

      None))

  }

  test("apply - nothing value") {

    assert((

      hydra.lib.maybes.apply[Int, Int](Some(hydra.lib.math.add(3)))(None)) == (

      None))

  }

  // bind

  test("bind - just to just") {

    assert((

      hydra.lib.maybes.bind[Int, Int](Some(5))((x: Int) => Some(hydra.lib.math.mul(x)(2)))) == (

      Some(10)))

  }

  test("bind - nothing to nothing") {

    assert((

      hydra.lib.maybes.bind[Int, Int](None)((x: Int) => Some(hydra.lib.math.mul(x)(2)))) == (

      None))

  }

  // cases

  test("cases - just applies function") {

    assert((

      hydra.lib.maybes.cases[Int, Int](Some(5))(0)((x: Int) => hydra.lib.math.mul(x)(2))) == (

      10))

  }

  test("cases - nothing returns default") {

    assert((

      hydra.lib.maybes.cases[Int, Int](None)(99)((x: Int) => hydra.lib.math.mul(x)(2))) == (

      99))

  }

  // cat

  test("cat - filters nothings") {

    assert((

      hydra.lib.maybes.cat[Int](Seq(Some(1), None, Some(2)))) == (

      Seq(1, 2)))

  }

  test("cat - all justs") {

    assert((

      hydra.lib.maybes.cat[Int](Seq(Some(1), Some(2)))) == (

      Seq(1, 2)))

  }

  test("cat - all nothings") {

    assert((

      hydra.lib.maybes.cat(Seq(None, None))) == (

      Seq()))

  }

  test("cat - empty list") {

    assert((

      hydra.lib.maybes.cat(Seq())) == (

      Seq()))

  }

  // compose

  test("compose - both succeed") {

    assert((

      hydra.lib.maybes.compose[Int, Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.lte[Int](x)(5))(Some(hydra.lib.math.add(x)(1)))(None))((y: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gte[Int](y)(5))(Some(hydra.lib.math.mul(y)(2)))(None))(5)) == (

      Some(12)))

  }

  test("compose - first fails") {

    assert((

      hydra.lib.maybes.compose[Int, Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.lte[Int](x)(5))(Some(hydra.lib.math.add(x)(1)))(None))((y: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gte[Int](y)(5))(Some(hydra.lib.math.mul(y)(2)))(None))(10)) == (

      None))

  }

  test("compose - second fails") {

    assert((

      hydra.lib.maybes.compose[Int, Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.lte[Int](x)(5))(Some(hydra.lib.math.add(x)(1)))(None))((y: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gte[Int](y)(5))(Some(hydra.lib.math.mul(y)(2)))(None))(3)) == (

      None))

  }

  // fromJust

  test("fromJust - extract from just") {

    assert((

      hydra.lib.maybes.fromJust[Int](Some(42))) == (

      42))

  }

  // fromMaybe

  test("fromMaybe - just value") {

    assert((

      hydra.lib.maybes.fromMaybe[Int](0)(Some(42))) == (

      42))

  }

  test("fromMaybe - nothing with default") {

    assert((

      hydra.lib.maybes.fromMaybe[Int](99)(None)) == (

      99))

  }

  // isJust

  test("isJust - just value") {

    assert((

      hydra.lib.maybes.isJust[Int](Some(42))) == (

      true))

  }

  test("isJust - nothing") {

    assert((

      hydra.lib.maybes.isJust(None)) == (

      false))

  }

  // isNothing

  test("isNothing - just value") {

    assert((

      hydra.lib.maybes.isNothing[Int](Some(42))) == (

      false))

  }

  test("isNothing - nothing") {

    assert((

      hydra.lib.maybes.isNothing(None)) == (

      true))

  }

  // map

  test("map - maps just value") {

    assert((

      hydra.lib.maybes.map[Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))(Some(5))) == (

      Some(10)))

  }

  test("map - nothing unchanged") {

    assert((

      hydra.lib.maybes.map[Int, Int]((x: Int) => hydra.lib.math.mul(x)(2))(None)) == (

      None))

  }

  // mapMaybe

  test("mapMaybe - filter and transform") {

    assert((

      hydra.lib.maybes.mapMaybe[Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gt[Int](x)(2))(Some(hydra.lib.math.mul(x)(2)))(None))(Seq(1, 2, 3, 4, 5))) == (

      Seq(6, 8, 10)))

  }

  test("mapMaybe - empty result") {

    assert((

      hydra.lib.maybes.mapMaybe[Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gt[Int](x)(2))(Some(hydra.lib.math.mul(x)(2)))(None))(Seq(1, 2))) == (

      Seq()))

  }

  test("mapMaybe - empty input") {

    assert((

      hydra.lib.maybes.mapMaybe[Int, Int]((x: Int) =>
  hydra.lib.logic.ifElse[Option[Int]](hydra.lib.equality.gt[Int](x)(2))(Some(hydra.lib.math.mul(x)(2)))(None))(Seq())) == (

      Seq()))

  }

  // maybe

  test("maybe - just value applies function") {

    assert((

      hydra.lib.maybes.maybe[Int, Int](0)((x: Int) => hydra.lib.math.mul(x)(2))(Some(5))) == (

      10))

  }

  test("maybe - nothing returns default") {

    assert((

      hydra.lib.maybes.maybe[Int, Int](99)((x: Int) => hydra.lib.math.mul(x)(2))(None)) == (

      99))

  }

  // pure

  test("pure - wraps integer") {

    assert((

      hydra.lib.maybes.pure[Int](42)) == (

      Some(42)))

  }

  test("pure - wraps string") {

    assert((

      hydra.lib.maybes.pure[scala.Predef.String]("hello")) == (

      Some("hello")))

  }

  // toList

  test("toList - just value") {

    assert((

      hydra.lib.maybes.toList[Int](Some(42))) == (

      Seq(42)))

  }

  test("toList - nothing") {

    assert((

      hydra.lib.maybes.toList(None)) == (

      Seq()))

  }
}

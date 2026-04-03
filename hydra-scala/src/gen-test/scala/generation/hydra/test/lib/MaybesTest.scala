// Note: this is an automatically generated file. Do not edit.
// hydra.lib.maybes primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class MaybesTest extends AnyFunSuite {

  // apply

  test("apply - both just") {

    assert((

      just(8)) == (

      just(8)))

  }

  test("apply - nothing function") {

    assert((

      nothing) == (

      nothing))

  }

  test("apply - nothing value") {

    assert((

      nothing) == (

      nothing))

  }

  // bind

  test("bind - just to just") {

    assert((

      just(10)) == (

      just(10)))

  }

  test("bind - nothing to nothing") {

    assert((

      nothing) == (

      nothing))

  }

  // cases

  test("cases - just applies function") {

    assert((

      10) == (

      10))

  }

  test("cases - nothing returns default") {

    assert((

      99) == (

      99))

  }

  // cat

  test("cat - filters nothings") {

    assert((

      [1, 2]) == (

      [1, 2]))

  }

  test("cat - all justs") {

    assert((

      [1, 2]) == (

      [1, 2]))

  }

  test("cat - all nothings") {

    assert((

      []) == (

      []))

  }

  test("cat - empty list") {

    assert((

      []) == (

      []))

  }

  // compose

  test("compose - both succeed") {

    assert((

      just(12)) == (

      just(12)))

  }

  test("compose - first fails") {

    assert((

      nothing) == (

      nothing))

  }

  test("compose - second fails") {

    assert((

      nothing) == (

      nothing))

  }

  // fromJust

  test("fromJust - extract from just") {

    assert((

      42) == (

      42))

  }

  // fromMaybe

  test("fromMaybe - just value") {

    assert((

      42) == (

      42))

  }

  test("fromMaybe - nothing with default") {

    assert((

      99) == (

      99))

  }

  // isJust

  test("isJust - just value") {

    assert((

      true) == (

      true))

  }

  test("isJust - nothing") {

    assert((

      false) == (

      false))

  }

  // isNothing

  test("isNothing - just value") {

    assert((

      false) == (

      false))

  }

  test("isNothing - nothing") {

    assert((

      true) == (

      true))

  }

  // map

  test("map - maps just value") {

    assert((

      just(10)) == (

      just(10)))

  }

  test("map - nothing unchanged") {

    assert((

      nothing) == (

      nothing))

  }

  // mapMaybe

  test("mapMaybe - filter and transform") {

    assert((

      [6, 8, 10]) == (

      [6, 8, 10]))

  }

  test("mapMaybe - empty result") {

    assert((

      []) == (

      []))

  }

  test("mapMaybe - empty input") {

    assert((

      []) == (

      []))

  }

  // maybe

  test("maybe - just value applies function") {

    assert((

      10) == (

      10))

  }

  test("maybe - nothing returns default") {

    assert((

      99) == (

      99))

  }

  // pure

  test("pure - wraps integer") {

    assert((

      just(42)) == (

      just(42)))

  }

  test("pure - wraps string") {

    assert((

      just("hello")) == (

      just("hello")))

  }

  // toList

  test("toList - just value") {

    assert((

      [42]) == (

      [42]))

  }

  test("toList - nothing") {

    assert((

      []) == (

      []))

  }
}

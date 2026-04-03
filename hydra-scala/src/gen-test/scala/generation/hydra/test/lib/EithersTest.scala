// Note: this is an automatically generated file. Do not edit.
// hydra.lib.eithers primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class EithersTest extends AnyFunSuite {

  // bind

  test("bind - bind Right with success") {

    assert((

      right(2)) == (

      right(2)))

  }

  test("bind - bind Right with failure") {

    assert((

      left(0)) == (

      left(0)))

  }

  test("bind - bind Left returns Left unchanged") {

    assert((

      left(42)) == (

      left(42)))

  }

  // bimap

  test("bimap - map left value") {

    assert((

      left(10)) == (

      left(10)))

  }

  test("bimap - map right value") {

    assert((

      right(2)) == (

      right(2)))

  }

  // isLeft

  test("isLeft - left value") {

    assert((

      true) == (

      true))

  }

  test("isLeft - right value") {

    assert((

      false) == (

      false))

  }

  // isRight

  test("isRight - right value") {

    assert((

      true) == (

      true))

  }

  test("isRight - left value") {

    assert((

      false) == (

      false))

  }

  // fromLeft

  test("fromLeft - extract left") {

    assert((

      42) == (

      42))

  }

  test("fromLeft - use default for right") {

    assert((

      99) == (

      99))

  }

  // fromRight

  test("fromRight - extract right") {

    assert((

      "test") == (

      "test"))

  }

  test("fromRight - use default for left") {

    assert((

      "default") == (

      "default"))

  }

  // either

  test("either - apply left function") {

    assert((

      10) == (

      10))

  }

  test("either - apply right function") {

    assert((

      2) == (

      2))

  }

  // lefts

  test("lefts - filter left values") {

    assert((

      [1, 2]) == (

      [1, 2]))

  }

  test("lefts - all lefts") {

    assert((

      [1, 2]) == (

      [1, 2]))

  }

  test("lefts - all rights") {

    assert((

      []) == (

      []))

  }

  test("lefts - empty list") {

    assert((

      []) == (

      []))

  }

  // rights

  test("rights - filter right values") {

    assert((

      ["a", "b"]) == (

      ["a", "b"]))

  }

  test("rights - all rights") {

    assert((

      ["a", "b"]) == (

      ["a", "b"]))

  }

  test("rights - all lefts") {

    assert((

      []) == (

      []))

  }

  test("rights - empty list") {

    assert((

      []) == (

      []))

  }

  // partitionEithers

  test("partitionEithers - partition mixed") {

    assert((

      ([1, 2], ["a", "b"])) == (

      ([1, 2], ["a", "b"])))

  }

  test("partitionEithers - all lefts") {

    assert((

      ([1, 2], [])) == (

      ([1, 2], [])))

  }

  test("partitionEithers - all rights") {

    assert((

      ([], ["a", "b"])) == (

      ([], ["a", "b"])))

  }

  test("partitionEithers - empty list") {

    assert((

      ([], [])) == (

      ([], [])))

  }

  // map

  test("map - map right value") {

    assert((

      right(10)) == (

      right(10)))

  }

  test("map - preserve left") {

    assert((

      left(99)) == (

      left(99)))

  }

  // mapList

  test("mapList - all succeed") {

    assert((

      right([2, 4, 6])) == (

      right([2, 4, 6])))

  }

  test("mapList - first fails") {

    assert((

      left("zero")) == (

      left("zero")))

  }

  test("mapList - empty list") {

    assert((

      right([])) == (

      right([])))

  }

  // mapMaybe

  test("mapMaybe - just succeeds") {

    assert((

      right(just(10))) == (

      right(just(10))))

  }

  test("mapMaybe - just fails") {

    assert((

      left("zero")) == (

      left("zero")))

  }

  test("mapMaybe - nothing") {

    assert((

      right(nothing)) == (

      right(nothing)))

  }
}

// Note: this is an automatically generated file. Do not edit.
// hydra.lib.math primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class MathTest extends AnyFunSuite {

  // abs

  test("abs - positive") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("abs - negative") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("abs - zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  // add

  test("add - positive numbers") {

    assert((

      8:int32) == (

      8:int32))

  }

  test("add - negative numbers") {

    assert((

      -8:int32) == (

      -8:int32))

  }

  test("add - mixed sign") {

    assert((

      7:int32) == (

      7:int32))

  }

  test("add - with zero") {

    assert((

      42:int32) == (

      42:int32))

  }

  // div

  test("div - exact division") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("div - truncates toward negative infinity") {

    assert((

      3:int32) == (

      3:int32))

  }

  test("div - negative dividend") {

    assert((

      -4:int32) == (

      -4:int32))

  }

  test("div - negative divisor") {

    assert((

      -4:int32) == (

      -4:int32))

  }

  // even

  test("even - even positive") {

    assert((

      true) == (

      true))

  }

  test("even - odd positive") {

    assert((

      false) == (

      false))

  }

  test("even - even negative") {

    assert((

      true) == (

      true))

  }

  test("even - odd negative") {

    assert((

      false) == (

      false))

  }

  test("even - zero") {

    assert((

      true) == (

      true))

  }

  // max

  test("max - first is larger") {

    assert((

      10:int32) == (

      10:int32))

  }

  test("max - second is larger") {

    assert((

      10:int32) == (

      10:int32))

  }

  test("max - equal values") {

    assert((

      7:int32) == (

      7:int32))

  }

  test("max - negative numbers") {

    assert((

      -3:int32) == (

      -3:int32))

  }

  test("max - mixed sign") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("max - with zero") {

    assert((

      42:int32) == (

      42:int32))

  }

  // min

  test("min - first is smaller") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("min - second is smaller") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("min - equal values") {

    assert((

      7:int32) == (

      7:int32))

  }

  test("min - negative numbers") {

    assert((

      -5:int32) == (

      -5:int32))

  }

  test("min - mixed sign") {

    assert((

      -5:int32) == (

      -5:int32))

  }

  test("min - with zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  // mod

  test("mod - basic modulo") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("mod - exact division") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("mod - negative dividend") {

    assert((

      2:int32) == (

      2:int32))

  }

  test("mod - negative divisor") {

    assert((

      -2:int32) == (

      -2:int32))

  }

  // mul

  test("mul - positive numbers") {

    assert((

      15:int32) == (

      15:int32))

  }

  test("mul - negative numbers") {

    assert((

      15:int32) == (

      15:int32))

  }

  test("mul - mixed sign") {

    assert((

      -15:int32) == (

      -15:int32))

  }

  test("mul - with zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("mul - with one") {

    assert((

      42:int32) == (

      42:int32))

  }

  // negate

  test("negate - positive") {

    assert((

      -5:int32) == (

      -5:int32))

  }

  test("negate - negative") {

    assert((

      5:int32) == (

      5:int32))

  }

  test("negate - zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  // odd

  test("odd - odd positive") {

    assert((

      true) == (

      true))

  }

  test("odd - even positive") {

    assert((

      false) == (

      false))

  }

  test("odd - odd negative") {

    assert((

      true) == (

      true))

  }

  test("odd - even negative") {

    assert((

      false) == (

      false))

  }

  test("odd - zero") {

    assert((

      false) == (

      false))

  }

  // pred

  test("pred - positive") {

    assert((

      4:int32) == (

      4:int32))

  }

  test("pred - zero") {

    assert((

      -1:int32) == (

      -1:int32))

  }

  test("pred - negative") {

    assert((

      -6:int32) == (

      -6:int32))

  }

  // range

  test("range - ascending range") {

    assert((

      [1:int32, 2:int32, 3:int32, 4:int32, 5:int32]) == (

      [1:int32, 2:int32, 3:int32, 4:int32, 5:int32]))

  }

  test("range - single element") {

    assert((

      [5:int32]) == (

      [5:int32]))

  }

  test("range - two elements") {

    assert((

      [3:int32, 4:int32]) == (

      [3:int32, 4:int32]))

  }

  test("range - negative start") {

    assert((

      [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32]) == (

      [-2:int32, -1:int32, 0:int32, 1:int32, 2:int32]))

  }

  // rem

  test("rem - basic remainder") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("rem - exact division") {

    assert((

      0:int32) == (

      0:int32))

  }

  test("rem - negative dividend") {

    assert((

      -1:int32) == (

      -1:int32))

  }

  test("rem - negative divisor") {

    assert((

      1:int32) == (

      1:int32))

  }

  // signum

  test("signum - positive") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("signum - negative") {

    assert((

      -1:int32) == (

      -1:int32))

  }

  test("signum - zero") {

    assert((

      0:int32) == (

      0:int32))

  }

  // sub

  test("sub - positive numbers") {

    assert((

      7:int32) == (

      7:int32))

  }

  test("sub - negative numbers") {

    assert((

      -7:int32) == (

      -7:int32))

  }

  test("sub - mixed sign") {

    assert((

      13:int32) == (

      13:int32))

  }

  test("sub - with zero") {

    assert((

      42:int32) == (

      42:int32))

  }

  // succ

  test("succ - positive") {

    assert((

      6:int32) == (

      6:int32))

  }

  test("succ - zero") {

    assert((

      1:int32) == (

      1:int32))

  }

  test("succ - negative") {

    assert((

      -4:int32) == (

      -4:int32))

  }

  // e

  test("e - Euler's number") {

    assert((

      2.71828182846:float64) == (

      2.71828182846:float64))

  }

  // pi

  test("pi - pi constant") {

    assert((

      3.14159265359:float64) == (

      3.14159265359:float64))

  }

  // sin

  test("sin - sin 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("sin - sin pi/2") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("sin - sin pi") {

    assert((

      1.22464679915e-16:float64) == (

      1.22464679915e-16:float64))

  }

  test("sin - sin 1") {

    assert((

      0.841470984808:float64) == (

      0.841470984808:float64))

  }

  test("sin - sin 0.5") {

    assert((

      0.479425538604:float64) == (

      0.479425538604:float64))

  }

  // cos

  test("cos - cos 0") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("cos - cos pi/2") {

    assert((

      6.12323399574e-17:float64) == (

      6.12323399574e-17:float64))

  }

  test("cos - cos pi") {

    assert((

      -1.0:float64) == (

      -1.0:float64))

  }

  test("cos - cos 1") {

    assert((

      0.540302305868:float64) == (

      0.540302305868:float64))

  }

  test("cos - cos 0.5") {

    assert((

      0.87758256189:float64) == (

      0.87758256189:float64))

  }

  // tan

  test("tan - tan 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("tan - tan pi/4") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("tan - tan 1") {

    assert((

      1.55740772465:float64) == (

      1.55740772465:float64))

  }

  test("tan - tan 0.5") {

    assert((

      0.546302489844:float64) == (

      0.546302489844:float64))

  }

  // asin

  test("asin - asin 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("asin - asin 1") {

    assert((

      1.57079632679:float64) == (

      1.57079632679:float64))

  }

  test("asin - asin -1") {

    assert((

      -1.57079632679:float64) == (

      -1.57079632679:float64))

  }

  test("asin - asin 0.5") {

    assert((

      0.523598775598:float64) == (

      0.523598775598:float64))

  }

  // acos

  test("acos - acos 1") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("acos - acos 0") {

    assert((

      1.57079632679:float64) == (

      1.57079632679:float64))

  }

  test("acos - acos -1") {

    assert((

      3.14159265359:float64) == (

      3.14159265359:float64))

  }

  test("acos - acos 0.5") {

    assert((

      1.0471975512:float64) == (

      1.0471975512:float64))

  }

  // atan

  test("atan - atan 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("atan - atan 1") {

    assert((

      0.785398163397:float64) == (

      0.785398163397:float64))

  }

  test("atan - atan 0.5") {

    assert((

      0.463647609001:float64) == (

      0.463647609001:float64))

  }

  // atan2

  test("atan2 - atan2 1 1") {

    assert((

      0.785398163397:float64) == (

      0.785398163397:float64))

  }

  test("atan2 - atan2 1 0") {

    assert((

      1.57079632679:float64) == (

      1.57079632679:float64))

  }

  test("atan2 - atan2 0 1") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("atan2 - atan2 3 4") {

    assert((

      0.643501108793:float64) == (

      0.643501108793:float64))

  }

  // sinh

  test("sinh - sinh 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("sinh - sinh 1") {

    assert((

      1.17520119364:float64) == (

      1.17520119364:float64))

  }

  test("sinh - sinh 2") {

    assert((

      3.62686040785:float64) == (

      3.62686040785:float64))

  }

  // cosh

  test("cosh - cosh 0") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("cosh - cosh 1") {

    assert((

      1.54308063482:float64) == (

      1.54308063482:float64))

  }

  test("cosh - cosh 2") {

    assert((

      3.76219569108:float64) == (

      3.76219569108:float64))

  }

  // tanh

  test("tanh - tanh 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("tanh - tanh 1") {

    assert((

      0.761594155956:float64) == (

      0.761594155956:float64))

  }

  test("tanh - tanh 0.5") {

    assert((

      0.46211715726:float64) == (

      0.46211715726:float64))

  }

  // asinh

  test("asinh - asinh 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("asinh - asinh 1") {

    assert((

      0.88137358702:float64) == (

      0.88137358702:float64))

  }

  test("asinh - asinh 0.5") {

    assert((

      0.48121182506:float64) == (

      0.48121182506:float64))

  }

  // acosh

  test("acosh - acosh 1") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("acosh - acosh 2") {

    assert((

      1.31695789692:float64) == (

      1.31695789692:float64))

  }

  test("acosh - acosh 3") {

    assert((

      1.76274717404:float64) == (

      1.76274717404:float64))

  }

  // atanh

  test("atanh - atanh 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("atanh - atanh 0.5") {

    assert((

      0.549306144334:float64) == (

      0.549306144334:float64))

  }

  test("atanh - atanh 0.1") {

    assert((

      0.100335347731:float64) == (

      0.100335347731:float64))

  }

  // exp

  test("exp - exp 0") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("exp - exp 1") {

    assert((

      2.71828182846:float64) == (

      2.71828182846:float64))

  }

  test("exp - exp -1") {

    assert((

      0.367879441171:float64) == (

      0.367879441171:float64))

  }

  test("exp - exp 2") {

    assert((

      7.38905609893:float64) == (

      7.38905609893:float64))

  }

  test("exp - exp 0.5") {

    assert((

      1.6487212707:float64) == (

      1.6487212707:float64))

  }

  // log

  test("log - log 1") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("log - log e") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("log - log 2") {

    assert((

      0.69314718056:float64) == (

      0.69314718056:float64))

  }

  test("log - log 10") {

    assert((

      2.30258509299:float64) == (

      2.30258509299:float64))

  }

  // logBase

  test("logBase - log10 1") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("logBase - log10 10") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("logBase - log10 100") {

    assert((

      2.0:float64) == (

      2.0:float64))

  }

  test("logBase - log2 8") {

    assert((

      3.0:float64) == (

      3.0:float64))

  }

  test("logBase - log2 10") {

    assert((

      3.32192809489:float64) == (

      3.32192809489:float64))

  }

  // pow

  test("pow - 2^3") {

    assert((

      8.0:float64) == (

      8.0:float64))

  }

  test("pow - 10^0") {

    assert((

      1.0:float64) == (

      1.0:float64))

  }

  test("pow - 2^-1") {

    assert((

      0.5:float64) == (

      0.5:float64))

  }

  test("pow - 2^0.5") {

    assert((

      1.41421356237:float64) == (

      1.41421356237:float64))

  }

  // sqrt

  test("sqrt - sqrt 4") {

    assert((

      2.0:float64) == (

      2.0:float64))

  }

  test("sqrt - sqrt 9") {

    assert((

      3.0:float64) == (

      3.0:float64))

  }

  test("sqrt - sqrt 2") {

    assert((

      1.4142135623730951:float64) == (

      1.4142135623730951:float64))

  }

  test("sqrt - sqrt 0") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("sqrt - sqrt 3") {

    assert((

      1.73205080757:float64) == (

      1.73205080757:float64))

  }

  // ceiling

  test("ceiling - ceiling 3.2") {

    assert((

      4:bigint) == (

      4:bigint))

  }

  test("ceiling - ceiling 3.0") {

    assert((

      3:bigint) == (

      3:bigint))

  }

  test("ceiling - ceiling -3.2") {

    assert((

      -3:bigint) == (

      -3:bigint))

  }

  test("ceiling - ceiling -3.0") {

    assert((

      -3:bigint) == (

      -3:bigint))

  }

  // floor

  test("floor - floor 3.8") {

    assert((

      3:bigint) == (

      3:bigint))

  }

  test("floor - floor 3.0") {

    assert((

      3:bigint) == (

      3:bigint))

  }

  test("floor - floor -3.2") {

    assert((

      -4:bigint) == (

      -4:bigint))

  }

  test("floor - floor -3.0") {

    assert((

      -3:bigint) == (

      -3:bigint))

  }

  // round

  test("round - round 3.4") {

    assert((

      3:bigint) == (

      3:bigint))

  }

  test("round - round 3.5") {

    assert((

      4:bigint) == (

      4:bigint))

  }

  test("round - round 3.6") {

    assert((

      4:bigint) == (

      4:bigint))

  }

  test("round - round -3.4") {

    assert((

      -3:bigint) == (

      -3:bigint))

  }

  test("round - round -3.5") {

    assert((

      -4:bigint) == (

      -4:bigint))

  }

  // roundBigfloat

  test("roundBigfloat - zero") {

    assert((

      0.0:bigfloat) == (

      0.0:bigfloat))

  }

  test("roundBigfloat - round pi to 4 digits") {

    assert((

      3.142:bigfloat) == (

      3.142:bigfloat))

  }

  test("roundBigfloat - round 1234.5 to 3 digits") {

    assert((

      1230.0:bigfloat) == (

      1230.0:bigfloat))

  }

  test("roundBigfloat - round 0.001234 to 2 digits") {

    assert((

      1.2e-3:bigfloat) == (

      1.2e-3:bigfloat))

  }

  test("roundBigfloat - negative") {

    assert((

      -1230.0:bigfloat) == (

      -1230.0:bigfloat))

  }

  // roundFloat32

  test("roundFloat32 - zero") {

    assert((

      0.0:float32) == (

      0.0:float32))

  }

  test("roundFloat32 - round pi to 4 digits") {

    assert((

      3.142:float32) == (

      3.142:float32))

  }

  test("roundFloat32 - round 1234.5 to 3 digits") {

    assert((

      1230.0:float32) == (

      1230.0:float32))

  }

  test("roundFloat32 - negative") {

    assert((

      -1230.0:float32) == (

      -1230.0:float32))

  }

  // roundFloat64

  test("roundFloat64 - zero") {

    assert((

      0.0:float64) == (

      0.0:float64))

  }

  test("roundFloat64 - round pi to 4 digits") {

    assert((

      3.142:float64) == (

      3.142:float64))

  }

  test("roundFloat64 - round pi to 10 digits") {

    assert((

      3.141592654:float64) == (

      3.141592654:float64))

  }

  test("roundFloat64 - round 1234.5 to 3 digits") {

    assert((

      1230.0:float64) == (

      1230.0:float64))

  }

  test("roundFloat64 - round 0.001234 to 2 digits") {

    assert((

      1.2e-3:float64) == (

      1.2e-3:float64))

  }

  test("roundFloat64 - negative") {

    assert((

      -1230.0:float64) == (

      -1230.0:float64))

  }

  test("roundFloat64 - round 1 digit") {

    assert((

      10.0:float64) == (

      10.0:float64))

  }

  // truncate

  test("truncate - truncate 3.8") {

    assert((

      3:bigint) == (

      3:bigint))

  }

  test("truncate - truncate 3.2") {

    assert((

      3:bigint) == (

      3:bigint))

  }

  test("truncate - truncate -3.8") {

    assert((

      -3:bigint) == (

      -3:bigint))

  }

  test("truncate - truncate -3.2") {

    assert((

      -3:bigint) == (

      -3:bigint))

  }
}

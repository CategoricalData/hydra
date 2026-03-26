// Note: this is an automatically generated file. Do not edit.
// hydra.lib.math primitives

package generation.hydra.test.lib

import org.scalatest.funsuite.AnyFunSuite

class MathTest extends AnyFunSuite {

  // abs

  test("abs - positive") {

    assert((

      hydra.lib.math.abs(5)) == (

      5))

  }

  test("abs - negative") {

    assert((

      hydra.lib.math.abs(-5)) == (

      5))

  }

  test("abs - zero") {

    assert((

      hydra.lib.math.abs(0)) == (

      0))

  }

  // add

  test("add - positive numbers") {

    assert((

      hydra.lib.math.add(3)(5)) == (

      8))

  }

  test("add - negative numbers") {

    assert((

      hydra.lib.math.add(-3)(-5)) == (

      -8))

  }

  test("add - mixed sign") {

    assert((

      hydra.lib.math.add(10)(-3)) == (

      7))

  }

  test("add - with zero") {

    assert((

      hydra.lib.math.add(42)(0)) == (

      42))

  }

  // div

  test("div - exact division") {

    assert((

      hydra.lib.math.div(10)(2)) == (

      5))

  }

  test("div - truncates toward negative infinity") {

    assert((

      hydra.lib.math.div(10)(3)) == (

      3))

  }

  test("div - negative dividend") {

    assert((

      hydra.lib.math.div(-10)(3)) == (

      -4))

  }

  test("div - negative divisor") {

    assert((

      hydra.lib.math.div(10)(-3)) == (

      -4))

  }

  // even

  test("even - even positive") {

    assert((

      hydra.lib.math.even(4)) == (

      true))

  }

  test("even - odd positive") {

    assert((

      hydra.lib.math.even(5)) == (

      false))

  }

  test("even - even negative") {

    assert((

      hydra.lib.math.even(-4)) == (

      true))

  }

  test("even - odd negative") {

    assert((

      hydra.lib.math.even(-5)) == (

      false))

  }

  test("even - zero") {

    assert((

      hydra.lib.math.even(0)) == (

      true))

  }

  // max

  test("max - first is larger") {

    assert((

      hydra.lib.math.max(10)(5)) == (

      10))

  }

  test("max - second is larger") {

    assert((

      hydra.lib.math.max(5)(10)) == (

      10))

  }

  test("max - equal values") {

    assert((

      hydra.lib.math.max(7)(7)) == (

      7))

  }

  test("max - negative numbers") {

    assert((

      hydra.lib.math.max(-3)(-5)) == (

      -3))

  }

  test("max - mixed sign") {

    assert((

      hydra.lib.math.max(-5)(5)) == (

      5))

  }

  test("max - with zero") {

    assert((

      hydra.lib.math.max(0)(42)) == (

      42))

  }

  // min

  test("min - first is smaller") {

    assert((

      hydra.lib.math.min(5)(10)) == (

      5))

  }

  test("min - second is smaller") {

    assert((

      hydra.lib.math.min(10)(5)) == (

      5))

  }

  test("min - equal values") {

    assert((

      hydra.lib.math.min(7)(7)) == (

      7))

  }

  test("min - negative numbers") {

    assert((

      hydra.lib.math.min(-3)(-5)) == (

      -5))

  }

  test("min - mixed sign") {

    assert((

      hydra.lib.math.min(-5)(5)) == (

      -5))

  }

  test("min - with zero") {

    assert((

      hydra.lib.math.min(0)(42)) == (

      0))

  }

  // mod

  test("mod - basic modulo") {

    assert((

      hydra.lib.math.mod(10)(3)) == (

      1))

  }

  test("mod - exact division") {

    assert((

      hydra.lib.math.mod(10)(2)) == (

      0))

  }

  test("mod - negative dividend") {

    assert((

      hydra.lib.math.mod(-10)(3)) == (

      2))

  }

  test("mod - negative divisor") {

    assert((

      hydra.lib.math.mod(10)(-3)) == (

      -2))

  }

  // mul

  test("mul - positive numbers") {

    assert((

      hydra.lib.math.mul(3)(5)) == (

      15))

  }

  test("mul - negative numbers") {

    assert((

      hydra.lib.math.mul(-3)(-5)) == (

      15))

  }

  test("mul - mixed sign") {

    assert((

      hydra.lib.math.mul(3)(-5)) == (

      -15))

  }

  test("mul - with zero") {

    assert((

      hydra.lib.math.mul(42)(0)) == (

      0))

  }

  test("mul - with one") {

    assert((

      hydra.lib.math.mul(42)(1)) == (

      42))

  }

  // negate

  test("negate - positive") {

    assert((

      hydra.lib.math.negate(5)) == (

      -5))

  }

  test("negate - negative") {

    assert((

      hydra.lib.math.negate(-5)) == (

      5))

  }

  test("negate - zero") {

    assert((

      hydra.lib.math.negate(0)) == (

      0))

  }

  // odd

  test("odd - odd positive") {

    assert((

      hydra.lib.math.odd(5)) == (

      true))

  }

  test("odd - even positive") {

    assert((

      hydra.lib.math.odd(4)) == (

      false))

  }

  test("odd - odd negative") {

    assert((

      hydra.lib.math.odd(-5)) == (

      true))

  }

  test("odd - even negative") {

    assert((

      hydra.lib.math.odd(-4)) == (

      false))

  }

  test("odd - zero") {

    assert((

      hydra.lib.math.odd(0)) == (

      false))

  }

  // pred

  test("pred - positive") {

    assert((

      hydra.lib.math.pred(5)) == (

      4))

  }

  test("pred - zero") {

    assert((

      hydra.lib.math.pred(0)) == (

      -1))

  }

  test("pred - negative") {

    assert((

      hydra.lib.math.pred(-5)) == (

      -6))

  }

  // range

  test("range - ascending range") {

    assert((

      hydra.lib.math.range(1)(5)) == (

      Seq(1, 2, 3, 4, 5)))

  }

  test("range - single element") {

    assert((

      hydra.lib.math.range(5)(5)) == (

      Seq(5)))

  }

  test("range - two elements") {

    assert((

      hydra.lib.math.range(3)(4)) == (

      Seq(3, 4)))

  }

  test("range - negative start") {

    assert((

      hydra.lib.math.range(-2)(2)) == (

      Seq(-2, -1, 0, 1, 2)))

  }

  // rem

  test("rem - basic remainder") {

    assert((

      hydra.lib.math.rem(10)(3)) == (

      1))

  }

  test("rem - exact division") {

    assert((

      hydra.lib.math.rem(10)(2)) == (

      0))

  }

  test("rem - negative dividend") {

    assert((

      hydra.lib.math.rem(-10)(3)) == (

      -1))

  }

  test("rem - negative divisor") {

    assert((

      hydra.lib.math.rem(10)(-3)) == (

      1))

  }

  // signum

  test("signum - positive") {

    assert((

      hydra.lib.math.signum(5)) == (

      1))

  }

  test("signum - negative") {

    assert((

      hydra.lib.math.signum(-5)) == (

      -1))

  }

  test("signum - zero") {

    assert((

      hydra.lib.math.signum(0)) == (

      0))

  }

  // sub

  test("sub - positive numbers") {

    assert((

      hydra.lib.math.sub(10)(3)) == (

      7))

  }

  test("sub - negative numbers") {

    assert((

      hydra.lib.math.sub(-10)(-3)) == (

      -7))

  }

  test("sub - mixed sign") {

    assert((

      hydra.lib.math.sub(10)(-3)) == (

      13))

  }

  test("sub - with zero") {

    assert((

      hydra.lib.math.sub(42)(0)) == (

      42))

  }

  // succ

  test("succ - positive") {

    assert((

      hydra.lib.math.succ(5)) == (

      6))

  }

  test("succ - zero") {

    assert((

      hydra.lib.math.succ(0)) == (

      1))

  }

  test("succ - negative") {

    assert((

      hydra.lib.math.succ(-5)) == (

      -4))

  }

  // e

  test("e - Euler's number") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.e)) - (2.71828182846)) <= 1e-15)

  }

  // pi

  test("pi - pi constant") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.pi)) - (3.14159265359)) <= 1e-15)

  }

  // sin

  test("sin - sin 0") {

    assert(math.abs((hydra.lib.math.sin(0.0)) - (0.0)) <= 1e-15)

  }

  test("sin - sin pi/2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sin(1.5707963267948966))) - (1.0)) <= 1e-15)

  }

  test("sin - sin pi") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sin(3.141592653589793))) - (1.22464679915e-16)) <= 1e-15)

  }

  test("sin - sin 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sin(1.0))) - (0.841470984808)) <= 1e-15)

  }

  test("sin - sin 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sin(0.5))) - (0.479425538604)) <= 1e-15)

  }

  // cos

  test("cos - cos 0") {

    assert(math.abs((hydra.lib.math.cos(0.0)) - (1.0)) <= 1e-15)

  }

  test("cos - cos pi/2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.cos(1.5707963267948966))) - (6.12323399574e-17)) <= 1e-15)

  }

  test("cos - cos pi") {

    assert(math.abs((hydra.lib.math.cos(3.141592653589793)) - (-1.0)) <= 1e-15)

  }

  test("cos - cos 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.cos(1.0))) - (0.540302305868)) <= 1e-15)

  }

  test("cos - cos 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.cos(0.5))) - (0.87758256189)) <= 1e-15)

  }

  // tan

  test("tan - tan 0") {

    assert(math.abs((hydra.lib.math.tan(0.0)) - (0.0)) <= 1e-15)

  }

  test("tan - tan pi/4") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.tan(0.7853981633974483))) - (1.0)) <= 1e-15)

  }

  test("tan - tan 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.tan(1.0))) - (1.55740772465)) <= 1e-15)

  }

  test("tan - tan 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.tan(0.5))) - (0.546302489844)) <= 1e-15)

  }

  // asin

  test("asin - asin 0") {

    assert(math.abs((hydra.lib.math.asin(0.0)) - (0.0)) <= 1e-15)

  }

  test("asin - asin 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.asin(1.0))) - (1.57079632679)) <= 1e-15)

  }

  test("asin - asin -1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.asin(-1.0))) - (-1.57079632679)) <= 1e-15)

  }

  test("asin - asin 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.asin(0.5))) - (0.523598775598)) <= 1e-15)

  }

  // acos

  test("acos - acos 1") {

    assert(math.abs((hydra.lib.math.acos(1.0)) - (0.0)) <= 1e-15)

  }

  test("acos - acos 0") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.acos(0.0))) - (1.57079632679)) <= 1e-15)

  }

  test("acos - acos -1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.acos(-1.0))) - (3.14159265359)) <= 1e-15)

  }

  test("acos - acos 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.acos(0.5))) - (1.0471975512)) <= 1e-15)

  }

  // atan

  test("atan - atan 0") {

    assert(math.abs((hydra.lib.math.atan(0.0)) - (0.0)) <= 1e-15)

  }

  test("atan - atan 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atan(1.0))) - (0.785398163397)) <= 1e-15)

  }

  test("atan - atan 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atan(0.5))) - (0.463647609001)) <= 1e-15)

  }

  // atan2

  test("atan2 - atan2 1 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atan2(1.0)(1.0))) - (0.785398163397)) <= 1e-15)

  }

  test("atan2 - atan2 1 0") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atan2(1.0)(0.0))) - (1.57079632679)) <= 1e-15)

  }

  test("atan2 - atan2 0 1") {

    assert(math.abs((hydra.lib.math.atan2(0.0)(1.0)) - (0.0)) <= 1e-15)

  }

  test("atan2 - atan2 3 4") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atan2(3.0)(4.0))) - (0.643501108793)) <= 1e-15)

  }

  // sinh

  test("sinh - sinh 0") {

    assert(math.abs((hydra.lib.math.sinh(0.0)) - (0.0)) <= 1e-15)

  }

  test("sinh - sinh 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sinh(1.0))) - (1.17520119364)) <= 1e-15)

  }

  test("sinh - sinh 2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sinh(2.0))) - (3.62686040785)) <= 1e-15)

  }

  // cosh

  test("cosh - cosh 0") {

    assert(math.abs((hydra.lib.math.cosh(0.0)) - (1.0)) <= 1e-15)

  }

  test("cosh - cosh 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.cosh(1.0))) - (1.54308063482)) <= 1e-15)

  }

  test("cosh - cosh 2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.cosh(2.0))) - (3.76219569108)) <= 1e-15)

  }

  // tanh

  test("tanh - tanh 0") {

    assert(math.abs((hydra.lib.math.tanh(0.0)) - (0.0)) <= 1e-15)

  }

  test("tanh - tanh 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.tanh(1.0))) - (0.761594155956)) <= 1e-15)

  }

  test("tanh - tanh 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.tanh(0.5))) - (0.46211715726)) <= 1e-15)

  }

  // asinh

  test("asinh - asinh 0") {

    assert(math.abs((hydra.lib.math.asinh(0.0)) - (0.0)) <= 1e-15)

  }

  test("asinh - asinh 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.asinh(1.0))) - (0.88137358702)) <= 1e-15)

  }

  test("asinh - asinh 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.asinh(0.5))) - (0.48121182506)) <= 1e-15)

  }

  // acosh

  test("acosh - acosh 1") {

    assert(math.abs((hydra.lib.math.acosh(1.0)) - (0.0)) <= 1e-15)

  }

  test("acosh - acosh 2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.acosh(2.0))) - (1.31695789692)) <= 1e-15)

  }

  test("acosh - acosh 3") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.acosh(3.0))) - (1.76274717404)) <= 1e-15)

  }

  // atanh

  test("atanh - atanh 0") {

    assert(math.abs((hydra.lib.math.atanh(0.0)) - (0.0)) <= 1e-15)

  }

  test("atanh - atanh 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atanh(0.5))) - (0.549306144334)) <= 1e-15)

  }

  test("atanh - atanh 0.1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.atanh(0.1))) - (0.100335347731)) <= 1e-15)

  }

  // exp

  test("exp - exp 0") {

    assert(math.abs((hydra.lib.math.exp(0.0)) - (1.0)) <= 1e-15)

  }

  test("exp - exp 1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.exp(1.0))) - (2.71828182846)) <= 1e-15)

  }

  test("exp - exp -1") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.exp(-1.0))) - (0.367879441171)) <= 1e-15)

  }

  test("exp - exp 2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.exp(2.0))) - (7.38905609893)) <= 1e-15)

  }

  test("exp - exp 0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.exp(0.5))) - (1.6487212707)) <= 1e-15)

  }

  // log

  test("log - log 1") {

    assert(math.abs((hydra.lib.math.log(1.0)) - (0.0)) <= 1e-15)

  }

  test("log - log e") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.log(2.718281828459045))) - (1.0)) <= 1e-15)

  }

  test("log - log 2") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.log(2.0))) - (0.69314718056)) <= 1e-15)

  }

  test("log - log 10") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.log(10.0))) - (2.30258509299)) <= 1e-15)

  }

  // logBase

  test("logBase - log10 1") {

    assert(math.abs((hydra.lib.math.logBase(10.0)(1.0)) - (0.0)) <= 1e-15)

  }

  test("logBase - log10 10") {

    assert(math.abs((hydra.lib.math.logBase(10.0)(10.0)) - (1.0)) <= 1e-15)

  }

  test("logBase - log10 100") {

    assert(math.abs((hydra.lib.math.logBase(10.0)(100.0)) - (2.0)) <= 1e-15)

  }

  test("logBase - log2 8") {

    assert(math.abs((hydra.lib.math.logBase(2.0)(8.0)) - (3.0)) <= 1e-15)

  }

  test("logBase - log2 10") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.logBase(2.0)(10.0))) - (3.32192809489)) <= 1e-15)

  }

  // pow

  test("pow - 2^3") {

    assert(math.abs((hydra.lib.math.pow(2.0)(3.0)) - (8.0)) <= 1e-15)

  }

  test("pow - 10^0") {

    assert(math.abs((hydra.lib.math.pow(10.0)(0.0)) - (1.0)) <= 1e-15)

  }

  test("pow - 2^-1") {

    assert(math.abs((hydra.lib.math.pow(2.0)(-1.0)) - (0.5)) <= 1e-15)

  }

  test("pow - 2^0.5") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.pow(2.0)(0.5))) - (1.41421356237)) <= 1e-15)

  }

  // sqrt

  test("sqrt - sqrt 4") {

    assert(math.abs((hydra.lib.math.sqrt(4.0)) - (2.0)) <= 1e-15)

  }

  test("sqrt - sqrt 9") {

    assert(math.abs((hydra.lib.math.sqrt(9.0)) - (3.0)) <= 1e-15)

  }

  test("sqrt - sqrt 2") {

    assert(math.abs((hydra.lib.math.sqrt(2.0)) - (1.4142135623730951)) <= 1e-15)

  }

  test("sqrt - sqrt 0") {

    assert(math.abs((hydra.lib.math.sqrt(0.0)) - (0.0)) <= 1e-15)

  }

  test("sqrt - sqrt 3") {

    assert(math.abs((hydra.lib.math.roundFloat64(12)(hydra.lib.math.sqrt(3.0))) - (1.73205080757)) <= 1e-15)

  }

  // ceiling

  test("ceiling - ceiling 3.2") {

    assert((

      hydra.lib.math.ceiling(3.2)) == (

      BigInt("4")))

  }

  test("ceiling - ceiling 3.0") {

    assert((

      hydra.lib.math.ceiling(3.0)) == (

      BigInt("3")))

  }

  test("ceiling - ceiling -3.2") {

    assert((

      hydra.lib.math.ceiling(-3.2)) == (

      BigInt("-3")))

  }

  test("ceiling - ceiling -3.0") {

    assert((

      hydra.lib.math.ceiling(-3.0)) == (

      BigInt("-3")))

  }

  // floor

  test("floor - floor 3.8") {

    assert((

      hydra.lib.math.floor(3.8)) == (

      BigInt("3")))

  }

  test("floor - floor 3.0") {

    assert((

      hydra.lib.math.floor(3.0)) == (

      BigInt("3")))

  }

  test("floor - floor -3.2") {

    assert((

      hydra.lib.math.floor(-3.2)) == (

      BigInt("-4")))

  }

  test("floor - floor -3.0") {

    assert((

      hydra.lib.math.floor(-3.0)) == (

      BigInt("-3")))

  }

  // round

  test("round - round 3.4") {

    assert((

      hydra.lib.math.round(3.4)) == (

      BigInt("3")))

  }

  test("round - round 3.5") {

    assert((

      hydra.lib.math.round(3.5)) == (

      BigInt("4")))

  }

  test("round - round 3.6") {

    assert((

      hydra.lib.math.round(3.6)) == (

      BigInt("4")))

  }

  test("round - round -3.4") {

    assert((

      hydra.lib.math.round(-3.4)) == (

      BigInt("-3")))

  }

  test("round - round -3.5") {

    assert((

      hydra.lib.math.round(-3.5)) == (

      BigInt("-4")))

  }

  // roundBigfloat

  test("roundBigfloat - zero") {

    assert((BigDecimal(0.0)).compare(hydra.lib.math.roundBigfloat(5)(BigDecimal(0.0))) == 0)

  }

  test("roundBigfloat - round pi to 4 digits") {

    assert((BigDecimal(3.142)).compare(hydra.lib.math.roundBigfloat(4)(BigDecimal(3.141592653589793))) == 0)

  }

  test("roundBigfloat - round 1234.5 to 3 digits") {

    assert((BigDecimal(1230.0)).compare(hydra.lib.math.roundBigfloat(3)(BigDecimal(1234.5))) == 0)

  }

  test("roundBigfloat - round 0.001234 to 2 digits") {

    assert((BigDecimal(1.2e-3)).compare(hydra.lib.math.roundBigfloat(2)(BigDecimal(1.234e-3))) == 0)

  }

  test("roundBigfloat - negative") {

    assert((BigDecimal(-1230.0)).compare(hydra.lib.math.roundBigfloat(3)(BigDecimal(-1234.5))) == 0)

  }

  // roundFloat32

  test("roundFloat32 - zero") {

    assert(math.abs((hydra.lib.math.roundFloat32(5)(0.0f)) - (0.0f)) <= 1e-15)

  }

  test("roundFloat32 - round pi to 4 digits") {

    assert(math.abs((hydra.lib.math.roundFloat32(4)(3.1415927f)) - (3.142f)) <= 1e-15)

  }

  test("roundFloat32 - round 1234.5 to 3 digits") {

    assert(math.abs((hydra.lib.math.roundFloat32(3)(1234.5f)) - (1230.0f)) <= 1e-15)

  }

  test("roundFloat32 - negative") {

    assert(math.abs((hydra.lib.math.roundFloat32(3)(-1234.5f)) - (-1230.0f)) <= 1e-15)

  }

  // roundFloat64

  test("roundFloat64 - zero") {

    assert(math.abs((hydra.lib.math.roundFloat64(5)(0.0)) - (0.0)) <= 1e-15)

  }

  test("roundFloat64 - round pi to 4 digits") {

    assert(math.abs((hydra.lib.math.roundFloat64(4)(3.141592653589793)) - (3.142)) <= 1e-15)

  }

  test("roundFloat64 - round pi to 10 digits") {

    assert(math.abs((hydra.lib.math.roundFloat64(10)(3.141592653589793)) - (3.141592654)) <= 1e-15)

  }

  test("roundFloat64 - round 1234.5 to 3 digits") {

    assert(math.abs((hydra.lib.math.roundFloat64(3)(1234.5)) - (1230.0)) <= 1e-15)

  }

  test("roundFloat64 - round 0.001234 to 2 digits") {

    assert(math.abs((hydra.lib.math.roundFloat64(2)(1.234e-3)) - (1.2e-3)) <= 1e-15)

  }

  test("roundFloat64 - negative") {

    assert(math.abs((hydra.lib.math.roundFloat64(3)(-1234.5)) - (-1230.0)) <= 1e-15)

  }

  test("roundFloat64 - round 1 digit") {

    assert(math.abs((hydra.lib.math.roundFloat64(1)(9.876)) - (10.0)) <= 1e-15)

  }

  // truncate

  test("truncate - truncate 3.8") {

    assert((

      hydra.lib.math.truncate(3.8)) == (

      BigInt("3")))

  }

  test("truncate - truncate 3.2") {

    assert((

      hydra.lib.math.truncate(3.2)) == (

      BigInt("3")))

  }

  test("truncate - truncate -3.8") {

    assert((

      hydra.lib.math.truncate(-3.8)) == (

      BigInt("-3")))

  }

  test("truncate - truncate -3.2") {

    assert((

      hydra.lib.math.truncate(-3.2)) == (

      BigInt("-3")))

  }
}

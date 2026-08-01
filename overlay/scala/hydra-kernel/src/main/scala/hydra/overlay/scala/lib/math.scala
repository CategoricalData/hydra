package hydra.overlay.scala.lib

object math:
  // Constraint-polymorphic ('numeric'/'integral'/'fractional') primitives: the type scheme
  // carries the class constraint (`numeric x => x -> x -> x`, `integral x => x -> x -> optional
  // x`, `fractional x => x -> x -> x`) for inference only, so the generated call site is generic
  // (e.g. `math.add[A](x)(y)`) with no evidence threaded from the caller. numeric (add/sub/mul/
  // negate/abs/signum) dispatches on the boxed representation via NumericDispatch, mirroring
  // Java's NumericDispatch.applyNativeBinary. fractional (divide) is unambiguous (float32/
  // float64 only) and implemented directly below.
  //
  // integral (div/mod/rem/even/odd) dispatches via IntegralDispatch, EXCEPT for a known
  // structural gap: uint8/16/32/64's boxed representation collides with a narrower signed type
  // (uint8->Byte collides with int8, uint16->Int collides with int32, uint32->Long collides with
  // int64), so a generic instanceof-style dispatch cannot recover the intended width/signedness
  // for THOSE FOUR TYPES once erased -- the same structural gap documented in Java's
  // NumericDispatch for uint64/bigint. This IS exercised by generated code (confirmed the hard
  // way: json/writer.scala's hex-nibble encoder calls math.div/math.mod on plain Int, i.e.
  // int32, which is unambiguous), so div/mod/rem/even/odd must exist and work correctly for the
  // unambiguous cases (int8/16/32/64, bigint); IntegralDispatch throws loudly if it ever receives
  // an ambiguous uint8/16/32 value rather than silently misinterpreting it as the colliding
  // signed type.
  def abs[A](x: A): A = NumericDispatch.applyNativeUnary("abs", x)
  def acos(x: Double): Double = scala.math.acos(x)
  def acosh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x - 1))
  def add[A](x: A)(y: A): A = NumericDispatch.applyNativeBinary("add", x, y)
  def addFloat64(x: Double)(y: Double): Double = x + y
  def asin(x: Double): Double = scala.math.asin(x)
  def asinh(x: Double): Double =
    // Special-case infinities: asinh(±Inf) = ±Inf (naive formula gives NaN for -Inf).
    if x.isInfinite then x else scala.math.log(x + scala.math.sqrt(x * x + 1))
  def atan(x: Double): Double = scala.math.atan(x)
  def atan2(y: Double)(x: Double): Double =
    // Match Haskell: atan2 returns NaN when both arguments are infinite
    // (Scala/Java's atan2 returns ±pi/4 or ±3pi/4 in these cases).
    if y.isInfinite && x.isInfinite then Double.NaN else scala.math.atan2(y, x)
  def atanh(x: Double): Double = 0.5 * scala.math.log((1 + x) / (1 - x))
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt, so that NaN/Inf
  // propagate naturally per IEEE 754 (matching C, Java, Go, Rust, JavaScript).
  def ceiling(x: Double): Double =
    if x.isNaN || x.isInfinite then x else scala.math.ceil(x)
  def cos(x: Double): Double = scala.math.cos(x)
  def cosh(x: Double): Double = scala.math.cosh(x)
  def div[A](x: A)(y: A): Option[A] = IntegralDispatch.applyNativeDiv(x, y)
  def divide[A](x: A)(y: A): A =
    (x.asInstanceOf[Any], y.asInstanceOf[Any]) match
      case (a: Double, b: Double) => (a / b).asInstanceOf[A]
      case (a: Float, b: Float) => (a / b).asInstanceOf[A]
      case (a, _) => throw new RuntimeException(s"hydra.lib.math.divide: operand is not fractional: $a")
  def e: Double = scala.math.E
  def even[A](x: A): Boolean = IntegralDispatch.applyNativeEven(x)
  def exp(x: Double): Double = scala.math.exp(x)
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt (see ceiling).
  def floor(x: Double): Double =
    if x.isNaN || x.isInfinite then x else scala.math.floor(x)
  def log(x: Double): Double = scala.math.log(x)
  def logBase(base: Double)(x: Double): Double = scala.math.log(x) / scala.math.log(base)
  def max(x: Int)(y: Int): Int = scala.math.max(x, y)
  def maybePred(x: Int): Option[Int] = if x == -2147483648 then None else Some(x - 1)
  def maybeSucc(x: Int): Option[Int] = if x == 2147483647 then None else Some(x + 1)
  def min(x: Int)(y: Int): Int = scala.math.min(x, y)
  def mod[A](x: A)(y: A): Option[A] = IntegralDispatch.applyNativeMod(x, y)
  def mul[A](x: A)(y: A): A = NumericDispatch.applyNativeBinary("mul", x, y)
  def mulFloat64(x: Double)(y: Double): Double = x * y
  def negate[A](x: A): A = NumericDispatch.applyNativeUnary("negate", x)
  def negateFloat64(x: Double): Double = -x
  def odd[A](x: A): Boolean = IntegralDispatch.applyNativeOdd(x)
  def pi: Double = scala.math.Pi
  def pow(base: Double)(exp: Double): Double = scala.math.pow(base, exp)
  def range(start: Int)(end: Int): Seq[Int] = (start to end).toSeq
  def rem[A](x: A)(y: A): Option[A] = IntegralDispatch.applyNativeRem(x, y)
  // Haskell uses half-even (banker's) rounding
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt (see ceiling).
  def round(x: Double): Double =
    if x.isNaN || x.isInfinite then x
    else BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toDouble
  // Round to n significant digits (not decimal places)
  // Returns NaN/Inf inputs unchanged (no rounding is possible).
  def roundFloat(precision: Int)(x: Double): Double =
    if x.isNaN || x.isInfinite then x
    else if x == 0 then 0.0
    else
      val factor = scala.math.pow(10, precision - 1 - scala.math.floor(scala.math.log10(scala.math.abs(x))))
      scala.math.round(x * factor) / factor
  def roundFloat32(precision: Int)(x: Float): Float =
    if x.isNaN || x.isInfinite then x
    else roundFloat(precision)(x.toDouble).toFloat
  def roundFloat64(precision: Int)(x: Double): Double =
    roundFloat(precision)(x)
  def signum[A](x: A): A = NumericDispatch.applyNativeUnary("signum", x)
  def sin(x: Double): Double = scala.math.sin(x)
  def sinh(x: Double): Double = scala.math.sinh(x)
  def sqrt(x: Double): Double = scala.math.sqrt(x)
  def sub[A](x: A)(y: A): A = NumericDispatch.applyNativeBinary("sub", x, y)
  def subFloat64(x: Double)(y: Double): Double = x - y
  def tan(x: Double): Double = scala.math.tan(x)
  def tanh(x: Double): Double = scala.math.tanh(x)
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt (see ceiling).
  def truncate(x: Double): Double =
    if x.isNaN || x.isInfinite then x
    else if x >= 0 then scala.math.floor(x)
    else scala.math.ceil(x)

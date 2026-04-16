package hydra.lib

object math:
  def abs(x: Int): Int = scala.math.abs(x)
  def acos(x: Double): Double = scala.math.acos(x)
  def acosh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x - 1))
  def add(x: Int)(y: Int): Int = x + y
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
  def e: Double = scala.math.E
  def even(x: Int): Boolean = x % 2 == 0
  def exp(x: Double): Double = scala.math.exp(x)
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt (see ceiling).
  def floor(x: Double): Double =
    if x.isNaN || x.isInfinite then x else scala.math.floor(x)
  def log(x: Double): Double = scala.math.log(x)
  def logBase(base: Double)(x: Double): Double = scala.math.log(x) / scala.math.log(base)
  def max(x: Int)(y: Int): Int = scala.math.max(x, y)
  def maybeDiv(x: Int)(y: Int): Option[Int] = if y == 0 then None else Some(Math.floorDiv(x, y))
  def maybeMod(x: Int)(y: Int): Option[Int] = if y == 0 then None else Some(Math.floorMod(x, y))
  def maybePred(x: Int): Option[Int] = if x == -2147483648 then None else Some(x - 1)
  def maybeRem(x: Int)(y: Int): Option[Int] = if y == 0 then None else Some(x % y)
  def maybeSucc(x: Int): Option[Int] = if x == 2147483647 then None else Some(x + 1)
  def min(x: Int)(y: Int): Int = scala.math.min(x, y)
  def mul(x: Int)(y: Int): Int = x * y
  def mulFloat64(x: Double)(y: Double): Double = x * y
  def negate(x: Int): Int = -x
  def negateFloat64(x: Double): Double = -x
  def odd(x: Int): Boolean = x % 2 != 0
  def pi: Double = scala.math.Pi
  def pow(base: Double)(exp: Double): Double = scala.math.pow(base, exp)
  def range(start: Int)(end: Int): Seq[Int] = (start to end).toSeq
  // Haskell uses half-even (banker's) rounding
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt (see ceiling).
  def round(x: Double): Double =
    if x.isNaN || x.isInfinite then x
    else BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toDouble
  // Round to n significant digits (not decimal places)
  // Returns NaN/Inf inputs unchanged (no rounding is possible).
  def roundBigfloat(precision: Int)(x: BigDecimal): BigDecimal =
    if x == 0 then BigDecimal(0.0)
    else
      val d = x.toDouble
      val factor = scala.math.pow(10, precision - 1 - scala.math.floor(scala.math.log10(scala.math.abs(d))))
      BigDecimal(scala.math.round(d * factor) / factor)
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
  def signum(x: Int): Int = x.sign
  def sin(x: Double): Double = scala.math.sin(x)
  def sinh(x: Double): Double = scala.math.sinh(x)
  def sqrt(x: Double): Double = scala.math.sqrt(x)
  def sub(x: Int)(y: Int): Int = x - y
  def subFloat64(x: Double)(y: Double): Double = x - y
  def tan(x: Double): Double = scala.math.tan(x)
  def tanh(x: Double): Double = scala.math.tanh(x)
  // DIVERGENCE FROM HASKELL: returns a Double, not a BigInt (see ceiling).
  def truncate(x: Double): Double =
    if x.isNaN || x.isInfinite then x
    else if x >= 0 then scala.math.floor(x)
    else scala.math.ceil(x)

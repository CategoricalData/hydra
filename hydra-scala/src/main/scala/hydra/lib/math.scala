package hydra.lib

object math:
  def abs(x: Int): Int = scala.math.abs(x)
  def acos(x: Double): Double = scala.math.acos(x)
  def acosh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x - 1))
  def add(x: Int)(y: Int): Int = x + y
  def asin(x: Double): Double = scala.math.asin(x)
  def asinh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x + 1))
  def atan(x: Double): Double = scala.math.atan(x)
  def atan2(y: Double)(x: Double): Double = scala.math.atan2(y, x)
  def atanh(x: Double): Double = 0.5 * scala.math.log((1 + x) / (1 - x))
  def ceiling(x: Double): BigInt = BigDecimal(scala.math.ceil(x)).toBigInt
  def cos(x: Double): Double = scala.math.cos(x)
  def cosh(x: Double): Double = scala.math.cosh(x)
  def div(x: Int)(y: Int): Int = if y == 0 then 0 else Math.floorDiv(x, y)
  def e: Double = scala.math.E
  def even(x: Int): Boolean = x % 2 == 0
  def exp(x: Double): Double = scala.math.exp(x)
  def floor(x: Double): BigInt = BigDecimal(scala.math.floor(x)).toBigInt
  def log(x: Double): Double = scala.math.log(x)
  def logBase(base: Double)(x: Double): Double = scala.math.log(x) / scala.math.log(base)
  def max(x: Int)(y: Int): Int = scala.math.max(x, y)
  def min(x: Int)(y: Int): Int = scala.math.min(x, y)
  def mod(x: Int)(y: Int): Int = if y == 0 then 0 else Math.floorMod(x, y)
  def mul(x: Int)(y: Int): Int = x * y
  def negate(x: Int): Int = -x
  def odd(x: Int): Boolean = x % 2 != 0
  def pi: Double = scala.math.Pi
  def pow(base: Double)(exp: Double): Double = scala.math.pow(base, exp)
  def pred(x: Int): Int = x - 1
  def range(start: Int)(end: Int): Seq[Int] = (start to end).toSeq
  def rem(x: Int)(y: Int): Int = if y == 0 then 0 else x % y
  // Haskell uses half-even (banker's) rounding
  def round(x: Double): BigInt = BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt
  // Round to n significant digits (not decimal places)
  def roundBigfloat(precision: Int)(x: BigDecimal): BigDecimal =
    if x == 0 then BigDecimal(0.0)
    else
      val d = x.toDouble
      val factor = scala.math.pow(10, precision - 1 - scala.math.floor(scala.math.log10(scala.math.abs(d))))
      BigDecimal(scala.math.round(d * factor) / factor)
  def roundFloat(precision: Int)(x: Double): Double =
    if x == 0 then 0.0
    else
      val factor = scala.math.pow(10, precision - 1 - scala.math.floor(scala.math.log10(scala.math.abs(x))))
      scala.math.round(x * factor) / factor
  def roundFloat32(precision: Int)(x: Float): Float =
    roundFloat(precision)(x.toDouble).toFloat
  def roundFloat64(precision: Int)(x: Double): Double =
    roundFloat(precision)(x)
  def signum(x: Int): Int = x.sign
  def sin(x: Double): Double = scala.math.sin(x)
  def sinh(x: Double): Double = scala.math.sinh(x)
  def sqrt(x: Double): Double = scala.math.sqrt(x)
  def sub(x: Int)(y: Int): Int = x - y
  def succ(x: Int): Int = x + 1
  def tan(x: Double): Double = scala.math.tan(x)
  def tanh(x: Double): Double = scala.math.tanh(x)
  def truncate(x: Double): BigInt = BigDecimal(x).toBigInt

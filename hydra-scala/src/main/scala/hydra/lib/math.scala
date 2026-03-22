package hydra.lib

object math:
  def abs(x: Int): Int = scala.math.abs(x)
  def acos(x: Double): Double = scala.math.acos(x)
  def acosh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x - 1))
  def add(x: Int)(y: Int): Int = x + y
  def asin(x: Double): Double = scala.math.asin(x)
  def asinh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x + 1))
  def atan(x: Double): Double = scala.math.atan(x)
  def atanh(x: Double): Double = 0.5 * scala.math.log((1 + x) / (1 - x))
  def ceiling(x: Double): Int = scala.math.ceil(x).toInt
  def cos(x: Double): Double = scala.math.cos(x)
  def cosh(x: Double): Double = scala.math.cosh(x)
  def div(x: Int)(y: Int): Int = if y == 0 then 0 else Math.floorDiv(x, y)
  def e: Double = scala.math.E
  def even(x: Int): Boolean = x % 2 == 0
  def exp(x: Double): Double = scala.math.exp(x)
  def floor(x: Double): Int = scala.math.floor(x).toInt
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
  def round(x: Double): Int = scala.math.round(x).toInt
  def roundBigfloat(precision: Int)(x: Double): Double = BigDecimal(x).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble
  def roundFloat(precision: Int)(x: Double): Double = BigDecimal(x).setScale(precision, BigDecimal.RoundingMode.HALF_UP).toDouble
  def signum(x: Int): Int = x.sign
  def sin(x: Double): Double = scala.math.sin(x)
  def sinh(x: Double): Double = scala.math.sinh(x)
  def sqrt(x: Double): Double = scala.math.sqrt(x)
  def sub(x: Int)(y: Int): Int = x - y
  def succ(x: Int): Int = x + 1
  def tan(x: Double): Double = scala.math.tan(x)
  def tanh(x: Double): Double = scala.math.tanh(x)
  def truncate(x: Double): Int = x.toInt

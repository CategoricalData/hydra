package hydra.lib

object math:
  /** Return the absolute value. */
  def abs(x: Int): Int = scala.math.abs(x)

  /** Return the arc cosine of x in radians. */
  def acos(x: Double): Double = scala.math.acos(x)

  /** Return the inverse hyperbolic cosine of x. */
  def acosh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x - 1))

  /** Add two numbers. */
  def add(x: Int)(y: Int): Int = x + y

  /** Return the arc sine of x in radians. */
  def asin(x: Double): Double = scala.math.asin(x)

  /** Return the inverse hyperbolic sine of x. */
  def asinh(x: Double): Double = scala.math.log(x + scala.math.sqrt(x * x + 1))

  /** Return the arc tangent of x in radians. */
  def atan(x: Double): Double = scala.math.atan(x)

  /** Return the arc tangent of y/x in radians, using signs to determine quadrant. */
  def atan2(y: Double)(x: Double): Double = scala.math.atan2(y, x)

  /** Return the inverse hyperbolic tangent of x. */
  def atanh(x: Double): Double = 0.5 * scala.math.log((1 + x) / (1 - x))

  /** Return the ceiling of x as an integer. */
  def ceiling(x: Double): BigInt = BigDecimal(scala.math.ceil(x)).toBigInt

  /** Return the cosine of x radians. */
  def cos(x: Double): Double = scala.math.cos(x)

  /** Return the hyperbolic cosine of x. */
  def cosh(x: Double): Double = scala.math.cosh(x)

  /** Divide two integers using integer division. */
  def div(x: Int)(y: Int): Int = if y == 0 then 0 else Math.floorDiv(x, y)

  /** Euler's number (e ≈ 2.71828). */
  def e: Double = scala.math.E

  /** Check if an integer is even. */
  def even(x: Int): Boolean = x % 2 == 0

  /** Return e raised to the power x. */
  def exp(x: Double): Double = scala.math.exp(x)

  /** Return the floor of x as an integer. */
  def floor(x: Double): BigInt = BigDecimal(scala.math.floor(x)).toBigInt

  /** Return the natural logarithm of x. */
  def log(x: Double): Double = scala.math.log(x)

  /** Return the logarithm of x to the given base. */
  def logBase(base: Double)(x: Double): Double = scala.math.log(x) / scala.math.log(base)

  /** Return the maximum of two values. */
  def max(x: Int)(y: Int): Int = scala.math.max(x, y)

  /** Return the minimum of two values. */
  def min(x: Int)(y: Int): Int = scala.math.min(x, y)

  /** Mathematical modulo. */
  def mod(x: Int)(y: Int): Int = if y == 0 then 0 else Math.floorMod(x, y)

  /** Multiply two numbers. */
  def mul(x: Int)(y: Int): Int = x * y

  /** Negate a number. */
  def negate(x: Int): Int = -x

  /** Check if an integer is odd. */
  def odd(x: Int): Boolean = x % 2 != 0

  /** Pi (π ≈ 3.14159). */
  def pi: Double = scala.math.Pi

  /** Return x raised to the power y. */
  def pow(base: Double)(exp: Double): Double = scala.math.pow(base, exp)

  /** Return the predecessor (x - 1). */
  def pred(x: Int): Int = x - 1

  /** Generate a range of values from start to end (inclusive). */
  def range(start: Int)(end: Int): Seq[Int] = (start to end).toSeq

  /** Integer remainder. */
  def rem(x: Int)(y: Int): Int = if y == 0 then 0 else x % y

  /** Return x rounded to the nearest integer. */
  // Haskell uses half-even (banker's) rounding
  def round(x: Double): BigInt = BigDecimal(x).setScale(0, BigDecimal.RoundingMode.HALF_EVEN).toBigInt

  /** Round a bigfloat to n significant digits. */
  def roundBigfloat(precision: Int)(x: BigDecimal): BigDecimal =
    if x == 0 then BigDecimal(0.0)
    else
      val d = x.toDouble
      val factor = scala.math.pow(10, precision - 1 - scala.math.floor(scala.math.log10(scala.math.abs(d))))
      BigDecimal(scala.math.round(d * factor) / factor)

  /** Round a float to n significant digits. */
  def roundFloat(precision: Int)(x: Double): Double =
    if x == 0 then 0.0
    else
      val factor = scala.math.pow(10, precision - 1 - scala.math.floor(scala.math.log10(scala.math.abs(x))))
      scala.math.round(x * factor) / factor

  /** Round a float32 to n significant digits. */
  def roundFloat32(precision: Int)(x: Float): Float =
    roundFloat(precision)(x.toDouble).toFloat

  /** Round a float64 to n significant digits. */
  def roundFloat64(precision: Int)(x: Double): Double =
    roundFloat(precision)(x)

  /** Return the sign of a number (-1, 0, or 1). */
  def signum(x: Int): Int = x.sign

  /** Return the sine of x radians. */
  def sin(x: Double): Double = scala.math.sin(x)

  /** Return the hyperbolic sine of x. */
  def sinh(x: Double): Double = scala.math.sinh(x)

  /** Return the square root of x. */
  def sqrt(x: Double): Double = scala.math.sqrt(x)

  /** Subtract two numbers. */
  def sub(x: Int)(y: Int): Int = x - y

  /** Return the successor (x + 1). */
  def succ(x: Int): Int = x + 1

  /** Return the tangent of x radians. */
  def tan(x: Double): Double = scala.math.tan(x)

  /** Return the hyperbolic tangent of x. */
  def tanh(x: Double): Double = scala.math.tanh(x)

  /** Return x truncated to an integer (towards zero). */
  def truncate(x: Double): BigInt = BigDecimal(x).toBigInt

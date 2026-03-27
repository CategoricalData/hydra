package hydra.lib

object equality:
  private def cmp(x: Any, y: Any): Int = (x, y) match
    case (a: BigInt, b: Long) => a.compare(BigInt(b))
    case (a: Long, b: BigInt) => BigInt(a).compare(b)
    case (a: Comparable[?], _) => a.asInstanceOf[Comparable[Any]].compareTo(y)
    case _ => x.toString.compareTo(y.toString)

  /** Compare two values and return a Comparison. */
  def compare[A](x: A)(y: A): hydra.util.Comparison =
    val c = cmp(x, y)
    if c < 0 then hydra.util.Comparison.lessThan
    else if c > 0 then hydra.util.Comparison.greaterThan
    else hydra.util.Comparison.equalTo

  /** Compare two Terms structurally, extracting literal values for proper numeric comparison. */
  def compareTerms(t1: hydra.core.Term, t2: hydra.core.Term): Int =
    (t1, t2) match
      case (hydra.core.Term.literal(l1), hydra.core.Term.literal(l2)) => compareLiterals(l1, l2)
      case _ => hydra.show.core.term(t1).compareTo(hydra.show.core.term(t2))

  /** Check if two values are equal. */
  def equal[A](x: A)(y: A): Boolean = x == y

  /** Check if first value is greater than second. */
  def gt[A](x: A)(y: A): Boolean = cmp(x, y) > 0

  /** Check if first value is greater than or equal to second. */
  def gte[A](x: A)(y: A): Boolean = cmp(x, y) >= 0

  /** Return a value unchanged. */
  def identity[A](x: A): A = x

  /** Check if first value is less than second. */
  def lt[A](x: A)(y: A): Boolean = cmp(x, y) < 0

  /** Check if first value is less than or equal to second. */
  def lte[A](x: A)(y: A): Boolean = cmp(x, y) <= 0

  /** Return the maximum of two values. */
  def max[A](x: A)(y: A): A = if cmp(x, y) >= 0 then x else y

  /** Return the minimum of two values. */
  def min[A](x: A)(y: A): A = if cmp(x, y) <= 0 then x else y

  private def compareLiterals(l1: hydra.core.Literal, l2: hydra.core.Literal): Int =
    (l1, l2) match
      case (hydra.core.Literal.integer(v1), hydra.core.Literal.integer(v2)) => compareIntegers(v1, v2)
      case (hydra.core.Literal.float(v1), hydra.core.Literal.float(v2)) => compareFloats(v1, v2)
      case (hydra.core.Literal.string(s1), hydra.core.Literal.string(s2)) => s1.compareTo(s2)
      case (hydra.core.Literal.boolean(b1), hydra.core.Literal.boolean(b2)) => java.lang.Boolean.compare(b1, b2)
      case _ => hydra.show.core.literal(l1).compareTo(hydra.show.core.literal(l2))

  private def compareIntegers(v1: hydra.core.IntegerValue, v2: hydra.core.IntegerValue): Int =
    val b1 = integerToBigInt(v1)
    val b2 = integerToBigInt(v2)
    b1.compare(b2)

  private def integerToBigInt(v: hydra.core.IntegerValue): BigInt = v match
    case hydra.core.IntegerValue.int8(n) => BigInt(n)
    case hydra.core.IntegerValue.int16(n) => BigInt(n)
    case hydra.core.IntegerValue.int32(n) => BigInt(n)
    case hydra.core.IntegerValue.int64(n) => BigInt(n)
    case hydra.core.IntegerValue.uint8(n) => BigInt(n.toInt & 0xff)
    case hydra.core.IntegerValue.uint16(n) => BigInt(n)
    case hydra.core.IntegerValue.uint32(n) => BigInt(n)
    case hydra.core.IntegerValue.uint64(n) => n
    case hydra.core.IntegerValue.bigint(n) => n

  private def compareFloats(v1: hydra.core.FloatValue, v2: hydra.core.FloatValue): Int =
    val d1 = floatToBigDecimal(v1)
    val d2 = floatToBigDecimal(v2)
    d1.compare(d2)

  private def floatToBigDecimal(v: hydra.core.FloatValue): BigDecimal = v match
    case hydra.core.FloatValue.float32(n) => BigDecimal(n.toDouble)
    case hydra.core.FloatValue.float64(n) => BigDecimal(n)
    case hydra.core.FloatValue.bigfloat(n) => n

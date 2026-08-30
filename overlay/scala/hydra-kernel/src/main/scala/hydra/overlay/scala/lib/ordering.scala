package hydra.overlay.scala.lib

/**
 * Structural comparison per docs/specification/ordering-and-equality.md: records compare
 * field-by-field in declaration order, unions by declared-variant order then payload, and
 * built-in constructors (list/set/map/optional/either/pair) per the spec's own rules — with
 * no print-based fallback. Coder-generated `enum` types (Term, Literal, Type, ...) expose
 * declaration order directly via `.ordinal`; coder-generated `case class` types (Record,
 * Injection, Field, ...) preserve declaration order in their `Product` element order, so a
 * single generic recursion over `Product`/`Iterable` structure handles every kernel type
 * without a per-type dispatch table.
 */
object ordering:
  private def cmp(x: Any, y: Any): Int = compareTerms(x, y)

  def compare[A](x: A)(y: A): hydra.util.Comparison =
    val c = cmp(x, y)
    if c < 0 then hydra.util.Comparison.lessThan
    else if c > 0 then hydra.util.Comparison.greaterThan
    else hydra.util.Comparison.equalTo
  def gt[A](x: A)(y: A): Boolean = cmp(x, y) > 0
  def gte[A](x: A)(y: A): Boolean = cmp(x, y) >= 0
  def lt[A](x: A)(y: A): Boolean = cmp(x, y) < 0
  def lte[A](x: A)(y: A): Boolean = cmp(x, y) <= 0
  def max[A](x: A)(y: A): A = if cmp(x, y) >= 0 then x else y
  def min[A](x: A)(y: A): A = if cmp(x, y) <= 0 then x else y

  /** Structural comparison of any two Hydra values of the same type. */
  def compareTerms(x: Any, y: Any): Int = (x, y) match
    // Decimals: numeric value first, then scale as tiebreak (spec: 1.1 < 1.10, distinct).
    case (a: BigDecimal, b: BigDecimal) =>
      val c = a.compare(b)
      if c != 0 then c else Integer.compare(a.scale, b.scale)
    case (a: BigInt, b: BigInt) => a.compare(b)
    // Extended total order incl. NaN-greatest and signed-zero, matching the spec: boxed
    // java.lang.Float/Double natively implement IEEE 754 totalOrder semantics.
    case (a: Float, b: Float) => java.lang.Float.compare(a, b)
    case (a: Double, b: Double) => java.lang.Double.compare(a, b)
    case (a: String, b: String) => a.compareTo(b)
    case (a: Boolean, b: Boolean) => java.lang.Boolean.compare(a, b)
    case (a: Byte, b: Byte) => java.lang.Byte.compare(a, b)
    case (a: Short, b: Short) => java.lang.Short.compare(a, b)
    case (a: Int, b: Int) => java.lang.Integer.compare(a, b)
    case (a: Long, b: Long) => java.lang.Long.compare(a, b)
    case (a: Array[Byte], b: Array[Byte]) => compareIterables(a.iterator, b.iterator)

    // Enum-generated union types (Term, Literal, Type, IntegerValue, FloatValue, ...):
    // declared-variant order via .ordinal, then recurse into the payload (nullary cases,
    // e.g. Term.unit, carry no payload and compare equal once ordinals match).
    case (a: reflect.Enum, b: reflect.Enum) =>
      val c = Integer.compare(a.ordinal, b.ordinal)
      if c != 0 then c
      else (a, b) match
        case (pa: Product, pb: Product) if pa.productArity == 1 => compareTerms(pa.productElement(0), pb.productElement(0))
        case _ => 0

    // Optionals: none < given x; given x vs given y compare by payload.
    case (None, None) => 0
    case (None, Some(_)) => -1
    case (Some(_), None) => 1
    case (a: Some[?], b: Some[?]) => compareTerms(a.get, b.get)

    // Eithers: every left is less than every right; same side compares by payload.
    case (Left(a), Left(b)) => compareTerms(a, b)
    case (Right(a), Right(b)) => compareTerms(a, b)
    case (Left(_), Right(_)) => -1
    case (Right(_), Left(_)) => 1

    // Maps: ascending-key sequence of bindings, each comparing by key then value.
    case (a: collection.Map[?, ?], b: collection.Map[?, ?]) =>
      val as = a.toSeq.sortWith((p, q) => compareTerms(p._1, q._1) < 0)
      val bs = b.toSeq.sortWith((p, q) => compareTerms(p._1, q._1) < 0)
      compareIterables(as.iterator, bs.iterator)

    // Sets: ascending sequence of elements.
    case (a: collection.Set[?], b: collection.Set[?]) =>
      val as = a.toSeq.sortWith((p, q) => compareTerms(p, q) < 0)
      val bs = b.toSeq.sortWith((p, q) => compareTerms(p, q) < 0)
      compareIterables(as.iterator, bs.iterator)

    // Lists/other sequences and tuples (Pair, etc.): lexicographic, prefix least.
    case (a: Iterable[?], b: Iterable[?]) => compareIterables(a.iterator, b.iterator)
    case (a: Product, b: Product) if a.productArity == b.productArity =>
      compareIterables(a.productIterator, b.productIterator)

    case (a: Comparable[?], _) => a.asInstanceOf[Comparable[Any]].compareTo(y)
    case _ => throw new IllegalArgumentException(s"Cannot compare $x and $y")

  private def compareIterables(xs: Iterator[Any], ys: Iterator[Any]): Int =
    while xs.hasNext && ys.hasNext do
      val c = compareTerms(xs.next(), ys.next())
      if c != 0 then return c
    Integer.compare(if xs.hasNext then 1 else 0, if ys.hasNext then 1 else 0)

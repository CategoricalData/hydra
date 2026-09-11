package hydra.overlay.scala.lib

object equality:
  def equal[A](x: A)(y: A): Boolean = (x, y) match
    // Decimals: equal iff both value and scale agree (docs/specification/
    // ordering-and-equality.md: 1.1 != 1.10, distinct). scala.math.BigDecimal's
    // own equals/hashCode are scale-BLIND by design (delegate to compareTo,
    // unlike java.math.BigDecimal) -- compare via the underlying scale-sensitive
    // java.math.BigDecimal instead.
    case (a: BigDecimal, b: BigDecimal) => a.bigDecimal.equals(b.bigDecimal)
    // Floats: native == is IEEE-754-native (NaN != NaN, -0.0 == 0.0), not Hydra's extended
    // totalOrder (docs/specification/ordering-and-equality.md: NaN equal to itself, -0.0 unequal
    // to +0.0). Route through the same comparator ordering.compare already uses so equal agrees
    // with compare (java.lang.Float/Double.compare are totalOrder-native).
    case (a: Float, b: Float) => java.lang.Float.compare(a, b) == 0
    case (a: Double, b: Double) => java.lang.Double.compare(a, b) == 0
    case _ => x == y

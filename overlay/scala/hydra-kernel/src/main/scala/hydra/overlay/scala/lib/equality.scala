package hydra.overlay.scala.lib

object equality:
  def equal[A](x: A)(y: A): Boolean = (x, y) match
    // Decimals: equal iff both value and scale agree (docs/specification/
    // ordering-and-equality.md: 1.1 != 1.10, distinct). scala.math.BigDecimal's
    // own equals/hashCode are scale-BLIND by design (delegate to compareTo,
    // unlike java.math.BigDecimal) -- compare via the underlying scale-sensitive
    // java.math.BigDecimal instead.
    case (a: BigDecimal, b: BigDecimal) => a.bigDecimal.equals(b.bigDecimal)
    case _ => x == y

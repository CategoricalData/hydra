package hydra.lib

object equality:
  private def cmp(x: Any, y: Any): Int = (x, y) match
    case (a: BigInt, b: Long) => a.compare(BigInt(b))
    case (a: Long, b: BigInt) => BigInt(a).compare(b)
    case (a: Comparable[?], _) => a.asInstanceOf[Comparable[Any]].compareTo(y)
    case _ => x.toString.compareTo(y.toString)
  def compare[A](x: A)(y: A): hydra.util.Comparison =
    val c = cmp(x, y)
    if c < 0 then hydra.util.Comparison.lessThan
    else if c > 0 then hydra.util.Comparison.greaterThan
    else hydra.util.Comparison.equalTo
  def equal[A](x: A)(y: A): Boolean = x == y
  def gt[A](x: A)(y: A): Boolean = cmp(x, y) > 0
  def gte[A](x: A)(y: A): Boolean = cmp(x, y) >= 0
  def identity[A](x: A): A = x
  def lt[A](x: A)(y: A): Boolean = cmp(x, y) < 0
  def lte[A](x: A)(y: A): Boolean = cmp(x, y) <= 0
  def max[A](x: A)(y: A): A = if cmp(x, y) >= 0 then x else y
  def min[A](x: A)(y: A): A = if cmp(x, y) <= 0 then x else y

package hydra.lib

object lists:
  def apply[A, B](fs: Seq[A => B])(xs: Seq[A]): Seq[B] = for { f <- fs; x <- xs } yield f(x)
  def at[A](i: Int)(xs: Seq[A]): A = xs(i)
  def bind[A, B](xs: Seq[A])(f: A => Seq[B]): Seq[B] = xs.flatMap(f)
  def concat[A](xss: Seq[Seq[A]]): Seq[A] = xss.flatten
  def concat2[A](xs: Seq[A])(ys: Seq[A]): Seq[A] = xs ++ ys
  def cons[A](x: A)(xs: Seq[A]): Seq[A] = x +: xs
  def drop[A](n: Int)(xs: Seq[A]): Seq[A] = xs.drop(n)
  def dropWhile[A](p: A => Boolean)(xs: Seq[A]): Seq[A] = xs.dropWhile(p)
  def elem[A](x: A)(xs: Seq[A]): Boolean = xs.contains(x)
  def filter[A](p: A => Boolean)(xs: Seq[A]): Seq[A] = xs.filter(p)
  def find[A](p: A => Boolean)(xs: Seq[A]): Option[A] = xs.find(p)
  def foldl[B, A](f: B => A => B)(z: B)(xs: Seq[A]): B = xs.foldLeft(z)((b, a) => f(b)(a))
  def foldr[A, B](f: A => B => B)(z: B)(xs: Seq[A]): B = xs.foldRight(z)((a, b) => f(a)(b))
  def group[A](xs: Seq[A]): Seq[Seq[A]] =
    if xs.isEmpty then Seq.empty
    else
      val (same, rest) = xs.span(_ == xs.head)
      same +: group(rest)
  def head[A](xs: Seq[A]): A = if xs.isEmpty then null.asInstanceOf[A] else xs.head
  def init[A](xs: Seq[A]): Seq[A] = if xs.isEmpty then Seq.empty else xs.init
  def intercalate[A](sep: Seq[A])(xss: Seq[Seq[A]]): Seq[A] =
    if xss.isEmpty then Seq.empty
    else xss.reduceLeft((a, b) => a ++ sep ++ b)
  def intersperse[A](sep: A)(xs: Seq[A]): Seq[A] =
    if xs.isEmpty then Seq.empty
    else xs.flatMap(x => Seq(sep, x)).tail
  def last[A](xs: Seq[A]): A = if xs.isEmpty then null.asInstanceOf[A] else xs.last
  def length[A](xs: Seq[A]): Int = xs.length
  def map[A, B](f: A => B)(xs: Seq[A]): Seq[B] = xs.map(f)
  def maybeAt[A](i: Int)(xs: Seq[A]): Option[A] = if i >= 0 && i < xs.length then Some(xs(i)) else None
  def maybeHead[A](xs: Seq[A]): Option[A] = xs.headOption
  def maybeInit[A](xs: Seq[A]): Option[Seq[A]] = if xs.isEmpty then None else Some(xs.init)
  def maybeLast[A](xs: Seq[A]): Option[A] = xs.lastOption
  def maybeTail[A](xs: Seq[A]): Option[Seq[A]] = if xs.isEmpty then None else Some(xs.tail)
  def nub[A](xs: Seq[A]): Seq[A] = xs.distinct
  def `null`[A](xs: Seq[A]): Boolean = xs.isEmpty
  def partition[A](p: A => Boolean)(xs: Seq[A]): (Seq[A], Seq[A]) = xs.partition(p)
  def pure[A](x: A): Seq[A] = Seq(x)
  def replicate[A](n: Int)(x: A): Seq[A] = Seq.fill(n)(x)
  def reverse[A](xs: Seq[A]): Seq[A] = xs.reverse
  def safeHead[A](xs: Seq[A]): Option[A] = xs.headOption
  def singleton[A](x: A): Seq[A] = Seq(x)
  def sort[A](xs: Seq[A]): Seq[A] =
    if xs.isEmpty then xs
    else xs.sortWith((a, b) => equality.lt(a)(b))
  def sortOn[A, B](f: A => B)(xs: Seq[A])(using ord: Ordering[B] = null): Seq[A] =
    if ord != null then xs.sortBy(f)(using ord)
    else xs.sortWith((a, b) => equality.lt(f(a))(f(b)))
  def span[A](p: A => Boolean)(xs: Seq[A]): (Seq[A], Seq[A]) = xs.span(p)
  def tail[A](xs: Seq[A]): Seq[A] = if xs.isEmpty then Seq.empty else xs.tail
  def take[A](n: Int)(xs: Seq[A]): Seq[A] = xs.take(n)
  // Haskell's transpose handles ragged matrices (unlike Scala's built-in transpose)
  def transpose[A](xss: Seq[Seq[A]]): Seq[Seq[A]] =
    if xss.isEmpty then Seq.empty
    else
      val maxLen = xss.map(_.length).max
      (0 until maxLen).map { i =>
        xss.flatMap(row => if i < row.length then Some(row(i)) else None)
      }.toSeq
  def uncons[A](xs: Seq[A]): Option[(A, Seq[A])] = if xs.isEmpty then None else Some((xs.head, xs.tail))
  def zip[A, B](xs: Seq[A])(ys: Seq[B]): Seq[(A, B)] = xs.zip(ys)
  def zipWith[A, B, C](f: A => B => C)(xs: Seq[A])(ys: Seq[B]): Seq[C] = xs.zip(ys).map((a, b) => f(a)(b))

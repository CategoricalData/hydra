package hydra.lib

object lists:
  /** Apply a list of functions to a list of values (applicative style). */
  def apply[A, B](fs: Seq[A => B])(xs: Seq[A]): Seq[B] = for { f <- fs; x <- xs } yield f(x)

  /** Get the element at a specified index in a list. */
  def at[A](i: Int)(xs: Seq[A]): A = xs(i)

  /** Apply a function that returns lists to each element and flatten results. */
  def bind[A, B](xs: Seq[A])(f: A => Seq[B]): Seq[B] = xs.flatMap(f)

  /** Concatenate a list of lists. */
  def concat[A](xss: Seq[Seq[A]]): Seq[A] = xss.flatten

  /** Concatenate two lists. */
  def concat2[A](xs: Seq[A])(ys: Seq[A]): Seq[A] = xs ++ ys

  /** Prepend a value to a list. */
  def cons[A](x: A)(xs: Seq[A]): Seq[A] = x +: xs

  /** Drop the first n elements from a list. */
  def drop[A](n: Int)(xs: Seq[A]): Seq[A] = xs.drop(n)

  /** Drop elements from the beginning of a list while predicate is true. */
  def dropWhile[A](p: A => Boolean)(xs: Seq[A]): Seq[A] = xs.dropWhile(p)

  /** Check if an element is in a list. */
  def elem[A](x: A)(xs: Seq[A]): Boolean = xs.contains(x)

  /** Filter a list based on a predicate. */
  def filter[A](p: A => Boolean)(xs: Seq[A]): Seq[A] = xs.filter(p)

  /** Find the first element matching a predicate. */
  def find[A](p: A => Boolean)(xs: Seq[A]): Option[A] = xs.find(p)

  /** Fold a list from the left. */
  def foldl[B, A](f: B => A => B)(z: B)(xs: Seq[A]): B = xs.foldLeft(z)((b, a) => f(b)(a))

  /** Fold a list from the right. */
  def foldr[A, B](f: A => B => B)(z: B)(xs: Seq[A]): B = xs.foldRight(z)((a, b) => f(a)(b))

  /** Group consecutive equal elements. */
  def group[A](xs: Seq[A]): Seq[Seq[A]] =
    if xs.isEmpty then Seq.empty
    else
      val (same, rest) = xs.span(_ == xs.head)
      same +: group(rest)

  /** Get the first element of a list. */
  def head[A](xs: Seq[A]): A = if xs.isEmpty then null.asInstanceOf[A] else xs.head

  /** Return all elements except the last one. */
  def init[A](xs: Seq[A]): Seq[A] = if xs.isEmpty then Seq.empty else xs.init

  /** Intercalate a list of lists with a separator list between each. */
  def intercalate[A](sep: Seq[A])(xss: Seq[Seq[A]]): Seq[A] =
    if xss.isEmpty then Seq.empty
    else xss.reduceLeft((a, b) => a ++ sep ++ b)

  /** Intersperse a value between elements of a list. */
  def intersperse[A](sep: A)(xs: Seq[A]): Seq[A] =
    if xs.isEmpty then Seq.empty
    else xs.flatMap(x => Seq(sep, x)).tail

  /** Get the last element of a list. */
  def last[A](xs: Seq[A]): A = if xs.isEmpty then null.asInstanceOf[A] else xs.last

  /** Get the length of a list. */
  def length[A](xs: Seq[A]): Int = xs.length

  /** Map a function over a list. */
  def map[A, B](f: A => B)(xs: Seq[A]): Seq[B] = xs.map(f)

  /** Remove duplicate elements from a list. */
  def nub[A](xs: Seq[A]): Seq[A] = xs.distinct

  /** Check if a list is empty. */
  def `null`[A](xs: Seq[A]): Boolean = xs.isEmpty

  /** Partition a list into elements that satisfy a predicate and elements that do not. */
  def partition[A](p: A => Boolean)(xs: Seq[A]): (Seq[A], Seq[A]) = xs.partition(p)

  /** Create a list with a single element. */
  def pure[A](x: A): Seq[A] = Seq(x)

  /** Create a list with n copies of a value. */
  def replicate[A](n: Int)(x: A): Seq[A] = Seq.fill(n)(x)

  /** Reverse a list. */
  def reverse[A](xs: Seq[A]): Seq[A] = xs.reverse

  /** Get the first element of a list, returning Nothing if the list is empty. */
  def safeHead[A](xs: Seq[A]): Option[A] = xs.headOption

  /** Create a single-element list. */
  def singleton[A](x: A): Seq[A] = Seq(x)

  /** Sort a list. */
  def sort[A](xs: Seq[A]): Seq[A] =
    if xs.isEmpty then xs
    else xs.sortWith((a, b) => equality.lt(a)(b))

  /** Sort a list based on a key function. */
  def sortOn[A, B](f: A => B)(xs: Seq[A])(using ord: Ordering[B] = null): Seq[A] =
    if ord != null then xs.sortBy(f)(using ord)
    else xs.sortWith((a, b) => equality.lt(f(a))(f(b)))

  /** Split a list at the first element where predicate fails. */
  def span[A](p: A => Boolean)(xs: Seq[A]): (Seq[A], Seq[A]) = xs.span(p)

  /** Get all elements of a list except the first. */
  def tail[A](xs: Seq[A]): Seq[A] = if xs.isEmpty then Seq.empty else xs.tail

  /** Take the first n elements from a list. */
  def take[A](n: Int)(xs: Seq[A]): Seq[A] = xs.take(n)

  /** Transpose a list of lists. */
  def transpose[A](xss: Seq[Seq[A]]): Seq[Seq[A]] =
    if xss.isEmpty then Seq.empty
    else
      val maxLen = xss.map(_.length).max
      (0 until maxLen).map { i =>
        xss.flatMap(row => if i < row.length then Some(row(i)) else None)
      }.toSeq

  /** Zip two lists into pairs. */
  def zip[A, B](xs: Seq[A])(ys: Seq[B]): Seq[(A, B)] = xs.zip(ys)

  /** Zip two lists with a combining function. */
  def zipWith[A, B, C](f: A => B => C)(xs: Seq[A])(ys: Seq[B]): Seq[C] = xs.zip(ys).map((a, b) => f(a)(b))

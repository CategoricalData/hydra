package hydra.lib

object sets:
  /** Delete an element from a set. */
  def delete[A](x: A)(s: Set[A]): Set[A] = s - x

  /** Compute the difference of two sets. */
  def difference[A](s1: Set[A])(s2: Set[A]): Set[A] = s1 -- s2

  /** Create an empty set. */
  def empty[A]: Set[A] = Set.empty

  /** Create a set from a list. */
  def fromList[A](xs: Seq[A]): Set[A] = xs.toSet

  /** Insert an element into a set. */
  def insert[A](x: A)(s: Set[A]): Set[A] = s + x

  /** Compute the intersection of two sets. */
  def intersection[A](s1: Set[A])(s2: Set[A]): Set[A] = s1.intersect(s2)

  /** Map a function over a set. */
  def map[A, B](f: A => B)(s: Set[A]): Set[B] = s.map(f)

  /** Check if an element is in a set. */
  def member[A](x: A)(s: Set[A]): Boolean = s.contains(x)

  /** Check if a set is empty. */
  def `null`[A](s: Set[A]): Boolean = s.isEmpty

  /** Create a singleton set. */
  def singleton[A](x: A): Set[A] = Set(x)

  /** Get the size of a set. */
  def size[A](s: Set[A]): Int = s.size

  /** Convert a set to a list. */
  def toList[A](s: Set[A]): Seq[A] = s.toSeq

  /** Compute the union of two sets. */
  def union[A](s1: Set[A])(s2: Set[A]): Set[A] = s1.union(s2)

  /** Compute the union of multiple sets. */
  def unions[A](ss: Seq[Set[A]]): Set[A] = ss.foldLeft(Set.empty[A])(_ ++ _)

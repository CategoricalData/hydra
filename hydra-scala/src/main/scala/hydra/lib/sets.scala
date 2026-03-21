package hydra.lib

object sets:
  def delete[A](x: A)(s: Set[A]): Set[A] = s - x
  def difference[A](s1: Set[A])(s2: Set[A]): Set[A] = s1 -- s2
  def empty[A]: Set[A] = Set.empty
  def fromList[A](xs: Seq[A]): Set[A] = xs.toSet
  def insert[A](x: A)(s: Set[A]): Set[A] = s + x
  def intersection[A](s1: Set[A])(s2: Set[A]): Set[A] = s1.intersect(s2)
  def map[A, B](f: A => B)(s: Set[A]): Set[B] = s.map(f)
  def member[A](x: A)(s: Set[A]): Boolean = s.contains(x)
  def `null`[A](s: Set[A]): Boolean = s.isEmpty
  def singleton[A](x: A): Set[A] = Set(x)
  def size[A](s: Set[A]): Int = s.size
  def toList[A](s: Set[A]): Seq[A] = s.toSeq
  def union[A](s1: Set[A])(s2: Set[A]): Set[A] = s1.union(s2)
  def unions[A](ss: Seq[Set[A]]): Set[A] = ss.foldLeft(Set.empty[A])(_ ++ _)

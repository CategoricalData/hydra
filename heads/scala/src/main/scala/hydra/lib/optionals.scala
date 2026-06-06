package hydra.lib

object optionals:
  def apply[A, B](mf: Option[A => B])(ma: Option[A]): Option[B] = mf.flatMap(f => ma.map(f))
  def bind[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  def cases[A, B](ma: Option[A])(ifNone: => B)(ifSome: A => B): B = ma match
    case None => ifNone
    case Some(a) => ifSome(a)
  def cat[A](xs: Seq[Option[A]]): Seq[A] = xs.flatten
  def compose[A, B, C](f: A => Option[B])(g: B => Option[C])(a: A): Option[C] = f(a).flatMap(g)
  def fromOptional[A](d: => A)(ma: Option[A]): A = ma.getOrElse(d)
  def isGiven[A](ma: Option[A]): Boolean = ma.isDefined
  def isNone[A](ma: Option[A]): Boolean = ma.isEmpty
  def map[A, B](f: A => B)(ma: Option[A]): Option[B] = ma.map(f)
  def mapOptional[A, B](f: A => Option[B])(xs: Seq[A]): Seq[B] = xs.flatMap(f)
  def pure[A](a: A): Option[A] = Some(a)
  def toList[A](ma: Option[A]): Seq[A] = ma.toSeq

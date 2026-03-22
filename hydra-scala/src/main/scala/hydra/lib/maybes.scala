package hydra.lib

object maybes:
  def apply[A, B](mf: Option[A => B])(ma: Option[A]): Option[B] = mf.flatMap(f => ma.map(f))
  def bind[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)
  def cases[A, B](ma: Option[A])(ifNone: => B)(ifSome: A => B): B = ma match
    case None => ifNone
    case Some(a) => ifSome(a)
  def cat[A](xs: Seq[Option[A]]): Seq[A] = xs.flatten
  def compose[A, B, C](f: A => Option[B])(g: B => Option[C])(a: A): Option[C] = f(a).flatMap(g)
  def fromJust[A](ma: Option[A]): A = ma.get
  def fromMaybe[A](d: => A)(ma: Option[A]): A = ma.getOrElse(d)
  def isJust[A](ma: Option[A]): Boolean = ma.isDefined
  def isNothing[A](ma: Option[A]): Boolean = ma.isEmpty
  def map[A, B](f: A => B)(ma: Option[A]): Option[B] = ma.map(f)
  def mapMaybe[A, B](f: A => Option[B])(xs: Seq[A]): Seq[B] = xs.flatMap(f)
  def maybe[B, A](d: => B)(f: A => B)(ma: Option[A]): B = ma.map(f).getOrElse(d)
  def pure[A](a: A): Option[A] = Some(a)
  def toList[A](ma: Option[A]): Seq[A] = ma.toSeq

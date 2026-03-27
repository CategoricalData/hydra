package hydra.lib

object maybes:
  /** Apply a function to an argument (applicative). */
  def apply[A, B](mf: Option[A => B])(ma: Option[A]): Option[B] = mf.flatMap(f => ma.map(f))

  /** Chain operations on optional values, handling Nothing cases automatically. */
  def bind[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma.flatMap(f)

  /** Handle an optional value with the maybe value as the first argument. */
  def cases[A, B](ma: Option[A])(ifNone: => B)(ifSome: A => B): B = ma match
    case None => ifNone
    case Some(a) => ifSome(a)

  /** Filter out Nothing values from a list. */
  def cat[A](xs: Seq[Option[A]]): Seq[A] = xs.flatten

  /** Compose two Maybe-returning functions (Kleisli composition). */
  def compose[A, B, C](f: A => Option[B])(g: B => Option[C])(a: A): Option[C] = f(a).flatMap(g)

  /** Extract value from a Just, or error on Nothing (partial function). */
  def fromJust[A](ma: Option[A]): A = ma.get

  /** Get a value from an optional value, or return a default value. */
  def fromMaybe[A](d: => A)(ma: Option[A]): A = ma.getOrElse(d)

  /** Check if a value is Just. */
  def isJust[A](ma: Option[A]): Boolean = ma.isDefined

  /** Check if a value is Nothing. */
  def isNothing[A](ma: Option[A]): Boolean = ma.isEmpty

  /** Map a function over an optional value. */
  def map[A, B](f: A => B)(ma: Option[A]): Option[B] = ma.map(f)

  /** Map a function over a list and collect Just results. */
  def mapMaybe[A, B](f: A => Option[B])(xs: Seq[A]): Seq[B] = xs.flatMap(f)

  /** Eliminate an optional value with a default and a function. */
  def maybe[B, A](d: => B)(f: A => B)(ma: Option[A]): B = ma.map(f).getOrElse(d)

  /** Lift a value into the Maybe type. */
  def pure[A](a: A): Option[A] = Some(a)

  /** Convert a Maybe to a list: Just x becomes [x], Nothing becomes []. */
  def toList[A](ma: Option[A]): Seq[A] = ma.toSeq

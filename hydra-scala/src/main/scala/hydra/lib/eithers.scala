package hydra.lib

object eithers:
  /** Map over both sides of an Either value. */
  def bimap[A, B, C, D](f: A => C)(g: B => D)(e: Either[A, B]): Either[C, D] = e match
    case Left(a) => Left(f(a))
    case Right(b) => Right(g(b))

  /** Bind (flatMap) for Either: if Right, apply the function; if Left, return unchanged. */
  def bind[A, B, C](e: Either[A, B])(f: B => Either[A, C]): Either[A, C] = e.flatMap(f)

  /** Eliminate an Either value by applying one of two functions. */
  def either[A, B, C](f: A => C)(g: B => C)(e: Either[A, B]): C = e match
    case Left(a) => f(a)
    case Right(b) => g(b)

  /** Left-fold over a list with an Either-returning function, short-circuiting on Left. */
  def foldl[B, A, E](f: B => A => Either[E, B])(z: B)(xs: Seq[A]): Either[E, B] =
    xs.foldLeft[Either[E, B]](Right(z)) { (acc, x) =>
      acc.flatMap(b => f(b)(x))
    }

  /** Extract the Left value, or return a default. */
  def fromLeft[A, B](d: => A)(e: Either[A, B]): A = e match
    case Left(a) => a
    case Right(_) => d

  /** Extract the Right value, or return a default. */
  def fromRight[A, B](d: => B)(e: Either[A, B]): B = e match
    case Left(_) => d
    case Right(b) => b

  /** Check if an Either is a Left value. */
  def isLeft[A, B](e: Either[A, B]): Boolean = e.isLeft

  /** Check if an Either is a Right value. */
  def isRight[A, B](e: Either[A, B]): Boolean = e.isRight

  /** Extract all Left values from a list of Eithers. */
  def lefts[A, B](es: Seq[Either[A, B]]): Seq[A] = es.collect { case Left(a) => a }

  /** Map a function over the Right side of an Either (standard functor map). */
  def map[B, C, A](f: B => C)(e: Either[A, B]): Either[A, C] = e.map(f)

  /** Map a function returning Either over a list, collecting results or short-circuiting on Left. */
  def mapList[B, C, A](f: B => Either[A, C])(xs: Seq[B]): Either[A, Seq[C]] =
    xs.foldLeft[Either[A, Seq[C]]](Right(Seq.empty)) { (acc, x) =>
      acc.flatMap(cs => f(x).map(c => cs :+ c))
    }

  /** Map a function returning Either over a Maybe, or return Right Nothing if Nothing. */
  def mapMaybe[B, C, A](f: B => Either[A, C])(m: Option[B]): Either[A, Option[C]] = m match
    case None => Right(None)
    case Some(b) => f(b).map(Some(_))

  /** Map a function returning Either over a Set, collecting results or short-circuiting on Left. */
  def mapSet[B, C, A](f: B => Either[A, C])(xs: Set[B]): Either[A, Set[C]] =
    xs.foldLeft[Either[A, Set[C]]](Right(Set.empty)) { (acc, x) =>
      acc.flatMap(cs => f(x).map(c => cs + c))
    }

  /** Partition a list of Eithers into lefts and rights. */
  def partitionEithers[A, B](es: Seq[Either[A, B]]): (Seq[A], Seq[B]) =
    (es.collect { case Left(a) => a }, es.collect { case Right(b) => b })

  /** Extract all Right values from a list of Eithers. */
  def rights[A, B](es: Seq[Either[A, B]]): Seq[B] = es.collect { case Right(b) => b }

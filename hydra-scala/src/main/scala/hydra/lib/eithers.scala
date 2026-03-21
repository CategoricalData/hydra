package hydra.lib

object eithers:
  def bimap[A, B, C, D](f: A => C)(g: B => D)(e: Either[A, B]): Either[C, D] = e match
    case Left(a) => Left(f(a))
    case Right(b) => Right(g(b))
  def bind[A, B, C](e: Either[A, B])(f: B => Either[A, C]): Either[A, C] = e.flatMap(f)
  def either[A, B, C](f: A => C)(g: B => C)(e: Either[A, B]): C = e match
    case Left(a) => f(a)
    case Right(b) => g(b)
  def foldl[B, A, E](f: B => A => Either[E, B])(z: B)(xs: Seq[A]): Either[E, B] =
    xs.foldLeft[Either[E, B]](Right(z)) { (acc, x) =>
      acc.flatMap(b => f(b)(x))
    }
  def fromLeft[A, B](d: A)(e: Either[A, B]): A = e match
    case Left(a) => a
    case Right(_) => d
  def fromRight[A, B](d: B)(e: Either[A, B]): B = e match
    case Left(_) => d
    case Right(b) => b
  def isLeft[A, B](e: Either[A, B]): Boolean = e.isLeft
  def isRight[A, B](e: Either[A, B]): Boolean = e.isRight
  def lefts[A, B](es: Seq[Either[A, B]]): Seq[A] = es.collect { case Left(a) => a }
  def map[B, C, A](f: B => C)(e: Either[A, B]): Either[A, C] = e.map(f)
  def mapList[B, C, A](f: B => Either[A, C])(xs: Seq[B]): Either[A, Seq[C]] =
    xs.foldLeft[Either[A, Seq[C]]](Right(Seq.empty)) { (acc, x) =>
      acc.flatMap(cs => f(x).map(c => cs :+ c))
    }
  def mapMaybe[B, C, A](f: B => Either[A, C])(m: Option[B]): Either[A, Option[C]] = m match
    case None => Right(None)
    case Some(b) => f(b).map(Some(_))
  def mapSet[B, C, A](f: B => Either[A, C])(xs: Set[B]): Either[A, Set[C]] =
    xs.foldLeft[Either[A, Set[C]]](Right(Set.empty)) { (acc, x) =>
      acc.flatMap(cs => f(x).map(c => cs + c))
    }
  def partitionEithers[A, B](es: Seq[Either[A, B]]): (Seq[A], Seq[B]) =
    (es.collect { case Left(a) => a }, es.collect { case Right(b) => b })
  def rights[A, B](es: Seq[Either[A, B]]): Seq[B] = es.collect { case Right(b) => b }

package hydra.lib

object pairs:
  /** Map over both elements of a pair. */
  def bimap[A, B, C, D](f: A => C)(g: B => D)(p: (A, B)): (C, D) = (f(p._1), g(p._2))

  /** Get the first element of a pair. */
  def first[A, B](p: (A, B)): A = p._1

  /** Get the second element of a pair. */
  def second[A, B](p: (A, B)): B = p._2

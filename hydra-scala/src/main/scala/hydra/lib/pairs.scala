package hydra.lib

object pairs:
  def bimap[A, B, C, D](f: A => C)(g: B => D)(p: (A, B)): (C, D) = (f(p._1), g(p._2))
  def first[A, B](p: (A, B)): A = p._1
  def second[A, B](p: (A, B)): B = p._2

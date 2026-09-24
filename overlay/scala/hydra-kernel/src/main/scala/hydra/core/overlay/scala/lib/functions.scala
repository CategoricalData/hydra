package hydra.core.overlay.scala.lib

object functions:
  def absurd[A, B](v: A): B = throw new IllegalStateException("hydra.core.lib.functions.absurd: void has no inhabitants")

  def identity[A](x: A): A = x

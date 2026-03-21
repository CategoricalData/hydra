package hydra.lib

object logic:
  def and(x: Boolean)(y: Boolean): Boolean = x && y
  def ifElse[A](cond: Boolean)(ifTrue: => A)(ifFalse: => A): A = if cond then ifTrue else ifFalse
  def not(x: Boolean): Boolean = !x
  def or(x: Boolean)(y: Boolean): Boolean = x || y

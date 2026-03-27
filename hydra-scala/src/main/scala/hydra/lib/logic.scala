package hydra.lib

object logic:
  /** Compute the logical AND of two boolean values. */
  def and(x: Boolean)(y: Boolean): Boolean = x && y

  /** Compute a conditional expression. */
  def ifElse[A](cond: Boolean)(ifTrue: => A)(ifFalse: => A): A = if cond then ifTrue else ifFalse

  /** Compute the logical NOT of a boolean value. */
  def not(x: Boolean): Boolean = !x

  /** Compute the logical OR of two boolean values. */
  def or(x: Boolean)(y: Boolean): Boolean = x || y

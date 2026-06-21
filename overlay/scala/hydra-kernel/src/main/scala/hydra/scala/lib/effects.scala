package hydra.scala.lib

/**
 * Scala implementations of hydra.lib.effects primitives (#494).
 *
 * The effect type is transparent in Scala: effect<t> is the same Scala type as t.
 * An effectful field of shape `unit -> effect<t>` becomes the Scala thunk `Unit => t`;
 * running the effect is forcing the thunk. These combinators therefore perform their
 * work eagerly, exactly as the Java and (transparent) Python hosts do.
 */
object effects:
  /** Lift a pure value into an effect (identity, since effects are transparent). */
  def pure[A](a: A): A = a

  /** Sequence two effectful computations: interpret a, then feed its result to f. */
  def bind[A, B](a: A)(f: A => B): B = f(a)

  /** Map a pure function over the result of an effect. */
  def map[A, B](f: A => B)(a: A): B = f(a)

  /** Applicative apply for effects. */
  def apply[A, B](f: A => B)(a: A): B = f(a)

  /** Kleisli composition for effects: compose(f, g, x) = g(f(x)). */
  def compose[A, B, C](f: A => B)(g: B => C)(x: A): C = g(f(x))

  /** Left-fold over a list with an effect-returning (curried) function, left to right. */
  def foldl[A, B](f: A => B => A)(acc: A)(xs: Seq[B]): A =
    xs.foldLeft(acc)((a, x) => f(a)(x))

  /** Map an effect-returning function over a list, collecting the results. */
  def mapList[A, B](f: A => B)(xs: Seq[A]): Seq[B] = xs.map(f)

  /** Map an effect-returning function over an optional. */
  def mapOptional[A, B](f: A => B)(opt: Option[A]): Option[B] = opt.map(f)

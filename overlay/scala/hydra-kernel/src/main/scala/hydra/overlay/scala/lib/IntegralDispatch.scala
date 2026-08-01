package hydra.overlay.scala.lib

/** Runtime dispatch for the constraint-polymorphic ('integral') primitives (div/mod/rem/even/odd).
  *
  * These primitives are registered with an `integral x => x -> x -> optional x` (div/mod/rem) or
  * `integral x => x -> boolean` (even/odd) type scheme, so generated code calls them as generic
  * `math.div[A](x)(y)` etc. with no evidence threaded from the caller (mirroring
  * `NumericDispatch`). The runtime integer type is discovered by dispatching on the boxed
  * argument's runtime class.
  *
  * div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows the
  * dividend) -- mirroring the Haskell/Java/Python/Clojure/Common-Lisp/Emacs-Lisp/Scheme/
  * TypeScript hosts' div/mod vs rem split. All three guard the zero-divisor case (returning
  * None) before computing. The (minBound, -1) boundary needs an explicit wrap-to-minBound on div
  * only; mod/rem have no overflow there.
  *
  * KNOWN GAP (same as NumericDispatch's add/sub/mul/negate/abs/signum): dispatch keys on the
  * boxed native type (Byte/Short/Int/Long/BigInt), which cannot distinguish uint8/16/32/64 from
  * the narrower signed type they're boxed as (uint8->Byte collides with int8, etc.) once erased.
  * This is not a NEW limitation introduced here -- NumericDispatch already has it for
  * add/sub/mul/negate/abs/signum -- so div/mod/rem/even/odd follow the same convention rather
  * than introducing an inconsistent special case.
  */
object IntegralDispatch:

  def applyNativeDiv[A](x: A, y: A): Option[A] = applyBinary("div", floorDiv, wrapMinBoundaryOnDiv = true, x, y)
  def applyNativeMod[A](x: A, y: A): Option[A] = applyBinary("mod", floorMod, wrapMinBoundaryOnDiv = false, x, y)
  def applyNativeRem[A](x: A, y: A): Option[A] = applyBinary("rem", (a, b) => a % b, wrapMinBoundaryOnDiv = false, x, y)

  def applyNativeEven[A](x: A): Boolean = toBigInt(x) % 2 == 0
  def applyNativeOdd[A](x: A): Boolean = toBigInt(x) % 2 != 0

  private def applyBinary[A](opName: String, op: (BigInt, BigInt) => BigInt, wrapMinBoundaryOnDiv: Boolean, x: A, y: A): Option[A] =
    (x.asInstanceOf[Any], y.asInstanceOf[Any]) match
      case (a: BigInt, b: BigInt) => guarded(opName, op, 0, signed = false, a, b, wrapMinBoundaryOnDiv).map(_.asInstanceOf[A])
      case (a: Byte, b: Byte) => guarded(opName, op, 8, signed = true, BigInt(a), BigInt(b), wrapMinBoundaryOnDiv).map(_.toByte.asInstanceOf[A])
      case (a: Short, b: Short) => guarded(opName, op, 16, signed = true, BigInt(a), BigInt(b), wrapMinBoundaryOnDiv).map(_.toShort.asInstanceOf[A])
      case (a: Int, b: Int) => guarded(opName, op, 32, signed = true, BigInt(a), BigInt(b), wrapMinBoundaryOnDiv).map(_.toInt.asInstanceOf[A])
      case (a: Long, b: Long) => guarded(opName, op, 64, signed = true, BigInt(a), BigInt(b), wrapMinBoundaryOnDiv).map(_.toLong.asInstanceOf[A])
      case (a, _) => throw new RuntimeException(s"hydra.lib.math.$opName: operand is not integral: $a")

  private def guarded(opName: String, op: (BigInt, BigInt) => BigInt, bits: Int, signed: Boolean, a: BigInt, b: BigInt, wrapMinBoundaryOnDiv: Boolean): Option[BigInt] =
    if b == 0 then None
    else if wrapMinBoundaryOnDiv && signed then
      val minBound = -(BigInt(1) << (bits - 1))
      if a == minBound && b == -1 then Some(minBound) else Some(op(a, b))
    else Some(op(a, b))

  private def floorDiv(a: BigInt, b: BigInt): BigInt =
    val q = a / b
    if (a % b != 0) && ((a < 0) != (b < 0)) then q - 1 else q

  private def floorMod(a: BigInt, b: BigInt): BigInt =
    val r = a % b
    if r != 0 && ((r < 0) != (b < 0)) then r + b else r

  private def toBigInt[A](x: A): BigInt =
    x.asInstanceOf[Any] match
      case a: BigInt => a
      case a: Byte => BigInt(a)
      case a: Short => BigInt(a)
      case a: Int => BigInt(a)
      case a: Long => BigInt(a)
      case a => throw new RuntimeException(s"hydra.lib.math.even/odd: operand is not integral: $a")

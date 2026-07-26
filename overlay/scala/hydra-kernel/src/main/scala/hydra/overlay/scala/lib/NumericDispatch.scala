package hydra.overlay.scala.lib

/** Runtime dispatch for the constraint-polymorphic ('numeric') arithmetic primitives
  * (add/sub/mul/negate).
  *
  * These primitives are registered with a `numeric x => x -> x -> x` type scheme (the constraint
  * is carried in the type for inference only), so generated code calls them as a generic
  * `math.add[A](x)(y)` with no `Numeric[A]` evidence threaded from the caller (Hydra's inference
  * resolves the constraint, not a Scala-level implicit). The runtime numeric type is therefore
  * discovered by dispatching on the boxed argument's runtime class, mirroring the Haskell host's
  * `numericBinary`/`numericUnary` (see `Hydra.Overlay.Haskell.Lib.Math`) and Java's
  * `NumericDispatch`.
  *
  * Integer arithmetic is performed in `BigInt` and narrowed back to the source variant's width,
  * giving two's-complement wraparound for the fixed-width types and arbitrary precision for
  * bigint. Float arithmetic is performed in IEEE 754 double/float per variant.
  *
  * Type inference guarantees both operands of a binary op share one `numeric` type, so the
  * dispatch keys on the first operand and requires the second to match; a mismatch or a
  * non-numeric operand is an internal invariant violation and fails loudly.
  */
object NumericDispatch:

  def applyNativeBinary[A](opName: String, x: A, y: A): A =
    (x.asInstanceOf[Any], y.asInstanceOf[Any]) match
      case (a: Double, b: Double) => applyFloatOp(opName, a, b).asInstanceOf[A]
      case (a: Float, b: Float) => applyFloat32Op(opName, a, b).asInstanceOf[A]
      case (a: BigInt, b: BigInt) => applyIntegerOp(opName, a, b).asInstanceOf[A]
      case (a: Byte, b: Byte) => wrapSigned(8, applyIntegerOp(opName, BigInt(a), BigInt(b))).toByte.asInstanceOf[A]
      case (a: Short, b: Short) => wrapSigned(16, applyIntegerOp(opName, BigInt(a), BigInt(b))).toShort.asInstanceOf[A]
      case (a: Int, b: Int) => wrapSigned(32, applyIntegerOp(opName, BigInt(a), BigInt(b))).toInt.asInstanceOf[A]
      case (a: Long, b: Long) => wrapSigned(64, applyIntegerOp(opName, BigInt(a), BigInt(b))).toLong.asInstanceOf[A]
      case (a, _) => throw new RuntimeException(s"hydra.lib.math.$opName: operand is not numeric: $a")

  def applyNativeUnary[A](opName: String, x: A): A =
    x.asInstanceOf[Any] match
      case a: Double => (-a).asInstanceOf[A]
      case a: Float => (-a).asInstanceOf[A]
      case a: BigInt => (-a).asInstanceOf[A]
      case a: Byte => wrapSigned(8, -BigInt(a)).toByte.asInstanceOf[A]
      case a: Short => wrapSigned(16, -BigInt(a)).toShort.asInstanceOf[A]
      case a: Int => wrapSigned(32, -BigInt(a)).toInt.asInstanceOf[A]
      case a: Long => wrapSigned(64, -BigInt(a)).toLong.asInstanceOf[A]
      case a => throw new RuntimeException(s"hydra.lib.math.$opName: operand is not numeric: $a")

  private def applyIntegerOp(opName: String, a: BigInt, b: BigInt): BigInt = opName match
    case "add" => a + b
    case "sub" => a - b
    case "mul" => a * b
    case _ => throw new RuntimeException(s"hydra.lib.math.$opName: unsupported integer op")

  private def applyFloatOp(opName: String, a: Double, b: Double): Double = opName match
    case "add" => a + b
    case "sub" => a - b
    case "mul" => a * b
    case _ => throw new RuntimeException(s"hydra.lib.math.$opName: unsupported float op")

  private def applyFloat32Op(opName: String, a: Float, b: Float): Float = opName match
    case "add" => a + b
    case "sub" => a - b
    case "mul" => a * b
    case _ => throw new RuntimeException(s"hydra.lib.math.$opName: unsupported float op")

  // Two's-complement wraparound narrowing back to the source width, mirroring Java's
  // NumericDispatch.rewrapInteger. Bigint (arbitrary precision) needs no narrowing.
  private def wrapSigned(bits: Int, r: BigInt): BigInt =
    val m = BigInt(1) << bits
    val w = ((r % m) + m) % m
    if w >= (m / 2) then w - m else w

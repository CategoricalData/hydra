package hydra.core.overlay.scala

import hydra.core.model.*
import hydra.core.graph.Primitive
// #473 Step 0: native primitive implementations live in package hydra.core.overlay.scala.lib
// (the analog of Haskell's Hydra.Haskell.Lib.*), leaving hydra.lib free for the generated
// PrimitiveDefinition def-modules. Import the impl objects so the bare references below
// (chars.isAlphaNum, lists.cons, …) still resolve.
import hydra.core.overlay.scala.lib.{chars, eithers, equality, functions, lists, literals, logic, maps, math, optionals, ordering, pairs, regex, sets, strings, text}

/** Registry of all primitive functions available in Hydra-Scala.
  * First-order primitives have real (native) implementations. Most higher-order
  * primitives also have term-level implementations that build application terms
  * for the reducer to evaluate. A few complex higher-order primitives (those
  * requiring intermediate evaluation, e.g. filter, find, bind) use stubs and
  * rely on eval elements.
  */
object Libraries:

  // ===== Infrastructure =====

  // Since #446 a Primitive's implementation carrier no longer threads the InferenceContext (cx);
  // it carries only the Graph and the argument terms. The Graph is still threaded because the
  // higher-order primitives that evaluate a function argument (via reduceTerm) need the live graph
  // to resolve primitive names.
  private type Impl = hydra.core.graph.Graph => Seq[Term] => Either[hydra.core.errors.Error, Term]

  private val stubImpl: Impl =
    _ => _ => Left(hydra.core.errors.Error.other("stub primitive"))

  private def ok(t: Term): Either[hydra.core.errors.Error, Term] = Right(t)

  private type E = Either[hydra.core.errors.Error, Term]

  // Placeholder InferenceContext for the reducer machinery used inside the higher-order prim impls.
  // Since #446 the implementation carrier no longer threads the InferenceContext, but reduceTerm
  // still takes one (used only for error context / fresh-variable bookkeeping, neither of which
  // matters for primitive reduction), so an empty cx is sufficient and correct. Mirrors Haskell's
  // primCx = emptyInferenceContext in Hydra.Core.Dsl.Prims.
  private val primCx: hydra.core.typing.InferenceContext = hydra.core.typing.InferenceContext(0, Seq.empty)

  // Reduce a term using the full reducer
  private def reduce(g: hydra.core.graph.Graph, t: Term): E =
    hydra.core.reduction.reduceTerm(primCx)(g)(true)(t)

  // Apply a function term to an argument and reduce
  private def applyAndReduce(g: hydra.core.graph.Graph, f: Term, x: Term): E =
    reduce(g, Term.application(Application(f, x)))

  // Apply a curried function to two arguments and reduce
  private def apply2AndReduce(g: hydra.core.graph.Graph, f: Term, x: Term, y: Term): E =
    reduce(g, Term.application(Application(Term.application(Application(f, x)), y)))

  private def impl0(t: => Term): Impl = _ => _ => ok(t)
  private def impl1(f: Term => Term): Impl = _ => args => ok(f(args(0)))
  private def impl2(f: (Term, Term) => Term): Impl = _ => args => ok(f(args(0), args(1)))
  private def impl3(f: (Term, Term, Term) => Term): Impl = _ => args => ok(f(args(0), args(1), args(2)))

  // --- Term extraction helpers ---

  private def exInt8(t: Term): Byte = t match
    case Term.literal(Literal.integer(IntegerValue.int8(n))) => n
    case _ => throw new RuntimeException(s"expected int8, got $t")

  private def exInt16(t: Term): Short = t match
    case Term.literal(Literal.integer(IntegerValue.int16(n))) => n
    case _ => throw new RuntimeException(s"expected int16, got $t")

  private def exInt32(t: Term): Int = t match
    case Term.literal(Literal.integer(IntegerValue.int32(n))) => n
    case _ => throw new RuntimeException(s"expected int32, got $t")

  private def exInt64(t: Term): Long = t match
    case Term.literal(Literal.integer(IntegerValue.int64(n))) => n
    case _ => throw new RuntimeException(s"expected int64, got $t")

  private def exUint8(t: Term): Byte = t match
    case Term.literal(Literal.integer(IntegerValue.uint8(n))) => n
    case _ => throw new RuntimeException(s"expected uint8, got $t")

  private def exUint16(t: Term): Int = t match
    case Term.literal(Literal.integer(IntegerValue.uint16(n))) => n
    case _ => throw new RuntimeException(s"expected uint16, got $t")

  private def exUint32(t: Term): Long = t match
    case Term.literal(Literal.integer(IntegerValue.uint32(n))) => n
    case _ => throw new RuntimeException(s"expected uint32, got $t")

  private def exUint64(t: Term): BigInt = t match
    case Term.literal(Literal.integer(IntegerValue.uint64(n))) => n
    case _ => throw new RuntimeException(s"expected uint64, got $t")

  private def exBigint(t: Term): BigInt = t match
    case Term.literal(Literal.integer(IntegerValue.bigint(n))) => n
    case _ => throw new RuntimeException(s"expected bigint, got $t")

  private def exFloat32(t: Term): Float = t match
    case Term.literal(Literal.float(FloatValue.float32(n))) => n
    case _ => throw new RuntimeException(s"expected float32, got $t")

  private def exFloat64(t: Term): Double = t match
    case Term.literal(Literal.float(FloatValue.float64(n))) => n
    case _ => throw new RuntimeException(s"expected float64, got $t")

  private def exDecimal(t: Term): BigDecimal = t match
    case Term.literal(Literal.decimal(n)) => n
    case _ => throw new RuntimeException(s"expected decimal, got $t")

  private def exString(t: Term): String = t match
    case Term.literal(Literal.string(s)) => s
    case _ => throw new RuntimeException(s"expected string, got $t")

  private def strip(t: Term): Term = t match
    case Term.annotated(at) => strip(at.body)
    case Term.typeApplication(ta) => strip(ta.body)
    case Term.typeLambda(tl) => strip(tl.body)
    case other => other

  private def exBool(t: Term): Boolean = strip(t) match
    case Term.literal(Literal.boolean(b)) => b
    case _ => throw new RuntimeException(s"expected boolean, got $t")

  private def exBinary(t: Term): String = t match
    case Term.literal(Literal.binary(b)) => b
    case _ => throw new RuntimeException(s"expected binary, got $t")

  private def exList(t: Term): Seq[Term] = strip(t) match
    case Term.list(items) => items
    case _ => throw new RuntimeException(s"expected list, got $t")

  private def exSet(t: Term): Set[Term] = strip(t) match
    case Term.set(items) => items
    case _ => throw new RuntimeException(s"expected set, got $t")

  private def exMap(t: Term): Map[Term, Term] = strip(t) match
    case Term.map(entries) => entries
    case _ => throw new RuntimeException(s"expected map, got $t")

  private def exMaybe(t: Term): Option[Term] = strip(t) match
    case Term.optional(opt) => opt
    case _ => throw new RuntimeException(s"expected maybe, got $t")

  private def exEither(t: Term): Either[Term, Term] = strip(t) match
    case Term.either(e) => e
    case _ => throw new RuntimeException(s"expected either, got $t")

  private def exPair(t: Term): (Term, Term) = strip(t) match
    case Term.pair(p) => p
    case _ => throw new RuntimeException(s"expected pair, got $t")

  // --- Term construction helpers ---

  private def mkInt8(n: Byte): Term = Term.literal(Literal.integer(IntegerValue.int8(n)))
  private def mkInt16(n: Short): Term = Term.literal(Literal.integer(IntegerValue.int16(n)))
  private def mkInt32(n: Int): Term = Term.literal(Literal.integer(IntegerValue.int32(n)))
  private def mkInt64(n: Long): Term = Term.literal(Literal.integer(IntegerValue.int64(n)))
  private def mkUint8(n: Byte): Term = Term.literal(Literal.integer(IntegerValue.uint8(n)))
  private def mkUint16(n: Int): Term = Term.literal(Literal.integer(IntegerValue.uint16(n)))
  private def mkUint32(n: Long): Term = Term.literal(Literal.integer(IntegerValue.uint32(n)))
  private def mkUint64(n: BigInt): Term = Term.literal(Literal.integer(IntegerValue.uint64(n)))
  private def mkBigint(n: BigInt): Term = Term.literal(Literal.integer(IntegerValue.bigint(n)))
  private def mkFloat32(n: Float): Term = Term.literal(Literal.float(FloatValue.float32(n)))
  private def mkFloat64(n: Double): Term = Term.literal(Literal.float(FloatValue.float64(n)))
  private def mkDecimal(n: BigDecimal): Term = Term.literal(Literal.decimal(n))
  private def mkString(s: String): Term = Term.literal(Literal.string(s))
  private def mkBool(b: Boolean): Term = Term.literal(Literal.boolean(b))
  private def mkBinary(b: String): Term = Term.literal(Literal.binary(b))
  private def mkList(items: Seq[Term]): Term = Term.list(items)
  private def mkSet(items: Set[Term]): Term = Term.set(items)
  private def mkMapTerm(entries: Map[Term, Term]): Term = Term.map(entries)
  private def mkMaybe(opt: Option[Term]): Term = Term.optional(opt)
  private def mkEither(e: Either[Term, Term]): Term = Term.either(e)
  private def mkPairTerm(a: Term, b: Term): Term = Term.pair((a, b))
  private val mkUnit: Term = Term.unit

  /** Apply a function term to an argument term. The reducer will evaluate the result. */
  private def app(f: Term, x: Term): Term = Term.application(Application(f, x))

  /** Apply a curried 2-argument function: f(x)(y). */
  private def app2(f: Term, x: Term, y: Term): Term = app(app(f, x), y)

  // --- Higher-order traversal helpers ---

  /** Apply predicate to each element, collecting those where it returns true. */
  private def filterList(g: hydra.core.graph.Graph, p: Term, xs: Seq[Term]): E =
    xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
      for {
        acc <- accE
        result <- applyAndReduce(g, p, x)
      } yield if exBool(result) then mkList(exList(acc) :+ x) else acc
    }

  /** Apply predicate to each element of a set, keeping only those for which it holds. */
  private def filterSet(g: hydra.core.graph.Graph, p: Term, xs: Set[Term]): E =
    xs.foldLeft[E](ok(mkSet(Set.empty))) { (accE, x) =>
      for {
        acc <- accE
        result <- applyAndReduce(g, p, x)
      } yield if exBool(result) then mkSet(exSet(acc) + x) else acc
    }

  /** Apply predicate to each element, partitioning into (true, false). */
  private def partitionList(g: hydra.core.graph.Graph, p: Term, xs: Seq[Term]): E =
    xs.foldLeft[E](ok(mkPairTerm(mkList(Seq.empty), mkList(Seq.empty)))) { (accE, x) =>
      for {
        acc <- accE
        result <- applyAndReduce(g, p, x)
      } yield {
        val (ts, fs) = exPair(acc)
        if exBool(result) then mkPairTerm(mkList(exList(ts) :+ x), fs)
        else mkPairTerm(ts, mkList(exList(fs) :+ x))
      }
    }


  // --- Constraint-polymorphic ('numeric') dispatch for add/sub/mul/negate ---
  //
  // These primitives are registered with a 'numeric' class constraint and identity (Term) coders,
  // so the runtime numeric type is discovered by dispatching on the operand's literal variant,
  // mirroring the Haskell host's numericBinary/numericUnary (Hydra.Core.Overlay.Haskell.Lib.Math) and
  // Java's NumericDispatch. No typeclass mechanism is consulted at runtime — the host has none.
  // Type inference guarantees both operands of a binary op share one numeric type, so the dispatch
  // keys on the first operand and requires the second to match; a mismatch or a non-numeric
  // operand is an internal invariant violation and fails loudly.
  private def numericBinary(opName: String, opInt: (BigInt, BigInt) => BigInt, opFloat: (Double, Double) => Double)(x: Term, y: Term): Term =
    (strip(x), strip(y)) match
      case (Term.literal(Literal.integer(ix)), Term.literal(Literal.integer(iy))) =>
        Term.literal(Literal.integer(integerBinary(opName, opInt, ix, iy)))
      case (Term.literal(Literal.float(fx)), Term.literal(Literal.float(fy))) =>
        Term.literal(Literal.float(floatBinary(opName, opFloat, fx, fy)))
      case _ => throw new RuntimeException(s"hydra.core.lib.math.$opName: operands are not the same numeric kind")

  private def numericUnary(opName: String, opInt: BigInt => BigInt, opFloat: Double => Double)(x: Term): Term =
    strip(x) match
      case Term.literal(Literal.integer(ix)) => Term.literal(Literal.integer(integerUnary(opInt, ix)))
      case Term.literal(Literal.float(fx)) => Term.literal(Literal.float(floatUnary(opFloat, fx)))
      case _ => throw new RuntimeException(s"hydra.core.lib.math.$opName: operand is not numeric")

  private def integerBinary(opName: String, op: (BigInt, BigInt) => BigInt, ix: IntegerValue, iy: IntegerValue): IntegerValue =
    (ix, iy) match
      case (IntegerValue.bigint(a), IntegerValue.bigint(b)) => IntegerValue.bigint(op(a, b))
      case (IntegerValue.int8(a), IntegerValue.int8(b)) => IntegerValue.int8(wrapSigned(8, op(BigInt(a), BigInt(b))).toByte)
      case (IntegerValue.int16(a), IntegerValue.int16(b)) => IntegerValue.int16(wrapSigned(16, op(BigInt(a), BigInt(b))).toShort)
      case (IntegerValue.int32(a), IntegerValue.int32(b)) => IntegerValue.int32(wrapSigned(32, op(BigInt(a), BigInt(b))).toInt)
      case (IntegerValue.int64(a), IntegerValue.int64(b)) => IntegerValue.int64(wrapSigned(64, op(BigInt(a), BigInt(b))).toLong)
      case (IntegerValue.uint8(a), IntegerValue.uint8(b)) => IntegerValue.uint8(wrapUnsigned(8, op(BigInt(a & 0xff), BigInt(b & 0xff))).toByte)
      case (IntegerValue.uint16(a), IntegerValue.uint16(b)) => IntegerValue.uint16(wrapUnsigned(16, op(BigInt(a), BigInt(b))).toInt)
      case (IntegerValue.uint32(a), IntegerValue.uint32(b)) => IntegerValue.uint32(wrapUnsigned(32, op(BigInt(a), BigInt(b))).toLong)
      case (IntegerValue.uint64(a), IntegerValue.uint64(b)) => IntegerValue.uint64(wrapUnsigned(64, op(a, b)))
      case _ => throw new RuntimeException(s"hydra.core.lib.math.$opName: integer operands differ in precision")

  private def integerUnary(op: BigInt => BigInt, iv: IntegerValue): IntegerValue = iv match
    case IntegerValue.bigint(a) => IntegerValue.bigint(op(a))
    case IntegerValue.int8(a) => IntegerValue.int8(wrapSigned(8, op(BigInt(a))).toByte)
    case IntegerValue.int16(a) => IntegerValue.int16(wrapSigned(16, op(BigInt(a))).toShort)
    case IntegerValue.int32(a) => IntegerValue.int32(wrapSigned(32, op(BigInt(a))).toInt)
    case IntegerValue.int64(a) => IntegerValue.int64(wrapSigned(64, op(BigInt(a))).toLong)
    case IntegerValue.uint8(a) => IntegerValue.uint8(wrapUnsigned(8, op(BigInt(a & 0xff))).toByte)
    case IntegerValue.uint16(a) => IntegerValue.uint16(wrapUnsigned(16, op(BigInt(a))).toInt)
    case IntegerValue.uint32(a) => IntegerValue.uint32(wrapUnsigned(32, op(a)).toLong)
    case IntegerValue.uint64(a) => IntegerValue.uint64(wrapUnsigned(64, op(a)))

  private def floatBinary(opName: String, op: (Double, Double) => Double, fx: FloatValue, fy: FloatValue): FloatValue =
    (fx, fy) match
      case (FloatValue.float32(a), FloatValue.float32(b)) => FloatValue.float32(op(a, b).toFloat)
      case (FloatValue.float64(a), FloatValue.float64(b)) => FloatValue.float64(op(a, b))
      case _ => throw new RuntimeException(s"hydra.core.lib.math.$opName: float operands differ in precision")

  private def floatUnary(op: Double => Double, fv: FloatValue): FloatValue = fv match
    case FloatValue.float32(a) => FloatValue.float32(op(a).toFloat)
    case FloatValue.float64(a) => FloatValue.float64(op(a))

  // Two's-complement wraparound narrowing back to the source width, mirroring Java's
  // NumericDispatch.rewrapInteger. Bigint (arbitrary precision) needs no narrowing.
  private def wrapSigned(bits: Int, r: BigInt): BigInt =
    val m = BigInt(1) << bits
    val w = ((r % m) + m) % m
    if w >= (m / 2) then w - m else w

  private def wrapUnsigned(bits: Int, r: BigInt): BigInt =
    val m = BigInt(1) << bits
    ((r % m) + m) % m

  // --- Constraint-polymorphic ('fractional') division and ('integral') div/mod/rem/even/odd ---
  //
  // Mirror the numeric dispatch above: identity (Term) coders, dispatch on the operand's literal
  // variant. div/mod are floor-based (sign follows the divisor); rem is truncated (sign follows
  // the dividend) — matching the Haskell/Java/Python hosts' div/mod vs rem split. All three guard
  // the zero-divisor case (returning None) before computing. The (minBound, -1) boundary needs an
  // explicit wrap-to-minBound on div only (mirroring the Haskell/Java hosts); mod/rem have no
  // overflow there (the remainder is always representable).

  private def divideTerm(x: Term, y: Term): Term =
    (strip(x), strip(y)) match
      case (Term.literal(Literal.float(fx)), Term.literal(Literal.float(fy))) =>
        Term.literal(Literal.float(floatBinary("divide", (_ / _), fx, fy)))
      case _ => throw new RuntimeException("hydra.core.lib.math.divide: operands are not the same fractional kind")

  private def integralBinaryMaybe(opName: String, op: (BigInt, BigInt) => BigInt, wrapMinBoundaryOnDiv: Boolean)(x: Term, y: Term): Option[Term] =
    (strip(x), strip(y)) match
      case (Term.literal(Literal.integer(ix)), Term.literal(Literal.integer(iy))) =>
        integralBinary(opName, op, wrapMinBoundaryOnDiv, ix, iy).map(iv => Term.literal(Literal.integer(iv)))
      case _ => throw new RuntimeException(s"hydra.core.lib.math.$opName: operands are not the same integral kind")

  private def integralBinary(opName: String, op: (BigInt, BigInt) => BigInt, wrapMinBoundaryOnDiv: Boolean, ix: IntegerValue, iy: IntegerValue): Option[IntegerValue] =
    def guarded(bits: Int, signed: Boolean, a: BigInt, b: BigInt): Option[BigInt] =
      if b == 0 then None
      else if wrapMinBoundaryOnDiv && signed then
        val m = BigInt(1) << bits
        val minBound = -(m / 2)
        if a == minBound && b == -1 then Some(minBound) else Some(op(a, b))
      else Some(op(a, b))
    (ix, iy) match
      case (IntegerValue.bigint(a), IntegerValue.bigint(b)) => guarded(0, false, a, b).map(IntegerValue.bigint(_))
      case (IntegerValue.int8(a), IntegerValue.int8(b)) => guarded(8, true, BigInt(a), BigInt(b)).map(r => IntegerValue.int8(wrapSigned(8, r).toByte))
      case (IntegerValue.int16(a), IntegerValue.int16(b)) => guarded(16, true, BigInt(a), BigInt(b)).map(r => IntegerValue.int16(wrapSigned(16, r).toShort))
      case (IntegerValue.int32(a), IntegerValue.int32(b)) => guarded(32, true, BigInt(a), BigInt(b)).map(r => IntegerValue.int32(wrapSigned(32, r).toInt))
      case (IntegerValue.int64(a), IntegerValue.int64(b)) => guarded(64, true, BigInt(a), BigInt(b)).map(r => IntegerValue.int64(wrapSigned(64, r).toLong))
      case (IntegerValue.uint8(a), IntegerValue.uint8(b)) => guarded(8, false, BigInt(a & 0xff), BigInt(b & 0xff)).map(r => IntegerValue.uint8(wrapUnsigned(8, r).toByte))
      case (IntegerValue.uint16(a), IntegerValue.uint16(b)) => guarded(16, false, BigInt(a), BigInt(b)).map(r => IntegerValue.uint16(wrapUnsigned(16, r).toInt))
      case (IntegerValue.uint32(a), IntegerValue.uint32(b)) => guarded(32, false, BigInt(a), BigInt(b)).map(r => IntegerValue.uint32(wrapUnsigned(32, r).toLong))
      case (IntegerValue.uint64(a), IntegerValue.uint64(b)) => guarded(64, false, a, b).map(r => IntegerValue.uint64(wrapUnsigned(64, r)))
      case _ => throw new RuntimeException(s"hydra.core.lib.math.$opName: integer operands differ in precision")

  private def floorDiv(a: BigInt, b: BigInt): BigInt =
    val q = a / b
    if (a % b != 0) && ((a < 0) != (b < 0)) then q - 1 else q

  private def floorMod(a: BigInt, b: BigInt): BigInt =
    val r = a % b
    if r != 0 && ((r < 0) != (b < 0)) then r + b else r

  private def divTerm(x: Term, y: Term): Option[Term] = integralBinaryMaybe("div", floorDiv, wrapMinBoundaryOnDiv = true)(x, y)
  private def modTerm(x: Term, y: Term): Option[Term] = integralBinaryMaybe("mod", floorMod, wrapMinBoundaryOnDiv = false)(x, y)
  private def remTerm(x: Term, y: Term): Option[Term] = integralBinaryMaybe("rem", (_ % _), wrapMinBoundaryOnDiv = false)(x, y)

  private def integralToBigInt(v: IntegerValue): BigInt = v match
    case IntegerValue.bigint(a) => a
    case IntegerValue.int8(a) => BigInt(a)
    case IntegerValue.int16(a) => BigInt(a)
    case IntegerValue.int32(a) => BigInt(a)
    case IntegerValue.int64(a) => BigInt(a)
    case IntegerValue.uint8(a) => BigInt(a & 0xff)
    case IntegerValue.uint16(a) => BigInt(a)
    case IntegerValue.uint32(a) => BigInt(a)
    case IntegerValue.uint64(a) => a

  private def evenTerm(x: Term): Boolean = strip(x) match
    case Term.literal(Literal.integer(iv)) => integralToBigInt(iv) % 2 == 0
    case _ => throw new RuntimeException("hydra.core.lib.math.even: operand is not integral")

  private def oddTerm(x: Term): Boolean = strip(x) match
    case Term.literal(Literal.integer(iv)) => integralToBigInt(iv) % 2 != 0
    case _ => throw new RuntimeException("hydra.core.lib.math.odd: operand is not integral")

  private def mkComparison(c: hydra.core.util.Comparison): Term =
    val fieldName = c match
      case hydra.core.util.Comparison.lessThan => "lessThan"
      case hydra.core.util.Comparison.equalTo => "equalTo"
      case hydra.core.util.Comparison.greaterThan => "greaterThan"
    Term.inject(Injection("hydra.core.util.Comparison", Field(fieldName, mkUnit)))

  // --- Primitive constructors ---

  private def mkPrimDef(name: String, ts: TypeScheme, isPure: Boolean = true, isTotal: Boolean = true): hydra.core.packaging.PrimitiveDefinition =
    hydra.core.packaging.PrimitiveDefinition(
      name,
      None,
      hydra.core.scoping.typeSchemeToTermSignature(ts),
      isPure,
      isTotal,
      hydra.core.lib.defaults.defaultImplementations.get(name))

  private def mkPrim(name: String, ts: TypeScheme): Primitive =
    Primitive(mkPrimDef(name, ts), stubImpl)

  private def mkPrimImpl(name: String, ts: TypeScheme, impl: Impl): Primitive =
    Primitive(mkPrimDef(name, ts), impl)

  // A primitive with no native Scala implementation, but which declares a portable
  // defaultImplementation term (see hydra.core.lib.defaults.defaultImplementations). Its implementation
  // folds call args into an Application chain over the term and evaluates via reduceTerm, rather
  // than running hand-written Scala logic.
  //
  // Note: the default term is already real and directly reducible (confirmed by reading
  // defaults.scala's entries — hydra.core.model.Term.lambda(...), not an encoded/reified term-as-data
  // requiring a decode step). Mirrors the Java/Python fallback (#609 Stage 2/3).
  private def mkPrimDefaultFallback(name: String, ts: TypeScheme): Primitive =
    val defaultImpl = hydra.core.lib.defaults.defaultImplementations.getOrElse(name,
      throw new RuntimeException(s"mkPrimDefaultFallback: no defaultImplementation for $name"))
    val impl: Impl = g => args =>
      val applied = args.foldLeft(defaultImpl)((fn, arg) => Term.application(Application(fn, arg)))
      reduce(g, applied)
    Primitive(mkPrimDef(name, ts), impl)

  // Primitives which have no native Scala implementation, but do declare a portable
  // defaultImplementation term. Spike (#609 Stage 3) wired lists.takeWhile, mirroring the
  // Java/Python validation case; #749 added equality.notEqual and functions.{const,flip}. 8
  // Group-A names remain: eithers.{apply,compose,pure}, functions.compose,
  // optionals.{foldList,mapList,mapSet}, sets.filter.
  private def defaultFallbackPrimitives(alreadyNative: Set[String]): Map[String, Primitive] =
    val x = tVar("x")
    val xEq = Seq(("x", Seq("equality")))
    val t1 = tVar("t1")
    val t2 = tVar("t2")
    val t3 = tVar("t3")
    val candidates: Seq[(String, TypeScheme)] = Seq(
      hydra.core.lib.lists.takeWhile.name -> tScheme(Seq("x"), tFun(tFun(x, tBool), tFun(tList(x), tList(x)))),
      hydra.core.lib.equality.notEqual.name -> tSchemeConstrained(xEq, tFun(x, tFun(x, tBool))),
      hydra.core.lib.functions.const.name -> tScheme(Seq("t1", "t2"), tFun(t1, tFun(t2, t1))),
      hydra.core.lib.functions.flip.name -> tScheme(Seq("t1", "t2", "t3"), tFun(tFun(t1, tFun(t2, t3)), tFun(t2, tFun(t1, t3)))),
    )
    candidates
      .filterNot((name, _) => alreadyNative.contains(name))
      .filter((name, _) => hydra.core.lib.defaults.defaultImplementations.contains(name))
      .map((name, ts) => name -> mkPrimDefaultFallback(name, ts))
      .toMap

  // Effectful primitives (hydra.lib.{effects,files,system}.*) are impure and non-total: they perform
  // real I/O / observable side effects, so the test framework must NOT constant-fold them (doing so
  // breaks the effectful test cases: time/file/getEnvironment). The kernel PrimitiveDefinition carries
  // isPure=false for these (dist/json .../hydra/lib/{files,system,effects}.json), but the Scala registry
  // reconstructs the def from the name, so it must set purity explicitly. Mirrors the Java host's
  // isPure()->false overrides (26d8219c51, For #494). Fixes the 4 scala→typescript effectful failures.
  private def mkPrimEffect(name: String, ts: TypeScheme): Primitive =
    Primitive(mkPrimDef(name, ts, isPure = false, isTotal = false), stubImpl)

  // Mark the given (0-based) parameter positions as lazy. The Lisp coder
  // reads parameterIsLazy to decide which arguments to wrap in `(fn [] ...)`
  // thunks; without this, higher-order primitives like optionals.match emit
  // strict recursive calls that blow the stack (#453). Mirrors Haskell's
  // lazySig / markLazyParams in Hydra.Sources.Kernel.Lib.{Optionals,Logic,...}.
  private def withLazy(prim: Primitive, idxs: Seq[Int]): Primitive =
    val sig = prim.definition.signature
    val params = sig.parameters.zipWithIndex.map { (p, i) =>
      if idxs.contains(i) then p.copy(isLazy = true) else p
    }
    val sig2 = sig.copy(parameters = params)
    val def2 = prim.definition.copy(signature = sig2)
    prim.copy(definition = def2)

  // Type construction helpers
  private def tVar(n: String): Type = Type.variable(n)
  private def tFun(d: Type, c: Type): Type = Type.function(FunctionType(d, c))
  private def tList(t: Type): Type = Type.list(t)
  private def tSet(t: Type): Type = Type.set(t)
  private def tMap(k: Type, v: Type): Type = Type.map(MapType(k, v))
  private def tOpt(t: Type): Type = Type.optional(t)
  private def tEither(l: Type, r: Type): Type = Type.either(EitherType(l, r))
  private def tPair(a: Type, b: Type): Type = Type.pair(PairType(a, b))
  private def tEffect(t: Type): Type = Type.effect(t)
  private val tFilePath: Type = Type.variable("hydra.core.file.FilePath")
  private val tFileError: Type = Type.variable("hydra.core.error.file.FileError")
  private val tFileStatus: Type = Type.variable("hydra.core.file.FileStatus")
  private val tCommand: Type = Type.variable("hydra.core.system.Command")
  private val tSystemError: Type = Type.variable("hydra.core.error.system.SystemError")
  private val tProcessResult: Type = Type.variable("hydra.core.system.ProcessResult")
  private val tStatusCode: Type = Type.variable("hydra.core.system.StatusCode")
  private val tTimespec: Type = Type.variable("hydra.core.time.Timespec")
  private val tEnvironmentVariable: Type = Type.variable("hydra.core.system.EnvironmentVariable")
  private val tString: Type = Type.literal(LiteralType.string)
  private val tBool: Type = Type.literal(LiteralType.boolean)
  private val tBinary: Type = Type.literal(LiteralType.binary)
  private val tInt8: Type = Type.literal(LiteralType.integer(IntegerType.int8))
  private val tInt16: Type = Type.literal(LiteralType.integer(IntegerType.int16))
  private val tInt32: Type = Type.literal(LiteralType.integer(IntegerType.int32))
  private val tInt64: Type = Type.literal(LiteralType.integer(IntegerType.int64))
  private val tUint8: Type = Type.literal(LiteralType.integer(IntegerType.uint8))
  private val tUint16: Type = Type.literal(LiteralType.integer(IntegerType.uint16))
  private val tUint32: Type = Type.literal(LiteralType.integer(IntegerType.uint32))
  private val tUint64: Type = Type.literal(LiteralType.integer(IntegerType.uint64))
  private val tBigint: Type = Type.literal(LiteralType.integer(IntegerType.bigint))
  private val tFloat32: Type = Type.literal(LiteralType.float(FloatType.float32))
  private val tFloat64: Type = Type.literal(LiteralType.float(FloatType.float64))
  private val tDecimal: Type = Type.literal(LiteralType.decimal)
  private val tUnit: Type = Type.unit
  private val tVoid: Type = Type.void
  private val tComparison: Type = Type.variable("hydra.core.util.Comparison")

  private def tScheme(vars: Seq[String], t: Type): TypeScheme = TypeScheme(vars, t, Map.empty)
  private def tMono(t: Type): TypeScheme = tScheme(Seq.empty, t)

  private def tSchemeConstrained(vars: Seq[(String, Seq[String])], t: Type): TypeScheme =
    val varNames = vars.map(_._1)
    val constraints = vars.collect { case (name, classes) if classes.nonEmpty =>
      name -> TypeVariableConstraints(classes.map(c => TypeClassConstraint.simple(c)).toSet)
    }.toMap
    TypeScheme(varNames, t, constraints)

  // ===== Chars primitives =====

  private def charsPrimitives(): Map[String, Primitive] =
    Map(
      hydra.core.lib.chars.isAlphaNum.name -> mkPrimImpl(hydra.core.lib.chars.isAlphaNum.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isAlphaNum(exInt32(a))))),
      hydra.core.lib.chars.isLower.name -> mkPrimImpl(hydra.core.lib.chars.isLower.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isLower(exInt32(a))))),
      hydra.core.lib.chars.isSpace.name -> mkPrimImpl(hydra.core.lib.chars.isSpace.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isSpace(exInt32(a))))),
      hydra.core.lib.chars.isUpper.name -> mkPrimImpl(hydra.core.lib.chars.isUpper.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isUpper(exInt32(a))))),
      hydra.core.lib.chars.toLower.name -> mkPrimImpl(hydra.core.lib.chars.toLower.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(chars.toLower(exInt32(a))))),
      hydra.core.lib.chars.toUpper.name -> mkPrimImpl(hydra.core.lib.chars.toUpper.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(chars.toUpper(exInt32(a))))),
    )

  // ===== Equality primitives =====

  private def equalityPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val xEq = Seq(("x", Seq("equality")))
    Map(
      // equal only here (moved: compare/gt/gte/lt/lte/max/min -> ordering; identity -> functions).
      // These work on Term values directly since they are polymorphic, so equal delegates to
      // ordering.compareTerms (equal iff compare == 0, per docs/specification/ordering-and-
      // equality.md) rather than plain Term structural equality (a == b): the latter is
      // scale-BLIND for decimals (Literal.decimal wraps a scala.math.BigDecimal, whose own
      // equals/hashCode delegate to compareTo unlike java.math.BigDecimal, so "1.1" == "1.10"
      // would wrongly be true -- #727/#719) and IEEE-754-native rather than Hydra's extended
      // totalOrder for floats (NaN != NaN; -0.0 == 0.0 -- #745). compareTerms already handles
      // both correctly (and recursively, so a decimal/float nested inside a map/set/record
      // value is covered too, not just a bare top-level literal).
      hydra.core.lib.equality.equal.name -> mkPrimImpl(hydra.core.lib.equality.equal.name, tSchemeConstrained(xEq, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(ordering.compareTerms(a, b) == 0))),
    )

  // ===== Ordering primitives (moved from equality — R20) =====

  private def orderingPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val xOrd = Seq(("x", Seq("ordering")))
    Map(
      // compare, gt, gte, lt, lte, max, min. Use compareTerms for structural comparison of values.
      hydra.core.lib.ordering.compare.name -> mkPrimImpl(hydra.core.lib.ordering.compare.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tComparison))),
        impl2 { (a, b) =>
          val c = ordering.compareTerms(a, b)
          val comp = if c < 0 then hydra.core.util.Comparison.lessThan
                     else if c > 0 then hydra.core.util.Comparison.greaterThan
                     else hydra.core.util.Comparison.equalTo
          mkComparison(comp)
        }),
      hydra.core.lib.ordering.gt.name -> mkPrimImpl(hydra.core.lib.ordering.gt.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(ordering.compareTerms(a, b) > 0))),
      hydra.core.lib.ordering.gte.name -> mkPrimImpl(hydra.core.lib.ordering.gte.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(ordering.compareTerms(a, b) >= 0))),
      hydra.core.lib.ordering.lt.name -> mkPrimImpl(hydra.core.lib.ordering.lt.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(ordering.compareTerms(a, b) < 0))),
      hydra.core.lib.ordering.lte.name -> mkPrimImpl(hydra.core.lib.ordering.lte.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(ordering.compareTerms(a, b) <= 0))),
      hydra.core.lib.ordering.max.name -> mkPrimImpl(hydra.core.lib.ordering.max.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, x))),
        impl2((a, b) => if ordering.compareTerms(a, b) >= 0 then a else b)),
      hydra.core.lib.ordering.min.name -> mkPrimImpl(hydra.core.lib.ordering.min.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, x))),
        impl2((a, b) => if ordering.compareTerms(a, b) <= 0 then a else b)),
    )

  // ===== Functions primitives (identity moved from equality — R21) =====

  private def functionsPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val xPlain = Seq(("x", Seq.empty))
    Map(
      hydra.core.lib.functions.absurd.name -> mkPrimImpl(hydra.core.lib.functions.absurd.name, tSchemeConstrained(xPlain, tFun(tVoid, x)),
        impl1(_ => throw new IllegalStateException("hydra.core.lib.functions.absurd: void has no inhabitants"))),
      hydra.core.lib.functions.identity.name -> mkPrimImpl(hydra.core.lib.functions.identity.name, tSchemeConstrained(xPlain, tFun(x, x)),
        impl1(a => a)),
    )

  // ===== Eithers primitives =====

  private def eithersPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val y = tVar("y")
    val z = tVar("z")
    val w = tVar("w")
    Map(
      // Higher-order: bind, bimap, either, foldList, map, mapList, mapOptional, mapSet
      hydra.core.lib.eithers.bind.name -> mkPrimImpl(hydra.core.lib.eithers.bind.name, tScheme(Seq("x", "y", "z"),
        tFun(tEither(x, y), tFun(tFun(y, tEither(x, z)), tEither(x, z)))),
        impl2 { (e, f) =>
          exEither(e) match
            case Left(_) => e
            case Right(v) => app(f, v)
        }),
      hydra.core.lib.eithers.bimap.name -> mkPrimImpl(hydra.core.lib.eithers.bimap.name, tScheme(Seq("x", "y", "z", "w"),
        tFun(tFun(x, z), tFun(tFun(y, w), tFun(tEither(x, y), tEither(z, w))))),
        impl3 { (fl, fr, e) =>
          exEither(e) match
            case Left(a) => mkEither(Left(app(fl, a)))
            case Right(b) => mkEither(Right(app(fr, b)))
        }),
      hydra.core.lib.eithers.either.name -> mkPrimImpl(hydra.core.lib.eithers.either.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, z), tFun(tFun(y, z), tFun(tEither(x, y), z)))),
        impl3 { (fl, fr, e) =>
          exEither(e) match
            case Left(a) => app(fl, a)
            case Right(b) => app(fr, b)
        }),
      hydra.core.lib.eithers.foldList.name -> mkPrimImpl(hydra.core.lib.eithers.foldList.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tFun(y, tEither(z, x))), tFun(x, tFun(tList(y), tEither(z, x))))),
        g => args => {
          val f = args(0); val init = args(1); val xs = exList(args(2))
          xs.foldLeft[E](ok(init)) { (accE, elem) =>
            accE.flatMap(acc => apply2AndReduce(g, f, acc, elem))
          }
        }),
      // First-order: isLeft, isRight, lefts, rights, partition
      hydra.core.lib.eithers.isLeft.name -> mkPrimImpl(hydra.core.lib.eithers.isLeft.name, tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool)),
        impl1(e => mkBool(exEither(e).isLeft))),
      hydra.core.lib.eithers.isRight.name -> mkPrimImpl(hydra.core.lib.eithers.isRight.name, tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool)),
        impl1(e => mkBool(exEither(e).isRight))),
      hydra.core.lib.eithers.left.name -> mkPrimImpl(hydra.core.lib.eithers.left.name, tScheme(Seq("x", "y"),
        tFun(x, tEither(x, y))),
        impl1(v => mkEither(Left(v)))),
      hydra.core.lib.eithers.lefts.name -> mkPrimImpl(hydra.core.lib.eithers.lefts.name, tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(x))),
        impl1(es => mkList(exList(es).collect { case t if exEither(t).isLeft => exEither(t).left.toOption.get }))),
      hydra.core.lib.eithers.map.name -> mkPrimImpl(hydra.core.lib.eithers.map.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, y), tFun(tEither(z, x), tEither(z, y)))),
        impl2 { (f, e) =>
          exEither(e) match
            case Left(_) => e
            case Right(v) => mkEither(Right(app(f, v)))
        }),
      hydra.core.lib.eithers.mapList.name -> mkPrimImpl(hydra.core.lib.eithers.mapList.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tList(x), tEither(z, tList(y))))),
        g => args => {
          val f = args(0); val xs = exList(args(1))
          xs.foldLeft[E](ok(mkEither(Right(mkList(Seq.empty))))) { (accE, elem) =>
            accE.flatMap { acc =>
              exEither(acc) match
                case Left(_) => ok(acc)
                case Right(soFar) =>
                  applyAndReduce(g, f, elem).map { result =>
                    exEither(result) match
                      case Left(err) => mkEither(Left(err))
                      case Right(v) => mkEither(Right(mkList(exList(soFar) :+ v)))
                  }
            }
          }
        }),
      hydra.core.lib.eithers.mapOptional.name -> mkPrimImpl(hydra.core.lib.eithers.mapOptional.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tOpt(x), tEither(z, tOpt(y))))),
        g => args => {
          val f = args(0); val mx = exMaybe(args(1))
          mx match
            case None => ok(mkEither(Right(mkMaybe(None))))
            case Some(x) =>
              applyAndReduce(g, f, x).map { result =>
                exEither(result) match
                  case Left(err) => mkEither(Left(err))
                  case Right(v) => mkEither(Right(mkMaybe(Some(v))))
              }
        }),
      hydra.core.lib.eithers.mapSet.name -> mkPrimImpl(hydra.core.lib.eithers.mapSet.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tSet(x), tEither(z, tSet(y))))),
        g => args => {
          val f = args(0); val xs = exSet(args(1)).toSeq
          xs.foldLeft[E](ok(mkEither(Right(mkSet(Set.empty))))) { (accE, elem) =>
            accE.flatMap { acc =>
              exEither(acc) match
                case Left(_) => ok(acc)
                case Right(soFar) =>
                  applyAndReduce(g, f, elem).map { result =>
                    exEither(result) match
                      case Left(err) => mkEither(Left(err))
                      case Right(v) => mkEither(Right(mkSet(exSet(soFar) + v)))
                  }
            }
          }
        }),
      hydra.core.lib.eithers.partition.name -> mkPrimImpl(hydra.core.lib.eithers.partition.name, tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tPair(tList(x), tList(y)))),
        impl1 { es =>
          val items = exList(es).map(exEither)
          val lefts = items.collect { case Left(a) => a }
          val rights = items.collect { case Right(b) => b }
          mkPairTerm(mkList(lefts), mkList(rights))
        }),
      hydra.core.lib.eithers.right.name -> mkPrimImpl(hydra.core.lib.eithers.right.name, tScheme(Seq("x", "y"),
        tFun(y, tEither(x, y))),
        impl1(v => mkEither(Right(v)))),
      hydra.core.lib.eithers.rights.name -> mkPrimImpl(hydra.core.lib.eithers.rights.name, tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(y))),
        impl1(es => mkList(exList(es).collect { case t if exEither(t).isRight => exEither(t).toOption.get }))),
    )

  // ===== Lists primitives =====

  private def listsPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    val aEq = Seq(("a", Seq("equality")))
    val aOrd = Seq(("a", Seq("ordering")))
    val bOrd = Seq(("b", Seq("ordering")))
    Map(
      // Higher-order: apply, bind, dropWhile, filter, find, foldl, foldr, map, partition, sortOn, span, zipWith
      hydra.core.lib.lists.apply.name -> mkPrimImpl(hydra.core.lib.lists.apply.name, tScheme(Seq("a", "b"),
        tFun(tList(tFun(a, b)), tFun(tList(a), tList(b)))),
        g => args => {
          val fs = exList(args(0)); val xs = exList(args(1))
          val results = for { f <- fs; x <- xs } yield applyAndReduce(g, f, x)
          results.foldLeft[E](ok(mkList(Seq.empty))) { (accE, rE) =>
            for { acc <- accE; r <- rE } yield mkList(exList(acc) :+ r)
          }
        }),
      hydra.core.lib.lists.bind.name -> mkPrimImpl(hydra.core.lib.lists.bind.name, tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tFun(a, tList(b)), tList(b)))),
        g => args => {
          val xs = exList(args(0)); val f = args(1)
          xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
            accE.flatMap { acc =>
              applyAndReduce(g, f, x).map { result =>
                mkList(exList(acc) ++ exList(result))
              }
            }
          }
        }),
      hydra.core.lib.lists.dropWhile.name -> mkPrimImpl(hydra.core.lib.lists.dropWhile.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a)))),
        g => args => {
          val p = args(0); val xs = exList(args(1))
          // Find index of first element where predicate is false
          xs.indices.foldLeft[E](ok(mkInt32(-1))) { (accE, i) =>
            accE.flatMap { acc =>
              if exInt32(acc) >= 0 then ok(acc) // already found
              else applyAndReduce(g, p, xs(i)).map(r => if !exBool(r) then mkInt32(i) else acc)
            }
          }.map { idx =>
            val i = exInt32(idx)
            if i < 0 then mkList(Seq.empty) else mkList(xs.drop(i))
          }
        }),
      hydra.core.lib.lists.filter.name -> mkPrimImpl(hydra.core.lib.lists.filter.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a)))),
        g => args => filterList(g, args(0), exList(args(1)))),
      hydra.core.lib.lists.find.name -> mkPrimImpl(hydra.core.lib.lists.find.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tOpt(a)))),
        g => args => {
          val p = args(0); val xs = exList(args(1))
          xs.foldLeft[E](ok(mkMaybe(None))) { (accE, x) =>
            accE.flatMap { acc =>
              exMaybe(acc) match
                case Some(_) => ok(acc) // already found
                case None => applyAndReduce(g, p, x).map(r => if exBool(r) then mkMaybe(Some(x)) else acc)
            }
          }
        }),
      hydra.core.lib.lists.foldl.name -> mkPrimImpl(hydra.core.lib.lists.foldl.name, tScheme(Seq("b", "a"),
        tFun(tFun(b, tFun(a, b)), tFun(b, tFun(tList(a), b)))),
        impl3 { (f, init, xs) =>
          exList(xs).foldLeft(init)((acc, x) => app2(f, acc, x))
        }),
      hydra.core.lib.lists.foldr.name -> mkPrimImpl(hydra.core.lib.lists.foldr.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, tFun(b, b)), tFun(b, tFun(tList(a), b)))),
        impl3 { (f, init, xs) =>
          exList(xs).foldRight(init)((x, acc) => app2(f, x, acc))
        }),
      hydra.core.lib.lists.map.name -> mkPrimImpl(hydra.core.lib.lists.map.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tList(a), tList(b)))),
        impl2 { (f, xs) =>
          mkList(exList(xs).map(x => app(f, x)))
        }),
      hydra.core.lib.lists.partition.name -> mkPrimImpl(hydra.core.lib.lists.partition.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a))))),
        g => args => partitionList(g, args(0), exList(args(1)))),
      hydra.core.lib.lists.sortBy.name -> mkPrimImpl(hydra.core.lib.lists.sortBy.name, tSchemeConstrained(Seq(("a", Seq.empty), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tList(a), tList(a)))),
        g => args => {
          val f = args(0); val xs = exList(args(1))
          // Compute sort keys for each element
          xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
            accE.flatMap { acc =>
              applyAndReduce(g, f, x).map { key =>
                mkList(exList(acc) :+ mkPairTerm(key, x))
              }
            }
          }.map { paired =>
            val pairs = exList(paired).map(exPair)
            mkList(pairs.sortWith((a, b) => ordering.compareTerms(a._1, b._1) < 0).map(_._2))
          }
        }),
      hydra.core.lib.lists.span.name -> mkPrimImpl(hydra.core.lib.lists.span.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a))))),
        g => args => {
          val p = args(0); val xs = exList(args(1))
          // Find index of first element where predicate is false
          xs.indices.foldLeft[E](ok(mkInt32(-1))) { (accE, i) =>
            accE.flatMap { acc =>
              if exInt32(acc) >= 0 then ok(acc)
              else applyAndReduce(g, p, xs(i)).map(r => if !exBool(r) then mkInt32(i) else acc)
            }
          }.map { idx =>
            val i = exInt32(idx)
            if i < 0 then mkPairTerm(mkList(xs), mkList(Seq.empty))
            else mkPairTerm(mkList(xs.take(i)), mkList(xs.drop(i)))
          }
        }),
      hydra.core.lib.lists.zipWith.name -> mkPrimImpl(hydra.core.lib.lists.zipWith.name, tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tFun(b, c)), tFun(tList(a), tFun(tList(b), tList(c))))),
        impl3 { (f, xs, ys) =>
          mkList(exList(xs).zip(exList(ys)).map((x, y) => app2(f, x, y)))
        }),
      // First-order
      hydra.core.lib.lists.concat.name -> mkPrimImpl(hydra.core.lib.lists.concat.name, tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(a))),
        impl1(xss => mkList(exList(xss).flatMap(exList)))),
      hydra.core.lib.lists.concat2.name -> mkPrimImpl(hydra.core.lib.lists.concat2.name, tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(a), tList(a)))),
        impl2((xs, ys) => mkList(exList(xs) ++ exList(ys)))),
      hydra.core.lib.lists.cons.name -> mkPrimImpl(hydra.core.lib.lists.cons.name, tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a)))),
        impl2((x, xs) => mkList(x +: exList(xs)))),
      hydra.core.lib.lists.drop.name -> mkPrimImpl(hydra.core.lib.lists.drop.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a)))),
        impl2((n, xs) => mkList(exList(xs).drop(exInt32(n))))),
      hydra.core.lib.lists.member.name -> mkPrimImpl(hydra.core.lib.lists.member.name, tSchemeConstrained(aEq,
        tFun(a, tFun(tList(a), tBool))),
        impl2((x, xs) => mkBool(exList(xs).contains(x)))),
      hydra.core.lib.lists.group.name -> mkPrimImpl(hydra.core.lib.lists.group.name, tSchemeConstrained(aEq,
        tFun(tList(a), tList(tList(a)))),
        impl1 { xs =>
          val items = exList(xs)
          def doGroup(remaining: Seq[Term]): Seq[Seq[Term]] =
            if remaining.isEmpty then Seq.empty
            else
              val (same, rest) = remaining.span(_ == remaining.head)
              same +: doGroup(rest)
          mkList(doGroup(items).map(mkList))
        }),
      hydra.core.lib.lists.join.name -> mkPrimImpl(hydra.core.lib.lists.join.name, tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(tList(a)), tList(a)))),
        impl2 { (sep, xss) =>
          val sepItems = exList(sep)
          val lists = exList(xss).map(exList)
          mkList(if lists.isEmpty then Seq.empty else lists.reduceLeft((a, b) => a ++ sepItems ++ b))
        }),
      hydra.core.lib.lists.intersperse.name -> mkPrimImpl(hydra.core.lib.lists.intersperse.name, tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a)))),
        impl2 { (sep, xs) =>
          val items = exList(xs)
          mkList(if items.isEmpty then Seq.empty else items.flatMap(x => Seq(sep, x)).tail)
        }),
      hydra.core.lib.lists.length.name -> mkPrimImpl(hydra.core.lib.lists.length.name, tScheme(Seq("a"),
        tFun(tList(a), tInt32)),
        impl1(xs => mkInt32(exList(xs).length))),
      hydra.core.lib.lists.at.name -> mkPrimImpl(hydra.core.lib.lists.at.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tOpt(a)))),
        impl2((i, xs) => mkMaybe(lists.at(exInt32(i))(exList(xs))))),
      hydra.core.lib.lists.head.name -> mkPrimImpl(hydra.core.lib.lists.head.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).headOption))),
      hydra.core.lib.lists.init.name -> mkPrimImpl(hydra.core.lib.lists.init.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(tList(a)))),
        impl1(xs => { val items = exList(xs); mkMaybe(lists.init(items).map(mkList)) })),
      hydra.core.lib.lists.last.name -> mkPrimImpl(hydra.core.lib.lists.last.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).lastOption))),
      hydra.core.lib.lists.tail.name -> mkPrimImpl(hydra.core.lib.lists.tail.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(tList(a)))),
        impl1(xs => { val items = exList(xs); mkMaybe(lists.tail(items).map(mkList)) })),
      hydra.core.lib.lists.distinct.name -> mkPrimImpl(hydra.core.lib.lists.distinct.name, tSchemeConstrained(aEq,
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).distinct))),
      hydra.core.lib.lists.isEmpty.name -> mkPrimImpl(hydra.core.lib.lists.isEmpty.name, tScheme(Seq("a"),
        tFun(tList(a), tBool)),
        impl1(xs => mkBool(exList(xs).isEmpty))),
      hydra.core.lib.lists.replicate.name -> mkPrimImpl(hydra.core.lib.lists.replicate.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(a, tList(a)))),
        impl2((n, x) => mkList(Seq.fill(exInt32(n))(x)))),
      hydra.core.lib.lists.reverse.name -> mkPrimImpl(hydra.core.lib.lists.reverse.name, tScheme(Seq("a"),
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).reverse))),
      hydra.core.lib.lists.singleton.name -> mkPrimImpl(hydra.core.lib.lists.singleton.name, tScheme(Seq("a"),
        tFun(a, tList(a))),
        impl1(x => mkList(Seq(x)))),
      hydra.core.lib.lists.sort.name -> mkPrimImpl(hydra.core.lib.lists.sort.name, tSchemeConstrained(aOrd,
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).sortWith((a, b) => ordering.lt(a)(b))))),
      hydra.core.lib.lists.take.name -> mkPrimImpl(hydra.core.lib.lists.take.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a)))),
        impl2((n, xs) => mkList(exList(xs).take(exInt32(n))))),
      hydra.core.lib.lists.transpose.name -> mkPrimImpl(hydra.core.lib.lists.transpose.name, tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(tList(a)))),
        impl1 { xss =>
          val innerLists = exList(xss).map(exList)
          mkList(hydra.core.overlay.scala.lib.lists.transpose(innerLists).map(mkList))
        }),
      hydra.core.lib.lists.uncons.name -> mkPrimImpl(hydra.core.lib.lists.uncons.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(tPair(a, tList(a))))),
        impl1(xs => {
          val items = exList(xs)
          mkMaybe(lists.uncons(items).map((h, t) => mkPairTerm(h, mkList(t))))
        })),
      hydra.core.lib.lists.zip.name -> mkPrimImpl(hydra.core.lib.lists.zip.name, tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tList(b), tList(tPair(a, b))))),
        impl2((xs, ys) => mkList(exList(xs).zip(exList(ys)).map((a, b) => mkPairTerm(a, b))))),
    )

  // ===== Logic primitives =====

  private def logicPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    Map(
      hydra.core.lib.logic.and.name -> mkPrimImpl(hydra.core.lib.logic.and.name, tMono(tFun(tBool, tFun(tBool, tBool))),
        impl2((a, b) => mkBool(exBool(a) && exBool(b)))),
      // ifElse is higher-order (lazy args act like functions)
      hydra.core.lib.logic.ifElse.name -> withLazy(mkPrimImpl(hydra.core.lib.logic.ifElse.name, tScheme(Seq("a"),
        tFun(tBool, tFun(a, tFun(a, a)))),
        impl3((cond, ifTrue, ifFalse) => if exBool(cond) then ifTrue else ifFalse)), Seq(1, 2)),
      hydra.core.lib.logic.not.name -> mkPrimImpl(hydra.core.lib.logic.not.name, tMono(tFun(tBool, tBool)),
        impl1(a => mkBool(!exBool(a)))),
      hydra.core.lib.logic.or.name -> mkPrimImpl(hydra.core.lib.logic.or.name, tMono(tFun(tBool, tFun(tBool, tBool))),
        impl2((a, b) => mkBool(exBool(a) || exBool(b)))),
    )

  // ===== Maps primitives =====

  private def mapsPrimitives(): Map[String, Primitive] =
    val k = tVar("k")
    val k1 = tVar("k1")
    val k2 = tVar("k2")
    val v = tVar("v")
    val v1 = tVar("v1")
    val v2 = tVar("v2")
    val mapKV = tMap(k, v)
    Map(
      // Higher-order: alter, bimap, filter, filterWithKey, map, mapKeys
      hydra.core.lib.maps.alter.name -> mkPrimImpl(hydra.core.lib.maps.alter.name, tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(tOpt(v), tOpt(v)), tFun(k, tFun(mapKV, mapKV)))),
        g => args => {
          val f = args(0); val key = args(1); val m = exMap(args(2))
          val current = mkMaybe(m.get(key))
          applyAndReduce(g, f, current).map { result =>
            exMaybe(result) match
              case None => mkMapTerm(m.removed(key))
              case Some(v) => mkMapTerm(m.updated(key, v))
          }
        }),
      hydra.core.lib.maps.bimap.name -> mkPrimImpl(hydra.core.lib.maps.bimap.name, tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v1", Seq.empty), ("v2", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tFun(v1, v2), tFun(tMap(k1, v1), tMap(k2, v2))))),
        impl3 { (fk, fv, m) =>
          mkMapTerm(exMap(m).map((k, v) => app(fk, k) -> app(fv, v)))
        }),
      hydra.core.lib.maps.filter.name -> mkPrimImpl(hydra.core.lib.maps.filter.name, tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v, tBool), tFun(mapKV, mapKV))),
        g => args => {
          val p = args(0); val m = exMap(args(1))
          m.toSeq.foldLeft[E](ok(mkMapTerm(Map.empty))) { case (accE, (ek, ev)) =>
            accE.flatMap { acc =>
              applyAndReduce(g, p, ev).map { result =>
                if exBool(result) then mkMapTerm(exMap(acc).updated(ek, ev)) else acc
              }
            }
          }
        }),
      hydra.core.lib.maps.filterWithKey.name -> mkPrimImpl(hydra.core.lib.maps.filterWithKey.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k, tFun(v, tBool)), tFun(mapKV, mapKV))),
        g => args => {
          val p = args(0); val m = exMap(args(1))
          m.toSeq.foldLeft[E](ok(mkMapTerm(Map.empty))) { case (accE, (ek, ev)) =>
            accE.flatMap { acc =>
              apply2AndReduce(g, p, ek, ev).map { result =>
                if exBool(result) then mkMapTerm(exMap(acc).updated(ek, ev)) else acc
              }
            }
          }
        }),
      hydra.core.lib.maps.map.name -> mkPrimImpl(hydra.core.lib.maps.map.name, tSchemeConstrained(Seq(("v1", Seq.empty), ("v2", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v1, v2), tFun(tMap(k, v1), tMap(k, v2)))),
        impl2 { (f, m) =>
          mkMapTerm(exMap(m).map((k, v) => k -> app(f, v)))
        }),
      hydra.core.lib.maps.mapKeys.name -> mkPrimImpl(hydra.core.lib.maps.mapKeys.name, tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tMap(k1, v), tMap(k2, v)))),
        impl2 { (f, m) =>
          mkMapTerm(exMap(m).map((k, v) => app(f, k) -> v))
        }),
      // First-order
      hydra.core.lib.maps.delete.name -> mkPrimImpl(hydra.core.lib.maps.delete.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, mapKV))),
        impl2((key, m) => mkMapTerm(exMap(m).removed(key)))),
      hydra.core.lib.maps.elems.name -> mkPrimImpl(hydra.core.lib.maps.elems.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(v))),
        impl1(m => mkList(exMap(m).values.toSeq))),
      hydra.core.lib.maps.empty.name -> mkPrimImpl(hydra.core.lib.maps.empty.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        mapKV),
        impl0(mkMapTerm(Map.empty))),
      hydra.core.lib.maps.findWithDefault.name -> withLazy(mkPrimImpl(hydra.core.lib.maps.findWithDefault.name, tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(v, tFun(k, tFun(mapKV, v)))),
        impl3((d, key, m) => exMap(m).getOrElse(key, d))), Seq(0)),
      hydra.core.lib.maps.fromList.name -> mkPrimImpl(hydra.core.lib.maps.fromList.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tList(tPair(k, v)), mapKV)),
        impl1(pairs => mkMapTerm(exList(pairs).map(p => { val (a, b) = exPair(p); a -> b }).toMap))),
      hydra.core.lib.maps.insert.name -> mkPrimImpl(hydra.core.lib.maps.insert.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, tFun(mapKV, mapKV)))),
        impl3((key, value, m) => mkMapTerm(exMap(m).updated(key, value)))),
      hydra.core.lib.maps.keys.name -> mkPrimImpl(hydra.core.lib.maps.keys.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(k))),
        impl1(m => mkList(exMap(m).keys.toSeq))),
      hydra.core.lib.maps.lookup.name -> mkPrimImpl(hydra.core.lib.maps.lookup.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tOpt(v)))),
        impl2((key, m) => mkMaybe(exMap(m).get(key)))),
      hydra.core.lib.maps.member.name -> mkPrimImpl(hydra.core.lib.maps.member.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tBool))),
        impl2((key, m) => mkBool(exMap(m).contains(key)))),
      hydra.core.lib.maps.isEmpty.name -> mkPrimImpl(hydra.core.lib.maps.isEmpty.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tBool)),
        impl1(m => mkBool(exMap(m).isEmpty))),
      hydra.core.lib.maps.singleton.name -> mkPrimImpl(hydra.core.lib.maps.singleton.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, mapKV))),
        impl2((key, value) => mkMapTerm(Map(key -> value)))),
      hydra.core.lib.maps.size.name -> mkPrimImpl(hydra.core.lib.maps.size.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tInt32)),
        impl1(m => mkInt32(exMap(m).size))),
      hydra.core.lib.maps.toList.name -> mkPrimImpl(hydra.core.lib.maps.toList.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(tPair(k, v)))),
        impl1(m => mkList(exMap(m).toSeq.map((k, v) => mkPairTerm(k, v))))),
      hydra.core.lib.maps.union.name -> mkPrimImpl(hydra.core.lib.maps.union.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tFun(mapKV, mapKV))),
        impl2((m1, m2) => mkMapTerm(exMap(m2) ++ exMap(m1)))),
    )

  // ===== Math primitives =====

  private def mathPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val xNumeric = Seq(("x", Seq("numeric")))
    val xIntegral = Seq(("x", Seq("integral")))
    val xFractional = Seq(("x", Seq("fractional")))
    Map(
      // Constraint-polymorphic ('numeric') primitives
      hydra.core.lib.math.abs.name -> mkPrimImpl(hydra.core.lib.math.abs.name, tSchemeConstrained(xNumeric, tFun(x, x)),
        impl1(numericUnary("abs", (a: BigInt) => a.abs, (a: Double) => scala.math.abs(a)))),
      hydra.core.lib.math.add.name -> mkPrimImpl(hydra.core.lib.math.add.name, tSchemeConstrained(xNumeric, tFun(x, tFun(x, x))),
        impl2(numericBinary("add", (_ + _), (_ + _)))),
      hydra.core.lib.math.mul.name -> mkPrimImpl(hydra.core.lib.math.mul.name, tSchemeConstrained(xNumeric, tFun(x, tFun(x, x))),
        impl2(numericBinary("mul", (_ * _), (_ * _)))),
      hydra.core.lib.math.negate.name -> mkPrimImpl(hydra.core.lib.math.negate.name, tSchemeConstrained(xNumeric, tFun(x, x)),
        impl1(numericUnary("negate", a => -a, a => -a))),
      hydra.core.lib.math.signum.name -> mkPrimImpl(hydra.core.lib.math.signum.name, tSchemeConstrained(xNumeric, tFun(x, x)),
        impl1(numericUnary("signum", (a: BigInt) => a.signum, (a: Double) => scala.math.signum(a)))),
      hydra.core.lib.math.sub.name -> mkPrimImpl(hydra.core.lib.math.sub.name, tSchemeConstrained(xNumeric, tFun(x, tFun(x, x))),
        impl2(numericBinary("sub", (_ - _), (_ - _)))),
      // Constraint-polymorphic ('integral') primitives
      hydra.core.lib.math.div.name -> mkPrimImpl(hydra.core.lib.math.div.name, tSchemeConstrained(xIntegral, tFun(x, tFun(x, tOpt(x)))),
        impl2((a, b) => mkMaybe(divTerm(a, b)))),
      hydra.core.lib.math.mod.name -> mkPrimImpl(hydra.core.lib.math.mod.name, tSchemeConstrained(xIntegral, tFun(x, tFun(x, tOpt(x)))),
        impl2((a, b) => mkMaybe(modTerm(a, b)))),
      hydra.core.lib.math.rem.name -> mkPrimImpl(hydra.core.lib.math.rem.name, tSchemeConstrained(xIntegral, tFun(x, tFun(x, tOpt(x)))),
        impl2((a, b) => mkMaybe(remTerm(a, b)))),
      hydra.core.lib.math.even.name -> mkPrimImpl(hydra.core.lib.math.even.name, tSchemeConstrained(xIntegral, tFun(x, tBool)),
        impl1(a => mkBool(evenTerm(a)))),
      hydra.core.lib.math.odd.name -> mkPrimImpl(hydra.core.lib.math.odd.name, tSchemeConstrained(xIntegral, tFun(x, tBool)),
        impl1(a => mkBool(oddTerm(a)))),
      // Constraint-polymorphic ('fractional') primitives
      hydra.core.lib.math.divide.name -> mkPrimImpl(hydra.core.lib.math.divide.name, tSchemeConstrained(xFractional, tFun(x, tFun(x, x))),
        impl2(divideTerm)),
      // Int32 primitives
      hydra.core.lib.math.range.name -> mkPrimImpl(hydra.core.lib.math.range.name, tMono(tFun(tInt32, tFun(tInt32, tList(tInt32)))),
        impl2((a, b) => mkList(math.range(exInt32(a))(exInt32(b)).map(mkInt32)))),
      // Float64 primitives
      hydra.core.lib.math.addFloat64.name -> mkPrimImpl(hydra.core.lib.math.addFloat64.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.addFloat64(exFloat64(a))(exFloat64(b))))),
      hydra.core.lib.math.acos.name -> mkPrimImpl(hydra.core.lib.math.acos.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.acos(exFloat64(a))))),
      hydra.core.lib.math.acosh.name -> mkPrimImpl(hydra.core.lib.math.acosh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.acosh(exFloat64(a))))),
      hydra.core.lib.math.asin.name -> mkPrimImpl(hydra.core.lib.math.asin.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.asin(exFloat64(a))))),
      hydra.core.lib.math.asinh.name -> mkPrimImpl(hydra.core.lib.math.asinh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.asinh(exFloat64(a))))),
      hydra.core.lib.math.atan.name -> mkPrimImpl(hydra.core.lib.math.atan.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.atan(exFloat64(a))))),
      hydra.core.lib.math.atan2.name -> mkPrimImpl(hydra.core.lib.math.atan2.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.atan2(exFloat64(a))(exFloat64(b))))),
      hydra.core.lib.math.atanh.name -> mkPrimImpl(hydra.core.lib.math.atanh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.atanh(exFloat64(a))))),
      hydra.core.lib.math.ceiling.name -> mkPrimImpl(hydra.core.lib.math.ceiling.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.ceiling(exFloat64(a))))),
      hydra.core.lib.math.cos.name -> mkPrimImpl(hydra.core.lib.math.cos.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.cos(exFloat64(a))))),
      hydra.core.lib.math.cosh.name -> mkPrimImpl(hydra.core.lib.math.cosh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.cosh(exFloat64(a))))),
      hydra.core.lib.math.e.name -> mkPrimImpl(hydra.core.lib.math.e.name, tMono(tFloat64),
        impl0(mkFloat64(math.e))),
      hydra.core.lib.math.exp.name -> mkPrimImpl(hydra.core.lib.math.exp.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.exp(exFloat64(a))))),
      hydra.core.lib.math.floor.name -> mkPrimImpl(hydra.core.lib.math.floor.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.floor(exFloat64(a))))),
      hydra.core.lib.math.log.name -> mkPrimImpl(hydra.core.lib.math.log.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.log(exFloat64(a))))),
      hydra.core.lib.math.mulFloat64.name -> mkPrimImpl(hydra.core.lib.math.mulFloat64.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.mulFloat64(exFloat64(a))(exFloat64(b))))),
      hydra.core.lib.math.negateFloat64.name -> mkPrimImpl(hydra.core.lib.math.negateFloat64.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.negateFloat64(exFloat64(a))))),
      hydra.core.lib.math.logBase.name -> mkPrimImpl(hydra.core.lib.math.logBase.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.logBase(exFloat64(a))(exFloat64(b))))),
      hydra.core.lib.math.pi.name -> mkPrimImpl(hydra.core.lib.math.pi.name, tMono(tFloat64),
        impl0(mkFloat64(math.pi))),
      hydra.core.lib.math.pow.name -> mkPrimImpl(hydra.core.lib.math.pow.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.pow(exFloat64(a))(exFloat64(b))))),
      hydra.core.lib.math.round.name -> mkPrimImpl(hydra.core.lib.math.round.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.round(exFloat64(a))))),
      hydra.core.lib.math.roundFloat32.name -> mkPrimImpl(hydra.core.lib.math.roundFloat32.name, tMono(tFun(tInt32, tFun(tFloat32, tFloat32))),
        impl2((p, x) => mkFloat32(math.roundFloat32(exInt32(p))(exFloat32(x))))),
      hydra.core.lib.math.roundFloat64.name -> mkPrimImpl(hydra.core.lib.math.roundFloat64.name, tMono(tFun(tInt32, tFun(tFloat64, tFloat64))),
        impl2((p, x) => mkFloat64(math.roundFloat64(exInt32(p))(exFloat64(x))))),
      hydra.core.lib.math.sin.name -> mkPrimImpl(hydra.core.lib.math.sin.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sin(exFloat64(a))))),
      hydra.core.lib.math.sinh.name -> mkPrimImpl(hydra.core.lib.math.sinh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sinh(exFloat64(a))))),
      hydra.core.lib.math.sqrt.name -> mkPrimImpl(hydra.core.lib.math.sqrt.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sqrt(exFloat64(a))))),
      hydra.core.lib.math.subFloat64.name -> mkPrimImpl(hydra.core.lib.math.subFloat64.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.subFloat64(exFloat64(a))(exFloat64(b))))),
      hydra.core.lib.math.tan.name -> mkPrimImpl(hydra.core.lib.math.tan.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.tan(exFloat64(a))))),
      hydra.core.lib.math.tanh.name -> mkPrimImpl(hydra.core.lib.math.tanh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.tanh(exFloat64(a))))),
      hydra.core.lib.math.truncate.name -> mkPrimImpl(hydra.core.lib.math.truncate.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.truncate(exFloat64(a))))),
    )

  // ===== Maybes primitives =====

  private def optionalsPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    Map(
      // Higher-order: apply, bind, cases, compose, map, mapOptional
      hydra.core.lib.optionals.apply.name -> mkPrimImpl(hydra.core.lib.optionals.apply.name, tScheme(Seq("a", "b"),
        tFun(tOpt(tFun(a, b)), tFun(tOpt(a), tOpt(b)))),
        impl2 { (mf, mx) =>
          (exMaybe(mf), exMaybe(mx)) match
            case (Some(f), Some(x)) => mkMaybe(Some(app(f, x)))
            case _ => mkMaybe(None)
        }),
      hydra.core.lib.optionals.bind.name -> mkPrimImpl(hydra.core.lib.optionals.bind.name, tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(tFun(a, tOpt(b)), tOpt(b)))),
        impl2 { (mx, f) =>
          exMaybe(mx) match
            case None => mkMaybe(None)
            case Some(x) => app(f, x)
        }),
      hydra.core.lib.optionals.`match`.name -> withLazy(mkPrimImpl(hydra.core.lib.optionals.`match`.name, tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(b, tFun(tFun(a, b), b)))),
        impl3 { (mx, d, f) =>
          exMaybe(mx) match
            case None => d
            case Some(x) => app(f, x)
        }), Seq(1)),
      hydra.core.lib.optionals.compose.name -> mkPrimImpl(hydra.core.lib.optionals.compose.name, tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tOpt(b)), tFun(tFun(b, tOpt(c)), tFun(a, tOpt(c))))),
        g => args => {
          val f = args(0); val g2 = args(1); val x = args(2)
          applyAndReduce(g, f, x).flatMap { mb =>
            exMaybe(mb) match
              case None => ok(mkMaybe(None))
              case Some(b) => applyAndReduce(g, g2, b)
          }
        }),
      hydra.core.lib.optionals.map.name -> mkPrimImpl(hydra.core.lib.optionals.map.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tOpt(a), tOpt(b)))),
        impl2 { (f, mx) =>
          exMaybe(mx) match
            case None => mkMaybe(None)
            case Some(x) => mkMaybe(Some(app(f, x)))
        }),
      hydra.core.lib.optionals.mapOptional.name -> mkPrimImpl(hydra.core.lib.optionals.mapOptional.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, tOpt(b)), tFun(tList(a), tList(b)))),
        g => args => {
          val f = args(0); val xs = exList(args(1))
          xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
            accE.flatMap { acc =>
              applyAndReduce(g, f, x).map { result =>
                exMaybe(result) match
                  case None => acc
                  case Some(v) => mkList(exList(acc) :+ v)
              }
            }
          }
        }),
      // First-order
      hydra.core.lib.optionals.givens.name -> mkPrimImpl(hydra.core.lib.optionals.givens.name, tScheme(Seq("a"),
        tFun(tList(tOpt(a)), tList(a))),
        impl1(xs => mkList(exList(xs).flatMap(exMaybe)))),
      hydra.core.lib.optionals.withDefault.name -> withLazy(mkPrimImpl(hydra.core.lib.optionals.withDefault.name, tScheme(Seq("a"),
        tFun(a, tFun(tOpt(a), a))),
        impl2((d, ma) => exMaybe(ma).getOrElse(d))), Seq(0)),
      hydra.core.lib.optionals.isGiven.name -> mkPrimImpl(hydra.core.lib.optionals.isGiven.name, tScheme(Seq("a"),
        tFun(tOpt(a), tBool)),
        impl1(ma => mkBool(exMaybe(ma).isDefined))),
      hydra.core.lib.optionals.isNone.name -> mkPrimImpl(hydra.core.lib.optionals.isNone.name, tScheme(Seq("a"),
        tFun(tOpt(a), tBool)),
        impl1(ma => mkBool(exMaybe(ma).isEmpty))),
      hydra.core.lib.optionals.`given`.name -> mkPrimImpl(hydra.core.lib.optionals.`given`.name, tScheme(Seq("a"),
        tFun(a, tOpt(a))),
        impl1(a => mkMaybe(Some(a)))),
      hydra.core.lib.optionals.toList.name -> mkPrimImpl(hydra.core.lib.optionals.toList.name, tScheme(Seq("a"),
        tFun(tOpt(a), tList(a))),
        impl1(ma => mkList(exMaybe(ma).toSeq))),
    )

  // ===== Sets primitives =====

  private def setsPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    val b = tVar("b")
    val aOrd = Seq(("a", Seq("ordering")))
    Map(
      // Higher-order: map
      hydra.core.lib.sets.map.name -> mkPrimImpl(hydra.core.lib.sets.map.name, tSchemeConstrained(Seq(("a", Seq("ordering")), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tSet(a), tSet(b)))),
        impl2 { (f, s) =>
          mkSet(exSet(s).map(x => app(f, x)))
        }),
      // First-order
      hydra.core.lib.sets.delete.name -> mkPrimImpl(hydra.core.lib.sets.delete.name, tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a)))),
        impl2((x, s) => mkSet(exSet(s) - x))),
      hydra.core.lib.sets.difference.name -> mkPrimImpl(hydra.core.lib.sets.difference.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1) -- exSet(s2)))),
      hydra.core.lib.sets.empty.name -> mkPrimImpl(hydra.core.lib.sets.empty.name, tSchemeConstrained(aOrd,
        tSet(a)),
        impl0(mkSet(Set.empty))),
      hydra.core.lib.sets.filter.name -> mkPrimImpl(hydra.core.lib.sets.filter.name, tSchemeConstrained(aOrd,
        tFun(tFun(a, tBool), tFun(tSet(a), tSet(a)))),
        g => args => filterSet(g, args(0), exSet(args(1)))),
      hydra.core.lib.sets.fromList.name -> mkPrimImpl(hydra.core.lib.sets.fromList.name, tSchemeConstrained(aOrd,
        tFun(tList(a), tSet(a))),
        impl1(xs => mkSet(exList(xs).toSet))),
      hydra.core.lib.sets.insert.name -> mkPrimImpl(hydra.core.lib.sets.insert.name, tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a)))),
        impl2((x, s) => mkSet(exSet(s) + x))),
      hydra.core.lib.sets.intersection.name -> mkPrimImpl(hydra.core.lib.sets.intersection.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1).intersect(exSet(s2))))),
      hydra.core.lib.sets.member.name -> mkPrimImpl(hydra.core.lib.sets.member.name, tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tBool))),
        impl2((x, s) => mkBool(exSet(s).contains(x)))),
      hydra.core.lib.sets.isEmpty.name -> mkPrimImpl(hydra.core.lib.sets.isEmpty.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tBool)),
        impl1(s => mkBool(exSet(s).isEmpty))),
      hydra.core.lib.sets.singleton.name -> mkPrimImpl(hydra.core.lib.sets.singleton.name, tSchemeConstrained(aOrd,
        tFun(a, tSet(a))),
        impl1(x => mkSet(Set(x)))),
      hydra.core.lib.sets.size.name -> mkPrimImpl(hydra.core.lib.sets.size.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tInt32)),
        impl1(s => mkInt32(exSet(s).size))),
      hydra.core.lib.sets.toList.name -> mkPrimImpl(hydra.core.lib.sets.toList.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tList(a))),
        impl1(s => mkList(exSet(s).toSeq))),
      hydra.core.lib.sets.union.name -> mkPrimImpl(hydra.core.lib.sets.union.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1).union(exSet(s2))))),
      hydra.core.lib.sets.unions.name -> mkPrimImpl(hydra.core.lib.sets.unions.name, tSchemeConstrained(aOrd,
        tFun(tList(tSet(a)), tSet(a))),
        impl1(ss => mkSet(exList(ss).flatMap(exSet).toSet))),
    )

  // ===== Regex primitives =====

  private def regexPrimitives(): Map[String, Primitive] =
    Map(
      hydra.core.lib.regex.find.name -> mkPrimImpl(hydra.core.lib.regex.find.name, tMono(tFun(tString, tFun(tString, tOpt(tString)))),
        impl2((pat, input) => mkMaybe(regex.find(exString(pat))(exString(input)).map(mkString)))),
      hydra.core.lib.regex.findAll.name -> mkPrimImpl(hydra.core.lib.regex.findAll.name, tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((pat, input) => mkList(regex.findAll(exString(pat))(exString(input)).map(mkString)))),
      hydra.core.lib.regex.matches.name -> mkPrimImpl(hydra.core.lib.regex.matches.name, tMono(tFun(tString, tFun(tString, tBool))),
        impl2((pat, input) => mkBool(regex.matches(exString(pat))(exString(input))))),
      hydra.core.lib.regex.replace.name -> mkPrimImpl(hydra.core.lib.regex.replace.name, tMono(tFun(tString, tFun(tString, tFun(tString, tString)))),
        impl3((pat, repl, input) => mkString(regex.replace(exString(pat))(exString(repl))(exString(input))))),
      hydra.core.lib.regex.replaceAll.name -> mkPrimImpl(hydra.core.lib.regex.replaceAll.name, tMono(tFun(tString, tFun(tString, tFun(tString, tString)))),
        impl3((pat, repl, input) => mkString(regex.replaceAll(exString(pat))(exString(repl))(exString(input))))),
      hydra.core.lib.regex.split.name -> mkPrimImpl(hydra.core.lib.regex.split.name, tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((pat, input) => mkList(regex.split(exString(pat))(exString(input)).map(mkString)))),
    )

  // ===== Strings primitives =====

  private def stringsPrimitives(): Map[String, Primitive] =
    Map(
      hydra.core.lib.strings.concat.name -> mkPrimImpl(hydra.core.lib.strings.concat.name, tMono(tFun(tList(tString), tString)),
        impl1(ss => mkString(strings.concat(exList(ss).map(exString))))),
      hydra.core.lib.strings.concat2.name -> mkPrimImpl(hydra.core.lib.strings.concat2.name, tMono(tFun(tString, tFun(tString, tString))),
        impl2((a, b) => mkString(strings.concat2(exString(a))(exString(b))))),
      hydra.core.lib.strings.fromList.name -> mkPrimImpl(hydra.core.lib.strings.fromList.name, tMono(tFun(tList(tInt32), tString)),
        impl1(cs => mkString(strings.fromList(exList(cs).map(exInt32))))),
      hydra.core.lib.strings.join.name -> mkPrimImpl(hydra.core.lib.strings.join.name, tMono(tFun(tString, tFun(tList(tString), tString))),
        impl2((sep, ss) => mkString(strings.join(exString(sep))(exList(ss).map(exString))))),
      hydra.core.lib.strings.length.name -> mkPrimImpl(hydra.core.lib.strings.length.name, tMono(tFun(tString, tInt32)),
        impl1(s => mkInt32(strings.length(exString(s))))),
      hydra.core.lib.strings.charAt.name -> mkPrimImpl(hydra.core.lib.strings.charAt.name, tMono(tFun(tInt32, tFun(tString, tOpt(tInt32)))),
        impl2((i, s) => mkMaybe(strings.charAt(exInt32(i))(exString(s)).map(mkInt32)))),
      hydra.core.lib.strings.isEmpty.name -> mkPrimImpl(hydra.core.lib.strings.isEmpty.name, tMono(tFun(tString, tBool)),
        impl1(s => mkBool(strings.isEmpty(exString(s))))),
      hydra.core.lib.strings.splitOn.name -> mkPrimImpl(hydra.core.lib.strings.splitOn.name, tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((sep, s) => mkList(strings.splitOn(exString(sep))(exString(s)).map(mkString)))),
      hydra.core.lib.strings.toList.name -> mkPrimImpl(hydra.core.lib.strings.toList.name, tMono(tFun(tString, tList(tInt32))),
        impl1(s => mkList(strings.toList(exString(s)).map(mkInt32)))),
      hydra.core.lib.strings.toLower.name -> mkPrimImpl(hydra.core.lib.strings.toLower.name, tMono(tFun(tString, tString)),
        impl1(s => mkString(strings.toLower(exString(s))))),
      hydra.core.lib.strings.toUpper.name -> mkPrimImpl(hydra.core.lib.strings.toUpper.name, tMono(tFun(tString, tString)),
        impl1(s => mkString(strings.toUpper(exString(s))))),
    )

  // ===== Literals primitives =====

  private def literalsPrimitives(): Map[String, Primitive] =
    Map(
      // Conversion primitives
      hydra.core.lib.literals.bigintToDecimal.name -> mkPrimImpl(hydra.core.lib.literals.bigintToDecimal.name, tMono(tFun(tBigint, tDecimal)),
        impl1(a => mkDecimal(literals.bigintToDecimal(exBigint(a))))),
      hydra.core.lib.literals.bigintToInt8.name -> mkPrimImpl(hydra.core.lib.literals.bigintToInt8.name, tMono(tFun(tBigint, tInt8)),
        impl1(a => mkInt8(literals.bigintToInt8(exBigint(a))))),
      hydra.core.lib.literals.bigintToInt16.name -> mkPrimImpl(hydra.core.lib.literals.bigintToInt16.name, tMono(tFun(tBigint, tInt16)),
        impl1(a => mkInt16(literals.bigintToInt16(exBigint(a))))),
      hydra.core.lib.literals.bigintToInt32.name -> mkPrimImpl(hydra.core.lib.literals.bigintToInt32.name, tMono(tFun(tBigint, tInt32)),
        impl1(a => mkInt32(literals.bigintToInt32(exBigint(a))))),
      hydra.core.lib.literals.bigintToInt64.name -> mkPrimImpl(hydra.core.lib.literals.bigintToInt64.name, tMono(tFun(tBigint, tInt64)),
        impl1(a => mkInt64(literals.bigintToInt64(exBigint(a))))),
      hydra.core.lib.literals.bigintToUint8.name -> mkPrimImpl(hydra.core.lib.literals.bigintToUint8.name, tMono(tFun(tBigint, tUint8)),
        impl1(a => mkUint8(literals.bigintToUint8(exBigint(a))))),
      hydra.core.lib.literals.bigintToUint16.name -> mkPrimImpl(hydra.core.lib.literals.bigintToUint16.name, tMono(tFun(tBigint, tUint16)),
        impl1(a => mkUint16(literals.bigintToUint16(exBigint(a))))),
      hydra.core.lib.literals.bigintToUint32.name -> mkPrimImpl(hydra.core.lib.literals.bigintToUint32.name, tMono(tFun(tBigint, tUint32)),
        impl1(a => mkUint32(literals.bigintToUint32(exBigint(a))))),
      hydra.core.lib.literals.bigintToUint64.name -> mkPrimImpl(hydra.core.lib.literals.bigintToUint64.name, tMono(tFun(tBigint, tUint64)),
        impl1(a => mkUint64(literals.bigintToUint64(exBigint(a))))),
      hydra.core.lib.literals.binaryToBytes.name -> mkPrimImpl(hydra.core.lib.literals.binaryToBytes.name, tMono(tFun(tBinary, tList(tInt32))),
        impl1(a => mkList(literals.binaryToBytes(exBinary(a)).map(mkInt32)))),
      hydra.core.lib.literals.binaryToBase64.name -> mkPrimImpl(hydra.core.lib.literals.binaryToBase64.name, tMono(tFun(tBinary, tString)),
        impl1(a => mkString(literals.binaryToBase64(exBinary(a))))),
      hydra.core.lib.literals.decimalToBigint.name -> mkPrimImpl(hydra.core.lib.literals.decimalToBigint.name, tMono(tFun(tDecimal, tBigint)),
        impl1(a => mkBigint(literals.decimalToBigint(exDecimal(a))))),
      hydra.core.lib.literals.decimalToFloat32.name -> mkPrimImpl(hydra.core.lib.literals.decimalToFloat32.name, tMono(tFun(tDecimal, tFloat32)),
        impl1(a => mkFloat32(literals.decimalToFloat32(exDecimal(a))))),
      hydra.core.lib.literals.decimalToFloat64.name -> mkPrimImpl(hydra.core.lib.literals.decimalToFloat64.name, tMono(tFun(tDecimal, tFloat64)),
        impl1(a => mkFloat64(literals.decimalToFloat64(exDecimal(a))))),
      hydra.core.lib.literals.float32ToDecimal.name -> mkPrimImpl(hydra.core.lib.literals.float32ToDecimal.name, tMono(tFun(tFloat32, tDecimal)),
        impl1(a => mkDecimal(literals.float32ToDecimal(exFloat32(a))))),
      hydra.core.lib.literals.float32ToFloat64.name -> mkPrimImpl(hydra.core.lib.literals.float32ToFloat64.name, tMono(tFun(tFloat32, tFloat64)),
        impl1(a => mkFloat64(literals.float32ToFloat64(exFloat32(a))))),
      hydra.core.lib.literals.float64ToDecimal.name -> mkPrimImpl(hydra.core.lib.literals.float64ToDecimal.name, tMono(tFun(tFloat64, tDecimal)),
        impl1(a => mkDecimal(literals.float64ToDecimal(exFloat64(a))))),
      hydra.core.lib.literals.float64ToFloat32.name -> mkPrimImpl(hydra.core.lib.literals.float64ToFloat32.name, tMono(tFun(tFloat64, tFloat32)),
        impl1(a => mkFloat32(literals.float64ToFloat32(exFloat64(a))))),
      hydra.core.lib.literals.int8ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.int8ToBigint.name, tMono(tFun(tInt8, tBigint)),
        impl1(a => mkBigint(literals.int8ToBigint(exInt8(a))))),
      hydra.core.lib.literals.int16ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.int16ToBigint.name, tMono(tFun(tInt16, tBigint)),
        impl1(a => mkBigint(literals.int16ToBigint(exInt16(a))))),
      hydra.core.lib.literals.int32ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.int32ToBigint.name, tMono(tFun(tInt32, tBigint)),
        impl1(a => mkBigint(literals.int32ToBigint(exInt32(a))))),
      hydra.core.lib.literals.int64ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.int64ToBigint.name, tMono(tFun(tInt64, tBigint)),
        impl1(a => mkBigint(literals.int64ToBigint(exInt64(a))))),
      // Read primitives
      hydra.core.lib.literals.parseBigint.name -> mkPrimImpl(hydra.core.lib.literals.parseBigint.name, tMono(tFun(tString, tOpt(tBigint))),
        impl1(s => mkMaybe(literals.parseBigint(exString(s)).map(mkBigint)))),
      hydra.core.lib.literals.parseBoolean.name -> mkPrimImpl(hydra.core.lib.literals.parseBoolean.name, tMono(tFun(tString, tOpt(tBool))),
        impl1(s => mkMaybe(literals.parseBoolean(exString(s)).map(mkBool)))),
      hydra.core.lib.literals.parseDecimal.name -> mkPrimImpl(hydra.core.lib.literals.parseDecimal.name, tMono(tFun(tString, tOpt(tDecimal))),
        impl1(s => mkMaybe(literals.parseDecimal(exString(s)).map(mkDecimal)))),
      hydra.core.lib.literals.parseFloat32.name -> mkPrimImpl(hydra.core.lib.literals.parseFloat32.name, tMono(tFun(tString, tOpt(tFloat32))),
        impl1(s => mkMaybe(literals.parseFloat32(exString(s)).map(mkFloat32)))),
      hydra.core.lib.literals.parseFloat64.name -> mkPrimImpl(hydra.core.lib.literals.parseFloat64.name, tMono(tFun(tString, tOpt(tFloat64))),
        impl1(s => mkMaybe(literals.parseFloat64(exString(s)).map(mkFloat64)))),
      hydra.core.lib.literals.parseInt8.name -> mkPrimImpl(hydra.core.lib.literals.parseInt8.name, tMono(tFun(tString, tOpt(tInt8))),
        impl1(s => mkMaybe(literals.parseInt8(exString(s)).map(mkInt8)))),
      hydra.core.lib.literals.parseInt16.name -> mkPrimImpl(hydra.core.lib.literals.parseInt16.name, tMono(tFun(tString, tOpt(tInt16))),
        impl1(s => mkMaybe(literals.parseInt16(exString(s)).map(mkInt16)))),
      hydra.core.lib.literals.parseInt32.name -> mkPrimImpl(hydra.core.lib.literals.parseInt32.name, tMono(tFun(tString, tOpt(tInt32))),
        impl1(s => mkMaybe(literals.parseInt32(exString(s)).map(mkInt32)))),
      hydra.core.lib.literals.parseInt64.name -> mkPrimImpl(hydra.core.lib.literals.parseInt64.name, tMono(tFun(tString, tOpt(tInt64))),
        impl1(s => mkMaybe(literals.parseInt64(exString(s)).map(mkInt64)))),
      hydra.core.lib.literals.parseString.name -> mkPrimImpl(hydra.core.lib.literals.parseString.name, tMono(tFun(tString, tOpt(tString))),
        impl1(s => mkMaybe(literals.parseString(exString(s)).map(mkString)))),
      hydra.core.lib.literals.parseUint8.name -> mkPrimImpl(hydra.core.lib.literals.parseUint8.name, tMono(tFun(tString, tOpt(tUint8))),
        impl1(s => mkMaybe(literals.parseUint8(exString(s)).map(mkUint8)))),
      hydra.core.lib.literals.parseUint16.name -> mkPrimImpl(hydra.core.lib.literals.parseUint16.name, tMono(tFun(tString, tOpt(tUint16))),
        impl1(s => mkMaybe(literals.parseUint16(exString(s)).map(mkUint16)))),
      hydra.core.lib.literals.parseUint32.name -> mkPrimImpl(hydra.core.lib.literals.parseUint32.name, tMono(tFun(tString, tOpt(tUint32))),
        impl1(s => mkMaybe(literals.parseUint32(exString(s)).map(mkUint32)))),
      hydra.core.lib.literals.parseUint64.name -> mkPrimImpl(hydra.core.lib.literals.parseUint64.name, tMono(tFun(tString, tOpt(tUint64))),
        impl1(s => mkMaybe(literals.parseUint64(exString(s)).map(mkUint64)))),
      // Show primitives
      hydra.core.lib.literals.printBigint.name -> mkPrimImpl(hydra.core.lib.literals.printBigint.name, tMono(tFun(tBigint, tString)),
        impl1(a => mkString(literals.printBigint(exBigint(a))))),
      hydra.core.lib.literals.printBoolean.name -> mkPrimImpl(hydra.core.lib.literals.printBoolean.name, tMono(tFun(tBool, tString)),
        impl1(a => mkString(literals.printBoolean(exBool(a))))),
      hydra.core.lib.literals.printDecimal.name -> mkPrimImpl(hydra.core.lib.literals.printDecimal.name, tMono(tFun(tDecimal, tString)),
        impl1(a => mkString(literals.printDecimal(exDecimal(a))))),
      hydra.core.lib.literals.printFloat32.name -> mkPrimImpl(hydra.core.lib.literals.printFloat32.name, tMono(tFun(tFloat32, tString)),
        impl1(a => mkString(literals.printFloat32(exFloat32(a))))),
      hydra.core.lib.literals.printFloat64.name -> mkPrimImpl(hydra.core.lib.literals.printFloat64.name, tMono(tFun(tFloat64, tString)),
        impl1(a => mkString(literals.printFloat64(exFloat64(a))))),
      hydra.core.lib.literals.printInt8.name -> mkPrimImpl(hydra.core.lib.literals.printInt8.name, tMono(tFun(tInt8, tString)),
        impl1(a => mkString(literals.printInt8(exInt8(a))))),
      hydra.core.lib.literals.printInt16.name -> mkPrimImpl(hydra.core.lib.literals.printInt16.name, tMono(tFun(tInt16, tString)),
        impl1(a => mkString(literals.printInt16(exInt16(a))))),
      hydra.core.lib.literals.printInt32.name -> mkPrimImpl(hydra.core.lib.literals.printInt32.name, tMono(tFun(tInt32, tString)),
        impl1(a => mkString(literals.printInt32(exInt32(a))))),
      hydra.core.lib.literals.printInt64.name -> mkPrimImpl(hydra.core.lib.literals.printInt64.name, tMono(tFun(tInt64, tString)),
        impl1(a => mkString(literals.printInt64(exInt64(a))))),
      hydra.core.lib.literals.printUint8.name -> mkPrimImpl(hydra.core.lib.literals.printUint8.name, tMono(tFun(tUint8, tString)),
        impl1(a => mkString(literals.printUint8(exUint8(a))))),
      hydra.core.lib.literals.printUint16.name -> mkPrimImpl(hydra.core.lib.literals.printUint16.name, tMono(tFun(tUint16, tString)),
        impl1(a => mkString(literals.printUint16(exUint16(a))))),
      hydra.core.lib.literals.printUint32.name -> mkPrimImpl(hydra.core.lib.literals.printUint32.name, tMono(tFun(tUint32, tString)),
        impl1(a => mkString(literals.printUint32(exUint32(a))))),
      hydra.core.lib.literals.printUint64.name -> mkPrimImpl(hydra.core.lib.literals.printUint64.name, tMono(tFun(tUint64, tString)),
        impl1(a => mkString(literals.printUint64(exUint64(a))))),
      hydra.core.lib.literals.printString.name -> mkPrimImpl(hydra.core.lib.literals.printString.name, tMono(tFun(tString, tString)),
        impl1(a => mkString(literals.printString(exString(a))))),
      hydra.core.lib.literals.base64ToBinary.name -> mkPrimImpl(hydra.core.lib.literals.base64ToBinary.name, tMono(tFun(tString, tBinary)),
        impl1(a => mkBinary(literals.base64ToBinary(exString(a))))),
      hydra.core.lib.literals.uint8ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.uint8ToBigint.name, tMono(tFun(tUint8, tBigint)),
        impl1(a => mkBigint(literals.uint8ToBigint(exUint8(a))))),
      hydra.core.lib.literals.uint16ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.uint16ToBigint.name, tMono(tFun(tUint16, tBigint)),
        impl1(a => mkBigint(literals.uint16ToBigint(exUint16(a))))),
      hydra.core.lib.literals.uint32ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.uint32ToBigint.name, tMono(tFun(tUint32, tBigint)),
        impl1(a => mkBigint(literals.uint32ToBigint(exUint32(a))))),
      hydra.core.lib.literals.uint64ToBigint.name -> mkPrimImpl(hydra.core.lib.literals.uint64ToBigint.name, tMono(tFun(tUint64, tBigint)),
        impl1(a => mkBigint(literals.uint64ToBigint(exUint64(a))))),
    )

  // ===== Pairs primitives =====

  private def pairsPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    val d = tVar("d")
    Map(
      // Higher-order: bimap
      hydra.core.lib.pairs.bimap.name -> mkPrimImpl(hydra.core.lib.pairs.bimap.name, tScheme(Seq("a", "b", "c", "d"),
        tFun(tFun(a, c), tFun(tFun(b, d), tFun(tPair(a, b), tPair(c, d))))),
        impl3 { (f, g, p) =>
          val (a, b) = exPair(p)
          mkPairTerm(app(f, a), app(g, b))
        }),
      // First-order
      hydra.core.lib.pairs.first.name -> mkPrimImpl(hydra.core.lib.pairs.first.name, tScheme(Seq("a", "b"),
        tFun(tPair(a, b), a)),
        impl1(p => exPair(p)._1)),
      hydra.core.lib.pairs.pair.name -> mkPrimImpl(hydra.core.lib.pairs.pair.name, tScheme(Seq("a", "b"),
        tFun(a, tFun(b, tPair(a, b)))),
        impl2((x, y) => mkPairTerm(x, y))),
      hydra.core.lib.pairs.second.name -> mkPrimImpl(hydra.core.lib.pairs.second.name, tScheme(Seq("a", "b"),
        tFun(tPair(a, b), b)),
        impl1(p => exPair(p)._2)),
    )

  // ===== Effects primitives (#494) =====
  //
  // effect<t> is transparent in Scala. These are registered so the inference graph can
  // resolve the hydra.core.lib.effects.* names; their type schemes match the kernel signatures
  // exactly (note pure is x -> effect<x>, including the function arrow). The interpreter
  // implementation is a deferred error (stub): effect primitives are evaluated through the
  // native (host) path, not Hydra's pure reducer. Forcing implementation() must never throw
  // at registration, so mkPrim's stubImpl (a deferred Left) is used throughout.

  private def effectsPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val y = tVar("y")
    val z = tVar("z")
    Map(
      // apply: effect<x -> y> -> effect<x> -> effect<y>
      hydra.core.lib.effects.apply.name -> mkPrimEffect(hydra.core.lib.effects.apply.name, tScheme(Seq("x", "y"),
        tFun(tEffect(tFun(x, y)), tFun(tEffect(x), tEffect(y))))),
      // bind: effect<x> -> (x -> effect<y>) -> effect<y>
      hydra.core.lib.effects.bind.name -> mkPrimEffect(hydra.core.lib.effects.bind.name, tScheme(Seq("x", "y"),
        tFun(tEffect(x), tFun(tFun(x, tEffect(y)), tEffect(y))))),
      // compose: (x -> effect<y>) -> (y -> effect<z>) -> x -> effect<z>
      hydra.core.lib.effects.compose.name -> mkPrimEffect(hydra.core.lib.effects.compose.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEffect(y)), tFun(tFun(y, tEffect(z)), tFun(x, tEffect(z)))))),
      // foldList: (x -> y -> effect<x>) -> x -> list<y> -> effect<x>
      hydra.core.lib.effects.foldList.name -> mkPrimEffect(hydra.core.lib.effects.foldList.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, tFun(y, tEffect(x))), tFun(x, tFun(tList(y), tEffect(x)))))),
      // map: (x -> y) -> effect<x> -> effect<y>
      hydra.core.lib.effects.map.name -> mkPrimEffect(hydra.core.lib.effects.map.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, y), tFun(tEffect(x), tEffect(y))))),
      // mapList: (x -> effect<y>) -> list<x> -> effect<list<y>>
      hydra.core.lib.effects.mapList.name -> mkPrimEffect(hydra.core.lib.effects.mapList.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, tEffect(y)), tFun(tList(x), tEffect(tList(y)))))),
      // mapOptional: (x -> effect<y>) -> optional<x> -> effect<optional<y>>
      hydra.core.lib.effects.mapOptional.name -> mkPrimEffect(hydra.core.lib.effects.mapOptional.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, tEffect(y)), tFun(tOpt(x), tEffect(tOpt(y)))))),
      // pure: x -> effect<x>
      hydra.core.lib.effects.pure.name -> mkPrimEffect(hydra.core.lib.effects.pure.name, tScheme(Seq("x"),
        tFun(x, tEffect(x)))),
    )

  // ===== Files primitives (#494) =====
  //
  // FilePath and FileError are nominal kernel types (referenced by name). unit maps to
  // Scala Unit, binary to Array[Byte], either to scala.util.Either. As with effects, the
  // interpreter implementation is a deferred stub; real I/O happens in hydra.core.overlay.scala.lib.files.

  private def filesPrimitives(): Map[String, Primitive] =
    Map(
      // appendFile: FilePath -> binary -> effect<either<FileError, unit>>
      hydra.core.lib.files.appendFile.name -> mkPrimEffect(hydra.core.lib.files.appendFile.name,
        tMono(tFun(tFilePath, tFun(tBinary, tEffect(tEither(tFileError, tUnit)))))),
      // copy: boolean -> FilePath -> FilePath -> effect<either<FileError, unit>>
      hydra.core.lib.files.copy.name -> mkPrimEffect(hydra.core.lib.files.copy.name,
        tMono(tFun(tBool, tFun(tFilePath, tFun(tFilePath, tEffect(tEither(tFileError, tUnit))))))),
      // createDirectory: boolean -> FilePath -> effect<either<FileError, unit>>
      hydra.core.lib.files.createDirectory.name -> mkPrimEffect(hydra.core.lib.files.createDirectory.name,
        tMono(tFun(tBool, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // createSymlink: FilePath -> FilePath -> effect<either<FileError, unit>>
      hydra.core.lib.files.createSymlink.name -> mkPrimEffect(hydra.core.lib.files.createSymlink.name,
        tMono(tFun(tFilePath, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // exists: FilePath -> effect<either<FileError, boolean>>
      hydra.core.lib.files.exists.name -> mkPrimEffect(hydra.core.lib.files.exists.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tBool))))),
      // listDirectory: FilePath -> effect<either<FileError, list<FilePath>>>
      hydra.core.lib.files.listDirectory.name -> mkPrimEffect(hydra.core.lib.files.listDirectory.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tList(tFilePath)))))),
      // readFile: FilePath -> effect<either<FileError, binary>>
      hydra.core.lib.files.readFile.name -> mkPrimEffect(hydra.core.lib.files.readFile.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tBinary))))),
      // readSymlink: FilePath -> effect<either<FileError, FilePath>>
      hydra.core.lib.files.readSymlink.name -> mkPrimEffect(hydra.core.lib.files.readSymlink.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tFilePath))))),
      // removeDirectory: boolean -> FilePath -> effect<either<FileError, unit>>
      hydra.core.lib.files.removeDirectory.name -> mkPrimEffect(hydra.core.lib.files.removeDirectory.name,
        tMono(tFun(tBool, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // removeFile: FilePath -> effect<either<FileError, unit>>
      hydra.core.lib.files.removeFile.name -> mkPrimEffect(hydra.core.lib.files.removeFile.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tUnit))))),
      // rename: FilePath -> FilePath -> effect<either<FileError, unit>>
      hydra.core.lib.files.rename.name -> mkPrimEffect(hydra.core.lib.files.rename.name,
        tMono(tFun(tFilePath, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // status: boolean -> FilePath -> effect<either<FileError, FileStatus>>
      hydra.core.lib.files.status.name -> mkPrimEffect(hydra.core.lib.files.status.name,
        tMono(tFun(tBool, tFun(tFilePath, tEffect(tEither(tFileError, tFileStatus)))))),
      // writeFile: FilePath -> binary -> effect<either<FileError, unit>>
      hydra.core.lib.files.writeFile.name -> mkPrimEffect(hydra.core.lib.files.writeFile.name,
        tMono(tFun(tFilePath, tFun(tBinary, tEffect(tEither(tFileError, tUnit)))))),
    )

  // ===== Hashing primitives (#524) =====

  private def hashingPrimitives(): Map[String, Primitive] =
    Map(
      // sha256: binary -> binary
      hydra.core.lib.hashing.sha256.name -> mkPrim(hydra.core.lib.hashing.sha256.name,
        tMono(tFun(tBinary, tBinary))),
      // sha256Hex: binary -> string
      hydra.core.lib.hashing.sha256Hex.name -> mkPrim(hydra.core.lib.hashing.sha256Hex.name,
        tMono(tFun(tBinary, tString))),
    )

  // ===== System primitives (#498) =====

  private def systemPrimitives(): Map[String, Primitive] =
    Map(
      // execute: Command -> effect<either<SystemError, ProcessResult>>
      hydra.core.lib.system.execute.name -> mkPrimEffect(hydra.core.lib.system.execute.name,
        tMono(tFun(tCommand, tEffect(tEither(tSystemError, tProcessResult))))),
      // exit: StatusCode -> effect<unit>
      hydra.core.lib.system.exit.name -> mkPrimEffect(hydra.core.lib.system.exit.name,
        tMono(tFun(tStatusCode, tEffect(tUnit)))),
      // getEnvironment: effect<map<EnvironmentVariable, string>>
      hydra.core.lib.system.getEnvironment.name -> mkPrimEffect(hydra.core.lib.system.getEnvironment.name,
        tMono(tEffect(tMap(tEnvironmentVariable, tString)))),
      // getEnvironmentVariable: EnvironmentVariable -> effect<optional<string>>
      hydra.core.lib.system.getEnvironmentVariable.name -> mkPrimEffect(hydra.core.lib.system.getEnvironmentVariable.name,
        tMono(tFun(tEnvironmentVariable, tEffect(tOpt(tString))))),
      // getTime: effect<Timespec>
      hydra.core.lib.system.getTime.name -> mkPrimEffect(hydra.core.lib.system.getTime.name,
        tMono(tEffect(tTimespec))),
      // getWorkingDirectory: effect<either<SystemError, FilePath>>
      hydra.core.lib.system.getWorkingDirectory.name -> mkPrimEffect(hydra.core.lib.system.getWorkingDirectory.name,
        tMono(tEffect(tEither(tSystemError, tFilePath)))),
      // readStdin: effect<either<SystemError, binary>>
      hydra.core.lib.system.readStdin.name -> mkPrimEffect(hydra.core.lib.system.readStdin.name,
        tMono(tEffect(tEither(tSystemError, tBinary)))),
      // writeStderr: binary -> effect<either<SystemError, unit>>
      hydra.core.lib.system.writeStderr.name -> mkPrimEffect(hydra.core.lib.system.writeStderr.name,
        tMono(tFun(tBinary, tEffect(tEither(tSystemError, tUnit))))),
      // writeStdout: binary -> effect<either<SystemError, unit>>
      hydra.core.lib.system.writeStdout.name -> mkPrimEffect(hydra.core.lib.system.writeStdout.name,
        tMono(tFun(tBinary, tEffect(tEither(tSystemError, tUnit))))),
    )


  // ===== Text primitives (#494) =====

  private def textPrimitives(): Map[String, Primitive] =
    Map(
      // decodeUtf8: binary -> either<string, string>
      hydra.core.lib.text.decodeUtf8.name -> mkPrimImpl(hydra.core.lib.text.decodeUtf8.name,
        tMono(tFun(tBinary, tEither(tString, tString))),
        impl1(a => text.decodeUtf8(exBinary(a)) match
          case Left(msg) => mkEither(Left(mkString(msg)))
          case Right(s) => mkEither(Right(mkString(s))))),
      // encodeUtf8: string -> binary
      hydra.core.lib.text.encodeUtf8.name -> mkPrimImpl(hydra.core.lib.text.encodeUtf8.name,
        tMono(tFun(tString, tBinary)),
        impl1(a => mkBinary(text.encodeUtf8(exString(a))))),
    )

  /** All standard primitives. */
  def standardPrimitives(): Map[String, Primitive] =
    val native =
      charsPrimitives() ++
      effectsPrimitives() ++
      equalityPrimitives() ++
      eithersPrimitives() ++
      filesPrimitives() ++
      functionsPrimitives() ++
      hashingPrimitives() ++
      listsPrimitives() ++
      literalsPrimitives() ++
      logicPrimitives() ++
      mapsPrimitives() ++
      mathPrimitives() ++
      optionalsPrimitives() ++
      orderingPrimitives() ++
      pairsPrimitives() ++
      regexPrimitives() ++
      setsPrimitives() ++
      stringsPrimitives() ++
      systemPrimitives() ++
      textPrimitives()
    native ++ defaultFallbackPrimitives(native.keySet)

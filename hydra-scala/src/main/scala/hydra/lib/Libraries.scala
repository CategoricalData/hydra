package hydra.lib

import hydra.core.*
import hydra.graph.Primitive

/** Registry of all primitive functions available in Hydra-Scala.
  * First-order primitives have real (native) implementations. Most higher-order
  * primitives also have term-level implementations that build application terms
  * for the reducer to evaluate. A few complex higher-order primitives (those
  * requiring intermediate evaluation, e.g. filter, find, bind) use stubs and
  * rely on eval elements.
  */
object Libraries:

  // ===== Infrastructure =====

  private type Impl = hydra.context.Context => hydra.graph.Graph => Seq[Term] => Either[hydra.errors.Error, Term]

  private val stubImpl: Impl =
    _ => _ => _ => Left(hydra.errors.Error.other("stub primitive"))

  private def ok(t: Term): Either[hydra.errors.Error, Term] = Right(t)

  private type E = Either[hydra.errors.Error, Term]

  // Reduce a term using the full reducer
  private def reduce(cx: hydra.context.Context, g: hydra.graph.Graph, t: Term): E =
    hydra.reduction.reduceTerm(cx)(g)(true)(t)

  // Apply a function term to an argument and reduce
  private def applyAndReduce(cx: hydra.context.Context, g: hydra.graph.Graph, f: Term, x: Term): E =
    reduce(cx, g, Term.application(Application(f, x)))

  // Apply a curried function to two arguments and reduce
  private def apply2AndReduce(cx: hydra.context.Context, g: hydra.graph.Graph, f: Term, x: Term, y: Term): E =
    reduce(cx, g, Term.application(Application(Term.application(Application(f, x)), y)))

  private def impl0(t: => Term): Impl = _ => _ => _ => ok(t)
  private def impl1(f: Term => Term): Impl = _ => _ => args => ok(f(args(0)))
  private def impl2(f: (Term, Term) => Term): Impl = _ => _ => args => ok(f(args(0), args(1)))
  private def impl3(f: (Term, Term, Term) => Term): Impl = _ => _ => args => ok(f(args(0), args(1), args(2)))

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

  private def exBigfloat(t: Term): BigDecimal = t match
    case Term.literal(Literal.float(FloatValue.bigfloat(n))) => n
    case _ => throw new RuntimeException(s"expected bigfloat, got $t")

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
    case Term.maybe(opt) => opt
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
  private def mkBigfloat(n: BigDecimal): Term = Term.literal(Literal.float(FloatValue.bigfloat(n)))
  private def mkString(s: String): Term = Term.literal(Literal.string(s))
  private def mkBool(b: Boolean): Term = Term.literal(Literal.boolean(b))
  private def mkBinary(b: String): Term = Term.literal(Literal.binary(b))
  private def mkList(items: Seq[Term]): Term = Term.list(items)
  private def mkSet(items: Set[Term]): Term = Term.set(items)
  private def mkMapTerm(entries: Map[Term, Term]): Term = Term.map(entries)
  private def mkMaybe(opt: Option[Term]): Term = Term.maybe(opt)
  private def mkEither(e: Either[Term, Term]): Term = Term.either(e)
  private def mkPairTerm(a: Term, b: Term): Term = Term.pair((a, b))
  private val mkUnit: Term = Term.unit

  /** Apply a function term to an argument term. The reducer will evaluate the result. */
  private def app(f: Term, x: Term): Term = Term.application(Application(f, x))

  /** Apply a curried 2-argument function: f(x)(y). */
  private def app2(f: Term, x: Term, y: Term): Term = app(app(f, x), y)

  // --- Higher-order traversal helpers ---

  /** Apply predicate to each element, collecting those where it returns true. */
  private def filterList(cx: hydra.context.Context, g: hydra.graph.Graph, p: Term, xs: Seq[Term]): E =
    xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
      for {
        acc <- accE
        result <- applyAndReduce(cx, g, p, x)
      } yield if exBool(result) then mkList(exList(acc) :+ x) else acc
    }

  /** Apply predicate to each element, partitioning into (true, false). */
  private def partitionList(cx: hydra.context.Context, g: hydra.graph.Graph, p: Term, xs: Seq[Term]): E =
    xs.foldLeft[E](ok(mkPairTerm(mkList(Seq.empty), mkList(Seq.empty)))) { (accE, x) =>
      for {
        acc <- accE
        result <- applyAndReduce(cx, g, p, x)
      } yield {
        val (ts, fs) = exPair(acc)
        if exBool(result) then mkPairTerm(mkList(exList(ts) :+ x), fs)
        else mkPairTerm(ts, mkList(exList(fs) :+ x))
      }
    }


  private def mkComparison(c: hydra.util.Comparison): Term =
    val fieldName = c match
      case hydra.util.Comparison.lessThan => "lessThan"
      case hydra.util.Comparison.equalTo => "equalTo"
      case hydra.util.Comparison.greaterThan => "greaterThan"
    Term.union(Injection("hydra.util.Comparison", Field(fieldName, mkUnit)))

  // --- Primitive constructors ---

  private def mkPrim(name: String, ts: TypeScheme): Primitive =
    Primitive(name, ts, stubImpl)

  private def mkPrimImpl(name: String, ts: TypeScheme, impl: Impl): Primitive =
    Primitive(name, ts, impl)

  // Type construction helpers
  private def tVar(n: String): Type = Type.variable(n)
  private def tFun(d: Type, c: Type): Type = Type.function(FunctionType(d, c))
  private def tList(t: Type): Type = Type.list(t)
  private def tSet(t: Type): Type = Type.set(t)
  private def tMap(k: Type, v: Type): Type = Type.map(MapType(k, v))
  private def tOpt(t: Type): Type = Type.maybe(t)
  private def tEither(l: Type, r: Type): Type = Type.either(EitherType(l, r))
  private def tPair(a: Type, b: Type): Type = Type.pair(PairType(a, b))
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
  private val tBigfloat: Type = Type.literal(LiteralType.float(FloatType.bigfloat))
  private val tUnit: Type = Type.unit
  private val tComparison: Type = Type.variable("hydra.util.Comparison")

  private def tScheme(vars: Seq[String], t: Type): TypeScheme = TypeScheme(vars, t, None)
  private def tMono(t: Type): TypeScheme = tScheme(Seq.empty, t)

  private def tSchemeConstrained(vars: Seq[(String, Seq[String])], t: Type): TypeScheme =
    val varNames = vars.map(_._1)
    val constraints = vars.collect { case (name, classes) if classes.nonEmpty =>
      name -> TypeVariableMetadata(classes.toSet)
    }.toMap
    TypeScheme(varNames, t, if constraints.isEmpty then None else Some(constraints))

  // ===== Chars primitives =====

  private def charsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.chars"
    Map(
      s"$ns.isAlphaNum" -> mkPrimImpl(s"$ns.isAlphaNum", tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isAlphaNum(exInt32(a))))),
      s"$ns.isLower" -> mkPrimImpl(s"$ns.isLower", tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isLower(exInt32(a))))),
      s"$ns.isSpace" -> mkPrimImpl(s"$ns.isSpace", tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isSpace(exInt32(a))))),
      s"$ns.isUpper" -> mkPrimImpl(s"$ns.isUpper", tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isUpper(exInt32(a))))),
      s"$ns.toLower" -> mkPrimImpl(s"$ns.toLower", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(chars.toLower(exInt32(a))))),
      s"$ns.toUpper" -> mkPrimImpl(s"$ns.toUpper", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(chars.toUpper(exInt32(a))))),
    )

  // ===== Equality primitives =====

  private def equalityPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.equality"
    val x = tVar("x")
    val xOrd = Seq(("x", Seq("ordering")))
    val xEq = Seq(("x", Seq("equality")))
    val xPlain = Seq(("x", Seq.empty))
    Map(
      // Polymorphic: compare, equal, gt, gte, lt, lte, max, min, identity
      // These work on Term values directly since they are polymorphic.
      // Use compareTerms for proper structural comparison of literal values.
      s"$ns.compare" -> mkPrimImpl(s"$ns.compare", tSchemeConstrained(xOrd, tFun(x, tFun(x, tComparison))),
        impl2 { (a, b) =>
          val c = equality.compareTerms(a, b)
          val comp = if c < 0 then hydra.util.Comparison.lessThan
                     else if c > 0 then hydra.util.Comparison.greaterThan
                     else hydra.util.Comparison.equalTo
          mkComparison(comp)
        }),
      s"$ns.equal" -> mkPrimImpl(s"$ns.equal", tSchemeConstrained(xEq, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(a == b))),
      s"$ns.identity" -> mkPrimImpl(s"$ns.identity", tSchemeConstrained(xPlain, tFun(x, x)),
        impl1(a => a)),
      s"$ns.gt" -> mkPrimImpl(s"$ns.gt", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) > 0))),
      s"$ns.gte" -> mkPrimImpl(s"$ns.gte", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) >= 0))),
      s"$ns.lt" -> mkPrimImpl(s"$ns.lt", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) < 0))),
      s"$ns.lte" -> mkPrimImpl(s"$ns.lte", tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) <= 0))),
      s"$ns.max" -> mkPrimImpl(s"$ns.max", tSchemeConstrained(xOrd, tFun(x, tFun(x, x))),
        impl2((a, b) => if equality.compareTerms(a, b) >= 0 then a else b)),
      s"$ns.min" -> mkPrimImpl(s"$ns.min", tSchemeConstrained(xOrd, tFun(x, tFun(x, x))),
        impl2((a, b) => if equality.compareTerms(a, b) <= 0 then a else b)),
    )

  // ===== Eithers primitives =====

  private def eithersPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.eithers"
    val x = tVar("x")
    val y = tVar("y")
    val z = tVar("z")
    val w = tVar("w")
    Map(
      // Higher-order: bind, bimap, either, foldl, map, mapList, mapMaybe, mapSet
      s"$ns.bind" -> mkPrimImpl(s"$ns.bind", tScheme(Seq("x", "y", "z"),
        tFun(tEither(x, y), tFun(tFun(y, tEither(x, z)), tEither(x, z)))),
        impl2 { (e, f) =>
          exEither(e) match
            case Left(_) => e
            case Right(v) => app(f, v)
        }),
      s"$ns.bimap" -> mkPrimImpl(s"$ns.bimap", tScheme(Seq("x", "y", "z", "w"),
        tFun(tFun(x, z), tFun(tFun(y, w), tFun(tEither(x, y), tEither(z, w))))),
        impl3 { (fl, fr, e) =>
          exEither(e) match
            case Left(a) => mkEither(Left(app(fl, a)))
            case Right(b) => mkEither(Right(app(fr, b)))
        }),
      s"$ns.either" -> mkPrimImpl(s"$ns.either", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, z), tFun(tFun(y, z), tFun(tEither(x, y), z)))),
        impl3 { (fl, fr, e) =>
          exEither(e) match
            case Left(a) => app(fl, a)
            case Right(b) => app(fr, b)
        }),
      s"$ns.foldl" -> mkPrimImpl(s"$ns.foldl", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tFun(y, tEither(z, x))), tFun(x, tFun(tList(y), tEither(z, x))))),
        cx => g => args => {
          val f = args(0); val init = args(1); val xs = exList(args(2))
          xs.foldLeft[E](ok(init)) { (accE, elem) =>
            accE.flatMap(acc => apply2AndReduce(cx, g, f, acc, elem))
          }
        }),
      // First-order: fromLeft, fromRight, isLeft, isRight, lefts, rights, partitionEithers
      s"$ns.fromLeft" -> mkPrimImpl(s"$ns.fromLeft", tScheme(Seq("x", "y"),
        tFun(x, tFun(tEither(x, y), x))),
        impl2((d, e) => exEither(e) match
          case Left(a) => a
          case Right(_) => d)),
      s"$ns.fromRight" -> mkPrimImpl(s"$ns.fromRight", tScheme(Seq("x", "y"),
        tFun(y, tFun(tEither(x, y), y))),
        impl2((d, e) => exEither(e) match
          case Left(_) => d
          case Right(b) => b)),
      s"$ns.isLeft" -> mkPrimImpl(s"$ns.isLeft", tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool)),
        impl1(e => mkBool(exEither(e).isLeft))),
      s"$ns.isRight" -> mkPrimImpl(s"$ns.isRight", tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool)),
        impl1(e => mkBool(exEither(e).isRight))),
      s"$ns.lefts" -> mkPrimImpl(s"$ns.lefts", tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(x))),
        impl1(es => mkList(exList(es).collect { case t if exEither(t).isLeft => exEither(t).left.toOption.get }))),
      s"$ns.map" -> mkPrimImpl(s"$ns.map", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, y), tFun(tEither(z, x), tEither(z, y)))),
        impl2 { (f, e) =>
          exEither(e) match
            case Left(_) => e
            case Right(v) => mkEither(Right(app(f, v)))
        }),
      s"$ns.mapList" -> mkPrimImpl(s"$ns.mapList", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tList(x), tEither(z, tList(y))))),
        cx => g => args => {
          val f = args(0); val xs = exList(args(1))
          xs.foldLeft[E](ok(mkEither(Right(mkList(Seq.empty))))) { (accE, elem) =>
            accE.flatMap { acc =>
              exEither(acc) match
                case Left(_) => ok(acc)
                case Right(soFar) =>
                  applyAndReduce(cx, g, f, elem).map { result =>
                    exEither(result) match
                      case Left(err) => mkEither(Left(err))
                      case Right(v) => mkEither(Right(mkList(exList(soFar) :+ v)))
                  }
            }
          }
        }),
      s"$ns.mapMaybe" -> mkPrimImpl(s"$ns.mapMaybe", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tOpt(x), tEither(z, tOpt(y))))),
        cx => g => args => {
          val f = args(0); val mx = exMaybe(args(1))
          mx match
            case None => ok(mkEither(Right(mkMaybe(None))))
            case Some(x) =>
              applyAndReduce(cx, g, f, x).map { result =>
                exEither(result) match
                  case Left(err) => mkEither(Left(err))
                  case Right(v) => mkEither(Right(mkMaybe(Some(v))))
              }
        }),
      s"$ns.mapSet" -> mkPrimImpl(s"$ns.mapSet", tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEither(z, y)), tFun(tSet(x), tEither(z, tSet(y))))),
        cx => g => args => {
          val f = args(0); val xs = exSet(args(1)).toSeq
          xs.foldLeft[E](ok(mkEither(Right(mkSet(Set.empty))))) { (accE, elem) =>
            accE.flatMap { acc =>
              exEither(acc) match
                case Left(_) => ok(acc)
                case Right(soFar) =>
                  applyAndReduce(cx, g, f, elem).map { result =>
                    exEither(result) match
                      case Left(err) => mkEither(Left(err))
                      case Right(v) => mkEither(Right(mkSet(exSet(soFar) + v)))
                  }
            }
          }
        }),
      s"$ns.partitionEithers" -> mkPrimImpl(s"$ns.partitionEithers", tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tPair(tList(x), tList(y)))),
        impl1 { es =>
          val items = exList(es).map(exEither)
          val lefts = items.collect { case Left(a) => a }
          val rights = items.collect { case Right(b) => b }
          mkPairTerm(mkList(lefts), mkList(rights))
        }),
      s"$ns.rights" -> mkPrimImpl(s"$ns.rights", tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(y))),
        impl1(es => mkList(exList(es).collect { case t if exEither(t).isRight => exEither(t).toOption.get }))),
    )

  // ===== Lists primitives =====

  private def listsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.lists"
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    val aEq = Seq(("a", Seq("equality")))
    val aOrd = Seq(("a", Seq("ordering")))
    val bOrd = Seq(("b", Seq("ordering")))
    Map(
      // Higher-order: apply, bind, dropWhile, filter, find, foldl, foldr, map, partition, sortOn, span, zipWith
      s"$ns.apply" -> mkPrimImpl(s"$ns.apply", tScheme(Seq("a", "b"),
        tFun(tList(tFun(a, b)), tFun(tList(a), tList(b)))),
        cx => g => args => {
          val fs = exList(args(0)); val xs = exList(args(1))
          val results = for { f <- fs; x <- xs } yield applyAndReduce(cx, g, f, x)
          results.foldLeft[E](ok(mkList(Seq.empty))) { (accE, rE) =>
            for { acc <- accE; r <- rE } yield mkList(exList(acc) :+ r)
          }
        }),
      s"$ns.bind" -> mkPrimImpl(s"$ns.bind", tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tFun(a, tList(b)), tList(b)))),
        cx => g => args => {
          val xs = exList(args(0)); val f = args(1)
          xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
            accE.flatMap { acc =>
              applyAndReduce(cx, g, f, x).map { result =>
                mkList(exList(acc) ++ exList(result))
              }
            }
          }
        }),
      s"$ns.dropWhile" -> mkPrimImpl(s"$ns.dropWhile", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a)))),
        cx => g => args => {
          val p = args(0); val xs = exList(args(1))
          // Find index of first element where predicate is false
          xs.indices.foldLeft[E](ok(mkInt32(-1))) { (accE, i) =>
            accE.flatMap { acc =>
              if exInt32(acc) >= 0 then ok(acc) // already found
              else applyAndReduce(cx, g, p, xs(i)).map(r => if !exBool(r) then mkInt32(i) else acc)
            }
          }.map { idx =>
            val i = exInt32(idx)
            if i < 0 then mkList(Seq.empty) else mkList(xs.drop(i))
          }
        }),
      s"$ns.filter" -> mkPrimImpl(s"$ns.filter", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a)))),
        cx => g => args => filterList(cx, g, args(0), exList(args(1)))),
      s"$ns.find" -> mkPrimImpl(s"$ns.find", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tOpt(a)))),
        cx => g => args => {
          val p = args(0); val xs = exList(args(1))
          xs.foldLeft[E](ok(mkMaybe(None))) { (accE, x) =>
            accE.flatMap { acc =>
              exMaybe(acc) match
                case Some(_) => ok(acc) // already found
                case None => applyAndReduce(cx, g, p, x).map(r => if exBool(r) then mkMaybe(Some(x)) else acc)
            }
          }
        }),
      s"$ns.foldl" -> mkPrimImpl(s"$ns.foldl", tScheme(Seq("b", "a"),
        tFun(tFun(b, tFun(a, b)), tFun(b, tFun(tList(a), b)))),
        impl3 { (f, init, xs) =>
          exList(xs).foldLeft(init)((acc, x) => app2(f, acc, x))
        }),
      s"$ns.foldr" -> mkPrimImpl(s"$ns.foldr", tScheme(Seq("a", "b"),
        tFun(tFun(a, tFun(b, b)), tFun(b, tFun(tList(a), b)))),
        impl3 { (f, init, xs) =>
          exList(xs).foldRight(init)((x, acc) => app2(f, x, acc))
        }),
      s"$ns.map" -> mkPrimImpl(s"$ns.map", tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tList(a), tList(b)))),
        impl2 { (f, xs) =>
          mkList(exList(xs).map(x => app(f, x)))
        }),
      s"$ns.partition" -> mkPrimImpl(s"$ns.partition", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a))))),
        cx => g => args => partitionList(cx, g, args(0), exList(args(1)))),
      s"$ns.sortOn" -> mkPrimImpl(s"$ns.sortOn", tSchemeConstrained(Seq(("a", Seq.empty), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tList(a), tList(a)))),
        cx => g => args => {
          val f = args(0); val xs = exList(args(1))
          // Compute sort keys for each element
          xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
            accE.flatMap { acc =>
              applyAndReduce(cx, g, f, x).map { key =>
                mkList(exList(acc) :+ mkPairTerm(key, x))
              }
            }
          }.map { paired =>
            val pairs = exList(paired).map(exPair)
            mkList(pairs.sortWith((a, b) => equality.compareTerms(a._1, b._1) < 0).map(_._2))
          }
        }),
      s"$ns.span" -> mkPrimImpl(s"$ns.span", tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a))))),
        cx => g => args => {
          val p = args(0); val xs = exList(args(1))
          // Find index of first element where predicate is false
          xs.indices.foldLeft[E](ok(mkInt32(-1))) { (accE, i) =>
            accE.flatMap { acc =>
              if exInt32(acc) >= 0 then ok(acc)
              else applyAndReduce(cx, g, p, xs(i)).map(r => if !exBool(r) then mkInt32(i) else acc)
            }
          }.map { idx =>
            val i = exInt32(idx)
            if i < 0 then mkPairTerm(mkList(xs), mkList(Seq.empty))
            else mkPairTerm(mkList(xs.take(i)), mkList(xs.drop(i)))
          }
        }),
      s"$ns.zipWith" -> mkPrimImpl(s"$ns.zipWith", tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tFun(b, c)), tFun(tList(a), tFun(tList(b), tList(c))))),
        impl3 { (f, xs, ys) =>
          mkList(exList(xs).zip(exList(ys)).map((x, y) => app2(f, x, y)))
        }),
      // First-order
      s"$ns.at" -> mkPrimImpl(s"$ns.at", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), a))),
        impl2((i, xs) => exList(xs)(exInt32(i)))),
      s"$ns.concat" -> mkPrimImpl(s"$ns.concat", tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(a))),
        impl1(xss => mkList(exList(xss).flatMap(exList)))),
      s"$ns.concat2" -> mkPrimImpl(s"$ns.concat2", tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(a), tList(a)))),
        impl2((xs, ys) => mkList(exList(xs) ++ exList(ys)))),
      s"$ns.cons" -> mkPrimImpl(s"$ns.cons", tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a)))),
        impl2((x, xs) => mkList(x +: exList(xs)))),
      s"$ns.drop" -> mkPrimImpl(s"$ns.drop", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a)))),
        impl2((n, xs) => mkList(exList(xs).drop(exInt32(n))))),
      s"$ns.elem" -> mkPrimImpl(s"$ns.elem", tSchemeConstrained(aEq,
        tFun(a, tFun(tList(a), tBool))),
        impl2((x, xs) => mkBool(exList(xs).contains(x)))),
      s"$ns.group" -> mkPrimImpl(s"$ns.group", tSchemeConstrained(aEq,
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
      s"$ns.head" -> mkPrimImpl(s"$ns.head", tScheme(Seq("a"),
        tFun(tList(a), a)),
        impl1(xs => exList(xs).head)),
      s"$ns.init" -> mkPrimImpl(s"$ns.init", tScheme(Seq("a"),
        tFun(tList(a), tList(a))),
        impl1(xs => { val items = exList(xs); mkList(if items.isEmpty then Seq.empty else items.init) })),
      s"$ns.intercalate" -> mkPrimImpl(s"$ns.intercalate", tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(tList(a)), tList(a)))),
        impl2 { (sep, xss) =>
          val sepItems = exList(sep)
          val lists = exList(xss).map(exList)
          mkList(if lists.isEmpty then Seq.empty else lists.reduceLeft((a, b) => a ++ sepItems ++ b))
        }),
      s"$ns.intersperse" -> mkPrimImpl(s"$ns.intersperse", tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a)))),
        impl2 { (sep, xs) =>
          val items = exList(xs)
          mkList(if items.isEmpty then Seq.empty else items.flatMap(x => Seq(sep, x)).tail)
        }),
      s"$ns.last" -> mkPrimImpl(s"$ns.last", tScheme(Seq("a"),
        tFun(tList(a), a)),
        impl1(xs => exList(xs).last)),
      s"$ns.length" -> mkPrimImpl(s"$ns.length", tScheme(Seq("a"),
        tFun(tList(a), tInt32)),
        impl1(xs => mkInt32(exList(xs).length))),
      s"$ns.maybeAt" -> mkPrimImpl(s"$ns.maybeAt", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tOpt(a)))),
        impl2((i, xs) => mkMaybe(lists.maybeAt(exInt32(i))(exList(xs))))),
      s"$ns.maybeHead" -> mkPrimImpl(s"$ns.maybeHead", tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).headOption))),
      s"$ns.maybeInit" -> mkPrimImpl(s"$ns.maybeInit", tScheme(Seq("a"),
        tFun(tList(a), tOpt(tList(a)))),
        impl1(xs => { val items = exList(xs); mkMaybe(lists.maybeInit(items).map(mkList)) })),
      s"$ns.maybeLast" -> mkPrimImpl(s"$ns.maybeLast", tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).lastOption))),
      s"$ns.maybeTail" -> mkPrimImpl(s"$ns.maybeTail", tScheme(Seq("a"),
        tFun(tList(a), tOpt(tList(a)))),
        impl1(xs => { val items = exList(xs); mkMaybe(lists.maybeTail(items).map(mkList)) })),
      s"$ns.nub" -> mkPrimImpl(s"$ns.nub", tSchemeConstrained(aEq,
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).distinct))),
      s"$ns.null" -> mkPrimImpl(s"$ns.null", tScheme(Seq("a"),
        tFun(tList(a), tBool)),
        impl1(xs => mkBool(exList(xs).isEmpty))),
      s"$ns.pure" -> mkPrimImpl(s"$ns.pure", tScheme(Seq("a"),
        tFun(a, tList(a))),
        impl1(x => mkList(Seq(x)))),
      s"$ns.replicate" -> mkPrimImpl(s"$ns.replicate", tScheme(Seq("a"),
        tFun(tInt32, tFun(a, tList(a)))),
        impl2((n, x) => mkList(Seq.fill(exInt32(n))(x)))),
      s"$ns.reverse" -> mkPrimImpl(s"$ns.reverse", tScheme(Seq("a"),
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).reverse))),
      s"$ns.safeHead" -> mkPrimImpl(s"$ns.safeHead", tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).headOption))),
      s"$ns.singleton" -> mkPrimImpl(s"$ns.singleton", tScheme(Seq("a"),
        tFun(a, tList(a))),
        impl1(x => mkList(Seq(x)))),
      s"$ns.sort" -> mkPrimImpl(s"$ns.sort", tSchemeConstrained(aOrd,
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).sortWith((a, b) => equality.lt(a)(b))))),
      s"$ns.tail" -> mkPrimImpl(s"$ns.tail", tScheme(Seq("a"),
        tFun(tList(a), tList(a))),
        impl1(xs => { val items = exList(xs); mkList(if items.isEmpty then Seq.empty else items.tail) })),
      s"$ns.take" -> mkPrimImpl(s"$ns.take", tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a)))),
        impl2((n, xs) => mkList(exList(xs).take(exInt32(n))))),
      s"$ns.transpose" -> mkPrimImpl(s"$ns.transpose", tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(tList(a)))),
        impl1 { xss =>
          val innerLists = exList(xss).map(exList)
          mkList(hydra.lib.lists.transpose(innerLists).map(mkList))
        }),
      s"$ns.zip" -> mkPrimImpl(s"$ns.zip", tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tList(b), tList(tPair(a, b))))),
        impl2((xs, ys) => mkList(exList(xs).zip(exList(ys)).map((a, b) => mkPairTerm(a, b))))),
    )

  // ===== Logic primitives =====

  private def logicPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.logic"
    val a = tVar("a")
    Map(
      s"$ns.and" -> mkPrimImpl(s"$ns.and", tMono(tFun(tBool, tFun(tBool, tBool))),
        impl2((a, b) => mkBool(exBool(a) && exBool(b)))),
      // ifElse is higher-order (lazy args act like functions)
      s"$ns.ifElse" -> mkPrimImpl(s"$ns.ifElse", tScheme(Seq("a"),
        tFun(tBool, tFun(a, tFun(a, a)))),
        impl3((cond, ifTrue, ifFalse) => if exBool(cond) then ifTrue else ifFalse)),
      s"$ns.not" -> mkPrimImpl(s"$ns.not", tMono(tFun(tBool, tBool)),
        impl1(a => mkBool(!exBool(a)))),
      s"$ns.or" -> mkPrimImpl(s"$ns.or", tMono(tFun(tBool, tFun(tBool, tBool))),
        impl2((a, b) => mkBool(exBool(a) || exBool(b)))),
    )

  // ===== Maps primitives =====

  private def mapsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.maps"
    val k = tVar("k")
    val k1 = tVar("k1")
    val k2 = tVar("k2")
    val v = tVar("v")
    val v1 = tVar("v1")
    val v2 = tVar("v2")
    val mapKV = tMap(k, v)
    Map(
      // Higher-order: alter, bimap, filter, filterWithKey, map, mapKeys
      s"$ns.alter" -> mkPrimImpl(s"$ns.alter", tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(tOpt(v), tOpt(v)), tFun(k, tFun(mapKV, mapKV)))),
        cx => g => args => {
          val f = args(0); val key = args(1); val m = exMap(args(2))
          val current = mkMaybe(m.get(key))
          applyAndReduce(cx, g, f, current).map { result =>
            exMaybe(result) match
              case None => mkMapTerm(m.removed(key))
              case Some(v) => mkMapTerm(m.updated(key, v))
          }
        }),
      s"$ns.bimap" -> mkPrimImpl(s"$ns.bimap", tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v1", Seq.empty), ("v2", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tFun(v1, v2), tFun(tMap(k1, v1), tMap(k2, v2))))),
        impl3 { (fk, fv, m) =>
          mkMapTerm(exMap(m).map((k, v) => app(fk, k) -> app(fv, v)))
        }),
      s"$ns.filter" -> mkPrimImpl(s"$ns.filter", tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v, tBool), tFun(mapKV, mapKV))),
        cx => g => args => {
          val p = args(0); val m = exMap(args(1))
          m.toSeq.foldLeft[E](ok(mkMapTerm(Map.empty))) { case (accE, (ek, ev)) =>
            accE.flatMap { acc =>
              applyAndReduce(cx, g, p, ev).map { result =>
                if exBool(result) then mkMapTerm(exMap(acc).updated(ek, ev)) else acc
              }
            }
          }
        }),
      s"$ns.filterWithKey" -> mkPrimImpl(s"$ns.filterWithKey", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k, tFun(v, tBool)), tFun(mapKV, mapKV))),
        cx => g => args => {
          val p = args(0); val m = exMap(args(1))
          m.toSeq.foldLeft[E](ok(mkMapTerm(Map.empty))) { case (accE, (ek, ev)) =>
            accE.flatMap { acc =>
              apply2AndReduce(cx, g, p, ek, ev).map { result =>
                if exBool(result) then mkMapTerm(exMap(acc).updated(ek, ev)) else acc
              }
            }
          }
        }),
      s"$ns.map" -> mkPrimImpl(s"$ns.map", tSchemeConstrained(Seq(("v1", Seq.empty), ("v2", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v1, v2), tFun(tMap(k, v1), tMap(k, v2)))),
        impl2 { (f, m) =>
          mkMapTerm(exMap(m).map((k, v) => k -> app(f, v)))
        }),
      s"$ns.mapKeys" -> mkPrimImpl(s"$ns.mapKeys", tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tMap(k1, v), tMap(k2, v)))),
        impl2 { (f, m) =>
          mkMapTerm(exMap(m).map((k, v) => app(f, k) -> v))
        }),
      // First-order
      s"$ns.delete" -> mkPrimImpl(s"$ns.delete", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, mapKV))),
        impl2((key, m) => mkMapTerm(exMap(m).removed(key)))),
      s"$ns.elems" -> mkPrimImpl(s"$ns.elems", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(v))),
        impl1(m => mkList(exMap(m).values.toSeq))),
      s"$ns.empty" -> mkPrimImpl(s"$ns.empty", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        mapKV),
        impl0(mkMapTerm(Map.empty))),
      s"$ns.findWithDefault" -> mkPrimImpl(s"$ns.findWithDefault", tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(v, tFun(k, tFun(mapKV, v)))),
        impl3((d, key, m) => exMap(m).getOrElse(key, d))),
      s"$ns.fromList" -> mkPrimImpl(s"$ns.fromList", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tList(tPair(k, v)), mapKV)),
        impl1(pairs => mkMapTerm(exList(pairs).map(p => { val (a, b) = exPair(p); a -> b }).toMap))),
      s"$ns.insert" -> mkPrimImpl(s"$ns.insert", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, tFun(mapKV, mapKV)))),
        impl3((key, value, m) => mkMapTerm(exMap(m).updated(key, value)))),
      s"$ns.keys" -> mkPrimImpl(s"$ns.keys", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(k))),
        impl1(m => mkList(exMap(m).keys.toSeq))),
      s"$ns.lookup" -> mkPrimImpl(s"$ns.lookup", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tOpt(v)))),
        impl2((key, m) => mkMaybe(exMap(m).get(key)))),
      s"$ns.member" -> mkPrimImpl(s"$ns.member", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tBool))),
        impl2((key, m) => mkBool(exMap(m).contains(key)))),
      s"$ns.null" -> mkPrimImpl(s"$ns.null", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tBool)),
        impl1(m => mkBool(exMap(m).isEmpty))),
      s"$ns.singleton" -> mkPrimImpl(s"$ns.singleton", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, mapKV))),
        impl2((key, value) => mkMapTerm(Map(key -> value)))),
      s"$ns.size" -> mkPrimImpl(s"$ns.size", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tInt32)),
        impl1(m => mkInt32(exMap(m).size))),
      s"$ns.toList" -> mkPrimImpl(s"$ns.toList", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(tPair(k, v)))),
        impl1(m => mkList(exMap(m).toSeq.map((k, v) => mkPairTerm(k, v))))),
      s"$ns.union" -> mkPrimImpl(s"$ns.union", tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tFun(mapKV, mapKV))),
        impl2((m1, m2) => mkMapTerm(exMap(m2) ++ exMap(m1)))),
    )

  // ===== Math primitives =====

  private def mathPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.math"
    Map(
      // Int32 primitives
      s"$ns.abs" -> mkPrimImpl(s"$ns.abs", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.abs(exInt32(a))))),
      s"$ns.add" -> mkPrimImpl(s"$ns.add", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.add(exInt32(a))(exInt32(b))))),
      s"$ns.div" -> mkPrimImpl(s"$ns.div", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.div(exInt32(a))(exInt32(b))))),
      s"$ns.even" -> mkPrimImpl(s"$ns.even", tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(math.even(exInt32(a))))),
      s"$ns.mod" -> mkPrimImpl(s"$ns.mod", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.mod(exInt32(a))(exInt32(b))))),
      s"$ns.mul" -> mkPrimImpl(s"$ns.mul", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.mul(exInt32(a))(exInt32(b))))),
      s"$ns.negate" -> mkPrimImpl(s"$ns.negate", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.negate(exInt32(a))))),
      s"$ns.odd" -> mkPrimImpl(s"$ns.odd", tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(math.odd(exInt32(a))))),
      s"$ns.pred" -> mkPrimImpl(s"$ns.pred", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.pred(exInt32(a))))),
      s"$ns.range" -> mkPrimImpl(s"$ns.range", tMono(tFun(tInt32, tFun(tInt32, tList(tInt32)))),
        impl2((a, b) => mkList(math.range(exInt32(a))(exInt32(b)).map(mkInt32)))),
      s"$ns.rem" -> mkPrimImpl(s"$ns.rem", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.rem(exInt32(a))(exInt32(b))))),
      s"$ns.signum" -> mkPrimImpl(s"$ns.signum", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.signum(exInt32(a))))),
      s"$ns.sub" -> mkPrimImpl(s"$ns.sub", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.sub(exInt32(a))(exInt32(b))))),
      s"$ns.succ" -> mkPrimImpl(s"$ns.succ", tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.succ(exInt32(a))))),
      s"$ns.max" -> mkPrimImpl(s"$ns.max", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.max(exInt32(a))(exInt32(b))))),
      s"$ns.maybeDiv" -> mkPrimImpl(s"$ns.maybeDiv", tMono(tFun(tInt32, tFun(tInt32, tOpt(tInt32)))),
        impl2((a, b) => mkMaybe(math.maybeDiv(exInt32(a))(exInt32(b)).map(mkInt32)))),
      s"$ns.maybeMod" -> mkPrimImpl(s"$ns.maybeMod", tMono(tFun(tInt32, tFun(tInt32, tOpt(tInt32)))),
        impl2((a, b) => mkMaybe(math.maybeMod(exInt32(a))(exInt32(b)).map(mkInt32)))),
      s"$ns.maybePred" -> mkPrimImpl(s"$ns.maybePred", tMono(tFun(tInt32, tOpt(tInt32))),
        impl1(a => mkMaybe(math.maybePred(exInt32(a)).map(mkInt32)))),
      s"$ns.maybeRem" -> mkPrimImpl(s"$ns.maybeRem", tMono(tFun(tInt32, tFun(tInt32, tOpt(tInt32)))),
        impl2((a, b) => mkMaybe(math.maybeRem(exInt32(a))(exInt32(b)).map(mkInt32)))),
      s"$ns.maybeSucc" -> mkPrimImpl(s"$ns.maybeSucc", tMono(tFun(tInt32, tOpt(tInt32))),
        impl1(a => mkMaybe(math.maybeSucc(exInt32(a)).map(mkInt32)))),
      s"$ns.min" -> mkPrimImpl(s"$ns.min", tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.min(exInt32(a))(exInt32(b))))),
      // Float64 primitives
      s"$ns.acos" -> mkPrimImpl(s"$ns.acos", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.acos(exFloat64(a))))),
      s"$ns.acosh" -> mkPrimImpl(s"$ns.acosh", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.acosh(exFloat64(a))))),
      s"$ns.asin" -> mkPrimImpl(s"$ns.asin", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.asin(exFloat64(a))))),
      s"$ns.asinh" -> mkPrimImpl(s"$ns.asinh", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.asinh(exFloat64(a))))),
      s"$ns.atan" -> mkPrimImpl(s"$ns.atan", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.atan(exFloat64(a))))),
      s"$ns.atan2" -> mkPrimImpl(s"$ns.atan2", tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.atan2(exFloat64(a))(exFloat64(b))))),
      s"$ns.atanh" -> mkPrimImpl(s"$ns.atanh", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.atanh(exFloat64(a))))),
      s"$ns.ceiling" -> mkPrimImpl(s"$ns.ceiling", tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.ceiling(exFloat64(a))))),
      s"$ns.cos" -> mkPrimImpl(s"$ns.cos", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.cos(exFloat64(a))))),
      s"$ns.cosh" -> mkPrimImpl(s"$ns.cosh", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.cosh(exFloat64(a))))),
      s"$ns.e" -> mkPrimImpl(s"$ns.e", tMono(tFloat64),
        impl0(mkFloat64(math.e))),
      s"$ns.exp" -> mkPrimImpl(s"$ns.exp", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.exp(exFloat64(a))))),
      s"$ns.floor" -> mkPrimImpl(s"$ns.floor", tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.floor(exFloat64(a))))),
      s"$ns.log" -> mkPrimImpl(s"$ns.log", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.log(exFloat64(a))))),
      s"$ns.logBase" -> mkPrimImpl(s"$ns.logBase", tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.logBase(exFloat64(a))(exFloat64(b))))),
      s"$ns.pi" -> mkPrimImpl(s"$ns.pi", tMono(tFloat64),
        impl0(mkFloat64(math.pi))),
      s"$ns.pow" -> mkPrimImpl(s"$ns.pow", tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.pow(exFloat64(a))(exFloat64(b))))),
      s"$ns.round" -> mkPrimImpl(s"$ns.round", tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.round(exFloat64(a))))),
      s"$ns.roundBigfloat" -> mkPrimImpl(s"$ns.roundBigfloat", tMono(tFun(tInt32, tFun(tBigfloat, tBigfloat))),
        impl2((p, x) => mkBigfloat(math.roundBigfloat(exInt32(p))(exBigfloat(x))))),
      s"$ns.roundFloat32" -> mkPrimImpl(s"$ns.roundFloat32", tMono(tFun(tInt32, tFun(tFloat32, tFloat32))),
        impl2((p, x) => mkFloat32(math.roundFloat32(exInt32(p))(exFloat32(x))))),
      s"$ns.roundFloat64" -> mkPrimImpl(s"$ns.roundFloat64", tMono(tFun(tInt32, tFun(tFloat64, tFloat64))),
        impl2((p, x) => mkFloat64(math.roundFloat64(exInt32(p))(exFloat64(x))))),
      s"$ns.sin" -> mkPrimImpl(s"$ns.sin", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sin(exFloat64(a))))),
      s"$ns.sinh" -> mkPrimImpl(s"$ns.sinh", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sinh(exFloat64(a))))),
      s"$ns.sqrt" -> mkPrimImpl(s"$ns.sqrt", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sqrt(exFloat64(a))))),
      s"$ns.tan" -> mkPrimImpl(s"$ns.tan", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.tan(exFloat64(a))))),
      s"$ns.tanh" -> mkPrimImpl(s"$ns.tanh", tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.tanh(exFloat64(a))))),
      s"$ns.truncate" -> mkPrimImpl(s"$ns.truncate", tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.truncate(exFloat64(a))))),
    )

  // ===== Maybes primitives =====

  private def maybesPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.maybes"
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    Map(
      // Higher-order: apply, bind, cases, compose, map, mapMaybe, maybe
      s"$ns.apply" -> mkPrimImpl(s"$ns.apply", tScheme(Seq("a", "b"),
        tFun(tOpt(tFun(a, b)), tFun(tOpt(a), tOpt(b)))),
        impl2 { (mf, mx) =>
          (exMaybe(mf), exMaybe(mx)) match
            case (Some(f), Some(x)) => mkMaybe(Some(app(f, x)))
            case _ => mkMaybe(None)
        }),
      s"$ns.bind" -> mkPrimImpl(s"$ns.bind", tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(tFun(a, tOpt(b)), tOpt(b)))),
        impl2 { (mx, f) =>
          exMaybe(mx) match
            case None => mkMaybe(None)
            case Some(x) => app(f, x)
        }),
      s"$ns.cases" -> mkPrimImpl(s"$ns.cases", tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(b, tFun(tFun(a, b), b)))),
        impl3 { (mx, d, f) =>
          exMaybe(mx) match
            case None => d
            case Some(x) => app(f, x)
        }),
      s"$ns.compose" -> mkPrimImpl(s"$ns.compose", tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tOpt(b)), tFun(tFun(b, tOpt(c)), tFun(a, tOpt(c))))),
        cx => g => args => {
          val f = args(0); val g2 = args(1); val x = args(2)
          applyAndReduce(cx, g, f, x).flatMap { mb =>
            exMaybe(mb) match
              case None => ok(mkMaybe(None))
              case Some(b) => applyAndReduce(cx, g, g2, b)
          }
        }),
      s"$ns.map" -> mkPrimImpl(s"$ns.map", tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tOpt(a), tOpt(b)))),
        impl2 { (f, mx) =>
          exMaybe(mx) match
            case None => mkMaybe(None)
            case Some(x) => mkMaybe(Some(app(f, x)))
        }),
      s"$ns.mapMaybe" -> mkPrimImpl(s"$ns.mapMaybe", tScheme(Seq("a", "b"),
        tFun(tFun(a, tOpt(b)), tFun(tList(a), tList(b)))),
        cx => g => args => {
          val f = args(0); val xs = exList(args(1))
          xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
            accE.flatMap { acc =>
              applyAndReduce(cx, g, f, x).map { result =>
                exMaybe(result) match
                  case None => acc
                  case Some(v) => mkList(exList(acc) :+ v)
              }
            }
          }
        }),
      s"$ns.maybe" -> mkPrimImpl(s"$ns.maybe", tScheme(Seq("b", "a"),
        tFun(b, tFun(tFun(a, b), tFun(tOpt(a), b)))),
        impl3 { (d, f, mx) =>
          exMaybe(mx) match
            case None => d
            case Some(x) => app(f, x)
        }),
      // First-order
      s"$ns.cat" -> mkPrimImpl(s"$ns.cat", tScheme(Seq("a"),
        tFun(tList(tOpt(a)), tList(a))),
        impl1(xs => mkList(exList(xs).flatMap(exMaybe)))),
      s"$ns.fromJust" -> mkPrimImpl(s"$ns.fromJust", tScheme(Seq("a"),
        tFun(tOpt(a), a)),
        impl1(ma => exMaybe(ma).get)),
      s"$ns.fromMaybe" -> mkPrimImpl(s"$ns.fromMaybe", tScheme(Seq("a"),
        tFun(a, tFun(tOpt(a), a))),
        impl2((d, ma) => exMaybe(ma).getOrElse(d))),
      s"$ns.isJust" -> mkPrimImpl(s"$ns.isJust", tScheme(Seq("a"),
        tFun(tOpt(a), tBool)),
        impl1(ma => mkBool(exMaybe(ma).isDefined))),
      s"$ns.isNothing" -> mkPrimImpl(s"$ns.isNothing", tScheme(Seq("a"),
        tFun(tOpt(a), tBool)),
        impl1(ma => mkBool(exMaybe(ma).isEmpty))),
      s"$ns.pure" -> mkPrimImpl(s"$ns.pure", tScheme(Seq("a"),
        tFun(a, tOpt(a))),
        impl1(a => mkMaybe(Some(a)))),
      s"$ns.toList" -> mkPrimImpl(s"$ns.toList", tScheme(Seq("a"),
        tFun(tOpt(a), tList(a))),
        impl1(ma => mkList(exMaybe(ma).toSeq))),
    )

  // ===== Sets primitives =====

  private def setsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.sets"
    val a = tVar("a")
    val b = tVar("b")
    val aOrd = Seq(("a", Seq("ordering")))
    Map(
      // Higher-order: map
      s"$ns.map" -> mkPrimImpl(s"$ns.map", tSchemeConstrained(Seq(("a", Seq("ordering")), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tSet(a), tSet(b)))),
        impl2 { (f, s) =>
          mkSet(exSet(s).map(x => app(f, x)))
        }),
      // First-order
      s"$ns.delete" -> mkPrimImpl(s"$ns.delete", tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a)))),
        impl2((x, s) => mkSet(exSet(s) - x))),
      s"$ns.difference" -> mkPrimImpl(s"$ns.difference", tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1) -- exSet(s2)))),
      s"$ns.empty" -> mkPrimImpl(s"$ns.empty", tSchemeConstrained(aOrd,
        tSet(a)),
        impl0(mkSet(Set.empty))),
      s"$ns.fromList" -> mkPrimImpl(s"$ns.fromList", tSchemeConstrained(aOrd,
        tFun(tList(a), tSet(a))),
        impl1(xs => mkSet(exList(xs).toSet))),
      s"$ns.insert" -> mkPrimImpl(s"$ns.insert", tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a)))),
        impl2((x, s) => mkSet(exSet(s) + x))),
      s"$ns.intersection" -> mkPrimImpl(s"$ns.intersection", tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1).intersect(exSet(s2))))),
      s"$ns.member" -> mkPrimImpl(s"$ns.member", tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tBool))),
        impl2((x, s) => mkBool(exSet(s).contains(x)))),
      s"$ns.null" -> mkPrimImpl(s"$ns.null", tSchemeConstrained(aOrd,
        tFun(tSet(a), tBool)),
        impl1(s => mkBool(exSet(s).isEmpty))),
      s"$ns.singleton" -> mkPrimImpl(s"$ns.singleton", tSchemeConstrained(aOrd,
        tFun(a, tSet(a))),
        impl1(x => mkSet(Set(x)))),
      s"$ns.size" -> mkPrimImpl(s"$ns.size", tSchemeConstrained(aOrd,
        tFun(tSet(a), tInt32)),
        impl1(s => mkInt32(exSet(s).size))),
      s"$ns.toList" -> mkPrimImpl(s"$ns.toList", tSchemeConstrained(aOrd,
        tFun(tSet(a), tList(a))),
        impl1(s => mkList(exSet(s).toSeq))),
      s"$ns.union" -> mkPrimImpl(s"$ns.union", tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1).union(exSet(s2))))),
      s"$ns.unions" -> mkPrimImpl(s"$ns.unions", tSchemeConstrained(aOrd,
        tFun(tList(tSet(a)), tSet(a))),
        impl1(ss => mkSet(exList(ss).flatMap(exSet).toSet))),
    )

  // ===== Regex primitives =====

  private def regexPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.regex"
    Map(
      s"$ns.find" -> mkPrimImpl(s"$ns.find", tMono(tFun(tString, tFun(tString, tOpt(tString)))),
        impl2((pat, input) => mkMaybe(regex.find(exString(pat))(exString(input)).map(mkString)))),
      s"$ns.findAll" -> mkPrimImpl(s"$ns.findAll", tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((pat, input) => mkList(regex.findAll(exString(pat))(exString(input)).map(mkString)))),
      s"$ns.matches" -> mkPrimImpl(s"$ns.matches", tMono(tFun(tString, tFun(tString, tBool))),
        impl2((pat, input) => mkBool(regex.matches(exString(pat))(exString(input))))),
      s"$ns.replace" -> mkPrimImpl(s"$ns.replace", tMono(tFun(tString, tFun(tString, tFun(tString, tString)))),
        impl3((pat, repl, input) => mkString(regex.replace(exString(pat))(exString(repl))(exString(input))))),
      s"$ns.replaceAll" -> mkPrimImpl(s"$ns.replaceAll", tMono(tFun(tString, tFun(tString, tFun(tString, tString)))),
        impl3((pat, repl, input) => mkString(regex.replaceAll(exString(pat))(exString(repl))(exString(input))))),
      s"$ns.split" -> mkPrimImpl(s"$ns.split", tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((pat, input) => mkList(regex.split(exString(pat))(exString(input)).map(mkString)))),
    )

  // ===== Strings primitives =====

  private def stringsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.strings"
    Map(
      s"$ns.cat" -> mkPrimImpl(s"$ns.cat", tMono(tFun(tList(tString), tString)),
        impl1(ss => mkString(strings.cat(exList(ss).map(exString))))),
      s"$ns.cat2" -> mkPrimImpl(s"$ns.cat2", tMono(tFun(tString, tFun(tString, tString))),
        impl2((a, b) => mkString(strings.cat2(exString(a))(exString(b))))),
      s"$ns.charAt" -> mkPrimImpl(s"$ns.charAt", tMono(tFun(tInt32, tFun(tString, tInt32))),
        impl2((i, s) => mkInt32(strings.charAt(exInt32(i))(exString(s))))),
      s"$ns.fromList" -> mkPrimImpl(s"$ns.fromList", tMono(tFun(tList(tInt32), tString)),
        impl1(cs => mkString(strings.fromList(exList(cs).map(exInt32))))),
      s"$ns.intercalate" -> mkPrimImpl(s"$ns.intercalate", tMono(tFun(tString, tFun(tList(tString), tString))),
        impl2((sep, ss) => mkString(strings.intercalate(exString(sep))(exList(ss).map(exString))))),
      s"$ns.length" -> mkPrimImpl(s"$ns.length", tMono(tFun(tString, tInt32)),
        impl1(s => mkInt32(strings.length(exString(s))))),
      s"$ns.lines" -> mkPrimImpl(s"$ns.lines", tMono(tFun(tString, tList(tString))),
        impl1(s => mkList(strings.lines(exString(s)).map(mkString)))),
      s"$ns.maybeCharAt" -> mkPrimImpl(s"$ns.maybeCharAt", tMono(tFun(tInt32, tFun(tString, tOpt(tInt32)))),
        impl2((i, s) => mkMaybe(strings.maybeCharAt(exInt32(i))(exString(s)).map(mkInt32)))),
      s"$ns.null" -> mkPrimImpl(s"$ns.null", tMono(tFun(tString, tBool)),
        impl1(s => mkBool(strings.`null`(exString(s))))),
      s"$ns.splitOn" -> mkPrimImpl(s"$ns.splitOn", tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((sep, s) => mkList(strings.splitOn(exString(sep))(exString(s)).map(mkString)))),
      s"$ns.toList" -> mkPrimImpl(s"$ns.toList", tMono(tFun(tString, tList(tInt32))),
        impl1(s => mkList(strings.toList(exString(s)).map(mkInt32)))),
      s"$ns.toLower" -> mkPrimImpl(s"$ns.toLower", tMono(tFun(tString, tString)),
        impl1(s => mkString(strings.toLower(exString(s))))),
      s"$ns.toUpper" -> mkPrimImpl(s"$ns.toUpper", tMono(tFun(tString, tString)),
        impl1(s => mkString(strings.toUpper(exString(s))))),
      s"$ns.unlines" -> mkPrimImpl(s"$ns.unlines", tMono(tFun(tList(tString), tString)),
        impl1(ss => mkString(strings.unlines(exList(ss).map(exString))))),
    )

  // ===== Literals primitives =====

  private def literalsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.literals"
    Map(
      // Conversion primitives
      s"$ns.bigfloatToBigint" -> mkPrimImpl(s"$ns.bigfloatToBigint", tMono(tFun(tBigfloat, tBigint)),
        impl1(a => mkBigint(literals.bigfloatToBigint(exBigfloat(a))))),
      s"$ns.bigfloatToFloat32" -> mkPrimImpl(s"$ns.bigfloatToFloat32", tMono(tFun(tBigfloat, tFloat32)),
        impl1(a => mkFloat32(literals.bigfloatToFloat32(exBigfloat(a))))),
      s"$ns.bigfloatToFloat64" -> mkPrimImpl(s"$ns.bigfloatToFloat64", tMono(tFun(tBigfloat, tFloat64)),
        impl1(a => mkFloat64(literals.bigfloatToFloat64(exBigfloat(a))))),
      s"$ns.bigintToBigfloat" -> mkPrimImpl(s"$ns.bigintToBigfloat", tMono(tFun(tBigint, tBigfloat)),
        impl1(a => mkBigfloat(literals.bigintToBigfloat(exBigint(a))))),
      s"$ns.bigintToInt8" -> mkPrimImpl(s"$ns.bigintToInt8", tMono(tFun(tBigint, tInt8)),
        impl1(a => mkInt8(literals.bigintToInt8(exBigint(a))))),
      s"$ns.bigintToInt16" -> mkPrimImpl(s"$ns.bigintToInt16", tMono(tFun(tBigint, tInt16)),
        impl1(a => mkInt16(literals.bigintToInt16(exBigint(a))))),
      s"$ns.bigintToInt32" -> mkPrimImpl(s"$ns.bigintToInt32", tMono(tFun(tBigint, tInt32)),
        impl1(a => mkInt32(literals.bigintToInt32(exBigint(a))))),
      s"$ns.bigintToInt64" -> mkPrimImpl(s"$ns.bigintToInt64", tMono(tFun(tBigint, tInt64)),
        impl1(a => mkInt64(literals.bigintToInt64(exBigint(a))))),
      s"$ns.bigintToUint8" -> mkPrimImpl(s"$ns.bigintToUint8", tMono(tFun(tBigint, tUint8)),
        impl1(a => mkUint8(literals.bigintToUint8(exBigint(a))))),
      s"$ns.bigintToUint16" -> mkPrimImpl(s"$ns.bigintToUint16", tMono(tFun(tBigint, tUint16)),
        impl1(a => mkUint16(literals.bigintToUint16(exBigint(a))))),
      s"$ns.bigintToUint32" -> mkPrimImpl(s"$ns.bigintToUint32", tMono(tFun(tBigint, tUint32)),
        impl1(a => mkUint32(literals.bigintToUint32(exBigint(a))))),
      s"$ns.bigintToUint64" -> mkPrimImpl(s"$ns.bigintToUint64", tMono(tFun(tBigint, tUint64)),
        impl1(a => mkUint64(literals.bigintToUint64(exBigint(a))))),
      s"$ns.binaryToBytes" -> mkPrimImpl(s"$ns.binaryToBytes", tMono(tFun(tBinary, tList(tInt32))),
        impl1(a => mkList(literals.binaryToBytes(exBinary(a)).map(mkInt32)))),
      s"$ns.binaryToString" -> mkPrimImpl(s"$ns.binaryToString", tMono(tFun(tBinary, tString)),
        impl1(a => mkString(literals.binaryToString(exBinary(a))))),
      s"$ns.float32ToBigfloat" -> mkPrimImpl(s"$ns.float32ToBigfloat", tMono(tFun(tFloat32, tBigfloat)),
        impl1(a => mkBigfloat(literals.float32ToBigfloat(exFloat32(a))))),
      s"$ns.float64ToBigfloat" -> mkPrimImpl(s"$ns.float64ToBigfloat", tMono(tFun(tFloat64, tBigfloat)),
        impl1(a => mkBigfloat(literals.float64ToBigfloat(exFloat64(a))))),
      s"$ns.int8ToBigint" -> mkPrimImpl(s"$ns.int8ToBigint", tMono(tFun(tInt8, tBigint)),
        impl1(a => mkBigint(literals.int8ToBigint(exInt8(a))))),
      s"$ns.int16ToBigint" -> mkPrimImpl(s"$ns.int16ToBigint", tMono(tFun(tInt16, tBigint)),
        impl1(a => mkBigint(literals.int16ToBigint(exInt16(a))))),
      s"$ns.int32ToBigint" -> mkPrimImpl(s"$ns.int32ToBigint", tMono(tFun(tInt32, tBigint)),
        impl1(a => mkBigint(literals.int32ToBigint(exInt32(a))))),
      s"$ns.int64ToBigint" -> mkPrimImpl(s"$ns.int64ToBigint", tMono(tFun(tInt64, tBigint)),
        impl1(a => mkBigint(literals.int64ToBigint(exInt64(a))))),
      // Read primitives
      s"$ns.readBigfloat" -> mkPrimImpl(s"$ns.readBigfloat", tMono(tFun(tString, tOpt(tBigfloat))),
        impl1(s => mkMaybe(literals.readBigfloat(exString(s)).map(mkBigfloat)))),
      s"$ns.readBigint" -> mkPrimImpl(s"$ns.readBigint", tMono(tFun(tString, tOpt(tBigint))),
        impl1(s => mkMaybe(literals.readBigint(exString(s)).map(mkBigint)))),
      s"$ns.readBoolean" -> mkPrimImpl(s"$ns.readBoolean", tMono(tFun(tString, tOpt(tBool))),
        impl1(s => mkMaybe(literals.readBoolean(exString(s)).map(mkBool)))),
      s"$ns.readFloat32" -> mkPrimImpl(s"$ns.readFloat32", tMono(tFun(tString, tOpt(tFloat32))),
        impl1(s => mkMaybe(literals.readFloat32(exString(s)).map(mkFloat32)))),
      s"$ns.readFloat64" -> mkPrimImpl(s"$ns.readFloat64", tMono(tFun(tString, tOpt(tFloat64))),
        impl1(s => mkMaybe(literals.readFloat64(exString(s)).map(mkFloat64)))),
      s"$ns.readInt8" -> mkPrimImpl(s"$ns.readInt8", tMono(tFun(tString, tOpt(tInt8))),
        impl1(s => mkMaybe(literals.readInt8(exString(s)).map(mkInt8)))),
      s"$ns.readInt16" -> mkPrimImpl(s"$ns.readInt16", tMono(tFun(tString, tOpt(tInt16))),
        impl1(s => mkMaybe(literals.readInt16(exString(s)).map(mkInt16)))),
      s"$ns.readInt32" -> mkPrimImpl(s"$ns.readInt32", tMono(tFun(tString, tOpt(tInt32))),
        impl1(s => mkMaybe(literals.readInt32(exString(s)).map(mkInt32)))),
      s"$ns.readInt64" -> mkPrimImpl(s"$ns.readInt64", tMono(tFun(tString, tOpt(tInt64))),
        impl1(s => mkMaybe(literals.readInt64(exString(s)).map(mkInt64)))),
      s"$ns.readString" -> mkPrimImpl(s"$ns.readString", tMono(tFun(tString, tOpt(tString))),
        impl1(s => mkMaybe(literals.readString(exString(s)).map(mkString)))),
      s"$ns.readUint8" -> mkPrimImpl(s"$ns.readUint8", tMono(tFun(tString, tOpt(tUint8))),
        impl1(s => mkMaybe(literals.readUint8(exString(s)).map(mkUint8)))),
      s"$ns.readUint16" -> mkPrimImpl(s"$ns.readUint16", tMono(tFun(tString, tOpt(tUint16))),
        impl1(s => mkMaybe(literals.readUint16(exString(s)).map(mkUint16)))),
      s"$ns.readUint32" -> mkPrimImpl(s"$ns.readUint32", tMono(tFun(tString, tOpt(tUint32))),
        impl1(s => mkMaybe(literals.readUint32(exString(s)).map(mkUint32)))),
      s"$ns.readUint64" -> mkPrimImpl(s"$ns.readUint64", tMono(tFun(tString, tOpt(tUint64))),
        impl1(s => mkMaybe(literals.readUint64(exString(s)).map(mkUint64)))),
      // Show primitives
      s"$ns.showBigfloat" -> mkPrimImpl(s"$ns.showBigfloat", tMono(tFun(tBigfloat, tString)),
        impl1(a => mkString(literals.showBigfloat(exBigfloat(a))))),
      s"$ns.showBigint" -> mkPrimImpl(s"$ns.showBigint", tMono(tFun(tBigint, tString)),
        impl1(a => mkString(literals.showBigint(exBigint(a))))),
      s"$ns.showBoolean" -> mkPrimImpl(s"$ns.showBoolean", tMono(tFun(tBool, tString)),
        impl1(a => mkString(literals.showBoolean(exBool(a))))),
      s"$ns.showFloat32" -> mkPrimImpl(s"$ns.showFloat32", tMono(tFun(tFloat32, tString)),
        impl1(a => mkString(literals.showFloat32(exFloat32(a))))),
      s"$ns.showFloat64" -> mkPrimImpl(s"$ns.showFloat64", tMono(tFun(tFloat64, tString)),
        impl1(a => mkString(literals.showFloat64(exFloat64(a))))),
      s"$ns.showInt8" -> mkPrimImpl(s"$ns.showInt8", tMono(tFun(tInt8, tString)),
        impl1(a => mkString(literals.showInt8(exInt8(a))))),
      s"$ns.showInt16" -> mkPrimImpl(s"$ns.showInt16", tMono(tFun(tInt16, tString)),
        impl1(a => mkString(literals.showInt16(exInt16(a))))),
      s"$ns.showInt32" -> mkPrimImpl(s"$ns.showInt32", tMono(tFun(tInt32, tString)),
        impl1(a => mkString(literals.showInt32(exInt32(a))))),
      s"$ns.showInt64" -> mkPrimImpl(s"$ns.showInt64", tMono(tFun(tInt64, tString)),
        impl1(a => mkString(literals.showInt64(exInt64(a))))),
      s"$ns.showUint8" -> mkPrimImpl(s"$ns.showUint8", tMono(tFun(tUint8, tString)),
        impl1(a => mkString(literals.showUint8(exUint8(a))))),
      s"$ns.showUint16" -> mkPrimImpl(s"$ns.showUint16", tMono(tFun(tUint16, tString)),
        impl1(a => mkString(literals.showUint16(exUint16(a))))),
      s"$ns.showUint32" -> mkPrimImpl(s"$ns.showUint32", tMono(tFun(tUint32, tString)),
        impl1(a => mkString(literals.showUint32(exUint32(a))))),
      s"$ns.showUint64" -> mkPrimImpl(s"$ns.showUint64", tMono(tFun(tUint64, tString)),
        impl1(a => mkString(literals.showUint64(exUint64(a))))),
      s"$ns.showString" -> mkPrimImpl(s"$ns.showString", tMono(tFun(tString, tString)),
        impl1(a => mkString(literals.showString(exString(a))))),
      s"$ns.stringToBinary" -> mkPrimImpl(s"$ns.stringToBinary", tMono(tFun(tString, tBinary)),
        impl1(a => mkBinary(literals.stringToBinary(exString(a))))),
      s"$ns.uint8ToBigint" -> mkPrimImpl(s"$ns.uint8ToBigint", tMono(tFun(tUint8, tBigint)),
        impl1(a => mkBigint(literals.uint8ToBigint(exUint8(a))))),
      s"$ns.uint16ToBigint" -> mkPrimImpl(s"$ns.uint16ToBigint", tMono(tFun(tUint16, tBigint)),
        impl1(a => mkBigint(literals.uint16ToBigint(exUint16(a))))),
      s"$ns.uint32ToBigint" -> mkPrimImpl(s"$ns.uint32ToBigint", tMono(tFun(tUint32, tBigint)),
        impl1(a => mkBigint(literals.uint32ToBigint(exUint32(a))))),
      s"$ns.uint64ToBigint" -> mkPrimImpl(s"$ns.uint64ToBigint", tMono(tFun(tUint64, tBigint)),
        impl1(a => mkBigint(literals.uint64ToBigint(exUint64(a))))),
    )

  // ===== Pairs primitives =====

  private def pairsPrimitives(): Map[String, Primitive] =
    val ns = "hydra.lib.pairs"
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    val d = tVar("d")
    Map(
      // Higher-order: bimap
      s"$ns.bimap" -> mkPrimImpl(s"$ns.bimap", tScheme(Seq("a", "b", "c", "d"),
        tFun(tFun(a, c), tFun(tFun(b, d), tFun(tPair(a, b), tPair(c, d))))),
        impl3 { (f, g, p) =>
          val (a, b) = exPair(p)
          mkPairTerm(app(f, a), app(g, b))
        }),
      // First-order
      s"$ns.first" -> mkPrimImpl(s"$ns.first", tScheme(Seq("a", "b"),
        tFun(tPair(a, b), a)),
        impl1(p => exPair(p)._1)),
      s"$ns.second" -> mkPrimImpl(s"$ns.second", tScheme(Seq("a", "b"),
        tFun(tPair(a, b), b)),
        impl1(p => exPair(p)._2)),
    )

  /** All standard primitives. */
  def standardPrimitives(): Map[String, Primitive] =
    charsPrimitives() ++
    equalityPrimitives() ++
    eithersPrimitives() ++
    listsPrimitives() ++
    literalsPrimitives() ++
    logicPrimitives() ++
    mapsPrimitives() ++
    mathPrimitives() ++
    maybesPrimitives() ++
    pairsPrimitives() ++
    regexPrimitives() ++
    setsPrimitives() ++
    stringsPrimitives()

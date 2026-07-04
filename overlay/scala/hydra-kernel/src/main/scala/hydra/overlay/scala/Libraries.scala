package hydra.overlay.scala

import hydra.core.*
import hydra.graph.Primitive
// #473 Step 0: native primitive implementations live in package hydra.overlay.scala.lib
// (the analog of Haskell's Hydra.Haskell.Lib.*), leaving hydra.lib free for the generated
// PrimitiveDefinition def-modules. Import the impl objects so the bare references below
// (chars.isAlphaNum, lists.cons, …) still resolve.
import hydra.overlay.scala.lib.{chars, eithers, equality, lists, literals, logic, maps, math, optionals, pairs, regex, sets, strings}

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
  private type Impl = hydra.graph.Graph => Seq[Term] => Either[hydra.errors.Error, Term]

  private val stubImpl: Impl =
    _ => _ => Left(hydra.errors.Error.other("stub primitive"))

  private def ok(t: Term): Either[hydra.errors.Error, Term] = Right(t)

  private type E = Either[hydra.errors.Error, Term]

  // Placeholder InferenceContext for the reducer machinery used inside the higher-order prim impls.
  // Since #446 the implementation carrier no longer threads the InferenceContext, but reduceTerm
  // still takes one (used only for error context / fresh-variable bookkeeping, neither of which
  // matters for primitive reduction), so an empty cx is sufficient and correct. Mirrors Haskell's
  // primCx = emptyInferenceContext in Hydra.Dsl.Prims.
  private val primCx: hydra.typing.InferenceContext = hydra.typing.InferenceContext(0, Seq.empty)

  // Reduce a term using the full reducer
  private def reduce(g: hydra.graph.Graph, t: Term): E =
    hydra.reduction.reduceTerm(primCx)(g)(true)(t)

  // Apply a function term to an argument and reduce
  private def applyAndReduce(g: hydra.graph.Graph, f: Term, x: Term): E =
    reduce(g, Term.application(Application(f, x)))

  // Apply a curried function to two arguments and reduce
  private def apply2AndReduce(g: hydra.graph.Graph, f: Term, x: Term, y: Term): E =
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
  private def filterList(g: hydra.graph.Graph, p: Term, xs: Seq[Term]): E =
    xs.foldLeft[E](ok(mkList(Seq.empty))) { (accE, x) =>
      for {
        acc <- accE
        result <- applyAndReduce(g, p, x)
      } yield if exBool(result) then mkList(exList(acc) :+ x) else acc
    }

  /** Apply predicate to each element, partitioning into (true, false). */
  private def partitionList(g: hydra.graph.Graph, p: Term, xs: Seq[Term]): E =
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


  private def mkComparison(c: hydra.util.Comparison): Term =
    val fieldName = c match
      case hydra.util.Comparison.lessThan => "lessThan"
      case hydra.util.Comparison.equalTo => "equalTo"
      case hydra.util.Comparison.greaterThan => "greaterThan"
    Term.inject(Injection("hydra.util.Comparison", Field(fieldName, mkUnit)))

  // --- Primitive constructors ---

  private def mkPrimDef(name: String, ts: TypeScheme): hydra.packaging.PrimitiveDefinition =
    hydra.packaging.PrimitiveDefinition(
      name,
      None,
      hydra.scoping.typeSchemeToTermSignature(ts),
      true,
      true,
      None)

  private def mkPrim(name: String, ts: TypeScheme): Primitive =
    Primitive(mkPrimDef(name, ts), stubImpl)

  private def mkPrimImpl(name: String, ts: TypeScheme, impl: Impl): Primitive =
    Primitive(mkPrimDef(name, ts), impl)

  // Mark the given (0-based) parameter positions as lazy. The Lisp coder
  // reads parameterIsLazy to decide which arguments to wrap in `(fn [] ...)`
  // thunks; without this, higher-order primitives like optionals.cases emit
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
  private val tFilePath: Type = Type.variable("hydra.file.FilePath")
  private val tFileError: Type = Type.variable("hydra.error.file.FileError")
  private val tFileStatus: Type = Type.variable("hydra.file.FileStatus")
  private val tCommand: Type = Type.variable("hydra.system.Command")
  private val tSystemError: Type = Type.variable("hydra.error.system.SystemError")
  private val tProcessResult: Type = Type.variable("hydra.system.ProcessResult")
  private val tStatusCode: Type = Type.variable("hydra.system.StatusCode")
  private val tTimespec: Type = Type.variable("hydra.time.Timespec")
  private val tEnvironmentVariable: Type = Type.variable("hydra.system.EnvironmentVariable")
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
  private val tComparison: Type = Type.variable("hydra.util.Comparison")

  private def tScheme(vars: Seq[String], t: Type): TypeScheme = TypeScheme(vars, t, None)
  private def tMono(t: Type): TypeScheme = tScheme(Seq.empty, t)

  private def tSchemeConstrained(vars: Seq[(String, Seq[String])], t: Type): TypeScheme =
    val varNames = vars.map(_._1)
    val constraints = vars.collect { case (name, classes) if classes.nonEmpty =>
      name -> TypeVariableConstraints(classes.map(c => TypeClassConstraint.simple(c)))
    }.toMap
    TypeScheme(varNames, t, if constraints.isEmpty then None else Some(constraints))

  // ===== Chars primitives =====

  private def charsPrimitives(): Map[String, Primitive] =
    Map(
      hydra.lib.chars.isAlphaNum.name -> mkPrimImpl(hydra.lib.chars.isAlphaNum.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isAlphaNum(exInt32(a))))),
      hydra.lib.chars.isLower.name -> mkPrimImpl(hydra.lib.chars.isLower.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isLower(exInt32(a))))),
      hydra.lib.chars.isSpace.name -> mkPrimImpl(hydra.lib.chars.isSpace.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isSpace(exInt32(a))))),
      hydra.lib.chars.isUpper.name -> mkPrimImpl(hydra.lib.chars.isUpper.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(chars.isUpper(exInt32(a))))),
      hydra.lib.chars.toLower.name -> mkPrimImpl(hydra.lib.chars.toLower.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(chars.toLower(exInt32(a))))),
      hydra.lib.chars.toUpper.name -> mkPrimImpl(hydra.lib.chars.toUpper.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(chars.toUpper(exInt32(a))))),
    )

  // ===== Equality primitives =====

  private def equalityPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val xOrd = Seq(("x", Seq("ordering")))
    val xEq = Seq(("x", Seq("equality")))
    val xPlain = Seq(("x", Seq.empty))
    Map(
      // Polymorphic: compare, equal, gt, gte, lt, lte, max, min, identity
      // These work on Term values directly since they are polymorphic.
      // Use compareTerms for proper structural comparison of literal values.
      hydra.lib.equality.compare.name -> mkPrimImpl(hydra.lib.equality.compare.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tComparison))),
        impl2 { (a, b) =>
          val c = equality.compareTerms(a, b)
          val comp = if c < 0 then hydra.util.Comparison.lessThan
                     else if c > 0 then hydra.util.Comparison.greaterThan
                     else hydra.util.Comparison.equalTo
          mkComparison(comp)
        }),
      hydra.lib.equality.equal.name -> mkPrimImpl(hydra.lib.equality.equal.name, tSchemeConstrained(xEq, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(a == b))),
      hydra.lib.equality.identity.name -> mkPrimImpl(hydra.lib.equality.identity.name, tSchemeConstrained(xPlain, tFun(x, x)),
        impl1(a => a)),
      hydra.lib.equality.gt.name -> mkPrimImpl(hydra.lib.equality.gt.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) > 0))),
      hydra.lib.equality.gte.name -> mkPrimImpl(hydra.lib.equality.gte.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) >= 0))),
      hydra.lib.equality.lt.name -> mkPrimImpl(hydra.lib.equality.lt.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) < 0))),
      hydra.lib.equality.lte.name -> mkPrimImpl(hydra.lib.equality.lte.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, tBool))),
        impl2((a, b) => mkBool(equality.compareTerms(a, b) <= 0))),
      hydra.lib.equality.max.name -> mkPrimImpl(hydra.lib.equality.max.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, x))),
        impl2((a, b) => if equality.compareTerms(a, b) >= 0 then a else b)),
      hydra.lib.equality.min.name -> mkPrimImpl(hydra.lib.equality.min.name, tSchemeConstrained(xOrd, tFun(x, tFun(x, x))),
        impl2((a, b) => if equality.compareTerms(a, b) <= 0 then a else b)),
    )

  // ===== Eithers primitives =====

  private def eithersPrimitives(): Map[String, Primitive] =
    val x = tVar("x")
    val y = tVar("y")
    val z = tVar("z")
    val w = tVar("w")
    Map(
      // Higher-order: bind, bimap, either, foldl, map, mapList, mapOptional, mapSet
      hydra.lib.eithers.bind.name -> mkPrimImpl(hydra.lib.eithers.bind.name, tScheme(Seq("x", "y", "z"),
        tFun(tEither(x, y), tFun(tFun(y, tEither(x, z)), tEither(x, z)))),
        impl2 { (e, f) =>
          exEither(e) match
            case Left(_) => e
            case Right(v) => app(f, v)
        }),
      hydra.lib.eithers.bimap.name -> mkPrimImpl(hydra.lib.eithers.bimap.name, tScheme(Seq("x", "y", "z", "w"),
        tFun(tFun(x, z), tFun(tFun(y, w), tFun(tEither(x, y), tEither(z, w))))),
        impl3 { (fl, fr, e) =>
          exEither(e) match
            case Left(a) => mkEither(Left(app(fl, a)))
            case Right(b) => mkEither(Right(app(fr, b)))
        }),
      hydra.lib.eithers.either.name -> mkPrimImpl(hydra.lib.eithers.either.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, z), tFun(tFun(y, z), tFun(tEither(x, y), z)))),
        impl3 { (fl, fr, e) =>
          exEither(e) match
            case Left(a) => app(fl, a)
            case Right(b) => app(fr, b)
        }),
      hydra.lib.eithers.foldl.name -> mkPrimImpl(hydra.lib.eithers.foldl.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tFun(y, tEither(z, x))), tFun(x, tFun(tList(y), tEither(z, x))))),
        g => args => {
          val f = args(0); val init = args(1); val xs = exList(args(2))
          xs.foldLeft[E](ok(init)) { (accE, elem) =>
            accE.flatMap(acc => apply2AndReduce(g, f, acc, elem))
          }
        }),
      // First-order: fromLeft, fromRight, isLeft, isRight, lefts, rights, partitionEithers
      hydra.lib.eithers.fromLeft.name -> withLazy(mkPrimImpl(hydra.lib.eithers.fromLeft.name, tScheme(Seq("x", "y"),
        tFun(x, tFun(tEither(x, y), x))),
        impl2((d, e) => exEither(e) match
          case Left(a) => a
          case Right(_) => d)), Seq(0)),
      hydra.lib.eithers.fromRight.name -> withLazy(mkPrimImpl(hydra.lib.eithers.fromRight.name, tScheme(Seq("x", "y"),
        tFun(y, tFun(tEither(x, y), y))),
        impl2((d, e) => exEither(e) match
          case Left(_) => d
          case Right(b) => b)), Seq(0)),
      hydra.lib.eithers.isLeft.name -> mkPrimImpl(hydra.lib.eithers.isLeft.name, tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool)),
        impl1(e => mkBool(exEither(e).isLeft))),
      hydra.lib.eithers.isRight.name -> mkPrimImpl(hydra.lib.eithers.isRight.name, tScheme(Seq("x", "y"),
        tFun(tEither(x, y), tBool)),
        impl1(e => mkBool(exEither(e).isRight))),
      hydra.lib.eithers.lefts.name -> mkPrimImpl(hydra.lib.eithers.lefts.name, tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tList(x))),
        impl1(es => mkList(exList(es).collect { case t if exEither(t).isLeft => exEither(t).left.toOption.get }))),
      hydra.lib.eithers.map.name -> mkPrimImpl(hydra.lib.eithers.map.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, y), tFun(tEither(z, x), tEither(z, y)))),
        impl2 { (f, e) =>
          exEither(e) match
            case Left(_) => e
            case Right(v) => mkEither(Right(app(f, v)))
        }),
      hydra.lib.eithers.mapList.name -> mkPrimImpl(hydra.lib.eithers.mapList.name, tScheme(Seq("x", "y", "z"),
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
      hydra.lib.eithers.mapOptional.name -> mkPrimImpl(hydra.lib.eithers.mapOptional.name, tScheme(Seq("x", "y", "z"),
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
      hydra.lib.eithers.mapSet.name -> mkPrimImpl(hydra.lib.eithers.mapSet.name, tScheme(Seq("x", "y", "z"),
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
      hydra.lib.eithers.partitionEithers.name -> mkPrimImpl(hydra.lib.eithers.partitionEithers.name, tScheme(Seq("x", "y"),
        tFun(tList(tEither(x, y)), tPair(tList(x), tList(y)))),
        impl1 { es =>
          val items = exList(es).map(exEither)
          val lefts = items.collect { case Left(a) => a }
          val rights = items.collect { case Right(b) => b }
          mkPairTerm(mkList(lefts), mkList(rights))
        }),
      hydra.lib.eithers.rights.name -> mkPrimImpl(hydra.lib.eithers.rights.name, tScheme(Seq("x", "y"),
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
      hydra.lib.lists.apply.name -> mkPrimImpl(hydra.lib.lists.apply.name, tScheme(Seq("a", "b"),
        tFun(tList(tFun(a, b)), tFun(tList(a), tList(b)))),
        g => args => {
          val fs = exList(args(0)); val xs = exList(args(1))
          val results = for { f <- fs; x <- xs } yield applyAndReduce(g, f, x)
          results.foldLeft[E](ok(mkList(Seq.empty))) { (accE, rE) =>
            for { acc <- accE; r <- rE } yield mkList(exList(acc) :+ r)
          }
        }),
      hydra.lib.lists.bind.name -> mkPrimImpl(hydra.lib.lists.bind.name, tScheme(Seq("a", "b"),
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
      hydra.lib.lists.dropWhile.name -> mkPrimImpl(hydra.lib.lists.dropWhile.name, tScheme(Seq("a"),
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
      hydra.lib.lists.filter.name -> mkPrimImpl(hydra.lib.lists.filter.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tList(a)))),
        g => args => filterList(g, args(0), exList(args(1)))),
      hydra.lib.lists.find.name -> mkPrimImpl(hydra.lib.lists.find.name, tScheme(Seq("a"),
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
      hydra.lib.lists.foldl.name -> mkPrimImpl(hydra.lib.lists.foldl.name, tScheme(Seq("b", "a"),
        tFun(tFun(b, tFun(a, b)), tFun(b, tFun(tList(a), b)))),
        impl3 { (f, init, xs) =>
          exList(xs).foldLeft(init)((acc, x) => app2(f, acc, x))
        }),
      hydra.lib.lists.foldr.name -> mkPrimImpl(hydra.lib.lists.foldr.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, tFun(b, b)), tFun(b, tFun(tList(a), b)))),
        impl3 { (f, init, xs) =>
          exList(xs).foldRight(init)((x, acc) => app2(f, x, acc))
        }),
      hydra.lib.lists.map.name -> mkPrimImpl(hydra.lib.lists.map.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tList(a), tList(b)))),
        impl2 { (f, xs) =>
          mkList(exList(xs).map(x => app(f, x)))
        }),
      hydra.lib.lists.partition.name -> mkPrimImpl(hydra.lib.lists.partition.name, tScheme(Seq("a"),
        tFun(tFun(a, tBool), tFun(tList(a), tPair(tList(a), tList(a))))),
        g => args => partitionList(g, args(0), exList(args(1)))),
      hydra.lib.lists.sortOn.name -> mkPrimImpl(hydra.lib.lists.sortOn.name, tSchemeConstrained(Seq(("a", Seq.empty), ("b", Seq("ordering"))),
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
            mkList(pairs.sortWith((a, b) => equality.compareTerms(a._1, b._1) < 0).map(_._2))
          }
        }),
      hydra.lib.lists.span.name -> mkPrimImpl(hydra.lib.lists.span.name, tScheme(Seq("a"),
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
      hydra.lib.lists.zipWith.name -> mkPrimImpl(hydra.lib.lists.zipWith.name, tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tFun(b, c)), tFun(tList(a), tFun(tList(b), tList(c))))),
        impl3 { (f, xs, ys) =>
          mkList(exList(xs).zip(exList(ys)).map((x, y) => app2(f, x, y)))
        }),
      // First-order
      hydra.lib.lists.concat.name -> mkPrimImpl(hydra.lib.lists.concat.name, tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(a))),
        impl1(xss => mkList(exList(xss).flatMap(exList)))),
      hydra.lib.lists.concat2.name -> mkPrimImpl(hydra.lib.lists.concat2.name, tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(a), tList(a)))),
        impl2((xs, ys) => mkList(exList(xs) ++ exList(ys)))),
      hydra.lib.lists.cons.name -> mkPrimImpl(hydra.lib.lists.cons.name, tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a)))),
        impl2((x, xs) => mkList(x +: exList(xs)))),
      hydra.lib.lists.drop.name -> mkPrimImpl(hydra.lib.lists.drop.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a)))),
        impl2((n, xs) => mkList(exList(xs).drop(exInt32(n))))),
      hydra.lib.lists.elem.name -> mkPrimImpl(hydra.lib.lists.elem.name, tSchemeConstrained(aEq,
        tFun(a, tFun(tList(a), tBool))),
        impl2((x, xs) => mkBool(exList(xs).contains(x)))),
      hydra.lib.lists.group.name -> mkPrimImpl(hydra.lib.lists.group.name, tSchemeConstrained(aEq,
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
      hydra.lib.lists.intercalate.name -> mkPrimImpl(hydra.lib.lists.intercalate.name, tScheme(Seq("a"),
        tFun(tList(a), tFun(tList(tList(a)), tList(a)))),
        impl2 { (sep, xss) =>
          val sepItems = exList(sep)
          val lists = exList(xss).map(exList)
          mkList(if lists.isEmpty then Seq.empty else lists.reduceLeft((a, b) => a ++ sepItems ++ b))
        }),
      hydra.lib.lists.intersperse.name -> mkPrimImpl(hydra.lib.lists.intersperse.name, tScheme(Seq("a"),
        tFun(a, tFun(tList(a), tList(a)))),
        impl2 { (sep, xs) =>
          val items = exList(xs)
          mkList(if items.isEmpty then Seq.empty else items.flatMap(x => Seq(sep, x)).tail)
        }),
      hydra.lib.lists.length.name -> mkPrimImpl(hydra.lib.lists.length.name, tScheme(Seq("a"),
        tFun(tList(a), tInt32)),
        impl1(xs => mkInt32(exList(xs).length))),
      hydra.lib.lists.maybeAt.name -> mkPrimImpl(hydra.lib.lists.maybeAt.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tOpt(a)))),
        impl2((i, xs) => mkMaybe(lists.maybeAt(exInt32(i))(exList(xs))))),
      hydra.lib.lists.maybeHead.name -> mkPrimImpl(hydra.lib.lists.maybeHead.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).headOption))),
      hydra.lib.lists.maybeInit.name -> mkPrimImpl(hydra.lib.lists.maybeInit.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(tList(a)))),
        impl1(xs => { val items = exList(xs); mkMaybe(lists.maybeInit(items).map(mkList)) })),
      hydra.lib.lists.maybeLast.name -> mkPrimImpl(hydra.lib.lists.maybeLast.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(a))),
        impl1(xs => mkMaybe(exList(xs).lastOption))),
      hydra.lib.lists.maybeTail.name -> mkPrimImpl(hydra.lib.lists.maybeTail.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(tList(a)))),
        impl1(xs => { val items = exList(xs); mkMaybe(lists.maybeTail(items).map(mkList)) })),
      hydra.lib.lists.nub.name -> mkPrimImpl(hydra.lib.lists.nub.name, tSchemeConstrained(aEq,
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).distinct))),
      hydra.lib.lists.`null`.name -> mkPrimImpl(hydra.lib.lists.`null`.name, tScheme(Seq("a"),
        tFun(tList(a), tBool)),
        impl1(xs => mkBool(exList(xs).isEmpty))),
      hydra.lib.lists.pure.name -> mkPrimImpl(hydra.lib.lists.pure.name, tScheme(Seq("a"),
        tFun(a, tList(a))),
        impl1(x => mkList(Seq(x)))),
      hydra.lib.lists.replicate.name -> mkPrimImpl(hydra.lib.lists.replicate.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(a, tList(a)))),
        impl2((n, x) => mkList(Seq.fill(exInt32(n))(x)))),
      hydra.lib.lists.reverse.name -> mkPrimImpl(hydra.lib.lists.reverse.name, tScheme(Seq("a"),
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).reverse))),
      hydra.lib.lists.singleton.name -> mkPrimImpl(hydra.lib.lists.singleton.name, tScheme(Seq("a"),
        tFun(a, tList(a))),
        impl1(x => mkList(Seq(x)))),
      hydra.lib.lists.sort.name -> mkPrimImpl(hydra.lib.lists.sort.name, tSchemeConstrained(aOrd,
        tFun(tList(a), tList(a))),
        impl1(xs => mkList(exList(xs).sortWith((a, b) => equality.lt(a)(b))))),
      hydra.lib.lists.take.name -> mkPrimImpl(hydra.lib.lists.take.name, tScheme(Seq("a"),
        tFun(tInt32, tFun(tList(a), tList(a)))),
        impl2((n, xs) => mkList(exList(xs).take(exInt32(n))))),
      hydra.lib.lists.transpose.name -> mkPrimImpl(hydra.lib.lists.transpose.name, tScheme(Seq("a"),
        tFun(tList(tList(a)), tList(tList(a)))),
        impl1 { xss =>
          val innerLists = exList(xss).map(exList)
          mkList(hydra.overlay.scala.lib.lists.transpose(innerLists).map(mkList))
        }),
      hydra.lib.lists.uncons.name -> mkPrimImpl(hydra.lib.lists.uncons.name, tScheme(Seq("a"),
        tFun(tList(a), tOpt(tPair(a, tList(a))))),
        impl1(xs => {
          val items = exList(xs)
          mkMaybe(lists.uncons(items).map((h, t) => mkPairTerm(h, mkList(t))))
        })),
      hydra.lib.lists.zip.name -> mkPrimImpl(hydra.lib.lists.zip.name, tScheme(Seq("a", "b"),
        tFun(tList(a), tFun(tList(b), tList(tPair(a, b))))),
        impl2((xs, ys) => mkList(exList(xs).zip(exList(ys)).map((a, b) => mkPairTerm(a, b))))),
    )

  // ===== Logic primitives =====

  private def logicPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    Map(
      hydra.lib.logic.and.name -> mkPrimImpl(hydra.lib.logic.and.name, tMono(tFun(tBool, tFun(tBool, tBool))),
        impl2((a, b) => mkBool(exBool(a) && exBool(b)))),
      // ifElse is higher-order (lazy args act like functions)
      hydra.lib.logic.ifElse.name -> withLazy(mkPrimImpl(hydra.lib.logic.ifElse.name, tScheme(Seq("a"),
        tFun(tBool, tFun(a, tFun(a, a)))),
        impl3((cond, ifTrue, ifFalse) => if exBool(cond) then ifTrue else ifFalse)), Seq(1, 2)),
      hydra.lib.logic.not.name -> mkPrimImpl(hydra.lib.logic.not.name, tMono(tFun(tBool, tBool)),
        impl1(a => mkBool(!exBool(a)))),
      hydra.lib.logic.or.name -> mkPrimImpl(hydra.lib.logic.or.name, tMono(tFun(tBool, tFun(tBool, tBool))),
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
      hydra.lib.maps.alter.name -> mkPrimImpl(hydra.lib.maps.alter.name, tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
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
      hydra.lib.maps.bimap.name -> mkPrimImpl(hydra.lib.maps.bimap.name, tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v1", Seq.empty), ("v2", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tFun(v1, v2), tFun(tMap(k1, v1), tMap(k2, v2))))),
        impl3 { (fk, fv, m) =>
          mkMapTerm(exMap(m).map((k, v) => app(fk, k) -> app(fv, v)))
        }),
      hydra.lib.maps.filter.name -> mkPrimImpl(hydra.lib.maps.filter.name, tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
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
      hydra.lib.maps.filterWithKey.name -> mkPrimImpl(hydra.lib.maps.filterWithKey.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
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
      hydra.lib.maps.map.name -> mkPrimImpl(hydra.lib.maps.map.name, tSchemeConstrained(Seq(("v1", Seq.empty), ("v2", Seq.empty), ("k", Seq("ordering"))),
        tFun(tFun(v1, v2), tFun(tMap(k, v1), tMap(k, v2)))),
        impl2 { (f, m) =>
          mkMapTerm(exMap(m).map((k, v) => k -> app(f, v)))
        }),
      hydra.lib.maps.mapKeys.name -> mkPrimImpl(hydra.lib.maps.mapKeys.name, tSchemeConstrained(Seq(("k1", Seq("ordering")), ("k2", Seq("ordering")), ("v", Seq.empty)),
        tFun(tFun(k1, k2), tFun(tMap(k1, v), tMap(k2, v)))),
        impl2 { (f, m) =>
          mkMapTerm(exMap(m).map((k, v) => app(f, k) -> v))
        }),
      // First-order
      hydra.lib.maps.delete.name -> mkPrimImpl(hydra.lib.maps.delete.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, mapKV))),
        impl2((key, m) => mkMapTerm(exMap(m).removed(key)))),
      hydra.lib.maps.elems.name -> mkPrimImpl(hydra.lib.maps.elems.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(v))),
        impl1(m => mkList(exMap(m).values.toSeq))),
      hydra.lib.maps.empty.name -> mkPrimImpl(hydra.lib.maps.empty.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        mapKV),
        impl0(mkMapTerm(Map.empty))),
      hydra.lib.maps.findWithDefault.name -> withLazy(mkPrimImpl(hydra.lib.maps.findWithDefault.name, tSchemeConstrained(Seq(("v", Seq.empty), ("k", Seq("ordering"))),
        tFun(v, tFun(k, tFun(mapKV, v)))),
        impl3((d, key, m) => exMap(m).getOrElse(key, d))), Seq(0)),
      hydra.lib.maps.fromList.name -> mkPrimImpl(hydra.lib.maps.fromList.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(tList(tPair(k, v)), mapKV)),
        impl1(pairs => mkMapTerm(exList(pairs).map(p => { val (a, b) = exPair(p); a -> b }).toMap))),
      hydra.lib.maps.insert.name -> mkPrimImpl(hydra.lib.maps.insert.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, tFun(mapKV, mapKV)))),
        impl3((key, value, m) => mkMapTerm(exMap(m).updated(key, value)))),
      hydra.lib.maps.keys.name -> mkPrimImpl(hydra.lib.maps.keys.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(k))),
        impl1(m => mkList(exMap(m).keys.toSeq))),
      hydra.lib.maps.lookup.name -> mkPrimImpl(hydra.lib.maps.lookup.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tOpt(v)))),
        impl2((key, m) => mkMaybe(exMap(m).get(key)))),
      hydra.lib.maps.member.name -> mkPrimImpl(hydra.lib.maps.member.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(mapKV, tBool))),
        impl2((key, m) => mkBool(exMap(m).contains(key)))),
      hydra.lib.maps.`null`.name -> mkPrimImpl(hydra.lib.maps.`null`.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tBool)),
        impl1(m => mkBool(exMap(m).isEmpty))),
      hydra.lib.maps.singleton.name -> mkPrimImpl(hydra.lib.maps.singleton.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(k, tFun(v, mapKV))),
        impl2((key, value) => mkMapTerm(Map(key -> value)))),
      hydra.lib.maps.size.name -> mkPrimImpl(hydra.lib.maps.size.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tInt32)),
        impl1(m => mkInt32(exMap(m).size))),
      hydra.lib.maps.toList.name -> mkPrimImpl(hydra.lib.maps.toList.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tList(tPair(k, v)))),
        impl1(m => mkList(exMap(m).toSeq.map((k, v) => mkPairTerm(k, v))))),
      hydra.lib.maps.union.name -> mkPrimImpl(hydra.lib.maps.union.name, tSchemeConstrained(Seq(("k", Seq("ordering")), ("v", Seq.empty)),
        tFun(mapKV, tFun(mapKV, mapKV))),
        impl2((m1, m2) => mkMapTerm(exMap(m2) ++ exMap(m1)))),
    )

  // ===== Math primitives =====

  private def mathPrimitives(): Map[String, Primitive] =
    Map(
      // Int32 primitives
      hydra.lib.math.abs.name -> mkPrimImpl(hydra.lib.math.abs.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.abs(exInt32(a))))),
      hydra.lib.math.add.name -> mkPrimImpl(hydra.lib.math.add.name, tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.add(exInt32(a))(exInt32(b))))),
      hydra.lib.math.even.name -> mkPrimImpl(hydra.lib.math.even.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(math.even(exInt32(a))))),
      hydra.lib.math.mul.name -> mkPrimImpl(hydra.lib.math.mul.name, tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.mul(exInt32(a))(exInt32(b))))),
      hydra.lib.math.negate.name -> mkPrimImpl(hydra.lib.math.negate.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.negate(exInt32(a))))),
      hydra.lib.math.odd.name -> mkPrimImpl(hydra.lib.math.odd.name, tMono(tFun(tInt32, tBool)),
        impl1(a => mkBool(math.odd(exInt32(a))))),
      hydra.lib.math.range.name -> mkPrimImpl(hydra.lib.math.range.name, tMono(tFun(tInt32, tFun(tInt32, tList(tInt32)))),
        impl2((a, b) => mkList(math.range(exInt32(a))(exInt32(b)).map(mkInt32)))),
      hydra.lib.math.signum.name -> mkPrimImpl(hydra.lib.math.signum.name, tMono(tFun(tInt32, tInt32)),
        impl1(a => mkInt32(math.signum(exInt32(a))))),
      hydra.lib.math.sub.name -> mkPrimImpl(hydra.lib.math.sub.name, tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.sub(exInt32(a))(exInt32(b))))),
      hydra.lib.math.max.name -> mkPrimImpl(hydra.lib.math.max.name, tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.max(exInt32(a))(exInt32(b))))),
      hydra.lib.math.maybeDiv.name -> mkPrimImpl(hydra.lib.math.maybeDiv.name, tMono(tFun(tInt32, tFun(tInt32, tOpt(tInt32)))),
        impl2((a, b) => mkMaybe(math.maybeDiv(exInt32(a))(exInt32(b)).map(mkInt32)))),
      hydra.lib.math.maybeMod.name -> mkPrimImpl(hydra.lib.math.maybeMod.name, tMono(tFun(tInt32, tFun(tInt32, tOpt(tInt32)))),
        impl2((a, b) => mkMaybe(math.maybeMod(exInt32(a))(exInt32(b)).map(mkInt32)))),
      hydra.lib.math.maybePred.name -> mkPrimImpl(hydra.lib.math.maybePred.name, tMono(tFun(tInt32, tOpt(tInt32))),
        impl1(a => mkMaybe(math.maybePred(exInt32(a)).map(mkInt32)))),
      hydra.lib.math.maybeRem.name -> mkPrimImpl(hydra.lib.math.maybeRem.name, tMono(tFun(tInt32, tFun(tInt32, tOpt(tInt32)))),
        impl2((a, b) => mkMaybe(math.maybeRem(exInt32(a))(exInt32(b)).map(mkInt32)))),
      hydra.lib.math.maybeSucc.name -> mkPrimImpl(hydra.lib.math.maybeSucc.name, tMono(tFun(tInt32, tOpt(tInt32))),
        impl1(a => mkMaybe(math.maybeSucc(exInt32(a)).map(mkInt32)))),
      hydra.lib.math.min.name -> mkPrimImpl(hydra.lib.math.min.name, tMono(tFun(tInt32, tFun(tInt32, tInt32))),
        impl2((a, b) => mkInt32(math.min(exInt32(a))(exInt32(b))))),
      // Float64 primitives
      hydra.lib.math.addFloat64.name -> mkPrimImpl(hydra.lib.math.addFloat64.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.addFloat64(exFloat64(a))(exFloat64(b))))),
      hydra.lib.math.acos.name -> mkPrimImpl(hydra.lib.math.acos.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.acos(exFloat64(a))))),
      hydra.lib.math.acosh.name -> mkPrimImpl(hydra.lib.math.acosh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.acosh(exFloat64(a))))),
      hydra.lib.math.asin.name -> mkPrimImpl(hydra.lib.math.asin.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.asin(exFloat64(a))))),
      hydra.lib.math.asinh.name -> mkPrimImpl(hydra.lib.math.asinh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.asinh(exFloat64(a))))),
      hydra.lib.math.atan.name -> mkPrimImpl(hydra.lib.math.atan.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.atan(exFloat64(a))))),
      hydra.lib.math.atan2.name -> mkPrimImpl(hydra.lib.math.atan2.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.atan2(exFloat64(a))(exFloat64(b))))),
      hydra.lib.math.atanh.name -> mkPrimImpl(hydra.lib.math.atanh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.atanh(exFloat64(a))))),
      hydra.lib.math.ceiling.name -> mkPrimImpl(hydra.lib.math.ceiling.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.ceiling(exFloat64(a))))),
      hydra.lib.math.cos.name -> mkPrimImpl(hydra.lib.math.cos.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.cos(exFloat64(a))))),
      hydra.lib.math.cosh.name -> mkPrimImpl(hydra.lib.math.cosh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.cosh(exFloat64(a))))),
      hydra.lib.math.e.name -> mkPrimImpl(hydra.lib.math.e.name, tMono(tFloat64),
        impl0(mkFloat64(math.e))),
      hydra.lib.math.exp.name -> mkPrimImpl(hydra.lib.math.exp.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.exp(exFloat64(a))))),
      hydra.lib.math.floor.name -> mkPrimImpl(hydra.lib.math.floor.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.floor(exFloat64(a))))),
      hydra.lib.math.log.name -> mkPrimImpl(hydra.lib.math.log.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.log(exFloat64(a))))),
      hydra.lib.math.mulFloat64.name -> mkPrimImpl(hydra.lib.math.mulFloat64.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.mulFloat64(exFloat64(a))(exFloat64(b))))),
      hydra.lib.math.negateFloat64.name -> mkPrimImpl(hydra.lib.math.negateFloat64.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.negateFloat64(exFloat64(a))))),
      hydra.lib.math.logBase.name -> mkPrimImpl(hydra.lib.math.logBase.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.logBase(exFloat64(a))(exFloat64(b))))),
      hydra.lib.math.pi.name -> mkPrimImpl(hydra.lib.math.pi.name, tMono(tFloat64),
        impl0(mkFloat64(math.pi))),
      hydra.lib.math.pow.name -> mkPrimImpl(hydra.lib.math.pow.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.pow(exFloat64(a))(exFloat64(b))))),
      hydra.lib.math.round.name -> mkPrimImpl(hydra.lib.math.round.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.round(exFloat64(a))))),
      hydra.lib.math.roundFloat32.name -> mkPrimImpl(hydra.lib.math.roundFloat32.name, tMono(tFun(tInt32, tFun(tFloat32, tFloat32))),
        impl2((p, x) => mkFloat32(math.roundFloat32(exInt32(p))(exFloat32(x))))),
      hydra.lib.math.roundFloat64.name -> mkPrimImpl(hydra.lib.math.roundFloat64.name, tMono(tFun(tInt32, tFun(tFloat64, tFloat64))),
        impl2((p, x) => mkFloat64(math.roundFloat64(exInt32(p))(exFloat64(x))))),
      hydra.lib.math.sin.name -> mkPrimImpl(hydra.lib.math.sin.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sin(exFloat64(a))))),
      hydra.lib.math.sinh.name -> mkPrimImpl(hydra.lib.math.sinh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sinh(exFloat64(a))))),
      hydra.lib.math.sqrt.name -> mkPrimImpl(hydra.lib.math.sqrt.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.sqrt(exFloat64(a))))),
      hydra.lib.math.subFloat64.name -> mkPrimImpl(hydra.lib.math.subFloat64.name, tMono(tFun(tFloat64, tFun(tFloat64, tFloat64))),
        impl2((a, b) => mkFloat64(math.subFloat64(exFloat64(a))(exFloat64(b))))),
      hydra.lib.math.tan.name -> mkPrimImpl(hydra.lib.math.tan.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.tan(exFloat64(a))))),
      hydra.lib.math.tanh.name -> mkPrimImpl(hydra.lib.math.tanh.name, tMono(tFun(tFloat64, tFloat64)),
        impl1(a => mkFloat64(math.tanh(exFloat64(a))))),
      hydra.lib.math.truncate.name -> mkPrimImpl(hydra.lib.math.truncate.name, tMono(tFun(tFloat64, tBigint)),
        impl1(a => mkFloat64(math.truncate(exFloat64(a))))),
    )

  // ===== Maybes primitives =====

  private def optionalsPrimitives(): Map[String, Primitive] =
    val a = tVar("a")
    val b = tVar("b")
    val c = tVar("c")
    Map(
      // Higher-order: apply, bind, cases, compose, map, mapOptional
      hydra.lib.optionals.apply.name -> mkPrimImpl(hydra.lib.optionals.apply.name, tScheme(Seq("a", "b"),
        tFun(tOpt(tFun(a, b)), tFun(tOpt(a), tOpt(b)))),
        impl2 { (mf, mx) =>
          (exMaybe(mf), exMaybe(mx)) match
            case (Some(f), Some(x)) => mkMaybe(Some(app(f, x)))
            case _ => mkMaybe(None)
        }),
      hydra.lib.optionals.bind.name -> mkPrimImpl(hydra.lib.optionals.bind.name, tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(tFun(a, tOpt(b)), tOpt(b)))),
        impl2 { (mx, f) =>
          exMaybe(mx) match
            case None => mkMaybe(None)
            case Some(x) => app(f, x)
        }),
      hydra.lib.optionals.cases.name -> withLazy(mkPrimImpl(hydra.lib.optionals.cases.name, tScheme(Seq("a", "b"),
        tFun(tOpt(a), tFun(b, tFun(tFun(a, b), b)))),
        impl3 { (mx, d, f) =>
          exMaybe(mx) match
            case None => d
            case Some(x) => app(f, x)
        }), Seq(1)),
      hydra.lib.optionals.compose.name -> mkPrimImpl(hydra.lib.optionals.compose.name, tScheme(Seq("a", "b", "c"),
        tFun(tFun(a, tOpt(b)), tFun(tFun(b, tOpt(c)), tFun(a, tOpt(c))))),
        g => args => {
          val f = args(0); val g2 = args(1); val x = args(2)
          applyAndReduce(g, f, x).flatMap { mb =>
            exMaybe(mb) match
              case None => ok(mkMaybe(None))
              case Some(b) => applyAndReduce(g, g2, b)
          }
        }),
      hydra.lib.optionals.map.name -> mkPrimImpl(hydra.lib.optionals.map.name, tScheme(Seq("a", "b"),
        tFun(tFun(a, b), tFun(tOpt(a), tOpt(b)))),
        impl2 { (f, mx) =>
          exMaybe(mx) match
            case None => mkMaybe(None)
            case Some(x) => mkMaybe(Some(app(f, x)))
        }),
      hydra.lib.optionals.mapOptional.name -> mkPrimImpl(hydra.lib.optionals.mapOptional.name, tScheme(Seq("a", "b"),
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
      hydra.lib.optionals.cat.name -> mkPrimImpl(hydra.lib.optionals.cat.name, tScheme(Seq("a"),
        tFun(tList(tOpt(a)), tList(a))),
        impl1(xs => mkList(exList(xs).flatMap(exMaybe)))),
      hydra.lib.optionals.fromOptional.name -> withLazy(mkPrimImpl(hydra.lib.optionals.fromOptional.name, tScheme(Seq("a"),
        tFun(a, tFun(tOpt(a), a))),
        impl2((d, ma) => exMaybe(ma).getOrElse(d))), Seq(0)),
      hydra.lib.optionals.isGiven.name -> mkPrimImpl(hydra.lib.optionals.isGiven.name, tScheme(Seq("a"),
        tFun(tOpt(a), tBool)),
        impl1(ma => mkBool(exMaybe(ma).isDefined))),
      hydra.lib.optionals.isNone.name -> mkPrimImpl(hydra.lib.optionals.isNone.name, tScheme(Seq("a"),
        tFun(tOpt(a), tBool)),
        impl1(ma => mkBool(exMaybe(ma).isEmpty))),
      hydra.lib.optionals.pure.name -> mkPrimImpl(hydra.lib.optionals.pure.name, tScheme(Seq("a"),
        tFun(a, tOpt(a))),
        impl1(a => mkMaybe(Some(a)))),
      hydra.lib.optionals.toList.name -> mkPrimImpl(hydra.lib.optionals.toList.name, tScheme(Seq("a"),
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
      hydra.lib.sets.map.name -> mkPrimImpl(hydra.lib.sets.map.name, tSchemeConstrained(Seq(("a", Seq("ordering")), ("b", Seq("ordering"))),
        tFun(tFun(a, b), tFun(tSet(a), tSet(b)))),
        impl2 { (f, s) =>
          mkSet(exSet(s).map(x => app(f, x)))
        }),
      // First-order
      hydra.lib.sets.delete.name -> mkPrimImpl(hydra.lib.sets.delete.name, tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a)))),
        impl2((x, s) => mkSet(exSet(s) - x))),
      hydra.lib.sets.difference.name -> mkPrimImpl(hydra.lib.sets.difference.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1) -- exSet(s2)))),
      hydra.lib.sets.empty.name -> mkPrimImpl(hydra.lib.sets.empty.name, tSchemeConstrained(aOrd,
        tSet(a)),
        impl0(mkSet(Set.empty))),
      hydra.lib.sets.fromList.name -> mkPrimImpl(hydra.lib.sets.fromList.name, tSchemeConstrained(aOrd,
        tFun(tList(a), tSet(a))),
        impl1(xs => mkSet(exList(xs).toSet))),
      hydra.lib.sets.insert.name -> mkPrimImpl(hydra.lib.sets.insert.name, tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tSet(a)))),
        impl2((x, s) => mkSet(exSet(s) + x))),
      hydra.lib.sets.intersection.name -> mkPrimImpl(hydra.lib.sets.intersection.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1).intersect(exSet(s2))))),
      hydra.lib.sets.member.name -> mkPrimImpl(hydra.lib.sets.member.name, tSchemeConstrained(aOrd,
        tFun(a, tFun(tSet(a), tBool))),
        impl2((x, s) => mkBool(exSet(s).contains(x)))),
      hydra.lib.sets.`null`.name -> mkPrimImpl(hydra.lib.sets.`null`.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tBool)),
        impl1(s => mkBool(exSet(s).isEmpty))),
      hydra.lib.sets.singleton.name -> mkPrimImpl(hydra.lib.sets.singleton.name, tSchemeConstrained(aOrd,
        tFun(a, tSet(a))),
        impl1(x => mkSet(Set(x)))),
      hydra.lib.sets.size.name -> mkPrimImpl(hydra.lib.sets.size.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tInt32)),
        impl1(s => mkInt32(exSet(s).size))),
      hydra.lib.sets.toList.name -> mkPrimImpl(hydra.lib.sets.toList.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tList(a))),
        impl1(s => mkList(exSet(s).toSeq))),
      hydra.lib.sets.union.name -> mkPrimImpl(hydra.lib.sets.union.name, tSchemeConstrained(aOrd,
        tFun(tSet(a), tFun(tSet(a), tSet(a)))),
        impl2((s1, s2) => mkSet(exSet(s1).union(exSet(s2))))),
      hydra.lib.sets.unions.name -> mkPrimImpl(hydra.lib.sets.unions.name, tSchemeConstrained(aOrd,
        tFun(tList(tSet(a)), tSet(a))),
        impl1(ss => mkSet(exList(ss).flatMap(exSet).toSet))),
    )

  // ===== Regex primitives =====

  private def regexPrimitives(): Map[String, Primitive] =
    Map(
      hydra.lib.regex.find.name -> mkPrimImpl(hydra.lib.regex.find.name, tMono(tFun(tString, tFun(tString, tOpt(tString)))),
        impl2((pat, input) => mkMaybe(regex.find(exString(pat))(exString(input)).map(mkString)))),
      hydra.lib.regex.findAll.name -> mkPrimImpl(hydra.lib.regex.findAll.name, tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((pat, input) => mkList(regex.findAll(exString(pat))(exString(input)).map(mkString)))),
      hydra.lib.regex.matches.name -> mkPrimImpl(hydra.lib.regex.matches.name, tMono(tFun(tString, tFun(tString, tBool))),
        impl2((pat, input) => mkBool(regex.matches(exString(pat))(exString(input))))),
      hydra.lib.regex.replace.name -> mkPrimImpl(hydra.lib.regex.replace.name, tMono(tFun(tString, tFun(tString, tFun(tString, tString)))),
        impl3((pat, repl, input) => mkString(regex.replace(exString(pat))(exString(repl))(exString(input))))),
      hydra.lib.regex.replaceAll.name -> mkPrimImpl(hydra.lib.regex.replaceAll.name, tMono(tFun(tString, tFun(tString, tFun(tString, tString)))),
        impl3((pat, repl, input) => mkString(regex.replaceAll(exString(pat))(exString(repl))(exString(input))))),
      hydra.lib.regex.split.name -> mkPrimImpl(hydra.lib.regex.split.name, tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((pat, input) => mkList(regex.split(exString(pat))(exString(input)).map(mkString)))),
    )

  // ===== Strings primitives =====

  private def stringsPrimitives(): Map[String, Primitive] =
    Map(
      hydra.lib.strings.cat.name -> mkPrimImpl(hydra.lib.strings.cat.name, tMono(tFun(tList(tString), tString)),
        impl1(ss => mkString(strings.cat(exList(ss).map(exString))))),
      hydra.lib.strings.cat2.name -> mkPrimImpl(hydra.lib.strings.cat2.name, tMono(tFun(tString, tFun(tString, tString))),
        impl2((a, b) => mkString(strings.cat2(exString(a))(exString(b))))),
      hydra.lib.strings.fromList.name -> mkPrimImpl(hydra.lib.strings.fromList.name, tMono(tFun(tList(tInt32), tString)),
        impl1(cs => mkString(strings.fromList(exList(cs).map(exInt32))))),
      hydra.lib.strings.intercalate.name -> mkPrimImpl(hydra.lib.strings.intercalate.name, tMono(tFun(tString, tFun(tList(tString), tString))),
        impl2((sep, ss) => mkString(strings.intercalate(exString(sep))(exList(ss).map(exString))))),
      hydra.lib.strings.length.name -> mkPrimImpl(hydra.lib.strings.length.name, tMono(tFun(tString, tInt32)),
        impl1(s => mkInt32(strings.length(exString(s))))),
      hydra.lib.strings.lines.name -> mkPrimImpl(hydra.lib.strings.lines.name, tMono(tFun(tString, tList(tString))),
        impl1(s => mkList(strings.lines(exString(s)).map(mkString)))),
      hydra.lib.strings.maybeCharAt.name -> mkPrimImpl(hydra.lib.strings.maybeCharAt.name, tMono(tFun(tInt32, tFun(tString, tOpt(tInt32)))),
        impl2((i, s) => mkMaybe(strings.maybeCharAt(exInt32(i))(exString(s)).map(mkInt32)))),
      hydra.lib.strings.`null`.name -> mkPrimImpl(hydra.lib.strings.`null`.name, tMono(tFun(tString, tBool)),
        impl1(s => mkBool(strings.`null`(exString(s))))),
      hydra.lib.strings.splitOn.name -> mkPrimImpl(hydra.lib.strings.splitOn.name, tMono(tFun(tString, tFun(tString, tList(tString)))),
        impl2((sep, s) => mkList(strings.splitOn(exString(sep))(exString(s)).map(mkString)))),
      hydra.lib.strings.toList.name -> mkPrimImpl(hydra.lib.strings.toList.name, tMono(tFun(tString, tList(tInt32))),
        impl1(s => mkList(strings.toList(exString(s)).map(mkInt32)))),
      hydra.lib.strings.toLower.name -> mkPrimImpl(hydra.lib.strings.toLower.name, tMono(tFun(tString, tString)),
        impl1(s => mkString(strings.toLower(exString(s))))),
      hydra.lib.strings.toUpper.name -> mkPrimImpl(hydra.lib.strings.toUpper.name, tMono(tFun(tString, tString)),
        impl1(s => mkString(strings.toUpper(exString(s))))),
      hydra.lib.strings.unlines.name -> mkPrimImpl(hydra.lib.strings.unlines.name, tMono(tFun(tList(tString), tString)),
        impl1(ss => mkString(strings.unlines(exList(ss).map(exString))))),
    )

  // ===== Literals primitives =====

  private def literalsPrimitives(): Map[String, Primitive] =
    Map(
      // Conversion primitives
      hydra.lib.literals.bigintToDecimal.name -> mkPrimImpl(hydra.lib.literals.bigintToDecimal.name, tMono(tFun(tBigint, tDecimal)),
        impl1(a => mkDecimal(literals.bigintToDecimal(exBigint(a))))),
      hydra.lib.literals.bigintToInt8.name -> mkPrimImpl(hydra.lib.literals.bigintToInt8.name, tMono(tFun(tBigint, tInt8)),
        impl1(a => mkInt8(literals.bigintToInt8(exBigint(a))))),
      hydra.lib.literals.bigintToInt16.name -> mkPrimImpl(hydra.lib.literals.bigintToInt16.name, tMono(tFun(tBigint, tInt16)),
        impl1(a => mkInt16(literals.bigintToInt16(exBigint(a))))),
      hydra.lib.literals.bigintToInt32.name -> mkPrimImpl(hydra.lib.literals.bigintToInt32.name, tMono(tFun(tBigint, tInt32)),
        impl1(a => mkInt32(literals.bigintToInt32(exBigint(a))))),
      hydra.lib.literals.bigintToInt64.name -> mkPrimImpl(hydra.lib.literals.bigintToInt64.name, tMono(tFun(tBigint, tInt64)),
        impl1(a => mkInt64(literals.bigintToInt64(exBigint(a))))),
      hydra.lib.literals.bigintToUint8.name -> mkPrimImpl(hydra.lib.literals.bigintToUint8.name, tMono(tFun(tBigint, tUint8)),
        impl1(a => mkUint8(literals.bigintToUint8(exBigint(a))))),
      hydra.lib.literals.bigintToUint16.name -> mkPrimImpl(hydra.lib.literals.bigintToUint16.name, tMono(tFun(tBigint, tUint16)),
        impl1(a => mkUint16(literals.bigintToUint16(exBigint(a))))),
      hydra.lib.literals.bigintToUint32.name -> mkPrimImpl(hydra.lib.literals.bigintToUint32.name, tMono(tFun(tBigint, tUint32)),
        impl1(a => mkUint32(literals.bigintToUint32(exBigint(a))))),
      hydra.lib.literals.bigintToUint64.name -> mkPrimImpl(hydra.lib.literals.bigintToUint64.name, tMono(tFun(tBigint, tUint64)),
        impl1(a => mkUint64(literals.bigintToUint64(exBigint(a))))),
      hydra.lib.literals.binaryToBytes.name -> mkPrimImpl(hydra.lib.literals.binaryToBytes.name, tMono(tFun(tBinary, tList(tInt32))),
        impl1(a => mkList(literals.binaryToBytes(exBinary(a)).map(mkInt32)))),
      hydra.lib.literals.binaryToString.name -> mkPrimImpl(hydra.lib.literals.binaryToString.name, tMono(tFun(tBinary, tString)),
        impl1(a => mkString(literals.binaryToString(exBinary(a))))),
      hydra.lib.literals.decimalToBigint.name -> mkPrimImpl(hydra.lib.literals.decimalToBigint.name, tMono(tFun(tDecimal, tBigint)),
        impl1(a => mkBigint(literals.decimalToBigint(exDecimal(a))))),
      hydra.lib.literals.decimalToFloat32.name -> mkPrimImpl(hydra.lib.literals.decimalToFloat32.name, tMono(tFun(tDecimal, tFloat32)),
        impl1(a => mkFloat32(literals.decimalToFloat32(exDecimal(a))))),
      hydra.lib.literals.decimalToFloat64.name -> mkPrimImpl(hydra.lib.literals.decimalToFloat64.name, tMono(tFun(tDecimal, tFloat64)),
        impl1(a => mkFloat64(literals.decimalToFloat64(exDecimal(a))))),
      hydra.lib.literals.float32ToDecimal.name -> mkPrimImpl(hydra.lib.literals.float32ToDecimal.name, tMono(tFun(tFloat32, tDecimal)),
        impl1(a => mkDecimal(literals.float32ToDecimal(exFloat32(a))))),
      hydra.lib.literals.float32ToFloat64.name -> mkPrimImpl(hydra.lib.literals.float32ToFloat64.name, tMono(tFun(tFloat32, tFloat64)),
        impl1(a => mkFloat64(literals.float32ToFloat64(exFloat32(a))))),
      hydra.lib.literals.float64ToDecimal.name -> mkPrimImpl(hydra.lib.literals.float64ToDecimal.name, tMono(tFun(tFloat64, tDecimal)),
        impl1(a => mkDecimal(literals.float64ToDecimal(exFloat64(a))))),
      hydra.lib.literals.float64ToFloat32.name -> mkPrimImpl(hydra.lib.literals.float64ToFloat32.name, tMono(tFun(tFloat64, tFloat32)),
        impl1(a => mkFloat32(literals.float64ToFloat32(exFloat64(a))))),
      hydra.lib.literals.int8ToBigint.name -> mkPrimImpl(hydra.lib.literals.int8ToBigint.name, tMono(tFun(tInt8, tBigint)),
        impl1(a => mkBigint(literals.int8ToBigint(exInt8(a))))),
      hydra.lib.literals.int16ToBigint.name -> mkPrimImpl(hydra.lib.literals.int16ToBigint.name, tMono(tFun(tInt16, tBigint)),
        impl1(a => mkBigint(literals.int16ToBigint(exInt16(a))))),
      hydra.lib.literals.int32ToBigint.name -> mkPrimImpl(hydra.lib.literals.int32ToBigint.name, tMono(tFun(tInt32, tBigint)),
        impl1(a => mkBigint(literals.int32ToBigint(exInt32(a))))),
      hydra.lib.literals.int64ToBigint.name -> mkPrimImpl(hydra.lib.literals.int64ToBigint.name, tMono(tFun(tInt64, tBigint)),
        impl1(a => mkBigint(literals.int64ToBigint(exInt64(a))))),
      // Read primitives
      hydra.lib.literals.readBigint.name -> mkPrimImpl(hydra.lib.literals.readBigint.name, tMono(tFun(tString, tOpt(tBigint))),
        impl1(s => mkMaybe(literals.readBigint(exString(s)).map(mkBigint)))),
      hydra.lib.literals.readBoolean.name -> mkPrimImpl(hydra.lib.literals.readBoolean.name, tMono(tFun(tString, tOpt(tBool))),
        impl1(s => mkMaybe(literals.readBoolean(exString(s)).map(mkBool)))),
      hydra.lib.literals.readDecimal.name -> mkPrimImpl(hydra.lib.literals.readDecimal.name, tMono(tFun(tString, tOpt(tDecimal))),
        impl1(s => mkMaybe(literals.readDecimal(exString(s)).map(mkDecimal)))),
      hydra.lib.literals.readFloat32.name -> mkPrimImpl(hydra.lib.literals.readFloat32.name, tMono(tFun(tString, tOpt(tFloat32))),
        impl1(s => mkMaybe(literals.readFloat32(exString(s)).map(mkFloat32)))),
      hydra.lib.literals.readFloat64.name -> mkPrimImpl(hydra.lib.literals.readFloat64.name, tMono(tFun(tString, tOpt(tFloat64))),
        impl1(s => mkMaybe(literals.readFloat64(exString(s)).map(mkFloat64)))),
      hydra.lib.literals.readInt8.name -> mkPrimImpl(hydra.lib.literals.readInt8.name, tMono(tFun(tString, tOpt(tInt8))),
        impl1(s => mkMaybe(literals.readInt8(exString(s)).map(mkInt8)))),
      hydra.lib.literals.readInt16.name -> mkPrimImpl(hydra.lib.literals.readInt16.name, tMono(tFun(tString, tOpt(tInt16))),
        impl1(s => mkMaybe(literals.readInt16(exString(s)).map(mkInt16)))),
      hydra.lib.literals.readInt32.name -> mkPrimImpl(hydra.lib.literals.readInt32.name, tMono(tFun(tString, tOpt(tInt32))),
        impl1(s => mkMaybe(literals.readInt32(exString(s)).map(mkInt32)))),
      hydra.lib.literals.readInt64.name -> mkPrimImpl(hydra.lib.literals.readInt64.name, tMono(tFun(tString, tOpt(tInt64))),
        impl1(s => mkMaybe(literals.readInt64(exString(s)).map(mkInt64)))),
      hydra.lib.literals.readString.name -> mkPrimImpl(hydra.lib.literals.readString.name, tMono(tFun(tString, tOpt(tString))),
        impl1(s => mkMaybe(literals.readString(exString(s)).map(mkString)))),
      hydra.lib.literals.readUint8.name -> mkPrimImpl(hydra.lib.literals.readUint8.name, tMono(tFun(tString, tOpt(tUint8))),
        impl1(s => mkMaybe(literals.readUint8(exString(s)).map(mkUint8)))),
      hydra.lib.literals.readUint16.name -> mkPrimImpl(hydra.lib.literals.readUint16.name, tMono(tFun(tString, tOpt(tUint16))),
        impl1(s => mkMaybe(literals.readUint16(exString(s)).map(mkUint16)))),
      hydra.lib.literals.readUint32.name -> mkPrimImpl(hydra.lib.literals.readUint32.name, tMono(tFun(tString, tOpt(tUint32))),
        impl1(s => mkMaybe(literals.readUint32(exString(s)).map(mkUint32)))),
      hydra.lib.literals.readUint64.name -> mkPrimImpl(hydra.lib.literals.readUint64.name, tMono(tFun(tString, tOpt(tUint64))),
        impl1(s => mkMaybe(literals.readUint64(exString(s)).map(mkUint64)))),
      // Show primitives
      hydra.lib.literals.showBigint.name -> mkPrimImpl(hydra.lib.literals.showBigint.name, tMono(tFun(tBigint, tString)),
        impl1(a => mkString(literals.showBigint(exBigint(a))))),
      hydra.lib.literals.showBoolean.name -> mkPrimImpl(hydra.lib.literals.showBoolean.name, tMono(tFun(tBool, tString)),
        impl1(a => mkString(literals.showBoolean(exBool(a))))),
      hydra.lib.literals.showDecimal.name -> mkPrimImpl(hydra.lib.literals.showDecimal.name, tMono(tFun(tDecimal, tString)),
        impl1(a => mkString(literals.showDecimal(exDecimal(a))))),
      hydra.lib.literals.showFloat32.name -> mkPrimImpl(hydra.lib.literals.showFloat32.name, tMono(tFun(tFloat32, tString)),
        impl1(a => mkString(literals.showFloat32(exFloat32(a))))),
      hydra.lib.literals.showFloat64.name -> mkPrimImpl(hydra.lib.literals.showFloat64.name, tMono(tFun(tFloat64, tString)),
        impl1(a => mkString(literals.showFloat64(exFloat64(a))))),
      hydra.lib.literals.showInt8.name -> mkPrimImpl(hydra.lib.literals.showInt8.name, tMono(tFun(tInt8, tString)),
        impl1(a => mkString(literals.showInt8(exInt8(a))))),
      hydra.lib.literals.showInt16.name -> mkPrimImpl(hydra.lib.literals.showInt16.name, tMono(tFun(tInt16, tString)),
        impl1(a => mkString(literals.showInt16(exInt16(a))))),
      hydra.lib.literals.showInt32.name -> mkPrimImpl(hydra.lib.literals.showInt32.name, tMono(tFun(tInt32, tString)),
        impl1(a => mkString(literals.showInt32(exInt32(a))))),
      hydra.lib.literals.showInt64.name -> mkPrimImpl(hydra.lib.literals.showInt64.name, tMono(tFun(tInt64, tString)),
        impl1(a => mkString(literals.showInt64(exInt64(a))))),
      hydra.lib.literals.showUint8.name -> mkPrimImpl(hydra.lib.literals.showUint8.name, tMono(tFun(tUint8, tString)),
        impl1(a => mkString(literals.showUint8(exUint8(a))))),
      hydra.lib.literals.showUint16.name -> mkPrimImpl(hydra.lib.literals.showUint16.name, tMono(tFun(tUint16, tString)),
        impl1(a => mkString(literals.showUint16(exUint16(a))))),
      hydra.lib.literals.showUint32.name -> mkPrimImpl(hydra.lib.literals.showUint32.name, tMono(tFun(tUint32, tString)),
        impl1(a => mkString(literals.showUint32(exUint32(a))))),
      hydra.lib.literals.showUint64.name -> mkPrimImpl(hydra.lib.literals.showUint64.name, tMono(tFun(tUint64, tString)),
        impl1(a => mkString(literals.showUint64(exUint64(a))))),
      hydra.lib.literals.showString.name -> mkPrimImpl(hydra.lib.literals.showString.name, tMono(tFun(tString, tString)),
        impl1(a => mkString(literals.showString(exString(a))))),
      hydra.lib.literals.stringToBinary.name -> mkPrimImpl(hydra.lib.literals.stringToBinary.name, tMono(tFun(tString, tBinary)),
        impl1(a => mkBinary(literals.stringToBinary(exString(a))))),
      hydra.lib.literals.uint8ToBigint.name -> mkPrimImpl(hydra.lib.literals.uint8ToBigint.name, tMono(tFun(tUint8, tBigint)),
        impl1(a => mkBigint(literals.uint8ToBigint(exUint8(a))))),
      hydra.lib.literals.uint16ToBigint.name -> mkPrimImpl(hydra.lib.literals.uint16ToBigint.name, tMono(tFun(tUint16, tBigint)),
        impl1(a => mkBigint(literals.uint16ToBigint(exUint16(a))))),
      hydra.lib.literals.uint32ToBigint.name -> mkPrimImpl(hydra.lib.literals.uint32ToBigint.name, tMono(tFun(tUint32, tBigint)),
        impl1(a => mkBigint(literals.uint32ToBigint(exUint32(a))))),
      hydra.lib.literals.uint64ToBigint.name -> mkPrimImpl(hydra.lib.literals.uint64ToBigint.name, tMono(tFun(tUint64, tBigint)),
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
      hydra.lib.pairs.bimap.name -> mkPrimImpl(hydra.lib.pairs.bimap.name, tScheme(Seq("a", "b", "c", "d"),
        tFun(tFun(a, c), tFun(tFun(b, d), tFun(tPair(a, b), tPair(c, d))))),
        impl3 { (f, g, p) =>
          val (a, b) = exPair(p)
          mkPairTerm(app(f, a), app(g, b))
        }),
      // First-order
      hydra.lib.pairs.first.name -> mkPrimImpl(hydra.lib.pairs.first.name, tScheme(Seq("a", "b"),
        tFun(tPair(a, b), a)),
        impl1(p => exPair(p)._1)),
      hydra.lib.pairs.second.name -> mkPrimImpl(hydra.lib.pairs.second.name, tScheme(Seq("a", "b"),
        tFun(tPair(a, b), b)),
        impl1(p => exPair(p)._2)),
    )

  // ===== Effects primitives (#494) =====
  //
  // effect<t> is transparent in Scala. These are registered so the inference graph can
  // resolve the hydra.lib.effects.* names; their type schemes match the kernel signatures
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
      hydra.lib.effects.apply.name -> mkPrim(hydra.lib.effects.apply.name, tScheme(Seq("x", "y"),
        tFun(tEffect(tFun(x, y)), tFun(tEffect(x), tEffect(y))))),
      // bind: effect<x> -> (x -> effect<y>) -> effect<y>
      hydra.lib.effects.bind.name -> mkPrim(hydra.lib.effects.bind.name, tScheme(Seq("x", "y"),
        tFun(tEffect(x), tFun(tFun(x, tEffect(y)), tEffect(y))))),
      // compose: (x -> effect<y>) -> (y -> effect<z>) -> x -> effect<z>
      hydra.lib.effects.compose.name -> mkPrim(hydra.lib.effects.compose.name, tScheme(Seq("x", "y", "z"),
        tFun(tFun(x, tEffect(y)), tFun(tFun(y, tEffect(z)), tFun(x, tEffect(z)))))),
      // foldl: (x -> y -> effect<x>) -> x -> list<y> -> effect<x>
      hydra.lib.effects.foldl.name -> mkPrim(hydra.lib.effects.foldl.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, tFun(y, tEffect(x))), tFun(x, tFun(tList(y), tEffect(x)))))),
      // map: (x -> y) -> effect<x> -> effect<y>
      hydra.lib.effects.map.name -> mkPrim(hydra.lib.effects.map.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, y), tFun(tEffect(x), tEffect(y))))),
      // mapList: (x -> effect<y>) -> list<x> -> effect<list<y>>
      hydra.lib.effects.mapList.name -> mkPrim(hydra.lib.effects.mapList.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, tEffect(y)), tFun(tList(x), tEffect(tList(y)))))),
      // mapOptional: (x -> effect<y>) -> optional<x> -> effect<optional<y>>
      hydra.lib.effects.mapOptional.name -> mkPrim(hydra.lib.effects.mapOptional.name, tScheme(Seq("x", "y"),
        tFun(tFun(x, tEffect(y)), tFun(tOpt(x), tEffect(tOpt(y)))))),
      // pure: x -> effect<x>
      hydra.lib.effects.pure.name -> mkPrim(hydra.lib.effects.pure.name, tScheme(Seq("x"),
        tFun(x, tEffect(x)))),
    )

  // ===== Files primitives (#494) =====
  //
  // FilePath and FileError are nominal kernel types (referenced by name). unit maps to
  // Scala Unit, binary to Array[Byte], either to scala.util.Either. As with effects, the
  // interpreter implementation is a deferred stub; real I/O happens in hydra.overlay.scala.lib.files.

  private def filesPrimitives(): Map[String, Primitive] =
    Map(
      // appendFile: FilePath -> binary -> effect<either<FileError, unit>>
      hydra.lib.files.appendFile.name -> mkPrim(hydra.lib.files.appendFile.name,
        tMono(tFun(tFilePath, tFun(tBinary, tEffect(tEither(tFileError, tUnit)))))),
      // copy: boolean -> FilePath -> FilePath -> effect<either<FileError, unit>>
      hydra.lib.files.copy.name -> mkPrim(hydra.lib.files.copy.name,
        tMono(tFun(tBool, tFun(tFilePath, tFun(tFilePath, tEffect(tEither(tFileError, tUnit))))))),
      // createDirectory: boolean -> FilePath -> effect<either<FileError, unit>>
      hydra.lib.files.createDirectory.name -> mkPrim(hydra.lib.files.createDirectory.name,
        tMono(tFun(tBool, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // exists: FilePath -> effect<either<FileError, boolean>>
      hydra.lib.files.exists.name -> mkPrim(hydra.lib.files.exists.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tBool))))),
      // listDirectory: FilePath -> effect<either<FileError, list<FilePath>>>
      hydra.lib.files.listDirectory.name -> mkPrim(hydra.lib.files.listDirectory.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tList(tFilePath)))))),
      // readFile: FilePath -> effect<either<FileError, binary>>
      hydra.lib.files.readFile.name -> mkPrim(hydra.lib.files.readFile.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tBinary))))),
      // removeDirectory: boolean -> FilePath -> effect<either<FileError, unit>>
      hydra.lib.files.removeDirectory.name -> mkPrim(hydra.lib.files.removeDirectory.name,
        tMono(tFun(tBool, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // removeFile: FilePath -> effect<either<FileError, unit>>
      hydra.lib.files.removeFile.name -> mkPrim(hydra.lib.files.removeFile.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tUnit))))),
      // rename: FilePath -> FilePath -> effect<either<FileError, unit>>
      hydra.lib.files.rename.name -> mkPrim(hydra.lib.files.rename.name,
        tMono(tFun(tFilePath, tFun(tFilePath, tEffect(tEither(tFileError, tUnit)))))),
      // status: FilePath -> effect<either<FileError, FileStatus>>
      hydra.lib.files.status.name -> mkPrim(hydra.lib.files.status.name,
        tMono(tFun(tFilePath, tEffect(tEither(tFileError, tFileStatus))))),
      // writeFile: FilePath -> binary -> effect<either<FileError, unit>>
      hydra.lib.files.writeFile.name -> mkPrim(hydra.lib.files.writeFile.name,
        tMono(tFun(tFilePath, tFun(tBinary, tEffect(tEither(tFileError, tUnit)))))),
    )

  // ===== Hashing primitives (#524) =====

  private def hashingPrimitives(): Map[String, Primitive] =
    Map(
      // sha256: binary -> binary
      hydra.lib.hashing.sha256.name -> mkPrim(hydra.lib.hashing.sha256.name,
        tMono(tFun(tBinary, tBinary))),
      // sha256Hex: binary -> string
      hydra.lib.hashing.sha256Hex.name -> mkPrim(hydra.lib.hashing.sha256Hex.name,
        tMono(tFun(tBinary, tString))),
    )

  // ===== System primitives (#498) =====

  private def systemPrimitives(): Map[String, Primitive] =
    Map(
      // execute: Command -> effect<either<SystemError, ProcessResult>>
      hydra.lib.system.execute.name -> mkPrim(hydra.lib.system.execute.name,
        tMono(tFun(tCommand, tEffect(tEither(tSystemError, tProcessResult))))),
      // exit: StatusCode -> effect<unit>
      hydra.lib.system.exit.name -> mkPrim(hydra.lib.system.exit.name,
        tMono(tFun(tStatusCode, tEffect(tUnit)))),
      // getEnvironment: effect<map<EnvironmentVariable, string>>
      hydra.lib.system.getEnvironment.name -> mkPrim(hydra.lib.system.getEnvironment.name,
        tMono(tEffect(tMap(tEnvironmentVariable, tString)))),
      // getEnvironmentVariable: EnvironmentVariable -> effect<optional<string>>
      hydra.lib.system.getEnvironmentVariable.name -> mkPrim(hydra.lib.system.getEnvironmentVariable.name,
        tMono(tFun(tEnvironmentVariable, tEffect(tOpt(tString))))),
      // getTime: effect<Timespec>
      hydra.lib.system.getTime.name -> mkPrim(hydra.lib.system.getTime.name,
        tMono(tEffect(tTimespec))),
      // getWorkingDirectory: effect<either<SystemError, FilePath>>
      hydra.lib.system.getWorkingDirectory.name -> mkPrim(hydra.lib.system.getWorkingDirectory.name,
        tMono(tEffect(tEither(tSystemError, tFilePath)))),
    )


  // ===== Text primitives (#494) =====

  private def textPrimitives(): Map[String, Primitive] =
    Map(
      // decodeUtf8: binary -> either<string, string>
      hydra.lib.text.decodeUtf8.name -> mkPrim(hydra.lib.text.decodeUtf8.name,
        tMono(tFun(tBinary, tEither(tString, tString)))),
      // encodeUtf8: string -> binary
      hydra.lib.text.encodeUtf8.name -> mkPrim(hydra.lib.text.encodeUtf8.name,
        tMono(tFun(tString, tBinary))),
    )

  /** All standard primitives. */
  def standardPrimitives(): Map[String, Primitive] =
    charsPrimitives() ++
    effectsPrimitives() ++
    equalityPrimitives() ++
    eithersPrimitives() ++
    filesPrimitives() ++
    hashingPrimitives() ++
    listsPrimitives() ++
    literalsPrimitives() ++
    logicPrimitives() ++
    mapsPrimitives() ++
    mathPrimitives() ++
    optionalsPrimitives() ++
    pairsPrimitives() ++
    regexPrimitives() ++
    setsPrimitives() ++
    stringsPrimitives() ++
    systemPrimitives() ++
    textPrimitives()

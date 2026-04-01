package hydra

import hydra.core.*
import hydra.graph.{Graph, Primitive}
import hydra.testing.*
import hydra.lib.Libraries
import org.scalatest.funsuite.AnyFunSuite

/**
 * Scala executor for Hydra's language-agnostic test suite.
 *
 * This runs the kernel tests (evaluation, inference, type checking, etc.)
 * using the Scala implementations of the kernel functions.
 *
 * in src/gen-test/scala/generation/ and are auto-discovered by ScalaTest.
 */
class TestSuiteRunner extends AnyFunSuite {

  private lazy val graph: Graph = TestSuiteRunner.buildTestGraph()
  private def emptyCtx: hydra.context.Context =
    hydra.context.Context(Seq.empty, Seq.empty, Map.empty)

  // Load all tests from the generated test suite
  private val allTests: TestGroup = hydra.test.testSuite.allTests

  // Register all test cases
  registerTests(allTests, allTests.name)

  private def registerTests(group: TestGroup, path: String): Unit = {
    for (tc <- group.cases) {
      val name = tc.name + tc.description.map(d => ": " + d).getOrElse("")
      val fullPath = path + "/" + name
      if (!shouldSkip(tc)) registerTestCase(fullPath, tc)
    }
    for (subgroup <- group.subgroups)
      registerTests(subgroup, path + "/" + subgroup.name)
  }

  private def shouldSkip(tc: TestCaseWithMetadata): Boolean =
    tc.tags.contains("disabled") || tc.tags.contains("disabledForPython")

  private def registerTestCase(name: String, tc: TestCaseWithMetadata): Unit = {
    tc.`case` match {
      case TestCase.evaluation(ec) =>
        test(name) {
          val reduced = hydra.reduction.reduceTerm(emptyCtx)(graph)(true)(ec.input)
          reduced match {
            case Right(result) =>
              if (!termsEqual(ec.output, result))
                assert(hydra.show.core.term(ec.output) == hydra.show.core.term(result),
                  "Original term does not reduce to expected term")
            case Left(err) =>
              fail(s"Evaluation failed: ${hydra.show.errors.error(err.`object`)}")
          }
        }

      case TestCase.delegatedEvaluation(_) =>
        test(name) { /* pass - handled by generation tests */ }

      case TestCase.inference(ic) =>
        test(name) {
          val result = hydra.inference.inferTypeOf(emptyCtx)(graph)(ic.input)
          result match {
            case Right(pair) =>
              val (termAndScheme, _) = pair
              val (_, resultScheme) = termAndScheme
              assert(hydra.show.core.typeScheme(ic.output) == hydra.show.core.typeScheme(resultScheme),
                "Type scheme mismatch")
            case Left(err) =>
              fail(s"Inference failed: ${hydra.show.errors.error(err.`object`)}")
          }
        }

      case TestCase.inferenceFailure(ifc) =>
        test(name) {
          val result = hydra.inference.inferTypeOf(emptyCtx)(graph)(ifc.input)
          assert(result.isLeft, "Expected inference failure but got success")
        }

      case TestCase.typeChecking(tcc) =>
        test(name) {
          val inferResult = hydra.inference.inferTypeOf(emptyCtx)(graph)(tcc.input)
          inferResult match {
            case Right(pair) =>
              val (termAndScheme, inferCx) = pair
              val (inferredTerm, inferredScheme) = termAndScheme
              val inferredType = typeSchemeToType(inferredScheme)
              val typeOfResult = hydra.checking.typeOf(inferCx)(graph)(Seq.empty)(inferredTerm)
              typeOfResult match {
                case Right(reconPair) =>
                  val (reconstructedType, _) = reconPair
                  assertAlphaEquivalentTerm(tcc.outputTerm, inferredTerm, "Inferred term")
                  assertAlphaEquivalentType(tcc.outputType, inferredType, "Inferred type")
                  assertAlphaEquivalentType(tcc.outputType, reconstructedType, "Reconstructed type")
                case Left(err) =>
                  fail(s"Type reconstruction failed: ${hydra.show.errors.error(err.`object`)}")
              }
            case Left(err) =>
              fail(s"Inference failed: ${hydra.show.errors.error(err.`object`)}")
          }
        }

      case TestCase.typeReduction(trc) =>
        test(name) {
          val result = hydra.reduction.betaReduceType(emptyCtx)(graph)(trc.input)
          result match {
            case Right(reduced) => assert(trc.output == reduced)
            case Left(err) => fail(s"Type reduction failed: ${hydra.show.errors.error(err.`object`)}")
          }
        }

      case TestCase.alphaConversion(ac) =>
        test(name) { assert(ac.result == hydra.reduction.alphaConvert(ac.oldVariable)(ac.newVariable)(ac.term)) }

      case TestCase.caseConversion(cc) =>
        test(name) { assert(cc.`toString_` == hydra.formatting.convertCase(cc.fromConvention)(cc.toConvention)(cc.fromString)) }

      case TestCase.deannotateTerm(dt) =>
        test(name) { assert(dt.output == hydra.rewriting.deannotateTerm(dt.input)) }

      case TestCase.deannotateType(dt) =>
        test(name) { assert(dt.output == hydra.rewriting.deannotateType(dt.input)) }

      case TestCase.serialization(sc) =>
        test(name) { assert(sc.output == hydra.serialization.printExpr(hydra.serialization.parenthesize(sc.input))) }

      case TestCase.topologicalSort(ts) =>
        test(name) { assert(ts.expected == hydra.sorting.topologicalSort(ts.adjacencyList)) }

      case TestCase.topologicalSortSCC(ts) =>
        test(name) { assert(ts.expected == hydra.sorting.topologicalSortComponents(ts.adjacencyList)) }

      case TestCase.flattenLetTerms(fl) =>
        test(name) { assert(fl.output == hydra.rewriting.flattenLetTerms(fl.input)) }

      case TestCase.freeVariables(fv) =>
        test(name) { assert(fv.output == hydra.rewriting.freeVariablesInTerm(fv.input)) }

      case TestCase.liftLambdaAboveLet(ll) =>
        test(name) { assert(ll.output == hydra.rewriting.liftLambdaAboveLet(ll.input)) }

      case TestCase.simplifyTerm(st) =>
        test(name) { assert(st.output == hydra.rewriting.simplifyTerm(st.input)) }

      case TestCase.normalizeTypeVariables(nt) =>
        test(name) { assert(nt.output == hydra.rewriting.normalizeTypeVariablesInTerm(nt.input)) }

      case TestCase.etaExpansion(ee) =>
        test(name) {
          val result = hydra.reduction.etaExpandTypedTerm(emptyCtx)(graph)(ee.input)
          result match {
            case Right(expanded) => assert(ee.output == expanded)
            case Left(err) => fail(s"Eta expansion failed: ${hydra.show.errors.error(err.`object`)}")
          }
        }

      case TestCase.hoistSubterms(hs) =>
        test(name) {
          assert(hs.output == hydra.hoisting.hoistSubterms(predicateFn(hs.predicate))(TestSuiteRunner.emptyGraph())(hs.input))
        }

      case TestCase.hoistCaseStatements(hcs) =>
        test(name) {
          assert(hcs.output == hydra.hoisting.hoistCaseStatements(TestSuiteRunner.emptyGraph())(hcs.input))
        }

      case TestCase.foldOverTerm(fot) =>
        test(name) { assert(fot.output == runFoldOperation(fot.traversalOrder, fot.operation, fot.input)) }

      case TestCase.rewriteTerm(rt) =>
        test(name) { assert(rt.output == runTermRewriter(rt.rewriter, rt.input)) }

      case TestCase.rewriteType(rt) =>
        test(name) { assert(rt.output == runTypeRewriter(rt.rewriter, rt.input)) }

      case TestCase.universal(uc) =>
        test(name) { assert(uc.expected == uc.actual) }

      case _ =>
        test(name) { cancel("Unhandled test case type") }
    }
  }

  private def termsEqual(expected: Term, actual: Term): Boolean = {
    if (expected == actual) return true
    (expected, actual) match {
      case (Term.literal(Literal.float(FloatValue.float64(e))), Term.literal(Literal.float(FloatValue.float64(a)))) =>
        scala.math.abs(e - a) <= 2 * scala.math.ulp(e)
      case (Term.literal(Literal.float(FloatValue.float32(e))), Term.literal(Literal.float(FloatValue.float32(a)))) =>
        scala.math.abs(e - a) <= 2 * scala.math.ulp(e)
      case (Term.literal(Literal.float(FloatValue.bigfloat(e))), Term.literal(Literal.float(FloatValue.bigfloat(a)))) =>
        e.compare(a) == 0
      case _ => false
    }
  }

  private def typeSchemeToType(ts: TypeScheme): Type =
    ts.variables.foldRight(ts.`type`) { (v, t) => Type.forall(ForallType(v, t)) }

  private def assertAlphaEquivalentTerm(expected: Term, actual: Term, label: String): Unit =
    assert(normalizeTypeVarNames(hydra.show.core.term(expected)) ==
      normalizeTypeVarNames(hydra.show.core.term(actual)), s"$label mismatch")

  private def assertAlphaEquivalentType(expected: Type, actual: Type, label: String): Unit =
    assert(normalizeTypeVarNames(hydra.show.core.`type`(expected)) ==
      normalizeTypeVarNames(hydra.show.core.`type`(actual)), s"$label mismatch")

  private def normalizeTypeVarNames(s: String): String = {
    var binders = List.empty[String]
    var remaining = s
    while (remaining.startsWith("\u039b")) {
      val dotIdx = remaining.indexOf('.')
      if (dotIdx < 0) return s
      binders = binders :+ remaining.substring(1, dotIdx)
      remaining = remaining.substring(dotIdx + 1)
    }
    if (binders.isEmpty) return s
    val binderSet = binders.toSet
    val varPattern = "\\b(t\\d+)\\b".r
    val ordered = scala.collection.mutable.ArrayBuffer.empty[String]
    for (m <- varPattern.findAllMatchIn(remaining))
      if (binderSet.contains(m.group(1)) && !ordered.contains(m.group(1))) ordered += m.group(1)
    for (b <- binders if !ordered.contains(b)) ordered += b
    val renaming = ordered.zipWithIndex.map { case (old, i) => old -> s"tv$i" }.toMap
    var normalized = remaining
    for ((old, tmp) <- renaming)
      normalized = normalized.replaceAll(s"\\b${java.util.regex.Pattern.quote(old)}\\b", tmp)
    for (i <- ordered.indices)
      normalized = normalized.replaceAll(s"\\btv$i\\b", s"t$i")
    ordered.indices.map(i => s"\u039bt$i.").mkString + normalized
  }

  private def predicateFn(pred: HoistPredicate): ((Seq[hydra.paths.SubtermStep], Term)) => Boolean = pred match {
    case HoistPredicate.caseStatements => { case (_, Term.function(Function.elimination(Elimination.union(_)))) => true; case _ => false }
    case HoistPredicate.applications => { case (_, Term.application(_)) => true; case _ => false }
    case HoistPredicate.lists => { case (_, Term.list(_)) => true; case _ => false }
    case HoistPredicate.nothing => _ => false
  }

  private def runFoldOperation(order: hydra.coders.TraversalOrder, op: FoldOperation, input: Term): Term = {
    op match {
      case FoldOperation.sumInt32Literals =>
        val fold: Term => Term => Term = acc => t => (acc, t) match {
          case (Term.literal(Literal.integer(IntegerValue.int32(a))), Term.literal(Literal.integer(IntegerValue.int32(b)))) =>
            Term.literal(Literal.integer(IntegerValue.int32(a + b)))
          case _ => acc
        }
        hydra.rewriting.foldOverTerm(order)(fold)(Term.literal(Literal.integer(IntegerValue.int32(0))))(input)
      case FoldOperation.collectListLengths =>
        val fold: Term => Term => Term = acc => t => t match {
          case Term.list(items) => acc match {
            case Term.list(existing) => Term.list(existing :+ Term.literal(Literal.integer(IntegerValue.int32(items.length))))
            case _ => acc
          }
          case _ => acc
        }
        hydra.rewriting.foldOverTerm(order)(fold)(Term.list(Seq.empty))(input)
      case FoldOperation.collectLabels =>
        val fold: Term => Term => Term = acc => t => t match {
          case Term.pair((Term.literal(Literal.string(_)), _)) =>
            val label = t match { case Term.pair((first, _)) => first; case _ => t }
            acc match {
              case Term.list(existing) => Term.list(existing :+ label)
              case _ => acc
            }
          case _ => acc
        }
        hydra.rewriting.foldOverTerm(order)(fold)(Term.list(Seq.empty))(input)
    }
  }

  private def runTermRewriter(rewriter: TermRewriter, input: Term): Term = rewriter match {
    case TermRewriter.replaceFooWithBar =>
      hydra.rewriting.rewriteTerm((recurse: Term => Term) => (t: Term) => t match {
        case Term.literal(Literal.string("foo")) => Term.literal(Literal.string("bar"))
        case other => recurse(other)
      })(input)
    case TermRewriter.replaceInt32WithInt64 =>
      hydra.rewriting.rewriteTerm((recurse: Term => Term) => (t: Term) => t match {
        case Term.literal(Literal.integer(IntegerValue.int32(n))) =>
          Term.literal(Literal.integer(IntegerValue.int64(n.toLong)))
        case other => recurse(other)
      })(input)
  }

  private def runTypeRewriter(rewriter: TypeRewriter, input: Type): Type = rewriter match {
    case TypeRewriter.replaceStringWithInt32 =>
      hydra.rewriting.rewriteType((recurse: Type => Type) => (t: Type) => t match {
        case Type.literal(LiteralType.string) => Type.literal(LiteralType.integer(IntegerType.int32))
        case other => recurse(other)
      })(input)
  }
}

object TestSuiteRunner {
  def emptyGraph(): Graph = {
    val primitives = Libraries.standardPrimitives()
    Graph(Map.empty, Map.empty, Map.empty, Set.empty, Map.empty, primitives, Map.empty, Set.empty)
  }

  def buildTestGraph(): Graph = {
    val primitives = Libraries.standardPrimitives()
    val testTypes = hydra.test.testGraph.testTypes
    val kernelTypes = buildKernelTypes()
    val allTypes = kernelTypes ++ testTypes // test types override kernel types if any overlap
    val schemaTypes = allTypes.map { case (name, typ) => name -> hydra.schemas.typeToTypeScheme(typ) }
    val boundTerms = scala.collection.mutable.Map.empty[String, Term]

    // Bridge primitives as term bindings
    val excludedNames = Set("hydra.annotations.setTermAnnotation", "hydra.annotations.setTermDescription", "hydra.rewriting.deannotateTerm")
    for ((name, _) <- primitives if !excludedNames.contains(name))
      boundTerms(name) = Term.function(Function.primitive(name))

    // Add kernel constants
    for ((k, v) <- Seq(
      "hydra.constants.key_classes" -> "classes", "hydra.constants.key_description" -> "description",
      "hydra.constants.key_type" -> "type", "hydra.constants.key_debugId" -> "debugId",
      "hydra.constants.key_firstClassType" -> "firstClassType"))
      boundTerms(k) = Term.wrap(WrappedTerm("hydra.core.Name", Term.literal(Literal.string(v))))

    boundTerms("hydra.lexical.emptyGraph") = Term.record(Record("hydra.graph.Graph", Seq(
      Field("boundTerms", Term.map(Map.empty)), Field("boundTypes", Term.map(Map.empty)),
      Field("classConstraints", Term.map(Map.empty)), Field("lambdaVariables", Term.set(Set.empty)),
      Field("metadata", Term.map(Map.empty)), Field("primitives", Term.map(Map.empty)),
      Field("schemaTypes", Term.map(Map.empty)), Field("typeVariables", Term.set(Set.empty)))))

    // Add annotation/rewriting term bindings (term-level implementations)
    addAnnotationBindings(boundTerms)

    // Add test term bindings
    for ((name, term) <- hydra.test.testGraph.testTerms) boundTerms(name) = term

    // Add type element terms
    for ((name, typ) <- testTypes) boundTerms(name) = hydra.encode.core.`type`(typ)

    Graph(boundTerms.toMap, Map.empty, Map.empty, Set.empty, Map.empty, primitives, schemaTypes, Set.empty)
  }

  // Term construction helpers for annotation bindings
  private def lam(v: String, body: Term): Term = Term.function(Function.lambda(Lambda(v, None, body)))
  private def v(name: String): Term = Term.variable(name)
  private def app(f: Term, x: Term): Term = Term.application(Application(f, x))
  private def prim(name: String): Term = Term.function(Function.primitive(name))
  private def proj(typeName: String, fieldName: String): Term = Term.function(Function.elimination(Elimination.record(Projection(typeName, fieldName))))
  private def cases(typeName: String, default: Option[Term], fields: Field*): Term = Term.function(Function.elimination(Elimination.union(CaseStatement(typeName, default, fields.toSeq))))
  private def inj(typeName: String, fieldName: String, value: Term): Term = Term.union(Injection(typeName, Field(fieldName, value)))
  private def rec(typeName: String, fields: Field*): Term = Term.record(Record(typeName, fields.toSeq))
  private def let1(name: String, value: Term, body: Term): Term = Term.let(Let(Seq(Binding(name, value, None)), body))

  /** Build kernel type definitions needed by inference/checking tests. */
  private def buildKernelTypes(): Map[String, Type] = {
    val typeName = "hydra.core.Type"
    val nameName = "hydra.core.Name"
    val contextName = "hydra.context.Context"
    val inContextName = "hydra.context.InContext"
    val errorName = "hydra.errors.Error"
    val otherErrorName = "hydra.errors.OtherError"
    val inContextError = Type.application(ApplicationType(Type.variable(inContextName), Type.variable(errorName)))
    def eitherInContextError(v: Type): Type = Type.either(EitherType(inContextError, v))
    Map(
      "hydra.coders.CoderDirection" -> Type.union(Seq(
        FieldType("encode", Type.unit), FieldType("decode", Type.unit))),
      "hydra.util.Coder" -> Type.forall(ForallType("v1", Type.forall(ForallType("v2",
        Type.record(Seq(
          FieldType("encode", Type.function(FunctionType(Type.variable(contextName),
            Type.function(FunctionType(Type.variable("v1"), eitherInContextError(Type.variable("v2"))))))),
          FieldType("decode", Type.function(FunctionType(Type.variable(contextName),
            Type.function(FunctionType(Type.variable("v2"), eitherInContextError(Type.variable("v1"))))))))))
        ))),
      contextName -> Type.record(Seq(
        FieldType("trace", Type.list(Type.literal(LiteralType.string))),
        FieldType("messages", Type.list(Type.literal(LiteralType.string))),
        FieldType("other", Type.map(MapType(Type.variable(nameName), Type.variable("hydra.core.Term")))))),
      inContextName -> Type.forall(ForallType("e", Type.record(Seq(
        FieldType("object", Type.variable("e")),
        FieldType("context", Type.variable(contextName)))))),
      otherErrorName -> Type.wrap(Type.literal(LiteralType.string)),
      errorName -> Type.union(Seq(FieldType("other", Type.variable(otherErrorName)))),
      typeName -> Type.union(Seq(
        FieldType("annotated", Type.variable("annotatedType")),
        FieldType("application", Type.variable("applicationElim")),
        FieldType("either", Type.variable("eitherType")),
        FieldType("forall", Type.variable("forallType")),
        FieldType("function", Type.variable("functionType")),
        FieldType("list", Type.variable(typeName)),
        FieldType("literal", Type.variable("literalType")),
        FieldType("map", Type.variable("mapType")),
        FieldType("maybe", Type.variable(typeName)),
        FieldType("pair", Type.variable("pairType")),
        FieldType("record", Type.variable("rowType")),
        FieldType("set", Type.variable(typeName)),
        FieldType("union", Type.variable("rowType")),
        FieldType("unit", Type.unit),
        FieldType("variable", Type.variable("name")),
        FieldType("wrap", Type.variable("wrappedType")))),
      nameName -> Type.wrap(Type.literal(LiteralType.string)),
      "hydra.core.ForallType" -> Type.record(Seq(
        FieldType("parameter", Type.variable(nameName)),
        FieldType("body", Type.variable(typeName)))),
      "hydra.util.Comparison" -> Type.union(Seq(
        FieldType("lessThan", Type.unit),
        FieldType("equalTo", Type.unit),
        FieldType("greaterThan", Type.unit))),
      "hydra.util.CaseConvention" -> Type.union(Seq(
        FieldType("camel", Type.unit), FieldType("pascal", Type.unit),
        FieldType("lowerSnake", Type.unit), FieldType("upperSnake", Type.unit))),
      "hydra.util.Precision" -> Type.union(Seq(
        FieldType("arbitrary", Type.unit),
        FieldType("bits", Type.literal(LiteralType.integer(IntegerType.int32)))))
    )
  }

  private def addAnnotationBindings(boundTerms: scala.collection.mutable.Map[String, Term]): Unit = {
    // hydra.rewriting.deannotateTerm = \t -> case t of { annotated(at) -> deannotateTerm(at.body); _ -> t }
    boundTerms("hydra.rewriting.deannotateTerm") =
      lam("t", app(
        cases("hydra.core.Term", Some(v("t")),
          Field("annotated", lam("at", app(v("hydra.rewriting.deannotateTerm"), app(proj("hydra.core.AnnotatedTerm", "body"), v("at")))))),
        v("t")))

    // hydra.annotations.termAnnotationInternal = \term ->
    //   let toPairs = \rest -> \t -> case t of { annotated(at) -> toPairs(cons(toList(at.annotation), rest))(at.body); _ -> rest }
    //   in fromList(concat(toPairs([])(term)))
    boundTerms("hydra.annotations.termAnnotationInternal") =
      lam("term", let1("toPairs",
        lam("rest", lam("t", app(
          cases("hydra.core.Term", Some(v("rest")),
            Field("annotated", lam("at",
              app(app(v("toPairs"),
                app(app(prim("hydra.lib.lists.cons"),
                  app(prim("hydra.lib.maps.toList"), app(proj("hydra.core.AnnotatedTerm", "annotation"), v("at")))),
                  v("rest"))),
                app(proj("hydra.core.AnnotatedTerm", "body"), v("at")))))),
          v("t")))),
        app(prim("hydra.lib.maps.fromList"),
          app(prim("hydra.lib.lists.concat"),
            app(app(v("toPairs"), Term.list(Seq.empty)), v("term"))))))

    // hydra.annotations.setAnnotation = \key -> \val -> \m ->
    //   maybe(delete(key, m), \v -> insert(key, v, m), val)
    boundTerms("hydra.annotations.setAnnotation") =
      lam("key", lam("val", lam("m",
        app(app(app(prim("hydra.lib.maybes.maybe"),
          app(app(prim("hydra.lib.maps.delete"), v("key")), v("m"))),
          lam("v2", app(app(app(prim("hydra.lib.maps.insert"), v("key")), v("v2")), v("m")))),
          v("val")))))

    // hydra.annotations.setTermAnnotation = \key -> \val -> \term ->
    //   let stripped = deannotateTerm(term)
    //       anns = setAnnotation(key, val, termAnnotationInternal(term))
    //   in if null(anns) then stripped else annotated(AnnotatedTerm(stripped, anns))
    boundTerms("hydra.annotations.setTermAnnotation") =
      lam("key", lam("val", lam("term",
        let1("stripped", app(v("hydra.rewriting.deannotateTerm"), v("term")),
          let1("anns",
            app(app(app(v("hydra.annotations.setAnnotation"), v("key")), v("val")),
              app(v("hydra.annotations.termAnnotationInternal"), v("term"))),
            app(app(app(prim("hydra.lib.logic.ifElse"),
              app(prim("hydra.lib.maps.null"), v("anns"))),
              v("stripped")),
              inj("hydra.core.Term", "annotated",
                rec("hydra.core.AnnotatedTerm",
                  Field("body", v("stripped")),
                  Field("annotation", v("anns"))))))))))

    // hydra.annotations.setTermDescription = \d -> setTermAnnotation(key_description, map(\s -> Term.literal(Literal.string(s)), d))
    boundTerms("hydra.annotations.setTermDescription") =
      lam("d", app(app(v("hydra.annotations.setTermAnnotation"),
        v("hydra.constants.key_description")),
        app(app(prim("hydra.lib.maybes.map"),
          lam("s", inj("hydra.core.Term", "literal", inj("hydra.core.Literal", "string", v("s"))))),
          v("d"))))

    // hydra.annotations.getTermAnnotation = \key -> \term -> lookup(key, termAnnotationInternal(term))
    boundTerms("hydra.annotations.getTermAnnotation") =
      lam("key", lam("term",
        app(app(prim("hydra.lib.maps.lookup"), v("key")),
          app(v("hydra.annotations.termAnnotationInternal"), v("term")))))

    // hydra.annotations.getDescription = \cx -> \g -> \anns ->
    //   maybe(right(nothing), \descTerm -> case descTerm of { literal(\l) -> case l of { string(\s) -> right(just(s)); _ -> left(err) }; _ -> left(err) }, lookup(key_description, anns))
    boundTerms("hydra.annotations.getDescription") =
      lam("cx", lam("g", lam("anns",
        app(app(app(prim("hydra.lib.maybes.maybe"),
          Term.either(Right(Term.maybe(None)))),  // right(nothing)
          lam("descTerm", app(
            cases("hydra.core.Term", Some(
              Term.either(Left(rec("hydra.context.InContext",
                Field("object", inj("hydra.errors.Error", "other", Term.wrap(WrappedTerm("hydra.errors.OtherError", Term.literal(Literal.string("Expected string literal")))))),
                Field("context", v("cx")))))),
              Field("literal", lam("lit", app(
                cases("hydra.core.Literal", Some(
                  Term.either(Left(rec("hydra.context.InContext",
                    Field("object", inj("hydra.errors.Error", "other", Term.wrap(WrappedTerm("hydra.errors.OtherError", Term.literal(Literal.string("Expected string literal")))))),
                    Field("context", v("cx")))))),
                  Field("string", lam("s", Term.either(Right(Term.maybe(Some(v("s")))))))),
                v("lit"))))),
            v("descTerm")))),
          app(app(prim("hydra.lib.maps.lookup"), v("hydra.constants.key_description")), v("anns"))))))

    // hydra.annotations.getTermDescription = \cx -> \g -> \term ->
    //   let peel = \t -> case t of { typeLambda(tl) -> peel(tl.body); typeApplication(ta) -> peel(ta.body); _ -> t }
    //   in getDescription(cx)(g)(termAnnotationInternal(peel(term)))
    boundTerms("hydra.annotations.getTermDescription") =
      lam("cx", lam("g", lam("term",
        let1("peel", lam("t", app(
          cases("hydra.core.Term", Some(v("t")),
            Field("typeLambda", lam("tl", app(v("peel"), app(proj("hydra.core.TypeLambda", "body"), v("tl"))))),
            Field("typeApplication", lam("ta", app(v("peel"), app(proj("hydra.core.TypeApplicationTerm", "body"), v("ta")))))),
          v("t"))),
          app(app(app(v("hydra.annotations.getDescription"), v("cx")), v("g")),
            app(v("hydra.annotations.termAnnotationInternal"), app(v("peel"), v("term"))))))))
  }
}

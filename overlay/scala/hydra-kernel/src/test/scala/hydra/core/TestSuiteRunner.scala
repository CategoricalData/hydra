package hydra

import hydra.core.model.*
import hydra.core.graph.{Graph, Primitive}
import hydra.core.testing.*
import hydra.core.overlay.scala.Libraries
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

/**
 * Scala executor for Hydra's language-agnostic test suite.
 *
 * All test cases are now UniversalTestCase instances (string comparison).
 * Legacy per-type handlers have been removed. Skip-tag checking, the effectful-test temp
 * dir, and benchmark JSON rendering are shared with hydra-build's BuildTestSuiteRunner via
 * HydraTestGroupSupport (#547); test(...) registration stays here (tied to ScalaTest's
 * AnyFunSuite construction-time DSL).
 */
class TestSuiteRunner extends AnyFunSuite with BeforeAndAfterAll with HydraTestGroupSupport {

  private val allTests: TestGroup = hydra.core.test.testSuite.allTests

  // Benchmark support: when HYDRA_BENCHMARK_OUTPUT is set, accumulate per-group
  // elapsed times by bracketing each group's tests with sentinel tests that
  // start/stop a timer keyed by Hydra-path. ScalaTest runs registered tests in
  // registration order, so sentinels reliably surround the group's real tests.
  // Mirrors the Java head's pattern (000_TIMER_START / 999_TIMER_END).
  private val benchmarkOutput: Option[String] = Option(System.getenv("HYDRA_BENCHMARK_OUTPUT"))
  private val benchmarkTimers = _root_.scala.collection.mutable.Map.empty[String, Long]
  private val benchmarkResults = _root_.scala.collection.mutable.Map.empty[String, Double]

  registerTests(allTests, allTests.name)

  override def afterAll(): Unit = {
    benchmarkOutput.foreach(path => writeBenchmarkJson(path, allTests, benchmarkResults))
    super.afterAll()
  }

  private def registerTests(group: TestGroup, path: String): Unit = {
    // Timer start sentinel
    if (benchmarkOutput.isDefined) {
      test(path + "/000_TIMER_START") { benchmarkTimers += (path -> System.nanoTime()) }
    }

    for (tc <- group.cases) {
      val name = tc.name + tc.description.map(d => ": " + d).getOrElse("")
      val fullPath = path + "/" + name
      if (!shouldSkip(tc)) registerTestCase(fullPath, tc)
    }
    for (subgroup <- group.subgroups)
      registerTests(subgroup, path + "/" + subgroup.name)

    // Timer stop sentinel
    if (benchmarkOutput.isDefined) {
      test(path + "/999_TIMER_END") {
        benchmarkTimers.get(path).foreach { start =>
          benchmarkResults += (path -> (System.nanoTime() - start) / 1_000_000.0)
        }
      }
    }
  }

  private def registerTestCase(name: String, tc: TestCaseWithMetadata): Unit = {
    tc.`case` match {
      case TestCase.universal(uc) =>
        // For #311: actual and expected are unit-thunks; force them inside the test
        // block so ScalaTest's per-test timer covers expression evaluation, rather
        // than firing them at allTests build time.
        test(name) { assert(uc.expected(()) == uc.actual(())) }
      case TestCase.effectful(ec) =>
        // For #494: effect<t> is transparent in Scala (effect<t> = t), so the 'actual'
        // thunk eagerly performs the effect (e.g. file I/O) and returns the resulting
        // string. Prepare a guaranteed-empty canonical temp directory first, then force
        // both unit-thunks inside the per-test timer. Mirrors the Java/Haskell runners.
        test(name) {
          prepareEffectfulTempDir()
          assert(ec.expected(()) == ec.actual(()))
        }
      case _ =>
        test(name) { cancel("Unhandled test case type") }
    }
  }
}

object TestSuiteRunner {

  // --- DSL helpers for building term-level representations ---

  private def lambda(param: String, body: Term): Term =
    Term.lambda(Lambda(param, None, body))

  private def lambda(p1: String, p2: String, body: Term): Term =
    lambda(p1, lambda(p2, body))

  private def apply(f: Term, a: Term): Term =
    Term.application(Application(f, a))

  // Variadic curried application, mirroring hydra.core.dsl.Terms.apply(func, args...)
  // in the Java head. Lets the #386/#443 annotation helpers below pass 3+ args
  // (e.g. foldl(fn, init, list)) the same way the Java TestSuiteRunner does.
  private def apply(f: Term, a: Term, rest: Term*): Term =
    rest.foldLeft(apply(f, a))((acc, t) => apply(acc, t))

  private def variable(name: String): Term =
    Term.variable(name)

  private def primitive(name: String): Term =
    Term.variable(name)

  private def matchTerm(typeName: String, default: Option[Term], fields: Field*): Term =
    Term.cases(CaseStatement(typeName, default, fields.toSeq.map(f => CaseAlternative(f.name, f.term))))

  private def project(typeName: String, fieldName: String): Term =
    Term.project(Projection(typeName, fieldName))

  private def let_(name: String, value: Term, body: Term): Term =
    Term.let(Let(Seq(Binding(name, value, None)), body))

  private def inject(typeName: String, fieldName: String, term: Term): Term =
    Term.inject(Injection(typeName, Field(fieldName, term)))

  private def record(typeName: String, fields: Field*): Term =
    Term.record(Record(typeName, fields.toSeq))

  private def field(name: String, term: Term): Field =
    Field(name, term)

  private def right(t: Term): Term =
    Term.either(Right(t))

  private def left(t: Term): Term =
    Term.either(Left(t))

  private def just(t: Term): Term =
    Term.optional(Some(t))

  private def nothing(): Term =
    Term.optional(None)

  private def string(s: String): Term =
    Term.literal(Literal.string(s))

  private def list(terms: Term*): Term =
    Term.list(terms.toSeq)

  // Build a Term.pair value (mirrors hydra.core.dsl.Terms.pair, #443).
  // Term.pair wraps a Tuple2[Term, Term] in generated Scala core (there is no
  // hydra.core.model.Pair term constructor — only PairType at the type level).
  private def pair(a: Term, b: Term): Term =
    Term.pair((a, b))

  private def wrap(typeName: String, term: Term): Term =
    Term.wrap(WrappedTerm(typeName, term))

  /**
   * Add term-level bindings for annotation and rewriting functions needed by tests.
   * These are hand-written because the generated source modules exceed method size limits.
   */
  private def addAnnotationsBindings(boundTerms: _root_.scala.collection.mutable.Map[String, Term]): Unit = {
    // hydra.core.annotations.getAnnotationMap (#386):
    //   getAnnotationMap :: Term -> Map<Name, Term>
    //   Project the (Name, value) entries from a TermMap-with-TermVariable-keys
    //   annotation; return Maps.empty for any other shape.
    // #443: previously these used hydra.lib.tuples.{pair,fst,snd}, which are
    // not registered primitives. Migrated to hydra.core.lib.pairs.{first,second}
    // for projection and a local pair() helper for construction (mirrors
    // Java's hydra.core.dsl.Terms.pair).
    boundTerms += ("hydra.core.annotations.getAnnotationMap" ->
      lambda("t",
        apply(
          matchTerm("hydra.core.model.Term", Some(apply(primitive("hydra.core.lib.maps.empty"), variable("t"))),
            field("map", lambda("m",
              apply(primitive("hydra.core.lib.maps.fromList"),
                apply(apply(primitive("hydra.core.lib.lists.foldl"),
                  lambda("acc", "pair",
                    apply(
                      matchTerm("hydra.core.model.Term",
                        Some(variable("acc")),
                        field("variable", lambda("n",
                          apply(apply(primitive("hydra.core.lib.lists.cons"),
                            pair(
                              variable("n"),
                              apply(primitive("hydra.core.lib.pairs.second"), variable("pair")))),
                            variable("acc"))))),
                      apply(primitive("hydra.core.lib.pairs.first"), variable("pair")))),
                  list()),
                  apply(primitive("hydra.core.lib.maps.toList"), variable("m"))))))),
          variable("t"))))

    // hydra.core.annotations.wrapAnnotationMap (#386):
    //   wrapAnnotationMap :: Map<Name, Term> -> Term
    //   Encode each Name key as a TermVariable, then wrap as a TermMap.
    boundTerms += ("hydra.core.annotations.wrapAnnotationMap" ->
      lambda("m",
        inject("hydra.core.model.Term", "map",
          apply(primitive("hydra.core.lib.maps.fromList"),
            apply(apply(primitive("hydra.core.lib.lists.map"),
              lambda("pair",
                pair(
                  inject("hydra.core.model.Term", "variable",
                    apply(primitive("hydra.core.lib.pairs.first"), variable("pair"))),
                  apply(primitive("hydra.core.lib.pairs.second"), variable("pair"))))),
              apply(primitive("hydra.core.lib.maps.toList"), variable("m")))))))

    // hydra.core.rewriting.deannotateTerm
    boundTerms += ("hydra.core.rewriting.deannotateTerm" ->
      lambda("t",
        apply(
          matchTerm("hydra.core.model.Term", Some(variable("t")),
            field("annotated", lambda("at",
              apply(variable("hydra.core.rewriting.deannotateTerm"),
                apply(project("hydra.core.model.AnnotatedTerm", "body"), variable("at")))))),
          variable("t"))))

    // hydra.core.annotations.termAnnotationInternal
    // After #386: the annotation field is a Term, not a Map. We project the
    // map payload out via Annotations.getAnnotationMap (which unwraps TermMap
    // entries whose keys are TermVariable into a Map<Name, Term>; non-map
    // annotations contribute the empty map).
    boundTerms += ("hydra.core.annotations.termAnnotationInternal" ->
      lambda("term",
        let_("toPairs",
          lambda("rest", "t",
            apply(
              matchTerm("hydra.core.model.Term",
                Some(variable("rest")),
                field("annotated", lambda("at",
                  apply(apply(variable("toPairs"),
                    apply(apply(primitive("hydra.core.lib.lists.cons"),
                      apply(primitive("hydra.core.lib.maps.toList"),
                        apply(variable("hydra.core.annotations.getAnnotationMap"),
                          apply(project("hydra.core.model.AnnotatedTerm", "annotation"), variable("at"))))),
                      variable("rest"))),
                    apply(project("hydra.core.model.AnnotatedTerm", "body"), variable("at")))))),
              variable("t"))),
          apply(primitive("hydra.core.lib.maps.fromList"),
            apply(primitive("hydra.core.lib.lists.concat"),
              apply(apply(variable("toPairs"), list()), variable("term")))))))

    // hydra.core.annotations.setAnnotation
    boundTerms += ("hydra.core.annotations.setAnnotation" ->
      lambda("key",
        lambda("val",
          lambda("m",
            apply(apply(apply(primitive("hydra.core.lib.optionals.match"),
              variable("val")),
              apply(apply(primitive("hydra.core.lib.maps.delete"), variable("key")), variable("m"))),
              lambda("v",
                apply(apply(apply(primitive("hydra.core.lib.maps.insert"),
                  variable("key")), variable("v")), variable("m"))))))))

    // hydra.core.annotations.setTermAnnotation
    // After #386: the annotation field is a Term. The map produced by
    // setAnnotation is wrapped via Annotations.wrapAnnotationMap before being
    // stored in AnnotatedTerm.
    boundTerms += ("hydra.core.annotations.setTermAnnotation" ->
      lambda("key",
        lambda("val",
          lambda("term",
            let_("stripped", apply(variable("hydra.core.rewriting.deannotateTerm"), variable("term")),
              let_("anns",
                apply(apply(apply(variable("hydra.core.annotations.setAnnotation"), variable("key")), variable("val")),
                  apply(variable("hydra.core.annotations.termAnnotationInternal"), variable("term"))),
                apply(apply(apply(primitive("hydra.core.lib.logic.ifElse"),
                  apply(primitive("hydra.core.lib.maps.isEmpty"), variable("anns"))),
                  variable("stripped")),
                  inject("hydra.core.model.Term", "annotated",
                    record("hydra.core.model.AnnotatedTerm",
                      field("body", variable("stripped")),
                      field("annotation",
                        apply(variable("hydra.core.annotations.wrapAnnotationMap"), variable("anns"))))))))))))

    // hydra.core.annotations.setTermDescription
    boundTerms += ("hydra.core.annotations.setTermDescription" ->
      lambda("d",
        apply(apply(variable("hydra.core.annotations.setTermAnnotation"),
          variable("hydra.core.constants.keyDescription")),
          apply(apply(primitive("hydra.core.lib.optionals.map"),
            lambda("s",
              inject("hydra.core.model.Term", "literal",
                inject("hydra.core.model.Literal", "string", variable("s"))))),
            variable("d")))))

    // hydra.core.annotations.getDescription
    boundTerms += ("hydra.core.annotations.getDescription" ->
      lambda("cx",
        lambda("g",
          lambda("anns",
            apply(apply(apply(primitive("hydra.core.lib.optionals.match"),
              apply(apply(primitive("hydra.core.lib.maps.lookup"),
                variable("hydra.core.constants.keyDescription")),
                variable("anns"))),
              right(nothing())),
              lambda("descTerm",
                apply(
                  matchTerm("hydra.core.model.Term", Some(
                    left(inject("hydra.core.errors.Error", "other", wrap("hydra.core.errors.OtherError", string("Expected string literal"))))),
                    field("literal", lambda("lit",
                      apply(
                        matchTerm("hydra.core.model.Literal", Some(
                          left(inject("hydra.core.errors.Error", "other", wrap("hydra.core.errors.OtherError", string("Expected string literal"))))),
                          field("string", lambda("s", right(just(variable("s")))))),
                        variable("lit"))))),
                  variable("descTerm"))))))))

    // hydra.core.annotations.getTermDescription
    boundTerms += ("hydra.core.annotations.getTermDescription" ->
      lambda("cx",
        lambda("g",
          lambda("term",
            let_("peel",
              lambda("t",
                apply(
                  matchTerm("hydra.core.model.Term", Some(variable("t")),
                    field("typeLambda", lambda("tl",
                      apply(variable("peel"),
                        apply(project("hydra.core.model.TypeLambda", "body"), variable("tl"))))),
                    field("typeApplication", lambda("ta",
                      apply(variable("peel"),
                        apply(project("hydra.core.model.TypeApplicationTerm", "body"), variable("ta")))))),
                  variable("t"))),
              apply(apply(apply(variable("hydra.core.annotations.getDescription"), variable("cx")), variable("g")),
                apply(variable("hydra.core.annotations.termAnnotationInternal"),
                  apply(variable("peel"), variable("term")))))))))

    // hydra.core.annotations.getTermAnnotation
    boundTerms += ("hydra.core.annotations.getTermAnnotation" ->
      lambda("key",
        lambda("term",
          apply(apply(primitive("hydra.core.lib.maps.lookup"), variable("key")),
            apply(variable("hydra.core.annotations.termAnnotationInternal"), variable("term"))))))
  }

  /**
   * Add term-level binding for hydra.core.lexical.emptyGraph.
   */
  private def addConstantBindings(boundTerms: _root_.scala.collection.mutable.Map[String, Term]): Unit = {
    boundTerms += ("hydra.core.lexical.emptyGraph" ->
      record("hydra.core.graph.Graph",
        field("boundTerms", Term.map(Map.empty)),
        field("boundTypes", Term.map(Map.empty)),
        field("classConstraints", Term.map(Map.empty)),
        field("lambdaVariables", Term.set(Set.empty)),
        field("metadata", Term.map(Map.empty)),
        field("primitives", Term.map(Map.empty)),
        field("schemaTypes", Term.map(Map.empty)),
        field("typeVariables", Term.set(Set.empty))))
  }

  /**
   * Build kernel type definitions needed by inference/checking tests.
   */
  private def buildKernelTypes(): Map[String, hydra.core.model.Type] = {
    var types: Map[String, hydra.core.model.Type] = Map.empty

    // CoderDirection: enum with encode, decode
    types += ("hydra.core.coders.CoderDirection" ->
      Type.union(Seq(
        FieldType("encode", Type.unit),
        FieldType("decode", Type.unit))))

    val contextName = "hydra.core.typing.InferenceContext"
    val errorName = "hydra.core.errors.Error"
    def eitherError(v: hydra.core.model.Type): hydra.core.model.Type =
      Type.either(EitherType(Type.variable("e"), v))

    // Coder: forall v1 v2 e. {encode: v1 -> Either e v2, decode: v2 -> Either e v1}
    val encodeType = Type.function(FunctionType(
      Type.variable("v1"),
      eitherError(Type.variable("v2"))))
    val decodeType = Type.function(FunctionType(
      Type.variable("v2"),
      eitherError(Type.variable("v1"))))
    val coderBody = Type.record(Seq(
      FieldType("encode", encodeType),
      FieldType("decode", decodeType)))
    types += ("hydra.core.coders.Coder" ->
      Type.forall(ForallType("v1",
        Type.forall(ForallType("v2",
          Type.forall(ForallType("e", coderBody)))))))

    // InferenceContext
    types += (contextName ->
      Type.record(Seq(
        FieldType("freshTypeVariableCount", Type.literal(LiteralType.integer(IntegerType.int32))),
        FieldType("trace", Type.list(Type.variable("hydra.core.paths.SubtermStep"))))))

    // Error types
    val otherErrorName = "hydra.core.errors.OtherError"
    types += (otherErrorName ->
      Type.wrap(Type.literal(LiteralType.string)))
    types += (errorName ->
      Type.union(Seq(
        FieldType("other", Type.variable(otherErrorName)))))

    // Type (hydra.core.model.Type)
    val typeName = "hydra.core.model.Type"
    types += (typeName ->
      Type.union(Seq(
        FieldType("annotated", Type.variable("annotatedType")),
        FieldType("application", Type.variable("applicationElim")),
        FieldType("either", Type.variable("eitherType")),
        FieldType("forall", Type.variable("forallType")),
        FieldType("function", Type.variable("functionType")),
        FieldType("list", Type.variable(typeName)),
        FieldType("literal", Type.variable("literalType")),
        FieldType("map", Type.variable("mapType")),
        FieldType("optional", Type.variable(typeName)),
        FieldType("pair", Type.variable("pairType")),
        FieldType("record", Type.variable("rowType")),
        FieldType("set", Type.variable(typeName)),
        FieldType("union", Type.variable("rowType")),
        FieldType("unit", Type.unit),
        FieldType("variable", Type.variable("name")),
        FieldType("wrap", Type.variable("wrappedType")))))

    // Name
    types += ("hydra.core.model.Name" ->
      Type.wrap(Type.literal(LiteralType.string)))

    // ForallType
    types += ("hydra.core.model.ForallType" ->
      Type.record(Seq(
        FieldType("parameter", Type.variable("hydra.core.model.Name")),
        FieldType("body", Type.variable(typeName)))))

    // Comparison
    types += ("hydra.core.util.Comparison" ->
      Type.union(Seq(
        FieldType("lessThan", Type.unit),
        FieldType("equalTo", Type.unit),
        FieldType("greaterThan", Type.unit))))

    // CaseConvention
    types += ("hydra.core.util.CaseConvention" ->
      Type.union(Seq(
        FieldType("camel", Type.unit),
        FieldType("pascal", Type.unit),
        FieldType("lowerSnake", Type.unit),
        FieldType("upperSnake", Type.unit))))

    // Precision
    types += ("hydra.core.util.Precision" ->
      Type.union(Seq(
        FieldType("arbitrary", Type.unit),
        FieldType("bits", Type.literal(LiteralType.integer(IntegerType.int32))))))

    types
  }

  // When true, use primitiveDefinitionDefaultImplementation instead of native implementations.
  // Activated via -Dhydra.defaultImpls=true or HYDRA_DEFAULT_IMPLS=1 env var.
  private val useDefaultImpls: Boolean =
    System.getProperty("hydra.defaultImpls") == "true" ||
    System.getenv("HYDRA_DEFAULT_IMPLS") == "1"

  private def patchWithDefaultImpls(primitives: Map[String, Primitive], nativeGraph: Graph): Map[String, Primitive] = {
    primitives.map { case (name, prim) =>
      prim.definition.defaultImplementation match {
        case Some(implTerm) =>
          name -> prim.copy(implementation = _ => args => {
            val applied = args.foldLeft(implTerm)((f, a) =>
              hydra.core.model.Term.application(hydra.core.model.Application(f, a)))
            hydra.core.reduction.reduceTerm(())(nativeGraph)(true)(applied)
          })
        case None => name -> prim
      }
    }
  }

  /**
   * Build the test graph with primitives, bound terms, and schema types.
   * Called from generated test code (testGraph.scala).
   */
  def buildTestGraph(): Graph = {
    val primitives: Map[String, Primitive] = Libraries.standardPrimitives()

    // Schema types from test types + kernel types
    val testTypes: Map[hydra.core.model.Name, hydra.core.model.Type] = hydra.core.test.testGraph.testTypes
    val kernelTypes: Map[hydra.core.model.Name, hydra.core.model.Type] = buildKernelTypes()
    val allTypes: Map[hydra.core.model.Name, hydra.core.model.Type] = kernelTypes ++ testTypes
    val schemaTypes: Map[hydra.core.model.Name, TypeScheme] = allTypes.map { case (k, v) =>
      k -> hydra.core.resolution.typeToTypeScheme(v)
    }

    // Bound terms
    val boundTerms = _root_.scala.collection.mutable.Map.empty[hydra.core.model.Name, hydra.core.model.Term]

    // Primitives are resolved via graphPrimitives, not boundTerms.
    // No need to bridge them as term bindings.

    // Kernel constants needed by annotation and other tests
    def nameConstant(s: String): hydra.core.model.Term =
      Term.wrap(WrappedTerm("hydra.core.model.Name", Term.literal(Literal.string(s))))
    boundTerms += ("hydra.core.constants.keyClasses" -> nameConstant("classes"))
    boundTerms += ("hydra.core.constants.keyDescription" -> nameConstant("description"))
    boundTerms += ("hydra.core.constants.keyType" -> nameConstant("type"))
    boundTerms += ("hydra.core.constants.keyDebugId" -> nameConstant("debugId"))
    boundTerms += ("hydra.core.constants.keyFirstClassType" -> nameConstant("firstClassType"))
    boundTerms += ("hydra.core.constants.keyDeprecated" -> nameConstant("deprecated"))
    boundTerms += ("hydra.core.constants.keyExclude" -> nameConstant("exclude"))
    boundTerms += ("hydra.core.constants.keyMaxLength" -> nameConstant("maxLength"))
    boundTerms += ("hydra.core.constants.keyMinLength" -> nameConstant("minLength"))
    boundTerms += ("hydra.core.constants.keyPreserveFieldName" -> nameConstant("preserveFieldName"))
    boundTerms += ("hydra.core.constants.keyFreshTypeVariableCount" -> nameConstant("freshTypeVariableCount"))
    boundTerms += ("hydra.core.constants.ignoredVariable" ->
      Term.literal(Literal.string("_")))
    boundTerms += ("hydra.core.constants.maxTraceDepth" ->
      Term.literal(Literal.integer(IntegerValue.int32(5000))))
    boundTerms += ("hydra.core.constants.debugInference" ->
      Term.literal(Literal.boolean(true)))

    // Add kernel constant term bindings
    addConstantBindings(boundTerms)

    // Add kernel annotation/rewriting term bindings
    addAnnotationsBindings(boundTerms)

    // Test term bindings
    boundTerms ++= hydra.core.test.testGraph.testTerms

    // Encoded types as term bindings
    for ((name, typ) <- allTypes) {
      boundTerms += (name -> hydra.core.encode.model.`type`(typ))
    }

    val nativeGraph = Graph(
      boundTerms = boundTerms.toMap,
      boundTypes = Map.empty,
      classConstraints = Map.empty,
      lambdaVariables = Set.empty,
      metadata = Map.empty,
      primitives = primitives,
      schemaTypes = schemaTypes,
      typeVariables = Set.empty)

    if (!useDefaultImpls) nativeGraph
    else nativeGraph.copy(primitives = patchWithDefaultImpls(primitives, nativeGraph))
  }
}

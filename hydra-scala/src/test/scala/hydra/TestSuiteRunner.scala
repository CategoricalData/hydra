package hydra

import hydra.core.*
import hydra.graph.{Graph, Primitive}
import hydra.testing.*
import hydra.lib.Libraries
import org.scalatest.funsuite.AnyFunSuite

/**
 * Scala executor for Hydra's language-agnostic test suite.
 *
 * All test cases are now UniversalTestCase instances (string comparison).
 * Legacy per-type handlers have been removed.
 */
class TestSuiteRunner extends AnyFunSuite {

  private val allTests: TestGroup = hydra.test.testSuite.allTests

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
      case TestCase.universal(uc) =>
        test(name) { assert(uc.expected == uc.actual) }
      case _ =>
        test(name) { cancel("Unhandled test case type") }
    }
  }
}

object TestSuiteRunner {

  // --- DSL helpers for building term-level representations ---

  private def lambda(param: String, body: Term): Term =
    Term.function(Function.lambda(Lambda(param, None, body)))

  private def lambda(p1: String, p2: String, body: Term): Term =
    lambda(p1, lambda(p2, body))

  private def apply(f: Term, a: Term): Term =
    Term.application(Application(f, a))

  private def variable(name: String): Term =
    Term.variable(name)

  private def primitive(name: String): Term =
    Term.variable(name)

  private def matchTerm(typeName: String, default: Option[Term], fields: Field*): Term =
    Term.function(Function.elimination(Elimination.union(CaseStatement(typeName, default, fields.toSeq))))

  private def project(typeName: String, fieldName: String): Term =
    Term.function(Function.elimination(Elimination.record(Projection(typeName, fieldName))))

  private def let_(name: String, value: Term, body: Term): Term =
    Term.let(Let(Seq(Binding(name, value, None)), body))

  private def inject(typeName: String, fieldName: String, term: Term): Term =
    Term.union(Injection(typeName, Field(fieldName, term)))

  private def record(typeName: String, fields: Field*): Term =
    Term.record(Record(typeName, fields.toSeq))

  private def field(name: String, term: Term): Field =
    Field(name, term)

  private def right(t: Term): Term =
    Term.either(Right(t))

  private def left(t: Term): Term =
    Term.either(Left(t))

  private def just(t: Term): Term =
    Term.maybe(Some(t))

  private def nothing(): Term =
    Term.maybe(None)

  private def string(s: String): Term =
    Term.literal(Literal.string(s))

  private def list(terms: Term*): Term =
    Term.list(terms.toSeq)

  private def wrap(typeName: String, term: Term): Term =
    Term.wrap(WrappedTerm(typeName, term))

  /**
   * Add term-level bindings for annotation and rewriting functions needed by tests.
   * These are hand-written because the generated source modules exceed method size limits.
   */
  private def addAnnotationsBindings(boundTerms: scala.collection.mutable.Map[String, Term]): Unit = {
    // hydra.rewriting.deannotateTerm
    boundTerms += ("hydra.rewriting.deannotateTerm" ->
      lambda("t",
        apply(
          matchTerm("hydra.core.Term", Some(variable("t")),
            field("annotated", lambda("at",
              apply(variable("hydra.rewriting.deannotateTerm"),
                apply(project("hydra.core.AnnotatedTerm", "body"), variable("at")))))),
          variable("t"))))

    // hydra.annotations.termAnnotationInternal
    boundTerms += ("hydra.annotations.termAnnotationInternal" ->
      lambda("term",
        let_("toPairs",
          lambda("rest", "t",
            apply(
              matchTerm("hydra.core.Term",
                Some(variable("rest")),
                field("annotated", lambda("at",
                  apply(apply(variable("toPairs"),
                    apply(apply(primitive("hydra.lib.lists.cons"),
                      apply(primitive("hydra.lib.maps.toList"),
                        apply(project("hydra.core.AnnotatedTerm", "annotation"), variable("at")))),
                      variable("rest"))),
                    apply(project("hydra.core.AnnotatedTerm", "body"), variable("at")))))),
              variable("t"))),
          apply(primitive("hydra.lib.maps.fromList"),
            apply(primitive("hydra.lib.lists.concat"),
              apply(apply(variable("toPairs"), list()), variable("term")))))))

    // hydra.annotations.setAnnotation
    boundTerms += ("hydra.annotations.setAnnotation" ->
      lambda("key",
        lambda("val",
          lambda("m",
            apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
              apply(apply(primitive("hydra.lib.maps.delete"), variable("key")), variable("m"))),
              lambda("v",
                apply(apply(apply(primitive("hydra.lib.maps.insert"),
                  variable("key")), variable("v")), variable("m")))),
              variable("val"))))))

    // hydra.annotations.setTermAnnotation
    boundTerms += ("hydra.annotations.setTermAnnotation" ->
      lambda("key",
        lambda("val",
          lambda("term",
            let_("stripped", apply(variable("hydra.rewriting.deannotateTerm"), variable("term")),
              let_("anns",
                apply(apply(apply(variable("hydra.annotations.setAnnotation"), variable("key")), variable("val")),
                  apply(variable("hydra.annotations.termAnnotationInternal"), variable("term"))),
                apply(apply(apply(primitive("hydra.lib.logic.ifElse"),
                  apply(primitive("hydra.lib.maps.null"), variable("anns"))),
                  variable("stripped")),
                  inject("hydra.core.Term", "annotated",
                    record("hydra.core.AnnotatedTerm",
                      field("body", variable("stripped")),
                      field("annotation", variable("anns")))))))))))

    // hydra.annotations.setTermDescription
    boundTerms += ("hydra.annotations.setTermDescription" ->
      lambda("d",
        apply(apply(variable("hydra.annotations.setTermAnnotation"),
          variable("hydra.constants.key_description")),
          apply(apply(primitive("hydra.lib.maybes.map"),
            lambda("s",
              inject("hydra.core.Term", "literal",
                inject("hydra.core.Literal", "string", variable("s"))))),
            variable("d")))))

    // hydra.annotations.getDescription
    boundTerms += ("hydra.annotations.getDescription" ->
      lambda("cx",
        lambda("g",
          lambda("anns",
            apply(apply(apply(primitive("hydra.lib.maybes.maybe"),
              right(nothing())),
              lambda("descTerm",
                apply(
                  matchTerm("hydra.core.Term", Some(
                    left(record("hydra.context.InContext",
                      field("object", inject("hydra.errors.Error", "other", wrap("hydra.errors.OtherError", string("Expected string literal")))),
                      field("context", variable("cx"))))),
                    field("literal", lambda("lit",
                      apply(
                        matchTerm("hydra.core.Literal", Some(
                          left(record("hydra.context.InContext",
                            field("object", inject("hydra.errors.Error", "other", wrap("hydra.errors.OtherError", string("Expected string literal")))),
                            field("context", variable("cx"))))),
                          field("string", lambda("s", right(just(variable("s")))))),
                        variable("lit"))))),
                  variable("descTerm")))),
              apply(apply(primitive("hydra.lib.maps.lookup"),
                variable("hydra.constants.key_description")),
                variable("anns")))))))

    // hydra.annotations.getTermDescription
    boundTerms += ("hydra.annotations.getTermDescription" ->
      lambda("cx",
        lambda("g",
          lambda("term",
            let_("peel",
              lambda("t",
                apply(
                  matchTerm("hydra.core.Term", Some(variable("t")),
                    field("typeLambda", lambda("tl",
                      apply(variable("peel"),
                        apply(project("hydra.core.TypeLambda", "body"), variable("tl"))))),
                    field("typeApplication", lambda("ta",
                      apply(variable("peel"),
                        apply(project("hydra.core.TypeApplicationTerm", "body"), variable("ta")))))),
                  variable("t"))),
              apply(apply(apply(variable("hydra.annotations.getDescription"), variable("cx")), variable("g")),
                apply(variable("hydra.annotations.termAnnotationInternal"),
                  apply(variable("peel"), variable("term")))))))))

    // hydra.annotations.getTermAnnotation
    boundTerms += ("hydra.annotations.getTermAnnotation" ->
      lambda("key",
        lambda("term",
          apply(apply(primitive("hydra.lib.maps.lookup"), variable("key")),
            apply(variable("hydra.annotations.termAnnotationInternal"), variable("term"))))))
  }

  /**
   * Add term-level binding for hydra.lexical.emptyGraph.
   */
  private def addConstantBindings(boundTerms: scala.collection.mutable.Map[String, Term]): Unit = {
    boundTerms += ("hydra.lexical.emptyGraph" ->
      record("hydra.graph.Graph",
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
  private def buildKernelTypes(): Map[String, hydra.core.Type] = {
    var types: Map[String, hydra.core.Type] = Map.empty

    // CoderDirection: enum with encode, decode
    types += ("hydra.coders.CoderDirection" ->
      Type.union(Seq(
        FieldType("encode", Type.unit),
        FieldType("decode", Type.unit))))

    val contextName = "hydra.context.Context"
    val inContextName = "hydra.context.InContext"
    val errorName = "hydra.errors.Error"
    val inContextError = Type.application(ApplicationType(
      Type.variable(inContextName),
      Type.variable(errorName)))
    def eitherInContextError(v: hydra.core.Type): hydra.core.Type =
      Type.either(EitherType(inContextError, v))

    // Coder: forall v1 v2. {encode: ..., decode: ...}
    val encodeType = Type.function(FunctionType(
      Type.variable(contextName),
      Type.function(FunctionType(
        Type.variable("v1"),
        eitherInContextError(Type.variable("v2"))))))
    val decodeType = Type.function(FunctionType(
      Type.variable(contextName),
      Type.function(FunctionType(
        Type.variable("v2"),
        eitherInContextError(Type.variable("v1"))))))
    val coderBody = Type.record(Seq(
      FieldType("encode", encodeType),
      FieldType("decode", decodeType)))
    types += ("hydra.util.Coder" ->
      Type.forall(ForallType("v1",
        Type.forall(ForallType("v2", coderBody)))))

    // Context
    types += (contextName ->
      Type.record(Seq(
        FieldType("trace", Type.list(Type.literal(LiteralType.string))),
        FieldType("messages", Type.list(Type.literal(LiteralType.string))),
        FieldType("other", Type.map(MapType(
          Type.variable("hydra.core.Name"),
          Type.variable("hydra.core.Term")))))))

    // InContext
    types += (inContextName ->
      Type.forall(ForallType("e",
        Type.record(Seq(
          FieldType("object", Type.variable("e")),
          FieldType("context", Type.variable(contextName)))))))

    // Error types
    val otherErrorName = "hydra.errors.OtherError"
    types += (otherErrorName ->
      Type.wrap(Type.literal(LiteralType.string)))
    types += (errorName ->
      Type.union(Seq(
        FieldType("other", Type.variable(otherErrorName)))))

    // Type (hydra.core.Type)
    val typeName = "hydra.core.Type"
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
        FieldType("maybe", Type.variable(typeName)),
        FieldType("pair", Type.variable("pairType")),
        FieldType("record", Type.variable("rowType")),
        FieldType("set", Type.variable(typeName)),
        FieldType("union", Type.variable("rowType")),
        FieldType("unit", Type.unit),
        FieldType("variable", Type.variable("name")),
        FieldType("wrap", Type.variable("wrappedType")))))

    // Name
    types += ("hydra.core.Name" ->
      Type.wrap(Type.literal(LiteralType.string)))

    // ForallType
    types += ("hydra.core.ForallType" ->
      Type.record(Seq(
        FieldType("parameter", Type.variable("hydra.core.Name")),
        FieldType("body", Type.variable(typeName)))))

    // Comparison
    types += ("hydra.util.Comparison" ->
      Type.union(Seq(
        FieldType("lessThan", Type.unit),
        FieldType("equalTo", Type.unit),
        FieldType("greaterThan", Type.unit))))

    // CaseConvention
    types += ("hydra.util.CaseConvention" ->
      Type.union(Seq(
        FieldType("camel", Type.unit),
        FieldType("pascal", Type.unit),
        FieldType("lowerSnake", Type.unit),
        FieldType("upperSnake", Type.unit))))

    // Precision
    types += ("hydra.util.Precision" ->
      Type.union(Seq(
        FieldType("arbitrary", Type.unit),
        FieldType("bits", Type.literal(LiteralType.integer(IntegerType.int32))))))

    types
  }

  /**
   * Build the test graph with primitives, bound terms, and schema types.
   * Called from generated test code (testGraph.scala).
   */
  def buildTestGraph(): Graph = {
    val primitives: Map[String, Primitive] = Libraries.standardPrimitives()

    // Schema types from test types + kernel types
    val testTypes: Map[hydra.core.Name, hydra.core.Type] = hydra.test.testGraph.testTypes
    val kernelTypes: Map[hydra.core.Name, hydra.core.Type] = buildKernelTypes()
    val allTypes: Map[hydra.core.Name, hydra.core.Type] = kernelTypes ++ testTypes
    val schemaTypes: Map[hydra.core.Name, TypeScheme] = allTypes.map { case (k, v) =>
      k -> hydra.resolution.typeToTypeScheme(v)
    }

    // Bound terms
    val boundTerms = scala.collection.mutable.Map.empty[hydra.core.Name, hydra.core.Term]

    // Primitives are resolved via graphPrimitives, not boundTerms.
    // No need to bridge them as term bindings.

    // Kernel constants needed by annotation and other tests
    def nameConstant(s: String): hydra.core.Term =
      Term.wrap(WrappedTerm("hydra.core.Name", Term.literal(Literal.string(s))))
    boundTerms += ("hydra.constants.key_classes" -> nameConstant("classes"))
    boundTerms += ("hydra.constants.key_description" -> nameConstant("description"))
    boundTerms += ("hydra.constants.key_type" -> nameConstant("type"))
    boundTerms += ("hydra.constants.key_debugId" -> nameConstant("debugId"))
    boundTerms += ("hydra.constants.key_firstClassType" -> nameConstant("firstClassType"))
    boundTerms += ("hydra.constants.key_deprecated" -> nameConstant("deprecated"))
    boundTerms += ("hydra.constants.key_exclude" -> nameConstant("exclude"))
    boundTerms += ("hydra.constants.key_maxLength" -> nameConstant("maxLength"))
    boundTerms += ("hydra.constants.key_minLength" -> nameConstant("minLength"))
    boundTerms += ("hydra.constants.key_preserveFieldName" -> nameConstant("preserveFieldName"))
    boundTerms += ("hydra.constants.key_freshTypeVariableCount" -> nameConstant("freshTypeVariableCount"))
    boundTerms += ("hydra.constants.ignoredVariable" ->
      Term.literal(Literal.string("_")))
    boundTerms += ("hydra.constants.maxTraceDepth" ->
      Term.literal(Literal.integer(IntegerValue.int32(5000))))
    boundTerms += ("hydra.constants.debugInference" ->
      Term.literal(Literal.boolean(true)))

    // Add kernel constant term bindings
    addConstantBindings(boundTerms)

    // Add kernel annotation/rewriting term bindings
    addAnnotationsBindings(boundTerms)

    // Test term bindings
    boundTerms ++= hydra.test.testGraph.testTerms

    // Encoded types as term bindings
    for ((name, typ) <- allTypes) {
      boundTerms += (name -> hydra.encode.core.`type`(typ))
    }

    Graph(
      boundTerms = boundTerms.toMap,
      boundTypes = Map.empty,
      classConstraints = Map.empty,
      lambdaVariables = Set.empty,
      metadata = Map.empty,
      primitives = primitives,
      schemaTypes = schemaTypes,
      typeVariables = Set.empty)
  }
}

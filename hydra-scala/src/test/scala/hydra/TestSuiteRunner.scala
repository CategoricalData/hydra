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

  /**
   * Build the test graph with primitives, bound terms, and schema types.
   * Called from generated test code (testGraph.scala).
   */
  def buildTestGraph(): Graph = {
    val primitives: Map[String, Primitive] = Libraries.standardPrimitives()

    // Schema types from test types
    val testTypes: Map[hydra.core.Name, hydra.core.Type] = hydra.test.testGraph.testTypes
    val schemaTypes: Map[hydra.core.Name, TypeScheme] = testTypes.map { case (k, v) =>
      k -> hydra.schemas.typeToTypeScheme(v)
    }

    // Bound terms
    var boundTerms: Map[hydra.core.Name, hydra.core.Term] = Map.empty

    // Bridge primitives as term bindings
    val excludedNames = Set(
      "hydra.annotations.setTermAnnotation",
      "hydra.annotations.setTermDescription",
      "hydra.rewriting.deannotateTerm")
    for ((name, _) <- primitives) {
      if (!excludedNames.contains(name)) {
        boundTerms = boundTerms + (name -> Term.function(Function.primitive(name)))
      }
    }

    // Kernel constants needed by annotation and other tests
    def nameConstant(s: String): hydra.core.Term =
      Term.wrap(WrappedTerm("hydra.core.Name", Term.literal(Literal.string(s))))
    boundTerms = boundTerms + ("hydra.constants.key_classes" -> nameConstant("classes"))
    boundTerms = boundTerms + ("hydra.constants.key_description" -> nameConstant("description"))
    boundTerms = boundTerms + ("hydra.constants.key_type" -> nameConstant("type"))
    boundTerms = boundTerms + ("hydra.constants.key_debugId" -> nameConstant("debugId"))
    boundTerms = boundTerms + ("hydra.constants.key_firstClassType" -> nameConstant("firstClassType"))
    boundTerms = boundTerms + ("hydra.constants.key_deprecated" -> nameConstant("deprecated"))
    boundTerms = boundTerms + ("hydra.constants.key_exclude" -> nameConstant("exclude"))
    boundTerms = boundTerms + ("hydra.constants.key_maxLength" -> nameConstant("maxLength"))
    boundTerms = boundTerms + ("hydra.constants.key_minLength" -> nameConstant("minLength"))
    boundTerms = boundTerms + ("hydra.constants.key_preserveFieldName" -> nameConstant("preserveFieldName"))
    boundTerms = boundTerms + ("hydra.constants.key_freshTypeVariableCount" -> nameConstant("freshTypeVariableCount"))
    boundTerms = boundTerms + ("hydra.constants.ignoredVariable" ->
      Term.literal(Literal.string("_")))
    boundTerms = boundTerms + ("hydra.constants.maxTraceDepth" ->
      Term.literal(Literal.integer(IntegerValue.int32(5000))))
    boundTerms = boundTerms + ("hydra.constants.debugInference" ->
      Term.literal(Literal.boolean(true)))

    // Test term bindings
    boundTerms = boundTerms ++ hydra.test.testGraph.testTerms

    // Encoded types as term bindings
    for ((name, typ) <- testTypes) {
      boundTerms = boundTerms + (name -> hydra.encode.core.`type`(typ))
    }

    Graph(
      boundTerms = boundTerms,
      boundTypes = Map.empty,
      classConstraints = Map.empty,
      lambdaVariables = Set.empty,
      metadata = Map.empty,
      primitives = primitives,
      schemaTypes = schemaTypes,
      typeVariables = Set.empty)
  }
}

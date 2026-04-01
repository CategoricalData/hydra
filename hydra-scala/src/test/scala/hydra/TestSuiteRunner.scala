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
 *
 * in src/gen-test/scala/generation/ and are auto-discovered by ScalaTest.
 */
class TestSuiteRunner extends AnyFunSuite {

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
      case TestCase.universal(uc) =>
        test(name) { assert(uc.expected == uc.actual) }

      case _ =>
        test(name) { cancel("Unhandled test case type") }
    }
  }
}

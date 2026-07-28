package hydra

import hydra.testing.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

/**
 * Scala executor for hydra-build's own test suite. Package-scoped counterpart to
 * TestSuiteRunner (hydra-kernel); part of #547's per-package test aggregation. ScalaTest
 * discovers this class alongside TestSuiteRunner automatically, so no
 * composition/registration step is needed.
 *
 * All of hydra-build's test cases are universal (pure string comparison); no
 * effectful-case handling exercised in practice, but included for parity with the kernel
 * runner via HydraTestGroupSupport (#547). No benchmark-output handling (kernel-only
 * feature, not needed for hydra-build's suite).
 */
class BuildTestSuiteRunner extends AnyFunSuite with BeforeAndAfterAll with HydraTestGroupSupport {

  private val allTests: TestGroup = hydra.test.build.testSuite.allTests

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

  private def registerTestCase(name: String, tc: TestCaseWithMetadata): Unit = {
    tc.`case` match {
      case TestCase.universal(uc) =>
        test(name) { assert(uc.expected(()) == uc.actual(())) }
      case TestCase.effectful(ec) =>
        test(name) {
          prepareEffectfulTempDir()
          assert(ec.expected(()) == ec.actual(()))
        }
      case _ =>
        test(name) { cancel("Unhandled test case type") }
    }
  }
}

// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking.failures;

/**
 * Type checking failure test cases
 */
public interface Failures {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(hydra.test.checking.failures.Failures.failOnUntypedTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
  
  static hydra.testing.TestGroup failOnUntypedTests() {
    return new hydra.testing.TestGroup("Fail on untyped (pre-inference) terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(hydra.test.checking.failures.Failures.untypedLambdasTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
  
  static hydra.testing.TestGroup untypedLambdasTests() {
    return new hydra.testing.TestGroup("Untyped lambdas", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.List.<hydra.testing.TestGroup>of()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}

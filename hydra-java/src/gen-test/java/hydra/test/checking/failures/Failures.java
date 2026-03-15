// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking.failures;

/**
 * Type checking failure test cases
 */
public interface Failures {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(hydra.test.checking.failures.Failures.failOnUntypedTests()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
  
  static hydra.testing.TestGroup failOnUntypedTests() {
    return new hydra.testing.TestGroup("Fail on untyped (pre-inference) terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(hydra.test.checking.failures.Failures.untypedLambdasTests()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
  
  static hydra.testing.TestGroup untypedLambdasTests() {
    return new hydra.testing.TestGroup("Untyped lambdas", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (hydra.util.ConsList<hydra.testing.TestGroup>) (hydra.util.ConsList.<hydra.testing.TestGroup>empty()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

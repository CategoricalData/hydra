// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking;

/**
 * Type checking failure test cases
 */
public interface Failures {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("Failures", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(hydra.test.checking.Failures.failOnUntypedTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup failOnUntypedTests() {
    return new hydra.testing.TestGroup("Fail on untyped (pre-inference) terms", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(hydra.test.checking.Failures.untypedLambdasTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }

  static hydra.testing.TestGroup untypedLambdasTests() {
    return new hydra.testing.TestGroup("Untyped lambdas", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), (java.util.List<hydra.testing.TestGroup>) (java.util.Collections.<hydra.testing.TestGroup>emptyList()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

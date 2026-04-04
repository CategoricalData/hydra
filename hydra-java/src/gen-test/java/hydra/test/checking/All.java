// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking;

/**
 * Hydra's type checking test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("checking", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.checking.Advanced.allTests(),
      hydra.test.checking.AlgebraicTypes.allTests(),
      hydra.test.checking.Collections.allTests(),
      hydra.test.checking.Failures.allTests(),
      hydra.test.checking.Fundamentals.allTests(),
      hydra.test.checking.NominalTypes.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

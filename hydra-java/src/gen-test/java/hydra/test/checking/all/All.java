// Note: this is an automatically generated file. Do not edit.

package hydra.test.checking.all;

/**
 * Hydra's type checking test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("checking", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      hydra.test.checking.advanced.Advanced.allTests(),
      hydra.test.checking.algebraicTypes.AlgebraicTypes.allTests(),
      hydra.test.checking.collections.Collections.allTests(),
      hydra.test.checking.failures.Failures.allTests(),
      hydra.test.checking.fundamentals.Fundamentals.allTests(),
      hydra.test.checking.nominalTypes.NominalTypes.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}

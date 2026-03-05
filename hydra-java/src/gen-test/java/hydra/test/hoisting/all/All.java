// Note: this is an automatically generated file. Do not edit.

package hydra.test.hoisting.all;

/**
 * Hydra's hoisting test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hoisting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      hydra.test.hoisting.cases.Cases.allTests(),
      hydra.test.hoisting.let.Let.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}

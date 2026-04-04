// Note: this is an automatically generated file. Do not edit.

package hydra.test.hoisting;

/**
 * Hydra's hoisting test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hoisting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.Arrays.asList(
      hydra.test.hoisting.Cases.allTests(),
      hydra.test.hoisting.Let.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.Collections.<hydra.testing.TestCaseWithMetadata>emptyList()));
  }
}

// Note: this is an automatically generated file. Do not edit.

package hydra.test.hoisting;

/**
 * Hydra's hoisting test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("hoisting", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      hydra.test.hoisting.Cases.allTests(),
      hydra.test.hoisting.Let.allTests()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

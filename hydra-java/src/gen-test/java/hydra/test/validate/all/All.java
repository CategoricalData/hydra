// Note: this is an automatically generated file. Do not edit.

package hydra.test.validate.all;

/**
 * Hydra's validation test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("validation", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(hydra.test.validate.core.Core.allTests()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

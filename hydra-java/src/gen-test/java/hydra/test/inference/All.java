// Note: this is an automatically generated file. Do not edit.

package hydra.test.inference;

/**
 * Hydra's inference test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("inference", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      hydra.test.inference.AlgebraicTypes.allTests(),
      hydra.test.inference.AlgorithmW.allTests(),
      hydra.test.inference.Classes.allTests(),
      hydra.test.inference.Failures.allTests(),
      hydra.test.inference.Fundamentals.allTests(),
      hydra.test.inference.KernelExamples.allTests(),
      hydra.test.inference.NominalTypes.allTests()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>empty()));
  }
}

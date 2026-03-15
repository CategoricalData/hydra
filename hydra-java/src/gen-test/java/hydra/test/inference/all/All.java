// Note: this is an automatically generated file. Do not edit.

package hydra.test.inference.all;

/**
 * Hydra's inference test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("inference", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), hydra.util.ConsList.of(
      hydra.test.inference.algebraicTypes.AlgebraicTypes.allTests(),
      hydra.test.inference.algorithmW.AlgorithmW.allTests(),
      hydra.test.inference.classes.Classes.allTests(),
      hydra.test.inference.failures.Failures.allTests(),
      hydra.test.inference.fundamentals.Fundamentals.allTests(),
      hydra.test.inference.kernelExamples.KernelExamples.allTests(),
      hydra.test.inference.nominalTypes.NominalTypes.allTests()), (hydra.util.ConsList<hydra.testing.TestCaseWithMetadata>) (hydra.util.ConsList.<hydra.testing.TestCaseWithMetadata>of()));
  }
}

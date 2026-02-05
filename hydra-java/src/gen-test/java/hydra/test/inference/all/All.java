// Note: this is an automatically generated file. Do not edit.

package hydra.test.inference.all;

/**
 * Hydra's inference test suite
 */
public interface All {
  static hydra.testing.TestGroup allTests() {
    return new hydra.testing.TestGroup("inference", (hydra.util.Maybe<String>) (hydra.util.Maybe.<String>nothing()), java.util.List.of(
      hydra.test.inference.algebraicTypes.AlgebraicTypes.allTests(),
      hydra.test.inference.algorithmW.AlgorithmW.allTests(),
      hydra.test.inference.failures.Failures.allTests(),
      hydra.test.inference.fundamentals.Fundamentals.allTests(),
      hydra.test.inference.kernelExamples.KernelExamples.allTests(),
      hydra.test.inference.nominalTypes.NominalTypes.allTests()), (java.util.List<hydra.testing.TestCaseWithMetadata>) (java.util.List.<hydra.testing.TestCaseWithMetadata>of()));
  }
}

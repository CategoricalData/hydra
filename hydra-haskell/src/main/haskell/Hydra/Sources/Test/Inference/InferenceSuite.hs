module Hydra.Sources.Test.Inference.InferenceSuite (inferenceTests) where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Testing as Testing
import Hydra.Sources.Test.Inference.AlgebraicTypes
import Hydra.Sources.Test.Inference.AlgorithmW
import Hydra.Sources.Test.Inference.Failures
import Hydra.Sources.Test.Inference.Fundamentals
import Hydra.Sources.Test.Inference.KernelExamples
import Hydra.Sources.Test.Inference.NominalTypes
import Hydra.Sources.Test.Inference.Simple


inferenceTests :: TTerm TestGroup
inferenceTests = supergroup "Inference tests" [
  algebraicTypesTests,
  algorithmWTests,
  failureTests,
  fundamentalsTests,
  kernelExamplesTests,
  nominalTypesTests,
  simpleTermsTests]

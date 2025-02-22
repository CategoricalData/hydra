module Hydra.Sources.Tier3.Test.Inference.InferenceSuite (inferenceTests) where

import Hydra.Kernel
import Hydra.Testing
import Hydra.Dsl.Testing as Testing
import Hydra.Sources.Tier3.Test.Inference.AlgebraicTypes
import Hydra.Sources.Tier3.Test.Inference.Fundamentals
import Hydra.Sources.Tier3.Test.Inference.NominalTypes
import Hydra.Sources.Tier3.Test.Inference.Simple


inferenceTests :: TTerm TestGroup
inferenceTests = supergroup "Inference tests" [
  algebraicTypesTests,
  fundamentalsTests,
  nominalTypesTests,
  simpleTermsTests]

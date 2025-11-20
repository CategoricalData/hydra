module Hydra.Sources.Test.Inference.InferenceSuite (inferenceTests) where

-- Standard imports for kernel tests
import Hydra.Kernel
import Hydra.Dsl.Meta.Testing as Testing
import Hydra.Dsl.Meta.Terms as Terms
import Hydra.Sources.Kernel.Types.All
import qualified Hydra.Dsl.Meta.Core as Core
import qualified Hydra.Dsl.Meta.Phantoms as Phantoms
import qualified Hydra.Dsl.Meta.Types as T
import qualified Hydra.Sources.Test.TestGraph as TestGraph
import qualified Hydra.Sources.Test.TestTerms as TestTerms
import qualified Hydra.Sources.Test.TestTypes as TestTypes
import qualified Data.List as L
import qualified Data.Map  as M

-- Additional imports
import Hydra.Sources.Test.Inference.AlgebraicTypes
import Hydra.Sources.Test.Inference.AlgorithmW
import Hydra.Sources.Test.Inference.Failures
import Hydra.Sources.Test.Inference.Fundamentals
import Hydra.Sources.Test.Inference.KernelExamples
import Hydra.Sources.Test.Inference.NominalTypes


inferenceTests :: TTerm TestGroup
inferenceTests = supergroup "Inference tests" []
--  ref algebraicTypesTestsDef,
--  ref algorithmWTestsDef,
--  ref failureTestsDef,
--  ref fundamentalsTestsDef,
--  ref kernelExamplesTestsDef,
--  ref nominalTypesTestsDef]

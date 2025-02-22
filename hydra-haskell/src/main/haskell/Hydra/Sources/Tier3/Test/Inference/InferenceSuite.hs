module Hydra.Sources.Tier3.Test.Inference.InferenceSuite (inferenceTests) where

import Hydra.Kernel
import Hydra.Testing
import qualified Hydra.Dsl.Base as Base
import qualified Hydra.Dsl.Core as Core
import Hydra.Dsl.Testing as Testing
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Lib.Lists as Lists
import qualified Hydra.Dsl.Lib.Maps as Maps
import qualified Hydra.Dsl.Lib.Math as Math
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier3.Test.TestGraph
import Hydra.Dsl.TTerms as TTerms
import qualified Hydra.Dsl.TTypes as T
import Hydra.Sources.Tier3.Test.Inference.Fundamentals
import Hydra.Sources.Tier3.Test.Inference.Simple

import qualified Data.Map as M
import Prelude hiding (map, product, sum)


inferenceTests :: TTerm TestGroup
inferenceTests = tgroup "Inference tests" Nothing subgroups []
  where
    subgroups = [simpleTermsTests, fundamentalsTests]

module Hydra.Sources.Tier3.Test.TestSuite (testSuiteModule) where

import Hydra.Testing
import qualified Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Types as Types
import Hydra.Sources.Tier2.All
import Hydra.Dsl.Base as Base
import Hydra.Dsl.Testing

import Hydra.Sources.Tier3.Test.Lib.Lists
import Hydra.Sources.Tier3.Test.Lib.Strings
import Hydra.Sources.Tier3.Test.Formatting
import Hydra.Sources.Tier3.Test.Inference
import Hydra.Sources.Tier3.Test.TestGraph

import qualified Data.List as L


testSuiteNs = Namespace "hydra/test/testSuite"

testSuiteModule :: Module
testSuiteModule = Module testSuiteNs elements [testGraphModule] [hydraCoreModule, hydraTestingModule] $
    Just "Test cases for primitive functions"
  where
    elements = [
      groupElement "allTests" allTests]

groupElement :: String -> TTerm TestGroup -> Element
groupElement lname group = Element name $ setTermType (Just typ) $ unTTerm group
  where
    name = unqualifyName $ QualifiedName (Just testSuiteNs) lname
    typ = TypeVariable _TestGroup

allTests :: TTerm TestGroup
allTests = tgroup "All tests" Nothing subgroups []
  where
    subgroups = (TTerm . encodeGroup <$> rawGroups) ++ tgroups
    tgroups = [inferenceTests]
    rawGroups = [
      listPrimitiveTests,
      stringPrimitiveTests]

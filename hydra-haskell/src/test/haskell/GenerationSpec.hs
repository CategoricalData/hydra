-- Bridge module to include generated tests in the test suite
-- This module re-exports the aggregated spec from generated tests

module GenerationSpec (spec) where

import qualified Generation.Spec as Generation
import qualified Test.Hspec as H

spec :: H.Spec
spec = Generation.spec

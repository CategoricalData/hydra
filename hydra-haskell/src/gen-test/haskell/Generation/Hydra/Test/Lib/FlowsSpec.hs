-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.flows"},ModuleName {unModuleName = "Flows"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.lib.maps"},ModuleName {unModuleName = "Maps"}),(Namespace {unNamespace = "hydra.lib.strings"},ModuleName {unModuleName = "Strings"})]

module Generation.Hydra.Test.Lib.FlowsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Strings as Strings

spec :: H.Spec
spec = H.describe "hydra.lib.maps primitives" $ do

  H.describe "singleton" $ do
    H.it "single entry" $ H.shouldBe
      (Maps.singleton 42 "hello")
      (M.fromList [
          (42, "hello")])

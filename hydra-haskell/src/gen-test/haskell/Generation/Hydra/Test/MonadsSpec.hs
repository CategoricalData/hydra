-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.monads"},ModuleName {unModuleName = "Monads"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.compute"},ModuleName {unModuleName = "Compute"}),(Namespace {unNamespace = "hydra.monads"},ModuleName {unModuleName = "Monads"})]

module Generation.Hydra.Test.MonadsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Compute as Compute
import qualified Hydra.Monads as Monads

spec :: H.Spec
spec = H.describe "monads" $ do
  H.describe "pure" $ do
    H.it "integer" $ H.shouldBe
      (Compute.unFlow (Monads.pure 42) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just 42),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
    H.it "string" $ H.shouldBe
      (Compute.unFlow (Monads.pure "hello") () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just "hello"),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})

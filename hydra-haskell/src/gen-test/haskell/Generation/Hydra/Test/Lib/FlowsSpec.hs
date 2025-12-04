-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.lib.flows"},ModuleName {unModuleName = "Flows"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.compute"},ModuleName {unModuleName = "Compute"}),(Namespace {unNamespace = "hydra.monads"},ModuleName {unModuleName = "Monads"})]

module Generation.Hydra.Test.Lib.FlowsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Compute as Compute
import qualified Hydra.Monads as Monads

spec :: H.Spec
spec = H.describe "hydra.lib.flows primitives" $ do
  H.describe "fail" $ do
    H.it "fail with message" $ H.shouldBe
      (Compute.unFlow (Monads.fail "test error message") () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = Nothing,
          Compute.flowStateState = (),
          Compute.flowStateTrace = Compute.Trace {
            Compute.traceStack = [],
            Compute.traceMessages = [
              "Error: test error message ()"],
            Compute.traceOther = M.empty}} :: Compute.FlowState () Int)
  H.describe "pure" $ do
    H.it "pure integer" $ H.shouldBe
      (Compute.unFlow (Monads.pure 42) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just 42),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
    H.it "pure zero" $ H.shouldBe
      (Compute.unFlow (Monads.pure 0) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just 0),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
    H.it "pure negative" $ H.shouldBe
      (Compute.unFlow (Monads.pure (-5)) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just (-5)),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
    H.it "pure string" $ H.shouldBe
      (Compute.unFlow (Monads.pure "hello") () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just "hello"),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})

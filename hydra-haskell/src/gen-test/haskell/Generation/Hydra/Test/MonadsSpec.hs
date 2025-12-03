-- Note: this is an automatically generated file. Do not edit.

-- DEBUG: Focus namespace = (Namespace {unNamespace = "generation.hydra.test.monads"},ModuleName {unModuleName = "Monads"})
-- DEBUG: Namespace mappings:
-- [(Namespace {unNamespace = "hydra.compute"},ModuleName {unModuleName = "Compute"}),(Namespace {unNamespace = "hydra.lib.math"},ModuleName {unModuleName = "Math"}),(Namespace {unNamespace = "hydra.monads"},ModuleName {unModuleName = "Monads"})]

module Generation.Hydra.Test.MonadsSpec where

import Hydra.Kernel
import qualified Test.Hspec as H
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Hydra.Compute as Compute
import qualified Hydra.Lib.Math as Math
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
  H.describe "map" $ do
    H.it "map negate" $ H.shouldBe
      (Compute.unFlow (Monads.map Math.negate (Monads.pure 5)) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just (-5)),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
    H.it "map absolute" $ H.shouldBe
      (Compute.unFlow (Monads.map Math.abs (Monads.pure (-3))) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just 3),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
  H.describe "bind" $ do
    H.it "bind add" $ H.shouldBe
      (Compute.unFlow (Monads.bind (Monads.pure 10) (\n -> Monads.pure (Math.add n 5))) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just 15),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
    H.it "bind multiply" $ H.shouldBe
      (Compute.unFlow (Monads.bind (Monads.pure 3) (\n -> Monads.pure (Math.mul n 4))) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = (Just 12),
          Compute.flowStateState = (),
          Compute.flowStateTrace = Monads.emptyTrace})
  H.describe "error traces" $ do
    H.it "Error traces are in the right order" $ H.shouldBe
      (Compute.unFlow (Monads.withTrace "one" (Monads.withTrace "two" (Monads.fail "oops"))) () Monads.emptyTrace)
      (Compute.FlowState {
          Compute.flowStateValue = Nothing,
          Compute.flowStateState = (),
          Compute.flowStateTrace = Compute.Trace {
            Compute.traceStack = [],
            Compute.traceMessages = [
              "Error: oops (one > two)"],
            Compute.traceOther = M.empty}} :: FlowState () String)

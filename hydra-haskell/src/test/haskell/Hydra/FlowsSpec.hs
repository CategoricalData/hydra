module Hydra.FlowsSpec where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Test.Hspec as H


checkErrorTrace :: H.SpecWith ()
checkErrorTrace = do
  H.describe "Check error traces resulting from an explicit failure" $ do

    H.it "Error traces are in the right order" $
      H.shouldBe
        (unFlow (withTrace "one" $ withTrace "two" $ if False then pure 42 else fail "oops") () emptyTrace)
        (FlowState Nothing () emptyTrace {traceMessages = ["Error: oops (one > two)"]})

checkMaxTraceDepth :: H.SpecWith ()
checkMaxTraceDepth = do
  H.describe "Check breaking out of flows with an infinite loop" $ do

    H.it "Flows with no trace are OK" $
      H.shouldBe
        (unFlow (testFlow 42 0) () emptyTrace)
        (FlowState (Just 42) () emptyTrace)

    H.it "Flows with a trace just below the maximum depth are OK" $
      H.shouldBe
        (unFlow (testFlow 42 maxTraceDepth) () emptyTrace)
        (FlowState (Just 42) () emptyTrace)

    H.it "Flows fail when their trace reaches the maximum depth" $ do
      H.shouldBe (flowStateValue overflow) Nothing
      H.shouldBe (L.length $ traceMessages $ flowStateTrace overflow) 1
  where
    overflow = unFlow (testFlow 42 (maxTraceDepth+1)) () emptyTrace

testFlow :: x -> Int -> Flow () x
testFlow seed depth = helper depth
  where
    helper d = if d == 0
      then pure seed
      else withTrace ("level " ++ show (depth-d)) $ helper (d-1)

spec :: H.Spec
spec = do
  checkErrorTrace
  checkMaxTraceDepth

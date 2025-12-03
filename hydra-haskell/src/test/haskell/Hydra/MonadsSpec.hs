{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.MonadsSpec.spec
-}

module Hydra.MonadsSpec where

import Hydra.Kernel
import qualified Hydra.Dsl.Terms as Terms
import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Test.Hspec as H


-- This test cannot be included in the kernel because it requires
-- building terms with 4000+ nested withTrace calls, which exceeds the trace depth limit
-- during kernel generation. These tests remain as hand-written HSpec tests only.
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
  checkMaxTraceDepth

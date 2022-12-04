-- | A small demo of "metered" Hydra evaluation. In this example, the evaluator keeps track of the number of times it
--   calls each primitive function (as a proxy for API calls, which can consume time and resources).

{-# LANGUAGE OverloadedStrings #-}
module Hydra.Demos.MeteredEvaluation (demoMeteredEvaluation) where

import Hydra.Kernel
import Hydra.Impl.Haskell.Dsl.Base as Base
import Hydra.Impl.Haskell.Sources.Module
import Hydra.Impl.Haskell.Sources.Mantle
import qualified Hydra.Impl.Haskell.Dsl.Standard as Standard
import qualified Hydra.Impl.Haskell.Dsl.Types as Types
import Hydra.Impl.Haskell.Dsl.Lib.Lists as Lists
import Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings
import Hydra.Impl.Haskell.Codegen
import Hydra.Reduction
import qualified Hydra.Impl.Haskell.Dsl.Lib.Literals as Literals
import qualified Hydra.Impl.Haskell.Dsl.Lib.Math as Math
import qualified Hydra.Impl.Haskell.Dsl.Lib.Strings as Strings
import Hydra.Impl.Haskell.Sources.Adapters.Utils
import Hydra.CoreEncoding

import System.IO
import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Prelude hiding ((++))


testNs = Namespace "com/linkedin/kgm/demo/meteredEvaluation"

testModule :: Module Meta
testModule = Module testNs elements [hydraMantleModule, adapterUtilsModule] Nothing
  where
    test = Definition . fromQname testNs
    elements = [
        el $ test "catStrings" (string "foo" ++ string "bar" ++ string "quux" ++ (Literals.showInt32 @@ int32 42)),
        el $ test "describeType" $ ref describeTypeSource @@ (Datum $ encodeType $ Types.list $ Types.int32)]

demoMeteredEvaluation :: IO ()
demoMeteredEvaluation = do
    let state = unFlow evaluateSelectedTerm context emptyTrace
    putStrLn $ traceSummary (flowStateTrace state)
    let result = flowStateValue state
    putStrLn $ "result: " <> show result
  where
    context = modulesToContext [testModule]
    evaluateSelectedTerm = do
      original <- dereferenceElement $ fromQname testNs "catStrings"
      reduced <- betaReduceTerm original
      return reduced

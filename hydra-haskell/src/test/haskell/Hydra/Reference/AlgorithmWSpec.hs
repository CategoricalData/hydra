{-# LANGUAGE OverloadedStrings #-}

{-
stack ghci hydra:lib hydra:hydra-test

Test.Hspec.hspec Hydra.Reference.AlgorithmWSpec.spec
-}
module Hydra.Reference.AlgorithmWSpec where

import Hydra.Kernel
import Hydra.Sources.Libraries
import Hydra.TestUtils
import Hydra.TestData
import qualified Hydra.Expect as Expect
import Hydra.Dsl.Terms as Terms
import qualified Hydra.Dsl.Annotations as Ann
import qualified Hydra.Dsl.Types as Types
import Hydra.Dsl.ShorthandTypes
import qualified Hydra.Reference.AlgorithmWBridge as W
import Hydra.Lib.Io

import qualified Hydra.TestUtils as TU
import Hydra.Testing
import Hydra.TestSuiteSpec
import Hydra.Test.TestSuite
import qualified Hydra.Dsl.Testing as Testing

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad


testHydraContext = W.HydraContext $ graphPrimitives testGraph

inferType :: Term -> IO (Term, TypeScheme)
inferType = W.termToInferredTerm testHydraContext

expectType :: Term -> TypeScheme -> H.Expectation
expectType term ts = do
  result <- inferType term
  H.shouldBe (showTypeScheme $ snd result) (showTypeScheme ts)
  H.shouldBe (showTerm $ stripTypesFromTerm $ fst result) (showTerm $ stripTypesFromTerm term)

algorithmWRunner :: TestRunner
algorithmWRunner desc tcase = if Testing.isDisabled tcase || Testing.isDisabledForMinimalInference tcase
  then Nothing
  else case testCaseWithMetadataCase tcase of
    TestCaseInference (InferenceTestCase input output) -> Just $ expectType input output
    _ -> Nothing

spec :: H.Spec
spec = runTestGroup "" algorithmWRunner allTests

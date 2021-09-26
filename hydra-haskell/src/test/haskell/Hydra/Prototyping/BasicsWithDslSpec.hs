module Hydra.Prototyping.BasicsWithDslSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Prototyping.Basics
import Hydra.Prototyping.BasicsWithDsl
import Hydra.Prototyping.Helpers
import Hydra.Prototyping.Interpreter
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.TermEncoding

import Hydra.ArbitraryCore
import Hydra.TestGraph

import Test.Hspec
import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.QuickCheck as QC


testEvaluate :: Term -> Either String Term
testEvaluate = evaluate $ testContext { contextElements = graphElementsForContext basicsGraph }

testsForBasicFunctions = do
  describe "Tests for basic DSL-defined functions" $ do
    
    it "Test atomicTypeVariant function element" $
      QC.property $ \at ->
        (testEvaluate (apply (deref "hydra/basics.atomicTypeVariant") (atomicTypeAsTerm at)))
        == (Right $ atomicVariantAsTerm $ atomicTypeVariant at)

{- TODO: also test:
        atomicValueType,
        atomicValueVariant,
        floatTypeVariant,
        floatValueType,
        floatValueVariant,
        integerTypeVariant,
        integerValueType,
        integerValueVariant,
        termVariant,
        typeVariant]
-}

spec :: Spec
spec = do
  testsForBasicFunctions

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

import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testEvaluate :: Term -> Either String Term
testEvaluate = evaluate $ testContext { contextElements = graphElementsForContext basicsGraph }

testsForTypeVariantFunctions = do
  H.describe "Tests for DSL-defined type-to-variant functions" $ do
    
    H.it "Test atomicTypeVariant function element" $
      QC.property $ \at ->
        (testEvaluate (apply (deref "hydra/basics.atomicTypeVariant") (atomicTypeAsTerm at)))
        == (Right $ atomicVariantAsTerm $ atomicTypeVariant at)

    H.it "Test floatTypeVariant function element" $
      QC.property $ \ft ->
        (testEvaluate (apply (deref "hydra/basics.floatTypeVariant") (floatTypeAsTerm ft)))
        == (Right $ floatVariantAsTerm $ floatTypeVariant ft)

    H.it "Test integerTypeVariant function element" $
      QC.property $ \at ->
        (testEvaluate (apply (deref "hydra/basics.integerTypeVariant") (integerTypeAsTerm at)))
        == (Right $ integerVariantAsTerm $ integerTypeVariant at)

    H.it "Test typeVariant function element" $
      QC.property $ \t ->
        (testEvaluate (apply (deref "hydra/basics.typeVariant") (typeAsTerm t)))
        == (Right $ typeVariantAsTerm $ typeVariant t)

{- TODO: also test:
        atomicValueType,
        atomicValueVariant,
        floatValueType,
        floatValueVariant,
        integerValueType,
        integerValueVariant,
        termVariant,
        ]
-}

spec :: H.Spec
spec = do
  testsForTypeVariantFunctions

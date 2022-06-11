module Hydra.Impl.Haskell.Sources.BasicsSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Graph
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.Basics
import Hydra.Impl.Haskell.Sources.Basics (hydraBasicsModule)
import Hydra.Interpreter
import Hydra.Primitives
import Hydra.CoreEncoding
import Hydra.Rewriting

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testEvaluate :: Term Meta -> Result (Term Meta)
testEvaluate term = do
  basics <- moduleGraph <$> hydraBasicsModule
  stripMeta <$> evaluate (testContext { contextElements = graphElementsMap basics }) term

testsForDataTypeExprFunctions :: H.SpecWith a
testsForDataTypeExprFunctions = do
  H.describe "Tests for DSL-defined term-to-type functions" $ do
    return ()
--    literalType,
--    floatValueType,
--    integerValueType,

testsForTermVariantFunctions :: H.SpecWith a
testsForTermVariantFunctions = do
  H.describe "Tests for DSL-defined term-to-variant functions" $ do
    return ()
--    literalVariant,
--    floatValueVariant,
--    integerValueVariant,
--    termVariant

testsForTypeVariantFunctions :: H.SpecWith ()
testsForTypeVariantFunctions = do
  H.describe "Tests for DSL-defined type-to-variant functions" $ do

    H.it "Test literalTypeVariant function element" $
      QC.property $ \at ->
        testEvaluate (apply (elementRefByName $ Name "hydra/basics.literalTypeVariant") (encodeLiteralType testContext at))
        `H.shouldBe`
        pure (stripMeta $ encodeLiteralVariant testContext $ literalTypeVariant at)

      -- TODO: interpreter support for projections
--    H.it "Test typeVariant function element" $
--      QC.property $ \t ->
--        testEvaluate (apply (elementRefByName "hydra/basics.typeVariant") (encodeType testContext t))
--        `H.shouldBe`
--        pure (stripMeta $ encodeTypeVariant testContext $ typeVariant t)

spec :: H.Spec
spec = do
  testsForDataTypeExprFunctions
  testsForTermVariantFunctions
  testsForTypeVariantFunctions

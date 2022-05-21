module Hydra.Impl.Haskell.Sources.BasicsSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Impl.Haskell.Extras
import Hydra.Basics
import Hydra.Impl.Haskell.Sources.Basics (hydraBasics)
import Hydra.Interpreter
import Hydra.Primitives
import Hydra.CoreEncoding
import Hydra.Rewriting

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testEvaluate :: Data Meta -> Result (Data Meta)
testEvaluate term = stripMeta <$> evaluate (testContext { contextElements = graphElementsMap hydraBasics }) term

testsForDataTypeTermFunctions :: H.SpecWith a
testsForDataTypeTermFunctions = do
  H.describe "Tests for DSL-defined term-to-type functions" $ do
    return ()
--    literalType,
--    floatValueType,
--    integerValueType,

testsForDataVariantFunctions :: H.SpecWith a
testsForDataVariantFunctions = do
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
  testsForDataTypeTermFunctions
  testsForDataVariantFunctions
  testsForTypeVariantFunctions

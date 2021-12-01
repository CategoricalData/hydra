module Hydra.Prototyping.BasicsWithDslSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl.Terms
import Hydra.Prototyping.Basics
import Hydra.Prototyping.BasicsWithDsl
import Hydra.Prototyping.Interpreter
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreEncoding

import Hydra.TestUtils

import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testEvaluate :: Term Meta -> Result (Term Meta)
testEvaluate = evaluate $ testContext { contextElements = graphElementsMap basicsGraph }

testsForTermTypeFunctions = do
  H.describe "Tests for DSL-defined term-to-type functions" $ do
    return ()
--    literalType,
--    floatValueType,
--    integerValueType,

testsForTermVariantFunctions = do
  H.describe "Tests for DSL-defined term-to-variant functions" $ do
    return ()
--    literalVariant,
--    floatValueVariant,
--    integerValueVariant,
--    termVariant

testsForTypeVariantFunctions = do
  H.describe "Tests for DSL-defined type-to-variant functions" $ do

    H.it "Test literalTypeVariant function element" $
      QC.property $ \at ->
        testEvaluate (apply (elementRefByName "hydra/basics.literalTypeVariant") (encodeLiteralType at))
        `H.shouldBe`
        pure (encodeLiteralVariant $ literalTypeVariant at)

    H.it "Test floatTypeVariant function element" $
      QC.property $ \ft ->
        testEvaluate (apply (elementRefByName "hydra/basics.floatTypeVariant") (encodeFloatType ft))
        `H.shouldBe`
        pure (encodeFloatVariant $ floatTypeVariant ft)

    H.it "Test integerTypeVariant function element" $
      QC.property $ \at ->
        testEvaluate (apply (elementRefByName "hydra/basics.integerTypeVariant") (encodeIntegerType at))
        `H.shouldBe`
        pure (encodeIntegerVariant $ integerTypeVariant at)

    H.it "Test typeVariant function element" $
      QC.property $ \t ->
        testEvaluate (apply (elementRefByName "hydra/basics.typeVariant") (encodeType t))
        `H.shouldBe`
        pure (encodeTypeVariant $ typeVariant t)

spec :: H.Spec
spec = do
  testsForTermTypeFunctions
  testsForTermVariantFunctions
  testsForTypeVariantFunctions

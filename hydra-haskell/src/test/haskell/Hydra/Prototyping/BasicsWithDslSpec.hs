module Hydra.Prototyping.BasicsWithDslSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Impl.Haskell.Dsl
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
--    atomicValueType,
--    floatValueType,
--    integerValueType,

testsForTermVariantFunctions = do
  H.describe "Tests for DSL-defined term-to-variant functions" $ do
    return ()
--    atomicValueVariant,
--    floatValueVariant,
--    integerValueVariant,
--    termVariant

testsForTypeVariantFunctions = do
  H.describe "Tests for DSL-defined type-to-variant functions" $ do

    H.it "Test atomicTypeVariant function element" $
      QC.property $ \at ->
        testEvaluate (apply (deref "hydra/basics.atomicTypeVariant") (encodeAtomicType at))
        == pure (encodeAtomicVariant $ atomicTypeVariant at)

    H.it "Test floatTypeVariant function element" $
      QC.property $ \ft ->
        testEvaluate (apply (deref "hydra/basics.floatTypeVariant") (encodeFloatType ft))
        == pure (encodeFloatVariant $ floatTypeVariant ft)

    H.it "Test integerTypeVariant function element" $
      QC.property $ \at ->
        testEvaluate (apply (deref "hydra/basics.integerTypeVariant") (encodeIntegerType at))
        == pure (encodeIntegerVariant $ integerTypeVariant at)

    H.it "Test typeVariant function element" $
      QC.property $ \t ->
        testEvaluate (apply (deref "hydra/basics.typeVariant") (encodeType t))
        == pure (encodeTypeVariant $ typeVariant t)

spec :: H.Spec
spec = do
  testsForTermTypeFunctions
  testsForTermVariantFunctions
  testsForTypeVariantFunctions

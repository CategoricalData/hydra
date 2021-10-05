module Hydra.Prototyping.BasicsWithDslSpec where

import Hydra.Core
import Hydra.Evaluation
import Hydra.Ext.Haskell.Dsl
import Hydra.Graph
import Hydra.Prototyping.Basics
import Hydra.Prototyping.BasicsWithDsl
import Hydra.Prototyping.Interpreter
import Hydra.Prototyping.Primitives
import Hydra.Prototyping.CoreEncoding

import Hydra.ArbitraryCore
import Hydra.TestGraph

import qualified Data.Char as C
import qualified Data.Either as E
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Test.Hspec as H
import qualified Test.QuickCheck as QC


testEvaluate :: Term -> Either String Term
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
        (testEvaluate (apply (deref "hydra/basics.atomicTypeVariant") (encodeAtomicType at)))
        == (Right $ encodeAtomicVariant $ atomicTypeVariant at)

    H.it "Test floatTypeVariant function element" $
      QC.property $ \ft ->
        (testEvaluate (apply (deref "hydra/basics.floatTypeVariant") (encodeFloatType ft)))
        == (Right $ encodeFloatVariant $ floatTypeVariant ft)

    H.it "Test integerTypeVariant function element" $
      QC.property $ \at ->
        (testEvaluate (apply (deref "hydra/basics.integerTypeVariant") (encodeIntegerType at)))
        == (Right $ encodeIntegerVariant $ integerTypeVariant at)

    H.it "Test typeVariant function element" $
      QC.property $ \t ->
        (testEvaluate (apply (deref "hydra/basics.typeVariant") (encodeType t)))
        == (Right $ encodeTypeVariant $ typeVariant t)

spec :: H.Spec
spec = do
  testsForTermTypeFunctions
  testsForTermVariantFunctions
  testsForTypeVariantFunctions

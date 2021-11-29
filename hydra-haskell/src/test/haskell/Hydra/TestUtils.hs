module Hydra.TestUtils (
  checkAtomicAdapter,
  checkFieldAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkTermAdapter,
  isFailure,
  termTestContext,
  module Hydra.TestGraph,
  module Hydra.Prototyping.Steps,
) where

import Hydra.ArbitraryCore()

import Hydra.Core
import Hydra.Adapter
import Hydra.Errors
import Hydra.TestGraph
import Hydra.Prototyping.Adapters.Atomic
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Steps
import Hydra.Impl.Haskell.Dsl

import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Test.Hspec as H

baseLanguage :: Language
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext Meta
baseContext = AdapterContext testContext baseLanguage baseLanguage

checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (AdapterContext Meta -> t -> Qualified (Adapter t v))
  -> (r -> AdapterContext Meta)
  -> r -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter mkAdapter context variants source target lossy vs vt = do
    (if Y.isNothing adapter' then warnings else []) `H.shouldBe` []
    adapterSource adapter `H.shouldBe` source
    adapterTarget adapter `H.shouldBe` target
    adapterIsLossy adapter `H.shouldBe` lossy
    stepOut step vs `H.shouldBe` ResultSuccess vt
    if lossy then True `H.shouldBe` True else (stepOut step vs >>= stepIn step) `H.shouldBe` ResultSuccess vs
  where
    Qualified adapter' warnings = mkAdapter (context variants) source
    adapter = Y.fromJust adapter'
    step = adapterStep adapter

checkAtomicAdapter :: [LiteralVariant] -> LiteralType -> LiteralType -> Bool -> Literal -> Literal -> H.Expectation
checkAtomicAdapter = checkAdapter atomicAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsLiteralVariants = S.fromList variants,
        languageConstraintsFloatVariants = floatVars,
        languageConstraintsIntegerVariants = integerVars }
      where
        floatVars = S.fromList [FloatVariantFloat32]
        integerVars = S.fromList [IntegerVariantInt16, IntegerVariantInt32]

checkFieldAdapter :: [TypeVariant] -> FieldType -> FieldType -> Bool -> Field Meta -> Field Meta -> H.Expectation
checkFieldAdapter = checkAdapter fieldAdapter termTestContext

checkFloatAdapter :: [FloatVariant] -> FloatType -> FloatType -> Bool -> FloatValue -> FloatValue -> H.Expectation
checkFloatAdapter = checkAdapter floatAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsFloatVariants = S.fromList variants }

checkIntegerAdapter :: [IntegerVariant] -> IntegerType -> IntegerType -> Bool -> IntegerValue -> IntegerValue -> H.Expectation
checkIntegerAdapter = checkAdapter integerAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsIntegerVariants = S.fromList variants }

checkTermAdapter :: [TypeVariant] -> Type -> Type -> Bool -> Term Meta -> Term Meta -> H.Expectation
checkTermAdapter = checkAdapter termAdapter termTestContext

termTestContext :: [TypeVariant] -> AdapterContext Meta
termTestContext variants = withConstraints $ (languageConstraints baseLanguage) {
    languageConstraintsTypeVariants = S.fromList variants,
    languageConstraintsLiteralVariants = atomicVars,
    languageConstraintsFloatVariants = floatVars,
    languageConstraintsIntegerVariants = integerVars }
  where
    atomicVars = S.fromList [LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString]
    floatVars = S.fromList [FloatVariantFloat32]
    integerVars = S.fromList [IntegerVariantInt16, IntegerVariantInt32]

isFailure :: Result a -> Bool
isFailure r = case r of
  ResultFailure _ -> True
  _ -> False

withConstraints :: Language_Constraints -> AdapterContext Meta
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}

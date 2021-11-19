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

import Hydra.V2.Core
import Hydra.V2.Adapter
import Hydra.Errors
import Hydra.TestGraph
import Hydra.Prototyping.Adapters.Atomic
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Steps

import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Test.Hspec as H

baseLanguage :: Language
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext
baseContext = AdapterContext testContext baseLanguage baseLanguage


checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (AdapterContext -> t -> Qualified (Adapter t v))
  -> (r -> AdapterContext)
  -> r -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter mkAdapter context variants source target lossy vs vt = do
    Y.isJust adapter' `H.shouldBe` True
    adapterSource adapter `H.shouldBe` source
    adapterTarget adapter `H.shouldBe` target
    adapterIsLossy adapter `H.shouldBe` lossy
    stepOut step vs `H.shouldBe` ResultSuccess vt
    if lossy then True `H.shouldBe` True else (stepOut step vs >>= stepIn step) `H.shouldBe` ResultSuccess vs
  where
    adapter' = qualifiedValue $ mkAdapter (context variants) source
    adapter = Y.fromJust adapter'
    step = adapterStep adapter

checkAtomicAdapter :: [AtomicVariant] -> AtomicType -> AtomicType -> Bool -> AtomicValue -> AtomicValue -> H.Expectation
checkAtomicAdapter = checkAdapter atomicAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsAtomicVariants = S.fromList variants,
        languageConstraintsFloatVariants = floatVars,
        languageConstraintsIntegerVariants = integerVars }
      where
        floatVars = S.fromList [FloatVariantFloat32]
        integerVars = S.fromList [IntegerVariantInt16, IntegerVariantInt32]

checkFieldAdapter :: [TypeVariant] -> FieldType -> FieldType -> Bool -> Field -> Field -> H.Expectation
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

checkTermAdapter :: [TypeVariant] -> Type -> Type -> Bool -> Term -> Term -> H.Expectation
checkTermAdapter = checkAdapter termAdapter termTestContext

termTestContext :: [TypeVariant] -> AdapterContext
termTestContext variants = withConstraints $ (languageConstraints baseLanguage) {
    languageConstraintsTypeVariants = S.fromList variants,
    languageConstraintsAtomicVariants = atomicVars,
    languageConstraintsFloatVariants = floatVars,
    languageConstraintsIntegerVariants = integerVars }
  where
    atomicVars = S.fromList [AtomicVariantFloat, AtomicVariantInteger, AtomicVariantString]
    floatVars = S.fromList [FloatVariantFloat32]
    integerVars = S.fromList [IntegerVariantInt16, IntegerVariantInt32]

isFailure :: Result a -> Bool
isFailure r = case r of
  ResultFailure _ -> True
  _ -> False

withConstraints :: Language_Constraints -> AdapterContext
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}

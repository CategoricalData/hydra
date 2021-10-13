module Hydra.TestUtils (
  checkAtomicAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkTermAdapter,
  isFailure,
  termTestContext,
  module Hydra.TestGraph,
  module Hydra.Prototyping.Steps,
) where

-- Do not remove.  
import Hydra.ArbitraryCore

import Hydra.Core
import Hydra.Adapter
import Hydra.Errors
import Hydra.TestGraph
import Hydra.Prototyping.Adapters.Atomic
import Hydra.Prototyping.Adapters.Term
import Hydra.Prototyping.Basics
import Hydra.Prototyping.Steps

import qualified Data.Set as S
import qualified Data.Maybe as Y


baseLanguage :: Language
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext
baseContext = AdapterContext testContext baseLanguage baseLanguage

checkAdapter :: (Eq t, Eq v)
  => (AdapterContext -> t -> Qualified (Adapter t v))
  -> (r -> AdapterContext)
  -> r -> t -> t -> Bool -> v -> v -> Bool
checkAdapter mkAdapter context variants source target lossy vs vt = 
    Y.isJust adapter'
    && adapterSource adapter == source
    && adapterTarget adapter == target
    && adapterIsLossy adapter == lossy
    && stepOut step vs == ResultSuccess vt
    && if lossy then True else (stepOut step vs >>= stepIn step) == ResultSuccess vs
  where
    adapter' = qualifiedValue $ mkAdapter (context variants) source
    adapter = Y.fromJust adapter'
    step = adapterStep adapter

checkAtomicAdapter :: [AtomicVariant] -> AtomicType -> AtomicType -> Bool -> AtomicValue -> AtomicValue -> Bool
checkAtomicAdapter = checkAdapter atomicAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsAtomicVariants = S.fromList variants,
        languageConstraintsFloatVariants = floatVars,
        languageConstraintsIntegerVariants = integerVars }
      where
        floatVars = S.fromList [FloatVariantFloat32]
        integerVars = S.fromList [IntegerVariantInt16, IntegerVariantInt32]

checkFloatAdapter :: [FloatVariant] -> FloatType -> FloatType -> Bool -> FloatValue -> FloatValue -> Bool
checkFloatAdapter = checkAdapter floatAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsFloatVariants = S.fromList variants }

checkIntegerAdapter :: [IntegerVariant] -> IntegerType -> IntegerType -> Bool -> IntegerValue -> IntegerValue -> Bool
checkIntegerAdapter = checkAdapter integerAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsIntegerVariants = S.fromList variants }

checkTermAdapter :: [TypeVariant] -> Type -> Type -> Bool -> Term -> Term -> Bool
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

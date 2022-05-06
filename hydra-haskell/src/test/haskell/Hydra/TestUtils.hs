module Hydra.TestUtils (
  checkAtomicAdapter,
  checkFieldAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkTermAdapter,
  isFailure,
  termTestContext,
  unitVar,
  var,
  module Hydra.TestGraph,
  module Hydra.Steps,
) where

import Hydra.ArbitraryCore()

import Hydra.Core
import Hydra.Adapter
import Hydra.Errors
import Hydra.TestGraph
import Hydra.Adapters.Atomic
import Hydra.Adapters.Term
import Hydra.CoreLanguage
import Hydra.Basics
import Hydra.Steps
import Hydra.Impl.Haskell.Extras
import Hydra.Impl.Haskell.Dsl.CoreMeta
import Hydra.Rewriting

import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Test.Hspec as H


baseLanguage :: Language m
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext Meta
baseContext = AdapterContext testContext baseLanguage baseLanguage

checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (v -> v)
  -> (AdapterContext Meta -> t -> Qualified (Adapter t v))
  -> (r -> AdapterContext Meta)
  -> r -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter normalize mkAdapter context variants source target lossy vs vt = do
    (if Y.isNothing adapter' then warnings else []) `H.shouldBe` []
    adapterSource adapter `H.shouldBe` source
    adapterTarget adapter `H.shouldBe` target
    adapterIsLossy adapter `H.shouldBe` lossy
    (normalize <$> stepOut step vs) `H.shouldBe` ResultSuccess (normalize vt)
    if lossy then True `H.shouldBe` True else (stepOut step vs >>= stepIn step) `H.shouldBe` ResultSuccess vs
  where
    Qualified adapter' warnings = mkAdapter (context variants) source
    adapter = Y.fromJust adapter'
    step = adapterStep adapter

checkAtomicAdapter :: [LiteralVariant] -> LiteralType -> LiteralType -> Bool -> Literal -> Literal -> H.Expectation
checkAtomicAdapter = checkAdapter id atomicAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
        languageConstraintsLiteralVariants = S.fromList variants,
        languageConstraintsFloatTypes = floatVars,
        languageConstraintsIntegerTypes = integerVars }
      where
        floatVars = S.fromList [FloatTypeFloat32]
        integerVars = S.fromList [IntegerTypeInt16, IntegerTypeInt32]

checkFieldAdapter :: [TypeVariant] -> FieldType Meta -> FieldType Meta -> Bool -> Field Meta -> Field Meta -> H.Expectation
checkFieldAdapter = checkAdapter id fieldAdapter termTestContext

checkFloatAdapter :: [FloatType] -> FloatType -> FloatType -> Bool -> FloatValue -> FloatValue -> H.Expectation
checkFloatAdapter = checkAdapter id floatAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsFloatTypes = S.fromList variants }

checkIntegerAdapter :: [IntegerType] -> IntegerType -> IntegerType -> Bool -> IntegerValue -> IntegerValue -> H.Expectation
checkIntegerAdapter = checkAdapter id integerAdapter context
  where
    context variants = withConstraints $ (languageConstraints baseLanguage) {
      languageConstraintsIntegerTypes = S.fromList variants }

checkTermAdapter :: [TypeVariant] -> Type Meta -> Type Meta -> Bool -> Term Meta -> Term Meta -> H.Expectation
checkTermAdapter = checkAdapter stripMeta termAdapter termTestContext

termTestContext :: [TypeVariant] -> AdapterContext Meta
termTestContext variants = withConstraints $ (languageConstraints baseLanguage) {
    languageConstraintsTypeVariants = S.fromList variants,
    languageConstraintsLiteralVariants = atomicVars,
    languageConstraintsFloatTypes = floatVars,
    languageConstraintsIntegerTypes = integerVars }
  where
    atomicVars = S.fromList [LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString]
    floatVars = S.fromList [FloatTypeFloat32]
    integerVars = S.fromList [IntegerTypeInt16, IntegerTypeInt32]

isFailure :: Result a -> Bool
isFailure r = case r of
  ResultFailure _ -> True
  _ -> False

unitVar :: Name -> FieldName -> Term Meta
unitVar = nominalUnitVariant testContext

var :: Name -> FieldName -> Term Meta -> Term Meta
var = nominalVariant testContext

withConstraints :: Language_Constraints Meta -> AdapterContext Meta
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}

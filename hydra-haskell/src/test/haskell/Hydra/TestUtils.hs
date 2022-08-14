module Hydra.TestUtils (
  checkLiteralAdapter,
  checkFieldAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkDataAdapter,
  isFailure,
  strip,
  termTestContext,
  module Hydra.TestGraph,
  module Hydra.Evaluation,
  module Hydra.Monads,
) where

import Hydra.ArbitraryCore()

import Hydra.Common
import Hydra.Core
import Hydra.Adapter
import Hydra.Errors
import Hydra.Evaluation
import Hydra.TestGraph
import Hydra.Adapters.Literal
import Hydra.Adapters.Term
import Hydra.CoreLanguage
import Hydra.Rewriting
import Hydra.Monads

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
    (normalize <$> coderEncode step vs) `H.shouldBe` ResultSuccess (normalize vt)
    if lossy then True `H.shouldBe` True else (coderEncode step vs >>= coderDecode step) `H.shouldBe` ResultSuccess vs
  where
    Qualified adapter' warnings = mkAdapter (context variants) source
    adapter = Y.fromJust adapter'
    step = adapterCoder adapter

checkLiteralAdapter :: [LiteralVariant] -> LiteralType -> LiteralType -> Bool -> Literal -> Literal -> H.Expectation
checkLiteralAdapter = checkAdapter id literalAdapter context
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

checkDataAdapter :: [TypeVariant] -> Type Meta -> Type Meta -> Bool -> Term Meta -> Term Meta -> H.Expectation
checkDataAdapter = checkAdapter (termExpr testContext) termAdapter termTestContext

termTestContext :: [TypeVariant] -> AdapterContext Meta
termTestContext variants = withConstraints $ (languageConstraints baseLanguage) {
    languageConstraintsTypeVariants = S.fromList variants,
    languageConstraintsLiteralVariants = literalVars,
    languageConstraintsFloatTypes = floatVars,
    languageConstraintsIntegerTypes = integerVars }
  where
    literalVars = S.fromList [LiteralVariantFloat, LiteralVariantInteger, LiteralVariantString]
    floatVars = S.fromList [FloatTypeFloat32]
    integerVars = S.fromList [IntegerTypeInt16, IntegerTypeInt32]

isFailure :: Result a -> Bool
isFailure r = case r of
  ResultFailure _ -> True
  _ -> False

strip :: Ord m => Term m -> Term m
strip = stripTermAnnotations

withConstraints :: LanguageConstraints Meta -> AdapterContext Meta
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}

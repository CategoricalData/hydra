module Hydra.TestUtils (
  checkLiteralAdapter,
  checkFieldAdapter,
  checkFloatAdapter,
  checkIntegerAdapter,
  checkDataAdapter,
  checkSerdeRoundTrip,
  checkSerialization,
  shouldFail,
  shouldSucceedWith,
  strip,
  termTestContext,
  module Hydra.TestGraph,
  module Hydra.Compute,
  module Hydra.Monads,
) where

import Hydra.ArbitraryCore()

import Hydra.Common
import Hydra.Core
import Hydra.Compute
import Hydra.TestGraph
import Hydra.Adapters.Literal
import Hydra.Adapters.Term
import Hydra.Adapters.UtilsEtc
import Hydra.CoreLanguage
import Hydra.Rewriting
import Hydra.Monads

import qualified Test.Hspec as H
import qualified Test.HUnit.Lang as HL
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Maybe as Y
import qualified Data.ByteString.Lazy as BS


baseLanguage :: Language m
baseLanguage = hydraCoreLanguage

baseContext :: AdapterContext Meta
baseContext = AdapterContext testContext baseLanguage baseLanguage

checkAdapter :: (Eq t, Eq v, Show t, Show v)
  => (v -> v)
  -> (t -> Flow (AdapterContext Meta) (SymmetricAdapter (Context Meta) t v))
  -> ([r] -> AdapterContext Meta)
  -> [r] -> t -> t -> Bool -> v -> v -> H.Expectation
checkAdapter normalize mkAdapter mkContext variants source target lossy vs vt = do
    let acx = mkContext variants :: AdapterContext Meta
    let cx = adapterContextEvaluation acx
    let FlowWrapper adapter' _ trace = unFlow (mkAdapter source) acx emptyTrace
    if Y.isNothing adapter' then HL.assertFailure (traceSummary trace) else pure ()
    let adapter = Y.fromJust adapter'
    let step = adapterCoder adapter
    adapterSource adapter `H.shouldBe` source
    adapterTarget adapter `H.shouldBe` target
    adapterIsLossy adapter `H.shouldBe` lossy
    fromFlow cx (normalize <$> coderEncode step vs) `H.shouldBe` (normalize vt)
    if lossy
      then True `H.shouldBe` True
      else fromFlow cx (coderEncode step vs >>= coderDecode step) `H.shouldBe` vs

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
checkDataAdapter = checkAdapter stripTerm termAdapter termTestContext

checkSerdeRoundTrip :: (Type Meta -> GraphFlow Meta (Coder (Context Meta) (Context Meta) (Term Meta) BS.ByteString))
  -> TypedTerm Meta -> H.Expectation
checkSerdeRoundTrip mkSerde (TypedTerm typ term) = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (stripTerm <$> (coderEncode serde term >>= coderDecode serde))
        (stripTerm term)
  where
    FlowWrapper mserde _ trace = unFlow (mkSerde typ) testContext emptyTrace

checkSerialization :: (Type Meta -> GraphFlow Meta (Coder (Context Meta) (Context Meta) (Term Meta) String))
  -> TypedTerm Meta -> String -> H.Expectation
checkSerialization mkSerdeStr (TypedTerm typ term) expected = do
    case mserde of
      Nothing -> HL.assertFailure (traceSummary trace)
      Just serde -> shouldSucceedWith
        (normalize <$> coderEncode serde term)
        (normalize expected)
  where
    normalize = unlines . L.filter (not . L.null) . lines
    FlowWrapper mserde _ trace = unFlow (mkSerdeStr typ) testContext emptyTrace

shouldFail :: GraphFlow Meta a -> H.Expectation
shouldFail f = H.shouldBe True (Y.isNothing $ flowWrapperValue $ unFlow f testContext emptyTrace)

shouldSucceed :: GraphFlow Meta a -> H.Expectation
shouldSucceed f = case my of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just y -> True `H.shouldBe` True
  where
    FlowWrapper my _ trace = unFlow f testContext emptyTrace

shouldSucceedWith :: (Eq a, Show a) => GraphFlow Meta a -> a -> H.Expectation
shouldSucceedWith f x = case my of
    Nothing -> HL.assertFailure (traceSummary trace)
    Just y -> y `H.shouldBe` x
  where
    FlowWrapper my _ trace = unFlow f testContext emptyTrace

strip :: Ord m => Term m -> Term m
strip = stripTerm

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

withConstraints :: LanguageConstraints Meta -> AdapterContext Meta
withConstraints c = baseContext { adapterContextTarget = baseLanguage { languageConstraints = c }}
